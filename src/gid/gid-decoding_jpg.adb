--  Steps for decoding a JPEG image
--
--  1. Huffman decompression
--  2. Inverse quantization
--  3. Inverse cosine transform
--  4. Upsampling
--  5. Color transformation
--  6. Image reconstruction
--
--  The JPEG decoder is largely inspired
--  by the NanoJPEG code by Martin J. Fiedler.
--  With the author's permission. Many thanks!
--
--  Other informations:
--  http://en.wikipedia.org/wiki/JPEG

--  !! ** Some optimizations to consider **
--  !! ssx, ssy ,ssxmax, ssymax
--       as generic parameters + specialized instances
--  !! consider only power-of-two upsampling factors ?
--  !! simplify upsampling loops in case of power-of-two upsampling factors
--       using Shift_Right
--  !! Col_IDCT output direct to "flat", or something similar to NanoJPEG

with GID.Buffering;
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.IO_Exceptions;

package body GID.Decoding_JPG is

  use Buffering, Ada.Text_IO, Interfaces;

  generic
    type Number is mod <>;
  procedure Big_endian_number (
    from : in out Input_Buffer;
    n    :    out Number
  );
  pragma Inline (Big_endian_number);

  procedure Big_endian_number (
    from : in out Input_Buffer;
    n    :    out Number
  )
  is
    b : U8;
  begin
    n := 0;
    for i in 1 .. Number'Size / 8 loop
      Get_Byte (from, b);
      n := n * 256 + Number (b);
    end loop;
  end Big_endian_number;

  procedure Big_endian is new Big_endian_number (U16);

  procedure Read (image : in out Image_descriptor; sh : out Segment_head) is
    b : U8;
    id : constant array (JPEG_marker) of U8 :=
    (SOI      => 16#D8#,
      --
      SOF_0  => 16#C0#, SOF_1  => 16#C1#, SOF_2  => 16#C2#, SOF_3  => 16#C3#,
      SOF_5  => 16#C5#, SOF_6  => 16#C6#, SOF_7  => 16#C7#, SOF_8  => 16#C8#,
      SOF_9  => 16#C9#, SOF_10 => 16#CA#, SOF_11 => 16#CB#, SOF_13 => 16#CD#,
      SOF_14 => 16#CE#, SOF_15 => 16#CF#,
      --
      DHT      => 16#C4#,
      DAC      => 16#CC#,
      DQT      => 16#DB#,
      DRI      => 16#DD#,
      --
      APP_0  => 16#E0#, APP_1  => 16#E1#, APP_2  => 16#E2#, APP_3  => 16#E3#,
      APP_4  => 16#E4#, APP_5  => 16#E5#, APP_6  => 16#E6#, APP_7  => 16#E7#,
      APP_8  => 16#E8#, APP_9  => 16#E9#, APP_10 => 16#EA#, APP_11 => 16#EB#,
      APP_12 => 16#EC#, APP_13 => 16#ED#, APP_14 => 16#EE#,
      --
      COM      => 16#FE#,
      SOS      => 16#DA#,
      EOI      => 16#D9#
    );
  begin
    Get_Byte (image.buffer, b);
    if b /= 16#FF# then
      raise error_in_image_data with "JPEG: expected marker here";
    end if;
    Get_Byte (image.buffer, b);
    for m in id'Range loop
      if id (m) = b then
        sh.kind := m;
        Big_endian (image.buffer, sh.length);
        sh.length := sh.length - 2;
        --  We consider length of contents, without the FFxx marker.
        if some_trace then
          Put_Line (
            "Segment [" & JPEG_marker'Image (sh.kind) &
            "], length:" & U16'Image (sh.length));
        end if;
        return;
      end if;
    end loop;
    raise error_in_image_data with "JPEG: unknown marker here: FF, " & U8'Image (b);
  end Read;

  shift_arg : constant array (0 .. 15) of Integer :=
    (1 => 0, 2 => 1, 4 => 2, 8 => 3, others => -1);

  --  SOF - Start Of Frame (the real header)
  procedure Read_SOF (image : in out Image_descriptor; sh : Segment_head) is
    use Bounded_255;
    b, bits_pp_primary, id_base : U8;
    w, h : U16;
    compo : JPEG_Defs.Component;
  begin
    case sh.kind is
      when SOF_0 =>
        image.detailed_format := To_Bounded_String ("JPEG, Baseline DCT (SOF_0)");
      when SOF_2 =>
        image.detailed_format := To_Bounded_String ("JPEG, Progressive DCT (SOF_2)");
        image.interlaced := True;
      when others =>
        raise unsupported_image_subformat with
          "JPEG: image type not yet supported: " & JPEG_marker'Image (sh.kind);
    end case;
    Get_Byte (image.buffer, bits_pp_primary);
    if bits_pp_primary /= 8 then
      raise unsupported_image_subformat with
        "JPEG: bits per primary color=" & U8'Image (bits_pp_primary) & " (not supported)";
    end if;
    image.bits_per_pixel := 3 * Positive (bits_pp_primary);
    Big_endian (image.buffer, h);
    Big_endian (image.buffer, w);
    if w = 0 then
      raise error_in_image_data with "JPEG: zero image width";
    end if;
    if h = 0 then
      raise error_in_image_data with "JPEG: zero image height";
    end if;
    image.width  := Positive_32 (w);
    image.height := Positive_32 (h);
    --  Number of components:
    Get_Byte (image.buffer, b);
    image.subformat_id := Integer (b);
    --
    image.JPEG_stuff.max_samples_hor := 0;
    image.JPEG_stuff.max_samples_ver := 0;
    id_base := 1;
    --  For each component: 3 bytes information: ID, sampling factors, quantization table number
    for i in 1 .. image.subformat_id loop
      --  Component ID (1 = Y, 2 = Cb, 3 = Cr, 4 = I, 5 = Q)
      Get_Byte (image.buffer, b);
      if b = 0 then
        --  Workaround for a bug in some encoders, for instance Intel(R) JPEG Library,
        --  version [2.0.18.50] as in some Photoshop versions : IDs are numbered 0, 1, 2.
        id_base := 0;
      end if;
      if b - id_base > Component'Pos (Component'Last) then
        raise error_in_image_data with "JPEG: SOF: invalid component ID: " & U8'Image (b);
      end if;
      compo := JPEG_Defs.Component'Val (b - id_base);
      image.JPEG_stuff.components (compo) := True;
      declare
        stuff : JPEG_Stuff_Type renames image.JPEG_stuff;
        info : JPEG_Defs.Info_per_Component_A renames stuff.info (compo);
      begin
        --  Sampling factors (bit 0-3 vert., 4-7 hor.)
        Get_Byte (image.buffer, b);
        info.samples_ver := Natural (b mod 16);
        info.samples_hor := Natural (b  /  16);
        stuff.max_samples_hor :=
          Integer'Max (stuff.max_samples_hor, info.samples_hor);
        stuff.max_samples_ver :=
          Integer'Max (stuff.max_samples_ver, info.samples_ver);
        --  Quantization table number
        Get_Byte (image.buffer, b);
        info.qt_assoc := Natural (b);
      end;
    end loop;
    for c in Component loop
      if image.JPEG_stuff.components (c) then
        declare
          stuff : JPEG_Stuff_Type renames image.JPEG_stuff;
          info : JPEG_Defs.Info_per_Component_A renames stuff.info (c);
        begin
          info.up_factor_x := stuff.max_samples_hor / info.samples_hor;
          info.up_factor_y := stuff.max_samples_ver / info.samples_ver;
          info.shift_x := shift_arg (info.up_factor_x);
          info.shift_y := shift_arg (info.up_factor_y);
        end;
      end if;
    end loop;
    if Natural (sh.length) < 6 + 3 * image.subformat_id then
      raise error_in_image_data with "JPEG: SOF segment too short";
    end if;
    if some_trace then
      Put_Line ("Frame has following components:");
      for c in JPEG_Defs.Component loop
        Put_Line (
          JPEG_Defs.Component'Image (c) & " -> " &
          Boolean'Image (image.JPEG_stuff.components (c))
        );
      end loop;
    end if;
    if image.JPEG_stuff.components = YCbCr_set then
      image.JPEG_stuff.color_space := YCbCr;
    elsif image.JPEG_stuff.components = Y_Grey_set then
      image.JPEG_stuff.color_space := Y_Grey;
      image.greyscale := True;
    elsif image.JPEG_stuff.components = CMYK_set then
      image.JPEG_stuff.color_space := CMYK;
    else
      raise unsupported_image_subformat with
        "JPEG: only YCbCr, Y_Grey and CMYK color spaces are currently supported";
    end if;
    image.detailed_format := image.detailed_format & ", " &
      JPEG_Defs.Supported_color_space'Image (image.JPEG_stuff.color_space);
    if some_trace then
      Put_Line (
        "Color space: " &
        JPEG_Defs.Supported_color_space'Image (image.JPEG_stuff.color_space)
      );
    end if;
    if image.JPEG_stuff.color_space = CMYK then
      raise unsupported_image_subformat with
        "JPEG: CMYK color space is currently not properly decoded";
    end if;
  end Read_SOF;

  procedure Read_DHT (image : in out Image_descriptor; data_length : Natural) is
    remaining : Integer_M32 := Integer_M32 (data_length); -- data remaining in segment
    b : U8;
    ht_idx : Natural;
    kind : AC_DC;
    counts : array (1 .. 16) of Integer_M32;
    idx : Natural;
    currcnt, spread, remain_vlc : Integer_M32;
  begin
    multi_tables :
    loop
      Get_Byte (image.buffer, b);
      remaining := remaining - 1;
      if b >= 8 then
        kind := AC;
      else
        kind := DC;
      end if;
      ht_idx := Natural (b and 7);
      if some_trace then
        Put_Line (
          "Huffman Table (HT) #" &
          Natural'Image (ht_idx) & ", " & AC_DC'Image (kind)
        );
      end if;
      if image.JPEG_stuff.vlc_defs (kind, ht_idx) = null then
        image.JPEG_stuff.vlc_defs (kind, ht_idx) := new VLC_table;
      end if;
      for i in counts'Range loop
        Get_Byte (image.buffer, b);
        remaining := remaining - 1;
        counts (i) := Integer_M32 (b);
      end loop;
      remain_vlc := 65_536;
      spread := 65_536;
      idx := 0;
      for codelen in counts'Range loop
        spread := spread / 2;
        currcnt := counts (codelen);
        if currcnt > 0 then
          if remaining < currcnt then
            raise error_in_image_data with "JPEG: DHT data too short";
          end if;
          remain_vlc := remain_vlc - currcnt * spread;
          if remain_vlc < 0 then
            raise error_in_image_data with "JPEG: DHT table too short for data";
          end if;
          for i in reverse 1 .. currcnt loop
            Get_Byte (image.buffer, b);
            for j in reverse 1 .. spread loop
              image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx) :=
                (bits => U8 (codelen), code => b);
              idx := idx + 1;
            end loop;
          end loop;
          remaining := remaining - currcnt;
        end if;
      end loop;
      while remain_vlc > 0 loop
        remain_vlc := remain_vlc - 1;
        image.JPEG_stuff.vlc_defs (kind, ht_idx)(idx).bits := 0;
        idx := idx + 1;
      end loop;
      exit multi_tables when remaining <= 0;
    end loop multi_tables;
  end Read_DHT;

  procedure Read_DQT (image : in out Image_descriptor; data_length : Natural) is
    remaining : Integer := data_length; -- data remaining in segment
    b, q8 : U8; q16 : U16;
    qt_idx : Natural;
    high_prec : Boolean;
  begin
    multi_tables :
    loop
      Get_Byte (image.buffer, b);
      remaining := remaining - 1;
      high_prec := b >= 8;
      qt_idx := Natural (b and 7);
      if some_trace then
        Put_Line ("Quantization Table (QT) #" & U8'Image (b));
      end if;
      for i in QT'Range loop
        if high_prec then
          Big_endian (image.buffer, q16);
          remaining := remaining - 2;
          image.JPEG_stuff.qt_list (qt_idx)(i) := Natural (q16);
        else
          Get_Byte (image.buffer, q8);
          remaining := remaining - 1;
          image.JPEG_stuff.qt_list (qt_idx)(i) := Natural (q8);
        end if;
      end loop;
      exit multi_tables when remaining <= 0;
    end loop multi_tables;
  end Read_DQT;

  procedure Read_DRI (image : in out Image_descriptor) is
    ri : U16;
  begin
    Big_endian (image.buffer, ri);
    if some_trace then
      Put_Line ("  Restart interval set to:" & U16'Image (ri));
    end if;
    image.JPEG_stuff.restart_interval := Natural (ri);
  end Read_DRI;

  procedure Read_EXIF (image : in out Image_descriptor; data_length : Natural) is
    b, orientation_value : U8;
    x, ifd0_entries : Natural;
    Exif_signature : constant String := "Exif" & ASCII.NUL & ASCII.NUL;
    signature : String (1 .. 6);
    IFD_tag : U16;
    endianness : Character;
    --  'M' (Motorola) or 'I' (Intel): EXIF chunks may have different endiannesses,
    --  even though the whole JPEG format has a fixed endianness!
  begin
    if some_trace then
      Put_Line ("APP1");
    end if;
    if data_length < 6 then
      --  Skip segment data
      for i in 1 .. data_length loop
        Get_Byte (image.buffer, b);
      end loop;
    else
      for i in 1 .. 6 loop
        Get_Byte (image.buffer, b);
        signature (i) := Character'Val (b);
      end loop;
      if signature /= Exif_signature then
        for i in 7 .. data_length loop -- Skip remaining of APP1 data
          Get_Byte (image.buffer, b); -- since we don't know how to use it.
        end loop;
        if some_trace then
          Put_Line ("APP1 is not Exif");
        end if;
        return;
      end if;
      Get_Byte (image.buffer, b); -- TIFF 6.0 header (1st of 8 bytes)
      endianness := Character'Val (b);
      if some_trace then
        Put_Line ("APP1 is Exif; endianness is " & endianness);
      end if;
      for i in 8 .. 14 loop -- TIFF 6.0 header (2-8 of 8 bytes)
        Get_Byte (image.buffer, b);
      end loop;
      --  Number of IFD0 entries (2 bytes)
      ifd0_entries := 0;
      Get_Byte (image.buffer, b);
      ifd0_entries := Natural (b);
      Get_Byte (image.buffer, b);
      if endianness = 'I' then
        ifd0_entries := ifd0_entries + 16#100# * Natural (b);
      else
        ifd0_entries := Natural (b) + 16#100# * ifd0_entries;
      end if;
      if some_trace then
        Put_Line ("EXIF's IFD0 has" & Natural'Image (ifd0_entries) & " entries.");
      end if;
      x := 17;
      while x <= data_length - 12 loop
        Get_Byte (image.buffer, b);
        IFD_tag := U16 (b);
        Get_Byte (image.buffer, b);
        if endianness = 'I' then
          IFD_tag := IFD_tag + 16#100# * U16 (b);
        else
          IFD_tag := U16 (b) + 16#100# * IFD_tag;
        end if;
        if some_trace then
          Put ("IFD tag:"); Ada.Integer_Text_IO.Put (Natural (IFD_tag), Base => 16); New_Line;
        end if;
        for i in 3 .. 8 loop
          Get_Byte (image.buffer, b);
        end loop;
        if endianness = 'I' then
          Get_Byte (image.buffer, orientation_value);
          for i in 10 .. 12 loop
            Get_Byte (image.buffer, b);
          end loop;
        else
          Get_Byte (image.buffer, b);
          Get_Byte (image.buffer, orientation_value);
          Get_Byte (image.buffer, b);
          Get_Byte (image.buffer, b);
        end if;
        x := x + 12;
        if IFD_tag = 16#112# then
          case orientation_value is
            when 1 =>
              image.display_orientation := Unchanged;
            when 8 =>
              image.display_orientation := Rotation_90;
            when 3 =>
              image.display_orientation := Rotation_180;
            when 6 =>
              image.display_orientation := Rotation_270;
            when others =>
              image.display_orientation := Unchanged;
          end case;
          if some_trace then
            Put_Line (
              "IFD tag 0112: Orientation set to: " &
              Orientation'Image (image.display_orientation)
            );
          end if;
          exit;
        end if;
      end loop;
      --  Skip rest of data
      for i in x .. data_length loop
        Get_Byte (image.buffer, b);
      end loop;
    end if;
  end Read_EXIF;

  --------------------
  -- Image decoding --
  --------------------

  procedure Load (image : in out Image_descriptor) is
    --
    --  Bit buffer
    --
    buf : U32 := 0;
    bufbits : Natural := 0;

    function Show_bits (bits : Natural) return Natural is
      newbyte, marker : U8;
    begin
      if bits = 0 then
        return 0;
      end if;
      while bufbits < bits loop
        begin
          Get_Byte (image.buffer, newbyte);
          bufbits := bufbits + 8;
          buf := buf * 256 + U32 (newbyte);
          if newbyte = 16#FF# then
            Get_Byte (image.buffer, marker);
            case marker is
              when 0 =>
                null;
              when 16#D8# =>  --  SOI here ?
                null;
                --  2015-04-26: occured in one (of many) picture
                --  taken by an Olympus VG120,D705. See test/img/bcase_1.jpg
              when 16#D9# =>  --  EOI here ?
                null; -- !! signal end
              when 16#D0# .. 16#D7# =>
                bufbits := bufbits + 8;
                buf := buf * 256 + U32 (marker);
              when others =>
                raise error_in_image_data with
                  "JPEG: Invalid code (bit buffer): " & U8'Image (marker);
            end case;
          end if;
        exception
          when Ada.IO_Exceptions.End_Error =>
            newbyte := 16#FF#;
            bufbits := bufbits + 8;
            buf := buf * 256 + U32 (newbyte);
        end;
      end loop;
      return Natural (
          Shift_Right (buf, bufbits - bits)
        and
          (Shift_Left (1, bits) - 1)
      );
    end Show_bits;

    procedure Skip_bits (bits : Natural) is
    pragma Inline (Skip_bits);
      dummy : Integer;
      pragma Unreferenced (dummy);
    begin
      if bufbits < bits then
        dummy := Show_bits (bits);
      end if;
      bufbits := bufbits - bits;
    end Skip_bits;

    function Get_bits (bits : Natural) return Integer is
    pragma Inline (Get_bits);
      res : constant Integer := Show_bits (bits);
    begin
      Skip_bits (bits);
      return res;
    end Get_bits;

    --

    type Info_per_component_B is record
      ht_idx_AC : Natural;
      ht_idx_DC : Natural;
      width, height, stride : Natural;
      dcpred : Integer := 0;
    end record;

    info_A : Component_Info_A renames image.JPEG_stuff.info;
    info_B : array (Component) of Info_per_component_B;

    procedure Get_VLC (
      vlc : VLC_table;
      code : out U8;
      value_ret : out Integer
    )
    is
      --  Step 1 happens here: Huffman decompression
      value : Integer := Show_bits (16);
      bits : Natural := Natural (vlc (value).bits);
    begin
      if bits = 0 then
        raise error_in_image_data with "JPEG: VLC table: bits = 0";
      end if;
      Skip_bits (bits);
      value := Integer (vlc (value).code);
      code := U8 (value);
      bits := Natural (U32 (value) and 15);
      value_ret := 0;
      if bits /= 0 then
        value := Get_bits (bits);
        if value < Integer (Shift_Left (U32'(1), bits - 1)) then
          value := value + 1 - Integer (Shift_Left (U32'(1), bits));
        end if;
        value_ret := value;
      end if;
    end Get_VLC;

    function Clip (x : Integer) return Integer is
    pragma Inline (Clip);
    begin
      if x < 0 then
        return 0;
      elsif x > 255 then
        return 255;
      else
        return x;
      end if;
    end Clip;

    type Block_8x8 is array (0 .. 63) of Integer;

    --  Ordering within a 8x8 block, in zig-zag
    zig_zag : constant Block_8x8 :=
     (0,  1,  8, 16,  9,  2,  3, 10, 17, 24, 32, 25, 18,
      11,  4,  5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20,
      13,  6,  7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43,
      36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
      38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63);

    procedure Decode_Block (c : Component; block : in out Block_8x8) is
      value, coef : Integer;
      code : U8;
      qt_local : JPEG_Defs.QT renames image.JPEG_stuff.qt_list (info_A (c).qt_assoc);
      --
      W1 : constant := 2841;
      W2 : constant := 2676;
      W3 : constant := 2408;
      W5 : constant := 1609;
      W6 : constant := 1108;
      W7 : constant :=  565;
      --
      procedure Row_IDCT (start : Integer) is
      pragma Inline (Row_IDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val : Integer;
      begin
        x1 := block (start + 4) * 2**11;
        x2 := block (start + 6);
        x3 := block (start + 2);
        x4 := block (start + 1);
        x5 := block (start + 7);
        x6 := block (start + 5);
        x7 := block (start + 3);
        if x1 = 0 and x2 = 0 and x3 = 0 and x4 = 0 and x5 = 0 and x6 = 0 and x7 = 0 then
          val := block (start + 0) * 8;
          block (start + 0 .. start + 7) := (others => val);
        else
          x0 := (block (start + 0) * 2**11) + 128;
          x8 := W7 * (x4 + x5);
          x4 := x8 + (W1 - W7) * x4;
          x5 := x8 - (W1 + W7) * x5;
          x8 := W3 * (x6 + x7);
          x6 := x8 - (W3 - W5) * x6;
          x7 := x8 - (W3 + W5) * x7;
          x8 := x0 + x1;
          x0 := x0 - x1;
          x1 := W6 * (x3 + x2);
          x2 := x1 - (W2 + W6) * x2;
          x3 := x1 + (W2 - W6) * x3;
          x1 := x4 + x6;
          x4 := x4 - x6;
          x6 := x5 + x7;
          x5 := x5 - x7;
          x7 := x8 + x3;
          x8 := x8 - x3;
          x3 := x0 + x2;
          x0 := x0 - x2;
          x2 := (181 * (x4 + x5) + 128) / 256;
          x4 := (181 * (x4 - x5) + 128) / 256;
          block (start + 0) := (x7 + x1) / 256;
          block (start + 1) := (x3 + x2) / 256;
          block (start + 2) := (x0 + x4) / 256;
          block (start + 3) := (x8 + x6) / 256;
          block (start + 4) := (x8 - x6) / 256;
          block (start + 5) := (x0 - x4) / 256;
          block (start + 6) := (x3 - x2) / 256;
          block (start + 7) := (x7 - x1) / 256;
        end if;
      end Row_IDCT;

      procedure Col_IDCT (start : Integer) is
      pragma Inline (Col_IDCT);
        x0, x1, x2, x3, x4, x5, x6, x7, x8, val : Integer;
      begin
        x1 := block (start + 8 * 4) * 256;
        x2 := block (start + 8 * 6);
        x3 := block (start + 8 * 2);
        x4 := block (start + 8 * 1);
        x5 := block (start + 8 * 7);
        x6 := block (start + 8 * 5);
        x7 := block (start + 8 * 3);
        if x1 = 0 and x2 = 0 and x3 = 0 and x4 = 0 and x5 = 0 and x6 = 0 and x7 = 0 then
          val := Clip (((block (start) + 32) / 2**6) + 128);
          for row in reverse 0 .. 7 loop
            block (start + row * 8) := val;
          end loop;
        else
          x0 := (block (start) * 256) + 8192;
          x8 := W7 * (x4 + x5) + 4;
          x4 := (x8 + (W1 - W7) * x4) / 8;
          x5 := (x8 - (W1 + W7) * x5) / 8;
          x8 := W3 * (x6 + x7) + 4;
          x6 := (x8 - (W3 - W5) * x6) / 8;
          x7 := (x8 - (W3 + W5) * x7) / 8;
          x8 := x0 + x1;
          x0 := x0 - x1;
          x1 := W6 * (x3 + x2) + 4;
          x2 := (x1 - (W2 + W6) * x2) / 8;
          x3 := (x1 + (W2 - W6) * x3) / 8;
          x1 := x4 + x6;
          x4 := x4 - x6;
          x6 := x5 + x7;
          x5 := x5 - x7;
          x7 := x8 + x3;
          x8 := x8 - x3;
          x3 := x0 + x2;
          x0 := x0 - x2;
          x2 := (181 * (x4 + x5) + 128) / 256;
          x4 := (181 * (x4 - x5) + 128) / 256;
          block (start + 8 * 0) := Clip (((x7 + x1) / 2**14) + 128);
          block (start + 8 * 1) := Clip (((x3 + x2) / 2**14) + 128);
          block (start + 8 * 2) := Clip (((x0 + x4) / 2**14) + 128);
          block (start + 8 * 3) := Clip (((x8 + x6) / 2**14) + 128);
          block (start + 8 * 4) := Clip (((x8 - x6) / 2**14) + 128);
          block (start + 8 * 5) := Clip (((x0 - x4) / 2**14) + 128);
          block (start + 8 * 6) := Clip (((x3 - x2) / 2**14) + 128);
          block (start + 8 * 7) := Clip (((x7 - x1) / 2**14) + 128);
        end if;
      end Col_IDCT;

    begin -- Decode_Block
      --
      --  Step 2 happens here: Inverse quantization
      Get_VLC (image.JPEG_stuff.vlc_defs (DC, info_B (c).ht_idx_DC).all, code, value);
      --  First value in block (0: top left) uses a predictor.
      info_B (c).dcpred := info_B (c).dcpred + value;
      block := (0 => info_B (c).dcpred * qt_local (0), others => 0);
      coef := 0;
      loop
        Get_VLC (image.JPEG_stuff.vlc_defs (AC, info_B (c).ht_idx_AC).all, code, value);
        exit when code = 0; -- EOB
        if (code and 16#0F#) = 0 and code /= 16#F0# then
          raise error_in_image_data with "JPEG: error in VLC AC code for de-quantization";
        end if;
        coef := coef + Integer (Shift_Right (code, 4)) + 1;
        if coef > 63 then
          raise error_in_image_data with "JPEG: coefficient for de-quantization is > 63";
        end if;
        block (zig_zag (coef)) := value * qt_local (coef);
        exit when coef = 63;
      end loop;
      --  Step 3 happens here: Inverse cosine transform
      for row in 0 .. 7 loop
        Row_IDCT (row * 8);
      end loop;
      for column in 0 .. 7 loop
        Col_IDCT (column);
      end loop;
    end Decode_Block;

    type Macro_block is array (
      Component range <>, -- component
      Positive range <>,  -- x sample range
      Positive range <>   -- y sample range
    ) of Block_8x8;

      procedure Out_Pixel_8 (br, bg, bb : U8) is
      pragma Inline (Out_Pixel_8);
        function Times_257 (x : Primary_color_range) return Primary_color_range is
        pragma Inline (Times_257);
        begin
          return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
          --  Numbers 8-bit -> no OA warning at instanciation. Returns x if type Primary_color_range is mod 2**8.
        end Times_257;
        full_opaque : constant Primary_color_range := Primary_color_range'Last;
      begin
        case Primary_color_range'Modulus is
          when 256 =>
            Put_Pixel (
              Primary_color_range (br),
              Primary_color_range (bg),
              Primary_color_range (bb),
              full_opaque
            );
          when 65_536 =>
            Put_Pixel (
              Times_257 (Primary_color_range (br)),
              Times_257 (Primary_color_range (bg)),
              Times_257 (Primary_color_range (bb)),
              full_opaque
              --  Times_257 makes max intensity FF go to FFFF
            );
          when others =>
            raise invalid_primary_color_range with "JPEG: color range not supported";
        end case;
      end Out_Pixel_8;

    --  !! might be generic parameters
    ssxmax : constant Natural := image.JPEG_stuff.max_samples_hor;
    ssymax : constant Natural := image.JPEG_stuff.max_samples_ver;

    procedure Upsampling_and_output (
      m : Macro_block;
      x0, y0 : Natural
    )
    is
      flat : array (Component, 0 .. 8 * ssxmax - 1, 0 .. 8 * ssymax - 1) of Integer;

      generic
        color_space : Supported_color_space;
      procedure Color_transformation_and_output;
      --
      procedure Color_transformation_and_output is
        y_val, cb_val, cr_val, c_val, m_val, w_val : Integer;
        y_val_8 : U8;
      begin
        for ymb in flat'Range (3) loop
          exit when y0 + ymb >= Integer (image.height);
          Set_X_Y (x0, Integer (image.height) - 1 - (y0 + ymb));
          for xmb in flat'Range (2) loop
            exit when x0 + xmb >= Integer (image.width);
            case color_space is
              when YCbCr =>
                y_val := flat (Y,  xmb, ymb) * 256;
                cb_val := flat (Cb, xmb, ymb) - 128;
                cr_val := flat (Cr, xmb, ymb) - 128;
                Out_Pixel_8 (
                  br => U8 (Clip ((y_val                + 359 * cr_val + 128) / 256)),
                  bg => U8 (Clip ((y_val -  88 * cb_val - 183 * cr_val + 128) / 256)),
                  bb => U8 (Clip ((y_val + 454 * cb_val                + 128) / 256))
                );
              when Y_Grey =>
                y_val_8 := U8 (flat (Y,  xmb, ymb));
                Out_Pixel_8 (y_val_8, y_val_8, y_val_8);
              when CMYK =>
                --  !! find a working conversion formula.
                --     perhaps it is more complicated (APP_2
                --     color profile must be used ?)
                c_val := flat (Y,  xmb, ymb);
                m_val := flat (Cb, xmb, ymb);
                y_val := flat (Cr, xmb, ymb);
                w_val := flat (I,  xmb, ymb) - 255;
                Out_Pixel_8 (
                  br => U8 (255 - Clip (c_val + w_val)),
                  bg => U8 (255 - Clip (m_val + w_val)),
                  bb => U8 (255 - Clip (y_val + w_val))
                );
            end case;
          end loop;
        end loop;
      end Color_transformation_and_output;
      --
      procedure Ct_YCbCr  is new Color_transformation_and_output (YCbCr);
      procedure Ct_Y_Grey is new Color_transformation_and_output (Y_Grey);
      procedure Ct_CMYK   is new Color_transformation_and_output (CMYK);

      blk_idx : Integer;
      upsx, upsy : Natural;
    begin
      --  Step 4 happens here: Upsampling
      for c in Component loop
        if image.JPEG_stuff.components (c) then
          upsx := info_A (c).up_factor_x;
          upsy := info_A (c).up_factor_y;
          for x in reverse 1 .. info_A (c).samples_hor loop
            for y in reverse 1 .. info_A (c).samples_ver loop
              --  We are at the 8x8 block level
              blk_idx := 63;
              for y8 in reverse 0 .. 7 loop
                for x8 in reverse 0 .. 7 loop
                  declare
                    val : constant Integer := m (c, x, y)(blk_idx);
                    big_pixel_x : constant Natural := upsx * (x8 + 8 * (x - 1));
                    big_pixel_y : constant Natural := upsy * (y8 + 8 * (y - 1));
                  begin
                    --  Repeat pixels for component c, sample (x,y),
                    --  position (x8,y8).
                    for rx in reverse 0 .. upsx - 1 loop
                      for ry in reverse 0 .. upsy - 1 loop
                        flat (c, rx + big_pixel_x, ry + big_pixel_y) := val;
                      end loop;
                    end loop;
                  end;
                  blk_idx := blk_idx - 1;
                end loop;
              end loop;
            end loop;
          end loop;
        end if;
      end loop;
      --  Step 5 and 6 happen here: Color transformation and output
      case image.JPEG_stuff.color_space is
        when YCbCr =>
          Ct_YCbCr;
        when Y_Grey =>
          Ct_Y_Grey;
        when CMYK =>
          Ct_CMYK;
      end case;
    end Upsampling_and_output;

    --  Start Of Scan (and image data which follow)
    --
    procedure Read_SOS is
      components, b, id_base : U8;
      compo : Component := Component'First;
      mbx, mby : Natural := 0;
      mbsizex, mbsizey, mbwidth, mbheight : Natural;
      rstcount : Natural := image.JPEG_stuff.restart_interval;
      nextrst : U16 := 0;
      w : U16;
      start_spectral_selection,
      end_spectral_selection,
      successive_approximation : U8;
    begin
      Get_Byte (image.buffer, components);
      if some_trace then
        Put_Line (
          "Start of Scan (SOS), with" & U8'Image (components) & " components"
        );
      end if;
      if image.subformat_id /= Natural (components) then
        raise error_in_image_data with "JPEG: components mismatch in Scan segment";
      end if;
      id_base := 1;
      for i in 1 .. components loop
        Get_Byte (image.buffer, b);
        if b = 0 then
          --  Workaround for bugged encoder (see above)
          id_base := 0;
        end if;
        if b - id_base > Component'Pos (Component'Last) then
          raise error_in_image_data with "JPEG: Scan: invalid ID: " & U8'Image (b);
        end if;
        compo := Component'Val (b - id_base);
        if not image.JPEG_stuff.components (compo) then
          raise error_in_image_data with
            "JPEG: component " & Component'Image (compo) &
            " has not been defined in the header (SOF) segment";
        end if;
        --  Huffman table selection
        Get_Byte (image.buffer, b);
        info_B (compo).ht_idx_AC := Natural (b mod 16);
        info_B (compo).ht_idx_DC := Natural (b  /  16);
      end loop;
      --  Parameters for progressive display format (SOF_2)
      Get_Byte (image.buffer, start_spectral_selection);
      Get_Byte (image.buffer, end_spectral_selection);
      Get_Byte (image.buffer, successive_approximation);
      --
      --  End of SOS segment, image data follow.
      --
      mbsizex := ssxmax * 8; -- pixels in a row of a macro-block
      mbsizey := ssymax * 8; -- pixels in a column of a macro-block
      mbwidth  := (Integer (image.width)  + mbsizex - 1) / mbsizex;
      --  width in macro-blocks
      mbheight := (Integer (image.height) + mbsizey - 1) / mbsizey;
      --  height in macro-blocks
      if some_trace then
        Put_Line (" mbsizex = " & Integer'Image (mbsizex));
        Put_Line (" mbsizey = " & Integer'Image (mbsizey));
        Put_Line (" mbwidth  = " & Integer'Image (mbwidth));
        Put_Line (" mbheight = " & Integer'Image (mbheight));
      end if;
      for c in Component loop
        if image.JPEG_stuff.components (c) then
          info_B (c).width := (Integer (image.width)  * info_A (c).samples_hor + ssxmax - 1) / ssxmax;
          info_B (c).height := (Integer (image.height) * info_A (c).samples_ver + ssymax - 1) / ssymax;
          info_B (c).stride := (mbwidth * mbsizex * info_A (c).samples_hor) / ssxmax;
          if some_trace then
            Put_Line ("  Details for component " & Component'Image (c));
            Put_Line ("    samples in x " & Integer'Image (info_A (c).samples_hor));
            Put_Line ("    samples in y " & Integer'Image (info_A (c).samples_ver));
            Put_Line ("    width " & Integer'Image (info_B (c).width));
            Put_Line ("    height " & Integer'Image (info_B (c).height));
            Put_Line ("    stride " & Integer'Image (info_B (c).stride));
            Put_Line (
              "    AC/DC table index " &
              Integer'Image (info_B (compo).ht_idx_AC) & ", " &
              Integer'Image (info_B (compo).ht_idx_DC)
            );
          end if;
          if (info_B (c).width < 3 and info_A (c).samples_hor /= ssxmax) or
             (info_B (c).height < 3 and info_A (c).samples_ver /= ssymax)
          then
            raise error_in_image_data with
              "JPEG: component " & Component'Image (c) & ": sample dimension mismatch";
          end if;
        end if;
      end loop;
      --
      if image.interlaced then
        raise unsupported_image_subformat
          with "JPEG: progressive format not yet functional";
      end if;
      declare
        mb : Macro_block (Component, 1 .. ssxmax, 1 .. ssymax);
        x0, y0 : Integer := 0;
      begin
        macro_blocks_loop :
        loop
          components_loop :
          for c in Component loop
            if image.JPEG_stuff.components (c) then
              samples_y_loop :
              for sby in 1 .. info_A (c).samples_ver loop
                samples_x_loop :
                for sbx in 1 .. info_A (c).samples_hor loop
                  Decode_Block (c, mb (c, sbx, sby));
                end loop samples_x_loop;
              end loop samples_y_loop;
            end if;
          end loop components_loop;
          --  All components of the current macro-block are decoded.
          --  Step 4, 5, 6 happen here: Upsampling, color transformation, output
          Upsampling_and_output (mb, x0, y0);
          --
          mbx := mbx + 1;
          x0 := x0 + ssxmax * 8;
          if mbx >= mbwidth then
            mbx := 0;
            x0 := 0;
            mby := mby + 1;
            y0 := y0 + ssymax * 8;
            Feedback ((100 * mby) / mbheight);
            exit macro_blocks_loop when mby >= mbheight;
          end if;
          if image.JPEG_stuff.restart_interval > 0 then
            rstcount := rstcount - 1;
            if rstcount = 0 then
              --  Here begins the restart.
              bufbits := Natural (U32 (bufbits) and 16#F8#); -- byte alignment
              --  Now the restart marker. We expect a
              w := U16 (Get_bits (16));
              if some_trace then
                Put_Line (
                  "  Restart #" & U16'Image (nextrst) &
                  "  Code " & U16'Image (w) &
                  " after" & Natural'Image (image.JPEG_stuff.restart_interval) &
                  " macro blocks"
                );
              end if;
              if w not in 16#FFD0# .. 16#FFD7# or (w and 7) /= nextrst then
                raise error_in_image_data with
                  "JPEG: expected RST (restart) marker Nb " & U16'Image (nextrst);
              end if;
              nextrst := (nextrst + 1) and 7;
              rstcount := image.JPEG_stuff.restart_interval;
              --  Block-to-block predictor variables are reset.
              for c in Component loop
                info_B (c).dcpred := 0;
              end loop;
            end if;
          end if;
        end loop macro_blocks_loop;
      end;
    end Read_SOS;

    --
    sh : Segment_head;
    b : U8;
  begin  --  Load
    loop
      Read (image, sh);
      case sh.kind is
        when DQT =>  --  Quantization Table
          Read_DQT (image, Natural (sh.length));
        when DHT =>  --  Huffman Table
          Read_DHT (image, Natural (sh.length));
        when DRI =>  --  Restart Interval
          Read_DRI (image);
        when EOI =>  --  End Of Input
          exit;
        when SOS =>  --  Start Of Scan
          Read_SOS;
          exit;
        when others =>
          --  Skip segment data
          for i in 1 .. sh.length loop
            Get_Byte (image.buffer, b);
          end loop;
      end case;
    end loop;
  end Load;

end GID.Decoding_JPG;
