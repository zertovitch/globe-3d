--
-- Input:
--   TGA : Orig. author is Nate Miller (tga.c, 7-Aug-1999), vandals1@home.com
--   BMP : from Gautier's SVGA.IO package
--
-- Output:
--   BMP : from http://wiki.delphigl.com/index.php/Screenshot (Delphi)
--   AVI : from specification, plus re-using the raw bitmap output from BMP

with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System;

package body GL.IO is

  use Ada.Streams.Stream_IO;

  type U8  is mod 2 ** 8;   for U8'Size  use 8;
  type U16 is mod 2 ** 16;  for U16'Size use 16;
  type U32 is mod 2 ** 32;  for U32'Size use 32;

  type I32 is range -2 ** 31 .. 2 ** 31 - 1; for I32'Size use 32;

  not_yet_implemented : exception;

  function to_greyscale_Pixels (the_Image : in Image) return Byte_grid
  is
     the_Grid : Byte_Grid (1 .. the_Image.Height, 1 .. the_Image.Width);
  begin
     case the_Image.tex_pixel_Format is
        when GL.LUMINANCE =>

           for Row in the_Grid'range (1) loop
              for Col in the_Grid'range (2) loop
                 the_Grid (Row, Col) := the_Image.Data (the_Image.Width * (Row - 1) + Col - 1);
              end loop;
           end loop;

        when others =>
           raise not_yet_implemented;       -- tbd: do these
     end case;
     return the_Grid;
  end;

  procedure Insert_into_GL(
              id             : Integer;
              size           : Integer;
              width          : Integer;
              height         : Integer;
              texFormat      : TexFormatEnm;
              texPixelFormat : TexPixelFormatEnm;
              image_p        : Byte_array_ptr
            ) is
    ptr: constant GL.Pointer:= image_p(0)'Access;
  begin
    BindTexture ( TEXTURE_2D, Uint(id) );
    PixelStore ( UNPACK_ALIGNMENT, 1 );
    TexParameter ( TEXTURE_2D, TEXTURE_WRAP_S, REPEAT );
    TexParameter ( TEXTURE_2D, TEXTURE_WRAP_T, REPEAT );
    -- TexParameter (TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST);
    TexParameter ( TEXTURE_2D, TEXTURE_MAG_FILTER, LINEAR);
    -- TexParameter (TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST);
    TexParameter ( TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR);
    TexEnv ( TEXTURE_ENV, TEXTURE_ENV_MODE, MODULATE );
    TexImage2D ( TEXTURE_2D, 0, texFormat, Sizei( width ),
                 Sizei( height ), 0, texPixelFormat, GL_UNSIGNED_BYTE,
                 ptr);
  end Insert_into_GL;


  -- Workaround for the severe xxx'Read xxx'Write performance
  -- problems in the GNAT and ObjectAda compilers (as in 2009)
  -- This is possible if and only if Byte = Stream_Element and
  -- arrays types are both packed the same way.
  --
  subtype Size_test_a is Byte_Array(1..16);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..16);
  workaround_possible: constant Boolean:= Size_test_a'Size = Size_test_b'Size;
  --

  procedure Attach_Stream(
    b   : out Input_buffer;
    stm : in Ada.Streams.Stream_IO.Stream_Access
  )
  is
  begin
    b.stm:= stm;
  end Attach_Stream;

  procedure Fill_Buffer(b: in out Input_buffer)
  is
    --
    procedure BlockRead(
      buffer       :    out Byte_Array;
      actually_read:    out Natural
    )
    is
      use Ada.Streams;
      Last_Read: Stream_Element_Offset;
    begin
      if workaround_possible then
        declare
          SE_Buffer: Stream_Element_Array (1 .. buffer'Length);
          -- direct mapping: buffer = SE_Buffer
          for SE_Buffer'Address use buffer'Address;
          pragma Import (Ada, SE_Buffer);
        begin
          Read(b.stm.all, SE_Buffer, Last_Read);
        end;
      else
        declare
          SE_Buffer: Stream_Element_Array (1 .. buffer'Length);
          -- need to copy array
        begin
          Read(b.stm.all, SE_Buffer, Last_Read);
          for i in buffer'Range loop
            buffer(i):= Ubyte(SE_Buffer(Stream_Element_Offset(i-buffer'First)+SE_buffer'First));
          end loop;
        end;
      end if;
      actually_read:= Natural(Last_Read);
    end BlockRead;
    --
  begin
    BlockRead(
      buffer        => b.data,
      actually_read => b.MaxInBufIdx
    );
    b.InputEoF:= b.MaxInBufIdx = 0;
    b.InBufIdx := 1;
  end Fill_Buffer;

  procedure Get_Byte(buf: in out Input_buffer; by: out UByte) is
    pragma Inline(Get_Byte);
  begin
    if buf.InBufIdx > buf.MaxInBufIdx then
      Fill_Buffer(buf);
      if buf.InputEoF then
        raise End_Error;
      end if;
    end if;
    by:= buf.data(buf.InBufIdx);
    buf.InBufIdx:= buf.InBufIdx + 1;
  end Get_Byte;

  function to_TGA_Image (S : in  Ada.Streams.Stream_IO.Stream_Access      -- Input data stream
                        ) return Image
  is
     the_Image : Image;
     stream_buf: Input_buffer;

     -- Run Length Encoding --
     RLE: Boolean;
     RLE_pixels_remaining: Natural:= 0;
     pix_mem: Byte_array(1..4);
     is_run_packet: Boolean;

     procedure RLE_Pixel(iBits: Integer; pix: out Byte_array) is

       procedure Get_pixel is
       begin
         case iBits is
           when 32 => -- BGRA
             Get_Byte(stream_buf, pix(pix'First+2) );
             Get_Byte(stream_buf, pix(pix'First+1) );
             Get_Byte(stream_buf, pix(pix'First  ) );
             Get_Byte(stream_buf, pix(pix'First+3) );
           when 24 => -- BGR
             Get_Byte(stream_buf, pix(pix'First+2) );
             Get_Byte(stream_buf, pix(pix'First+1) );
             Get_Byte(stream_buf, pix(pix'First  ) );
           when 8  => -- Grey
             Get_Byte(stream_buf, pix(pix'First  ) );
           when others =>
             null;
         end case;
       end Get_pixel;

       tmp: GL.UByte;

     begin --  RLE_Pixel
       if RLE_pixels_remaining = 0 then -- load RLE code
         Get_Byte(stream_buf, tmp );
         Get_pixel;
         RLE_pixels_remaining:= GL.UByte'Pos(tmp and 16#7F#);
         is_run_packet:= (tmp and 16#80#) /= 0;
         if is_run_packet then
           case iBits is
             when 32 =>
               pix_mem(1..4):= pix;
             when 24 =>
               pix_mem(1..3):= pix;
             when 8  =>
               pix_mem(1..1):= pix;
             when others =>
               null;
           end case;
         end if;
       else
         if is_run_packet then
           case iBits is
             when 32 =>
               pix:= pix_mem(1..4);
             when 24 =>
               pix:= pix_mem(1..3);
             when 8  =>
               pix:= pix_mem(1..1);
             when others =>
               null;
           end case;
         else
           Get_pixel;
         end if;
         RLE_pixels_remaining:= RLE_pixels_remaining - 1;
       end if;
     end RLE_Pixel;

     --  =============
     --  getRGBA

     --  Reads in RGBA data for a 32bit image.
     --  =============

     procedure getRGBA ( buffer: out Byte_array ) is
       i: Integer:= buffer'First;
     begin
       if RLE then
         while i <= buffer'Last-3 loop
           RLE_Pixel( 32, buffer(i..i+3) );
           i:= i + 4;
         end loop;
       else
         while i <= buffer'Last-3 loop
           -- TGA is stored in BGRA, make it RGBA
           Get_Byte(stream_buf, buffer(i+2) );
           Get_Byte(stream_buf, buffer(i+1) );
           Get_Byte(stream_buf, buffer(i  ) );
           Get_Byte(stream_buf, buffer(i+3) );
           i:= i + 4;
         end loop;
       end if;
       the_Image.tex_Format      := GL.RGBA;
       the_Image.tex_pixel_Format:= GL.RGBA;
     end getRGBA;

     --  =============
     --  getRGB

     --  Reads in RGB data for a 24bit image.
     --  =============

     procedure getRGB ( buffer: out Byte_array ) is
       i: Integer:= buffer'First;
     begin
       if RLE then
         while i <= buffer'Last-2 loop
           RLE_Pixel( 24, buffer(i..i+2) );
           i:= i + 3;
         end loop;
       else
         while i <= buffer'Last-2 loop
           -- TGA is stored in BGR, make it RGB
           Get_Byte(stream_buf, buffer(i+2) );
           Get_Byte(stream_buf, buffer(i+1) );
           Get_Byte(stream_buf, buffer(i  ) );
           i:= i + 3;
         end loop;
       end if;
       the_Image.tex_Format      := GL.RGB;
       the_Image.tex_pixel_Format:= GL.RGB;
     end getRGB;

     --  =============
     --  getGray

     --  Gets the grayscale image data.  Used as an alpha channel.
     --  =============

     procedure getGray ( buffer: out Byte_array ) is
     begin
       if RLE then
         for b in buffer'range loop
           RLE_Pixel( 8, buffer(b..b) );
         end loop;
       else
         for b in buffer'range loop
           Get_Byte(stream_buf, buffer(b) );
         end loop;
       end if;
       the_Image.tex_Format      := GL.LUMINANCE; -- ALPHA
       the_Image.tex_pixel_Format:= GL.LUMINANCE;
     end getGray;

     --  =============
     --  getData

     --  Gets the image data for the specified bit depth.
     --  =============

     procedure getData ( iBits: Integer; buffer: out Byte_array ) is
     begin
       Attach_Stream(stream_buf, S);
       Fill_Buffer(stream_buf);
       case iBits is
         when 32 =>
           getRGBA(buffer);
           the_Image.blending_hint:= True;
         when 24 =>
           getRGB(buffer);
           the_Image.blending_hint:= False;
         when 8  =>
           getGray(buffer);
           the_Image.blending_hint:= True;
         when others => null;
       end case;
     end getData;

     TGA_type: Byte_Array(0..3);
     info    : Byte_Array(0..5);
     dummy   : Byte_Array(1..8);

     imageBits: Integer;

     image_type: Integer;

  begin -- to_TGA_Image
     Byte_Array'Read( s, TGA_type ); -- read in colormap info and image type
     Byte_Array'Read( s, dummy );    -- seek past the header and useless info
     Byte_Array'Read( s, info );

     if TGA_type(1) /= GL.UByte'Val(0) then
       raise TGA_Unsupported_Image_Type
         with "TGA: palette not supported, please use BMP";
     end if;

     -- Image type:
     --      1=8-bit palette style
     --      2=Direct [A]RGB image
     --      3=grayscale
     --      9=RLE version of Type 1
     --     10=RLE version of Type 2
     --     11=RLE version of Type 3

     image_type:= GL.UByte'Pos(TGA_type(2));
     RLE:= image_Type >= 9;
     if RLE then
       image_type:= image_type - 8;
       RLE_pixels_remaining:= 0;
     end if;
     if image_type /= 2 and image_type /= 3 then
       raise TGA_Unsupported_Image_Type
         with "TGA type =" & Integer'Image(image_type);
     end if;

     the_Image.Width  := GL.UByte'Pos(info(0)) + GL.UByte'Pos(info(1)) * 256;
     the_Image.Height := GL.UByte'Pos(info(2)) + GL.UByte'Pos(info(3)) * 256;
     imageBits   := GL.UByte'Pos(info(4));

     the_Image.size := the_Image.Width * the_Image.Height;

     -- 30-Apr-2006: dimensions not power of two allowed, but discouraged in the docs.
     --
     --  -- make sure dimension is a power of 2
     --  if not (checkSize (imageWidth)  and  checkSize (imageHeight)) then
     --     raise TGA_BAD_DIMENSION;
     --  end if;

     -- make sure we are loading a supported TGA_type
     if imageBits /= 32 and imageBits /= 24 and imageBits /= 8 then
        raise TGA_Unsupported_BITS_per_pixel;
     end if;

     -- Allocation
     the_Image.Data:= new Byte_array(0..(imagebits/8)*the_Image.size-1);
     getData (imageBits, the_Image.Data.all);

     return the_Image;
  end to_TGA_Image;


  function to_TGA_Image (Filename : in  String      -- Input data filename
                        ) return Image
  is
     f: File_Type;
     the_Image : Image;
  begin
     begin
        Open(f,in_file,Filename);
     exception
        when name_error => raise_exception(FILE_NOT_FOUND'Identity, " file name:" & Filename);
     end;
     the_Image := to_TGA_Image ( Stream(f) );
     Close(f);
     return the_Image;
  exception
     when e:others =>
        Close(f);
        raise_exception(Exception_Identity(e), " file name:" & Filename);
        return the_Image;
  end to_TGA_Image;



  --  =============
  --  loadTGA

  --  Loads up a targa stream.
  --  Supported types are 8,24 and 32 uncompressed images.
  --  id is the texture ID to bind to.
  --  =============

  procedure Load_TGA (
        S            : in  Ada.Streams.Stream_IO.Stream_Access;
        -- Input data stream
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  )
  is
    the_Image : Image := to_tga_Image (S);
  begin
    Insert_into_GL(
              id             => id,
              size           => the_Image.size,
              width          => the_Image.Width,
              height         => the_Image.Height,
              texFormat      => the_Image.tex_Format,
              texPixelFormat => the_Image.tex_pixel_Format,
              image_p        => the_Image.Data
    );

    -- release our data, its been uploaded to the GL system
    Free( the_Image.Data );

    blending_hint := the_Image.blending_hint;
  end Load_TGA;

  -- Template for all loaders from a file
  generic
    with procedure Stream_Loader( S: Stream_Access; id: Integer; blending_hint: out Boolean );
  procedure Load_XXX ( name: String; id: Integer; blending_hint: out Boolean );

  procedure Load_XXX ( name: String; id: Integer; blending_hint: out Boolean ) is
    f: File_Type;
  begin
    begin
      Open(f,in_file,name);
    exception
      when name_error => raise_exception(FILE_NOT_FOUND'Identity, " file name:" & name);
    end;
    Stream_Loader( Stream(f), id, blending_hint );
    Close(f);
  exception
    when e:others =>
      Close(f);
      raise_exception(Exception_Identity(e), " file name:" & name);
  end Load_XXX;

  procedure i_Load_TGA is new Load_XXX( Stream_Loader => Load_TGA );

  procedure Load_TGA ( Name: String; Id: Integer; blending_hint: out Boolean ) renames i_Load_TGA;

  -- BMP

  procedure Load_BMP (
        S            : in  Ada.Streams.Stream_IO.Stream_Access;
        -- Input data stream
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  )
  is

    imageData: Byte_Array_Ptr:= null;
    stream_buf: Input_buffer;

    subtype Y_Loc is Natural range 0 .. 4095;
    subtype X_Loc is Natural range 0 .. 4095;

    -- 256-col types

    subtype Color_Type is GL.UByte;
    subtype Color_Value is Color_Type range 0 .. 63;

    type RGB_Color is
       record
          Red   : Color_Value;
          Green : Color_Value;
          Blue  : Color_Value;
       end record;

    type Color_Palette is array (Color_Type) of RGB_Color;

    Palette : Color_Palette;

    ----------------------------------------------------
    -- BMP format I/O                                 --
    --                                                --
    -- Rev 1.5  10-May-2006 GdM: added 4-bit support  --
    -- Rev 1.4  11/02/99    RBS                       --
    --                                                --
    ----------------------------------------------------
    -- Coded by G. de Montmollin

    -- Code additions, changes, and corrections by Bob Sutton
    --
    -- Remarks expanded and altered
    -- Provided for scanline padding in data stream
    -- Corrected stream reading for images exceeding screen size.
    -- Provided selectable trim modes for oversize images
    -- Procedures originally Read_BMP_dimensions now Read_BMP_Header
    -- Some exceptions added
    --
    -- Rev 1.2  RBS.  Added variable XY screen location for BMP
    -- Rev 1.3  RBS.  Added image inversion & reversal capability
    -- Rev 1.4  RBS.  Activated LOCATE centering / clipping options
    --
    -- This version presumes that the infile is a new style, 256 color bitmap.
    -- The Bitmap Information Header structure (40 bytes) is presumed
    -- instead of the pre-Windows 3.0 Bitmap Core Header Structure (12 Bytes)
    -- Pos 15 (0EH), if 28H, is valid BIH structure.  If 0CH, is BCH structure.

    procedure Read_BMP_Header (S: Stream_Access;
                               width     : out X_Loc;
                               height    : out Y_Loc;
                               image_bits: out Integer;
                               offset    : out U32 ) is

      fsz: U32;
      ih: U32;
      w, dummy16: U16;
      n: U32;
      Str2:  String(1..2);
      Str4:  String(1..4);
      Str20: String(1..20);

      -- Get numbers with correct trucmuche endian, to ensure
      -- correct header loading on some non-Intel machines

      generic
        type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
      procedure Read_Intel_x86_number(n: out Number);

      procedure Read_Intel_x86_number(n: out Number) is
        b: GL.UByte;
        m: Number:= 1;
      begin
        n:= 0;
        for i in 1..Number'Size/8 loop
          GL.UByte'Read(s,b);
          n:= n + m * Number(b);
          m:= m * 256;
        end loop;
      end Read_Intel_x86_number;

      procedure Read_Intel is new Read_Intel_x86_number( U16 );
      procedure Read_Intel is new Read_Intel_x86_number( U32 );

      begin
        --   First 14 bytes is file header structure.
        --   Pos= 1,  read 2 bytes, file signature word
        String'Read(S, Str2);
        if Str2 /= "BM" then
           raise Not_BMP_format;
        end if;
        --   Pos= 3,  read the file size
        Read_Intel(fsz);
        --   Pos= 7, read four bytes, unknown
        String'Read(S, Str4);
        --   Pos= 11, read four bytes offset, file top to bitmap data.
        --            For 256 colors, this is usually 36 04 00 00
        Read_Intel(offset);
        --   Pos= 15. The beginning of Bitmap information header.
        --   Data expected:  28H, denoting 40 byte header
        Read_Intel(ih);
        --   Pos= 19. Bitmap width, in pixels.  Four bytes
        Read_Intel(n);
        width:=  X_Loc(n);
        --   Pos= 23. Bitmap height, in pixels.  Four bytes
        Read_Intel(n);
        height:= Y_Loc(n);
        --   Pos= 27, skip two bytes.  Data is number of Bitmap planes.
        Read_Intel(dummy16); -- perform the skip
        --   Pos= 29, Number of bits per pixel
        --   Value 8, denoting 256 color, is expected
        Read_Intel(w);
        if w/=8 and w/=4 and w/=1 then
           raise BMP_Unsupported_bits_per_pixel;
        end if;
        image_bits:= Integer(w);
        --   Pos= 31, read four bytees
        Read_Intel(n);                --Type of compression used
        if n /= 0 then
           raise Unsupported_compression;
        end if;

        --   Pos= 35 (23H), skip twenty bytes
        String'Read(S, Str20);     -- perform the skip

        --   Pos= 55 (36H), - start of palette
      end Read_BMP_Header;

     procedure Load_BMP_Palette (S         : Stream_Access;
                                 image_bits: in Integer;
                                 Palette   : out Color_Palette) is
       d: GL.UByte;
       mc: constant Color_Type:= (2**image_bits)-1;
     begin
       for DAC in 0..mc loop
         GL.UByte'Read(S,d); Palette(DAC).Blue  := Color_Value(d / 4);
         GL.UByte'Read(S,d); Palette(DAC).Green := Color_Value(d / 4);
         GL.UByte'Read(S,d); Palette(DAC).Red   := Color_Value(d / 4);
         GL.UByte'Read(S,d);
       end loop;
     end Load_BMP_Palette;

    -- Load image only from stream (after having read header and palette!)

    procedure Load_BMP_Image (S       : Stream_Access;
                              width   : in X_Loc;
                              height  : in Y_Loc;
                              Buffer  : in out Byte_array;
                              BMP_bits: Integer;
                              Palette : Color_Palette) is
       idx: Natural;
       b01, b: GL.UByte:= 0;
       pair: Boolean:= True;
       bit: Natural range 0..7:= 0;
       --
       x3: Natural; -- idx + x*3 (each pixel takes 3 bytes)
       x3_max: Natural;
       --
       procedure Fill_palettized is
         pragma Inline(Fill_palettized);
       begin
         Buffer( x3     ):= UByte(Palette(b).Red  ) * 4;
         Buffer( x3 + 1 ):= UByte(Palette(b).Green) * 4;
         Buffer( x3 + 2 ):= UByte(Palette(b).Blue ) * 4;
       end Fill_palettized;
       --
    begin
      Attach_Stream(stream_buf, S);
      Fill_Buffer(stream_buf);
      for y in 0 .. height-1 loop
        idx:= y * width * 3; -- GL destination picture is 24 bit
        x3:= idx;
        x3_max:= idx + (width-1)*3;
        case BMP_bits is
          when 1 => -- B/W
            while x3 <= x3_max loop
              if bit=0 then
                Get_Byte(stream_buf, b01);
              end if;
              b:= (b01 and 16#80#) / 16#80#;
              Fill_palettized;
              b01:= b01 * 2; -- cannot overflow.
              if bit=7 then
                bit:= 0;
              else
                bit:= bit + 1;
              end if;
              x3:= x3 + 3;
            end loop;
          when 4 => -- 16 colour image
            while x3 <= x3_max loop
              if pair then
                Get_Byte(stream_buf, b01);
                b:= (b01 and 16#F0#) / 16#10#;
              else
                b:= (b01 and 16#0F#);
              end if;
              pair:= not pair;
              Fill_palettized;
              x3:= x3 + 3;
            end loop;
          when 8 => -- 256 colour image
            while x3 <= x3_max loop
              Get_Byte(stream_buf, b);
              Fill_palettized;
              x3:= x3 + 3;
            end loop;
          when others =>
            null;
        end case;
      end loop;
    end Load_BMP_Image;

    width: X_Loc;
    height: Y_Loc;
    offset : U32;
    BMP_bits, imagebits: Integer;
    size: Integer;
    BMP_tex_format      : GL.TexFormatEnm;
    BMP_tex_pixel_format: GL.TexPixelFormatEnm;
  begin
    Read_BMP_Header (S, width, height, BMP_bits, offset);
    imagebits:= 24;
    blending_hint:= False; -- no blending with BMP's
    BMP_tex_format      := GL.RGB;
    BMP_tex_pixel_format:= GL.RGB;
    Load_BMP_Palette(S, BMP_bits, Palette);

    size := Width * Height;

    -- Allocation
    imageData:= new Byte_array(0..(imagebits/8)*size-1);

    Load_BMP_Image
      (S, width, height, imageData.all,
       BMP_bits, Palette);

    Insert_into_GL(
              id             => id,
              size           => size,
              width          => width,
              height         => height,
              texFormat      => BMP_tex_format,
              texPixelFormat => BMP_tex_pixel_format,
              image_p        => imageData
    );

    -- release our data, its been uploaded to the GL system
    Free( imageData );

  end Load_BMP;

  procedure i_Load_BMP is new Load_XXX( Stream_Loader => Load_BMP );
  procedure Load_BMP ( Name: String; Id: Integer; blending_hint: out Boolean ) renames i_Load_BMP;

  procedure Load (
    name  : String;            -- file name
    format: Supported_format;  -- expected file format
    ID    : Integer;           -- ID is the texture identifier to bind to
    blending_hint: out Boolean -- has blending / transparency /alpha ?
  )
  is
  begin
    case format is
      when BMP => Load_BMP(name, ID, blending_hint);
      when TGA => Load_TGA(name, ID, blending_hint);
    end case;
  end Load;

  procedure Load (
    s     : Ada.Streams.Stream_IO.Stream_Access;
                               -- input data stream (e.g. Unzip.Streams)
    format: Supported_format;  -- expected file format
    ID    : Integer;           -- ID is the texture identifier to bind to
    blending_hint: out Boolean -- has blending / transparency /alpha ?
  )
  is
  begin
    case format is
      when BMP => Load_BMP(s, ID, blending_hint);
      when TGA => Load_TGA(s, ID, blending_hint);
    end case;
  end Load;

  -------------
  -- Outputs --
  -------------

  generic
    type Number is mod <>;
    s: Stream_Access;
  procedure Write_Intel_x86_number(n: in Number);

  procedure Write_Intel_x86_number(n: in Number) is
    m: Number:= n;
    bytes: constant Integer:= Number'Size/8;
  begin
    for i in 1..bytes loop
      U8'Write(s, U8(m mod 256));
      m:= m / 256;
    end loop;
  end Write_Intel_x86_number;

  procedure Write_raw_BGR_frame(s: Stream_Access; width, height: Natural) is
    type Temp_bitmap_type is array(Natural range <>) of aliased GL.Ubyte;
    PicData: Temp_bitmap_type(0..(width+4) * (height+4) * 3 - 1);
    -- No dynamic allocation needed!
    -- The "+4" are there to avoid parity address problems when GL writes
    -- to the buffer.
    type loc_pointer is new GL.pointer;
    function Cvt is new Ada.Unchecked_Conversion(System.Address,loc_pointer);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
    pragma No_Strict_Aliasing(loc_pointer); -- recommended by GNAT 2005+
    pPicData: loc_pointer;
    data_max: constant Integer:= width * height * 3 -1;
  begin
    pPicData:= Cvt(PicData(0)'Address);
    GL.ReadPixels(
      0, 0,
      GL.Sizei(width), GL.Sizei(height),
      GL.BGR,
      GL.GL_UNSIGNED_BYTE,
      GL.pointer(pPicData)
    );
    if workaround_possible then
      declare
        use Ada.Streams;
        SE_Buffer   : Stream_Element_Array (0..Stream_Element_Offset(PicData'Last));
        for SE_Buffer'Address use PicData'Address;
        pragma Import (Ada, SE_Buffer);
      begin
        Ada.Streams.Write(s.all, SE_Buffer(0..Stream_Element_Offset(data_max)));
      end;
    else
      Temp_bitmap_type'Write(s, PicData(0..data_max) );
    end if;
  end Write_raw_BGR_frame;

  -------------------------------------------------------
  -- BMP RGB(A) output of the current, active viewport --
  -------------------------------------------------------

  procedure Screenshot( Name: in String ) is
    -- Translated by (New) P2Ada v. 15-Nov-2006
    -- http://wiki.delphigl.com/index.php/Screenshot
    f: Ada.Streams.Stream_IO.File_Type;

    type BITMAPFILEHEADER is record
      bfType     : U16;
      bfSize     : U32;
      bfReserved1: U16;
      bfReserved2: U16;
      bfOffBits  : U32;
    end record;
    pragma Pack(BITMAPFILEHEADER);
    for BITMAPFILEHEADER'Size use 8 * 14;

    type BITMAPINFOHEADER is record
      biSize         : U32;
      biWidth        : I32;
      biHeight       : I32;
      biPlanes       : U16;
      biBitCount     : U16;
      biCompression  : U32;
      biSizeImage    : U32;
      biXPelsPerMeter: I32;
      biYPelsPerMeter: I32;
      biClrUsed      : U32;
      biClrImportant : U32;
    end record;
    pragma Pack(BITMAPINFOHEADER);
    for BITMAPINFOHEADER'Size use 8 * 40;

    FileInfo  : BITMAPINFOHEADER;
    FileHeader: BITMAPFILEHEADER;
    type intPtr is new GL.intPointer;
    Viewport  : array (0 .. 3) of aliased GL.Int;
    function Cvt is new Ada.Unchecked_Conversion(System.Address,intPtr);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
    pragma No_Strict_Aliasing(intPtr); -- recommended by GNAT 2005+
  begin
    --  Größe des Viewports abfragen --> Spätere Bildgrößenangaben
    GL.GetIntegerv(GL.VIEWPORT, GL.intPointer(Cvt(Viewport(0)'Address)) );

    --  Initialisieren der Daten des Headers
    FileHeader.bfType := 16#4D42#; -- 'BM'
    FileHeader.bfOffBits :=
      BITMAPINFOHEADER'Size / 8 +
      BITMAPFILEHEADER'Size / 8;
    FileHeader.bfReserved1:= 0;
    FileHeader.bfReserved2:= 0;

    --  Schreiben der Bitmap-Informationen
    FileInfo.biSize       := BITMAPINFOHEADER'Size / 8;
    FileInfo.biWidth      := I32(Viewport(2));
    FileInfo.biHeight     := I32(Viewport(3));
    FileInfo.biPlanes     := 1;
    FileInfo.biBitCount   := 24;
    FileInfo.biCompression:= 0;
    FileInfo.biSizeImage  :=
      U32(FileInfo.biWidth * FileInfo.biHeight) *
      U32(FileInfo.biBitCount / 8);
    FileInfo.biXPelsPerMeter:= 0;
    FileInfo.biYPelsPerMeter:= 0;
    FileInfo.biClrUsed      := 0;
    FileInfo.biClrImportant := 0;

    --  Größenangabe auch in den Header übernehmen
    FileHeader.bfSize := FileHeader.bfOffBits + FileInfo.biSizeImage;

    --  Und den ganzen Müll in die Datei schieben ;-)
    --  Moderne Leute nehmen dafür auch Streams ...
    Create(f, Out_File, name);
    declare
      procedure Write_Intel is new Write_Intel_x86_number( U16, Stream(f) );
      procedure Write_Intel is new Write_Intel_x86_number( U32, Stream(f) );
      function Cvt is new Ada.Unchecked_Conversion( I32, U32 );
    begin
      -- ** Only for Intel endianess: ** --
      --  BITMAPFILEHEADER'Write(Stream(F), FileHeader);
      --  BITMAPINFOHEADER'Write(Stream(F), FileInfo);
      --
      -- ** Endian-safe: ** --
      Write_Intel(FileHeader.bfType);
      Write_Intel(FileHeader.bfSize);
      Write_Intel(FileHeader.bfReserved1);
      Write_Intel(FileHeader.bfReserved2);
      Write_Intel(FileHeader.bfOffBits);
      --
      Write_Intel(FileInfo.biSize);
      Write_Intel(Cvt(FileInfo.biWidth));
      Write_Intel(Cvt(FileInfo.biHeight));
      Write_Intel(FileInfo.biPlanes);
      Write_Intel(FileInfo.biBitCount);
      Write_Intel(FileInfo.biCompression);
      Write_Intel(FileInfo.biSizeImage);
      Write_Intel(Cvt(FileInfo.biXPelsPerMeter));
      Write_Intel(Cvt(FileInfo.biYPelsPerMeter));
      Write_Intel(FileInfo.biClrUsed);
      Write_Intel(FileInfo.biClrImportant);
      --
      Write_raw_BGR_frame(Stream(f),Integer(Viewport(2)), Integer(Viewport(3)));
      Close(f);
    exception
      when others =>
        Close(f);
        raise;
    end;
  end Screenshot;

  -------------------
  -- Video capture --
  -------------------

  -- Exceptionally we define global variables since it is not expected
  -- that more that one capture is taken at the same time.
  avi: Ada.Streams.Stream_IO.File_Type;
  frames: Natural;
  rate: Positive;
  width, height: Positive;
  bmp_size: U32;

  procedure Write_RIFF_headers is
    -- Written 1st time to take place (but # of frames unknown)
    -- Written 2nd time for setting # of frames, sizes, etc.
    --
    calc_bmp_size: constant U32:= U32(((width)) * height * 3);
    -- !! stuff to multiple of 4 !!
    index_size: constant U32:= U32(frames)*16;
    movie_size: constant U32:= 4 + U32(frames)*(calc_bmp_size+8);
    second_list_size: constant U32:= 4+64+48;
    first_list_size : constant U32:= (4+64) + (8+second_list_size);
    file_size: constant U32:= 8 + (8+first_list_size) + (4+movie_size) + (8+index_size);
    s: constant Stream_Access:= Stream(avi);
    procedure Write_Intel is new Write_Intel_x86_number( U16, s );
    procedure Write_Intel is new Write_Intel_x86_number( U32, s );
    microseconds_per_frame: constant U32:= U32( 1_000_000.0 / Long_Float(rate) );
  begin
    bmp_size:= calc_bmp_size;
    String'Write(s, "RIFF");
    U32'Write(s, file_size);
    String'Write(s, "AVI ");
    String'Write(s, "LIST");
    Write_Intel(first_list_size);
    String'Write(s, "hdrl");
    String'Write(s, "avih");
    Write_Intel(U32'(56));
    -- Begin of AVI Header
    Write_Intel(microseconds_per_frame);
    Write_Intel(U32'(0));  -- MaxBytesPerSec
    Write_Intel(U32'(0));  -- Reserved1
    Write_Intel(U32'(16)); -- Flags (16 = has an index)
    Write_Intel(U32(frames));
    Write_Intel(U32'(0));  -- InitialFrames
    Write_Intel(U32'(1));  -- Streams
    Write_Intel(bmp_size);
    Write_Intel(U32(width));
    Write_Intel(U32(height));
    Write_Intel(U32'(0));  -- Scale
    Write_Intel(U32'(0));  -- Rate
    Write_Intel(U32'(0));  -- Start
    Write_Intel(U32'(0));  -- Length
    -- End of AVI Header
    String'Write(s, "LIST");
    Write_Intel(second_list_size);
    String'Write(s, "strl");
    -- Begin of Str
    String'Write(s, "strh");
    Write_Intel(U32'(56));
    String'Write(s, "vids");
    String'Write(s, "DIB ");
    Write_Intel(U32'(0));     -- flags
    Write_Intel(U32'(0));     -- priority
    Write_Intel(U32'(0));     -- initial frames
    Write_Intel(microseconds_per_frame); -- Scale
    Write_Intel(U32'(1_000_000));        -- Rate
    Write_Intel(U32'(0));                -- Start
    Write_Intel(U32(frames));            -- Length
    Write_Intel(bmp_size);               -- SuggestedBufferSize
    Write_Intel(U32'(0));                -- Quality
    Write_Intel(U32'(0));                -- SampleSize
    Write_Intel(U32'(0));
    Write_Intel(U16(width));
    Write_Intel(U16(height));
    -- End of Str
    String'Write(s, "strf");
    Write_Intel( U32'(40) );
    -- Begin of BMI
    Write_Intel(U32'(40));    -- BM header size (like BMP)
    Write_Intel(U32(width));
    Write_Intel(U32(height));
    Write_Intel(U16'(1));     -- Planes
    Write_Intel(U16'(24));    -- BitCount
    Write_Intel(U32'(0));     -- Compression
    Write_Intel(bmp_size);    -- SizeImage
    Write_Intel(U32'(3780));  -- XPelsPerMeter
    Write_Intel(U32'(3780));  -- YPelsPerMeter
    Write_Intel(U32'(0));     -- ClrUsed
    Write_Intel(U32'(0));     -- ClrImportant
    -- End of BMI
    String'Write(s, "LIST");
    Write_Intel(movie_size);
    String'Write(s, "movi");
  end Write_RIFF_headers;

  procedure Start_capture( AVI_name: String; frame_rate: Positive ) is
    type intPtr is new GL.intPointer;
    Viewport  : array (0 .. 3) of aliased GL.Int;
    function Cvt is new Ada.Unchecked_Conversion(System.Address,intPtr);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
    pragma No_Strict_Aliasing(intPtr); -- recommended by GNAT 2005+
  begin
    Create(avi, Out_File, AVI_name);
    frames:= 0;
    rate:= frame_rate;
    GL.GetIntegerv(GL.VIEWPORT, GL.intPointer(Cvt(Viewport(0)'Address)) );
    width := Positive(Viewport(2));
    height:= Positive(Viewport(3));
    -- NB: GL viewport resizing should be blocked during the video capture!
    Write_RIFF_headers;
  end Start_capture;

  procedure Capture_frame is
    s: constant Stream_Access:= Stream(avi);
    procedure Write_Intel is new Write_Intel_x86_number( U32, s );
  begin
    String'Write(s, "00db");
    Write_Intel(bmp_size);
    Write_raw_BGR_frame(s,width,height);
    frames:= frames + 1;
  end Capture_frame;

  procedure Stop_capture is
    index_size: constant U32:= U32(frames)*16;
    s: constant Stream_Access:= Stream(avi);
    procedure Write_Intel is new Write_Intel_x86_number( U32, s );
    ChunkOffset: U32:= 4;
  begin
    -- write the index section
    String'Write(s, "idx1");
    --
    Write_Intel(index_size);
    for f in 1..frames loop
      String'Write(s, "00db");
      Write_Intel(U32'(16)); -- keyframe
      Write_Intel(ChunkOffset);
      ChunkOffset:= ChunkOffset + bmp_size + 8;
      Write_Intel(bmp_size);
    end loop;
    -- Go back to file beginning...
    Set_Index(avi, 1);
    Write_RIFF_headers; -- rewrite headers with correct data
    Close(avi);
  end Stop_capture;

end GL.IO;
