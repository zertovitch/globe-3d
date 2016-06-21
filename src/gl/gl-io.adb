--
-- Input:
--   Uses GID, the Generic Image Decoder ( http://gen-img-dec.sourceforge.net/ )
--
-- Output:
--   BMP : from http://wiki.delphigl.com/index.php/Screenshot (Delphi)
--   AVI : from specification, plus re-using the raw bitmap output from BMP

with GID;

with Ada.Calendar;
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

  function To_greyscale_pixels (the_Image : in Image) return Byte_grid
  is
     the_Grid : Byte_grid (1 .. the_Image.Height, 1 .. the_Image.Width);
  begin
     case the_Image.tex_pixel_Format is
        when GL.LUMINANCE =>

           for Row in the_Grid'Range (1) loop
              for Col in the_Grid'Range (2) loop
                 the_Grid (Row, Col) := the_Image.Data (the_Image.Width * (Row - 1) + Col - 1);
              end loop;
           end loop;

        when others =>
           raise not_yet_implemented;       -- tbd: do these
     end case;
     return the_Grid;
  end To_greyscale_pixels;

  procedure Insert_into_GL(
              id             : Integer;
              size           : Integer;
              width          : Integer;
              height         : Integer;
              texFormat      : TexFormatEnm;
              texPixelFormat : TexPixelFormatEnm;
              image_p        : Byte_array_ptr
            )
  is
    pragma Unreferenced (size);
    ptr: constant GL.pointer:= image_p(0)'Access;
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
  subtype Size_test_a is Byte_array(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);
  workaround_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and then
    Size_test_a'Alignment = Size_test_b'Alignment;
  --

  procedure Fill_Buffer(b: in out Input_buffer);
  -- ^ Spec here to avoid in Get_Byte below (GNAT 2009):
  -- warning: call to subprogram with no separate spec prevents inlining

  procedure Fill_Buffer(b: in out Input_buffer)
  is
    --
    procedure BlockRead(
      buffer       :    out Byte_array;
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
            buffer(i):= Ubyte(SE_Buffer(Stream_Element_Offset(i-buffer'First)+SE_Buffer'First));
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

  procedure Attach_Stream(
    b   : out Input_buffer;
    stm : in Ada.Streams.Stream_IO.Stream_Access
  )
  is
  begin
    b.stm:= stm;
    Fill_Buffer(b);
  end Attach_Stream;

  procedure Get_Byte(b: in out Input_buffer; byte: out Ubyte) is
  begin
    if b.InBufIdx > b.MaxInBufIdx then
      Fill_Buffer(b);
      if b.InputEoF then
        raise End_Error;
      end if;
    end if;
    byte:= b.data(b.InBufIdx);
    b.InBufIdx:= b.InBufIdx + 1;
  end Get_Byte;

  function Load (S : in  Ada.Streams.Stream_IO.Stream_Access) return Image
  is
    the_Image : Image;
    idx: Natural;
    im_desc: GID.Image_descriptor;

    --  Generic parameter: bit depth (outside of GID)
    --  We don't want to test the bit depth at each pixel!
    generic
      bit_depth: Positive;
    procedure GID_with_generic_bit_depth(image: in out GID.Image_descriptor);

    procedure GID_with_generic_bit_depth(image: in out GID.Image_descriptor) is

      next_frame: Ada.Calendar.Day_Duration;  --  animation time, unused

      --  Generic parameters for GID's Load_Image

      subtype Primary_color_range is GL.Ubyte;

      procedure Set_X_Y (x, y: Natural) is
      begin
        idx:= (bit_depth / 8) * (x + the_Image.Width * y);
      end Set_X_Y;
      --
      procedure Put_Pixel (
        red, green, blue : Primary_color_range;
        alpha            : Primary_color_range
      )
      is
      pragma Warnings(off, alpha); -- alpha is just ignored
      begin
        case bit_depth is  --  This test happens actually at compile time :-)
          when 32 =>
            the_Image.Data(idx..idx+3):= (red, green, blue, alpha);
            idx:= idx + 4;  -- Index on next pixel on the right, for next time.
          when 24 =>
            the_Image.Data(idx..idx+2):= (red, green, blue);
            idx:= idx + 3;  -- Index on next pixel on the right, for next time.
          when  8 =>
            the_Image.Data(idx):= red;  --  = green = blue
            idx:= idx + 1;  -- Index on next pixel on the right, for next time.
          when others =>
            null;
        end case;
      end Put_Pixel;

      procedure Feedback(percents: Natural) is null;

      procedure GID_Load_image_instanciated is
        new GID.Load_image_contents(
          Primary_color_range, Set_X_Y,
          Put_Pixel, Feedback, GID.fast
        );

    begin
      GID_Load_image_instanciated(image, next_frame);
    end GID_with_generic_bit_depth;

    procedure GID_32bpp is new GID_with_generic_bit_depth(32);
    procedure GID_24bpp is new GID_with_generic_bit_depth(24);
    procedure GID_8bpp  is new GID_with_generic_bit_depth(8);

    imageBits, dest_bits: Integer;

  begin
    --  TGA files are headerless, so, "know your data!"
    GID.Load_image_header(im_desc, S.all, try_tga => True);
    the_Image.Width:=  GID.Pixel_width(im_desc);
    the_Image.Height:= GID.Pixel_height(im_desc);
    imageBits   := GID.Bits_per_pixel(im_desc);
    the_Image.size := the_Image.Width * the_Image.Height;
    --
    --  Now a little headache.
    --
    case GID.Format(im_desc) is
      when GID.TGA =>
        case imageBits is
          when 32 | 24 | 8 =>
            --  For 8 bits, we take alpha := grey value (Rod special setting:-) )
            dest_bits:= imageBits;
          when others =>
            raise Constraint_Error with "Tricky TGA BPP not supported" & Integer'Image(imageBits);
        end case;
      when others =>
        case imageBits is
          when 32 =>
            dest_bits:= 32;
          when others =>
            dest_bits:= 24;  -- 8 or 4 is actually a RGB with palette
        end case;
    end case;

    -- Allocation
    the_Image.Data:= new Byte_array(0..(dest_bits/8)*the_Image.size-1);
    case dest_bits is
      when 32 =>
        GID_32bpp(im_desc);
        the_Image.blending_hint:= True;
        the_Image.tex_Format      := GL.RGBA;
        the_Image.tex_pixel_Format:= GL.RGBA;
      when 24 =>
        GID_24bpp(im_desc);
        the_Image.blending_hint:= False;
        the_Image.tex_Format      := GL.RGB;
        the_Image.tex_pixel_Format:= GL.RGB;
      when 4 | 8  =>
        GID_8bpp(im_desc);
        the_Image.blending_hint:= True;
        the_Image.tex_Format      := GL.LUMINANCE; -- ALPHA
        the_Image.tex_pixel_Format:= GL.LUMINANCE;
      when others =>
        raise Constraint_Error with "BPP not supported" & Integer'Image(imageBits);
    end case;
    return the_Image;
  end Load;

  function Load (file_name : in  String) return Image is
    f: File_Type;
    the_Image : Image;
  begin
    begin
      Open(f,In_File,file_name);
    exception
      when Name_Error => raise File_Not_Found with "file name:" & file_name;
    end;
    the_Image := Load ( Stream(f) );
    Close(f);
    return the_Image;
  exception
    when e: others =>
      Close(f);
      Raise_Exception(Exception_Identity(e), "file name:" & file_name);
      return the_Image;
  end Load;

  procedure Load (
    file_name    :     String;
    ID           :     Integer;  --  ID is the GL texture identifier to bind to
    blending_hint: out Boolean   --  might have blending / transparency / alpha ?
  )
  is
    f: File_Type;
  begin
    begin
      Open(f,In_File, file_name);
    exception
      when Name_Error => raise File_Not_Found with "file name:" & file_name;
    end;
    Load ( Stream(f), ID, blending_hint );
    Close(f);
  exception
    when e: others =>
      Close(f);
      Raise_Exception(Exception_Identity(e), "file name:" & file_name);
  end Load;

  procedure Load (
    s            :     Ada.Streams.Stream_IO.Stream_Access;  -- input data stream
    ID           :     Integer;  --  ID is the GL texture identifier to bind to
    blending_hint: out Boolean   --  might have blending / transparency / alpha ?
  )
  is
    the_Image : Image := Load (s);
  begin
    Insert_into_GL(
              id             => ID,
              size           => the_Image.size,
              width          => the_Image.Width,
              height         => the_Image.Height,
              texFormat      => the_Image.tex_Format,
              texPixelFormat => the_Image.tex_pixel_Format,
              image_p        => the_Image.Data
    );
    --  Release our data, its been uploaded to the GL system
    Free( the_Image.Data );
    blending_hint := the_Image.blending_hint;
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
    -- 4-byte padding for .bmp/.avi formats is the same as GL's default
    -- padding: see glPixelStore, GL_[UN]PACK_ALIGNMENT = 4 as initial value.
    -- http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml
    --
    padded_row_size: constant Positive:=
      4 * Integer(Float'Ceiling(Float(width) * 3.0 / 4.0));
    -- (in bytes)
    --
    type Temp_bitmap_type is array(Natural range <>) of aliased GL.Ubyte;
    PicData: Temp_bitmap_type(0..(padded_row_size+4) * (height+4) - 1);
    -- No dynamic allocation needed!
    -- The "+4" are there to avoid parity address problems when GL writes
    -- to the buffer.
    type loc_pointer is new GL.pointer;
    function Cvt is new Ada.Unchecked_Conversion(System.Address,loc_pointer);
    -- This method is functionally identical as GNAT's Unrestricted_Access
    -- but has no type safety (cf GNAT Docs)
    pragma No_Strict_Aliasing(loc_pointer); -- recommended by GNAT 2005+
    pPicData: loc_pointer;
    data_max: constant Integer:= padded_row_size * height - 1;
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

  procedure Screenshot( name: in String ) is
    -- Translated by (New) P2Ada v. 15-Nov-2006
    -- http://wiki.delphigl.com/index.php/Screenshot
    f: Ada.Streams.Stream_IO.File_Type;

    type BITMAPFILEHEADER is record
      bfType     : U16;
      bfSize     : U32;
      bfReserved1: U16:= 0;
      bfReserved2: U16:= 0;
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
      biXPelsPerMeter: I32:= 0;
      biYPelsPerMeter: I32:= 0;
      biClrUsed      : U32:= 0;
      biClrImportant : U32:= 0;
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

    --  Schreiben der Bitmap-Informationen
    FileInfo.biSize       := BITMAPINFOHEADER'Size / 8;
    FileInfo.biWidth      := I32(Viewport(2));
    FileInfo.biHeight     := I32(Viewport(3));
    FileInfo.biPlanes     := 1;
    FileInfo.biBitCount   := 24;
    FileInfo.biCompression:= 0;
    FileInfo.biSizeImage  :=
      U32(
        -- 4-byte padding for .bmp/.avi formats
        4 * Integer(Float'Ceiling(Float(FileInfo.biWidth) * 3.0 / 4.0)) *
        Integer(FileInfo.biHeight)
      );

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
    padded_row_size: constant Positive:=
      4 * Integer(Float'Ceiling(Float(width) * 3.0 / 4.0));
    calc_bmp_size: constant U32:= U32(padded_row_size * height);
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
