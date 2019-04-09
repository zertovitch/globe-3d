------------------------------------------------------------------
--  File:            GL-IO.ads
--  Description:     I/O for (Open)GL graphics
--
--  This package provides currently:
--
--  ******************************************************
--  * INPUT * from a file or a data stream, to a texture *
--  ******************************************************
--
--   - BMP, GIF, JPEG, PNG, PNM, TGA images, including
--     transparency for TGA, PNG (alpha level) and GIF (on/off)
--
--  ***************************************************
--  * OUTPUT * from the GL active viewport, to a file *
--  ***************************************************
--
--   - BMP image: screenshot
--   - AVI video: video capture
--
------------------------------------------------------------------
-- Change log:
--
-- 21-Jun-2016 (GdM): GL.IO image input switched to GID ( http://gen-img-dec.sf.net/ )
--                      format support extended from TGA & indexed BMP to the
--                      formats listed above.
--
-- 19-Jan-2010 (GdM): using workaround to the slow attribute I/O issue (GNAT,OA);
--                      buffered input; improvements on BMP
--
-- 26-May-2008 (GdM): added support for TGA images with RLE encoding
--
-- 27-Jan-2008 (RK):  added 'Image' record and a function to get greyscale pixels from an Image.
--
-- 10-May-2007 (GdM): screenshot and video capture
--
-- 13-Oct-2006 (GdM): new blending_hint out parameter, indicates possible
--                      blending/transparency
--
-- 30-Apr-2006 (GdM): - added multi-format loaders
--                    - dimensions not power of two allowed, but
--                      discouraged in the docs.
--                      -> removed TGA_BAD_DIMENSION

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package GL.IO is

  File_Not_Found : exception;

  type Byte_array is array( Integer range <> ) of aliased GL.Ubyte;
  type Byte_array_ptr is access all Byte_array;

  procedure Free is
    new Ada.Unchecked_Deallocation(Byte_array, Byte_array_ptr);

  type Byte_grid is array (Integer range <>, Integer range <>) of aliased GL.Ubyte;

  type Image is
    record
      blending_hint    : Boolean;        -- has the image blending / transparency / alpha ?
      tex_format       : GL.TexFormatEnm;
      tex_pixel_format : GL.TexPixelFormatEnm;
      size             : Integer;
      width            : Integer;  --  Some hardware assume a multiple of 2 or 4, or a power of 2.
      height           : Integer;  --  Same for height (less likely).
      data             : Byte_array_ptr;
    end record;


  function To_greyscale_pixels (the_Image : in Image) return Byte_grid;

  --  Input from a file or a stream to an object of the Image type.

  function Load (file_name : in  String) return Image;
  function Load (S : in  Ada.Streams.Stream_IO.Stream_Access ) return Image;

  --  Input from a file or a stream to GL, using a given identifier.
  --  NB: Some hardware assume an image width of a multiple of 2 or 4, or a power of 2.
  --      Display can appear slanted otherwise. Same for height (less likely).

  procedure Load (
    file_name    :     String;
    ID           :     Integer;  --  ID is the GL texture identifier to bind to
    blending_hint: out Boolean   --  might have blending / transparency / alpha ?
  );

  procedure Load (
    s            :     Ada.Streams.Stream_IO.Stream_Access;  -- input data stream
    ID           :     Integer;  --  ID is the GL texture identifier to bind to
    blending_hint: out Boolean   --  might have blending / transparency / alpha ?
  );

  ---------------------------------------------------------------------------
  -- Image ("screenshot") of the current, active viewport (RGB BMP format) --
  ---------------------------------------------------------------------------

  procedure Screenshot( name: String );

  --------------------------------------------------
  -- Video capture (RGB uncompressed, AVI format) --
  --------------------------------------------------

  procedure Start_capture( AVI_name: String; frame_rate: Positive );

  procedure Capture_frame; -- captures the current, active viewport.

  procedure Stop_capture;

  --------------------------------------------------------------------------
  -- An object-oriented stream buffering, initially for reading images to --
  -- the GL system, but that may be useful elsewhere, hence its presence  --
  -- in this package's specification                                      --
  --------------------------------------------------------------------------
  --
  type Input_buffer is private;

  procedure Attach_Stream(
    b   : out Input_buffer;
    stm : in Ada.Streams.Stream_IO.Stream_Access
  );

  procedure Get_Byte(b: in out Input_buffer; byte: out Ubyte);
  pragma Inline(Get_Byte);

private

  type Input_buffer is record
    data       : Byte_array(1..1024);
    stm        : Ada.Streams.Stream_IO.Stream_Access;
    InBufIdx   : Positive;   --  Points to next char in buffer to be read
    MaxInBufIdx: Natural;    --  Count of valid chars in input buffer
    InputEoF   : Boolean;    --  End of file indicator
  end record;

end GL.IO;
