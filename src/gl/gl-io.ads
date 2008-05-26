------------------------------------------------------------------------------
--  File:            GL-IO.ads
--  Description:     I/O for (Open)GL graphics
--
--                   This package provides currently:
--
--                   * INPUT:
--
--                    - load a TGA image (RGA, RGBA, Grey) from a file or
--                        a data stream to a texture
--                    - load a BMP image (B&W, 16 colours indexed (palette),
--                        256 colours indexed) from a file or a data stream
--                        to a texture
--
--                   * OUTPUT:
--
--                    - save the current, active viewport to a BMP file
--                        i.e. a screenshot
--                    - capture a video of the current, active viewport
--                        to an AVI file
--
------------------------------------------------------------------------------
-- Change log:
--
-- 26-May-2008 (GdM): added support for TGA images with RLE encoding
--
-- 27-Jan-2008 (RK):  added 'Image' record and a function to get greyscale pixels from an Image.
--
-- 10-May-2007 (GdM): screenshot and video capture
--
-- 13-Oct-2006 (GdM): new blending_hint out parameter, indicates possible
--                    blending/transparency
-- 30-Apr-2006 (GdM): - added multi-format loaders
--                    - dimensions not power of two allowed, but
--                      discouraged in the docs.
--                      -> removed TGA_BAD_DIMENSION

with Ada.Streams.Stream_IO;
with Ada.Unchecked_deallocation;

package GL.IO is

  File_Not_Found : exception;

  type Supported_format is (BMP, TGA);


  type Byte_array is array( Integer range <> ) of aliased GL.UByte;
  type Byte_array_ptr is access all Byte_array;

  procedure Free is
    new Ada.Unchecked_deallocation(Byte_array, Byte_array_ptr);

  type Byte_grid is array (Integer range <>, Integer range <>) of aliased GL.UByte;


  type Image is
    record
      blending_hint    : Boolean;        -- has the image blending / transparency /alpha ?
      tex_Format       : GL.TexFormatEnm;
      tex_pixel_Format : GL.TexPixelFormatEnm;
      size             : Integer;
      Width,
      Height           : Integer;
      Data             : Byte_Array_Ptr;
    end record;



  function to_TGA_Image (Filename : in  String                            -- Input data tga filename
                        ) return Image;
  function to_TGA_Image (S : in  Ada.Streams.Stream_IO.Stream_Access      -- Input data stream
                        ) return Image;

  function to_greyscale_Pixels (the_Image : in Image) return Byte_grid;


  -- Multi-format loader:

  procedure Load (
    name  : String;            -- file name
    format: Supported_format;  -- expected file format
    ID    : Integer;           -- ID is the texture identifier to bind to
    blending_hint: out Boolean -- has blending / transparency /alpha ?
  );

  procedure Load (
    s     : Ada.Streams.Stream_IO.Stream_Access;
                               -- input data stream (e.g. UnZip.Streams)
    format: Supported_format;  -- expected file format
    ID    : Integer;           -- ID is the texture identifier to bind to
    blending_hint: out Boolean -- has blending / transparency /alpha ?
  );

  -- Loaders specific to different formats:

  ----------------------
  -- BMP format Input --
  ----------------------

  procedure Load_BMP (
        Name : String;
        -- File name
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  );

  procedure Load_BMP (
        S            : in  Ada.Streams.Stream_IO.Stream_Access;
        -- Input data stream
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  );

  Unsupported_BMP_format,
  Not_BMP_format,
  BMP_unsupported_bits_per_pixel,
  Unsupported_compression:      exception;

  ----------------------
  -- TGA format Input --
  ----------------------

  procedure Load_TGA (
        Name : String;
        -- File name
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  );

  procedure Load_TGA (
        S            : in  Ada.Streams.Stream_IO.Stream_Access;
        -- Input data stream
        Id           : in  Integer;
        -- Id is the texture identifier to bind to
        blending_hint: out Boolean
        -- has the image blending / transparency /alpha ?
  );

  TGA_Unsupported_Image_Type     : exception;   -- color mapped or compressed image
  TGA_Unsupported_Bits_per_pixel : exception;   -- image bits is not 8, 24 or 32
  TGA_Bad_Data                   : exception;   -- image data could not be loaded

  ------------------------------------------------------------
  -- Image of the current, active viewport (RGB BMP format) --
  ------------------------------------------------------------

  procedure Screenshot( name: String );

  --------------------------------------------------
  -- Video capture (RGB uncompressed, AVI format) --
  --------------------------------------------------

  procedure Start_capture( AVI_name: String; frame_rate: Positive );

  procedure Capture_frame; -- captures the current, active viewport.

  procedure Stop_capture;

end GL.IO;
