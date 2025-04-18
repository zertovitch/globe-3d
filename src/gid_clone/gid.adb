---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Copyright (c) Gautier de Montmollin 2010 .. 2023
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 2-May-2010 on the site
--  http://www.opensource.org/licenses/mit-license.php

with GID.Headers,
     GID.Decoding_BMP,
     GID.Decoding_GIF,
     GID.Decoding_JPG,
     GID.Decoding_QOI,
     GID.Decoding_PNG,
     GID.Decoding_PNM,
     GID.Decoding_TGA;

with Ada.Unchecked_Deallocation;

package body GID is

  --  Internal: a few header items (palette, some large JPEG tables)
  --  are heap allocated; we need to release them upon finalization
  --  or descriptor reuse.

  procedure Clear_Heap_Allocated_Memory (Object : in out Image_Descriptor) is
    procedure Dispose is
      new Ada.Unchecked_Deallocation (Color_Table, p_Color_Table);
    procedure Dispose is
      new Ada.Unchecked_Deallocation
        (JPEG_Defs.VLC_Table,
         JPEG_Defs.p_VLC_Table);
  begin
    --  Deterministic garbage collection of heap allocated objects.
    --  -> Palette
    Dispose (Object.palette);
    --  -> JPEG tables
    for ad in JPEG_Defs.VLC_Defs_Type'Range (1) loop
      for idx in JPEG_Defs.VLC_Defs_Type'Range (2) loop
        Dispose (Object.JPEG_stuff.vlc_defs (ad, idx));
      end loop;
    end loop;
  end Clear_Heap_Allocated_Memory;

  -----------------------
  -- Load_image_header --
  -----------------------

  procedure Load_Image_Header
    (image   :    out Image_Descriptor;
     from    : in out Ada.Streams.Root_Stream_Type'Class;
     try_tga :        Boolean := False)
  is
  begin
    Clear_Heap_Allocated_Memory (image);
    image.stream := from'Unchecked_Access;
    --
    --  Load the very first symbols of the header,
    --  this identifies the image format.
    --
    Headers.Load_Signature (image, try_tga);
    --
    case image.format is
      when BMP  => Headers.Load_BMP_Header  (image);
      when FITS => Headers.Load_FITS_Header (image);
      when GIF  => Headers.Load_GIF_Header  (image);
      when JPEG => Headers.Load_JPEG_Header (image);
      when PNG  => Headers.Load_PNG_Header  (image);
      when PNM  => Headers.Load_PNM_Header  (image);
      when QOI  => Headers.Load_QOI_Header  (image);
      when RIFF => Headers.Load_RIFF_Header (image);
      when TGA  => Headers.Load_TGA_Header  (image);
      when TIFF => Headers.Load_TIFF_Header (image);
    end case;
  end Load_Image_Header;

  -----------------
  -- Pixel_width --
  -----------------

  function Pixel_Width (image : Image_Descriptor) return Positive is
  begin
    return Positive (image.width);
  end Pixel_Width;

  ------------------
  -- Pixel_height --
  ------------------

  function Pixel_Height (image : Image_Descriptor) return Positive is
  begin
    return Positive (image.height);
  end Pixel_Height;

  function Display_Orientation (image : Image_Descriptor) return Orientation is
  begin
    return image.display_orientation;
  end Display_Orientation;

  -------------------------
  -- Load_image_contents --
  -------------------------

  procedure Load_Image_Contents
    (image      : in out Image_Descriptor;
     next_frame :    out Ada.Calendar.Day_Duration)
  is
    procedure BMP_Load is
      new Decoding_BMP.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

    procedure GIF_Load is
      new Decoding_GIF.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback, mode);

    procedure JPG_Load is
      new Decoding_JPG.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

    procedure PNG_Load is
      new Decoding_PNG.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

    procedure PNM_Load is
      new Decoding_PNM.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

    procedure QOI_Load is
      new Decoding_QOI.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

    procedure TGA_Load is
      new Decoding_TGA.Load (Primary_Color_Range, Set_X_Y, Put_Pixel, Feedback);

  begin
    next_frame := 0.0;
    --  ^  Value updated in case of an animation and when
    --     current frame is not the last frame.
    case image.format is
      when BMP =>        BMP_Load (image);
      when GIF =>        GIF_Load (image, next_frame);
      when JPEG =>       JPG_Load (image);
      when PNG =>        PNG_Load (image, next_frame);
      when PNM =>        PNM_Load (image);
      when QOI =>        QOI_Load (image);
      when TGA =>        TGA_Load (image);
      when others =>     raise known_but_unsupported_image_format;
    end case;
  end Load_Image_Contents;

  ---------------------------------------
  -- Some informations about the image --
  ---------------------------------------

  function Format (image : Image_Descriptor) return Image_Format_Type is
  begin
    return image.format;
  end Format;

  function Detailed_Format (image : Image_Descriptor) return String is
  begin
    return Bounded_255.To_String (image.detailed_format);
  end Detailed_Format;

  function Subformat (image : Image_Descriptor) return Integer is
  begin
    return image.subformat_id;
  end Subformat;

  function Bits_per_Pixel (image : Image_Descriptor) return Positive is
  begin
    return image.bits_per_pixel;
  end Bits_per_Pixel;

  function Is_RLE_Encoded (image : Image_Descriptor) return Boolean is
  begin
    return image.RLE_encoded;
  end Is_RLE_Encoded;

  function Is_Progressive (image : Image_Descriptor) return Boolean is
  begin
    return image.progressive;
  end Is_Progressive;

  function Greyscale (image : Image_Descriptor) return Boolean is
  begin
    return image.greyscale;
  end Greyscale;

  function Has_Palette (image : Image_Descriptor) return Boolean is
  begin
    return image.palette /= null;
  end Has_Palette;

  function Expect_Transparency (image : Image_Descriptor) return Boolean is
  begin
    return image.transparency;
  end Expect_Transparency;

  procedure Get_Next_Frame_Informations
    (image             : in     Image_Descriptor;
     dispose_operation :    out PNG_Defs.Dispose_Op_Type;
     blend_operation   :    out PNG_Defs.Blend_Op_Type)
  is
  begin
    dispose_operation := image.PNG_stuff.dispose_op;
    blend_operation   := image.PNG_stuff.blend_op;
  end Get_Next_Frame_Informations;

  package body JPEG_Defs is

    function Identify
      (param : Upsampling_Parameters) return Upsampling_Profile_Type
    is
      subtype Defined_Upsampling_Profile_Type is Upsampling_Profile_Type
        range hor_1_1_ver_1_1 .. hor_2_1_ver_2_1;
      profile_catalogue : constant array (Defined_Upsampling_Profile_Type)
        of Upsampling_Parameters :=
        (hor_1_1_ver_1_1 => (1, 1, 1, 1),
         hor_1_2_ver_1_2 => (1, 2, 1, 2),
         hor_2_1_ver_2_1 => (2, 1, 2, 1));
    begin
      for p in Defined_Upsampling_Profile_Type loop
        if profile_catalogue (p) = param then
          return p;
        end if;
      end loop;
      return other_profile;
    end Identify;

  end JPEG_Defs;

  overriding procedure Adjust (Object : in out Image_Descriptor) is
    use JPEG_Defs;
  begin
    --  Clone heap allocated objects, if any.
    --  -> Palette
    if Object.palette /= null then
      Object.palette := new Color_Table'(Object.palette.all);
    end if;
    --  -> JPEG tables
    for ad in JPEG_Defs.VLC_Defs_Type'Range (1) loop
      for idx in JPEG_Defs.VLC_Defs_Type'Range (2) loop
        if Object.JPEG_stuff.vlc_defs (ad, idx) /= null then
          Object.JPEG_stuff.vlc_defs (ad, idx) :=
            new VLC_Table'(Object.JPEG_stuff.vlc_defs (ad, idx).all);
        end if;
      end loop;
    end loop;
  end Adjust;

  overriding procedure Finalize (Object : in out Image_Descriptor) is
  begin
    Clear_Heap_Allocated_Memory (Object);
  end Finalize;

end GID;
