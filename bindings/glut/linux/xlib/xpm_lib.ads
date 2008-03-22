-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2000 Hans-Frieder Vogt                                --
--                                                                           --
--  Adabindx is free software; you can redistribute it and/or modify it      --
--  under the terms of the GNU General Public License as published by the    --
--  Free Software Foundation; either version 2 of the License, or (at your   --
--  option) any later version.                                               --
--                                                                           --
--  This program is distributed in the hope that it will be useful, but      --
--  WITHOUT ANY WARRANTY; without even the implied warranty of               --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  See the GNU General Public License for more details.                     --
--                                                                           --
--  You should have received a copy of the GNU General Public License        --
--  along with this program; if not, write to the                            --
--  Free Software Foundation, Inc.,                                          --
--  59 Temple Place - Suite 330,                                             --
--  Boston, MA 02111-1307, USA.                                              --
--                                                                           --
--  As a special exception, if other files instantiate generics from this    --
--  unit, or you link this unit with other files to produce an executable,   --
--  this unit does not by itself cause the resulting executable to be        --
--  covered by the GNU General Public License. This exception does not       --
--  however invalidate any other reasons why the executable file might be    --
--  covered by the GNU General Public License.                               --
--                                                                           --
--  X Window System is copyrighted by the X Consortium                       --
--  Motif(tm)       is copyrighted by the Open Software Foundation, Inc.     --
--                  and by The Open Group                                    --
--                                                                           --
--                                                                           --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--
-- HISTORY:
--          June 20, 1998 begin of history
--
-------------------------------------------------------------------------------

with Interfaces.C,
     String_List,
     String_List_Conversion,
     X_Strings,
     X_Lib;

package Xpm_Lib is

   use X_Lib;

--
--  Types
--

   type Xpm_Color is record
      Str       : X_Strings.X_String;
      Symbolic  : X_Strings.X_String;
      M_Color   : X_Strings.X_String;
      G4_Color  : X_Strings.X_String;
      G_Color   : X_Strings.X_String;
      C_Color   : X_Strings.X_String;
   end record;
   type Xpm_Color_Array is
      array (Natural range <>) of aliased Xpm_Color;
   type Xpm_Color_Array_Access is access all Xpm_Color_Array;


   type Xpm_Color_Symbol is record
      Name  : X_Strings.X_String;
      Value : X_Strings.X_String;
      Pix   : Pixel;
   end record;
   type Xpm_Color_Symbol_Array is
      array (Natural range <>) of aliased Xpm_Color_Symbol;
   type Xpm_Color_Symbol_Array_Access is access all Xpm_Color_Symbol_Array;


   type Pixel_Array is
      array (Natural range <>) of aliased Pixel;
   type Pixel_Array_Access is access all Pixel_Array;


   type Xpm_Extension is record
      Name   : X_Strings.X_String;
      Nlines : Interfaces.C.unsigned;
      Lines  : String_List_Conversion.Chars_Ptr_List_Type;
   end record;
   type Xpm_Extension_Array is
      array (Natural range <>) of aliased Xpm_Extension;
   type Xpm_Extension_Array_Access is access all Xpm_Extension_Array;


   type Xpm_Color_Key_Class is (Mono, Grey4, Grey, Color);
   Gray4 : constant Xpm_Color_Key_Class;
   Gray  : constant Xpm_Color_Key_Class;


   type Xpm_Attr_Valuemask is record
      Visual              : Boolean;
      Colormap            : Boolean;
      Depth               : Boolean;
      Size                : Boolean;
      Hotspot             : Boolean;
      Chars_Per_Pixel     : Boolean;
      Color_Symbols       : Boolean;
      Rgb_Filename        : Boolean;
      Infos               : Boolean;
      Return_Pixels       : Boolean;
      Extensions          : Boolean;
      Exact_Colors        : Boolean;
      Closeness           : Boolean;
      Rgb_Closeness       : Boolean;
      Color_Key           : Boolean;
      Color_Table         : Boolean;
      Return_Alloc_Pixels : Boolean;
   end record;


   subtype Hotspot_Position is X_Lib.Position range 0 .. X_Lib.Position'Last;


   type Xpm_Attributes is record
      Value_Mask       : Xpm_Attr_Valuemask;         -- mask of defined attributes
      Visual           : Visual_Pointer;             -- Visual to use
      Colormap         : Colormap_ID;                -- Colormap to use
      Depth            : Color_Depth;                -- depth of visual
      Width            : Dimension;                  -- width of the pixmap
      Height           : Dimension;                  -- height of the pixmap
      X_Hotspot        : Hotspot_Position;           -- position of the hotspot
      Y_Hotspot        : Hotspot_Position;
      Cpp              : Natural;                    -- number of characters per pixel
      Pixels           : Pixel_Array_Access;         -- used color pixels
      Color_Symbols    : Xpm_Color_Symbol_Array_Access;  -- color symbols to override
      Rgb_Fname        : X_Strings.X_String;             -- name of the RGB file
                                                         -- (e.g. /usr/lib/X11/rgb.txt)
      Extensions       : Xpm_Extension_Array_Access; -- list of extension
      Color_Table      : Xpm_Color_Array_Access;     -- list of colors
      Mask_Pixel       : Pixel;                      -- pixel value of transparent color
      -- color allocation directives
      Exact_Colors     : Boolean;                    -- if true only use exact colors
      Closeness        : Interfaces.C.unsigned;                   -- allowable rgb deviation
      Red_Closeness    : Interfaces.C.unsigned;                   -- allowable red deviation
      Green_Closeness  : Interfaces.C.unsigned;                   -- allowable green deviation
      Blue_Closeness   : Interfaces.C.unsigned;                   -- allowable blue deviation
      Color_Key        : Xpm_Color_Key_Class;        -- use color from this color set
      -- new from Xpm-Lib 3.4g
      Alloc_Pixels     : Pixel_Array_Access;         -- return allocated pixels
   end record;

   type Xpm_Attributes_Access is access all Xpm_Attributes;

   -- Value returned if no mask exists
   Xpm_Undef_Pixel : constant Interfaces.C.unsigned := 16#80000000#;


   -- type which is an Ada-Representation of the C style string array
   -- in an xpm file
   --
   subtype Pixmap_Data_Type is String_List.Element_Access_List;
   Null_Pixmap_Data : Pixmap_Data_Type
                    := String_List.Null_Element_Access_List;


   -- -------------------------------------------------------------------------
   --
   -- exceptions
   --
   Xpm_Error_Color_Error  : exception;  -- partial success

   Xpm_Error_Open_Failed  : exception renames X_Error_Open_Failed;
   Xpm_Error_File_Invalid : exception renames X_Error_File_Invalid;
   Xpm_Error_No_Memory    : exception renames X_Error_No_Memory;
   Xpm_Error_Color_Failed : exception;

   Xpm_Error_Unknown      : exception;


   type Xpm_Version is new Natural;

   -- return the Version of the xpm-library in use
   --
   --
   -- I don't interface this function now, because it exists only in the
   -- most recent versions of the xpm-library and gives all the others
   -- unresolved externals when linking
   --
   -- function Xpm_Library_Version return Xpm_Version;


-- ----------------------------------------------------------------------------
--
-- routines which read from files containing XPM-Data
--

   -- Xpm_Read_File_To_Image
   --
   -- reads the .xpm-file Filename and returns an Image and eventually
   -- as well a shape mask image (only if the color none was used)
   --
   -- The Images returned have to be freed with X_Destroy_Image when not
   -- needed any longer
   --
   procedure Xpm_Read_File_To_Image
     (Display     : in	  Display_Pointer;
      Filename    : in	  String;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Read_File_To_Image
     (Display     : in	  Display_Pointer;
      Filename    : in	  String;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer);


   -- Xpm_Read_File_To_Pixmap
   --
   -- reads the .xpm-file Filename and returns a Pixmap_ID and eventually
   -- as well a shape mask Pixmap_ID (only if the color none was used)
   --
   -- The Pixmap_IDs returned have to be freed with X_Free_Pixmap when not
   -- needed any longer
   --
   procedure Xpm_Read_File_To_Pixmap
     (Display    : in     Display_Pointer;
      Drawable   : in     Drawable_ID;
      Filename   : in     String;
      Pix        :    out Pixmap_ID;
      Shape_Mask :    out Pixmap_ID;
      Attr       : in out Xpm_Attributes);

   procedure Xpm_Read_File_To_Pixmap
     (Display    : in     Display_Pointer;
      Drawable   : in     Drawable_ID;
      Filename   : in     String;
      Pix        :    out Pixmap_ID;
      Shape_Mask :    out Pixmap_ID);


   -- Xpm_Read_File_To_Data
   --
   -- reads the .xpm-file Filename into a Pixmap_Data_Type, which is
   -- simply a representation of the string list in the .xpm-file in memory
   --
   procedure Xpm_Read_File_To_Data
     (Filename  : in     String;
      Data      :    out Pixmap_Data_Type);


-- ----------------------------------------------------------------------------
--
-- conversion routines
--

   -- Xpm_Create_Image_From_Data
   --
   -- create an image and eventually as well a shape mask image from the Pixmap
   -- stored in the string list Pixmap_Data_Type
   --
   -- The Images returned have to be freed with X_Destroy_Image when not
   -- needed any longer
   --
   procedure Xpm_Create_Image_From_Data
     (Display     : in	   Display_Pointer;
      Data        : in	   Pixmap_Data_Type;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Create_Image_From_Data
     (Display     : in	   Display_Pointer;
      Data        : in	   Pixmap_Data_Type;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer);


   -- Xpm_Create_Pixmap_From_Data
   --
   -- create a Pixmap_ID and eventually as well a shape mask Pixmap_ID from
   -- the Pixmap stored in the string list Pixmap_Data_Type
   --
   -- The Pixmap_IDs returned have to be freed with X_Free_Pixmap when not
   -- needed any longer
   --
   procedure Xpm_Create_Pixmap_From_Data
     (Display     : in     Display_Pointer;
      Drawable    : in     Drawable_ID;
      Data        : in     Pixmap_Data_Type;
      Pix         :    out Pixmap_ID;
      Shape_Mask  :    out Pixmap_ID;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Create_Pixmap_From_Data
     (Display     : in     Display_Pointer;
      Drawable    : in     Drawable_ID;
      Data        : in     Pixmap_Data_Type;
      Pix         :    out Pixmap_ID;
      Shape_Mask  :    out Pixmap_ID);


   -- Xpm_Create_Data_From_Image
   --
   -- take an Image and a shape mask Image and create an internal
   -- representation of a .xpm-file from it
   --
   procedure Xpm_Create_Data_From_Image
     (Display     : in     Display_Pointer;
      Image       : in     X_Image_Pointer;
      Shape_Image : in     X_Image_Pointer;
      Data        :    out Pixmap_Data_Type;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Create_Data_From_Image
     (Display     : in     Display_Pointer;
      Image       : in     X_Image_Pointer;
      Shape_Image : in     X_Image_Pointer;
      Data        :    out Pixmap_Data_Type);


   -- Xpm_Create_Data_From_Pixmap
   --
   -- take a Pixmap_ID and a shape mask Pixmap_ID and create an internal
   -- representation of a .xpm-file from it
   --
   procedure Xpm_Create_Data_From_Pixmap
     (Display     : in     Display_Pointer;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Data        :    out Pixmap_Data_Type;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Create_Data_From_Pixmap
     (Display     : in     Display_Pointer;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Data        :    out Pixmap_Data_Type);


-- ----------------------------------------------------------------------------
--
-- routines which write Image-Data to XPM-files
--

   -- Xpm_Write_File_From_Image
   --
   -- create a .xpm-file from the image and shape mask image given
   --
   procedure Xpm_Write_File_From_Image
     (Display      : in     Display_Pointer;
      Filename     : in     String;
      Image        : in     X_Image_Pointer;
      Shape_Image  : in     X_Image_Pointer;
      Attr         : in out Xpm_Attributes);

   procedure Xpm_Write_File_From_Image
     (Display      : in     Display_Pointer;
      Filename     : in     String;
      Image        : in     X_Image_Pointer;
      Shape_Image  : in     X_Image_Pointer);


   -- Xpm_Write_File_From_Pixmap
   --
   -- create a .xpm-file from the Pixmap_ID and shape mask Pixmap_ID given
   --
   procedure Xpm_Write_File_From_Pixmap
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Attr        : in out Xpm_Attributes);

   procedure Xpm_Write_File_From_Pixmap
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID);


   -- Xpm_Write_File_From_Data
   --
   -- create a .xpm-file from the internal representation of it
   --
   procedure Xpm_Write_File_From_Data
     (Filename    : in String;
      Data        : in Pixmap_Data_Type);



private

   for Xpm_Color_Key_Class use (Mono => 2, Grey4 => 3, Grey => 4, Color => 5);
   for Xpm_Color_Key_Class'Size use Interfaces.C.int'Size;
   Gray4 : constant Xpm_Color_Key_Class := Grey4;
   Gray  : constant Xpm_Color_Key_Class := Grey;


   for Xpm_Attr_Valuemask use record
-- UseLittleEndian
      Visual		  at 0 range  0 ..  0;
      Colormap  	  at 0 range  1 ..  1;
      Depth		  at 0 range  2 ..  2;
      Size		  at 0 range  3 ..  3;
      Hotspot		  at 0 range  4 ..  4;
      Chars_Per_Pixel	  at 0 range  5 ..  5;
      Color_Symbols	  at 0 range  6 ..  6;
      Rgb_Filename	  at 0 range  7 ..  7;
      Infos		  at 0 range  8 ..  8;
      Return_Pixels	  at 0 range  9 ..  9;
      Extensions	  at 0 range 10 .. 10;
      Exact_Colors	  at 0 range 11 .. 11;
      Closeness 	  at 0 range 12 .. 12;
      Rgb_Closeness	  at 0 range 13 .. 13;
      Color_Key 	  at 0 range 14 .. 14;
      Color_Table	  at 0 range 15 .. 15;
      Return_Alloc_Pixels at 0 range 16 .. 16;
-- NotLittleEndian
--!       Visual              at 0 range 16 .. 16;
--!       Colormap            at 0 range 15 .. 15;
--!       Depth               at 0 range 14 .. 14;
--!       Size                at 0 range 13 .. 13;
--!       Hotspot             at 0 range 12 .. 12;
--!       Chars_Per_Pixel     at 0 range 11 .. 11;
--!       Color_Symbols       at 0 range 10 .. 10;
--!       Rgb_Filename        at 0 range  9 ..  9;
--!       Infos               at 0 range  8 ..  8;
--!       Return_Pixels       at 0 range  7 ..  7;
--!       Extensions          at 0 range  6 ..  6;
--!       Exact_Colors        at 0 range  5 ..  5;
--!       Closeness           at 0 range  4 ..  4;
--!       Rgb_Closeness       at 0 range  3 ..  3;
--!       Color_Key           at 0 range  2 ..  2;
--!       Color_Table         at 0 range  1 ..  1;
--!       Return_Alloc_Pixels at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   pragma Pack (Xpm_Attr_Valuemask);
   for Xpm_Attr_Valuemask'Size use 17;


end Xpm_Lib;
