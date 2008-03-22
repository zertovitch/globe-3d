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

package body X_Lib.Cursor is

   function X_Create_Pixmap_Cursor
     (Display          : in Display_Pointer;
      Source           : in Pixmap_ID;
      Mask             : in Pixmap_ID;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color;
      X_Hot, Y_Hot     : in Natural_Position)
      return Cursor_ID is
      function XCreatePixmapCursor
        (Display          : in Display_Pointer;
         Source           : in Pixmap_ID;
         Mask             : in Pixmap_ID;
         Foreground_Color : in System.Address;
         Background_Color : in System.Address;
         X_Hot, Y_Hot     : in Interfaces.C.unsigned)
         return Cursor_ID;
      pragma Import (C, XCreatePixmapCursor, "XCreatePixmapCursor");
   begin
      return XCreatePixmapCursor (Display, Source, Mask,
                                  Foreground_Color'Address,
                                  Background_Color'Address,
                                  Interfaces.C.unsigned (X_Hot),
                                  Interfaces.C.unsigned (Y_Hot));
   end X_Create_Pixmap_Cursor;
   pragma Inline (X_Create_Pixmap_Cursor);


   function X_Create_Glyph_Cursor
     (Display          : in Display_Pointer;
      Source_Font      : in Font_ID;
      Mask_Font        : in Font_ID;
      Source_Char      : in Interfaces.C.unsigned;
      Mask_Char        : in Interfaces.C.unsigned;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color)
      return Cursor_ID is
      function XCreateGlyphCursor
        (Display          : in Display_Pointer;
         Source_Font      : in Font_ID;
         Mask_Font        : in Font_ID;
         Source_Char      : in Interfaces.C.unsigned;
         Mask_Char        : in Interfaces.C.unsigned;
         Foreground_Color : in System.Address;
         Background_Color : in System.Address)
         return Cursor_ID;
      pragma Import (C, XCreateGlyphCursor, "XCreateGlyphCursor");
   begin
      return XCreateGlyphCursor (Display, Source_Font, Mask_Font,
                                 Source_Char, Mask_Char,
                                 Foreground_Color'Address,
                                 Background_Color'Address);
   end X_Create_Glyph_Cursor;
   pragma Inline (X_Create_Glyph_Cursor);


   procedure X_Query_Best_Cursor
     (Display     : in     Display_Pointer;
      D           : in     Drawable_ID;
      Width,
      Height      : in     Dimension;
      Best_Width,
      Best_Height :    out Dimension) is
      function XQueryBestCursor
        (Display     : in Display_Pointer;
         D           : in Drawable_ID;
         Width,
         Height      : in Interfaces.C.unsigned;
         Best_Width,
         Best_Height : in System.Address)
	 return Status_Type;
      pragma Import (C, XQueryBestCursor, "XQueryBestCursor");
      Best_W, Best_H : Interfaces.C.unsigned;
   begin
      if XQueryBestCursor (Display, D, Interfaces.C.unsigned (Width),
                           Interfaces.C.unsigned (Height),
                           Best_W'Address, Best_H'Address) = Error_Status then
         raise X_Error;
      end if;
      Best_Width  := Dimension (Best_W);
      Best_Height := Dimension (Best_H);
   end X_Query_Best_Cursor;


   procedure X_Recolor_Cursor
     (Display          : in Display_Pointer;
      Cursor           : in Cursor_ID;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color) is
      function XRecolorCursor
        (Display          : in Display_Pointer;
         Cursor           : in Cursor_ID;
         Foreground_Color : in System.Address;
         Background_Color : in System.Address)
         return Integer;
      pragma Import (C, XRecolorCursor, "XRecolorCursor");
      Return_Value : Integer;
   begin
      Return_Value := XRecolorCursor (Display,
                                      Cursor, 
                                      Foreground_Color'Address,
                                      Background_Color'Address);
   end X_Recolor_Cursor;
   pragma Inline (X_Recolor_Cursor);

end X_Lib.Cursor;
