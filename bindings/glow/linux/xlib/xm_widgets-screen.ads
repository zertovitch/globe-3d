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

package Xm_Widgets.Screen is

   --
   --  XmScreen Widget
   --
   function Xm_Get_Xm_Screen (Screen : in X_Lib.Screen_Pointer) return Widget;

   -- -------------------------------------------------------------------------
   --
   -- resources
   --
-- UseMotif2.0 Motif2.1
   Xm_N_Bitmap_Conversion_Model       : constant Xt_N_Resource_String;

   type Bitmap_Conversion_Model_Type is (Match_Depth, Dynamic_Depth);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Bitmap_Conversion_Model_Type);
 
   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Bitmap_Conversion_Model_Type);
   pragma Convention (C, Append_Get);
 
   Xm_N_Color_Allocation_Proc         : constant Xt_N_Resource_String;
   Xm_N_Color_Calculation_Proc        : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Dark_Threshold                : constant Xt_N_Resource_String;
   Xm_N_Default_Copy_Cursor_Icon      : constant Xt_N_Resource_String;
   Xm_N_Default_Invalid_Cursor_Icon   : constant Xt_N_Resource_String;
   Xm_N_Default_Link_Cursor_Icon      : constant Xt_N_Resource_String;
   Xm_N_Default_Move_Cursor_Icon      : constant Xt_N_Resource_String;
   Xm_N_Default_None_Cursor_Icon      : constant Xt_N_Resource_String;
   Xm_N_Default_Source_Cursor_Icon    : constant Xt_N_Resource_String;
   Xm_N_Default_Valid_Cursor_Icon     : constant Xt_N_Resource_String;
   Xm_N_Font                          : constant Xt_N_Resource_String;
   Xm_N_Foreground_Threshold          : constant Xt_N_Resource_String;
   Xm_N_Horizontal_Font_Unit          : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Insensitive_Stipple_Bitmap    : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Light_Threshold               : constant Xt_N_Resource_String;
   Xm_N_Menu_Cursor                   : constant Xt_N_Resource_String;
   Xm_N_Move_Opaque                   : constant Xt_N_Resource_String;
   Xm_N_Unpost_Behavior               : constant Xt_N_Resource_String;

   type Unpost_Behavior_Type is (Unpost, Unpost_And_Replay);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Unpost_Behavior_Type);
 
   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Unpost_Behavior_Type);
   pragma Convention (C, Append_Get);
 
-- UseMotif2.0 Motif2.1
   Xm_N_Use_Color_Object              : constant Xt_N_Resource_String;
   Xm_N_User_Data                     : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Vertical_Font_Unit            : constant Xt_N_Resource_String;

private

   pragma Import (C, Xm_Get_Xm_Screen, "XmGetXmScreen");

-- UseMotif2.0 Motif2.1
   for Bitmap_Conversion_Model_Type use (Match_Depth => 0, Dynamic_Depth => 1);
   for Bitmap_Conversion_Model_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   for Unpost_Behavior_Type use (Unpost => 0, Unpost_And_Replay => 1);
   for Unpost_Behavior_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   Xm_N_Bitmap_Conversion_Model       : constant Xt_N_Resource_String :=
      To_Resource_String ("bitmapConversionModel");
   Xm_N_Color_Allocation_Proc         : constant Xt_N_Resource_String :=
      To_Resource_String ("colorAllocationProc");
   Xm_N_Color_Calculation_Proc        : constant Xt_N_Resource_String :=
      To_Resource_String ("colorCalculationProc");
-- EndMotif2.0 Motif2.1
   Xm_N_Dark_Threshold                : constant Xt_N_Resource_String :=
      To_Resource_String ("darkThreshold");
   Xm_N_Default_Copy_Cursor_Icon      : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultCopyCursorIcon");
   Xm_N_Default_Invalid_Cursor_Icon   : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultInvalidCursorIcon");
   Xm_N_Default_Link_Cursor_Icon      : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultLinkCursorIcon");
   Xm_N_Default_Move_Cursor_Icon      : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultMoveCursorIcon");
   Xm_N_Default_None_Cursor_Icon      : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultNoneCursorIcon");
   Xm_N_Default_Source_Cursor_Icon    : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultSourceCursorIcon");
   Xm_N_Default_Valid_Cursor_Icon     : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultValidCursorIcon");
   Xm_N_Font                          : constant Xt_N_Resource_String :=
      To_Resource_String ("font");
   Xm_N_Foreground_Threshold          : constant Xt_N_Resource_String :=
      To_Resource_String ("foregroundThreshold");
   Xm_N_Horizontal_Font_Unit          : constant Xt_N_Resource_String :=
      To_Resource_String ("horizontalFontUnit");
-- UseMotif2.0 Motif2.1
   Xm_N_Insensitive_Stipple_Bitmap    : constant Xt_N_Resource_String :=
      To_Resource_String ("insensitiveStippleBitmap");
-- EndMotif2.0 Motif2.1
   Xm_N_Light_Threshold               : constant Xt_N_Resource_String :=
      To_Resource_String ("lightThreshold");
   Xm_N_Menu_Cursor                   : constant Xt_N_Resource_String :=
      To_Resource_String ("menuCursor");
   Xm_N_Move_Opaque                   : constant Xt_N_Resource_String :=
      To_Resource_String ("moveOpaque");
   Xm_N_Unpost_Behavior               : constant Xt_N_Resource_String :=
      To_Resource_String ("unpostBehavior");
-- UseMotif2.0 Motif2.1
   Xm_N_Use_Color_Object              : constant Xt_N_Resource_String :=
      To_Resource_String ("useColorObject");
   Xm_N_User_Data                     : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_User_Data;
-- EndMotif2.0 Motif2.1
   Xm_N_Vertical_Font_Unit            : constant Xt_N_Resource_String :=
      To_Resource_String ("verticalFontUnit");


end Xm_Widgets.Screen;
