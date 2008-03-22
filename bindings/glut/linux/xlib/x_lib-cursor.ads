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

package X_Lib.Cursor is

   type Font_Cursor_Shape is (
      XC_X_cursor,
      XC_arrow,
      XC_based_arrow_down,
      XC_based_arrow_up,
      XC_boat,
      XC_bogosity,
      XC_bottom_left_corner,
      XC_bottom_right_corner,
      XC_bottom_side,
      XC_bottom_tee,
      XC_box_spiral,
      XC_center_ptr,
      XC_circle,
      XC_clock,
      XC_coffee_mug,
      XC_cross,
      XC_cross_reverse,
      XC_crosshair,
      XC_diamond_cross,
      XC_dot,
      XC_dotbox,
      XC_double_arrow,
      XC_draft_large,
      XC_draft_small,
      XC_draped_box,
      XC_exchange,
      XC_fleur,
      XC_gobbler,
      XC_gumby,
      XC_hand1,
      XC_hand2,
      XC_heart,
      XC_icon,
      XC_iron_cross,
      XC_left_ptr,
      XC_left_side,
      XC_left_tee,
      XC_leftbutton,
      XC_ll_angle,
      XC_lr_angle,
      XC_man,
      XC_middlebutton,
      XC_mouse,
      XC_pencil,
      XC_pirate,
      XC_plus,
      XC_question_arrow,
      XC_right_ptr,
      XC_right_side,
      XC_right_tee,
      XC_rightbutton,
      XC_rtl_logo,
      XC_sailboat,
      XC_sb_down_arrow,
      XC_sb_h_double_arrow,
      XC_sb_left_arrow,
      XC_sb_right_arrow,
      XC_sb_up_arrow,
      XC_sb_v_double_arrow,
      XC_shuttle,
      XC_sizing,
      XC_spider,
      XC_spraycan,
      XC_star,
      XC_target,
      XC_tcross,
      XC_top_left_arrow,
      XC_top_left_corner,
      XC_top_right_corner,
      XC_top_side,
      XC_top_tee,
      XC_trek,
      XC_ul_angle,
      XC_umbrella,
      XC_ur_angle,
      XC_watch,
      XC_xterm
                             );


   function X_Create_Font_Cursor
     (Display     : in Display_Pointer;
      Shape       : in Font_Cursor_Shape)
      return Cursor_ID;

   function X_Create_Pixmap_Cursor
     (Display          : in Display_Pointer;
      Source           : in Pixmap_ID;
      Mask             : in Pixmap_ID;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color;
      X_Hot, Y_Hot     : in Natural_Position)
      return Cursor_ID;

   function X_Create_Glyph_Cursor
     (Display          : in Display_Pointer;
      Source_Font      : in Font_ID;
      Mask_Font        : in Font_ID;
      Source_Char      : in Interfaces.C.unsigned;
      Mask_Char        : in Interfaces.C.unsigned;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color)
      return Cursor_ID;


   procedure X_Define_Cursor
     (Display     : in Display_Pointer;
      Window      : in Window_ID;
      Cursor      : in Cursor_ID);

   procedure X_Undefine_Cursor
     (Display     : in Display_Pointer;
      Window      : in Window_ID);

   procedure X_Free_Cursor
     (Display     : in Display_Pointer;
      Cursor      : in Cursor_ID);


   procedure X_Query_Best_Cursor
     (Display     : in     Display_Pointer;
      D           : in     Drawable_ID;
      Width,
      Height      : in     Dimension;
      Best_Width,
      Best_Height :    out Dimension);


   procedure X_Recolor_Cursor
     (Display          : in Display_Pointer;
      Cursor           : in Cursor_ID;
      Foreground_Color : in X_Color;
      Background_Color : in X_Color);

private

   for Font_Cursor_Shape use (
      XC_X_cursor              => 0,
      XC_arrow                 => 2,
      XC_based_arrow_down      => 4,
      XC_based_arrow_up        => 6,
      XC_boat                  => 8,
      XC_bogosity              => 10,
      XC_bottom_left_corner    => 12,
      XC_bottom_right_corner   => 14,
      XC_bottom_side           => 16,
      XC_bottom_tee            => 18,
      XC_box_spiral            => 20,
      XC_center_ptr            => 22,
      XC_circle                => 24,
      XC_clock                 => 26,
      XC_coffee_mug            => 28,
      XC_cross                 => 30,
      XC_cross_reverse         => 32,
      XC_crosshair             => 34,
      XC_diamond_cross         => 36,
      XC_dot                   => 38,
      XC_dotbox                => 40,
      XC_double_arrow          => 42,
      XC_draft_large           => 44,
      XC_draft_small           => 46,
      XC_draped_box            => 48,
      XC_exchange              => 50,
      XC_fleur                 => 52,
      XC_gobbler               => 54,
      XC_gumby                 => 56,
      XC_hand1                 => 58,
      XC_hand2                 => 60,
      XC_heart                 => 62,
      XC_icon                  => 64,
      XC_iron_cross            => 66,
      XC_left_ptr              => 68,
      XC_left_side             => 70,
      XC_left_tee              => 72,
      XC_leftbutton            => 74,
      XC_ll_angle              => 76,
      XC_lr_angle              => 78,
      XC_man                   => 80,
      XC_middlebutton          => 82,
      XC_mouse                 => 84,
      XC_pencil                => 86,
      XC_pirate                => 88,
      XC_plus                  => 90,
      XC_question_arrow        => 92,
      XC_right_ptr             => 94,
      XC_right_side            => 96,
      XC_right_tee             => 98,
      XC_rightbutton           => 100,
      XC_rtl_logo              => 102,
      XC_sailboat              => 104,
      XC_sb_down_arrow         => 106,
      XC_sb_h_double_arrow     => 108,
      XC_sb_left_arrow         => 110,
      XC_sb_right_arrow        => 112,
      XC_sb_up_arrow           => 114,
      XC_sb_v_double_arrow     => 116,
      XC_shuttle               => 118,
      XC_sizing                => 120,
      XC_spider                => 122,
      XC_spraycan              => 124,
      XC_star                  => 126,
      XC_target                => 128,
      XC_tcross                => 130,
      XC_top_left_arrow        => 132,
      XC_top_left_corner       => 134,
      XC_top_right_corner      => 136,
      XC_top_side              => 138,
      XC_top_tee               => 140,
      XC_trek                  => 142,
      XC_ul_angle              => 144,
      XC_umbrella              => 146,
      XC_ur_angle              => 148,
      XC_watch                 => 150,
      XC_xterm                 => 152
                             );
   for Font_Cursor_Shape'Size use Interfaces.C.unsigned'Size;


   pragma Import (C, X_Create_Font_Cursor, "XCreateFontCursor");
   pragma Import (C, X_Define_Cursor, "XDefineCursor");
   pragma Import (C, X_Undefine_Cursor, "XUndefineCursor");
   pragma Import (C, X_Free_Cursor, "XFreeCursor");


end X_Lib.Cursor;
