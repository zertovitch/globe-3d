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

package X_Lib.Key_Syms.ISO9995 is

   -- -------------------------------------------------------------------------
   --
   --   ISO 9995  F U N C T I O N   A N D   M O D I F I E R   K E Y S
   --
   -- Byte 3 is 16#FE#
   --
   XK_ISO_Lock                   : constant Key_Sym_ID := 16#fe01#;
   XK_ISO_Level2_Latch           : constant Key_Sym_ID := 16#fe02#;
   XK_ISO_Level3_Shift           : constant Key_Sym_ID := 16#fe03#;
   XK_ISO_Level3_Latch           : constant Key_Sym_ID := 16#fe04#;
   XK_ISO_Level3_Lock            : constant Key_Sym_ID := 16#fe05#;
   XK_ISO_Group_Shift            : constant Key_Sym_ID := 16#ff7e#; -- Alias for mode_switch 
   XK_ISO_Group_Latch            : constant Key_Sym_ID := 16#fe06#;
   XK_ISO_Group_Lock             : constant Key_Sym_ID := 16#fe07#;
   XK_ISO_Next_Group             : constant Key_Sym_ID := 16#fe08#;
   XK_ISO_Next_Group_Lock        : constant Key_Sym_ID := 16#fe09#;
   XK_ISO_Prev_Group             : constant Key_Sym_ID := 16#fe0a#;
   XK_ISO_Prev_Group_Lock        : constant Key_Sym_ID := 16#fe0b#;
   XK_ISO_First_Group            : constant Key_Sym_ID := 16#fe0c#;
   XK_ISO_First_Group_Lock       : constant Key_Sym_ID := 16#fe0d#;
   XK_ISO_Last_Group             : constant Key_Sym_ID := 16#fe0e#;
   XK_ISO_Last_Group_Lock        : constant Key_Sym_ID := 16#fe0f#;

   XK_ISO_Left_Tab               : constant Key_Sym_ID := 16#fe20#;
   XK_ISO_Move_Line_Up           : constant Key_Sym_ID := 16#fe21#;
   XK_ISO_Move_Line_Down         : constant Key_Sym_ID := 16#fe22#;
   XK_ISO_Partial_Line_Up        : constant Key_Sym_ID := 16#fe23#;
   XK_ISO_Partial_Line_Down      : constant Key_Sym_ID := 16#fe24#;
   XK_ISO_Partial_Space_Left     : constant Key_Sym_ID := 16#fe25#;
   XK_ISO_Partial_Space_Right    : constant Key_Sym_ID := 16#fe26#;
   XK_ISO_Set_Margin_Left        : constant Key_Sym_ID := 16#fe27#;
   XK_ISO_Set_Margin_Right       : constant Key_Sym_ID := 16#fe28#;
   XK_ISO_Release_Margin_Left    : constant Key_Sym_ID := 16#fe29#;
   XK_ISO_Release_Margin_Right   : constant Key_Sym_ID := 16#fe2a#;
   XK_ISO_Release_Both_Margins   : constant Key_Sym_ID := 16#fe2b#;
   XK_ISO_Fast_Cursor_Left       : constant Key_Sym_ID := 16#fe2c#;
   XK_ISO_Fast_Cursor_Right      : constant Key_Sym_ID := 16#fe2d#;
   XK_ISO_Fast_Cursor_Up         : constant Key_Sym_ID := 16#fe2e#;
   XK_ISO_Fast_Cursor_Down       : constant Key_Sym_ID := 16#fe2f#;
   XK_ISO_Continuous_Underline   : constant Key_Sym_ID := 16#fe30#;
   XK_ISO_Discontinuous_Underline: constant Key_Sym_ID := 16#fe31#;
   XK_ISO_Emphasize              : constant Key_Sym_ID := 16#fe32#;
   XK_ISO_Center_Object          : constant Key_Sym_ID := 16#fe33#;
   XK_ISO_Enter                  : constant Key_Sym_ID := 16#fe34#;

   XK_dead_grave                 : constant Key_Sym_ID := 16#fe50#;
   XK_dead_acute                 : constant Key_Sym_ID := 16#fe51#;
   XK_dead_circumflex            : constant Key_Sym_ID := 16#fe52#;
   XK_dead_tilde                 : constant Key_Sym_ID := 16#fe53#;
   XK_dead_macron                : constant Key_Sym_ID := 16#fe54#;
   XK_dead_breve                 : constant Key_Sym_ID := 16#fe55#;
   XK_dead_abovedot              : constant Key_Sym_ID := 16#fe56#;
   XK_dead_diaeresis             : constant Key_Sym_ID := 16#fe57#;
   XK_dead_abovering             : constant Key_Sym_ID := 16#fe58#;
   XK_dead_doubleacute           : constant Key_Sym_ID := 16#fe59#;
   XK_dead_caron                 : constant Key_Sym_ID := 16#fe5a#;
   XK_dead_cedilla               : constant Key_Sym_ID := 16#fe5b#;
   XK_dead_ogonek                : constant Key_Sym_ID := 16#fe5c#;
   XK_dead_iota                  : constant Key_Sym_ID := 16#fe5d#;
   XK_dead_voiced_sound          : constant Key_Sym_ID := 16#fe5e#;
   XK_dead_semivoiced_sound      : constant Key_Sym_ID := 16#fe5f#;
   XK_dead_belowdot              : constant Key_Sym_ID := 16#fe60#;

   XK_First_Virtual_Screen       : constant Key_Sym_ID := 16#fed0#;
   XK_Prev_Virtual_Screen        : constant Key_Sym_ID := 16#fed1#;
   XK_Next_Virtual_Screen        : constant Key_Sym_ID := 16#fed2#;
   XK_Last_Virtual_Screen        : constant Key_Sym_ID := 16#fed4#;
   XK_Terminate_Server           : constant Key_Sym_ID := 16#fed5#;

   XK_AccessX_Enable             : constant Key_Sym_ID := 16#fe70#;
   XK_AccessX_Feedback_Enable    : constant Key_Sym_ID := 16#fe71#;
   XK_RepeatKeys_Enable          : constant Key_Sym_ID := 16#fe72#;
   XK_SlowKeys_Enable            : constant Key_Sym_ID := 16#fe73#;
   XK_BounceKeys_Enable          : constant Key_Sym_ID := 16#fe74#;
   XK_StickyKeys_Enable          : constant Key_Sym_ID := 16#fe75#;
   XK_MouseKeys_Enable           : constant Key_Sym_ID := 16#fe76#;
   XK_MouseKeys_Accel_Enable     : constant Key_Sym_ID := 16#fe77#;
   XK_Overlay1_Enable            : constant Key_Sym_ID := 16#fe78#;
   XK_Overlay2_Enable            : constant Key_Sym_ID := 16#fe79#;
   XK_AudibleBell_Enable         : constant Key_Sym_ID := 16#fe7a#;

   XK_Pointer_Left               : constant Key_Sym_ID := 16#fee0#;
   XK_Pointer_Right              : constant Key_Sym_ID := 16#fee1#;
   XK_Pointer_Up                 : constant Key_Sym_ID := 16#fee2#;
   XK_Pointer_Down               : constant Key_Sym_ID := 16#fee3#;
   XK_Pointer_UpLeft             : constant Key_Sym_ID := 16#fee4#;
   XK_Pointer_UpRight            : constant Key_Sym_ID := 16#fee5#;
   XK_Pointer_DownLeft           : constant Key_Sym_ID := 16#fee6#;
   XK_Pointer_DownRight          : constant Key_Sym_ID := 16#fee7#;
   XK_Pointer_Button_Dflt        : constant Key_Sym_ID := 16#fee8#;
   XK_Pointer_Button1            : constant Key_Sym_ID := 16#fee9#;
   XK_Pointer_Button2            : constant Key_Sym_ID := 16#feea#;
   XK_Pointer_Button3            : constant Key_Sym_ID := 16#feeb#;
   XK_Pointer_Button4            : constant Key_Sym_ID := 16#feec#;
   XK_Pointer_Button5            : constant Key_Sym_ID := 16#feed#;
   XK_Pointer_DblClick_Dflt      : constant Key_Sym_ID := 16#feee#;
   XK_Pointer_DblClick1          : constant Key_Sym_ID := 16#feef#;
   XK_Pointer_DblClick2          : constant Key_Sym_ID := 16#fef0#;
   XK_Pointer_DblClick3          : constant Key_Sym_ID := 16#fef1#;
   XK_Pointer_DblClick4          : constant Key_Sym_ID := 16#fef2#;
   XK_Pointer_DblClick5          : constant Key_Sym_ID := 16#fef3#;
   XK_Pointer_Drag_Dflt          : constant Key_Sym_ID := 16#fef4#;
   XK_Pointer_Drag1              : constant Key_Sym_ID := 16#fef5#;
   XK_Pointer_Drag2              : constant Key_Sym_ID := 16#fef6#;
   XK_Pointer_Drag3              : constant Key_Sym_ID := 16#fef7#;
   XK_Pointer_Drag4              : constant Key_Sym_ID := 16#fef8#;
   XK_Pointer_Drag5              : constant Key_Sym_ID := 16#fefd#;

   XK_Pointer_EnableKeys         : constant Key_Sym_ID := 16#fef9#;
   XK_Pointer_Accelerate         : constant Key_Sym_ID := 16#fefa#;
   XK_Pointer_DfltBtnNext        : constant Key_Sym_ID := 16#fefb#;
   XK_Pointer_DfltBtnPrev        : constant Key_Sym_ID := 16#fefc#;

end X_Lib.Key_Syms.ISO9995;
