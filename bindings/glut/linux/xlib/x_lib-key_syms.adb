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

package body X_Lib.Key_Syms is

   function Is_Keypad_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in XK_KP_Space .. XK_KP_Equal;
   end Is_Keypad_Key;


   function Is_Private_Keypad_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in 16#11000000# .. 16#1100FFFF#;
   end Is_Private_Keypad_Key;


   function Is_Cursor_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in XK_Home .. Key_Sym_ID'Pred (XK_Select);
   end Is_Cursor_Key;


   function Is_PF_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in XK_KP_F1 .. XK_KP_F4;
   end Is_PF_Key;


   function Is_Function_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in XK_F1 .. XK_F35;
   end Is_Function_Key;


   function Is_Misc_Function_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return Keysym in XK_Select .. XK_Break;
   end Is_Misc_Function_Key;


   function Is_Modifier_Key (Keysym : in Key_Sym_ID) return Boolean is
   begin
      return keysym in XK_Shift_L .. XK_Hyper_R or else
             keysym = XK_Mode_Switch or else
	     keysym = XK_Num_Lock;
   end Is_Modifier_Key;


end X_Lib.Key_Syms;
