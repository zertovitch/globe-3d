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

package X_Lib.Key_Syms is

   XK_VoidSymbol       : constant Key_Sym_ID := 16#ffffff#;              -- void symbol 


   -- -------------------------------------------------------------------------
   --
   --   T T Y   F U N C T I O N S
   --
   -- TTY Functions, cleverly chosen to map to ascii, for convenience of
   -- programming, but could have been arbitrary (at the cost of lookup
   -- tables in client code.
   --

   XK_BackSpace        : constant Key_Sym_ID := 16#ff08#;                -- back space, back char 
   XK_Tab              : constant Key_Sym_ID := 16#ff09#;
   XK_Linefeed         : constant Key_Sym_ID := 16#ff0a#;                -- Linefeed, LF 
   XK_Clear            : constant Key_Sym_ID := 16#ff0b#;
   XK_Return           : constant Key_Sym_ID := 16#ff0d#;                -- Return, enter 
   XK_Pause            : constant Key_Sym_ID := 16#ff13#;                -- Pause, hold 
   XK_Scroll_Lock      : constant Key_Sym_ID := 16#ff14#;
   XK_Sys_Req          : constant Key_Sym_ID := 16#ff15#;
   XK_Escape           : constant Key_Sym_ID := 16#ff1b#;
   XK_Delete           : constant Key_Sym_ID := 16#ffff#;                -- Delete, rubout 

   -- -------------------------------------------------------------------------
   --
   --   I N T E R N A T I O N A L   &   M U L T I - K E Y
   --
   -- International & multi-key character composition
   --

   XK_Multi_key        : constant Key_Sym_ID := 16#ff20#;                -- Multi-key character compose 

   -- Japanese keyboard suport
   XK_Kanji            : constant Key_Sym_ID := 16#ff21#;                -- Kanji, Kanji convert 
   XK_Muhenkan         : constant Key_Sym_ID := 16#ff22#;                -- Cancel Conversion 
   XK_Henkan_Mode      : constant Key_Sym_ID := 16#ff23#;                -- Start/Stop Conversion 
   XK_Henkan           : constant Key_Sym_ID := 16#ff23#;                -- Alias for Henkan_Mode 
   XK_Romaji           : constant Key_Sym_ID := 16#ff24#;                -- to Romaji 
   XK_Hiragana         : constant Key_Sym_ID := 16#ff25#;                -- to Hiragana 
   XK_Katakana         : constant Key_Sym_ID := 16#ff26#;                -- to Katakana 
   XK_Hiragana_Katakana: constant Key_Sym_ID := 16#ff27#;                -- Hiragana/Katakana toggle 
   XK_Zenkaku          : constant Key_Sym_ID := 16#ff28#;                -- to Zenkaku 
   XK_Hankaku          : constant Key_Sym_ID := 16#ff29#;                -- to Hankaku 
   XK_Zenkaku_Hankaku  : constant Key_Sym_ID := 16#ff2a#;                -- Zenkaku/Hankaku toggle 
   XK_Touroku          : constant Key_Sym_ID := 16#ff2b#;                -- Add to Dictionary 
   XK_Massyo           : constant Key_Sym_ID := 16#ff2c#;                -- Delete from Dictionary 
   XK_Kana_Lock        : constant Key_Sym_ID := 16#ff2d#;                -- Kana Lock 
   XK_Kana_Shift       : constant Key_Sym_ID := 16#ff2e#;                -- Kana Shift 
   XK_Eisu_Shift       : constant Key_Sym_ID := 16#ff2f#;                -- Alphanumeric Shift 
   XK_Eisu_toggle      : constant Key_Sym_ID := 16#ff30#;                -- Alphanumeric toggle 

   -- 16#FF31# thru 16#FF3F# are under Korean

   -- -------------------------------------------------------------------------
   --
   --   C U R S O R   C O N T R O L   &   M O T I O N
   --

   XK_Home             : constant Key_Sym_ID := 16#ff50#;
   XK_Left             : constant Key_Sym_ID := 16#ff51#;                -- Move left, left arrow 
   XK_Up               : constant Key_Sym_ID := 16#ff52#;                -- Move up, up arrow 
   XK_Right            : constant Key_Sym_ID := 16#ff53#;                -- Move right, right arrow 
   XK_Down             : constant Key_Sym_ID := 16#ff54#;                -- Move down, down arrow 
   XK_Prior            : constant Key_Sym_ID := 16#ff55#;                -- Prior, previous 
   XK_Page_Up          : constant Key_Sym_ID := 16#ff55#;
   XK_Next             : constant Key_Sym_ID := 16#ff56#;                -- Next 
   XK_Page_Down        : constant Key_Sym_ID := 16#ff56#;
   XK_End              : constant Key_Sym_ID := 16#ff57#;                -- EOL 
   XK_Begin            : constant Key_Sym_ID := 16#ff58#;                -- BOL 

   -- -------------------------------------------------------------------------
   --
   --   M I S C   F U N C T I O N S
   --

   XK_Select           : constant Key_Sym_ID := 16#ff60#;                -- Select, mark 
   XK_Print            : constant Key_Sym_ID := 16#ff61#;
   XK_Execute          : constant Key_Sym_ID := 16#ff62#;                -- Execute, run, do 
   XK_Insert           : constant Key_Sym_ID := 16#ff63#;                -- Insert, insert here 
   XK_Undo             : constant Key_Sym_ID := 16#ff65#;                -- Undo, oops 
   XK_Redo             : constant Key_Sym_ID := 16#ff66#;                -- redo, again 
   XK_Menu             : constant Key_Sym_ID := 16#ff67#;
   XK_Find             : constant Key_Sym_ID := 16#ff68#;                -- Find, search 
   XK_Cancel           : constant Key_Sym_ID := 16#ff69#;                -- Cancel, stop, abort, exit 
   XK_Help             : constant Key_Sym_ID := 16#ff6a#;                -- Help 
   XK_Break            : constant Key_Sym_ID := 16#ff6b#;
   XK_Mode_switch      : constant Key_Sym_ID := 16#ff7e#;                -- Character set switch 
   XK_script_switch    : constant Key_Sym_ID := 16#ff7e#;                -- Alias for mode_switch 
   XK_Num_Lock         : constant Key_Sym_ID := 16#ff7f#;


   -- -------------------------------------------------------------------------
   --
   --   K E Y P A D   F U N C T I O N S
   --
   -- Keypad Functions, keypad numbers cleverly chosen to map to ascii
   --

   XK_KP_Space         : constant Key_Sym_ID := 16#ff80#;                -- space 
   XK_KP_Tab           : constant Key_Sym_ID := 16#ff89#;
   XK_KP_Enter         : constant Key_Sym_ID := 16#ff8d#;                -- enter 
   XK_KP_F1            : constant Key_Sym_ID := 16#ff91#;                -- PF1, KP_A, ... 
   XK_KP_F2            : constant Key_Sym_ID := 16#ff92#;
   XK_KP_F3            : constant Key_Sym_ID := 16#ff93#;
   XK_KP_F4            : constant Key_Sym_ID := 16#ff94#;
   XK_KP_Home          : constant Key_Sym_ID := 16#ff95#;
   XK_KP_Left          : constant Key_Sym_ID := 16#ff96#;
   XK_KP_Up            : constant Key_Sym_ID := 16#ff97#;
   XK_KP_Right         : constant Key_Sym_ID := 16#ff98#;
   XK_KP_Down          : constant Key_Sym_ID := 16#ff99#;
   XK_KP_Prior         : constant Key_Sym_ID := 16#ff9a#;
   XK_KP_Page_Up       : constant Key_Sym_ID := 16#ff9a#;
   XK_KP_Next          : constant Key_Sym_ID := 16#ff9b#;
   XK_KP_Page_Down     : constant Key_Sym_ID := 16#ff9b#;
   XK_KP_End           : constant Key_Sym_ID := 16#ff9c#;
   XK_KP_Begin         : constant Key_Sym_ID := 16#ff9d#;
   XK_KP_Insert        : constant Key_Sym_ID := 16#ff9e#;
   XK_KP_Delete        : constant Key_Sym_ID := 16#ff9f#;
   XK_KP_Equal         : constant Key_Sym_ID := 16#ffbd#;
   XK_KP_Multiply      : constant Key_Sym_ID := 16#ffaa#;
   XK_KP_Add           : constant Key_Sym_ID := 16#ffab#;
   XK_KP_Separator     : constant Key_Sym_ID := 16#ffac#;                -- separator, often comma 
   XK_KP_Subtract      : constant Key_Sym_ID := 16#ffad#;
   XK_KP_Decimal       : constant Key_Sym_ID := 16#ffae#;
   XK_KP_Divide        : constant Key_Sym_ID := 16#ffaf#;

   XK_KP_0             : constant Key_Sym_ID := 16#ffb0#;
   XK_KP_1             : constant Key_Sym_ID := 16#ffb1#;
   XK_KP_2             : constant Key_Sym_ID := 16#ffb2#;
   XK_KP_3             : constant Key_Sym_ID := 16#ffb3#;
   XK_KP_4             : constant Key_Sym_ID := 16#ffb4#;
   XK_KP_5             : constant Key_Sym_ID := 16#ffb5#;
   XK_KP_6             : constant Key_Sym_ID := 16#ffb6#;
   XK_KP_7             : constant Key_Sym_ID := 16#ffb7#;
   XK_KP_8             : constant Key_Sym_ID := 16#ffb8#;
   XK_KP_9             : constant Key_Sym_ID := 16#ffb9#;


   -- -------------------------------------------------------------------------
   --
   --   A U X I L I A R Y   F U N C T I O N S
   --
   -- note the duplicate definitions for left and right function keys;
   -- Sun keyboards and a few other manufactures have such
   -- function key groups on the left and/or right sides of the keyboard.
   -- We've not found a keyboard with more than 35 function keys total.
   --

   XK_F1               : constant Key_Sym_ID := 16#ffbe#;
   XK_F2               : constant Key_Sym_ID := 16#ffbf#;
   XK_F3               : constant Key_Sym_ID := 16#ffc0#;
   XK_F4               : constant Key_Sym_ID := 16#ffc1#;
   XK_F5               : constant Key_Sym_ID := 16#ffc2#;
   XK_F6               : constant Key_Sym_ID := 16#ffc3#;
   XK_F7               : constant Key_Sym_ID := 16#ffc4#;
   XK_F8               : constant Key_Sym_ID := 16#ffc5#;
   XK_F9               : constant Key_Sym_ID := 16#ffc6#;
   XK_F10              : constant Key_Sym_ID := 16#ffc7#;
   XK_F11              : constant Key_Sym_ID := 16#ffc8#;
   XK_L1               : constant Key_Sym_ID := 16#ffc8#;
   XK_F12              : constant Key_Sym_ID := 16#ffc9#;
   XK_L2               : constant Key_Sym_ID := 16#ffc9#;
   XK_F13              : constant Key_Sym_ID := 16#ffca#;
   XK_L3               : constant Key_Sym_ID := 16#ffca#;
   XK_F14              : constant Key_Sym_ID := 16#ffcb#;
   XK_L4               : constant Key_Sym_ID := 16#ffcb#;
   XK_F15              : constant Key_Sym_ID := 16#ffcc#;
   XK_L5               : constant Key_Sym_ID := 16#ffcc#;
   XK_F16              : constant Key_Sym_ID := 16#ffcd#;
   XK_L6               : constant Key_Sym_ID := 16#ffcd#;
   XK_F17              : constant Key_Sym_ID := 16#ffce#;
   XK_L7               : constant Key_Sym_ID := 16#ffce#;
   XK_F18              : constant Key_Sym_ID := 16#ffcf#;
   XK_L8               : constant Key_Sym_ID := 16#ffcf#;
   XK_F19              : constant Key_Sym_ID := 16#ffd0#;
   XK_L9               : constant Key_Sym_ID := 16#ffd0#;
   XK_F20              : constant Key_Sym_ID := 16#ffd1#;
   XK_L10              : constant Key_Sym_ID := 16#ffd1#;
   XK_F21              : constant Key_Sym_ID := 16#ffd2#;
   XK_R1               : constant Key_Sym_ID := 16#ffd2#;
   XK_F22              : constant Key_Sym_ID := 16#ffd3#;
   XK_R2               : constant Key_Sym_ID := 16#ffd3#;
   XK_F23              : constant Key_Sym_ID := 16#ffd4#;
   XK_R3               : constant Key_Sym_ID := 16#ffd4#;
   XK_F24              : constant Key_Sym_ID := 16#ffd5#;
   XK_R4               : constant Key_Sym_ID := 16#ffd5#;
   XK_F25              : constant Key_Sym_ID := 16#ffd6#;
   XK_R5               : constant Key_Sym_ID := 16#ffd6#;
   XK_F26              : constant Key_Sym_ID := 16#ffd7#;
   XK_R6               : constant Key_Sym_ID := 16#ffd7#;
   XK_F27              : constant Key_Sym_ID := 16#ffd8#;
   XK_R7               : constant Key_Sym_ID := 16#ffd8#;
   XK_F28              : constant Key_Sym_ID := 16#ffd9#;
   XK_R8               : constant Key_Sym_ID := 16#ffd9#;
   XK_F29              : constant Key_Sym_ID := 16#ffda#;
   XK_R9               : constant Key_Sym_ID := 16#ffda#;
   XK_F30              : constant Key_Sym_ID := 16#ffdb#;
   XK_R10              : constant Key_Sym_ID := 16#ffdb#;
   XK_F31              : constant Key_Sym_ID := 16#ffdc#;
   XK_R11              : constant Key_Sym_ID := 16#ffdc#;
   XK_F32              : constant Key_Sym_ID := 16#ffdd#;
   XK_R12              : constant Key_Sym_ID := 16#ffdd#;
   XK_F33              : constant Key_Sym_ID := 16#ffde#;
   XK_R13              : constant Key_Sym_ID := 16#ffde#;
   XK_F34              : constant Key_Sym_ID := 16#ffdf#;
   XK_R14              : constant Key_Sym_ID := 16#ffdf#;
   XK_F35              : constant Key_Sym_ID := 16#ffe0#;
   XK_R15              : constant Key_Sym_ID := 16#ffe0#;

   -- -------------------------------------------------------------------------
   --
   --   M O D I F I E R S
   --
   XK_Shift_L          : constant Key_Sym_ID := 16#ffe1#;
   XK_Shift_R          : constant Key_Sym_ID := 16#ffe2#;
   XK_Control_L        : constant Key_Sym_ID := 16#ffe3#;
   XK_Control_R        : constant Key_Sym_ID := 16#ffe4#;
   XK_Caps_Lock        : constant Key_Sym_ID := 16#ffe5#;
   XK_Shift_Lock       : constant Key_Sym_ID := 16#ffe6#;

   XK_Meta_L           : constant Key_Sym_ID := 16#ffe7#;
   XK_Meta_R           : constant Key_Sym_ID := 16#ffe8#;
   XK_Alt_L            : constant Key_Sym_ID := 16#ffe9#;
   XK_Alt_R            : constant Key_Sym_ID := 16#ffea#;
   XK_Super_L          : constant Key_Sym_ID := 16#ffeb#;
   XK_Super_R          : constant Key_Sym_ID := 16#ffec#;
   XK_Hyper_L          : constant Key_Sym_ID := 16#ffed#;
   XK_Hyper_R          : constant Key_Sym_ID := 16#ffee#;


   -- -------------------------------------------------------------------------
   --
   --   testing functions
   --
   function Is_Keypad_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_Private_Keypad_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_Cursor_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_PF_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_Function_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_Misc_Function_Key (Keysym : in Key_Sym_ID) return Boolean;

   function Is_Modifier_Key (Keysym : in Key_Sym_ID) return Boolean;


end X_Lib.Key_Syms;
