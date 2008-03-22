-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
--  This file also copyright (c) 2001 Vadim Godunko                          --
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
--          19 May 2001 Vadim Godunko: first definition of this file
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Row_Column.Simple is


   function Xm_Create_Simple_Check_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Simple_Menu_Bar
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Simple_Option_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Simple_Popup_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Simple_Pulldown_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Simple_Radio_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   
   Xm_N_Button_Accelerators       : constant Xt_N_Resource_String;
   Xm_N_Button_Accelerator_Text   : constant Xt_N_Resource_String;
   Xm_N_Button_Count              : constant Xt_N_Resource_String;
   Xm_N_Button_Mnemonic_Char_Sets : constant Xt_N_Resource_String;
   Xm_N_Button_Mnemonics          : constant Xt_N_Resource_String;
   Xm_N_Buttons                   : constant Xt_N_Resource_String;
   Xm_N_Button_Set                : constant Xt_N_Resource_String;
   Xm_N_Button_Type               : constant Xt_N_Resource_String;

   type Button_Type is
     (Push_Button, Check_Button, Radio_Button, Cascade_Button,
      Separator, Double_Separator, Title);

   type Button_Type_Table is array (Natural range <>) of Button_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Button_Type_Table);

   Xm_N_Option_Label              : constant Xt_N_Resource_String;
   Xm_N_Option_Mnemonics          : constant Xt_N_Resource_String;
   Xm_N_Post_From_Button          : constant Xt_N_Resource_String;
   Xm_N_Simple_Callback           : constant Xt_N_Resource_String;


private

   for Button_Type use
     (Push_Button => 1, Check_Button => 2,
      Radio_Button => 3, Cascade_Button => 4,
      Separator => 5, Double_Separator => 6, Title => 7);
   for Button_Type'Size use Interfaces.C.unsigned_char'Size;

   Xm_N_Button_Accelerators       : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonAccelerators");
   Xm_N_Button_Accelerator_Text   : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonAcceleratorText");
   Xm_N_Button_Count              : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonCount");
   Xm_N_Button_Mnemonic_Char_Sets : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonMnemonicCharSets");
   Xm_N_Button_Mnemonics          : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonMnemonics");
   Xm_N_Buttons                   : constant Xt_N_Resource_String :=
      To_Resource_String ("buttons");
   Xm_N_Button_Set                : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonSet");
   Xm_N_Button_Type               : constant Xt_N_Resource_String :=
      To_Resource_String ("buttonType");
   Xm_N_Option_Label              : constant Xt_N_Resource_String :=
      To_Resource_String ("optionLabel");
   Xm_N_Option_Mnemonics          : constant Xt_N_Resource_String :=
      To_Resource_String ("optionMnemonic");
   Xm_N_Post_From_Button          : constant Xt_N_Resource_String :=
      To_Resource_String ("postFromButton");
   Xm_N_Simple_Callback           : constant Xt_N_Resource_String :=
      To_Resource_String ("simpleCallback");

end Xm_Widgets.Manager.Row_Column.Simple;
