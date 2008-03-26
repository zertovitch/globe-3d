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

with Interfaces.C;
with X_Toolkit.Internal;

package body Xm_Widgets.Manager.Row_Column.Simple is


   function Xm_Create_Simple_Check_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimpleCheckBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimpleCheckBox, "XmCreateSimpleCheckBox");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimpleCheckBox (Parent,
                                     Name_String'Address,
                                     X_Toolkit.Internal.Hook (Arglist),
                                     Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Check_Box;


   function Xm_Create_Simple_Menu_Bar
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimpleMenuBar
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimpleMenuBar, "XmCreateSimpleMenuBar");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimpleMenuBar (Parent,
                                    Name_String'Address,
                                    X_Toolkit.Internal.Hook (Arglist),
                                    Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Menu_Bar;


   function Xm_Create_Simple_Option_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimpleOptionMenu
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimpleOptionMenu, "XmCreateSimpleOptionMenu");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimpleOptionMenu (Parent,
                                       Name_String'Address,
                                       X_Toolkit.Internal.Hook (Arglist),
                                       Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Option_Menu;


   function Xm_Create_Simple_Popup_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimplePopupMenu
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimplePopupMenu, "XmCreateSimplePopupMenu");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimplePopupMenu (Parent,
                                      Name_String'Address,
                                      X_Toolkit.Internal.Hook (Arglist),
                                      Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Popup_Menu;


   function Xm_Create_Simple_Pulldown_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimplePulldownMenu
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimplePulldownMenu, "XmCreateSimplePulldownMenu");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimplePulldownMenu (Parent,
                                         Name_String'Address,
                                         X_Toolkit.Internal.Hook (Arglist),
                                         Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Pulldown_Menu;


   function Xm_Create_Simple_Radio_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimpleRadioBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimpleRadioBox, "XmCreateSimpleRadioBox");

      Name_String : constant Interfaces.C.Char_Array :=
         Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimpleRadioBox (Parent,
                                     Name_String'Address,
                                     X_Toolkit.Internal.Hook (Arglist),
                                     Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Radio_Box;


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Button_Type_Table) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value (Value'First)'Address);
   end Append_Set;

end Xm_Widgets.Manager.Row_Column.Simple;
