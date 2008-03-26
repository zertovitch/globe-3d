-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
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
--          19 May 2001 Vadim Godunko: implement Xm_Combo_Box_Add_Item
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Manager.Combo_Box is
 
-- UseMotif2.0 Motif2.1

   function Xm_Is_Combo_Box (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Combo_Box_Widget_Class);
   end Xm_Is_Combo_Box;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Combo_Box_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Combo_Box_Callback_Struct,
             Xm_Combo_Box_Callback_Struct_Access,
             Callback_Reason_Array_Type'(1 => Cr_Select));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);





   function Xm_Create_Combo_Box
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateComboBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateComboBox, "XmCreateComboBox");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateComboBox (Parent,
                               Name_String'Address,
                               X_Toolkit.Internal.Hook (Arglist),
                               Cardinal (Length (Arglist)));
   end Xm_Create_Combo_Box;


   function Xm_Create_Drop_Down_Combo_Box
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateDropDownComboBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateDropDownComboBox, "XmCreateDropDownComboBox");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateDropDownComboBox (Parent,
                                       Name_String'Address,
                                       X_Toolkit.Internal.Hook (Arglist),
                                       Cardinal (Length (Arglist)));
   end Xm_Create_Drop_Down_Combo_Box;


   function Xm_Create_Drop_Down_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateDropDownList
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateDropDownList, "XmCreateDropDownList");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateDropDownList (Parent,
                                   Name_String'Address,
                                   X_Toolkit.Internal.Hook (Arglist),
                                   Cardinal (Length (Arglist)));
   end Xm_Create_Drop_Down_List;

-- UseMotif2.1
   procedure Xm_Combo_Box_Add_Item
     (W      : in Widget;
      Item   : in Xm_String;
      Pos    : in Integer;
      Unique : in Boolean) is
      procedure XmComboBoxAddItem
	(W	: in Widget;
	 Item	: in Xm_String;
	 Pos	: in Integer;
	 Unique : in Xt_Boolean);
      pragma Import (C, XmComboBoxAddItem, "XmComboBoxAddItem");
   begin
      XmComboBoxAddItem (W, Item, Pos, To_Xt_Boolean (Unique));
   end Xm_Combo_Box_Add_Item;
   pragma Inline (Xm_Combo_Box_Add_Item);
-- EndMotif2.1


   function To_Integer is
      new Ada.Unchecked_Conversion (Combo_Box_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Combo_Box_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Combo_Box_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Match_Behavior_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Match_Behavior_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Match_Behavior_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- UseMotif2.1
   function To_Integer is
      new Ada.Unchecked_Conversion (Position_Mode_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Position_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Position_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;

-- EndMotif2.1

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Combo_Box;
