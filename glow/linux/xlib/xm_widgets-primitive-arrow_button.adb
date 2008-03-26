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

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Primitive.Arrow_Button is


   --
   -- convertion function
   --
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Arrow_Button_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Arrow_Button_Callback_Struct,
            Xm_Arrow_Button_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Activate,
	                                2 => Cr_Arm,
					3 => Cr_Disarm));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Is_Arrow_Button (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Arrow_Button_Widget_Class);
   end Xm_Is_Arrow_Button;


   function Xm_Is_Arrow_Button_Gadget (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Arrow_Button_Gadget_Class);
   end Xm_Is_Arrow_Button_Gadget;



   function Xm_Create_Arrow_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateArrowButton
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateArrowButton, "XmCreateArrowButton");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateArrowButton (Parent,
                                  Name_String'Address,
                                  X_Toolkit.Internal.Hook (Arglist),
                                  Cardinal (Length (Arglist)));
   end Xm_Create_Arrow_Button;


   function Xm_Create_Arrow_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateArrowButtonGadget
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateArrowButtonGadget, "XmCreateArrowButtonGadget");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateArrowButtonGadget (Parent,
                                  Name_String'Address,
                                  X_Toolkit.Internal.Hook (Arglist),
                                  Cardinal (Length (Arglist)));
   end Xm_Create_Arrow_Button_Gadget;


   function To_Integer is
      new Ada.Unchecked_Conversion (Arrow_Direction_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Arrow_Direction_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Arrow_Direction_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


end Xm_Widgets.Primitive.Arrow_Button;
