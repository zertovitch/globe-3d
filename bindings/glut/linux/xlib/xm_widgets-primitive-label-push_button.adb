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
package body Xm_Widgets.Primitive.Label.Push_Button is

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Push_Button_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Push_Button_Callback_Struct,
            Xm_Push_Button_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Activate,
	                                2 => Cr_Arm,
					3 => Cr_Disarm));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Is_Push_Button (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Push_Button_Widget_Class);
   end Xm_Is_Push_Button;

   function Xm_Is_Push_Button_Gadget (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Push_Button_Gadget_Class);
   end Xm_Is_Push_Button_Gadget;


   function Xm_Create_Push_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreatePushButton
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreatePushButton, "XmCreatePushButton");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreatePushButton (Parent,
                                 Name_String'Address,
                                 X_Toolkit.Internal.Hook (Arglist),
                                 Cardinal (Length (Arglist)));
   end Xm_Create_Push_Button;


   function Xm_Create_Push_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreatePushButtonGadget
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreatePushButtonGadget, "XmCreatePushButtonGadget");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreatePushButtonGadget (Parent,
                                 Name_String'Address,
                                 X_Toolkit.Internal.Hook (Arglist),
                                 Cardinal (Length (Arglist)));
   end Xm_Create_Push_Button_Gadget;

end Xm_Widgets.Primitive.Label.Push_Button;
