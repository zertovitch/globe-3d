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

with Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Primitive.Label.Cascade_Button is


   function Xm_Is_Cascade_Button (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Cascade_Button_Widget_Class);
   end Xm_Is_Cascade_Button;


   function Xm_Is_Cascade_Button_Gadget (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Cascade_Button_Gadget_Class);
   end Xm_Is_Cascade_Button_Gadget;


   function Xm_Create_Cascade_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateCascadeButton
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateCascadeButton, "XmCreateCascadeButton");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateCascadeButton (Parent,
                                    Name_String'Address,
                                    X_Toolkit.Internal.Hook (Arglist),
                                    Cardinal (Length (Arglist)));
   end Xm_Create_Cascade_Button;


   function Xm_Create_Cascade_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateCascadeButtonGadget
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateCascadeButtonGadget, "XmCreateCascadeButtonGadget");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateCascadeButtonGadget (Parent,
                                    Name_String'Address,
                                    X_Toolkit.Internal.Hook (Arglist),
                                    Cardinal (Length (Arglist)));
   end Xm_Create_Cascade_Button_Gadget;



   procedure Xm_Cascade_Button_Highlight
     (Button    : in Widget;
      Highlight : in Boolean) is
      procedure XmCascadeButtonHighlight
        (Button    : in Widget;
         Highlight : in Xt_Boolean);
      pragma Import (C, XmCascadeButtonHighlight, "XmCascadeButtonHighlight");
   begin
      XmCascadeButtonHighlight (Button, To_Xt_Boolean (Highlight));
   end Xm_Cascade_Button_Highlight;


   procedure Xm_Cascade_Button_Gadget_Highlight
     (Button    : in Widget;
      Highlight : in Boolean) is
      procedure XmCascadeButtonGadgetHighlight
        (Button    : in Widget;
         Highlight : in Xt_Boolean);
      pragma Import (C, XmCascadeButtonGadgetHighlight, "XmCascadeButtonGadgetHighlight");
   begin
      XmCascadeButtonGadgetHighlight (Button, To_Xt_Boolean (Highlight));
   end Xm_Cascade_Button_Gadget_Highlight;


end Xm_Widgets.Primitive.Label.Cascade_Button;
