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
--          19 May 2001 Vadim Godunko: new procedure Xm_Toggle_Button_Set_Value
--          26 Aug 2001 H.-F. Vogt: Xm_Toggle_Button_Gadget_Set_Value and
--                                  better handling of Set_Type
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Primitive.Label.Toggle_Button is


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Toggle_Button_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Toggle_Button_Callback_Struct,
             Xm_Toggle_Button_Callback_Struct_Access,
             Callback_Reason_Array_Type'(1 => Cr_Value_Changed,
	                                 2 => Cr_Arm,
					 3 => Cr_Disarm));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Is_Toggle_Button (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Toggle_Button_Widget_Class);
   end Xm_Is_Toggle_Button;

   function Xm_Is_Toggle_Button_Gadget (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Toggle_Button_Gadget_Class);
   end Xm_Is_Toggle_Button_Gadget;


   function Xm_Create_Toggle_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateToggleButton
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal) return Widget;
      pragma Import (C, XmCreateToggleButton, "XmCreateToggleButton");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateToggleButton (Parent,
                                   Name_String'Address,
                                   X_Toolkit.Internal.Hook (Arglist),
                                   Cardinal (Length (Arglist)));
   end Xm_Create_Toggle_Button;


   function Xm_Create_Toggle_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateToggleButtonGadget
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal) return Widget;
      pragma Import (C, XmCreateToggleButtonGadget, "XmCreateToggleButtonGadget");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateToggleButtonGadget (Parent,
                                   Name_String'Address,
                                   X_Toolkit.Internal.Hook (Arglist),
                                   Cardinal (Length (Arglist)));
   end Xm_Create_Toggle_Button_Gadget;
 

   -- -------------------------------------------------------------------------
   --
   -- get/set state
   --

   function Xm_Toggle_Button_Get_State (W : in Widget) return Boolean is
      function XmToggleButtonGetState (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmToggleButtonGetState,"XmToggleButtonGetState");
   begin
      return To_Boolean (XmToggleButtonGetState (W));
   end Xm_Toggle_Button_Get_State;


   function Xm_Toggle_Button_Gadget_Get_State (W : in Widget) return Boolean is
      function XmToggleButtonGadgetGetState (W : in Widget) return Xt_Boolean; 
      pragma Import (C, XmToggleButtonGadgetGetState,"XmToggleButtonGadgetGetState");
   begin
      return To_Boolean (XmToggleButtonGadgetGetState (W));
   end Xm_Toggle_Button_Gadget_Get_State;


   procedure Xm_Toggle_Button_Gadget_Set_State
     (W      : in Widget;
      State  : in Boolean;
      Notify : in Boolean) is
      procedure XmToggleButtonGadgetSetState
        (W      : in Widget;
         State  : in Xt_Boolean;
         Notify : in Xt_Boolean);
      pragma Import (C, XmToggleButtonGadgetSetState,"XmToggleButtonGadgetSetState");
   begin
      XmToggleButtonGadgetSetState (W, To_Xt_Boolean (State), To_Xt_Boolean (Notify));
   end Xm_Toggle_Button_Gadget_Set_State;


   procedure Xm_Toggle_Button_Set_State
     (W      : in Widget;
      State  : in Boolean;
      Notify : in Boolean) is
      procedure XmToggleButtonSetState
        (W      : in Widget;
         State  : in Xt_Boolean;
         Notify : in Xt_Boolean);
      pragma Import (C, XmToggleButtonSetState,"XmToggleButtonSetState");
   begin
      XmToggleButtonSetState (W, To_Xt_Boolean (State), To_Xt_Boolean (Notify));
   end Xm_Toggle_Button_Set_State;


-- UseMotif2.0 Motif2.1
   procedure Xm_Toggle_Button_Set_Value
     (W      : in Widget;
      State  : in Set_Type;
      Notify : in Boolean) is
      function XmToggleButtonSetValue
	(W	: in Widget;
	 State  : in Set_Type;
	 Notify : in Xt_Boolean)
         return Xt_Boolean;
      pragma Import (C, XmToggleButtonSetValue, "XmToggleButtonSetValue");
   begin
      if not To_Boolean (XmToggleButtonSetValue (W, State, To_Xt_Boolean (Notify))) then
         raise Invalid_State_Error;
      end if;
   end Xm_Toggle_Button_Set_Value;

   procedure Xm_Toggle_Button_Gadget_Set_Value
     (W      : in Widget;
      State  : in Set_Type;
      Notify : in Boolean) is
      function XmToggleButtonGadgetSetValue
	(W	: in Widget;
	 State  : in Set_Type;
	 Notify : in Xt_Boolean)
         return Xt_Boolean;
      pragma Import (C, XmToggleButtonGadgetSetValue, "XmToggleButtonGadgetSetValue");
   begin
      if not To_Boolean (XmToggleButtonGadgetSetValue (W, State, To_Xt_Boolean (Notify))) then
         raise Invalid_State_Error;
      end if;
   end Xm_Toggle_Button_Gadget_Set_Value;
-- EndMotif2.0 Motif2.1


   function To_Integer is
      new Ada.Unchecked_Conversion (Indicator_On_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Indicator_On_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Indicator_On_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Indicator_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Indicator_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Indicator_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Set_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Set_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Set_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- UseMotif2.0 Motif2.1
   function To_Integer is
      new Ada.Unchecked_Conversion (Toggle_Mode_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Toggle_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Toggle_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1

end Xm_Widgets.Primitive.Label.Toggle_Button;
