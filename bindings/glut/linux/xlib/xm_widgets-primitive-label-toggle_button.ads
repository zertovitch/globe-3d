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

package Xm_Widgets.Primitive.Label.Toggle_Button is

   Xm_Toggle_Button_Widget_Class       : constant Widget_Class;
   Xm_Toggle_Button_Gadget_Class       : constant Widget_Class;


   --  Motif prior to 2.0 has two states:
   --  Unset and Set, or, as previously defined, the Boolean value False and
   --  True.
   --  The new definition is to use the type Set_Type, which is differently
   --  defined for Motif 1.2 and Motif 2.0+
   --
-- UseMotif2.0 Motif2.1
   type Set_Type is (Unset, Set, Indeterminate);
-- NotMotif2.0 Motif2.1
--!    type Set_Type is (Unset, Set);
-- EndMotif2.0 Motif2.1


   type Xm_Toggle_Button_Callback_Struct is record
      Reason : Callback_Reason;
      Event  : X_Lib.X_Event_Pointer;
      Set    : Set_Type;
   end record;
   for Xm_Toggle_Button_Callback_Struct use record
      Reason at 0 range 0 .. 31;
      Event  at 4 range 0 .. 31;
      Set    at 8 range 0 .. 31;
   end record;
   pragma Convention (C, Xm_Toggle_Button_Callback_Struct);

   type Xm_Toggle_Button_Callback_Struct_Access is
      access all Xm_Toggle_Button_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Toggle_Button_Callback_Struct_Access;




   function Xm_Is_Toggle_Button (W : in Widget) return Boolean;
   function Xm_Is_Toggle_Button_Gadget (W : in Widget) return Boolean;


   function Xm_Create_Toggle_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Toggle_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- get/set state
   --
   function Xm_Toggle_Button_Get_State (W : in Widget) return Boolean;

   function Xm_Toggle_Button_Gadget_Get_State (W : in Widget) return Boolean;

   procedure Xm_Toggle_Button_Set_State
     (W      : in Widget;
      State  : in Boolean;
      Notify : in Boolean);

   procedure Xm_Toggle_Button_Gadget_Set_State
     (W      : in Widget;
      State  : in Boolean;
      Notify : in Boolean);  

-- UseMotif2.0 Motif2.1
   --  if in two-states (On/Off) mode and Indeterminate is chosen, the exception
   --  Invalid_State_Error is raised
   --
   Invalid_State_Error : exception;

   procedure Xm_Toggle_Button_Set_Value
     (W      : in Widget;
      State  : in Set_Type;
      Notify : in Boolean);

   procedure Xm_Toggle_Button_Gadget_Set_Value
     (W      : in Widget;
      State  : in Set_Type;
      Notify : in Boolean);
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arm_Callback            : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Detail_Shadow_Thickness : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String;
   Xm_N_Fill_On_Select          : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Indeterminate_Pixmap    : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Indicator_On            : constant Xt_N_Resource_String;

   type Indicator_On_Type is (None, Fill,
                              Check, Check_Box,
                              Cross, Cross_Box);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Indicator_On_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Indicator_On_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Indicator_Size          : constant Xt_N_Resource_String;
   Xm_N_Indicator_Type          : constant Xt_N_Resource_String;

-- UseMotif2.0 Motif2.1
   type Indicator_Type is (N_Of_Many, One_Of_Many, One_Of_Many_Round);
-- NotMotif2.0 Motif2.1
--!    type Indicator_Type is (N_Of_Many, One_Of_Many);
-- EndMotif2.0 Motif2.1

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Indicator_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Indicator_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Select_Color              : constant Xt_N_Resource_String;
   Xm_N_Select_Insensitive_Pixmap : constant Xt_N_Resource_String;
   Xm_N_Select_Pixmap             : constant Xt_N_Resource_String;
   Xm_N_Set                       : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Set_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Set_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Spacing                 : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Toggle_Mode             : constant Xt_N_Resource_String;

   type Toggle_Mode_Type is (Toggle_Boolean, Toggle_Indeterminate);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Toggle_Mode_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Toggle_Mode_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Unselect_Color          : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String;
   Xm_N_Visible_When_Off        : constant Xt_N_Resource_String;

private

-- UseMotif2.0 Motif2.1
   for Set_Type use (Unset => 0, Set => 1, Indeterminate => 2);
   for Set_Type'Size use Interfaces.C.unsigned_char'Size;
-- NotMotif2.0 Motif2.1
--!    for Set_Type use (Unset => 0, Set => 1);
--!    for Set_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   XmINDICATOR_3D_BOX      : constant := 16#01#;
   XmINDICATOR_FLAT_BOX    : constant := 16#02#;
   XmINDICATOR_CHECK_GLYPH : constant := 16#10#;
   XmINDICATOR_CROSS_GLYPH : constant := 16#20#;

   for Indicator_On_Type use
     (None => 0, Fill => XmINDICATOR_3D_BOX,
      Check     => XmINDICATOR_CHECK_GLYPH,
      Check_Box => XmINDICATOR_CHECK_GLYPH + XmINDICATOR_3D_BOX,
      Cross     => XmINDICATOR_CROSS_GLYPH,
      Cross_Box => XmINDICATOR_CROSS_GLYPH + XmINDICATOR_3D_BOX);
   for Indicator_On_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Indicator_Type use (N_Of_Many => 1, One_Of_Many => 2, One_Of_Many_Round => 3);
-- NotMotif2.0 Motif2.1
--!    for Indicator_Type use (N_Of_Many => 1, One_Of_Many => 2);
-- EndMotif2.0 Motif2.1
   for Indicator_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Toggle_Mode_Type use (Toggle_Boolean => 0, Toggle_Indeterminate => 1);
   for Toggle_Mode_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1


   c_const_Xm_Toggle_Button_Widget_Class       : Widget_Class;
   c_const_Xm_Toggle_Button_Gadget_Class       : Widget_Class;

   pragma Import (C, c_const_Xm_Toggle_Button_Widget_Class, "xmToggleButtonWidgetClass");
   pragma Import (C, c_const_Xm_Toggle_Button_Gadget_Class, "xmToggleButtonGadgetClass");

   Xm_Toggle_Button_Widget_Class       : constant Widget_Class :=
    c_const_Xm_Toggle_Button_Widget_Class;
   Xm_Toggle_Button_Gadget_Class       : constant Widget_Class :=
    c_const_Xm_Toggle_Button_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arm_Callback            : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Arm_Callback;
-- UseMotif2.0 Motif2.1
   Xm_N_Detail_Shadow_Thickness : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Detail_Shadow_Thickness;
-- EndMotif2.0 Motif2.1
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Disarm_Callback;
   Xm_N_Fill_On_Select          : constant Xt_N_Resource_String :=
      To_Resource_String ("fillOnSelect");
-- UseMotif2.0 Motif2.1
   Xm_N_Indeterminate_Pixmap    : constant Xt_N_Resource_String :=
      To_Resource_String ("indeterminatePixmap");
-- EndMotif2.0 Motif2.1
   Xm_N_Indicator_On            : constant Xt_N_Resource_String :=
      To_Resource_String ("indicatorOn");

   Xm_N_Indicator_Size          : constant Xt_N_Resource_String :=
      To_Resource_String ("indicatorSize");
   Xm_N_Indicator_Type          : constant Xt_N_Resource_String :=
      To_Resource_String ("indicatorType");

   Xm_N_Select_Color            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Select_Color;
   Xm_N_Select_Insensitive_Pixmap : constant Xt_N_Resource_String :=
      To_Resource_String ("selectInsensitivePixmap");
   Xm_N_Select_Pixmap           : constant Xt_N_Resource_String :=
      To_Resource_String ("selectPixmap");
   Xm_N_Set                     : constant Xt_N_Resource_String :=
      To_Resource_String ("set");

   Xm_N_Spacing                 : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Spacing;
-- UseMotif2.0 Motif2.1
   Xm_N_Toggle_Mode             : constant Xt_N_Resource_String :=
      To_Resource_String ("toggleMode");

   Xm_N_Unselect_Color          : constant Xt_N_Resource_String :=
      To_Resource_String ("unselectColor");
-- EndMotif2.0 Motif2.1
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Value_Changed_Callback;
   Xm_N_Visible_When_Off        : constant Xt_N_Resource_String :=
      To_Resource_String ("visibleWhenOff");

end Xm_Widgets.Primitive.Label.Toggle_Button;
