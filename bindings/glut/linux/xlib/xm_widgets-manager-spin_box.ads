-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2002 Hans-Frieder Vogt                                --
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
--          19 May 2001 Vadim Godunko: introduce Validate_Position_Type
--          26 Aug 2001 H.-F. Vogt: implementation of
--                                  Xm_Spin_Box_Validate_Position
--          29 Aug 2001 H.-F. Vogt: correct Xm_N_Spin_Box_Child_Type and
--                                  add Xm_N_Position_Type according
--                                  to a hint from Vadim Godunko
--          17 Nov 2001 Vadim Godunko: add Arrow_Orientation resource
--          12 Jan 2002 H.-F. Vogt: add previously forgotten Wrap constraint
--                                  resource
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Spin_Box is
 
-- UseMotif2.0 Motif2.1

   Xm_Spin_Box_Widget_Class            : constant Widget_Class;


   type Xm_Spin_Box_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      W                : Widget;
      Doit             : Boolean;
      Pos              : Integer;
      Value            : Xm_String;
      Crossed_Boundary : Boolean;
   end record;
   pragma Convention (C, Xm_Spin_Box_Callback_Struct);

   type Xm_Spin_Box_Callback_Struct_Access is
      access all Xm_Spin_Box_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Spin_Box_Callback_Struct_Access;





   function Xm_Is_Spin_Box (W : in Widget) return Boolean;


   function Xm_Create_Spin_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   type Validate_Position_Type is
     (Valid_Value, Current_Value, Maximum_Value, Minimum_Value,
      Increment_Value);

   procedure Xm_Spin_Box_Validate_Position
     (Text_Field    : in     Widget;
      Position      :    out Integer;
      Position_Type :    out Validate_Position_Type);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arrow_Layout            : constant Xt_N_Resource_String;

   --  for Xm_N_Arrow_Layout
   --
   type Arrow_Layout_Type is
     (Arrows_End,      Arrows_Beginning,
      Arrows_Split,
      Arrows_Flat_End, Arrows_Flat_Beginning);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Arrow_Layout_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Arrow_Layout_Type);
   pragma Convention (C, Append_Get);


-- UseMotif2.1
   --  specifies whether arrows point horizontally or vertically
   --  if Arrows_Vertical, then
   --   the decrement arrow points downwards, the increment arrow upwards
   --  if Arrows_Horizontal, then
   --   the decrement arrow points to the left, the increment arrow to the right
   --
   Xm_N_Arrow_Orientation       : constant Xt_N_Resource_String;

   type Arrow_Orientation is (Arrows_Vertical, Arrows_Horizontal);

   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Arrow_Orientation);

   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Arrow_Orientation);
   pragma Convention (C, Append_Get);
-- EndMotif2.1

   Xm_N_Arrow_Size                : constant Xt_N_Resource_String;
   Xm_N_Default_Arrow_Sensitivity : constant Xt_N_Resource_String;

   type Arrow_Sensitivity_Type is
     (Arrows_Insensitive,
      Arrows_Increment_Sensitive, Arrows_Decrement_Sensitive,
      Arrows_Sensitive,
      Arrows_Default_Sensitivity);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Arrow_Sensitivity_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Arrow_Sensitivity_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Detail_Shadow_Thickness : constant Xt_N_Resource_String;
   Xm_N_Initial_Delay           : constant Xt_N_Resource_String;
   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;
   Xm_N_Modify_Verify_Callback  : constant Xt_N_Resource_String;
   Xm_N_Repeat_Delay            : constant Xt_N_Resource_String;
   Xm_N_Spacing                 : constant Xt_N_Resource_String;
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String;

   --
   -- constraint resources for children of a spin box
   --
   Xm_N_Arrow_Sensitivity       : constant Xt_N_Resource_String;
   Xm_N_Decimal_Points          : constant Xt_N_Resource_String;
   Xm_N_Increment_Value         : constant Xt_N_Resource_String;
   Xm_N_Maximum_Value           : constant Xt_N_Resource_String;
   Xm_N_Minimum_Value           : constant Xt_N_Resource_String;
   Xm_N_Num_Values              : constant Xt_N_Resource_String;
   Xm_N_Position                : constant Xt_N_Resource_String;
   Xm_N_Position_Type           : constant Xt_N_Resource_String;

   type Position_Type is (Index, Value);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Position_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Position_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Spin_Box_Child_Type     : constant Xt_N_Resource_String;

   --  for Xm_N_Spin_Box_Child_Type
   --
   type Child_Type is (String, Numeric);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Child_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Child_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Values                  : constant Xt_N_Resource_String;

   --  a boolean value that specifies whether the Spin_Box wraps around the set
   --  of values
   --
   Xm_N_Wrap                    : constant Xt_N_Resource_String;

private

   for Validate_Position_Type use
     (Valid_Value => 0, Current_Value => 1, Maximum_Value => 2,
      Minimum_Value => 3, Increment_Value => 4);
   for Validate_Position_Type'Size use Interfaces.C.int'Size;

   for Arrow_Sensitivity_Type use
     (Arrows_Insensitive => 0,
      Arrows_Increment_Sensitive => 1, Arrows_Decrement_Sensitive => 2,
      Arrows_Sensitive => 3,
      Arrows_Default_Sensitivity => 4);
   for Arrow_Sensitivity_Type'Size use Interfaces.C.unsigned_char'Size;

   for Arrow_Layout_Type use
     (Arrows_End => 0,      Arrows_Beginning => 1,
      Arrows_Split => 2,
      Arrows_Flat_End => 3, Arrows_Flat_Beginning => 4);
   for Arrow_Layout_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.1
   for Arrow_Orientation use (Arrows_Vertical => 0, Arrows_Horizontal => 1);
   for Arrow_Orientation'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.1

   for Position_Type use (Index => 0, Value => 1);
   for Position_Type'Size use Interfaces.C.unsigned_char'Size;

   for Child_Type use (String => 2, Numeric => 3);
   for Child_Type'Size use Interfaces.C.unsigned_char'Size;

   c_const_Xm_Spin_Box_Widget_Class            : Widget_Class;

   pragma Import (C, c_const_Xm_Spin_Box_Widget_Class, "xmSpinBoxWidgetClass");

   Xm_Spin_Box_Widget_Class            : constant Widget_Class :=
    c_const_Xm_Spin_Box_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arrow_Layout            : constant Xt_N_Resource_String :=
      To_Resource_String ("arrowLayout");

-- UseMotif2.1
   Xm_N_Arrow_Orientation       : constant Xt_N_Resource_String :=
      To_Resource_String ("arrowOrientation");
-- EndMotif2.1
   Xm_N_Arrow_Size              : constant Xt_N_Resource_String :=
      To_Resource_String ("arrowSize");
   Xm_N_Default_Arrow_Sensitivity : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultArrowSensitivity");

   Xm_N_Detail_Shadow_Thickness : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Detail_Shadow_Thickness;
   Xm_N_Initial_Delay           : constant Xt_N_Resource_String :=
      To_Resource_String ("initialDelay");
   Xm_N_Margin_Height           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Modify_Verify_Callback  : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Modify_Verify_Callback;
   Xm_N_Repeat_Delay            : constant Xt_N_Resource_String :=
      To_Resource_String ("repeatDelay");
   Xm_N_Spacing                 : constant Xt_N_Resource_String :=
      To_Resource_String ("spacing");
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Value_Changed_Callback;

   --
   -- constraint resources for children of a spin box
   --
   Xm_N_Arrow_Sensitivity       : constant Xt_N_Resource_String :=
      To_Resource_String ("arrowSensitivity");
   Xm_N_Decimal_Points          : constant Xt_N_Resource_String :=
      To_Resource_String ("decimalPoints");
   Xm_N_Increment_Value         : constant Xt_N_Resource_String :=
      To_Resource_String ("incrementValue");
   Xm_N_Maximum_Value           : constant Xt_N_Resource_String :=
      To_Resource_String ("maximumValue");
   Xm_N_Minimum_Value           : constant Xt_N_Resource_String :=
      To_Resource_String ("minimumValue");
   Xm_N_Num_Values              : constant Xt_N_Resource_String :=
      To_Resource_String ("numValues");
   Xm_N_Position                : constant Xt_N_Resource_String :=
      To_Resource_String ("position");
   Xm_N_Position_Type           : constant Xt_N_Resource_String :=
      To_Resource_String ("positionType");

   Xm_N_Spin_Box_Child_Type     : constant Xt_N_Resource_String :=
      To_Resource_String ("spinBoxChildType");

   Xm_N_Values                  : constant Xt_N_Resource_String :=
      To_Resource_String ("values");
   Xm_N_Wrap                    : constant Xt_N_Resource_String :=
      To_Resource_String ("wrap");

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Spin_Box;
