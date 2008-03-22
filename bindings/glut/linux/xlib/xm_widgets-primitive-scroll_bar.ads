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
--          20 Jan 2002 H.-F. Vogt: Orientation_Type was moved to Xm_Widgets
--
-------------------------------------------------------------------------------

package Xm_Widgets.Primitive.Scroll_Bar is
 

   Xm_Scroll_Bar_Widget_Class          : constant Widget_Class;


   type Xm_Scroll_Bar_Callback_Struct is record
      Reason : Callback_Reason;
      Event  : X_Lib.X_Event_Pointer;
      Value  : Integer;
      Pixel  : X_Lib.Position;
   end record;
   for Xm_Scroll_Bar_Callback_Struct use record
      Reason at  0 range 0 .. 31;
      Event  at  4 range 0 .. 31;
      Value  at  8 range 0 .. 31;
      Pixel  at 12 range 0 .. 31;
   end record;
   pragma Convention (C, Xm_Scroll_Bar_Callback_Struct);

   type Xm_Scroll_Bar_Callback_Struct_Access is
      access all Xm_Scroll_Bar_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Scroll_Bar_Callback_Struct_Access;




   function Xm_Is_Scroll_Bar (W : in Widget) return Boolean;


   function Xm_Create_Scroll_Bar
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   procedure Xm_Scroll_Bar_Get_Values
     (W              : in  Widget;
      Value          : out Integer;
      Increment      : out Integer;
      Page_Increment : out Integer);

   procedure Xm_Scroll_Bar_Set_Values
     (W              : in Widget;
      Value          : in Integer;
      Increment      : in Integer;
      Page_Increment : in Integer;
      Notify         : in Boolean);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Decrement_Callback      : constant Xt_N_Resource_String;
   Xm_N_Drag_Callback           : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Editable                : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Increment               : constant Xt_N_Resource_String;
   Xm_N_Increment_Callback      : constant Xt_N_Resource_String;
   Xm_N_Initial_Delay           : constant Xt_N_Resource_String;
   Xm_N_Maximum                 : constant Xt_N_Resource_String;
   Xm_N_Minimum                 : constant Xt_N_Resource_String;
   --  use Xm_Widgets.Orientation_Type for this
   --
   Xm_N_Orientation             : constant Xt_N_Resource_String;

   Xm_N_Page_Decrement_Callback : constant Xt_N_Resource_String;
   Xm_N_Page_Increment          : constant Xt_N_Resource_String;
   Xm_N_Page_Increment_Callback : constant Xt_N_Resource_String;
   Xm_N_Processing_Direction    : constant Xt_N_Resource_String;

   type Processing_Direction_Type is (Max_On_Top,
                                      Max_On_Bottom,
                                      Max_On_Left,
                                      Max_On_Right);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Processing_Direction_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Processing_Direction_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Repeat_Delay            : constant Xt_N_Resource_String;

   Xm_N_Show_Arrows             : constant Xt_N_Resource_String;

   type Show_Arrows_Type is (None, Each_Side, Max_Side, Min_Side);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Show_Arrows_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Show_Arrows_Type);
   pragma Convention (C, Append_Get);

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Mark             : constant Xt_N_Resource_String;

   type Slider_Mark_Type is (None, Etched_Line, Thumb_Mark, Round_Mark);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Slider_Mark_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Slider_Mark_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   Xm_N_Slider_Size             : constant Xt_N_Resource_String;

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Visual           : constant Xt_N_Resource_String;

   type Slider_Visual_Type is (Background_Color, 
                               Foreground_Color,
                               Through_Color,
                               Shadowed_Background);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Slider_Visual_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Slider_Visual_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Sliding_Mode            : constant Xt_N_Resource_String;

   type Sliding_Mode_Type is (Slider, Thermometer);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Sliding_Mode_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Sliding_Mode_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Snap_Back_Multiple      : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_To_Bottom_Callback      : constant Xt_N_Resource_String;
   Xm_N_To_Top_Callback         : constant Xt_N_Resource_String;
   Xm_N_Through_Color           : constant Xt_N_Resource_String;
   Xm_N_Value                   : constant Xt_N_Resource_String;
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String;

private

   for Processing_Direction_Type use (Max_On_Top => 0,
                                      Max_On_Bottom => 1,
                                      Max_On_Left => 2,
                                      Max_On_Right => 3);
   for Processing_Direction_Type'Size use Interfaces.C.unsigned_char'Size;

   for Show_Arrows_Type use (None => 0, Each_Side => 1, Max_Side => 2, Min_Side => 3);
   for Show_Arrows_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Slider_Mark_Type use (None => 0, Etched_Line => 1, Thumb_Mark => 2, Round_Mark => 3);
   for Slider_Mark_Type'Size use Interfaces.C.unsigned_char'Size;

   for Sliding_Mode_Type use (Slider => 0, Thermometer => 1);
   for Sliding_Mode_Type'Size use Interfaces.C.unsigned_char'Size;

   for Slider_Visual_Type use (Background_Color => 0, 
                               Foreground_Color => 1,
                               Through_Color	=> 2,
                               Shadowed_Background => 3);
   for Slider_Visual_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1


   c_const_Xm_Scroll_Bar_Widget_Class          : Widget_Class;
   
   pragma Import (C, c_const_Xm_Scroll_Bar_Widget_Class, "xmScrollBarWidgetClass");
   
   Xm_Scroll_Bar_Widget_Class          : constant Widget_Class :=
    c_const_Xm_Scroll_Bar_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Decrement_Callback      : constant Xt_N_Resource_String :=
      To_Resource_String ("decrementCallback");
   Xm_N_Drag_Callback           : constant Xt_N_Resource_String :=
      To_Resource_String ("dragCallback");
-- UseMotif2.0 Motif2.1
   Xm_N_Editable                : constant Xt_N_Resource_String :=
      To_Resource_String ("editable");
-- EndMotif2.0 Motif2.1
   Xm_N_Increment               : constant Xt_N_Resource_String :=
      To_Resource_String ("increment");
   Xm_N_Increment_Callback      : constant Xt_N_Resource_String :=
      To_Resource_String ("incrementCallback");
   Xm_N_Initial_Delay           : constant Xt_N_Resource_String :=
      To_Resource_String ("initialDelay");
   Xm_N_Maximum                 : constant Xt_N_Resource_String :=
      To_Resource_String ("maximum");
   Xm_N_Minimum                 : constant Xt_N_Resource_String :=
      To_Resource_String ("minimum");
   Xm_N_Orientation             : constant Xt_N_Resource_String :=
      X_Toolkit.Xt_N_Orientation;
   Xm_N_Page_Decrement_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("pageDecrementCallback");
   Xm_N_Page_Increment          : constant Xt_N_Resource_String :=
      To_Resource_String ("pageIncrement");
   Xm_N_Page_Increment_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("pageIncrementCallback");
   Xm_N_Processing_Direction    : constant Xt_N_Resource_String :=
      To_Resource_String ("processingDirection");

   Xm_N_Repeat_Delay            : constant Xt_N_Resource_String :=
      To_Resource_String ("repeatDelay");

   Xm_N_Show_Arrows             : constant Xt_N_Resource_String :=
      To_Resource_String ("showArrows");

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Mark             : constant Xt_N_Resource_String :=
      To_Resource_String ("sliderMark");

-- EndMotif2.0 Motif2.1

   Xm_N_Slider_Size             : constant Xt_N_Resource_String :=
      To_Resource_String ("sliderSize");

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Visual           : constant Xt_N_Resource_String :=
      To_Resource_String ("sliderVisual");
   Xm_N_Sliding_Mode            : constant Xt_N_Resource_String :=
      To_Resource_String ("slidingMode");

   Xm_N_Snap_Back_Multiple      : constant Xt_N_Resource_String :=
      To_Resource_String ("snapBackMultiple");
-- EndMotif2.0 Motif2.1
   Xm_N_To_Bottom_Callback      : constant Xt_N_Resource_String :=
      To_Resource_String ("toBottomCallback");
   Xm_N_To_Top_Callback         : constant Xt_N_Resource_String :=
      To_Resource_String ("toTopCallback");
   Xm_N_Through_Color           : constant Xt_N_Resource_String :=
      To_Resource_String ("throughColor");
   Xm_N_Value                   : constant Xt_N_Resource_String :=
      To_Resource_String ("value");
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Value_Changed_Callback;

end Xm_Widgets.Primitive.Scroll_Bar;
