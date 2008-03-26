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

with Xm_Widgets.Primitive.Scroll_Bar;
package Xm_Widgets.Manager.Scale is
 

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Scale_Widget_Class               : constant Widget_Class;


   type Xm_Scale_Callback_Struct is record
      Reason : Callback_Reason;
      Event  : X_Lib.X_Event_Pointer;
      Value  : Integer;
   end record;
   pragma Convention (C, Xm_Scale_Callback_Struct);

   type Xm_Scale_Callback_Struct_Access is
      access all Xm_Scale_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Scale_Callback_Struct_Access;




   function Xm_Is_Scale (W : in Widget) return Boolean;


   function Xm_Create_Scale
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   procedure Xm_Scale_Set_Value (W     : in Widget;
                                 Value : in Integer);

   procedure Xm_Scale_Get_Value (W     : in  Widget;
                                 Value : out Integer);

   procedure Xm_Scale_Set_Ticks
     (Scale       : in Widget;
      Big_Every   : in Integer;
      Num_Med     : in Cardinal;
      Num_Small   : in Cardinal;
      Size_Big    : in X_Lib.Dimension;
      Size_Med    : in X_Lib.Dimension;
      Size_Small  : in X_Lib.Dimension);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --
   package SB renames Xm_Widgets.Primitive.Scroll_Bar;

-- UseMotif2.0 Motif2.1
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Decimal_Points          : constant Xt_N_Resource_String;
   Xm_N_Drag_Callback           : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Editable                : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Font_List               : constant Xt_N_Resource_String;
   Xm_N_Highlight_On_Enter      : constant Xt_N_Resource_String;
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String;
   Xm_N_Maximum                 : constant Xt_N_Resource_String;
   Xm_N_Minimum                 : constant Xt_N_Resource_String;
   --  use Xm_Widgets.Orientation_Type for this
   --
   Xm_N_Orientation             : constant Xt_N_Resource_String;

   Xm_N_Processing_Direction    : constant Xt_N_Resource_String;

   subtype Processing_Direction_Type is SB.Processing_Direction_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     SB.Processing_Direction_Type)
      renames SB.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out SB.Processing_Direction_Type)
      renames SB.Append_Get;


-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Scale_Height            : constant Xt_N_Resource_String;
   Xm_N_Scale_Multiple          : constant Xt_N_Resource_String;
   Xm_N_Scale_Width             : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Show_Arrows             : constant Xt_N_Resource_String;

   subtype Show_Arrows_Type is SB.Show_Arrows_Type;
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     SB.Show_Arrows_Type)
      renames SB.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out SB.Show_Arrows_Type)
      renames SB.Append_Get;

-- EndMotif2.0 Motif2.1

   Xm_N_Show_Value              : constant Xt_N_Resource_String;

   type Show_Value_Type is
     (None, Near_Slider, Near_Border);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Show_Value_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Show_Value_Type);
   pragma Convention (C, Append_Get);

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Mark             : constant Xt_N_Resource_String;

   subtype Slider_Mark_Type is SB.Slider_Mark_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     SB.Slider_Mark_Type)
      renames SB.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out SB.Slider_Mark_Type)
      renames SB.Append_Get;


   Xm_N_Slider_Visual           : constant Xt_N_Resource_String;

   subtype Slider_Visual_Type is SB.Slider_Visual_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     SB.Slider_Visual_Type)
      renames SB.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out SB.Slider_Visual_Type)
      renames SB.Append_Get;


   Xm_N_Sliding_Mode            : constant Xt_N_Resource_String;

   subtype Sliding_Mode_Type is SB.Sliding_Mode_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     SB.Sliding_Mode_Type)
   renames SB.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out SB.Sliding_Mode_Type)
   renames SB.Append_Get;

-- EndMotif2.0 Motif2.1

   Xm_N_Title_String            : constant Xt_N_Resource_String;
   Xm_N_Value                   : constant Xt_N_Resource_String;
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String;

private

   c_const_Xm_Scale_Widget_Class               : Widget_Class;

   pragma Import (C, c_const_Xm_Scale_Widget_Class, "xmScaleWidgetClass");

   Xm_Scale_Widget_Class               : constant Widget_Class :=
    c_const_Xm_Scale_Widget_Class;

   pragma Import (C, Xm_Scale_Set_Value, "XmScaleSetValue");
   pragma Import (C, Xm_Scale_Set_Ticks, "XmScaleSetTicks");


   for Show_Value_Type use
     (None => 0, Near_Slider => 1, Near_Border => 2);
   for Show_Value_Type'Size use Interfaces.C.unsigned_char'Size;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

-- UseMotif2.0 Motif2.1
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String :=
      To_Resource_String ("convertCallback");
-- EndMotif2.0 Motif2.1
   Xm_N_Decimal_Points          : constant Xt_N_Resource_String :=
      To_Resource_String ("decimalPoints");
   Xm_N_Drag_Callback           : constant Xt_N_Resource_String :=
      To_Resource_String ("dragCallback");
-- UseMotif2.0 Motif2.1
   Xm_N_Editable                : constant Xt_N_Resource_String :=
      To_Resource_String ("editable");
-- EndMotif2.0 Motif2.1
   Xm_N_Font_List               : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Font_List;
   Xm_N_Highlight_On_Enter      : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightOnEnter");
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightThickness");
   Xm_N_Maximum                 : constant Xt_N_Resource_String :=
      To_Resource_String ("maximum");
   Xm_N_Minimum                 : constant Xt_N_Resource_String :=
      To_Resource_String ("minimum");
   Xm_N_Orientation             : constant Xt_N_Resource_String :=
      X_Toolkit.Xt_N_Orientation;

   Xm_N_Processing_Direction    : constant Xt_N_Resource_String :=
      To_Resource_String ("processingDirection");

-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Render_Table;
-- EndMotif2.0 Motif2.1
   Xm_N_Scale_Height            : constant Xt_N_Resource_String :=
      To_Resource_String ("scaleHeight");
   Xm_N_Scale_Multiple          : constant Xt_N_Resource_String :=
      To_Resource_String ("scaleMultiple");
   Xm_N_Scale_Width             : constant Xt_N_Resource_String :=
      To_Resource_String ("scaleWidth");
-- UseMotif2.0 Motif2.1
   Xm_N_Show_Arrows             : constant Xt_N_Resource_String :=
      To_Resource_String ("showArrows");
-- EndMotif2.0 Motif2.1

   Xm_N_Show_Value              : constant Xt_N_Resource_String :=
      To_Resource_String ("showValue");

-- UseMotif2.0 Motif2.1
   Xm_N_Slider_Mark             : constant Xt_N_Resource_String :=
      To_Resource_String ("sliderMark");
   Xm_N_Slider_Visual           : constant Xt_N_Resource_String :=
      To_Resource_String ("sliderVisual");
   Xm_N_Sliding_Mode            : constant Xt_N_Resource_String :=
      To_Resource_String ("slidingMode");
-- EndMotif2.0 Motif2.1

   Xm_N_Title_String            : constant Xt_N_Resource_String :=
      To_Resource_String ("titleString");
   Xm_N_Value                   : constant Xt_N_Resource_String :=
      To_Resource_String ("value");
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Value_Changed_Callback;

end Xm_Widgets.Manager.Scale;
