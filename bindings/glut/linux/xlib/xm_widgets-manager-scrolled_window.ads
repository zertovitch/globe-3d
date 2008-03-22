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

package Xm_Widgets.Manager.Scrolled_Window is
 

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Scrolled_Window_Widget_Class     : constant Widget_Class;


   type Xm_Traverse_Obscured_Callback_Struct is record
      Reason                : Callback_Reason;
      Event                 : X_Lib.X_Event_Pointer;
      Traversal_Destination : Widget;
      Direction             : Xm_Traversal_Direction;
   end record;
   pragma Convention (C, Xm_Traverse_Obscured_Callback_Struct);

   type Xm_Traverse_Obscured_Callback_Struct_Access is
      access all Xm_Traverse_Obscured_Callback_Struct;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Traverse_Obscured_Callback_Struct_Access;


   function Xm_Is_Scrolled_Window (W : in Widget) return Boolean;


   function Xm_Create_Scrolled_Window
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   procedure Xm_Scrolled_Window_Set_Areas
     (W       : in Widget;
      HScroll : in Widget;
      VScroll : in Widget;
      WRegion : in Widget);


   procedure Xm_Scroll_Visible
     (Scrw        : in Widget;
      Wid         : in Widget;
      Hor_Margin  : in X_Lib.Dimension;
      Ver_Margin  : in X_Lib.Dimension);

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

-- UseMotif2.0 Motif2.1
   Xm_N_Auto_Drag_Model            : constant Xt_N_Resource_String;

   -- for Xm_N_Auto_Drag_Model
   --
   type Auto_Drag_Model_Type is
     (Enabled, Disabled);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Auto_Drag_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Auto_Drag_Model_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   Xm_N_Clip_Window                : constant Xt_N_Resource_String;
   Xm_N_Horizontal_Scroll_Bar      : constant Xt_N_Resource_String;
   Xm_N_Scroll_Bar_Display_Policy  : constant Xt_N_Resource_String;

   type Scroll_Bar_Display_Policy_Type is (Static, As_Needed);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Scroll_Bar_Display_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Scroll_Bar_Display_Policy_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Scroll_Bar_Placement       : constant Xt_N_Resource_String;

   type Scroll_Bar_Placement_Type is
     (Bottom_Right, Top_Right, Bottom_Left, Top_Left);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Scroll_Bar_Placement_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Scroll_Bar_Placement_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Scrolled_Window_Margin_Height : constant Xt_N_Resource_String;
   Xm_N_Scrolled_Window_Margin_Width  : constant Xt_N_Resource_String;
   Xm_N_Scrolling_Policy              : constant Xt_N_Resource_String;

   type Scrolling_Policy_Type is
     (Automatic, Application_Defined);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Scrolling_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Scrolling_Policy_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Spacing                    : constant Xt_N_Resource_String;
   Xm_N_Traverse_Obscured_Callback : constant Xt_N_Resource_String;
   Xm_N_Vertical_Scroll_Bar        : constant Xt_N_Resource_String;
   Xm_N_Visual_Policy              : constant Xt_N_Resource_String;

   -- Visual_Policy'(Constant_Visual) should have been named
   -- "Constant", but this is a reserved word
   --
   type Visual_Policy_Type is
     (Variable_Visual, Constant_Visual, Resize_If_Possible);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Visual_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Visual_Policy_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Work_Window                : constant Xt_N_Resource_String;


-- UseMotif2.0 Motif2.1
   -- constraint resource for the children
   Xm_N_Scrolled_Window_Child_Type  : constant Xt_N_Resource_String;

   type Scrolled_Window_Child_Type is private;
   Work_Area      : constant Scrolled_Window_Child_Type;
   Hor_Scrollbar  : constant Scrolled_Window_Child_Type;
   Vert_Scrollbar : constant Scrolled_Window_Child_Type;
   Scroll_Hor     : constant Scrolled_Window_Child_Type;
   Scroll_Vert    : constant Scrolled_Window_Child_Type;
   No_Scroll      : constant Scrolled_Window_Child_Type;
   Clip_Window    : constant Scrolled_Window_Child_Type;
   Generic_Child  : constant Scrolled_Window_Child_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Scrolled_Window_Child_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Scrolled_Window_Child_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

private

-- UseMotif2.0 Motif2.1
   for Auto_Drag_Model_Type use (Enabled => 0, Disabled => 1);
   for Auto_Drag_Model_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   for Scroll_Bar_Display_Policy_Type use (Static => 0, As_Needed => 1);
   for Scroll_Bar_Display_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

   SW_TOP    : constant := 1;
   SW_BOTTOM : constant := 0;
   SW_LEFT   : constant := 2;
   SW_RIGHT  : constant := 0;
   for Scroll_Bar_Placement_Type use
     (Bottom_Right => SW_BOTTOM + SW_RIGHT,
      Top_Right    => SW_TOP + SW_RIGHT,
      Bottom_Left  => SW_BOTTOM + SW_LEFT,
      Top_Left     => SW_TOP + SW_LEFT);
   for Scroll_Bar_Placement_Type'Size use Interfaces.C.unsigned_char'Size;

   for Scrolling_Policy_Type use (Automatic => 0, Application_Defined => 1);
   for Scrolling_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

   for Visual_Policy_Type use
     (Variable_Visual => 0, Constant_Visual => 1, Resize_If_Possible => 2);
   for Visual_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   type Scrolled_Window_Child_Type is new Natural range 0 .. 11;
   Work_Area      : constant Scrolled_Window_Child_Type :=  0;
   Hor_Scrollbar  : constant Scrolled_Window_Child_Type :=  2;
   Vert_Scrollbar : constant Scrolled_Window_Child_Type :=  3;
   Scroll_Hor     : constant Scrolled_Window_Child_Type :=  7;
   Scroll_Vert    : constant Scrolled_Window_Child_Type :=  8;
   No_Scroll      : constant Scrolled_Window_Child_Type :=  9;
   Clip_Window    : constant Scrolled_Window_Child_Type := 10;
   Generic_Child  : constant Scrolled_Window_Child_Type := 11;
-- EndMotif2.0 Motif2.1

   pragma Import (C, Xm_Scroll_Visible, "XmScrollVisible");
   pragma Import (C, Xm_Scrolled_Window_Set_Areas, "XmScrolledWindowSetAreas");


   c_const_Xm_Scrolled_Window_Widget_Class     : Widget_Class;

   pragma Import (C, c_const_Xm_Scrolled_Window_Widget_Class, "xmScrolledWindowWidgetClass");

   Xm_Scrolled_Window_Widget_Class     : constant Widget_Class :=
    c_const_Xm_Scrolled_Window_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

-- UseMotif2.0 Motif2.1
   Xm_N_Auto_Drag_Model            : constant Xt_N_Resource_String := To_Resource_String ("autoDragModel");
-- EndMotif2.0 Motif2.1

   Xm_N_Clip_Window                : constant Xt_N_Resource_String := To_Resource_String ("clipWindow");
   Xm_N_Horizontal_Scroll_Bar      : constant Xt_N_Resource_String := To_Resource_String ("horizontalScrollBar");
   Xm_N_Scroll_Bar_Display_Policy  : constant Xt_N_Resource_String := To_Resource_String ("scrollBarDisplayPolicy");

   Xm_N_Scroll_Bar_Placement       : constant Xt_N_Resource_String := To_Resource_String ("scrollBarPlacement");

   Xm_N_Scrolled_Window_Margin_Height : constant Xt_N_Resource_String := To_Resource_String ("scrolledWindowMarginHeight");
   Xm_N_Scrolled_Window_Margin_Width  : constant Xt_N_Resource_String := To_Resource_String ("scrolledWindowMarginWidth");
   Xm_N_Scrolling_Policy           : constant Xt_N_Resource_String := To_Resource_String ("scrollingPolicy");

   Xm_N_Spacing                    : constant Xt_N_Resource_String := To_Resource_String ("spacing");
   Xm_N_Traverse_Obscured_Callback : constant Xt_N_Resource_String := To_Resource_String ("traverseObscuredCallback");
   Xm_N_Vertical_Scroll_Bar        : constant Xt_N_Resource_String := To_Resource_String ("verticalScrollBar");
   Xm_N_Visual_Policy              : constant Xt_N_Resource_String := To_Resource_String ("visualPolicy");

   Xm_N_Work_Window                : constant Xt_N_Resource_String := To_Resource_String ("workWindow");

-- UseMotif2.0 Motif2.1
   -- constraint resource for the children
   Xm_N_Scrolled_Window_Child_Type  : constant Xt_N_Resource_String := To_Resource_String ("scrolledWindowChildType");
-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Scrolled_Window;
