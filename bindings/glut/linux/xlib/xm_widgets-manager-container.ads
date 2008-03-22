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

package Xm_Widgets.Manager.Container is
 
-- UseMotif2.0 Motif2.1

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Container_Widget_Class           : constant Widget_Class;


   type Outline_State_Type is (Collapsed, Expanded);

   type Auto_Selection_Type is
     (Auto_Unset, Auto_Begin, Auto_Motion,
      Auto_Cancel, Auto_No_Change, Auto_Change);


   type Xm_Container_Outline_Callback_Struct is record
      Reason             : Callback_Reason;
      Event              : X_Lib.X_Event_Pointer;
      Item               : Widget;
      New_Outline_State  : Outline_State_Type;
   end record;
   pragma Convention (C, Xm_Container_Outline_Callback_Struct);

   type Xm_Container_Outline_Callback_Struct_Access is
      access all Xm_Container_Outline_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Container_Outline_Callback_Struct_Access;


   type Xm_Container_Select_Callback_Struct is record
      Reason               : Callback_Reason;
      Event                : X_Lib.X_Event_Pointer;
      Selected_Items       : System.Address;
      Selected_Item_Count  : Natural;
      Auto_Selection       : Auto_Selection_Type;
   end record;
   pragma Convention (C, Xm_Container_Select_Callback_Struct);

   type Xm_Container_Select_Callback_Struct_Access is
      access all Xm_Container_Select_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Container_Select_Callback_Struct_Access;


   function Xm_Create_Container
    (Parent   : in  Widget; 
     Name     : in  String;
     Arglist  : in  Arg_List := Null_Arg_List)
     return Widget;

   procedure Xm_Container_Get_Item_Children
    (Wid            : in     Widget;
     Item           : in     Widget;
     Item_Children  : in out Widget_List);

   procedure Xm_Container_Relayout (Wid : in Widget);

   procedure Xm_Container_Reorder (Wid       : in Widget;
                                   CWid_List : in Widget_List);

   function Xm_Container_Cut (Wid       : in Widget;
                              Timestamp : in X_Lib.Server_Time)
                              return Boolean;

   function Xm_Container_Copy (Wid       : in Widget;
                               Timestamp : in X_Lib.Server_Time)
                               return Boolean;

   function Xm_Container_Paste (Wid : in Widget) return Boolean;

   function Xm_Container_Copy_Link (Wid       : in Widget;
                                    Timestamp : in X_Lib.Server_Time)
                                    return Boolean;

   function Xm_Container_Paste_Link (Wid : in Widget) return Boolean;

   function Xm_Is_Container (W : in Widget) return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Automatic_Selection     : constant Xt_N_Resource_String;

   type Automatic_Selection_Type is (No_Auto_Select, Auto_Select);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Automatic_Selection_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Automatic_Selection_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Collapsed_State_Pixmap  : constant Xt_N_Resource_String;
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String;
   Xm_N_Default_Action_Callback : constant Xt_N_Resource_String;
   Xm_N_Destination_Callback    : constant Xt_N_Resource_String;
   Xm_N_Detail_Column_Heading   : constant Xt_N_Resource_String;
   Xm_N_Detail_Column_Heading_Count :  constant Xt_N_Resource_String;
   Xm_N_Detail_Order            : constant Xt_N_Resource_String;
   Xm_N_Detail_Order_Count      : constant Xt_N_Resource_String;
   Xm_N_Detail_Tab_List         : constant Xt_N_Resource_String;

   Xm_N_Entry_View_Type         : constant Xt_N_Resource_String;

   type Entry_View_Type is (Large_Icon, Small_Icon, Any_Icon);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Entry_View_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Entry_View_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Expanded_State_Pixmap   : constant Xt_N_Resource_String;
   Xm_N_Font_List               : constant Xt_N_Resource_String;
   Xm_N_Large_Cell_Height       : constant Xt_N_Resource_String;
   Xm_N_Large_Cell_Width        : constant Xt_N_Resource_String;

   Xm_N_Layout_Type             : constant Xt_N_Resource_String;

   type Layout_Type is (Outline, Spatial, Detail);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Layout_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Layout_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;

   Xm_N_Outline_Button_Policy   : constant Xt_N_Resource_String;

   type Outline_Button_Policy_Type is (Outline_Button_Present, Outline_Button_Absent);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_Button_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Outline_Button_Policy_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Outline_Changed_Callback : constant Xt_N_Resource_String;
   Xm_N_Outline_Column_Width    : constant Xt_N_Resource_String;
   Xm_N_Outline_Indentation     : constant Xt_N_Resource_String;

   Xm_N_Outline_Line_Style      : constant Xt_N_Resource_String;

   type Outline_Line_Style_Type is (No_Line, Single);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_Line_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Outline_Line_Style_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Primary_Ownership       : constant Xt_N_Resource_String;

   type Primary_Ownership_Type is
     (Own_Never, Own_Always, Own_Multiple, Own_Possible_Multiple);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Primary_Ownership_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Primary_Ownership_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Render_Table            : constant Xt_N_Resource_String;

   Xm_N_Select_Color            : constant Xt_N_Resource_String;
   Xm_N_Selected_Objects        : constant Xt_N_Resource_String;
   Xm_N_Selected_Object_Count   : constant Xt_N_Resource_String;
   Xm_N_Selection_Callback      : constant Xt_N_Resource_String;
   Xm_N_Selection_Policy        : constant Xt_N_Resource_String;

   type Selection_Policy_Type is
     (Single_Select, Multiple_Select, Extended_Select, Browse_Select);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Policy_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Selection_Technique     : constant Xt_N_Resource_String;

   type Selection_Technique_Type is (Marquee,
                                     Marquee_Extend_Start,
                                     Marquee_Extend_Both,
                                     Touch_Only,
                                     Touch_Over);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Technique_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Technique_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Small_Cell_Height       : constant Xt_N_Resource_String;
   Xm_N_Small_Cell_Width        : constant Xt_N_Resource_String;


   Xm_N_Spatial_Include_Model   : constant Xt_N_Resource_String;

   type Spatial_Include_Model_Type is (Append, Closest, First_Fit);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Include_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Include_Model_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Spatial_Resize_Model    : constant Xt_N_Resource_String;

   type Spatial_Resize_Model_Type is (Grow_Minor, Grow_Major, Grow_Balanced);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Resize_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Resize_Model_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Spatial_Snap_Model      : constant Xt_N_Resource_String;

   type Spatial_Snap_Model_Type is (None, Snap_To_Grid, Center);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Snap_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Snap_Model_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Spatial_Style           : constant Xt_N_Resource_String;

   type Spatial_Style_Type is (None, Grid, Cells);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Style_Type);
   pragma Convention (C, Append_Get);

   --
   -- contraint resources for children of Xm_Container
   --
   Xm_N_Entry_Parent            : constant Xt_N_Resource_String;

   Xm_N_Outline_State           : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_State_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Outline_State_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Position_Index          : constant Xt_N_Resource_String;

private

   for Outline_State_Type use (Collapsed => 0, Expanded => 1);
   for Outline_State_Type'Size use Interfaces.C.unsigned_char'Size;

   for Auto_Selection_Type use
     (Auto_Unset => 0, Auto_Begin => 1, Auto_Motion => 2,
      Auto_Cancel => 3, Auto_No_Change => 4, Auto_Change => 5);
   for Auto_Selection_Type'Size use Interfaces.C.unsigned_char'Size;

   for Automatic_Selection_Type use (No_Auto_Select => 0, Auto_Select => 1);
   for Automatic_Selection_Type'Size use Interfaces.C.unsigned_char'Size;

   for Entry_View_Type use (Large_Icon => 0, Small_Icon => 1, Any_Icon => 2);
   for Entry_View_Type'Size use Interfaces.C.unsigned_char'Size;

   for Layout_Type use (Outline => 0, Spatial => 1, Detail => 2);
   for Layout_Type'Size use Interfaces.C.unsigned_char'Size;

   for Outline_Button_Policy_Type use (Outline_Button_Present => 0, Outline_Button_Absent => 1);
   for Outline_Button_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

   for Outline_Line_Style_Type use (No_Line => 0, Single => 1);
   for Outline_Line_Style_Type'Size use Interfaces.C.unsigned_char'Size;

   for Primary_Ownership_Type use
     (Own_Never => 0, Own_Always => 1, Own_Multiple => 2, Own_Possible_Multiple => 3);
   for Primary_Ownership_Type'Size use Interfaces.C.unsigned_char'Size;

   for Selection_Policy_Type use
     (Single_Select => 0, Multiple_Select => 1, Extended_Select => 2, Browse_Select => 3);
   for Selection_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

   for Selection_Technique_Type use (Marquee              => 0,
                                     Marquee_Extend_Start => 1,
                                     Marquee_Extend_Both  => 2,
                                     Touch_Only 	  => 3,
                                     Touch_Over 	  => 4);
   for Selection_Technique_Type'Size use Interfaces.C.unsigned_char'Size;

   for Spatial_Include_Model_Type use (Append => 0, Closest => 1, First_Fit => 2);
   for Spatial_Include_Model_Type'Size use Interfaces.C.unsigned_char'Size;

   for Spatial_Resize_Model_Type use (Grow_Minor => 0, Grow_Major => 1, Grow_Balanced => 2);
   for Spatial_Resize_Model_Type'Size use Interfaces.C.unsigned_char'Size;

   for Spatial_Snap_Model_Type use (None => 0, Snap_To_Grid => 1, Center => 2);
   for Spatial_Snap_Model_Type'Size use Interfaces.C.unsigned_char'Size;

   for Spatial_Style_Type use (None => 0, Grid => 1, Cells => 2);
   for Spatial_Style_Type'Size use Interfaces.C.unsigned_char'Size;

   pragma Import (C, Xm_Container_Relayout, "XmContainerRelayout");

   c_const_Xm_Container_Widget_Class           : Widget_Class;

   pragma Import (C, c_const_Xm_Container_Widget_Class, "xmContainerWidgetClass");

   Xm_Container_Widget_Class           : constant Widget_Class :=
    c_const_Xm_Container_Widget_Class;



   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Automatic_Selection     : constant Xt_N_Resource_String
      := To_Resource_String ("automaticSelection");

   Xm_N_Collapsed_State_Pixmap  : constant Xt_N_Resource_String
      := To_Resource_String ("collapsedStatePixmap");
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String
      := To_Resource_String ("convertCallback");
   Xm_N_Default_Action_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("defaultActionCallback");
   Xm_N_Destination_Callback    : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Destination_Callback;
   Xm_N_Detail_Column_Heading   : constant Xt_N_Resource_String
      := To_Resource_String ("detailColumnHeading");
   Xm_N_Detail_Column_Heading_Count :  constant Xt_N_Resource_String
      := To_Resource_String ("detailColumnHeadingCount");
   Xm_N_Detail_Order            : constant Xt_N_Resource_String
      := To_Resource_String ("detailOrder");
   Xm_N_Detail_Order_Count      : constant Xt_N_Resource_String
      := To_Resource_String ("detailOrderCount");
   Xm_N_Detail_Tab_List         : constant Xt_N_Resource_String
      := To_Resource_String ("detailTabList");

   Xm_N_Entry_View_Type         : constant Xt_N_Resource_String
      := To_Resource_String ("entryViewType");

   Xm_N_Expanded_State_Pixmap   : constant Xt_N_Resource_String
      := To_Resource_String ("expandedStatePixmap");
   Xm_N_Font_List               : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Font_List;
   Xm_N_Large_Cell_Height       : constant Xt_N_Resource_String
      := To_Resource_String ("largeCellHeight");
   Xm_N_Large_Cell_Width        : constant Xt_N_Resource_String
      := To_Resource_String ("largeCellWidth");

   Xm_N_Layout_Type             : constant Xt_N_Resource_String
      := To_Resource_String ("layoutType");

   Xm_N_Margin_Height           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;

   Xm_N_Outline_Button_Policy   : constant Xt_N_Resource_String
      := To_Resource_String ("outlineButtonPolicy");

   Xm_N_Outline_Changed_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("outlineChangedCallback");
   Xm_N_Outline_Column_Width    : constant Xt_N_Resource_String
      := To_Resource_String ("outlineColumnWidth");
   Xm_N_Outline_Indentation     : constant Xt_N_Resource_String
      := To_Resource_String ("outlineIndentation");
   Xm_N_Outline_Line_Style      : constant Xt_N_Resource_String
      := To_Resource_String ("outlineLineStyle");

   Xm_N_Primary_Ownership       : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Primary_Ownership;
   Xm_N_Render_Table            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Render_Table;

   Xm_N_Select_Color            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Select_Color;
   Xm_N_Selected_Objects        : constant Xt_N_Resource_String
      := To_Resource_String ("selectedObjects");
   Xm_N_Selected_Object_Count   : constant Xt_N_Resource_String
      := To_Resource_String ("selectedObjectCount");
   Xm_N_Selection_Callback      : constant Xt_N_Resource_String
      := To_Resource_String ("selectionCallback");
   Xm_N_Selection_Policy        : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Selection_Policy;

   Xm_N_Selection_Technique     : constant Xt_N_Resource_String
      := To_Resource_String ("selectionTechnique");

   Xm_N_Small_Cell_Height       : constant Xt_N_Resource_String
      := To_Resource_String ("smallCellHeight");
   Xm_N_Small_Cell_Width        : constant Xt_N_Resource_String
      := To_Resource_String ("smallCellWidth");

   Xm_N_Spatial_Include_Model   : constant Xt_N_Resource_String
      := To_Resource_String ("spatialIncludeModel");

   Xm_N_Spatial_Resize_Model    : constant Xt_N_Resource_String
      := To_Resource_String ("spatialResizeModel");

   Xm_N_Spatial_Snap_Model      : constant Xt_N_Resource_String
      := To_Resource_String ("spatialSnapModel");

   Xm_N_Spatial_Style           : constant Xt_N_Resource_String
      := To_Resource_String ("spatialStyle");

   --
   -- contraint resources for children of Xm_Container
   --
   Xm_N_Entry_Parent            : constant Xt_N_Resource_String
      := To_Resource_String ("entryParent");

   Xm_N_Outline_State           : constant Xt_N_Resource_String
      := To_Resource_String ("outlineState");

   Xm_N_Position_Index          : constant Xt_N_Resource_String
      := To_Resource_String ("positionIndex");

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Container;
