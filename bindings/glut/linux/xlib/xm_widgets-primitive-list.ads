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

with Xm_Widgets.Manager.Scrolled_Window;
package Xm_Widgets.Primitive.List is
 
   Xm_List_Widget_Class                : constant Widget_Class;


   type Xm_String_List is array (Integer range <>) of Xm_String;
   type Xm_String_List_Pointer is access all Xm_String_List;

   type Integer_Array_Type is array (Integer range <>) of Integer;
   type Integer_Array_Pointer_Type is access all Integer_Array_Type;

   type Selection_Type is
     (Initial, Addition, Modification);

-- UseMotif2.0 Motif2.1
   type Auto_Selection_Type is
     (Auto_Unset, Auto_Begin, Auto_Motion,
      Auto_Cancel, Auto_No_Change, Auto_Change);
-- EndMotif2.0 Motif2.1

   type Xm_List_Callback_Struct is record
      Reason                 : Callback_Reason;
      Event                  : X_Lib.X_Event_Pointer;
      Item                   : Xm_String;
      Item_Length            : Integer;
      Item_Position          : Integer;
      Selected_Items         : System.Address;  -- preliminary, should be Xm_String_List_Pointer
      Selected_Item_Count    : Integer;
      Selected_Item_Position : System.Address;  -- preliminary, should be Integer_Array_Pointer_Type
      Sel_Type               : Selection_Type;
-- UseMotif2.0 Motif2.1
      Auto_Sel_Type          : Auto_Selection_Type;
-- EndMotif2.0 Motif2.1
   end record;
   pragma Convention (C, Xm_List_Callback_Struct);

   type Xm_List_Callback_Struct_Access is
      access all Xm_List_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_List_Callback_Struct_Access;




   -- -------------------------------------------------------------------------
   --
   -- XmIsList
   --
   function Xm_Is_List (W : in Widget) return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- Xm_Create_List
   --
   function Xm_Create_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   -- -------------------------------------------------------------------------
   --
   -- Xm_Create_Scrolled_List
   --
   function Xm_Create_Scrolled_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Add....
   --
   procedure Xm_List_Add_Item
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer);

   procedure Xm_List_Add_Items
     (W     : in Widget;
      Items : in Xm_String_List;
      Pos   : in Integer);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Add....Unselected
   --
   procedure Xm_List_Add_Item_Unselected
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer);

   procedure Xm_List_Add_Items_Unselected
     (W     : in Widget;
      Items : in Xm_String_List;
      Pos   : in Integer);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Delete
   --
   procedure Xm_List_Delete_Item
     (W     : in Widget;
      Item  : in Xm_String);
   
   procedure Xm_List_Delete_Items
     (W     : in Widget;
      Items : in Xm_String_List);
   
   procedure Xm_List_Delete_Pos
     (W    : in Widget;
      Pos  : in Integer);

   procedure Xm_List_Delete_Positions
     (W             : in Widget;
      Position_List : in Integer_Array_Type);

   procedure Xm_List_Delete_Items_Pos
     (W          : in Widget;
      Item_Count : in Integer;
      Pos        : in Integer);

   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Delete_All_Items
   --
   procedure Xm_List_Delete_All_Items (W : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Replace
   --
   Invalid_Items_Count : exception;

   procedure Xm_List_Replace_Items
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List);

   procedure Xm_List_Replace_Items_Pos
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer);

   procedure Xm_List_Replace_Items_Unselected
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List);

   procedure Xm_List_Replace_Items_Pos_Unselected
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer);

   procedure Xm_List_Replace_Positions
     (W             : in Widget;
      Position_List : in Integer_Array_Type;
      Item_List     : in Xm_String_List);

   procedure Xm_List_Replace
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List);

   procedure Xm_List_Replace
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer);

   procedure Xm_List_Replace_Unselected
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List);

   procedure Xm_List_Replace_Unselected
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer);

   procedure Xm_List_Replace
     (W             : in Widget;
      Position_List : in Integer_Array_Type;
      Item_List     : in Xm_String_List);



   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Select
   --

   procedure Xm_List_Select_Item
     (W      : in Widget;
      Item   : in Xm_String;
      Notify : in Boolean);

   procedure Xm_List_Select_Pos
     (W      : in Widget;
      Pos    : in Integer;
      Notify : in Boolean);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Deselect
   --
   procedure Xm_List_Deselect_Item
     (W     : in Widget;
      Item  : in Xm_String);

   procedure Xm_List_Deselect_Pos
     (W    : in Widget;
      Pos  : in Integer);

   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Deselect_All_Items
   --
   procedure Xm_List_Deselect_All_Items (W : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set...
   --
   procedure Xm_List_Set_Pos
     (W     : in Widget;
      Pos   : in Integer);

   procedure Xm_List_Set_Bottom_Pos
     (W     : in Widget;
      Pos   : in Integer);

   procedure Xm_List_Set_Horiz_Pos
     (W     : in Widget;
      Pos   : in Integer);

   procedure Xm_List_Set_Item
     (W     : in Widget;
      Item  : in Xm_String);

   procedure Xm_List_Set_Bottom_Item
     (W     : in Widget;
      Item  : in Xm_String);

   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set_Add_Mode
   --
   procedure Xm_List_Set_Add_Mode
     (W        : in Widget;
      Add_Mode : in Boolean);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Item_Exists
   --
   function Xm_List_Item_Exists
     (W        : in Widget;
      Item     : in Xm_String)
      return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Item_Pos
   --
   function Xm_List_Item_Pos
     (W        : in Widget;
      Item     : in Xm_String)
      return Integer;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Get_Kbd_Item_Pos
   --
   function Xm_List_Get_Kbd_Item_Pos (W : in Widget) return Integer;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set_Kbd_Item_Pos
   --
   Invalid_List_Item_Pos : exception;

   procedure Xm_List_Set_Kbd_Item_Pos
     (W   : in Widget;
      Pos : in Integer);


   -- -------------------------------------------------------------------------
   --
   -- XmListYToPos
   --
   function Xm_List_Y_To_Pos
     (W   : in Widget;
      Y   : in X_Lib.Position)
      return Integer;


   -- -------------------------------------------------------------------------
   --
   -- XmListPosToBounds
   --
   procedure Xm_List_Pos_To_Bounds
     (W        : in     Widget;
      Pos      : in     Integer;
      X        :    out X_Lib.Position;
      Y        :    out X_Lib.Position;
      Width    :    out X_Lib.Dimension;
      Height   :    out X_Lib.Dimension);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Get_Match_Pos
   --
   function Xm_List_Get_Match
     (W        : in     Widget;
      Item     : in     Xm_String)
      return Integer_Array_Type;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Get_Selected_Pos
   --
   function Xm_List_Get_Selected
     (W        : in     Widget)
      return Integer_Array_Type;


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Update_Selected_List
   --
   procedure Xm_List_Update_Selected_List (W : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Pos_Selected
   --
   function Xm_List_Pos_Selected
     (W   : in Widget;
      Pos : in Integer)
      return Boolean;




   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Automatic_Selection    : constant Xt_N_Resource_String;

   Xm_N_Browse_Selection_Callback : constant Xt_N_Resource_String;
   Xm_N_Default_Action_Callback   : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Destination_Callback   : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Double_Click_Interval  : constant Xt_N_Resource_String;
   Xm_N_Extended_Selection_Callback : constant Xt_N_Resource_String;
   Xm_N_Font_List              : constant Xt_N_Resource_String;
   Xm_N_Item_Count             : constant Xt_N_Resource_String;
   Xm_N_Items                  : constant Xt_N_Resource_String;
   Xm_N_List_Margin_Height     : constant Xt_N_Resource_String;
   Xm_N_List_Margin_Width      : constant Xt_N_Resource_String;
   Xm_N_List_Size_Policy       : constant Xt_N_Resource_String;

   type List_Size_Policy_Type is
      (Change_All, Change_None, Change_Width, Change_Height);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     List_Size_Policy_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out List_Size_Policy_Type);
   pragma Convention (C, Append_Get);

   Xm_N_List_Spacing           : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Match_Behavior         : constant Xt_N_Resource_String;

   type Match_Behavior_Type is
     (None, Quick_Navigate, Invalid_Match_Behavior);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Match_Behavior_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Match_Behavior_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1
   Xm_N_Multiple_Selection_Callback : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
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
-- EndMotif2.0 Motif2.1
   Xm_N_Scroll_Bar_Display_Policy : constant Xt_N_Resource_String;

   subtype Scroll_Bar_Display_Policy_Type is
      Xm_Widgets.Manager.Scrolled_Window.Scroll_Bar_Display_Policy_Type;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Scroll_Bar_Display_Policy_Type)
      renames Xm_Widgets.Manager.Scrolled_Window.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Scroll_Bar_Display_Policy_Type)
      renames Xm_Widgets.Manager.Scrolled_Window.Append_Get;

-- UseMotif2.0 Motif2.1
   Xm_N_Select_Color            : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Selected_Item_Count     : constant Xt_N_Resource_String;
   Xm_N_Selected_Items          : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Selected_Position_Count : constant Xt_N_Resource_String;
   Xm_N_Selected_Positions      : constant Xt_N_Resource_String;
   Xm_N_Selection_Mode          : constant Xt_N_Resource_String;

   type Selection_Mode_Type is (Normal_Mode, Add_Mode);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Mode_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Mode_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

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

   Xm_N_Single_Selection_Callback  : constant Xt_N_Resource_String;
   Xm_N_String_Direction        : constant Xt_N_Resource_String;
   Xm_N_Top_Item_Position       : constant Xt_N_Resource_String;
   Xm_N_Visible_Item_Count      : constant Xt_N_Resource_String;


private

   for Selection_Type use
     (Initial => 0, Addition => 1, Modification => 2);
   for Selection_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Auto_Selection_Type use
     (Auto_Unset => 0, Auto_Begin => 1, Auto_Motion => 2,
      Auto_Cancel => 3, Auto_No_Change => 4, Auto_Change => 5);
   for Auto_Selection_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   for List_Size_Policy_Type use
      (Change_All => 0, Change_None => 1, Change_Width => 2, Change_Height => 3);
   for List_Size_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Match_Behavior_Type use
     (None => 0, Quick_Navigate => 1, Invalid_Match_Behavior => 2);
   for Match_Behavior_Type'Size use Interfaces.C.unsigned_char'Size;

   for Primary_Ownership_Type use
     (Own_Never => 0, Own_Always => 1, Own_Multiple => 2, Own_Possible_Multiple => 3);
   for Primary_Ownership_Type'Size use Interfaces.C.unsigned_char'Size;

   for Selection_Mode_Type use (Normal_Mode => 0, Add_Mode => 1);
   for Selection_Mode_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   for Selection_Policy_Type use
     (Single_Select => 0, Multiple_Select => 1, Extended_Select => 2, Browse_Select => 3);
   for Selection_Policy_Type'Size use Interfaces.C.unsigned_char'Size;

   pragma Import (C, Xm_List_Delete_All_Items, "XmListDeleteAllItems");
   pragma Import (C, Xm_List_Deselect_All_Items, "XmListDeselectAllItems");
   pragma Import (C, Xm_List_Item_Pos, "XmListItemPos");
   pragma Import (C, Xm_List_Get_Kbd_Item_Pos, "XmListGetKbdItemPos");
   pragma Import (C, Xm_List_Y_To_Pos, "XmListYToPos");
   pragma Import (C, Xm_List_Update_Selected_List, "XmListUpdateSelectedList");

   c_const_Xm_List_Widget_Class                : Widget_Class;
   
   pragma Import (C, c_const_Xm_List_Widget_Class, "xmListWidgetClass");

   Xm_List_Widget_Class                : constant Widget_Class :=
    c_const_Xm_List_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Automatic_Selection    : constant Xt_N_Resource_String
      := To_Resource_String ("automaticSelection");

   Xm_N_Browse_Selection_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("browseSelectionCallback");
   Xm_N_Default_Action_Callback   : constant Xt_N_Resource_String
      := To_Resource_String ("defaultActionCallback");
-- UseMotif2.0 Motif2.1
   Xm_N_Destination_Callback   : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Destination_Callback;
-- EndMotif2.0 Motif2.1
   Xm_N_Double_Click_Interval  : constant Xt_N_Resource_String
      := To_Resource_String ("doubleClickInterval");
   Xm_N_Extended_Selection_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("extendedSelectionCallback");
   Xm_N_Font_List              : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Font_List;
   Xm_N_Item_Count             : constant Xt_N_Resource_String
      := To_Resource_String ("itemCount");
   Xm_N_Items                  : constant Xt_N_Resource_String
      := To_Resource_String ("items");
   Xm_N_List_Margin_Height     : constant Xt_N_Resource_String
      := To_Resource_String ("listMarginHeight");
   Xm_N_List_Margin_Width      : constant Xt_N_Resource_String
      := To_Resource_String ("listMarginWidth");
   Xm_N_List_Size_Policy       : constant Xt_N_Resource_String
      := To_Resource_String ("listSizePolicy");

   Xm_N_List_Spacing           : constant Xt_N_Resource_String
      := To_Resource_String ("listSpacing");
-- UseMotif2.0 Motif2.1
   Xm_N_Match_Behavior         : constant Xt_N_Resource_String
      := To_Resource_String ("matchBehavior");

-- EndMotif2.0 Motif2.1
   Xm_N_Multiple_Selection_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("multipleSelectionCallback");
-- UseMotif2.0 Motif2.1
   Xm_N_Primary_Ownership       : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Primary_Ownership;

   Xm_N_Render_Table            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Render_Table;
-- EndMotif2.0 Motif2.1
   Xm_N_Scroll_Bar_Display_Policy : constant Xt_N_Resource_String :=
      Xm_Widgets.Manager.Scrolled_Window.Xm_N_Scroll_Bar_Display_Policy;

-- UseMotif2.0 Motif2.1
   Xm_N_Select_Color            : constant Xt_N_Resource_String
      := To_Resource_String ("selectColor");
-- EndMotif2.0 Motif2.1
   Xm_N_Selected_Item_Count     : constant Xt_N_Resource_String
      := To_Resource_String ("selectedItemCount");
   Xm_N_Selected_Items          : constant Xt_N_Resource_String
      := To_Resource_String ("selectedItems");
-- UseMotif2.0 Motif2.1
   Xm_N_Selected_Position_Count : constant Xt_N_Resource_String
      := To_Resource_String ("selectedPositionCount");
   Xm_N_Selected_Positions      : constant Xt_N_Resource_String
      := To_Resource_String ("selectedPositions");
   Xm_N_Selection_Mode          : constant Xt_N_Resource_String
      := To_Resource_String ("selectionMode");

-- EndMotif2.0 Motif2.1

   Xm_N_Selection_Policy        : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Selection_Policy;

   Xm_N_Single_Selection_Callback  : constant Xt_N_Resource_String :=
      To_Resource_String ("singleSelectionCallback");
   Xm_N_String_Direction        : constant Xt_N_Resource_String :=
      To_Resource_String ("stringDirection");
   Xm_N_Top_Item_Position       : constant Xt_N_Resource_String :=
      To_Resource_String ("topItemPosition");
   Xm_N_Visible_Item_Count      : constant Xt_N_Resource_String :=
      To_Resource_String ("visibleItemCount");

end Xm_Widgets.Primitive.List;
