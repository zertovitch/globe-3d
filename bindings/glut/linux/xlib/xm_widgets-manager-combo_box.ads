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
--          19 May 2001 Vadim Godunko: implement Xm_Combo_Box_Add_Item,
--                                     Xm_Combo_Box_Delete_Pos,
--                                     Xm_Combo_Box_Select_Item,
--                                     Xm_Combo_Box_Set_Item
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Combo_Box is
 
-- UseMotif2.0 Motif2.1

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Combo_Box_Widget_Class : constant Widget_Class;


   type Xm_Combo_Box_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      Item_Or_Text     : Xm_String;
      Item_Position    : Integer;
   end record;
   pragma Convention (C, Xm_Combo_Box_Callback_Struct);

   type Xm_Combo_Box_Callback_Struct_Access is
      access all Xm_Combo_Box_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Combo_Box_Callback_Struct_Access;


   function Xm_Is_Combo_Box (W : in Widget) return Boolean;


   function Xm_Create_Combo_Box
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Drop_Down_Combo_Box
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Drop_Down_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   procedure Xm_Combo_Box_Update (W : in Widget);


-- UseMotif2.1
   procedure Xm_Combo_Box_Add_Item
     (W      : in Widget;
      Item   : in Xm_String;
      Pos    : in Integer;
      Unique : in Boolean);

   procedure Xm_Combo_Box_Delete_Pos
     (W   : in Widget;
      Pos : in Integer);

   procedure Xm_Combo_Box_Select_Item
     (W    : in Widget;
      Item : in Xm_String);

   procedure Xm_Combo_Box_Set_Item
     (W    : in Widget;
      Item : in Xm_String);
-- EndMotif2.1

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arrow_Size              : constant Xt_N_Resource_String;
   Xm_N_Arrow_Spacing           : constant Xt_N_Resource_String;
-- UseMotif2.1
   Xm_N_Columns                 : constant Xt_N_Resource_String;
-- EndMotif2.1
   Xm_N_Combo_Box_Type          : constant Xt_N_Resource_String;

   type Combo_Box_Type is (Combo_Box, Drop_Down_Combo_Box, Drop_Down_List);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Combo_Box_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Combo_Box_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Font_List               : constant Xt_N_Resource_String;
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String;
-- UseMotif2.1
   Xm_N_Item_Count              : constant Xt_N_Resource_String;
   Xm_N_Items                   : constant Xt_N_Resource_String;
   Xm_N_List                    : constant Xt_N_Resource_String;
-- EndMotif2.1
   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;
   Xm_N_Match_Behavior          : constant Xt_N_Resource_String;

   type Match_Behavior_Type is (None, Quick_Navigate, Invalid_Match_Behavior);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Match_Behavior_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Match_Behavior_Type);
   pragma Convention (C, Append_Get);

-- UseMotif2.1
   Xm_N_Position_Mode           : constant Xt_N_Resource_String;
   
   type Position_Mode_Type is (Zero_Based, One_Based);
   
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Position_Mode_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Position_Mode_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.1

   Xm_N_Render_Table            : constant Xt_N_Resource_String;
   Xm_N_Selected_Item           : constant Xt_N_Resource_String;
   Xm_N_Selected_Position       : constant Xt_N_Resource_String;
   Xm_N_Selection_Callback      : constant Xt_N_Resource_String;

-- UseMotif2.1
   Xm_N_Text_Field              : constant Xt_N_Resource_String;
   Xm_N_Visible_Item_Count      : constant Xt_N_Resource_String;
-- EndMotif2.1

private

   for Combo_Box_Type use (Combo_Box => 0, Drop_Down_Combo_Box => 1, Drop_Down_List => 2);
   for Combo_Box_Type'Size use Interfaces.C.unsigned_char'Size;

   for Match_Behavior_Type use (None => 0, Quick_Navigate => 1, Invalid_Match_Behavior => 2);
   for Match_Behavior_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.1
   for Position_Mode_Type use (Zero_Based => 0, One_Based => 1);
   for Position_Mode_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.1

   pragma Import (C, Xm_Combo_Box_Update, "XmComboBoxUpdate");
-- UseMotif2.1
   pragma Import (C, Xm_Combo_Box_Delete_Pos, "XmComboBoxDeletePos");
   pragma Import (C, Xm_Combo_Box_Select_Item, "XmComboBoxSelectItem");
   pragma Import (C, Xm_Combo_Box_Set_Item, "XmComboBoxSetItem");
-- EndMotif2.1

   c_const_Xm_Combo_Box_Widget_Class           : Widget_Class;

   pragma Import (C, c_const_Xm_Combo_Box_Widget_Class, "xmComboBoxWidgetClass");

   Xm_Combo_Box_Widget_Class           : constant Widget_Class :=
    c_const_Xm_Combo_Box_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Arrow_Size              : constant Xt_N_Resource_String
      := To_Resource_String ("arrowSize");
   Xm_N_Arrow_Spacing           : constant Xt_N_Resource_String
      := To_Resource_String ("arrowSpacing");
-- UseMotif2.1
   Xm_N_Columns                 : constant Xt_N_Resource_String
      := To_Resource_String ("columns");
-- EndMotif2.1
   Xm_N_Combo_Box_Type          : constant Xt_N_Resource_String
      := To_Resource_String ("comboBoxType");

   Xm_N_Font_List               : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Font_List;
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String
      := To_Resource_String ("highlightThickness");
-- UseMotif2.1
   Xm_N_Item_Count              : constant Xt_N_Resource_String
      := To_Resource_String ("itemCount");
   Xm_N_Items                   : constant Xt_N_Resource_String
      := To_Resource_String ("items");
   Xm_N_List                    : constant Xt_N_Resource_String
      := To_Resource_String ("list");
-- EndMotif2.1
   Xm_N_Margin_Height           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Match_Behavior          : constant Xt_N_Resource_String
      := To_Resource_String ("matchBehavior");
-- UseMotif2.1
   Xm_N_Position_Mode           : constant Xt_N_Resource_String
      := To_Resource_String ("positionMode");
-- EndMotif2.1

   Xm_N_Render_Table            : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Render_Table;
   Xm_N_Selected_Item           : constant Xt_N_Resource_String
      := To_Resource_String ("selectedItem");
   Xm_N_Selected_Position       : constant Xt_N_Resource_String
      := To_Resource_String ("selectedPosition");
   Xm_N_Selection_Callback      : constant Xt_N_Resource_String
      := To_Resource_String ("selectionCallback");

-- UseMotif2.1
   Xm_N_Text_Field              : constant Xt_N_Resource_String
      := To_Resource_String ("textField");
   Xm_N_Visible_Item_Count      : constant Xt_N_Resource_String
      := To_Resource_String ("visibleItemCount");
-- EndMotif2.1

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Combo_Box;
