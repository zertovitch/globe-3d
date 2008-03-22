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

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Primitive.List is


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_List_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_List_Callback_Struct,
            Xm_List_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Value_Changed,
	                                2 => Cr_Single_Select,
					3 => Cr_Multiple_Select,
					4 => Cr_Extended_Select,
					5 => Cr_Browse_Select,
					6 => Cr_Default_Action));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);




   -- -------------------------------------------------------------------------
   --
   -- XmIsList
   --
   function Xm_Is_List (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_List_Widget_Class);
   end Xm_Is_List;


   -- -------------------------------------------------------------------------
   --
   -- XmCreateList
   --
   function Xm_Create_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateList
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal) return Widget;
      pragma Import (C, XmCreateList, "XmCreateList");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateList (Parent,
                           Name_String'Address,
                           X_Toolkit.Internal.Hook (Arglist),
                           Cardinal (Length (Arglist)));
   end Xm_Create_List;


   -- -------------------------------------------------------------------------
   --
   -- XmCreateScrolledList
   --
   function Xm_Create_Scrolled_List
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateScrolledList
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal) return Widget;
      pragma Import (C, XmCreateScrolledList, "XmCreateScrolledList");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateScrolledList (Parent,
                                   Name_String'Address,
                           X_Toolkit.Internal.Hook (Arglist),
                           Cardinal (Length (Arglist)));
   end Xm_Create_Scrolled_List;


   -- C routines needed for the following procedures
   --
   procedure XmListAddItem
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer);
   pragma Import (C, XmListAddItem, "XmListAddItem");

   procedure XmListAddItems
     (W 	 : in Widget;
      Items	 : in System.Address;
      Item_Count : in Integer;
      Pos	 : in Integer);
   pragma Import (C, XmListAddItems, "XmListAddItems");

   procedure XmListAddItemUnselected
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer);
   pragma Import (C, XmListAddItemUnselected, "XmListAddItemUnselected");

   procedure XmListAddItemsUnselected
     (W 	 : in Widget;
      Items	 : in System.Address;
      Item_Count : in Integer;
      Pos	 : in Integer);
   pragma Import (C, XmListAddItemsUnselected, "XmListAddItemsUnselected");


   procedure XmListDeleteItem
     (W    : in Widget;
      Item : in Xm_String);
   pragma Import (C, XmListDeleteItem, "XmListDeleteItem");

   procedure XmListDeleteItems
     (W 	 : in Widget;
      Items	 : in System.Address;
      Item_Count : in Integer);
   pragma Import (C, XmListDeleteItems, "XmListDeleteItems");

   procedure XmListDeletePos
     (W    : in Widget;
      Pos  : in Integer);
   pragma Import (C, XmListDeletePos, "XmListDeletePos");

   procedure XmListDeletePositions
     (W 	     : in Widget;
      Position_List  : in System.Address;
      Position_Count : in Integer);
   pragma Import (C, XmListDeletePositions, "XmListDeletePositions");

   procedure XmListDeleteItemsPos
     (W 	 : in Widget;
      Item_Count : in Integer;
      Pos	 : in Integer);
   pragma Import (C, XmListDeleteItemsPos, "XmListDeleteItemsPos");


   procedure XmListReplaceItems
     (W 	 : in Widget;
      Old_Items  : in System.Address;
      Item_Count : in Integer;
      New_Items  : in System.Address);
   pragma Import (C, XmListReplaceItems, "XmListReplaceItems");

   procedure XmListReplaceItemsPos
     (W 	 : in Widget;
      New_Items  : in System.Address;
      Item_Count : in Integer;
      Pos	 : in Integer);
   pragma Import (C, XmListReplaceItemsPos, "XmListReplaceItemsPos");

   procedure XmListReplaceItemsUnselected
     (W 	 : in Widget;
      Old_Items  : in System.Address;
      Item_Count : in Integer;
      New_Items  : in System.Address);
   pragma Import (C, XmListReplaceItemsUnselected, "XmListReplaceItemsUnselected");

   procedure XmListReplaceItemsPosUnselected
     (W 	 : in Widget;
      New_Items  : in System.Address;
      Item_Count : in Integer;
      Pos	 : in Integer);
   pragma Import (C, XmListReplaceItemsPosUnselected, "XmListReplaceItemsPosUnselected");

   procedure XmListReplacePositions
     (W 	    : in Widget;
      Position_List : in System.Address;
      Item_List     : in System.Address;
      Item_Count    : in Integer);
   pragma Import (C, XmListReplacePositions, "XmListReplacePositions");


   procedure XmListSelectItem
     (W      : in Widget;
      Item   : in Xm_String;
      Notify : in Xt_Boolean);
   pragma Import (C, XmListSelectItem, "XmListSelectItem");

   procedure XmListSelectPos
     (W      : in Widget;
      Pos    : in Integer;
      Notify : in Xt_Boolean);
   pragma Import (C, XmListSelectPos, "XmListSelectPos");


   procedure XmListDeselectItem
     (W      : in Widget;
      Item   : in Xm_String);
   pragma Import (C, XmListDeselectItem, "XmListDeselectItem");


   procedure XmListDeselectPos
     (W      : in Widget;
      Pos    : in Integer);
   pragma Import (C, XmListDeselectPos, "XmListDeselectPos");


   procedure XmListSetPos
     (W     : in Widget;
      Pos   : in Integer);
   pragma Import (C, XmListSetPos, "XmListSetPos");

   procedure XmListSetBottomPos
     (W     : in Widget;
      Pos   : in Integer);
   pragma Import (C, XmListSetBottomPos, "XmListSetBottomPos");

   procedure XmListSetHorizPos
     (W     : in Widget;
      Pos   : in Integer);
   pragma Import (C, XmListSetHorizPos, "XmListSetHorizPos");

   procedure XmListSetItem
     (W     : in Widget;
      Item  : in Xm_String);
   pragma Import (C, XmListSetItem, "XmListSetItem");

   procedure XmListSetBottomItem
     (W     : in Widget;
      Item  : in Xm_String);
   pragma Import (C, XmListSetBottomItem, "XmListSetBottomItem");


   procedure XtFree (Adr : in System.Address);
   pragma Import (C, XtFree, "XtFree");


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Add....
   --

   procedure Xm_List_Add_Item
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer) is
   begin
      XmListAddItem (W, Item, Pos);
   end Xm_List_Add_Item;
   pragma Inline (Xm_List_Add_Item);


   procedure Xm_List_Add_Items
     (W     : in Widget;
      Items : in Xm_String_List;
      Pos   : in Integer) is
   begin
      XmListAddItems (W, Items (Items'First)'Address, Integer(Items'Length), Pos);
   end Xm_List_Add_Items;
   pragma Inline (Xm_List_Add_Items);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Add....Unselected
   --

   procedure Xm_List_Add_Item_Unselected
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer) is
   begin
      XmListAddItemUnselected (W, Item, Pos);
   end Xm_List_Add_Item_Unselected;
   pragma Inline (Xm_List_Add_Item_Unselected);


   procedure Xm_List_Add_Items_Unselected
     (W     : in Widget;
      Items : in Xm_String_List;
      Pos   : in Integer) is
   begin
      XmListAddItemsUnselected (W, Items (Items'First)'Address, Integer(Items'Length), Pos);
   end Xm_List_Add_Items_Unselected;
   pragma Inline (Xm_List_Add_Items_Unselected);



   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Delete
   --
   procedure Xm_List_Delete_Item
     (W     : in Widget;
      Item  : in Xm_String) is
   begin
      XmListDeleteItem (W, Item);
   end Xm_List_Delete_Item;
   pragma Inline (Xm_List_Delete_Item);


   procedure Xm_List_Delete_Items
     (W     : in Widget;
      Items : in Xm_String_List) is
   begin
      XmListDeleteItems (W, Items (Items'First)'Address, Integer (Items'Length));
   end Xm_List_Delete_Items;
   pragma Inline (Xm_List_Delete_Items);


   procedure Xm_List_Delete_Pos
     (W    : in Widget;
      Pos  : in Integer) is
   begin
      XmListDeletePos (W, Pos);
   end Xm_List_Delete_Pos;
   pragma Inline (Xm_List_Delete_Pos);


   procedure Xm_List_Delete_Positions
     (W             : in Widget;
      Position_List : in Integer_Array_Type) is
   begin
      XmListDeletePositions (W, Position_List (Position_List'First)'Address,
                             Integer (Position_List'Length));
   end Xm_List_Delete_Positions;
   pragma Inline (Xm_List_Delete_Positions);


   procedure Xm_List_Delete_Items_Pos
     (W          : in Widget;
      Item_Count : in Integer;
      Pos        : in Integer) is
   begin
      XmListDeleteItemsPos (W, Item_Count, Pos);
   end Xm_List_Delete_Items_Pos;
   pragma Inline (Xm_List_Delete_Items_Pos);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Replace
   --

   procedure Xm_List_Replace_Items
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List) is
   begin
      if Old_Items'Length = New_Items'Length then
         XmListReplaceItems (W,
                             Old_Items (Old_Items'First)'Address,
                             Integer (Old_Items'Length),
                             New_Items (New_Items'First)'Address);
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace_Items;
   pragma Inline (Xm_List_Replace_Items);


   procedure Xm_List_Replace_Items_Pos
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer) is
   begin
      XmListReplaceItemsPos (W,
                             New_Items (New_Items'First)'Address,
                             Integer (New_Items'Length),
                             Pos);
   end Xm_List_Replace_Items_Pos;
   pragma Inline (Xm_List_Replace_Items_Pos);


   procedure Xm_List_Replace_Items_Unselected
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List) is
   begin
      if Old_Items'Length = New_Items'Length then
         XmListReplaceItemsUnselected (W,
                             Old_Items (Old_Items'First)'Address,
                             Integer (Old_Items'Length),
                             New_Items (New_Items'First)'Address);
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace_Items_Unselected;
   pragma Inline (Xm_List_Replace_Items_Unselected);


   procedure Xm_List_Replace_Items_Pos_Unselected
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer) is
   begin
      XmListReplaceItemsPosUnselected (W,
                             New_Items (New_Items'First)'Address,
                             Integer (New_Items'Length),
                             Pos);
   end Xm_List_Replace_Items_Pos_Unselected;
   pragma Inline (Xm_List_Replace_Items_Pos_Unselected);


   procedure Xm_List_Replace_Positions
     (W             : in Widget;
      Position_List : in Integer_Array_Type;
      Item_List     : in Xm_String_List) is
   begin
      if Position_List'Length = Item_List'Length then
         XmListReplacePositions (W,
                             Position_List (Position_List'First)'Address,
                             Item_List (Item_List'First)'Address,
                             Integer (Position_List'Length));
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace_Positions;
   pragma Inline (Xm_List_Replace_Positions);


   procedure Xm_List_Replace
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List) is
   begin
      if Old_Items'Length = New_Items'Length then
         XmListReplaceItems (W,
                             Old_Items (Old_Items'First)'Address,
                             Integer (Old_Items'Length),
                             New_Items (New_Items'First)'Address);
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace;
   pragma Inline (Xm_List_Replace);


   procedure Xm_List_Replace
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer) is
   begin
      XmListReplaceItemsPos (W,
                             New_Items (New_Items'First)'Address,
                             Integer (New_Items'Length),
                             Pos);
   end Xm_List_Replace;
   pragma Inline (Xm_List_Replace);


   procedure Xm_List_Replace_Unselected
     (W          : in Widget;
      Old_Items  : in Xm_String_List;
      New_Items  : in Xm_String_List) is
   begin
      if Old_Items'Length = New_Items'Length then
         XmListReplaceItemsUnselected (W,
                             Old_Items (Old_Items'First)'Address,
                             Integer (Old_Items'Length),
                             New_Items (New_Items'First)'Address);
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace_Unselected;
   pragma Inline (Xm_List_Replace_Unselected);


   procedure Xm_List_Replace_Unselected
     (W          : in Widget;
      New_Items  : in Xm_String_List;
      Pos        : in Integer) is
   begin
      XmListReplaceItemsPosUnselected (W,
                             New_Items (New_Items'First)'Address,
                             Integer (New_Items'Length),
                             Pos);
   end Xm_List_Replace_Unselected;
   pragma Inline (Xm_List_Replace_Unselected);


   procedure Xm_List_Replace
     (W             : in Widget;
      Position_List : in Integer_Array_Type;
      Item_List     : in Xm_String_List) is
   begin
      if Position_List'Length = Item_List'Length then
         XmListReplacePositions (W,
                             Position_List (Position_List'First)'Address,
                             Item_List (Item_List'First)'Address,
                             Integer (Position_List'Length));
      else
         raise Invalid_Items_Count;
      end if;
   end Xm_List_Replace;
   pragma Inline (Xm_List_Replace);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Select
   --

   procedure Xm_List_Select_Item
     (W      : in Widget;
      Item   : in Xm_String;
      Notify : in Boolean) is
   begin
      XmListSelectItem (W, Item, To_Xt_Boolean (Notify));
   end Xm_List_Select_Item;
   pragma Inline (Xm_List_Select_Item);


   procedure Xm_List_Select_Pos
     (W      : in Widget;
      Pos    : in Integer;
      Notify : in Boolean) is
   begin
      XmListSelectPos (W, Pos, To_Xt_Boolean (Notify));
   end Xm_List_Select_Pos;
   pragma Inline (Xm_List_Select_Pos);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Deselect
   --

   procedure Xm_List_Deselect_Item
     (W      : in Widget;
      Item   : in Xm_String) is
   begin
      XmListDeselectItem (W, Item);
   end Xm_List_Deselect_Item;
   pragma Inline (Xm_List_Deselect_Item);


   procedure Xm_List_Deselect_Pos
     (W      : in Widget;
      Pos    : in Integer) is
   begin
      XmListDeselectPos (W, Pos);
   end Xm_List_Deselect_Pos;
   pragma Inline (Xm_List_Deselect_Pos);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set...
   --
   procedure Xm_List_Set_Pos
     (W     : in Widget;
      Pos   : in Integer) is
   begin
      XmListSetPos (W, Pos);
   end Xm_List_Set_Pos;
   pragma Inline (Xm_List_Set_Pos);


   procedure Xm_List_Set_Bottom_Pos
     (W     : in Widget;
      Pos   : in Integer) is
   begin
      XmListSetBottomPos (W, Pos);
   end Xm_List_Set_Bottom_Pos;
   pragma Inline (Xm_List_Set_Bottom_Pos);


   procedure Xm_List_Set_Horiz_Pos
     (W     : in Widget;
      Pos   : in Integer) is
   begin
      XmListSetHorizPos (W, Pos);
   end Xm_List_Set_Horiz_Pos;
   pragma Inline (Xm_List_Set_Horiz_Pos);


   procedure Xm_List_Set_Item
     (W     : in Widget;
      Item  : in Xm_String) is
   begin
      XmListSetItem (W, Item);
   end Xm_List_Set_Item;
   pragma Inline (Xm_List_Set_Item);


   procedure Xm_List_Set_Bottom_Item
     (W     : in Widget;
      Item  : in Xm_String) is
   begin
      XmListSetBottomItem (W, Item);
   end Xm_List_Set_Bottom_Item;
   pragma Inline (Xm_List_Set_Bottom_Item);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set_Add_Mode
   --
   procedure Xm_List_Set_Add_Mode
     (W        : in Widget;
      Add_Mode : in Boolean) is
      procedure XmListSetAddMode
        (W        : in Widget;
         Add_Mode : in Xt_Boolean);
      pragma Import (C, XmListSetAddMode, "XmListSetAddMode");
   begin
      XmListSetAddMode (W, To_Xt_Boolean (Add_Mode));
   end Xm_List_Set_Add_Mode;
   pragma Inline (Xm_List_Set_Add_Mode);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Item_Exists
   --
   function Xm_List_Item_Exists
     (W        : in Widget;
      Item     : in Xm_String)
      return Boolean is
      function XmListItemExists
        (W        : in Widget;
         Item     : in Xm_String)
         return Xt_Boolean;
      pragma Import (C, XmListItemExists, "XmListItemExists");
   begin
      return To_Boolean (XmListItemExists (W, Item));
   end Xm_List_Item_Exists;
   pragma Inline (Xm_List_Item_Exists);



   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Set_Kbd_Item_Pos
   --
   procedure Xm_List_Set_Kbd_Item_Pos
     (W   : in Widget;
      Pos : in Integer) is
      function XmListSetKbdItemPos
        (W   : in Widget;
         Pos : in Integer)
	 return Xt_Boolean;
      pragma Import (C, XmListSetKbdItemPos, "XmListSetKbdItemPos");
   begin
      if not To_Boolean (XmListSetKbdItemPos (W, Pos)) then
         raise Invalid_List_Item_Pos;
      end if;
   end Xm_List_Set_Kbd_Item_Pos;
   pragma Inline (Xm_List_Set_Kbd_Item_Pos);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Pos_Selected
   --
   function Xm_List_Pos_Selected
     (W   : in Widget;
      Pos : in Integer) return Boolean is
      function XmListPosSelected
        (W   : in Widget;
         Pos : in Integer)
	 return Xt_Boolean;
      pragma Import (C, XmListPosSelected, "XmListPosSelected");
   begin
      return To_Boolean (XmListPosSelected (W, Pos));
   end Xm_List_Pos_Selected;
   pragma Inline (Xm_List_Pos_Selected);


   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Pos_To_Bounds
   --
   procedure Xm_List_Pos_To_Bounds
     (W        : in Widget;
      Pos      : in Integer;
      X        : out X_Lib.Position;
      Y        : out X_Lib.Position;
      Width    : out X_Lib.Dimension;
      Height   : out X_Lib.Dimension) is
      function XmListPosToBounds
        (W        : in Widget;
         Pos      : in Integer;
         X        : in System.Address;
         Y        : in System.Address;
         Width    : in System.Address;
         Height   : in System.Address)
	 return Xt_Boolean;
      pragma Import (C, XmListPosToBounds, "XmListPosToBounds");
   begin
      if not To_Boolean (XmListPosToBounds (W, Pos,
                                            X'Address, Y'Address,
                                            Width'Address, Height'Address)) then
         raise Invalid_List_Item_Pos;
      end if;
   end Xm_List_Pos_To_Bounds;
   pragma Inline (Xm_List_Pos_To_Bounds);



   function Xm_List_Get_Match
     (W        : in     Widget;
      Item     : in     Xm_String)
      return Integer_Array_Type is

      function XmListGetMatchPos
        (W         : in Widget;
         Item      : in Xm_String;
         Pos_List  : in System.Address;
         Pos_Count : in System.Address)
	 return Xt_Boolean;
      pragma Import (C, XmListGetMatchPos, "XmListGetMatchPos");

      Hook   : System.Address;
      Number : Integer;
   begin
      if not To_Boolean (XmListGetMatchPos (W, Item, Hook'Address, Number'Address)) then
         return Integer_Array_Type'(1 .. 0 => 0);
      else
         declare
	    subtype My_Integer_Array_Type is Integer_Array_Type (1 .. Number);
	    type My_Integer_Array_Access_Type is access My_Integer_Array_Type;
	    My_Integer_Arry : My_Integer_Array_Type;
	    function To_My_Array is
	       new Ada.Unchecked_Conversion (System.Address, My_Integer_Array_Access_Type);
	 begin
	    My_Integer_Arry := To_My_Array (Hook).all;
	    XtFree (Hook);
	    return My_Integer_Arry;
	 end;
      end if;
   end Xm_List_Get_Match;
   pragma Inline (Xm_List_Get_Match);



   -- -------------------------------------------------------------------------
   --
   -- Xm_List_Get_Selected_Pos
   --
   function Xm_List_Get_Selected
     (W        : in     Widget)
      return Integer_Array_Type is

      function XmListGetSelectedPos
        (W         : in Widget;
         Pos_List  : in System.Address;
         Pos_Count : in System.Address)
	 return Xt_Boolean;
      pragma Import (C, XmListGetSelectedPos, "XmListGetSelectedPos");

      Hook   : System.Address;
      Number : Integer;
   begin
      if not To_Boolean (XmListGetSelectedPos (W, Hook'Address, Number'Address)) then
         return Integer_Array_Type'(1 .. 0 => 0);
      else
         declare
	    subtype My_Integer_Array_Type is Integer_Array_Type (1 .. Number);
	    type My_Integer_Array_Access_Type is access My_Integer_Array_Type;
	    My_Integer_Arry : My_Integer_Array_Type;
	    function To_My_Array is
	       new Ada.Unchecked_Conversion (System.Address, My_Integer_Array_Access_Type);
	 begin
	    My_Integer_Arry := To_My_Array (Hook).all;
	    XtFree (Hook);
	    return My_Integer_Arry;
	 end;
      end if;
   end Xm_List_Get_Selected;
   pragma Inline (Xm_List_Get_Selected);






   function To_Integer is
      new Ada.Unchecked_Conversion (List_Size_Policy_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     List_Size_Policy_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out List_Size_Policy_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- UseMotif2.0 Motif2.1
   function To_Integer is
      new Ada.Unchecked_Conversion (Match_Behavior_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Match_Behavior_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Match_Behavior_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Primary_Ownership_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Primary_Ownership_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Primary_Ownership_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Selection_Mode_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Mode_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1


   function To_Integer is
      new Ada.Unchecked_Conversion (Selection_Policy_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Policy_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Policy_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


end Xm_Widgets.Primitive.List;
