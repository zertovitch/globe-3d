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
package body Xm_Widgets.Manager.Container is

-- UseMotif2.0 Motif2.1

   function Xm_Is_Container (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Container_Widget_Class);
   end Xm_Is_Container;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Container_Outline_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Container_Outline_Callback_Struct,
            Xm_Container_Outline_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Collapsed, 2 => Cr_Expanded));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Container_Select_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Container_Select_Callback_Struct,
            Xm_Container_Select_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Single_Select,
	                                2 => Cr_Multiple_Select,
					3 => Cr_Extended_Select,
					4 => Cr_Browse_Select,
					5 => Cr_Default_Action));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Create_Container
    (Parent   : in  Widget; 
     Name     : in  String;
     Arglist  : in  Arg_List := Null_Arg_List)
     return Widget is
      function XmCreateContainer
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateContainer, "XmCreateContainer");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateContainer (Parent,
                                Name_String'Address,
                                X_Toolkit.Internal.Hook (Arglist),
                                Cardinal (Length (Arglist)));
   end Xm_Create_Container;


   procedure Xm_Container_Get_Item_Children
    (Wid            : in     Widget;
     Item           : in     Widget;
     Item_Children  : in out Widget_List) is

      function XmContainerGetItemChildren (
                Wid            : in Widget;
                Item           : in Widget;
                Item_Children  : in System.Address)
                return Integer;
      pragma Import (C, XmContainerGetItemChildren, "XmContainerGetItemChildren");
      Num_Elems     : Integer;
      Where_To_Find : System.Address;
   begin
      Num_Elems := XmContainerGetItemChildren (Wid, Item, Where_To_Find'Address);
      declare
         type Widget_Array_Return is array (1 .. Num_Elems) of Widget;
         type Wi_List is access Widget_Array_Return;

         function To_Wa is new
            Ada.Unchecked_Conversion (System.Address, Wi_List);
      begin
         -- first empty the list
         Item_Children := Null_Widget_List;
         for I in 1 .. Num_Elems loop
            Append (Item_Children, To_Wa (Where_To_Find).all (I));
         end loop;
      end;
   end Xm_Container_Get_Item_Children;


   procedure Xm_Container_Reorder
     (Wid       : in Widget;
      Cwid_List : in Widget_List) is
      procedure XmContainerReorder
        (Wid        : in Widget;
         Cwid_List  : in X_Toolkit.Internal.Widget_Access;
         Cwid_Count : in Integer);
      pragma Import (C, XmContainerReorder, "XmContainerReorder");
   begin
      XmContainerReorder (Wid,
                          X_Toolkit.Internal.Hook (Cwid_List),
			  Integer (Length (Cwid_List)));
   end Xm_Container_Reorder;


   function Xm_Container_Cut (Wid       : in Widget;
                              Timestamp : in X_Lib.Server_Time)
                              return Boolean is
      function XmContainerCut (Wid       : in Widget;
                               Timestamp : in X_Lib.Server_Time)
                               return Xt_Boolean;
      pragma Import (C, XmContainerCut, "XmContainerCut");
   begin
      return XmContainerCut (Wid, Timestamp) = Xt_Boolean'(True);
   end Xm_Container_Cut;
   pragma Inline (Xm_Container_Cut);


   function Xm_Container_Copy (Wid       : in Widget;
                              Timestamp : in X_Lib.Server_Time)
                              return Boolean is
      function XmContainerCopy (Wid       : in Widget;
                               Timestamp : in X_Lib.Server_Time)
                               return Xt_Boolean;
      pragma Import (C, XmContainerCopy, "XmContainerCopy");
   begin
      return XmContainerCopy (Wid, Timestamp) = Xt_Boolean'(True);
   end Xm_Container_Copy;
   pragma Inline (Xm_Container_Copy);


   function Xm_Container_Paste (Wid : in Widget) return Boolean is
      function XmContainerPaste (Wid : in Widget) return Xt_Boolean;
      pragma Import (C, XmContainerPaste, "XmContainerPaste");
   begin
      return XmContainerPaste (Wid) = Xt_Boolean'(True);
   end Xm_Container_Paste;
   pragma Inline (Xm_Container_Paste);


   function Xm_Container_Copy_Link (Wid       : in Widget;
                                    Timestamp : in X_Lib.Server_Time)
                                    return Boolean is
      function XmContainerCopyLink (Wid       : in Widget;
                                    Timestamp : in X_Lib.Server_Time)
                                    return Xt_Boolean;
     pragma Import (C, XmContainerCopyLink, "XmContainerCopyLink");
   begin
      return XmContainerCopyLink (Wid, Timestamp) = Xt_Boolean'(True);
   end Xm_Container_Copy_Link;
   pragma Inline (Xm_Container_Copy_Link);


   function Xm_Container_Paste_Link (Wid : in Widget) return Boolean is
      function XmContainerPasteLink (Wid : in Widget) return Xt_Boolean;
      pragma Import (C, XmContainerPasteLink, "XmContainerPasteLink");
   begin
      return XmContainerPasteLink (Wid) = Xt_Boolean'(True);
   end Xm_Container_Paste_Link;
   pragma Inline (Xm_Container_Paste_Link);



   -- -------------------------------------------------------------------------
   --
   --   Argument list handling
   --


   type Automatic_Selection_Int is range 0 .. 2**Automatic_Selection_Type'Size - 1;
   for Automatic_Selection_Int'Size use Automatic_Selection_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Automatic_Selection_Type, Automatic_Selection_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Automatic_Selection_Type) is
   begin
      Append_Set (List,
                  Name,
                  Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Automatic_Selection_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Entry_View_Type_Int is range 0 .. 2**Entry_View_Type'Size - 1;
   for Entry_View_Type_Int'Size use Entry_View_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Entry_View_Type, Entry_View_Type_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Entry_View_Type) is
   begin
      Append_Set (List,
                  Name,
                  Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Entry_View_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Layout_Type_Int is range 0 .. 2**Layout_Type'Size - 1;
   for Layout_Type_Int'Size use Layout_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Layout_Type, Layout_Type_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Layout_Type) is
   begin
      Append_Set (List,
                  Name,
                  Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Layout_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Outline_Button_Policy_Int is range 0 .. 2**Outline_Button_Policy_Type'Size - 1;
   for Outline_Button_Policy_Int'Size use Outline_Button_Policy_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Outline_Button_Policy_Type, Outline_Button_Policy_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_Button_Policy_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Outline_Button_Policy_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Outline_Line_Style_Int is range 0 .. 2**Outline_Line_Style_Type'Size - 1;
   for Outline_Line_Style_Int'Size use Outline_Line_Style_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Outline_Line_Style_Type, Outline_Line_Style_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_Line_Style_Type) is
   begin
      Append_Set (List,
                  Name,
                  Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Outline_Line_Style_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Primary_Ownership_Int is range 0 .. 2**Primary_Ownership_Type'Size - 1;
   for Primary_Ownership_Int'Size use Primary_Ownership_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Primary_Ownership_Type, Primary_Ownership_Int);

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
                         Value : out    Primary_Ownership_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Selection_Policy_Int is range 0 .. 2**Selection_Policy_Type'Size - 1;
   for Selection_Policy_Int'Size use Selection_Policy_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Selection_Policy_Type, Selection_Policy_Int);

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


   type Selection_Technique_Int is range 0 .. 2**Selection_Technique_Type'Size - 1;
   for Selection_Technique_Int'Size use Selection_Technique_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Selection_Technique_Type, Selection_Technique_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Selection_Technique_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Selection_Technique_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Spatial_Include_Model_Int is range 0 .. 2**Spatial_Include_Model_Type'Size - 1;
   for Spatial_Include_Model_Int'Size use Spatial_Include_Model_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Spatial_Include_Model_Type, Spatial_Include_Model_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Include_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Spatial_Include_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Spatial_Resize_Model_Int is range 0 .. 2**Spatial_Resize_Model_Type'Size - 1;
   for Spatial_Resize_Model_Int'Size use Spatial_Resize_Model_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Spatial_Resize_Model_Type, Spatial_Resize_Model_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Resize_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Resize_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Spatial_Snap_Model_Int is range 0 .. 2**Spatial_Snap_Model_Type'Size - 1;
   for Spatial_Snap_Model_Int'Size use Spatial_Snap_Model_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Spatial_Snap_Model_Type, Spatial_Snap_Model_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Snap_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Snap_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   type Spatial_Style_Int is range 0 .. 2**Spatial_Style_Type'Size - 1;
   for Spatial_Style_Int'Size use Spatial_Style_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Spatial_Style_Type, Spatial_Style_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Spatial_Style_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Spatial_Style_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   --
   -- contraint resources for children of Xm_Container
   --
   type Outline_State_Int is range 0 .. 2**Outline_State_Type'Size - 1;
   for Outline_State_Int'Size use Outline_State_Type'Size;
   function To_Integer is
      new Ada.Unchecked_Conversion (Outline_State_Type, Outline_State_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Outline_State_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
		  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Outline_State_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Container;
