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
--          22 Jun 2001 Vadim Godunko: add Append_Set/Append_Get for Xm_String_Char_Set
--          17 Nov 2001 Vadim Godunko: add tab list related routines
--                                     Xm_String_Table_Propose_Tablist,
--                                     Xm_Tab_List_Free
--                                     change Xm_Rendition_Free,
--                                     Xm_Render_Table_Free,
--                                     Xm_Parse_Mapping_Free,
--                                     Xm_Parse_Table_Free,
--                                     Xm_String_Free, Xm_String_Free_Context
--                                     argument access
--                                     add Xm_Render_Table_Copy
--          13 Jan 2002 H.-F. Vogt: make Unit_Type size Short_Short_Unsigned
--                                  add tab related routines
--                                  Xm_Tab_Create, Xm_Tab_Free,
--                                  Xm_Tab_Get_Values
--                                  add tab list related routines
--                                  Xm_Tab_List_Insert_Tabs,
--                                  Xm_Tab_List_Remove_Tabs,
--                                  Xm_Tab_List_Replace_Positions
--          20 Jan 2002 H.-F. Vogt: add Xm_Get_Scaled_Pixmap
--                                  add Append_Set and Append_Get routines
--                                  for type Orientation_Type
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation,
     Interfaces.C.Strings,
     Interfaces.C.Wstrings,
     X_Lib,
     X_Toolkit.Internal;
use type Interfaces.C.unsigned_long;

package body Xm_Widgets is

   procedure XtFree (Adr : in System.Address);
   procedure XtFree (Str : in Interfaces.C.Strings.chars_ptr);
   procedure XtFree (Str : in Interfaces.C.Wstrings.wchars_ptr);
   pragma Import (C, XtFree, "XtFree");


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Any_Callback_Struct_Access is
      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Xm_Any_Callback_Struct_Access);
   begin
      -- test if Pointer is Null Pointer
      if X_Lib."=" (Pointer, Null_Xt_Pointer) then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Popup_Handler_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Popup_Handler_Callback_Struct,
             Xm_Popup_Handler_Callback_Struct_Access,
             Callback_Reason_Array_Type'(1 => Cr_Post, 2 => Cr_Repost));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function To_Generic_Callback_Struct_Access (Pointer : in Xt_Pointer)
      return Callback_Struct_Access is

      type Callback_Reason_Pointer is
         access all Callback_Reason;
      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Callback_Struct_Access);
      function Go_Reason   is
         new Ada.Unchecked_Conversion (Xt_Pointer, Callback_Reason_Pointer);

      CS_Reason  : Callback_Reason_Pointer;
      Reason_Accepted : Boolean := False;
   begin
      -- test if Pointer is Null Pointer
      if X_Lib."=" (Pointer, Null_Xt_Pointer) then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      -- test if reason is valid
      CS_Reason  := Go_Reason (Pointer);
      for I in Valid_Reasons'Range loop
         if CS_Reason.all = Valid_Reasons (I) then
	    Reason_Accepted := True;
	 end if;
      end loop;
      if not Reason_Accepted then
         raise Xm_Error_Invalid_Callback_Reason;
      end if;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Generic_Callback_Struct_Access;


-- ----------------------------------------------------------------------------
--
--                      P I X M A P  and   I M A G E
--
--  routines related to pixmap and image caching
--

   function Xm_Install_Image
     (Image_Name  : in String)
      return X_Lib.X_Image_Pointer is
      function XmInstallImage
        (Image      : access X_Lib.X_Image;
	 Image_Name : in System.Address)
	 return Xt_Boolean;
      pragma Import (C, XmInstallImage, "XmInstallImage");
      
      Image_Name_String : constant Interfaces.C.Char_Array
                        := Interfaces.C.To_C (Image_Name, Append_Nul => True);
      Return_Image : aliased X_Lib.X_Image;
   begin
      if XmInstallImage (Return_Image'Access, Image_Name_String'Address) = Xt_Boolean'(False) then
         raise Xm_Cache_Error;
      else
         return new X_Lib.X_Image'(Return_Image);
      end if; 
   end Xm_Install_Image;


   procedure Xm_Uninstall_Image
     (Image       : in X_Lib.X_Image_Pointer) is
      function XmUninstallImage
        (Image      : in X_Lib.X_Image_Pointer)
	 return Xt_Boolean;
      pragma Import (C, XmUninstallImage, "XmUninstallImage");
   begin
      if XmUninstallImage (Image) = Xt_Boolean'(False) then
         raise Xm_Cache_Error;
      end if;
   end Xm_Uninstall_Image;


   function Xm_Get_Pixmap
     (Screen      : in X_Lib.Screen_Pointer;
      Image_Name  : in String;
      Foreground  : in X_Lib.Pixel;
      Background  : in X_Lib.Pixel)
      return X_Lib.Pixmap_ID is
      function XmGetPixmap
        (Screen      : in X_Lib.Screen_Pointer;
         Image_Name  : in System.Address;
         Foreground  : in X_Lib.Pixel;
         Background  : in X_Lib.Pixel)
         return X_Lib.Pixmap_ID;
      pragma Import (C, XmGetPixmap, "XmGetPixmap");

      Image_Name_String : constant Interfaces.C.Char_Array
                        := Interfaces.C.To_C (Image_Name, Append_Nul => True);
   begin
      return XmGetPixmap (Screen, Image_Name_String'Address,
                          Foreground, Background);
   end Xm_Get_Pixmap;


   function Xm_Get_Pixmap_By_Depth
     (Screen      : in X_Lib.Screen_Pointer;
      Image_Name  : in String;
      Foreground  : in X_Lib.Pixel;
      Background  : in X_Lib.Pixel;
      Depth       : in X_Lib.Color_Depth)
      return X_Lib.Pixmap_ID is
      function XmGetPixmapByDepth
        (Screen      : in X_Lib.Screen_Pointer;
         Image_Name  : in System.Address;
         Foreground  : in X_Lib.Pixel;
         Background  : in X_Lib.Pixel;
         Depth       : in X_Lib.Color_Depth)
         return X_Lib.Pixmap_ID;
      pragma Import (C, XmGetPixmapByDepth, "XmGetPixmapByDepth");

      Image_Name_String : constant Interfaces.C.Char_Array
                        := Interfaces.C.To_C (Image_Name, Append_Nul => True);
   begin
      return XmGetPixmapByDepth (Screen, Image_Name_String'Address,
                                 Foreground, Background, Depth);
   end Xm_Get_Pixmap_By_Depth;


-- UseMotif2.1
   function Xm_Get_Scaled_Pixmap
     (Wid           : in Widget;
      Image_Name    : in String;
      Foreground    : in X_Lib.Pixel;
      Background    : in X_Lib.Pixel;
      Depth         : in X_Lib.Color_Depth;
      Scaling_Ratio : in Long_Float)
      return X_Lib.Pixmap_ID is
      function XmGetScaledPixmap
        (Wid         : in Widget;
         Image_Name  : in System.Address;
         Foreground  : in X_Lib.Pixel;
         Background  : in X_Lib.Pixel;
         Depth       : in X_Lib.Color_Depth;
         Scal_Ratio  : in Interfaces.C.double)
         return X_Lib.Pixmap_ID;
      pragma Import (C, XmGetScaledPixmap, "XmGetScaledPixmap");

      Image_Name_String : constant Interfaces.C.Char_Array
                        := Interfaces.C.To_C (Image_Name, Append_Nul => True);
   begin
      return XmGetScaledPixmap (Wid, Image_Name_String'Address,
                                Foreground, Background, Depth,
				Interfaces.C.double (Scaling_Ratio));
   end Xm_Get_Scaled_Pixmap;
-- EndMotif2.1

   procedure Xm_Destroy_Pixmap
     (Screen      : in X_Lib.Screen_Pointer;
      Pixmap      : in X_Lib.Pixmap_ID) is
      function XmDestroyPixmap
        (Screen      : in X_Lib.Screen_Pointer;
         Pixmap      : in X_Lib.Pixmap_ID)
	 return Xt_Boolean;
      pragma Import (C, XmDestroyPixmap, "XmDestroyPixmap");
   begin
      if XmDestroyPixmap (Screen, Pixmap) = Xt_Boolean'(False) then
         raise Xm_Cache_Error;
      end if;
   end Xm_Destroy_Pixmap;


   function To_Integer is
      new Ada.Unchecked_Conversion (Unit_Type, Interfaces.C.unsigned_char);


-- UseMotif2.0 Motif2.1
-- ----------------------------------------------------------------------------
--
--                                 T A B
--
--  tab related routines
--  tab specifies a tab stops to be used in the lay out of Xm_Strings
--

   function Xm_Tab_Create
     (Value        : in Float;
      Units        : in Unit_Type;
      Offset_Model : in Xm_Offset_Model;
      Align        : in Alignment;
      Dec_Point    : in String)    --  should be Multibyte_Char!
      return Xm_Tab is
      function XmTabCreate
        (Value        : in Interfaces.C.C_Float;
	 Units        : in Interfaces.C.unsigned_char;
	 Offset_Model : in Xm_Offset_Model;
	 Align        : in Alignment;
	 Dec_Point    : in System.Address)
	 return Xm_Tab;
      pragma Import (C, XmTabCreate, "XmTabCreate");

      Dec_Point_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Dec_Point, Append_Nul => True);
   begin
      return XmTabCreate (Interfaces.C.C_Float (Value),
                          Interfaces.C.unsigned_char (To_Integer (Units)),
			  Offset_Model, Align, Dec_Point_String'Address);
   end Xm_Tab_Create;
   pragma Inline (Xm_Tab_Create);


   procedure Xm_Tab_Free (Tab : in out Xm_Tab) is
      procedure XmTabFree (Tab : in Xm_Tab);
      pragma Import (C, XmTabFree, "XmTabFree");
   begin
      XmTabFree (Tab);
      Tab := Null_Tab;
   end Xm_Tab_Free;
   pragma Inline (Xm_Tab_Free);


   procedure Xm_Tab_Get_Values
     (Tab          : in     Xm_Tab;
      Value        :    out Float;
      Units        :    out Unit_Type;
      Offset_Model :    out Xm_Offset_Model;
      Align        :    out Alignment;
      Dec_Point    :    out String) is   --  should be out Multibyte_Char
     function XmTabGetValues
       (Tab          : in Xm_Tab;
        Units        : in System.Address;
	Offset_Model : in System.Address;
	Align        : in System.Address;
	Dec_Point    : in System.Address)
	return Interfaces.C.C_Float;
      pragma Import (C, XmTabGetValues, "XmTabGetValues");
      Chars_P : Interfaces.C.Strings.chars_ptr;
      Dec_P_L : Natural;
   begin
      Value := Float (XmTabGetValues (Tab, Units'Address, Offset_Model'Address,
                                      Align'Address, Chars_P'Address));
      Dec_P_L := Natural (Interfaces.C.Strings.Strlen (Chars_P));
      if Dec_Point'Length /= Dec_P_L then
         Dec_Point (1 .. Dec_P_L) := Interfaces.C.Strings.Value (Chars_P);
      end if;
   end Xm_Tab_Get_Values;


-- ----------------------------------------------------------------------------
--
--				T A B L I S T
--
--  tab list related routines
--

   function Xm_Tab_List_Insert_Tabs
     (Tab_List : in Xm_Tab_List;
      Tabs     : in Xm_Tab_Array;
      Pos      : in Integer)
      return Xm_Tab_List is
      function XmTabListInsertTabs
        (Tab_List  : in Xm_Tab_List;
	 Tabs      : in System.Address;
	 Tab_Count : in Cardinal;
	 Pos       : in Integer)
	 return Xm_Tab_List;
      pragma Import (C, XmTabListInsertTabs, "XmTabListInsertTabs");
   begin
      return XmTabListInsertTabs (Tab_List, Tabs'Address,
                                  Cardinal (Tabs'Length), Pos);
   end Xm_Tab_List_Insert_Tabs;


   function Xm_Tab_List_Remove_Tabs
     (Tab_List : in Xm_Tab_List;
      Pos_List : in Cardinal_Array)
      return Xm_Tab_List is
      function XmTabListRemoveTabs
        (Tab_List  : in Xm_Tab_List;
	 Pos_List  : in System.Address;
	 Pos_Count : in Cardinal)
	 return Xm_Tab_List;
      pragma Import (C, XmTabListRemoveTabs, "XmTabListRemoveTabs");
   begin
      return XmTabListRemoveTabs (Tab_List, Pos_List'Address,
                                  Cardinal (Pos_List'Length));
   end Xm_Tab_List_Remove_Tabs;


   --  XmTabListReplacePositions
   --
   --  replace the Tabs at the positions given in Pos_List by the Tabs in
   --  Replace_Tabs.
   --  if the Array sizes of those arrays are different, Constraint_Error
   --  is raised
   --
   function Xm_Tab_List_Replace_Positions
     (Old_List     : in Xm_Tab_List;
      Pos_List     : in Cardinal_Array;
      Replace_Tabs : in Xm_Tab_Array)
      return Xm_Tab_List is
      function XmTabListReplacePositions
        (Old_List     : in Xm_Tab_List;
	 Pos_List     : in System.Address;
	 Replace_Tabs : in System.Address;
	 Tab_Count    : in Cardinal)
	 return Xm_Tab_List;
      pragma Import (C, XmTabListReplacePositions, "XmTabListReplacePositions");
   begin
      if Pos_List'Length /= Replace_Tabs'Length then
         raise Constraint_Error;
      else
         return XmTabListReplacePositions (Old_List, Pos_List'Address,
                                           Replace_Tabs'Address,
					   Cardinal (Pos_List'Length));
      end if;
   end Xm_Tab_List_Replace_Positions;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Table_Propose_Tablist
   --
   function Xm_String_Table_Propose_Tablist
     (Strings	   : in Xm_String_Table;
      W 	   : in Widget;
      Pad_Value    : in Float;
      Offset_Model : in Xm_Offset_Model)
      return Xm_Tab_List
   is
      function XmStringTableProposeTablist
	(Strings      : in Xm_String_Table;
	 Num_Strings  : in Cardinal;
	 W	      : in Widget;
	 Pad_Value    : in Interfaces.C.C_Float;
	 Offset_Model : in Xm_Offset_Model)
	 return Xm_Tab_List;
      pragma Import (C, XmStringTableProposeTablist,
		       "XmStringTableProposeTablist");
   begin
      return
	 XmStringTableProposeTablist (Strings, Cardinal (Strings'Length),
				      W, Interfaces.C.C_Float (Pad_Value),
				      Offset_Model);
   end Xm_String_Table_Propose_Tablist;
   pragma Inline (Xm_String_Table_Propose_Tablist);


   -- -------------------------------------------------------------------------
   --
   --  Xm_Tab_List_Free
   --
   procedure Xm_Tab_List_Free (Tablist : in out Xm_Tab_List) is
      procedure XmTabListFree (Tablist : in Xm_Tab_List);
      pragma Import (C, XmTabListFree, "XmTabListFree");
   begin
      XmTabListFree (Tablist);
      Tablist := Null_Tab_List;
   end Xm_Tab_List_Free;
   pragma Inline (Xm_Tab_List_Free);


   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xm_Tab_List)
   is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => System.Address (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xm_Tab_List)
   is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- ----------------------------------------------------------------------------
--
--                            R E N D I T I O N
--
--  rendition related routines
--

   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Create
   --
   function Xm_Rendition_Create (W             : in Widget;
                                 Rendition_Tag : in Xm_String_Tag;
                                 Arglist       : in Arg_List)
                                 return Xm_Rendition is
      function XmRenditionCreate (W             : in Widget;
                                  Rendition_Tag : in Xm_String_Tag;
                                  Arg           : in X_Toolkit.Internal.Arg_Rec_Access;
                                  Count         : in Cardinal)
                                  return Xm_Rendition;
      pragma Import (C, XmRenditionCreate, "XmRenditionCreate");
   begin
      return XmRenditionCreate (W, Rendition_Tag,
                                X_Toolkit.Internal.Hook (Arglist),
                                Cardinal (Length (Arglist)));
   end Xm_Rendition_Create;
 

   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Free
   --
   procedure Xm_Rendition_Free (Rendition : in out Xm_Rendition) is
      procedure XmRenditionFree (Rendition : in Xm_Rendition);
      pragma Import (C, XmRenditionFree, "XmRenditionFree");
   begin
      XmRenditionFree (Rendition);
      Rendition := Null_Rendition;
   end Xm_Rendition_Free;
   pragma Inline (Xm_Rendition_Free);


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Update
   --
   procedure Xm_Rendition_Update (Rendition : in Xm_Rendition;
                                  Args      : in Arg_List) is
      procedure XmRenditionUpdate (Rendition : in Xm_Rendition;
                                   Arg       : in X_Toolkit.Internal.Arg_Rec_Access;
                                   Count     : in Cardinal);
      pragma Import (C, XmRenditionUpdate, "XmRenditionUpdate");
   begin
      if Length (Args) > 0 then
         XmRenditionUpdate (Rendition,
                            X_Toolkit.Internal.Hook (Args),
                            Cardinal (Length (Args)));
      end if;
   end Xm_Rendition_Update;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Retrieve
   --
   procedure Xm_Rendition_Retrieve (Rendition : in Xm_Rendition;
                                    Args      : in Arg_List) is
      procedure XmRenditionRetrieve (Rendition : in Xm_Rendition;
                                     Arg       : in X_Toolkit.Internal.Arg_Rec_Access;
                                     Count     : in CardinaL);
      pragma Import (C, XmRenditionRetrieve, "XmRenditionRetrieve");
   begin
      if Length (Args) > 0 then
         XmRenditionRetrieve (Rendition,
                              X_Toolkit.Internal.Hook (Args),
                              Cardinal (Length (Args)));
      end if;
   end Xm_Rendition_Retrieve;



   function To_Int is
      new Ada.Unchecked_Conversion (Font_Load_Model_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Font_Load_Model_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.Long (To_Int (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Font_Load_Model_Type) is
   begin
      X_Toolkit.Append_Set (List  => List,
                            Name  => Name,
                            Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Int is
      new Ada.Unchecked_Conversion (Line_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Line_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (To_Int (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Line_Type) is
   begin
      X_Toolkit.Append_Set (List  => List,
                            Name  => Name,
                            Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



   -- -------------------------------------------------------------------------
   --
   --  XmRenderTableAddRenditions
   --
   function Xm_Render_Table_Add_Renditions (Old_Table  : in Xm_Render_Table;
                                            Renditions : in Xm_Rendition_Array;
                                            Merge_Mode : in Xm_Merge_Mode)
                                            return Xm_Render_Table is
      function XmRenderTableAddRenditions (Old_Table  : in Xm_Render_Table;
                                           Renditions : in SYSTEM.ADDRESS;
                                           Count      : in Cardinal;
                                           Merge_Mode : in Xm_Merge_Mode)
                                           return Xm_Render_Table;
      pragma Import (C, XmRenderTableAddRenditions, "XmRenderTableAddRenditions");
   begin
      if Renditions'Length < 1 then
         return XmRenderTableAddRenditions (Old_Table, System.Null_Address,
                                            0, Merge_Mode);
      else
         return XmRenderTableAddRenditions (Old_Table, Renditions (Renditions'First)'Address,
                                            Renditions'Length, Merge_Mode);
      end if;
   end Xm_Render_Table_Add_Renditions;


   -- -------------------------------------------------------------------------
   --
   --  XmRenderTableAddRenditions
   --
   function Xm_Render_Table_Copy (Table : in Xm_Render_Table;
				  Tags  : in Xm_String_Tag_Array)
      return Xm_Render_Table
   is
      function XmRenderTableCopy (Table     : in Xm_Render_Table;
				  Tags      : in System.Address;
				  Tag_Count : in Integer)
	 return Xm_Render_Table;
      pragma Import (C, XmRenderTableCopy, "XmRenderTableCopy");
   begin
      if Tags'Length = 0 then
	 return XmRenderTableCopy (Table, System.Null_Address, 0);
      else
	 return XmRenderTableCopy (Table, Tags (Tags'First)'Address, Tags'Length);
      end if;
   end Xm_Render_Table_Copy;


   -- -------------------------------------------------------------------------
   --
   --  XmRenderTableFree
   --
   procedure Xm_Render_Table_Free (Table : in out Xm_Render_Table) is
      procedure XmRenderTableFree (Table : in Xm_Render_Table);
      pragma Import (C, XmRenderTableFree, "XmRenderTableFree");
   begin
      XmRenderTableFree (Table);
      Table := Null_Render_Table;
   end Xm_Render_Table_Free;
   pragma Inline (Xm_Render_Table_Free);


-- ----------------------------------------------------------------------------
--
--                   P A R S E   M A P P I N G
--
-- parse table related routines
--

   -- -------------------------------------------------------------------------
   --
   --  XmParseMappingCreate
   --
   function Xm_Parse_Mapping_Create
     (Args      : in Arg_List)
      return Xm_Parse_Mapping is
      function XmParseMappingCreate
        (Arg       : in X_Toolkit.Internal.Arg_Rec_Access;
         Count     : in Cardinal)
	 return Xm_Parse_Mapping;
      pragma Import (C, XmParseMappingCreate, "XmParseMappingCreate");
   begin
      if Length (Args) > 0 then
         return XmParseMappingCreate (X_Toolkit.Internal.Hook (Args),
                                      Cardinal (Length (Args)));
      else
         return Null_Parse_Mapping;
      end if;
   end Xm_Parse_Mapping_Create;


   -- -------------------------------------------------------------------------
   --
   --  XmParseMappingSetValues
   --
   procedure Xm_Parse_Mapping_Set_Values
     (Parse_Mapping : in Xm_Parse_Mapping;
      Args          : in Arg_List) is
      procedure XmParseMappingSetValues
        (Parse_Mapping : in Xm_Parse_Mapping;
         Arg           : in X_Toolkit.Internal.Arg_Rec_Access;
         Count         : in Cardinal);
      pragma Import (C, XmParseMappingSetValues, "XmParseMappingSetValues");
   begin
      if Length (Args) > 0 then
         XmParseMappingSetValues (Parse_Mapping,
	                          X_Toolkit.Internal.Hook (Args),
                                  Cardinal (Length (Args)));
      end if;
   end Xm_Parse_Mapping_Set_Values;


   -- -------------------------------------------------------------------------
   --
   --  XmParseMappingGetValues
   --
   procedure Xm_Parse_Mapping_Get_Values
     (Parse_Mapping : in Xm_Parse_Mapping;
      Args          : in Arg_List) is
      procedure XmParseMappingGetValues
        (Parse_Mapping : in Xm_Parse_Mapping;
         Arg           : in X_Toolkit.Internal.Arg_Rec_Access;
         Count         : in Cardinal);
      pragma Import (C, XmParseMappingGetValues, "XmParseMappingGetValues");
   begin
      if Length (Args) > 0 then
         XmParseMappingGetValues (Parse_Mapping,
	                          X_Toolkit.Internal.Hook (Args),
                                  Cardinal (Length (Args)));
      end if;
   end Xm_Parse_Mapping_Get_Values;


   -- -------------------------------------------------------------------------
   --
   --  XmParseMappingFree
   --
   procedure Xm_Parse_Mapping_Free (Parse_Mapping : in out Xm_Parse_Mapping) is
      procedure XmParseMappingFree (Parse_Mapping : in Xm_Parse_Mapping);
      pragma Import (C, XmParseMappingFree, "XmParseMappingFree");
   begin
      XmParseMappingFree (Parse_Mapping);
      Parse_Mapping := Null_Parse_Mapping;
   end Xm_Parse_Mapping_Free;
   pragma Inline (Xm_Parse_Mapping_Free);


   -- -------------------------------------------------------------------------
   --
   --  XmParseTableFree
   --
   procedure Xm_Parse_Table_Free (Parse_Table : in out Xm_Parse_Table) is
--    can not use XmParseTableFree because it free array itself, but in
--    current Xm_Parse_Table definition this can do Ada
--	procedure XmParseTableFree
--	  (Parse_Table : in System.Address;
--	Parse_Count : in Cardinal);
--	pragma Import (C, XmParseTableFree, "XmParseTableFree");
   begin
--	if Parse_Table'Length > 0 then
--	   XmParseTableFree (Parse_Table'Address, Cardinal (Parse_Table'Length));
--	end if;
      for I in Parse_Table'Range loop
	 Xm_Parse_Mapping_Free (Parse_Table (I));
      end loop;
   end Xm_Parse_Table_Free;

-- EndMotif2.0 Motif2.1

   -- -------------------------------------------------------------------------
   --
   --  XmFontListFree
   --
   procedure Xm_Font_List_Free (Fontlist : in out Xm_Font_List) is
      procedure XmFontListFree (Fontlist : in Xm_Font_List);
      pragma Import (C, XmFontListFree, "XmFontListFree");
   begin
      XmFontListFree (Fontlist);
      Fontlist := Null_Font_List;
   end Xm_Font_List_Free;
   pragma Inline (Xm_Font_List_Free);


-- UseMotif2.0 Motif2.1
-- ----------------------------------------------------------------------------
--
--                            X M _ S T R I N G
--
--  Interface to the string related routines, using the Motif 2.0 scheme
--


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Generate
   --
   function XmStringGenerate
     (Text           : in System.Address;
      Font_Or_Locale : in Xm_String_Tag;
      Text_Type      : in Xm_Text_Type;
      Rendition_Tag  : in Xm_String_Tag)
      return Xm_String;
   pragma Import (C, XmStringGenerate, "XmStringGenerate");

   function Xm_String_Generate
     (Text           : in String;
      Font_Or_Locale : in Xm_String_Tag;
      Rendition_Tag  : in Xm_String_Tag)
      return Xm_String is

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringGenerate (Text_String'Address,
                               Font_Or_Locale,
                               Charset_Text,
                               Rendition_Tag);
   end Xm_String_Generate;
   pragma Inline (Xm_String_Generate);


   function Xm_String_Generate
     (Text           : in Wide_String;
      Font_Or_Locale : in Xm_String_Tag;
      Rendition_Tag  : in Xm_String_Tag)
      return Xm_String is
      W_String   : constant Interfaces.C.Wchar_Array
                 := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringGenerate (W_String'Address,
                               Font_Or_Locale,
                               Widechar_Text,
                               Rendition_Tag);
   end Xm_String_Generate;
   pragma Inline (Xm_String_Generate);


--   function Xm_String_Generate
--     (Text           : in Multibyte_String;
--      Font_Or_Locale : in Xm_String_Tag;
--      Rendition_Tag  : in Xm_String_Tag)
--      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Component_Create
   --
   --  it stands here because I expect many changes
   --

   function Xm_String_Component_Create
     (Tag    : in Xm_String_Component_Type;
      Length : in Interfaces.C.unsigned;
      Value  : in Xt_Pointer)
      return Xm_String is
      function XmStringComponentCreate (Tag    : in Xm_String_Component_Type;
                                        Length : in Interfaces.C.unsigned;
                                        Value  : in Xt_Pointer)
                                        return Xm_String;
      pragma Import (C, XmStringComponentCreate, "XmStringComponentCreate");
   begin
      return XmStringComponentCreate (Tag, Length, Value);
   end Xm_String_Component_Create;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Unparse
   --
   function Xm_String_Unparse
     (Str         : in Xm_String;
      Tag         : in Xm_String_Tag;
      Tag_Type    : in Xm_Text_Type;
      Parse_Table : in Xm_Parse_Table;
      Parse_Model : in Xm_Parse_Model) return String is
      function XmStringUnparse
        (Str         : in Xm_String;
         Tag         : in Xm_String_Tag;
         Tag_Type    : in Xm_Text_Type;
         Output_Type : in Xm_Text_Type;
         Parse_Table : in System.Address;
         Parse_Count : in X_Toolkit.Cardinal;
         Parse_Model : in Xm_Parse_Model)
	 return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XmStringUnparse, "XmStringUnparse");

      X_S : Interfaces.C.Strings.chars_ptr;
   begin
      X_S := XmStringUnparse (Str, Tag, Tag_Type,
	                      Charset_Text,
			      Parse_Table'Address,
			      X_Toolkit.Cardinal (Parse_Table'Length),
			      Parse_Model);
      declare
         Ret_Str : constant String := Interfaces.C.Strings.Value (X_S);
      begin
         XtFree (X_S);
         return Ret_Str;
      end;
   end Xm_String_Unparse;


   function Xm_String_Unparse
     (Str         : in Xm_String;
      Tag         : in Xm_String_Tag;
      Tag_Type    : in Xm_Text_Type;
      Parse_Table : in Xm_Parse_Table;
      Parse_Model : in Xm_Parse_Model) return Wide_String is
      function XmStringUnparse
        (Str         : in Xm_String;
         Tag         : in Xm_String_Tag;
         Tag_Type    : in Xm_Text_Type;
         Output_Type : in Xm_Text_Type;
         Parse_Table : in System.Address;
         Parse_Count : in X_Toolkit.Cardinal;
         Parse_Model : in Xm_Parse_Model)
	 return Interfaces.C.Wstrings.wchars_ptr;
      pragma Import (C, XmStringUnparse, "XmStringUnparse");

      Wide_S : Interfaces.C.Wstrings.wchars_ptr;
   begin
      Wide_S := XmStringUnparse (Str, Tag, Tag_Type,
	                         Multibyte_Text,
			         Parse_Table'Address,
			         X_Toolkit.Cardinal (Parse_Table'Length),
			         Parse_Model);
      declare
         Ret_Str : constant Wide_String := Interfaces.C.Wstrings.Value (Wide_S);
      begin
         XtFree (Wide_S);
         return Ret_Str;
      end;
   end Xm_String_Unparse;


-- EndMotif2.0 Motif2.1

-- ----------------------------------------------------------------------------
--
--                            X M _ S T R I N G
--
--  Interface to the string related routines, using the Motif 1.2 scheme
--


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create
   --
   function Xm_String_Create
     (Text    : in String;
      Charset : in Xm_String_Char_Set := Xm_String_Default_Charset)
      return Xm_String is
      function XmStringCreate (Text    : in System.Address;
                               Charset : in Xm_String_Char_Set) return Xm_String;
      pragma Import (C, XmStringCreate, "XmStringCreate");

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringCreate (Text_String'Address, Charset);
   end Xm_String_Create;
   pragma Inline (Xm_String_Create);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_Simple
   --
   function Xm_String_Create_Simple (Text : in String)
      return Xm_String is
      function XmStringCreateSimple (Text : in System.Address)
         return Xm_String;
      pragma Import (C, XmStringCreateSimple, "XmStringCreateSimple");

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringCreateSimple (Text_String'Address);
   end Xm_String_Create_Simple;
   pragma Inline (Xm_String_Create_Simple);


   -- -------------------------------------------------------------------------
   --
   --  XmStringSegmentCreate
   --
   function Xm_String_Segment_Create
     (Text      : in String;
      Charset   : in Xm_String_Char_Set  := Xm_String_Default_Charset;
      Direction : in Xm_String_Direction := Left_To_Right;
      Separator : in Boolean             := False)
      return Xm_String is

      function XmStringSegmentCreate (Text      : in System.Address;
                                      Charset   : in Xm_String_Char_Set;
                                      Direction : in Xm_String_Direction;
                                      Separator : in Xt_Boolean)
         return Xm_String;
      pragma Import (C, XmStringSegmentCreate, "XmStringSegmentCreate");

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringSegmentCreate (Text_String'Address,
                                    Charset,
                                    Direction,
                                    To_Xt_Boolean (Separator));
   end Xm_String_Segment_Create;
   pragma Inline (Xm_String_Segment_Create);



   -- -------------------------------------------------------------------------
   --
   --  XmStringCreateLtoR
   --
   function Xm_String_Create_L_To_R
     (Text    : in String;
      Charset : in Xm_String_Char_Set := Xm_String_Default_Charset)
      return Xm_String is
      function XmStringCreateLtoR (Text : in System.Address;
                                   Tag  : in Xm_String_Char_Set)
         return Xm_String;
      pragma Import (C, XmStringCreateLtoR, "XmStringCreateLtoR");

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringCreateLtoR (Text_String'Address, Charset);
   end Xm_String_Create_L_To_R;
   pragma Inline (Xm_String_Create_L_To_R);


   -- -------------------------------------------------------------------------
   --
   --  XmStringGetNextComponent
   --
   procedure Xm_String_Get_Next_Component
     (Context        : in  Xm_String_Context;
      Component      : out Xm_String_Component_Type;
      Text           : out Unbounded_String;
      Tag            : out Xm_String_Tag;
      Direction      : out Xm_String_Direction;
      Unknown_Tag    : out Xm_String_Component_Type;
      Unknown_Length : out System.Address;      -- unsigned short *unknown_lengt
      Unknown_Value  : out System.Address) is   -- unsigned char **unknown_value
      function XmStringGetNextComponent
        (Context        : in Xm_String_Context;
         Text           : in System.Address;
         Tag            : in System.Address;
         Direction      : in System.Address;
         Unknown_Tag    : in System.Address;
         Unknown_Length : in System.Address;
         Unknown_Value  : in System.Address) return Xm_String_Component_Type;
      pragma Import (C, XmStringGetNextComponent, "XmStringGetNextComponent");

      Ret_Str : Interfaces.C.Strings.Chars_Ptr;
   begin
      Component := XmStringGetNextComponent (Context, Ret_Str'Address,
                                             Tag'Address,
                                             Direction'Address,
                                             Unknown_Tag'Address,
                                             Unknown_Length'Address,
                                             Unknown_Value'Address);
      if Interfaces.C.Strings."=" (Ret_Str, Interfaces.C.Strings.Null_Ptr) then
         Text := Null_Unbounded_String;
      else
         Text := To_Unbounded_String (Interfaces.C.Strings.Value (Ret_Str));
         XtFree (Ret_Str);
      end if;
   end Xm_String_Get_Next_Component;



   -- -------------------------------------------------------------------------
   --
   --  XmStringGetNextSegment
   --
   procedure Xm_String_Get_Next_Segment
      (Context    : in  Xm_String_Context;
       Text       : out Unbounded_String;
       Charset    : out Xm_String_Char_Set;
       Direction  : out Xm_String_Direction;
       Separator  : out Boolean) is
      function XmStringGetNextSegment
        (Context   :  in Xm_String_Context;
         Text      :  in System.Address;
         Charset   :  in System.Address;
         Direction :  in System.Address;
         Separator :  in System.Address)
         return Xt_Boolean;

      pragma Import (C, XmStringGetNextSegment, "XmStringGetNextSegment");

      Ret_Text  : Interfaces.C.Strings.Chars_Ptr;
      Sep_There : Xt_Boolean;
   begin
      if XmStringGetNextSegment (Context,
                                 Ret_Text'Address,
                                 Charset'Address,
                                 Direction'Address,
                                 Sep_There'Address) = Xt_Boolean'(False) then
         raise Xm_String_Error;
      else
         Separator := To_Boolean (Sep_There);
         if Interfaces.C.Strings."=" (Ret_Text, Interfaces.C.Strings.Null_Ptr) then
            Text := Null_Unbounded_String;
         else
            Text := To_Unbounded_String (Interfaces.C.Strings.Value (Ret_Text));
         end if;
         return;
      end if;
   end Xm_String_Get_Next_Segment;



-- ----------------------------------------------------------------------------
--
--                            X M _ S T R I N G
--
--  Part of the string related routines, common to the Motif 2.0 and 1.2 scheme
--

   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_Localized
   --
   function Xm_String_Create_Localized (Text : in String)
      return Xm_String is
      function XmStringCreateLocalized (Text : in System.Address)
         return Xm_String;
      pragma Import (C, XmStringCreateLocalized, "XmStringCreateLocalized");

      Text_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Text, Append_Nul => True);
   begin
      return XmStringCreateLocalized (Text_String'Address);
   end Xm_String_Create_Localized;
   pragma Inline (Xm_String_Create_Localized);


   -- -------------------------------------------------------------------------
   --
   --  XmStringFree
   --
   procedure Xm_String_Free (String : in out Xm_String) is
      procedure XmStringFree (String : in Xm_String);
      pragma Import (C, XmStringFree, "XmStringFree");
   begin
      XmStringFree (String);
      String := Null_Xm_String;
   end Xm_String_Free;
   pragma Inline (Xm_String_Free);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Compare
   --
   function Xm_String_Compare
     (A : in Xm_String;
      B : in Xm_String)
      return Boolean is
      function XmStringCompare (A : in Xm_String;
                                B : in Xm_String) return Xt_Boolean;
      pragma Import (C, XmStringCompare, "XmStringCompare");
   begin
      return XmStringCompare (A, B) = Xt_Boolean'(True);
   end Xm_String_Compare;
   pragma Inline (Xm_String_Compare);


   -- -------------------------------------------------------------------------
   --
   --  String Context
   --
   procedure Xm_String_Init_Context
     (Context : out Xm_String_Context;
      Str     : in  Xm_String) is
     function XmStringInitContext (Context : in System.Address;
                                   Str     : in Xm_String)
        return Xt_Boolean;
     pragma Import (C, XmStringInitContext, "XmStringInitContext");
   begin
      if XmStringInitContext (Context'Address, Str) = Xt_Boolean'(False) then
         raise Xm_String_Error;
      end if;
   end Xm_String_Init_Context;


   -- -------------------------------------------------------------------------
   --
   --  XmStringContextFree
   --
   procedure Xm_String_Free_Context (Context : in out Xm_String_Context) is
      procedure XmStringFreeContext (Context : in Xm_String_Context);
      pragma Import (C, XmStringFreeContext, "XmStringFreeContext");
   begin
      XmStringFreeContext (Context);
      Context := Null_String_Context;
   end Xm_String_Free_Context;
   pragma Inline (Xm_String_Free_Context);


   -- -------------------------------------------------------------------------
   --
   -- Xm_String_Empty
   --
   function Xm_String_Empty (Str : in Xm_String) return Boolean is
      function XmStringEmpty (Str : in Xm_String) return Xt_Boolean;
      pragma Import (C, XmStringEmpty,"XmStringEmpty");
   begin
      return XmStringEmpty (Str) = Xt_Boolean'(True);
   end Xm_String_Empty;
      

-- UseMotif2.0 Motif2.1
   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Is_Void
   --
   function Xm_String_Is_Void (Str : in Xm_String) return Boolean is
      function XmStringIsVoid (Str : in Xm_String) return Xt_Boolean;
      pragma Import (C, XmStringIsVoid,"XmStringIsVoid");
   begin
      return XmStringIsVoid (Str) = Xt_Boolean'(True);
   end Xm_String_Is_Void;
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Has_Substring
   --
   function Xm_String_Has_Substring
     (Str       : in Xm_String; 
      Substring : in Xm_String) 
      return Boolean is
      function XmStringHasSubstring (Str       : in Xm_String;
                                     Substring : in Xm_String) 
               return Xt_Boolean;
      pragma Import (C, XmStringHasSubstring, "XmStringHasSubstring");
   begin
      return XmStringHasSubstring (Str, Substring) = Xt_Boolean'(True);
   end Xm_String_Has_Substring;



   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Get_L_To_R
   --
   function Xm_String_Get_L_To_R
     (Str        : in  Xm_String;
      Charset    : in  Xm_String_Char_Set)
      return String is
      function XmStringGetLtoR (Str        : in Xm_String;
                                Charset    : in Xm_String_Char_Set;
                                Text       : in System.Address)
                                return Xt_Boolean;
      pragma Import (C, XmStringGetLtoR,"XmStringGetLtoR");

      Ret_Str : Interfaces.C.Strings.chars_ptr;
   begin
      if XmStringGetLtoR (Str, Charset, Ret_Str'Address) = Xt_Boolean'(True) then
         declare
            Back_Str : constant String := Interfaces.C.Strings.Value (Ret_Str);
         begin
            XtFree (Ret_Str);
            return Back_Str;
         end;
      else
         raise Xm_String_Error;
      end if;
   end Xm_String_Get_L_To_R;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Draw
   --
   procedure Xm_String_Draw
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction;
      Clip         : in X_Lib.Rectangle) is
      procedure XmStringDraw
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address);
      pragma Import (C, XmStringDraw, "XmStringDraw");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDraw (Display, W, Render_Table, Str, GC, X, Y, Width,
                    Align, Lay_Dir, Clip'Address);
-- NotMotif2.0 Motif2.1
--!       XmStringDraw (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                     Align, Lay_Dir, Clip'Address);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw;


   procedure Xm_String_Draw
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction) is
      procedure XmStringDraw
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address);
      pragma Import (C, XmStringDraw, "XmStringDraw");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDraw (Display, W, Render_Table, Str, GC, X, Y, Width,
                    Align, Lay_Dir, System.Null_Address);
-- NotMotif2.0 Motif2.1
--!       XmStringDraw (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                     Align, Lay_Dir, System.Null_Address);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw;



   procedure Xm_String_Draw_Image
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction;
      Clip         : in X_Lib.Rectangle) is
      procedure XmStringDrawImage
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address);
      pragma Import (C, XmStringDrawImage, "XmStringDrawImage");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDrawImage (Display, W, Render_Table, Str, GC, X, Y, Width,
                         Align, Lay_Dir, Clip'Address);
-- NotMotif2.0 Motif2.1
--!       XmStringDrawImage (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                          Align, Lay_Dir, Clip'Address);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw_Image;


   procedure Xm_String_Draw_Image
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction) is
      procedure XmStringDrawImage
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address);
      pragma Import (C, XmStringDrawImage, "XmStringDrawImage");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDrawImage (Display, W, Render_Table, Str, GC, X, Y, Width,
                         Align, Lay_Dir, System.Null_Address);
-- NotMotif2.0 Motif2.1
--!       XmStringDrawImage (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                          Align, Lay_Dir, System.Null_Address);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw_Image;



   procedure Xm_String_Draw_Underline
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction;
      Clip         : in X_Lib.Rectangle;
      Underline    : in Xm_String) is
      procedure XmStringDrawUnderline
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address;
         Underline    : in Xm_String);
      pragma Import (C, XmStringDrawUnderline, "XmStringDrawUnderline");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDrawUnderline (Display, W, Render_Table, Str, GC, X, Y, Width,
                             Align, Lay_Dir, Clip'Address, Underline);
-- NotMotif2.0 Motif2.1
--!       XmStringDrawUnderline (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                              Align, Lay_Dir, Clip'Address, Underline);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw_Underline;


   procedure Xm_String_Draw_Underline
     (Display      : in X_Lib.Display_Pointer;
      W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
      Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!       Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str          : in Xm_String;
      GC           : in X_Lib.GC_Pointer;
      X,
      Y            : in X_Lib.Position;
      Width        : in X_Lib.Dimension;
      Align        : in Alignment;
      Lay_Dir      : in Layout_Direction;
      Underline    : in Xm_String) is
      procedure XmStringDrawUnderline
        (Display      : in X_Lib.Display_Pointer;
         W            : in X_Lib.Window_ID;
-- UseMotif2.0 Motif2.1
         Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!          Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
         Str          : in Xm_String;
         GC           : in X_Lib.GC_Pointer;
         X,
         Y            : in X_Lib.Position;
         Width        : in X_Lib.Dimension;
         Align        : in Alignment;
         Lay_Dir      : in Layout_Direction;
         Clip         : in System.Address;
         Underline    : in Xm_String);
      pragma Import (C, XmStringDrawUnderline, "XmStringDrawUnderline");
   begin
-- UseMotif2.0 Motif2.1
      XmStringDrawUnderline (Display, W, Render_Table, Str, GC, X, Y, Width,
                             Align, Lay_Dir, System.Null_Address, Underline);
-- NotMotif2.0 Motif2.1
--!       XmStringDrawUnderline (Display, W, Fontlist, Str, GC, X, Y, Width,
--!                              Align, Lay_Dir, System.Null_Address, Underline);
-- EndMotif2.0 Motif2.1
   end Xm_String_Draw_Underline;




-- ----------------------------------------------------------------------------
--
--                  C O M P O U N D _ T E X T
--
--  routines related to compound text
--

   type Compound_Text_Element_Access_Type is access Compound_Text_Element_Type;

   function To_Uns is
      new Ada.Unchecked_Conversion (System.Address, Interfaces.C.unsigned_long);
   function To_Uns is
      new Ada.Unchecked_Conversion (Xt_Pointer, Interfaces.C.unsigned_long);
   function To_Acc is
      new Ada.Unchecked_Conversion (Interfaces.C.unsigned_long, Compound_Text_Element_Access_Type);


   function Find_End_Address (Adr : in Interfaces.C.unsigned_long)
      return Interfaces.C.unsigned_long is
      My_Adr : Interfaces.C.unsigned_long := Adr;
   begin
      loop
	 exit when To_Acc (My_Adr).all = Compound_Text_Element_Type'Val (0);
         My_Adr := My_Adr + 1;
      end loop;
      return My_Adr;
   end Find_End_Address;


   function To_Compound_Text_Type (Adr : in Xt_Pointer) return Compound_Text_Type is
      Uns_Adr, Mom_Adr : Interfaces.C.unsigned_long;
   begin
      if X_Lib."=" (Adr, Null_Xt_Pointer) then
         return Null_Compound_Text_Type;
      else
         Uns_Adr := To_Uns (Adr);
	 Mom_Adr := Find_End_Address (Uns_Adr);
	 if Mom_Adr /= Uns_Adr then
	    declare
	       subtype This_Compound_Text_Type is Compound_Text_Type (1 .. Natural (Mom_Adr-Uns_Adr));
	       type Acc_This_CT_Type is access This_Compound_Text_Type;
               function To_Acc is
                  new Ada.Unchecked_Conversion (Interfaces.C.unsigned_long, Acc_This_CT_Type);
	       CT_Acc : Acc_This_CT_Type := To_Acc (Uns_Adr);
	       Return_CT : Compound_Text_Type (1 .. Natural (Mom_Adr-Uns_Adr));
	    begin
	       Return_CT := CT_Acc.all;
	       return Return_CT;
	    end;
	 else
            return Null_Compound_Text_Type;
	 end if;
      end if;
   end To_Compound_Text_Type;


   function Xm_Cvt_Xm_String_To_CT (Str : in Xm_String) return Compound_Text_Type is
      function XmCvtXmStringToCT (Str : in Xm_String) return System.Address;
      pragma Import (C, XmCvtXmStringToCT, "XmCvtXmStringToCT");

      use System;

      Adr              : System.Address;
      Uns_Adr, Mom_Adr : Interfaces.C.unsigned_long;
      Length           : Natural := 0;
   begin
      Adr := XmCvtXmStringToCT (Str);
      if Adr = System.Null_Address then
         return Null_Compound_Text_Type;
      else
         Uns_Adr := To_Uns (Adr);
	 Mom_Adr := Find_End_Address (Uns_Adr);
	 if Mom_Adr /= Uns_Adr then
	    declare
	       subtype This_Compound_Text_Type is Compound_Text_Type (1 .. Natural (Mom_Adr-Uns_Adr));
	       type Acc_This_CT_Type is access This_Compound_Text_Type;
               function To_Acc is
                  new Ada.Unchecked_Conversion (Interfaces.C.unsigned_long, Acc_This_CT_Type);
	       CT_Acc : Acc_This_CT_Type := To_Acc (Uns_Adr);
	       Return_CT : Compound_Text_Type (1 .. Natural (Mom_Adr-Uns_Adr));
	    begin
	       Return_CT := CT_Acc.all;
	       XtFree (Adr);
	       return Return_CT;
	    end;
	 else
            return Null_Compound_Text_Type;
	 end if;
      end if;
   end Xm_Cvt_Xm_String_To_CT;


   function Xm_Cvt_CT_To_Xm_String (Str : in Compound_Text_Type) return Xm_String is
      function XmCvtCTToXmString (Str : in System.Address) return Xm_String;
      pragma Import (C, XmCvtCTToXmString, "XmCvtCTToXmString");
      CT : Compound_Text_Type (1 .. Str'Length + 1);
   begin
      CT (1 .. Str'Length) := Str;
      CT (Str'Length + 1)  := Compound_Text_Element_Type'Val (0);
      return XmCvtCTToXmString (CT'Address);
   end Xm_Cvt_CT_To_Xm_String;


-- ----------------------------------------------------------------------------
--
--                            M I S C
--

   function Xm_Is_Traversable (W : in Widget) return Boolean is
      function XmIsTraversable (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmIsTraversable, "XmIsTraversable");
   begin
      return XmIsTraversable (W) = Xt_Boolean'(True);
   end Xm_Is_Traversable;
   pragma Inline (Xm_Is_Traversable);


   function Xm_Process_Traversal
     (W   : in Widget;
      Dir : in Xm_Traversal_Direction)
      return Boolean is
      function XmProcessTraversal
        (W   : in Widget;
         Dir : in Xm_Traversal_Direction)
         return Xt_Boolean;
      pragma Import (C, XmProcessTraversal, "XmProcessTraversal");
   begin
      return XmProcessTraversal (W, Dir) = Xt_Boolean'(True);
   end Xm_Process_Traversal;
   pragma Inline (Xm_Process_Traversal);


   function To_Integer is
      new Ada.Unchecked_Conversion (Orientation_Type, Interfaces.C.unsigned_char);

-- UseMotif2.0 Motif2.1

   --  if unsuccessful, raise Xm_Error_Conversion_Failed
   -- 
   function Xm_Convert_String_To_Units
     (Scr         : in X_lib.Screen_Pointer;
      Spec        : in String;
      Orientation : in Orientation_Type;
      To_Unit     : in Unit_Type)
      return Integer is
      function XmConvertStringToUnits
        (Scr         : in X_lib.Screen_Pointer;
	 Spec        : in System.Address;
	 Orientation : in Interfaces.C.Int;
	 To_Unit     : in Interfaces.C.Int;
	 Error_Ret   : in System.Address)
	 return Interfaces.C.Int;
      pragma Import (C, XmConvertStringToUnits, "XmConvertStringToUnits");

      use Interfaces.C;

-- Use64Bit
--!       Error_Return : Interfaces.C.long := 0;
-- Not64Bit
      Error_Return : Interfaces.C.unsigned_char := 0;
-- End64Bit
      Value_Return : Interfaces.C.Int;

      Spec_String   : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Spec, Append_Nul => True);
   begin
      Value_Return := XmConvertStringToUnits (Scr, Spec_String'Address,
                                              Interfaces.C.Int (To_Integer (Orientation)),
					      Interfaces.C.Int (To_Integer (To_Unit)),
					      Error_Return'Address);
      if Error_Return /= 0 then
         raise Xm_Error_Conversion_Failed;
      else
         return Integer (Value_Return);
      end if;
   end Xm_Convert_String_To_Units;

-- EndMotif2.0 Motif2.1


--
--
--

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Address (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_String) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String_Table) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value (Value'First)'Address);
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Font_List) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => System.Address (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Font_List) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



   function To_Int is
      new Ada.Unchecked_Conversion (Xm_String_Direction, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String_Direction) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (To_Int (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_String_Direction) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xm_String_Char_Set) is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => To_Address (X_Strings.X_String (Value)));
   end Append_Set;


   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xm_String_Char_Set) is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => Value'Address);
   end Append_Get;


-- UseMotif2.0 Motif2.1
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Render_Table) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => System.Address (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Render_Table) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);
-- EndMotif2.0 Motif2.1

 
   function To_Int is
      new Ada.Unchecked_Conversion (Font_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Font_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (To_Int (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Font_Type) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Int is
      new Ada.Unchecked_Conversion (Interfaces.C.Strings.Chars_Ptr, Integer);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     String) is
      -- Null_Term_String : constant String := Value & Character'Val (0);
      Null_Term_String : Ada.Strings.Unbounded.String_Access := new String'(Value & Character'Val (0));
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Null_Term_String.all'Address);
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Unit_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Interfaces.C.long (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Unit_Type) is
   begin
      Append_Set (List =>  List,
                  Name =>  Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Orientation_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Orientation_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- Use64Bit
--!    function To_Integer is
--!       new Ada.Unchecked_Conversion (Alignment, Unsigned);
-- Not64Bit
   function To_Integer is
      new Ada.Unchecked_Conversion (Alignment, Interfaces.C.unsigned_char);
-- End64Bit

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Alignment) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Alignment) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



   type Layout_Direction_Int is range 0 .. 2**Layout_Direction'Size - 1;
   for Layout_Direction_Int'Size use Layout_Direction'Size;
   function To_Integer is new Ada.Unchecked_Conversion (Layout_Direction, Layout_Direction_Int);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Layout_Direction) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Layout_Direction) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- ----------------------------------------------------------------------------
--
--  Type conversion
--
   function To_Address (Xm_Str : in Xm_String) return System.Address is
   begin
       return (System.Address (Xm_Str));
   end To_Address;
   pragma Inline (To_Address);


end Xm_Widgets;
