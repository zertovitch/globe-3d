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
--          22 Jun 2001 Vadim Godunko: inclusion of X_Wide_Strings,
--                                     Color calculations, Xm_Get_Destination
--                                     and Append_Set/Append_Get for Xm_String_Char_Set
--          05 Aug 2001 H.-F. Vogt: remove Xm_Add_To_Post_From_List (now in
--                                  Xm_Widgets.Manager.Row_Column where it
--                                  really belongs)
--          17 Nov 2001 Vadim Godunko: add types Xm_String_Tag_Array and
--                                     Xm_Tab_List
--                                     add tab list related routines
--                                     Xm_String_Table_Propose_Tablist,
--                                     Xm_Tab_List_Free
--                                     change Xm_Rendition_Free,
--                                     Xm_Render_Table_Free,
--                                     Xm_Parse_Mapping_Free,
--                                     Xm_Parse_Table_Free,
--                                     Xm_String_Free, Xm_String_Free_Context
--                                     argument access
--                                     add Xm_Render_Table_Copy,
--                                     Xm_Render_Table_Get_Rendition
--          13 Jan 2002 H.-F. Vogt: make Unit_Type size Short_Short_Unsigned
--                                  add tab related routines
--                                  Xm_Tab_Create, Xm_Tab_Free,
--                                  Xm_Tab_Get_Values, Xm_Tab_Set_Value
--                                  add tab list related routines
--                                  Xm_Tab_List_Tab_Copy, Xm_Tab_List_Get_Tab,
--                                  Xm_Tab_List_Insert_Tabs,
--                                  Xm_Tab_List_Remove_Tabs,
--                                  Xm_Tab_List_Replace_Positions
--          20 Jan 2002 H.-F. Vogt: add comment to Xm_Add_Tab_Group
--                                  add Xm_Get_Scaled_Pixmap
--                                  add type Orientation_Type with Append_Set
--                                  and Append_Get routines (replaces
--                                  Orientation_Type currently defined in
--                                    Xm_Widgets.Manager.Paned_Window,
--                                    Xm_Widgets.Manager.Row_Column,
--                                    Xm_Widgets.Manager.Scale,
--                                    Xm_Widgets.Primitive.Scroll_Bar,
--                                    Xm_Widgets.Primitive.Separator.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded,
     Interfaces.C,
     System,
     X_Strings,
     X_Wide_Strings,
     X_Lib,
     X_Toolkit;
use  Ada.Strings.Unbounded,
     X_Toolkit;
package Xm_Widgets is

-- ----------------------------------------------------------------------------
--
--     T Y P E   D E F I N I T I O N S
--

   --
   -- String Component
   --
   type Xm_String_Component_Type is new Interfaces.C.unsigned_char;

   Component_Unknown		  : constant Xm_String_Component_Type := 0;
   Component_Charset		  : constant Xm_String_Component_Type := 1;
   Component_Text		  : constant Xm_String_Component_Type := 2;
   Component_Direction  	  : constant Xm_String_Component_Type := 3;
   Component_Separator  	  : constant Xm_String_Component_Type := 4;
   Component_Locale_Text	  : constant Xm_String_Component_Type := 5;
-- UseMotif2.0 Motif2.1
   Component_Locale		  : constant Xm_String_Component_Type := 6;
   Component_Widechar_Text	  : constant Xm_String_Component_Type := 7;
   Component_Layout_Push	  : constant Xm_String_Component_Type := 8;
   Component_Layout_Pop 	  : constant Xm_String_Component_Type := 9;
   Component_Rendition_Begin	  : constant Xm_String_Component_Type := 10;
   Component_Rendition_End	  : constant Xm_String_Component_Type := 11;
   Component_Tab		  : constant Xm_String_Component_Type := 12;
-- EndMotif2.0 Motif2.1
   -- Elements 13 to 126 are reserved
   Component_End		  : constant Xm_String_Component_Type := 126;
   Component_User_Begin 	  : constant Xm_String_Component_Type := 128;
   Component_User_End		  : constant Xm_String_Component_Type := 255;
   
   -- aliases
   Component_Fontlist_Element_Tag : constant Xm_String_Component_Type := Component_Charset;
   Component_Tag		  : constant Xm_String_Component_Type := Component_Charset;


   --
   -- String Direction
   --
   type Xm_String_Direction is (Left_To_Right,
                                Right_To_Left,
                                Unset,
                                Default);
   for Xm_String_Direction use (Left_To_Right   => 0,
                                Right_To_Left   => 1,
                                Unset           => 3,
                                Default         => 255);
   for Xm_String_Direction'Size use Interfaces.C.unsigned_char'Size;




   --
   -- Compound String
   --
   type Xm_String is private;
   Null_Xm_String : constant Xm_String;

   --
   -- Compound String Table
   --
   type Xm_String_Table is array (Natural range <>) of Xm_String;

   --
   -- String Context
   --
   type Xm_String_Context  is private;
   Null_String_Context : constant Xm_String_Context;

   --
   -- Font List
   --
   type Xm_Font_List       is private;
   Null_Font_List : constant Xm_Font_List;

   --
   -- Char Set
   --
   subtype Xm_String_Char_Set is X_Strings.X_String;
   Xm_String_Default_Charset : constant Xm_String_Char_Set
      := X_Strings.To_X_String ("" & Character'Val (0));
   Xm_String_ISO8859_1       : constant Xm_String_Char_Set
      := X_Strings.To_X_String ("ISO8859-1");

   --
   -- String Tag
   --
   subtype Xm_String_Tag is X_Strings.X_String;
   Null_String_Tag : constant Xm_String_Tag
      := Xm_String_Tag (X_Strings.Null_X_String);

   Motif_Default_Locale : constant Xm_String_Tag
      := X_Strings.To_X_String ("_MOTIF_DEFAULT_LOCALE");


   type Xm_String_Tag_Array is array (Natural range <>) of Xm_String_Tag;
   Null_Xm_String_Tag_Array : constant Xm_String_Tag_Array (1 .. 0) :=
     (others => Null_String_Tag);

   --
   -- Text Type
   --
   type Xm_Text_Type is (Charset_Text,
                         Multibyte_Text,
                         Widechar_Text,
                         No_Text);


   --
   -- Text Position
   --
   type Xm_Text_Position is new Interfaces.C.long;

   --
   -- Text Direction
   --
   type Xm_Text_Direction is (Forward, Backward);


   --
   -- Text Scan Type
   --
   type Xm_Text_Scan_Type is (Select_Position,
                              Select_Whitespace,
                              Select_Word,
                              Select_Line,
                              Select_All,
                              Select_Paragraph,
                              Select_Out_Line);


   --
   -- Text Format
   --
   subtype Xm_Text_Format is X_Lib.Atom;

   type Xm_Text_Block_Rec is record
      Ptr    : X_Strings.X_String;
      Length : Integer;
      Format : Xm_Text_Format;
   end record;
   pragma Convention (C, Xm_Text_Block_Rec);

   type Xm_Text_Block is access all Xm_Text_Block_Rec;


   type Xm_Text_Block_Rec_Wcs is record
      Wcs_Ptr : X_Wide_Strings.X_Wide_String;
      Length  : Integer;
   end record;
   pragma Convention (C, Xm_Text_Block_Rec_Wcs);

   type Xm_Text_Block_Wcs is access all Xm_Text_Block_Rec_Wcs;


   --
   -- Highlight Mode
   --
   type Xm_Highlight_Mode is (Highlight_Normal,
                              Highlight_Selected,
                              Highlight_Secondary_Selected,
                              See_Detail); 
   for Xm_Highlight_Mode use (Highlight_Normal => 0,
                              Highlight_Selected => 1,
                              Highlight_Secondary_Selected => 2,
                              See_Detail => 3); 
   for Xm_Highlight_Mode'Size use Interfaces.C.unsigned_char'Size;




   --
   -- Font Type
   --
   type Font_Type is (Font_Is_Font, Font_is_Fontset);



   --
   -- Include Status
   --
   type Xm_Include_Status is (Insert, Do_Terminate, Invoke);


   --
   -- Offset Model
   --
   type Xm_Offset_Model is (Absolute, Relative);


   --
   -- Merge Mode
   --
-- UseMotif2.1
   type Xm_Merge_Mode is (Skip,      Merge_Replace,     Merge_Old,
                          Merge_New, Duplicate);
   for Xm_Merge_Mode use (Skip => 0,      Merge_Replace => 1, Merge_Old => 2,
                          Merge_New => 3, Duplicate => 4);
-- NotMotif2.1
--!    type Xm_Merge_Mode is (Skip,      Replace,     Merge_Old,
--!                           Merge_New, Duplicate);
--!    for Xm_Merge_Mode use (Skip => 0,      Replace => 1,     Merge_Old => 2,
--!                           Merge_New => 3, Duplicate => 4);
-- EndMotif2.1
   for Xm_Merge_Mode'Size use Interfaces.C.unsigned_char'Size;
-- UseMotif2.1
   -- for compatibility with M*tif versions < 2.1
   --
   Replace : constant Xm_Merge_Mode := Merge_Replace;
-- EndMotif2.1


   --
   -- Visibility
   --
   type Xm_Visibility is (Unobscured,
                          Partially_Obscured,
                          Fully_Obscured);

   --
   -- Traversal Direction
   --
   type Xm_Traversal_Direction is (Current,
                                   Next,
                                   Prev,
                                   Home,
                                   Next_Tab_Group,
                                   Prev_Tab_Group,
                                   Up,
                                   Down,
                                   Left,
                                   Right,
                                   Globally_Forward,
                                   Globally_Backward);


   --
   -- Callback Reason
   --
   type Callback_Reason is new Integer;
   
   Cr_None                    : constant Callback_Reason := 0;
   Cr_Help                    : constant Callback_Reason := 1;
   Cr_Value_Changed           : constant Callback_Reason := 2;
   Cr_Increment               : constant Callback_Reason := 3;
   Cr_Decrement               : constant Callback_Reason := 4;
   Cr_Page_Increment          : constant Callback_Reason := 5;
   Cr_Page_Decrement          : constant Callback_Reason := 6;
   Cr_To_Top                  : constant Callback_Reason := 7;
   Cr_To_Bottom               : constant Callback_Reason := 8;
   Cr_Drag                    : constant Callback_Reason := 9;
   Cr_Activate                : constant Callback_Reason := 10;
   Cr_Arm                     : constant Callback_Reason := 11;
   Cr_Disarm                  : constant Callback_Reason := 12;
   Cr_Map                     : constant Callback_Reason := 16;
   Cr_Unmap                   : constant Callback_Reason := 17;
   Cr_Focus                   : constant Callback_Reason := 18;
   Cr_Losing_Focus            : constant Callback_Reason := 19;
   Cr_Modifying_Text_Value    : constant Callback_Reason := 20;
   Cr_Moving_Insert_Cursor    : constant Callback_Reason := 21;
   Cr_Execute                 : constant Callback_Reason := 22;
   Cr_Single_Select           : constant Callback_Reason := 23;
   Cr_Multiple_Select         : constant Callback_Reason := 24;
   Cr_Extended_Select         : constant Callback_Reason := 25;
   Cr_Browse_Select           : constant Callback_Reason := 26;
   Cr_Default_Action          : constant Callback_Reason := 27;
   Cr_Clipboard_Data_Request  : constant Callback_Reason := 28;
   Cr_Clipboard_Data_Delete   : constant Callback_Reason := 29;
   Cr_Cascading               : constant Callback_Reason := 30;
   Cr_Ok                      : constant Callback_Reason := 31;
   Cr_Cancel                  : constant Callback_Reason := 32;
   Cr_Apply                   : constant Callback_Reason := 34;
   Cr_No_Match                : constant Callback_Reason := 35;
   Cr_Command_Entered         : constant Callback_Reason := 36;
   Cr_Command_Changed         : constant Callback_Reason := 37;
   Cr_Expose                  : constant Callback_Reason := 38;
   Cr_Resize                  : constant Callback_Reason := 39;
   Cr_Input                   : constant Callback_Reason := 40;
   Cr_Gain_Primary            : constant Callback_Reason := 41;
   Cr_Lose_Primary            : constant Callback_Reason := 42;
   Cr_Create                  : constant Callback_Reason := 43;
   Cr_Tear_Off_Activate       : constant Callback_Reason := 44;
   Cr_Tear_Off_Deactivate     : constant Callback_Reason := 45;
   Cr_Obscured_Traversal      : constant Callback_Reason := 46;
   Cr_Focus_Moved             : constant Callback_Reason := 47;
   Cr_Repost                  : constant Callback_Reason := 54;
   Cr_Collapsed               : constant Callback_Reason := 55;
   Cr_Expanded                : constant Callback_Reason := 56;
   Cr_Select                  : constant Callback_Reason := 57;
   Cr_Drag_Start              : constant Callback_Reason := 58;
   Cr_No_Font                 : constant Callback_Reason := 59;
   Cr_No_Rendition            : constant Callback_Reason := 60;
   Cr_Post                    : constant Callback_Reason := 61;
   Cr_Spin_Next               : constant Callback_Reason := 62;
   Cr_Spin_Prior              : constant Callback_Reason := 63;
   Cr_Spin_First              : constant Callback_Reason := 64;
   Cr_Spin_Last               : constant Callback_Reason := 65;
   Cr_Page_Scroller_Increment : constant Callback_Reason := 66;
   Cr_Page_Scroller_Decrement : constant Callback_Reason := 67;
   Cr_Major_Tab               : constant Callback_Reason := 68;
   Cr_Minor_Tab               : constant Callback_Reason := 69;
   Cr_Protocols               : constant Callback_Reason := 6666;


   --
   -- Callback Structs
   --

   type Xm_Any_Callback_Struct is record
      Reason : Callback_Reason;
      Event  : X_Lib.X_Event_Pointer;
   end record;
   pragma Convention (C, Xm_Any_Callback_Struct);

   type Xm_Any_Callback_Struct_Access is
      access all Xm_Any_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Any_Callback_Struct_Access;



   type Xm_Popup_Handler_Callback_Struct is record
      Reason        : Callback_Reason;
      Event         : X_Lib.X_Event_Pointer;
      Menu_To_Post  : Widget;
      Post_It       : Boolean;
      Target        : Widget;
   end record;
   pragma Convention (C, Xm_Popup_Handler_Callback_Struct);

   type Xm_Popup_Handler_Callback_Struct_Access is
      access all Xm_Popup_Handler_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Popup_Handler_Callback_Struct_Access;



   --
   -- String Ptr
   --
   type Xm_String_Ptr is access Xm_String; 


-- UseMotif2.0 Motif2.1
   type Unit_Type is (Pixels,           Centi_Millimeters,
                      Milli_Inches,     Centi_Points,
                      Centi_Font_Units, Inches,
                      Centimeters,      Millimeters,
                      Points,           Font_Units);
-- NotMotif2.0 Motif2.1
--!    type Unit_Type is (Pixels,           Centi_Millimeters,
--!                       Milli_Inches,     Centi_Points,
--!                       Centi_Font_Units);
-- EndMotif2.0 Motif2.1

   type Orientation_Type is (No_Orientation, Vertical, Horizontal);

   type Alignment is (Alignment_Beginning, Alignment_Center, Alignment_End);

   type Layout_Direction is (Right_To_Left_Top_To_Bottom,
                             Left_To_Right_Top_To_Bottom,
                             Right_To_Left_Bottom_To_Top,
                             Left_To_Right_Bottom_To_Top,
                             Top_To_Bottom_Right_To_Left,
                             Top_To_Bottom_Left_To_Right,
                             Bottom_To_Top_Right_To_Left,
                             Bottom_To_Top_Left_To_Right,
                             Top_To_Bottom,
                             Bottom_To_Top,
                             Right_To_Left,
                             Left_To_Right,
                             Default_Direction);

   --
   -- special resource values --- handle with care
   --
   Xm_Unspecified_Pixmap : constant X_Lib.Pixmap_ID := X_Lib.Pixmap_ID (2);
   Xm_Unspecified_Pixel  : constant X_Lib.Pixel := X_Lib."not" (X_Lib.Pixel (0));

   Xm_Unspecified_Position : constant X_Lib.Position := X_Lib.Position (-1);


-- ----------------------------------------------------------------------------
--
--                      P I X M A P  and   I M A G E
--
--  routines related to pixmap and image caching
--
   Xm_Cache_Error : exception;

   -- add the specified image to the image cache and return it
   function Xm_Install_Image
     (Image_Name  : in String)
      return X_Lib.X_Image_Pointer;

   -- remove an image from the image cache (doesn't free the image structure)
   --
   procedure Xm_Uninstall_Image
     (Image       : in X_Lib.X_Image_Pointer);

   -- get a pixmap, add it to the pixmap cache and return the pixmap
   -- if not found, return Xm_Unspecified_Pixmap
   -- if the Image_Name is given as relative path, it is searched in several
   -- (implementation dependent) directories
   --
   function Xm_Get_Pixmap
     (Screen      : in X_Lib.Screen_Pointer;
      Image_Name  : in String;
      Foreground  : in X_Lib.Pixel;
      Background  : in X_Lib.Pixel)
      return X_Lib.Pixmap_ID;

   function Xm_Get_Pixmap_By_Depth
     (Screen      : in X_Lib.Screen_Pointer;
      Image_Name  : in String;
      Foreground  : in X_Lib.Pixel;
      Background  : in X_Lib.Pixel;
      Depth       : in X_Lib.Color_Depth)
      return X_Lib.Pixmap_ID;

-- UseMotif2.1
   function Xm_Get_Scaled_Pixmap
     (Wid           : in Widget;
      Image_Name    : in String;
      Foreground    : in X_Lib.Pixel;
      Background    : in X_Lib.Pixel;
      Depth         : in X_Lib.Color_Depth;
      Scaling_Ratio : in Long_Float)
      return X_Lib.Pixmap_ID;
-- EndMotif2.1


   -- remove the given pixmap from the pixmap cache
   --
   procedure Xm_Destroy_Pixmap
     (Screen      : in X_Lib.Screen_Pointer;
      Pixmap      : in X_Lib.Pixmap_ID);


-- UseMotif2.0 Motif2.1
-- ----------------------------------------------------------------------------
--
--                                 T A B
--
--  tab related routines
--  tab specifies a tab stops to be used in the lay out of Xm_Strings
--

   type Xm_Tab is private;
   Null_Tab : constant Xm_Tab;

   type Xm_Tab_Array is array (Natural range <>) of Xm_Tab;


   function Xm_Tab_Create
     (Value        : in Float;
      Units        : in Unit_Type;
      Offset_Model : in Xm_Offset_Model;
      Align        : in Alignment;
      Dec_Point    : in String)    --  should be Multibyte_Char!
      return Xm_Tab;


   procedure Xm_Tab_Free (Tab : in out Xm_Tab);


   procedure Xm_Tab_Get_Values
     (Tab          : in     Xm_Tab;
      Value        :    out Float;
      Units        :    out Unit_Type;
      Offset_Model :    out Xm_Offset_Model;
      Align        :    out Alignment;
      Dec_Point    :    out String);   --  should be out Multibyte_Char


   procedure Xm_Tab_Set_Value
     (Tab          : in Xm_Tab;
      Value        : in Float);


-- ----------------------------------------------------------------------------
--
--                              T A B L I S T
--
--  tab list related routines
--  tab list specifies a list of tab stops to lay out Xm_Strings
--

   type Xm_Tab_List is private;
   Null_Tab_List : constant Xm_Tab_List;


   function Xm_Tab_List_Tab_Copy
     (Tab_List : in Xm_Tab_List;
      Offset   : in Integer;
      Count    : in Cardinal)
      return Xm_Tab_List;

   function Xm_Tab_List_Get_Tab
     (Tab_List : in Xm_Tab_List;
      Pos      : in Cardinal)
      return Xm_Tab;

   function Xm_Tab_List_Insert_Tabs
     (Tab_List : in Xm_Tab_List;
      Tabs     : in Xm_Tab_Array;
      Pos      : in Integer)
      return Xm_Tab_List;

   function Xm_Tab_List_Remove_Tabs
     (Tab_List : in Xm_Tab_List;
      Pos_List : in Cardinal_Array)
      return Xm_Tab_List;

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
      return Xm_Tab_List;

   function Xm_String_Table_Propose_Tablist
     (Strings	   : in Xm_String_Table;
      W 	   : in Widget;
      Pad_Value    : in Float;
      Offset_Model : in Xm_Offset_Model)
      return Xm_Tab_List;

   procedure Xm_Tab_List_Free (Tablist : in out Xm_Tab_List);


   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xm_Tab_List);

   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xm_Tab_List);
   pragma Convention (C, Append_Get);


-- ----------------------------------------------------------------------------
--
--                            R E N D I T I O N
--
--  rendition related routines
--

   --
   -- pseudo widget Xm_Rendition
   --
   type Xm_Rendition is private;
   Null_Rendition : constant Xm_Rendition;

   type Xm_Rendition_Array is array (Natural range <>) of Xm_Rendition;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Create
   --
   function Xm_Rendition_Create
     (W             : in Widget;
      Rendition_Tag : in Xm_String_Tag;
      Arglist       : in Arg_List)
      return Xm_Rendition;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Free
   --
   procedure Xm_Rendition_Free (Rendition : in out Xm_Rendition);


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Update
   --
   procedure Xm_Rendition_Update (Rendition : in Xm_Rendition;
                                  Args      : in Arg_List);


   -- -------------------------------------------------------------------------
   --
   --  Xm_Rendition_Retrieve
   --
   procedure Xm_Rendition_Retrieve (Rendition : in Xm_Rendition;
                                    Args      : in Arg_List);


   --
   -- resource strings and values of pseudo widget Xm_Rendition
   --
   Xm_N_Font                 : constant Xt_N_Resource_String;
   Xm_N_Font_Name            : constant Xt_N_Resource_String;
   Xm_N_Font_Type            : constant Xt_N_Resource_String;

   Xm_N_Load_Model           : constant Xt_N_Resource_String;

   type Font_Load_Model_Type is (Unspecified, Deferred, Immediate);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Font_Load_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Font_Load_Model_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Rendition_Background : constant Xt_N_Resource_String;
   Xm_N_Rendition_Foreground : constant Xt_N_Resource_String;

   Xm_N_Strikethru_Type      : constant Xt_N_Resource_String;
   Xm_N_Underline_Type       : constant Xt_N_Resource_String;

   type Line_Type is
     (No_Line,
      Single_Line,           Double_Line,
      Single_Dashed_Line,    Double_Dashed_Line,
      Not_Specified);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Line_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Line_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Tab_List             : constant Xt_N_Resource_String;
   Xm_N_Tag                  : constant Xt_N_Resource_String;



   type Xm_Render_Table is private;
   Null_Render_Table : constant Xm_Render_Table;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Render_Table_Add_Renditions
   --
   function Xm_Render_Table_Add_Renditions
     (Old_Table  : in Xm_Render_Table;
      Renditions : in Xm_Rendition_Array;
      Merge_Mode : in Xm_Merge_Mode)
      return Xm_Render_Table;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Render_Table_Copy
   --
   function Xm_Render_Table_Copy (Table : in Xm_Render_Table;
				  Tags  : in Xm_String_Tag_Array)
      return Xm_Render_Table;


   -- -------------------------------------------------------------------------
   --
   --  Xm_Render_Table_Free
   --
   procedure Xm_Render_Table_Free (Table : in out Xm_Render_Table);


   -- -------------------------------------------------------------------------
   --
   --  Xm_Render_Table_Get_Rendition
   --
   function Xm_Render_Table_Get_Rendition (Table : in Xm_Render_Table;
					   Tag   : in Xm_String_Tag)
      return Xm_Rendition;


-- ----------------------------------------------------------------------------
--
--                   P A R S E   M A P P I N G
--
-- parse table related routines
--

   type Xm_Parse_Mapping is private;
   Null_Parse_Mapping : constant Xm_Parse_Mapping;

   type Xm_Parse_Table is array (Natural range <>) of Xm_Parse_Mapping;
   Null_Parse_Table : constant Xm_Parse_Table;


   function Xm_Parse_Mapping_Create
     (Args      : in Arg_List)
      return Xm_Parse_Mapping;

   procedure Xm_Parse_Mapping_Set_Values
     (Parse_Mapping : in Xm_Parse_Mapping;
      Args          : in Arg_List);

   procedure Xm_Parse_Mapping_Get_Values
     (Parse_Mapping : in Xm_Parse_Mapping;
      Args          : in Arg_List);

   procedure Xm_Parse_Mapping_Free (Parse_Mapping : in out Xm_Parse_Mapping);

   procedure Xm_Parse_Table_Free   (Parse_Table   : in out Xm_Parse_Table);

-- EndMotif2.0 Motif2.1


-- ----------------------------------------------------------------------------
--
--                      F O N T   L I S T
--
--  font list related routines
--

   -- -------------------------------------------------------------------------
   --
   --  XmFontListAdd
   --
   function Xm_Font_List_Add
     (Old     : in Xm_Font_List;
      Font    : in X_Lib.X_Font_Struct_Pointer;
      Charset : in Xm_String_Char_Set)
     return Xm_Font_List;


   -- -------------------------------------------------------------------------
   --
   --  XmFontListCopy
   --
   function Xm_Font_List_Copy (Fontlist : in Xm_Font_List) 
      return Xm_Font_List;


   -- -------------------------------------------------------------------------
   --
   --  XmFontListCopy
   --
   function Xm_Font_List_Create
     (Font    : in X_Lib.X_Font_Struct_Pointer;
      Charset : in Xm_String_Char_Set)
      return Xm_Font_List;


   -- -------------------------------------------------------------------------
   --
   --  XmFontListFree
   --
   procedure Xm_Font_List_Free (Fontlist : in out Xm_Font_List);



-- UseMotif2.0 Motif2.1
-- ----------------------------------------------------------------------------
--
--                            X M _ S T R I N G
--
--  Interface to the string related routines, using the Motif 2.x scheme
--

   --
   -- Parse Model
   --
   type Xm_Parse_Model is (Output_All,       Output_Between,
                           Output_Beginning, Output_End,
                           Output_Both);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Generate
   --
   function Xm_String_Generate
     (Text           : in String;
      Font_Or_Locale : in Xm_String_Tag;
      Rendition_Tag  : in Xm_String_Tag)
      return Xm_String;

   function Xm_String_Generate
     (Text           : in Wide_String;
      Font_Or_Locale : in Xm_String_Tag;
      Rendition_Tag  : in Xm_String_Tag)
      return Xm_String;

--   function Xm_String_Generate
--     (Text           : in Multibyte_String;
--      Font_Or_Locale : in Xm_String_Tag;
--      Rendition_Tag  : in Xm_String_Tag)
--      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Component_Create
   --
   function Xm_String_Component_Create
     (Tag    : in Xm_String_Component_Type;
      Length : in Interfaces.C.unsigned;
      Value  : in Xt_Pointer)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Put_Rendition
   --
   function Xm_String_Put_Rendition
     (Str           : in Xm_String;
      Rendition_Tag : in Xm_String_Tag)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Unparse
   --
   function Xm_String_Unparse
     (Str         : in Xm_String;
      Tag         : in Xm_String_Tag;
      Tag_Type    : in Xm_Text_Type;
      Parse_Table : in Xm_Parse_Table;
      Parse_Model : in Xm_Parse_Model) return String;

   function Xm_String_Unparse
     (Str         : in Xm_String;
      Tag         : in Xm_String_Tag;
      Tag_Type    : in Xm_Text_Type;
      Parse_Table : in Xm_Parse_Table;
      Parse_Model : in Xm_Parse_Model) return Wide_String;

--   function Xm_String_Unparse
--     (Str         : in Xm_String;
--      Tag         : in Xm_String_Tag;
--      Tag_Type    : in Xm_Text_Type;
--      Parse_Table : in Xm_Parse_Table;
--      Parse_Model : in Xm_Parse_Model) return Multibyte_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Is_Void
   --
   function Xm_String_Is_Void (Str : in Xm_String) return Boolean;

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
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_Simple
   --
   function Xm_String_Create_Simple (Text : in String)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Segment_Create
   --
   function Xm_String_Segment_Create
     (Text      : in String;
      Charset   : in Xm_String_Char_Set  := Xm_String_Default_Charset;
      Direction : in Xm_String_Direction := Left_To_Right;
      Separator : in Boolean             := False)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_L_To_R
   --
   function Xm_String_Create_L_To_R
     (Text    : in String;
      Charset : in Xm_String_Char_Set := Xm_String_Default_Charset)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Direction_Create
   --
   function Xm_String_Direction_Create
     (Direction : in Xm_String_Direction)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Separator_Create
   --
   function Xm_String_Separator_Create return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Get_Next_Component
   --
   -- preliminary!
   procedure Xm_String_Get_Next_Component
     (Context        : in  Xm_String_Context;
      Component      : out Xm_String_Component_Type;
      Text           : out Unbounded_String;
      Tag            : out Xm_String_Tag;
      Direction      : out Xm_String_Direction;
      Unknown_Tag    : out Xm_String_Component_Type;
      Unknown_Length : out System.Address;      -- unsigned short *unknown_length
      Unknown_Value  : out System.Address);     -- unsigned char **unknown_value


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Peek_Next_Component
   --
   function Xm_String_Peek_Next_Component
     (Context : in  Xm_String_Context)
     return Xm_String_Component_Type;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Get_Next_Segment
   --
   procedure Xm_String_Get_Next_Segment
      (Context    : in  Xm_String_Context;
       Text       : out Unbounded_String;
       Charset    : out Xm_String_Char_Set;
       Direction  : out Xm_String_Direction;
       Separator  : out Boolean);


-- ----------------------------------------------------------------------------
--
--                            X M _ S T R I N G
--
--  Part of the string related routines, common to the Motif 2.0 and 1.2 scheme
--

   Xm_String_Error : exception;

   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_Localized
   --
   function Xm_String_Create_Localized (Text : in String)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Free
   --
   procedure Xm_String_Free (String : in out Xm_String);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Copy
   --
   function Xm_String_Copy
     (Str : in Xm_String)
      return Xm_String;

   function Xm_String_N_Copy
     (Str    : in Xm_String;
      N      : in Natural)
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Compare
   --
   function Xm_String_Compare
     (A : in Xm_String;
      B : in Xm_String)
      return Boolean;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Concat
   --
   function Xm_String_Concat
     (String1 : in Xm_String; 
      String2 : in Xm_String)
      return Xm_String;
                            
   function Xm_String_N_Concat
     (String1 : in Xm_String; 
      String2 : in Xm_String;
      N       : in Natural)
      return Xm_String;
                            
   function Xm_String_Concat_And_Free
     (String1 : in Xm_String; 
      String2 : in Xm_String) 
      return Xm_String;


   -- -------------------------------------------------------------------------
   --
   -- String Context
   --
   procedure Xm_String_Init_Context
     (Context : out Xm_String_Context;
      Str     : in  Xm_String);

   procedure Xm_String_Free_Context
     (Context : in out Xm_String_Context);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Empty
   --
   function Xm_String_Empty (Str : in Xm_String) return Boolean;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Has_Substring
   --
   function Xm_String_Has_Substring
     (Str       : in Xm_String;
      Substring : in Xm_String) 
      return Boolean;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Line_Count
   --
   function Xm_String_Line_Count (Str : in Xm_String)
      return Natural;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Width
   --
   function Xm_String_Width
-- UseMotif2.0 Motif2.1
     (Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!      (Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str      : in Xm_String)
      return X_Lib.Dimension;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Height
   --
   function Xm_String_Height
-- UseMotif2.0 Motif2.1
     (Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!      (Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str      : in Xm_String)
      return X_Lib.Dimension;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Extent
   --
   procedure Xm_String_Extent
-- UseMotif2.0 Motif2.1
     (Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!      (Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str       : in     Xm_String;
      Width     :    out X_Lib.Dimension;
      Height    :    out X_Lib.Dimension);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Baseline
   --
   function Xm_String_Baseline
-- UseMotif2.0 Motif2.1
     (Render_Table : in Xm_Render_Table;     -- NEW in Motif 2.0
-- NotMotif2.0 Motif2.1
--!      (Fontlist     : in Xm_Font_List;     -- Motif 1.2
-- EndMotif2.0 Motif2.1
      Str      : in Xm_String)
      return X_Lib.Dimension;


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
      Clip         : in X_Lib.Rectangle);

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
      Lay_Dir      : in Layout_Direction);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Draw_Image
   --
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
      Clip         : in X_Lib.Rectangle);

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
      Lay_Dir      : in Layout_Direction);


   -- -------------------------------------------------------------------------
   --
   --  XmStringDrawUnderline
   --
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
      Underline    : in Xm_String);

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
      Underline    : in Xm_String);


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Create_Font_List
   --
   function Xm_String_Create_Font_List
     (Font    : in X_Lib.X_Font_Struct_Pointer;
      Charset : in Xm_String_Char_Set)
      return Xm_Font_List renames Xm_Font_List_Create;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Get_L_To_R
   --
   function Xm_String_Get_L_To_R
     (Str        : in  Xm_String;
      Charset    : in  Xm_String_Char_Set)
      return String;


   -- -------------------------------------------------------------------------
   --
   --  Xm_String_Length
   --
   function Xm_String_Length (String : in Xm_String)
      return Natural;


-- ----------------------------------------------------------------------------
--
--                  C O M P O U N D _ T E X T
--
--  routines related to compound text
--

   type Compound_Text_Element_Type is new Character;
   type Compound_Text_Type is
      array (Positive range <>) of Compound_Text_Element_Type;

   Null_Compound_Text_Type : constant Compound_Text_Type := "";

   -- conversion routines
   function To_Compound_Text_Type
     (Adr : in Xt_Pointer)
      return Compound_Text_Type;

   function Xm_Cvt_Xm_String_To_CT
     (Str : in Xm_String)
      return Compound_Text_Type;

   function Xm_Cvt_CT_To_Xm_String
     (Str : in Compound_Text_Type)
      return Xm_String;

-- ----------------------------------------------------------------------------
--
--  color calculation
--

   type Xm_Color_Proc is
      access procedure (Background_Color    : in     X_Lib.X_Color;
			Foreground_Color    :    out X_Lib.X_Color;
			Select_Color	    :    out X_Lib.X_Color;
			Top_Shadow_Color    :    out X_Lib.X_Color;
			Bottom_Shadow_Color :    out X_Lib.X_Color);
   pragma Convention (C, Xm_Color_Proc);

   function Xm_Get_Color_Calculation return Xm_Color_Proc;

   function Xm_Set_Color_Calculation (Proc : in Xm_Color_Proc)
      return Xm_Color_Proc;

   procedure Xm_Get_Colors
     (Screen	    : in     X_Lib.Screen_Pointer;
      Colormap      : in     X_Lib.Colormap_Id;
      Background    : in     X_Lib.Pixel;
      Foreground    :    out X_Lib.Pixel;
      Top_Shadow    :    out X_Lib.Pixel;
      Bottom_Shadow :    out X_Lib.Pixel;
      Sel	    :    out X_Lib.Pixel);

   procedure Xm_Change_Color (W 	 : in Widget;
			      Background : in X_Lib.Pixel);



-- ----------------------------------------------------------------------------
--
--                            M I S C
--

   procedure Xm_Update_Display (W : in Widget);

   function Xm_Object_At_Point
     (W    : in Widget;
      X, Y : in X_Lib.Position)
      return Widget;
                  
   function Xm_Get_Visibility (W : in Widget) return Xm_Visibility;

   function Xm_Get_Tab_Group (W : in Widget) return Widget;

   function Xm_Get_Focus_Widget (W : in Widget) return Widget;

   --  Xm_Add_Tab_Group is regarded to be obsolete
   --  use Xm_N_Navigation_Type with Exclusive_Tab_Group instead
   --
   procedure Xm_Add_Tab_Group (Tab_Group : in Widget);

   procedure Xm_Remove_Tab_Group (W : in Widget);

   function Xm_Is_Traversable (W : in Widget) return Boolean;

   function Xm_Process_Traversal
     (W   : in Widget;
      Dir : in Xm_Traversal_Direction)
      return Boolean;

   function Xm_Get_Destination (Display : in X_Lib.Display_Pointer)
      return Widget;
   

   -- error being posted when trying to translate a wrong Xt_Pointer to
   -- a certain Callback_Struct

   Xm_Error_No_Callback_Struct_Pointer : exception;
   Xm_Error_Invalid_Callback_Reason    : exception;


-- UseMotif2.0 Motif2.1
   Xm_Error_Conversion_Failed          : exception;

   --  if unsuccessful, raise Xm_Error_Conversion_Failed
   -- 
   function Xm_Convert_String_To_Units
     (Scr         : in X_lib.Screen_Pointer;
      Spec        : in String;
      Orientation : in Orientation_Type;
      To_Unit     : in Unit_Type)
      return Integer;
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   -- -------------------------------------------------------------------------
   --
   -- resource strings of core intrinsics
   --
   Xm_N_Accelerators        : constant Xt_N_Resource_String;
   Xm_N_Ancestor_Sensitive  : constant Xt_N_Resource_String;
   Xm_N_Background          : constant Xt_N_Resource_String;
   Xm_N_Background_Pixmap   : constant Xt_N_Resource_String;
   Xm_N_Border_Color        : constant Xt_N_Resource_String;
   Xm_N_Border_Pixmap       : constant Xt_N_Resource_String;
   Xm_N_Border_Width        : constant Xt_N_Resource_String;
   Xm_N_Colormap            : constant Xt_N_Resource_String;
   Xm_N_Depth               : constant Xt_N_Resource_String;
   Xm_N_Destroy_Callback    : constant Xt_N_Resource_String;
   Xm_N_Height              : constant Xt_N_Resource_String;
   Xm_N_Initial_Resources_Persistent : constant Xt_N_Resource_String;
   Xm_N_Mapped_When_Managed : constant Xt_N_Resource_String;
   Xm_N_Screen              : constant Xt_N_Resource_String;
   Xm_N_Sensitive           : constant Xt_N_Resource_String;
   Xm_N_Translations        : constant Xt_N_Resource_String;
   Xm_N_Width               : constant Xt_N_Resource_String;
   Xm_N_X                   : constant Xt_N_Resource_String;
   Xm_N_Y                   : constant Xt_N_Resource_String;

   -- -------------------------------------------------------------------------
   --
   -- resource strings of composites
   --
   Xm_N_Children            : constant Xt_N_Resource_String;
   Xm_N_Insert_Position     : constant Xt_N_Resource_String;
   Xm_N_Num_Children        : constant Xt_N_Resource_String;




--
--
--
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_String);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String_Table);

   -- procedure Append_Get (List  : in out Arg_List;
   --                       Name  : in     Xt_N_Resource_String;
   --                       Value : out    Xm_String_Table);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Font_List);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Font_List);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_String_Direction);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_String_Direction);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xm_String_Char_Set);

   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xm_String_Char_Set);
   pragma Convention (C, Append_Get);


-- UseMotif2.0 Motif2.1
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Render_Table);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Render_Table);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Font_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Font_Type);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     String);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Unit_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Unit_Type);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Orientation_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Orientation_Type);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Alignment);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Alignment);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Layout_Direction);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Layout_Direction);
   pragma Convention (C, Append_Get);


-- ---------------------------------------------------------------------------
--
--  Type conversion
--
   function To_Address (Xm_Str : in Xm_String) return System.Address;

private

   type Xm_String is new System.Address;
   Null_Xm_String : constant Xm_String := Xm_String (System.Null_Address);

   type Xm_String_Context is new System.Address;
   Null_String_Context : constant Xm_String_Context :=  Xm_String_Context(System.Null_Address);


   for Xm_Text_Type use (Charset_Text => 0,
                         Multibyte_Text => 1,
                         Widechar_Text => 2,
                         No_Text => 3);
   for Xm_Text_Type'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Text_Direction use (Forward => 0, Backward => 1);
   for Xm_Text_Direction'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Text_Scan_Type use (Select_Position => 0,
                              Select_Whitespace => 1,
                              Select_Word => 2,
                              Select_Line => 3,
                              Select_All => 4,
                              Select_Paragraph => 5,
                              Select_Out_Line => 6);
   for Xm_Text_Scan_Type'Size use Interfaces.C.unsigned_char'Size;


-- UseMotif2.0 Motif2.1
   type Xm_Tab is new System.Address;
   Null_Tab : constant Xm_Tab := Xm_Tab (System.Null_Address);

   type Xm_Tab_List is new System.Address;
   Null_Tab_List : constant Xm_Tab_List := Xm_Tab_List (System.Null_Address);

   type Xm_Rendition is new System.Address;
   Null_Rendition : constant Xm_Rendition := Xm_Rendition (System.Null_Address);

   for Font_Load_Model_Type use (Unspecified => 0, Deferred => 1, Immediate => 2);
   for Font_Load_Model_Type'Size use Interfaces.C.unsigned_char'Size;

   for Line_Type use
     (No_Line => 0,
      Single_Line => 1,           Double_Line => 2,
      Single_Dashed_Line => 3,    Double_Dashed_Line => 4,
      Not_Specified => 255);
   for Line_Type'Size use Interfaces.C.unsigned_char'Size;


   type Xm_Render_Table is new System.Address;
   Null_Render_Table : constant Xm_Render_Table := Xm_Render_Table (System.Null_Address);


   type Xm_Parse_Mapping is new System.Address;
   Null_Parse_Mapping : constant Xm_Parse_Mapping := Xm_Parse_Mapping (System.Null_Address);
   Null_Parse_Table : constant Xm_Parse_Table (1 .. 0) := (others => Null_Parse_Mapping);


   for Xm_Parse_Model use (Output_All => 0,       Output_Between => 1,
                           Output_Beginning => 2, Output_End => 3,
                           Output_Both => 4);
   for Xm_Parse_Model'Size use Interfaces.C.unsigned_char'Size;

-- EndMotif2.0 Motif2.1

   type Xm_Font_List  is new System.Address;
   Null_Font_List : constant Xm_Font_List := Xm_Font_List (System.Null_Address);

   for Font_Type use (Font_Is_Font    => 0,
                      Font_is_Fontset => 1);
   for Font_Type'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Include_Status use (Insert => 0, Do_Terminate => 1, Invoke => 2);
   for Xm_Include_Status'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Offset_Model use (Absolute => 0, Relative => 1);
   for Xm_Offset_Model'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Visibility use (Unobscured => 0,
                          Partially_Obscured => 1,
                          Fully_Obscured => 2);
   for Xm_Visibility'Size use Interfaces.C.unsigned_char'Size;


   for Xm_Traversal_Direction use (Current => 0,
                                   Next => 1,
                                   Prev => 2,
                                   Home => 3,
                                   Next_Tab_Group => 4,
                                   Prev_Tab_Group => 5,
                                   Up => 6,
                                   Down => 7,
                                   Left => 8,
                                   Right => 9,
                                   Globally_Forward => 10,
                                   Globally_Backward => 11);
   for Xm_Traversal_Direction'Size use Interfaces.C.unsigned_char'Size;


   -- this generic function can be used to obtain a callback struct from
   -- the third parameter of a callback procedure (Call_Data)
   --

   type Callback_Reason_Array_Type is
      array (Natural range <>) of Callback_Reason;

   generic
      type Callback_Struct is private;
      type Callback_Struct_Access is access all Callback_Struct;
      Valid_Reasons : Callback_Reason_Array_Type;
   function To_Generic_Callback_Struct_Access (Pointer : in Xt_Pointer)
      return Callback_Struct_Access;


-- UseMotif2.0 Motif2.1
   for Unit_Type use (Pixels => 0,           Centi_Millimeters => 1,
                      Milli_Inches => 2,     Centi_Points => 3,
                      Centi_Font_Units => 4, Inches => 5,
                      Centimeters => 6,      Millimeters => 7,
                      Points => 8,           Font_Units => 9);
-- NotMotif2.0 Motif2.1
--!    for Unit_Type use (Pixels => 0,           Centi_Millimeters => 1,
--!                       Milli_Inches => 2,     Centi_Points => 3,
--!                       Centi_Font_Units => 4);
-- EndMotif2.0 Motif2.1
   for Unit_Type'Size use Interfaces.C.unsigned_char'Size;

   for Orientation_Type use (No_Orientation => 0, Vertical => 1, Horizontal => 2);
   for Orientation_Type'Size use Interfaces.C.unsigned_char'Size;

   for Alignment use (Alignment_Beginning => 0,
                      Alignment_Center    => 1,
                      Alignment_End       => 2);
-- Use64Bit
--!    for Alignment'Size use Unsigned'Size;
-- Not64Bit
   for Alignment'Size use Interfaces.C.unsigned_char'Size;
-- End64Bit

   XmRIGHT_TO_LEFT_MASK    : constant := 16#01#;
   XmLEFT_TO_RIGHT_MASK    : constant := 16#02#;
   XmHORIZONTAL_MASK       : constant := XmRIGHT_TO_LEFT_MASK +
                                         XmLEFT_TO_RIGHT_MASK;
   XmTOP_TO_BOTTOM_MASK    : constant := 16#04#;
   XmBOTTOM_TO_TOP_MASK    : constant := 16#08#;
   XmVERTICAL_MASK         : constant := XmTOP_TO_BOTTOM_MASK +
                                         XmBOTTOM_TO_TOP_MASK;
   XmPRECEDENCE_HORIZ_MASK : constant := 16#40#;
   XmPRECEDENCE_VERT_MASK  : constant := 16#80#;
   XmPRECEDENCE_MASK       : constant := XmPRECEDENCE_HORIZ_MASK +
                                         XmPRECEDENCE_VERT_MASK;

   for Layout_Direction use
     (Right_To_Left_Top_To_Bottom => XmRIGHT_TO_LEFT_MASK +
                                     XmTOP_TO_BOTTOM_MASK +
                                     XmPRECEDENCE_HORIZ_MASK,
      Left_To_Right_Top_To_Bottom => XmLEFT_TO_RIGHT_MASK +
                                     XmTOP_TO_BOTTOM_MASK +
                                     XmPRECEDENCE_HORIZ_MASK,
      Right_To_Left_Bottom_To_Top => XmRIGHT_TO_LEFT_MASK +
                                     XmBOTTOM_TO_TOP_MASK +
                                     XmPRECEDENCE_HORIZ_MASK,
      Left_To_Right_Bottom_To_Top => XmLEFT_TO_RIGHT_MASK +
                                     XmBOTTOM_TO_TOP_MASK +
                                     XmPRECEDENCE_HORIZ_MASK,
      Top_To_Bottom_Right_To_Left => XmRIGHT_TO_LEFT_MASK +
                                     XmTOP_TO_BOTTOM_MASK +
                                     XmPRECEDENCE_VERT_MASK,
      Top_To_Bottom_Left_To_Right => XmLEFT_TO_RIGHT_MASK +
                                     XmTOP_TO_BOTTOM_MASK +
                                     XmPRECEDENCE_VERT_MASK,
      Bottom_To_Top_Right_To_Left => XmRIGHT_TO_LEFT_MASK +
                                     XmBOTTOM_TO_TOP_MASK +
                                     XmPRECEDENCE_VERT_MASK,
      Bottom_To_Top_Left_To_Right => XmLEFT_TO_RIGHT_MASK +
                                     XmBOTTOM_TO_TOP_MASK +
                                     XmPRECEDENCE_VERT_MASK,
      Top_To_Bottom               => XmTOP_TO_BOTTOM_MASK +
                                     XmHORIZONTAL_MASK +
                                     XmPRECEDENCE_MASK,
      Bottom_To_Top               => XmBOTTOM_TO_TOP_MASK +
                                     XmHORIZONTAL_MASK +
                                     XmPRECEDENCE_MASK,
      Right_To_Left               => XmRIGHT_TO_LEFT_MASK +
                                     XmVERTICAL_MASK +
                                     XmPRECEDENCE_MASK,
      Left_To_Right               => XmLEFT_TO_RIGHT_MASK +
                                     XmVERTICAL_MASK +
                                     XmPRECEDENCE_MASK,
      Default_Direction           => 16#FF#);
-- Use64Bit
--!    for Layout_Direction'Size use Unsigned'Size;
-- Not64Bit
   for Layout_Direction'Size use Interfaces.C.unsigned_char'Size;
-- End64Bit


-- UseMotif2.0 Motif2.1
   pragma Import (C, Xm_Tab_Set_Value, "XmTabSetValue");
   pragma Import (C, Xm_Tab_List_Tab_Copy, "XmTabListTabCopy");
   pragma Import (C, Xm_Tab_List_Get_Tab, "XmTabListGetTab");
-- EndMotif2.0 Motif2.1
   pragma Import (C, Xm_Font_List_Add, "XmFontListAdd");
   pragma Import (C, Xm_Font_List_Copy, "XmFontListCopy");
   pragma Import (C, Xm_Font_List_Create, "XmFontListCreate");
   pragma Import (C, Xm_String_Direction_Create, "XmStringDirectionCreate");
   pragma Import (C, Xm_String_Peek_Next_Component,"XmStringPeekNextComponent");
-- UseMotif2.0 Motif2.1
   pragma Import (C, Xm_String_Put_Rendition, "XmStringPutRendition");
   pragma Import (C, Xm_Render_Table_Get_Rendition, "XmRenderTableGetRendition");
-- EndMotif2.0 Motif2.1
   pragma Import (C, Xm_String_Copy, "XmStringCopy");
   pragma Import (C, Xm_String_N_Copy, "XmStringNCopy");
   pragma Import (C, Xm_String_Concat, "XmStringConcat");
   pragma Import (C, Xm_String_N_Concat, "XmStringNConcat");
   pragma Import (C, Xm_String_Concat_And_Free, "XmStringConcatAndFree");
   pragma Import (C, Xm_String_Line_Count, "XmStringLineCount");
   pragma Import (C, Xm_String_Length, "XmStringLength");
   pragma Import (C, Xm_String_Width, "XmStringWidth");
   pragma Import (C, Xm_String_Height, "XmStringHeight");
   pragma Import (C, Xm_String_Extent, "XmStringExtent");
   pragma Import (C, Xm_String_Baseline, "XmStringBaseline");
   pragma Import (C, Xm_String_Separator_Create, "XmStringSeparatorCreate");
   pragma Import (C, Xm_Get_Color_Calculation, "XmGetColorCalculation");
   pragma Import (C, Xm_Set_Color_Calculation, "XmSetColorCalculation");
   pragma Import (C, Xm_Get_Colors, "XmGetColors");
   pragma Import (C, Xm_Change_Color, "XmChangeColor");
   pragma Import (C, Xm_Update_Display,"XmUpdateDisplay");
   pragma Import (C, Xm_Object_At_Point,"XmObjectAtPoint");
   pragma Import (C, Xm_Get_Visibility, "XmGetVisibility");
   pragma Import (C, Xm_Get_Tab_Group, "XmGetTabGroup");
   pragma Import (C, Xm_Get_Focus_Widget, "XmGetFocusWidget");
   pragma Import (C, Xm_Add_Tab_Group, "XmAddTabGroup");
   pragma Import (C, Xm_Remove_Tab_Group, "XmRemoveTabGroup");
   pragma Import (C, Xm_Get_Destination, "XmGetDestination");



   --
   -- from XtIntrinsics core
   --
   Xm_N_Accelerators        : constant Xt_N_Resource_String := Xt_N_Accelerators;
   Xm_N_Ancestor_Sensitive  : constant Xt_N_Resource_String := Xt_N_Ancestor_Sensitive;
   Xm_N_Background          : constant Xt_N_Resource_String := Xt_N_Background;
   Xm_N_Background_Pixmap   : constant Xt_N_Resource_String := Xt_N_Background_Pixmap;
   Xm_N_Border_Color        : constant Xt_N_Resource_String := Xt_N_Border_Color;
   Xm_N_Border_Pixmap       : constant Xt_N_Resource_String := Xt_N_Border_Pixmap;
   Xm_N_Border_Width        : constant Xt_N_Resource_String := Xt_N_Border_Width;
   Xm_N_Colormap            : constant Xt_N_Resource_String := Xt_N_Colormap;
   Xm_N_Depth               : constant Xt_N_Resource_String := Xt_N_Depth;
   Xm_N_Destroy_Callback    : constant Xt_N_Resource_String := Xt_N_Destroy_Callback;
   Xm_N_Height              : constant Xt_N_Resource_String := Xt_N_Height;
   Xm_N_Initial_Resources_Persistent : constant Xt_N_Resource_String
      := Xt_N_Initial_Resources_Persistent;
   Xm_N_Mapped_When_Managed : constant Xt_N_Resource_String := Xt_N_Mapped_When_Managed;
   Xm_N_Screen              : constant Xt_N_Resource_String := Xt_N_Screen;
   Xm_N_Sensitive           : constant Xt_N_Resource_String := Xt_N_Sensitive;
   Xm_N_Translations        : constant Xt_N_Resource_String := Xt_N_Translations;
   Xm_N_Width               : constant Xt_N_Resource_String := Xt_N_Width;
   Xm_N_X                   : constant Xt_N_Resource_String := Xt_N_X;
   Xm_N_Y                   : constant Xt_N_Resource_String := Xt_N_Y;

   --
   -- resource strings of composites
   --
   Xm_N_Children            : constant Xt_N_Resource_String :=
      Xt_N_Children;
   Xm_N_Insert_Position     : constant Xt_N_Resource_String :=
      Xt_N_Insert_Position;
   Xm_N_Num_Children        : constant Xt_N_Resource_String :=
      Xt_N_Num_Children;

   Xm_N_Font_Name           : constant Xt_N_Resource_String :=
      To_Resource_String ("fontName");
   Xm_N_Font_Type           : constant Xt_N_Resource_String :=
      To_Resource_String ("fontType");
   Xm_N_Font                : constant Xt_N_Resource_String :=
      To_Resource_String ("font");
   Xm_N_Underline_Type      : constant Xt_N_Resource_String :=
      To_Resource_String ("underlineType");
   Xm_N_Strikethru_Type     : constant Xt_N_Resource_String :=
      To_Resource_String ("strikethruType");
   Xm_N_Tab_List            : constant Xt_N_Resource_String :=
      To_Resource_String ("tabList");
   Xm_N_Rendition_Background : constant Xt_N_Resource_String :=
      To_Resource_String ("renditionBackground");
   Xm_N_Rendition_Foreground : constant Xt_N_Resource_String :=
      To_Resource_String ("renditionForeground");
   Xm_N_Load_Model           : constant Xt_N_Resource_String :=
      To_Resource_String ("loadModel");
   Xm_N_Tag                  : constant Xt_N_Resource_String :=
      To_Resource_String ("Tag");


   --
   -- resource strings of subpackages of xm_widgets
   --
-- UseMotif2.0 Motif2.1
   Xm_N_Destination_Callback    : constant Xt_N_Resource_String :=
      To_Resource_String ("destinationCallback");
   Xm_N_Detail_Shadow_Thickness : constant Xt_N_Resource_String :=
      To_Resource_String ("detailShadowThickness");
-- EndMotif2.0 Motif2.1
   Xm_N_Font_List               : constant Xt_N_Resource_String :=
      To_Resource_String ("fontList");
   Xm_N_Label_String            : constant Xt_N_Resource_String :=
      To_Resource_String ("labelString");
   Xm_N_Margin_Height           : constant Xt_N_Resource_String :=
      To_Resource_String ("marginHeight");
   Xm_N_Margin_Width            : constant Xt_N_Resource_String :=
      To_Resource_String ("marginWidth");
   Xm_N_Modify_Verify_Callback  : constant Xt_N_Resource_String :=
      To_Resource_String ("modifyVerifyCallback");
-- UseMotif2.0 Motif2.1
   Xm_N_Primary_Ownership       : constant Xt_N_Resource_String :=
      To_Resource_String ("primaryOwnership");
   Xm_N_Render_Table            : constant Xt_N_Resource_String :=
      To_Resource_String ("renderTable");
-- EndMotif2.0 Motif2.1
   Xm_N_Select_Color            : constant Xt_N_Resource_String :=
      To_Resource_String ("selectColor");
   Xm_N_Selection_Policy        : constant Xt_N_Resource_String :=
      To_Resource_String ("selectionPolicy");
   Xm_N_Unit_Type               : constant Xt_N_Resource_String :=
      To_Resource_String ("unitType");
   Xm_N_User_Data               : constant Xt_N_Resource_String :=
      To_Resource_String ("userData");
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String :=
      To_Resource_String ("valueChangedCallback");

end Xm_Widgets;
