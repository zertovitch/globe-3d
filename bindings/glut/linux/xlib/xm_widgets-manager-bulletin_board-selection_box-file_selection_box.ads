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
--          27 Jan 2002 V. Godunko: mark Xm_File_Selection_Box_Get_Child
--                                  obsolete for Motif 2.x, add name strings
--                                  for children
--          03 Feb 2002 H.-F. Vogt: move definition of children name strings
--                                  into private section of package
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Bulletin_Board.Selection_Box.File_Selection_Box is

   Xm_File_Selection_Box_Widget_Class  : constant Widget_Class;


   type Xm_File_Selection_Box_Callback_Struct is record
      Reason         : Callback_Reason;
      Event          : X_Lib.X_Event_Pointer;
      Value          : Xm_String;
      Length         : Integer;
      Mask           : Xm_String;
      Mask_Length    : Integer;
      Dir            : Xm_String;
      Dir_Length     : Integer;
      Pattern        : Xm_String;
      Pattern_Length : Integer;
   end record;
   pragma Convention (C, Xm_File_Selection_Box_Callback_Struct);

   type Xm_File_Selection_Box_Callback_Struct_Access is
      access all Xm_File_Selection_Box_Callback_Struct;

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_File_Selection_Box_Callback_Struct_Access;


   function Xm_Is_File_Selection_Box (W: in Widget) return Boolean;


   function Xm_Create_File_Selection_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_File_Selection_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   type File_Selection_Box_Child_Type is (None,
                                          Apply_Button,
                                          Cancel_Button,
                                          Default_Button,
                                          OK_Button,
                                          Filter_Label,
                                          Filter_Text,
                                          Help_Button,
                                          File_List,
                                          File_List_Label,
                                          Selection_Label,
                                          Text,
                                          Separator,
                                          Dir_List,
                                          Dir_List_Label);

   -- in Motif 2.x this function is obsolete, use Xt_Name_To_Widget instead
   --
   function Xm_File_Selection_Box_Get_Child
     (W      : in Widget;
      Child  : in File_Selection_Box_Child_Type)
      return Widget;

   Apply_Button_Name               : constant String;
   Cancel_Button_Name              : constant String;
   Dir_Label_Name                  : constant String;
   Dir_List_Name                   : constant String;
   Dir_List_Scrolled_Window_Name   : constant String;
   Filter_Label_Name               : constant String;
   Filter_Text_Field_Name          : constant String;
   Help_Button_Name                : constant String;
   Items_Label_Name                : constant String;
   Items_List_Name                 : constant String;
   Items_List_Scrolled_Window_Name : constant String;
   Ok_Button_Name                  : constant String;
   Selection_Label_Name            : constant String;
   Separator_Name                  : constant String;
   Text_Field_Name                 : constant String;


   procedure Xm_File_Selection_Do_Search
     (FS      : in Widget;
      Dirmask : in Xm_String);


   type Xm_Search_Proc is access procedure
     (W           : in Widget;
      Search_Data : in Xm_File_Selection_Box_Callback_Struct);
   pragma Convention (C, Xm_Search_Proc);
   Null_Search_Proc : constant Xm_Search_Proc := null;

   type Xm_Qualify_Proc is access procedure
     (W           : in     Widget;
      Input_Data  : in     Xm_File_Selection_Box_Callback_Struct;
      Output_Data :    out Xm_File_Selection_Box_Callback_Struct);
   pragma Convention (C, Xm_Qualify_Proc);
   Null_Qualify_Proc : constant Xm_Qualify_Proc := null;



   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Directory              : constant Xt_N_Resource_String;
   Xm_N_Directory_Valid        : constant Xt_N_Resource_String;
   Xm_N_Dir_List_Items         : constant Xt_N_Resource_String;
   Xm_N_Dir_List_Item_Count    : constant Xt_N_Resource_String;
   Xm_N_Dir_List_Label_String  : constant Xt_N_Resource_String;
   Xm_N_Dir_Mask               : constant Xt_N_Resource_String;
   Xm_N_Dir_Search_Proc        : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Search_Proc);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Search_Proc);
   pragma Convention (C, Append_Get);

   Xm_N_Dir_Spec               : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Dir_Text_Label_String  : constant Xt_N_Resource_String;
   Xm_N_Filter_Label_String    : constant Xt_N_Resource_String;
   Xm_N_Filter_Style           : constant Xt_N_Resource_String;

   type Filter_Style_Type is (Filter_None, Filter_Hidden_Files);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Filter_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Filter_Style_Type);
   pragma Convention (C, Append_Get);


-- EndMotif2.0 Motif2.1
   Xm_N_File_List_Items        : constant Xt_N_Resource_String;
   Xm_N_File_List_Item_Count   : constant Xt_N_Resource_String;
   Xm_N_File_List_Label_String : constant Xt_N_Resource_String;
   Xm_N_File_Search_Proc       : constant Xt_N_Resource_String;
   Xm_N_File_Type_Mask         : constant Xt_N_Resource_String;

   type File_Type_Mask is (Directory, Regular, Any_Type);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     File_Type_Mask);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out File_Type_Mask);
   pragma Convention (C, Append_Get);

   Xm_N_List_Updated           : constant Xt_N_Resource_String;
   Xm_N_No_Match_String        : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Path_Mode              : constant Xt_N_Resource_String;

   type Path_Mode_Type is (Mode_Full, Mode_Relative);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Path_Mode_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Path_Mode_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   Xm_N_Pattern                : constant Xt_N_Resource_String;
   Xm_N_Qualify_Search_Data_Proc : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xm_Qualify_Proc);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xm_Qualify_Proc);
   pragma Convention (C, Append_Get);




private

   for File_Selection_Box_Child_Type use (None            => 0,
                                          Apply_Button    => 1,
                                          Cancel_Button   => 2,
                                          Default_Button  => 3,
                                          OK_Button       => 4,
                                          Filter_Label    => 5,
                                          Filter_Text     => 6,
                                          Help_Button     => 7,
                                          File_List       => 8,
                                          File_List_Label => 9,
                                          Selection_Label => 11,
                                          Text            => 13,
                                          Separator       => 14,
                                          Dir_List        => 15,
                                          Dir_List_Label  => 16);
   for File_Selection_Box_Child_Type'Size use Interfaces.C.unsigned_char'Size;

   Apply_Button_Name               : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Apply_Button_Name;
   Cancel_Button_Name              : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Cancel_Button_Name;
   Dir_Label_Name                  : constant String := "Dir";
   Dir_List_Name                   : constant String := "DirList";
   Dir_List_Scrolled_Window_Name   : constant String := "DirListSW";
   Filter_Label_Name               : constant String := "FilterLabel";
   Filter_Text_Field_Name          : constant String := "FilterText";
   Help_Button_Name                : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Help_Button_Name;
   Items_Label_Name                : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Items_Label_Name;
   Items_List_Name                 : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Items_List_Name;
   Items_List_Scrolled_Window_Name : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Items_List_Scrolled_Window_Name;
   Ok_Button_Name                  : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Ok_Button_Name;
   Selection_Label_Name            : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Selection_Label_Name;
   Separator_Name                  : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Separator_Name;
   Text_Field_Name                 : constant String :=
      Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Text_Field_Name;


-- UseMotif2.0 Motif2.1
   for Filter_Style_Type use (Filter_None => 0, Filter_Hidden_Files => 1);
   for Filter_Style_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1
   for File_Type_Mask use (Directory => 0, Regular => 1, Any_Type => 2);
   for File_Type_Mask'Size use Interfaces.C.unsigned_char'Size;
-- UseMotif2.0 Motif2.1
   for Path_Mode_Type use (Mode_Full => 0, Mode_Relative => 1);
   for Path_Mode_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   pragma Import (C, Xm_File_Selection_Box_Get_Child, "XmFileSelectionBoxGetChild");
   pragma Import (C, Xm_File_Selection_Do_Search, "XmFileSelectionDoSearch");


   c_const_Xm_File_Selection_Box_Widget_Class  : Widget_Class;

   pragma Import (C, c_const_Xm_File_Selection_Box_Widget_Class, "xmFileSelectionBoxWidgetClass");

   Xm_File_Selection_Box_Widget_Class  : constant Widget_Class :=
    c_const_Xm_File_Selection_Box_Widget_Class;

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Directory              : constant Xt_N_Resource_String
      := To_Resource_String ("directory");
   Xm_N_Directory_Valid        : constant Xt_N_Resource_String
      := To_Resource_String ("directoryValid");
   Xm_N_Dir_List_Items         : constant Xt_N_Resource_String
      := To_Resource_String ("dirListItems");
   Xm_N_Dir_List_Item_Count    : constant Xt_N_Resource_String
      := To_Resource_String ("dirListItemCount");
   Xm_N_Dir_List_Label_String  : constant Xt_N_Resource_String
      := To_Resource_String ("dirListLabelString");
   Xm_N_Dir_Mask               : constant Xt_N_Resource_String
      := To_Resource_String ("dirMask");
   Xm_N_Dir_Search_Proc        : constant Xt_N_Resource_String
      := To_Resource_String ("dirSearchProc");
   Xm_N_Dir_Spec               : constant Xt_N_Resource_String
      := To_Resource_String ("dirSpec");
-- UseMotif2.0 Motif2.1
   Xm_N_Dir_Text_Label_String  : constant Xt_N_Resource_String
      := To_Resource_String ("dirTextLabelString");
   Xm_N_Filter_Label_String    : constant Xt_N_Resource_String
      := To_Resource_String ("filterLabelString");
   Xm_N_Filter_Style           : constant Xt_N_Resource_String
      := To_Resource_String ("filterStyle");
-- EndMotif2.0 Motif2.1
   Xm_N_File_List_Items        : constant Xt_N_Resource_String
      := To_Resource_String ("fileListItems");
   Xm_N_File_List_Item_Count   : constant Xt_N_Resource_String
      := To_Resource_String ("fileListItemCount");
   Xm_N_File_List_Label_String : constant Xt_N_Resource_String
      := To_Resource_String ("fileListLabelString");
   Xm_N_File_Search_Proc       : constant Xt_N_Resource_String
      := To_Resource_String ("fileSearchProc");
   Xm_N_File_Type_Mask         : constant Xt_N_Resource_String
      := To_Resource_String ("fileTypeMask");
   Xm_N_List_Updated           : constant Xt_N_Resource_String
      := To_Resource_String ("listUpdated");
   Xm_N_No_Match_String        : constant Xt_N_Resource_String
      := To_Resource_String ("noMatchString");
-- UseMotif2.0 Motif2.1
   Xm_N_Path_Mode              : constant Xt_N_Resource_String
      := To_Resource_String ("pathMode");
-- EndMotif2.0 Motif2.1
   Xm_N_Pattern                : constant Xt_N_Resource_String
      := To_Resource_String ("pattern");
   Xm_N_Qualify_Search_Data_Proc : constant Xt_N_Resource_String
      := To_Resource_String ("qualifySearchDataProc");


end Xm_Widgets.Manager.Bulletin_Board.Selection_Box.File_Selection_Box;
