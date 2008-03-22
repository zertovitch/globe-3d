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
--          27 Jan 2002 V. Godunko: mark Xm_Selection_Box_Get_Child obsolete
--                                  for Motif = 2.x, add name strings for
--                                  children
--          03 Feb 2002 H.-F. Vogt: move definition of children name strings
--                                  into private section of package
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Bulletin_Board.Selection_Box is

   Xm_Selection_Box_Widget_Class       : constant Widget_Class;


   type Xm_Selection_Box_Callback_Struct is record
      Reason  : Callback_Reason;
      Event   : X_Lib.X_Event_Pointer;
      Value   : Xm_String;
      Length  : Integer;
   end record;
   pragma Convention (C, Xm_Selection_Box_Callback_Struct);

   type Xm_Selection_Box_Callback_Struct_Access is
      access all Xm_Selection_Box_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Selection_Box_Callback_Struct_Access;



   function Xm_Is_Selection_Box (W: in Widget) return Boolean;


   function Xm_Create_Selection_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Selection_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Prompt_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   type Selection_Box_Child_Type is (None,
                                     Apply_Button,
                                     Cancel_Button,
                                     Default_Button,
                                     Ok_Button,
                                     Help_Button,
                                     List,
                                     List_Label,
                                     Selection_Label,
                                     Text,
                                     Separator);

   --  in Motif 2.x this function is obsolete, instead use Xt_Name_To_Widget
   --
   function Xm_Selection_Box_Get_Child
     (W      : in Widget;
      Child  : in Selection_Box_Child_Type)
      return Widget;

   Apply_Button_Name               : constant String;
   Cancel_Button_Name              : constant String;
   Help_Button_Name                : constant String;
   Items_Label_Name                : constant String;
   Items_List_Name                 : constant String;
   Items_List_Scrolled_Window_Name : constant String;
   Ok_Button_Name                  : constant String;
   Selection_Label_Name            : constant String;
   Separator_Name                  : constant String;
   Text_Field_Name                 : constant String;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Apply_Callback         : constant Xt_N_Resource_String;
   Xm_N_Apply_Label_String     : constant Xt_N_Resource_String;
   Xm_N_Cancel_Callback        : constant Xt_N_Resource_String;
   Xm_N_Cancel_Label_String    : constant Xt_N_Resource_String;
   Xm_N_Child_Placement        : constant Xt_N_Resource_String;

   type Child_Placement_Type is (Place_Top,
                                 Place_Above_Selection,
                                 Place_Below_Selection);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Child_Placement_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Child_Placement_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Dialog_Type            : constant Xt_N_Resource_String;

   type Dialog_Type is (Work_Area_Dialog, Prompt_Dialog, Selection_Dialog,
                        Command_Dialog, File_Selection_Dialog);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Dialog_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Dialog_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Help_Label_String      : constant Xt_N_Resource_String;
   Xm_N_List_Item_Count        : constant Xt_N_Resource_String;
   Xm_N_List_Items             : constant Xt_N_Resource_String;
   Xm_N_List_Visible_Item_Count :  constant Xt_N_Resource_String;
   Xm_N_Minimize_Buttons       :  constant Xt_N_Resource_String;
   Xm_N_Must_Match             :  constant Xt_N_Resource_String;
   Xm_N_No_Match_Callback      :  constant Xt_N_Resource_String;
   Xm_N_OK_Callback            :  constant Xt_N_Resource_String;
   Xm_N_OK_Label_String        :  constant Xt_N_Resource_String;
   Xm_N_Selection_Label_String :  constant Xt_N_Resource_String;
   Xm_N_Text_Accelerators      :  constant Xt_N_Resource_String;
   Xm_N_Text_Columns           :  constant Xt_N_Resource_String;
   Xm_N_Text_String            :  constant Xt_N_Resource_String;



private

   for Selection_Box_Child_Type use (None            => 0,
                                     Apply_Button    => 1,
                                     Cancel_Button   => 2,
                                     Default_Button  => 3,
                                     Ok_Button       => 4,
                                     Help_Button     => 7,
                                     List            => 8,
                                     List_Label      => 9,
                                     Selection_Label => 11,
                                     Text            => 13,
                                     Separator       => 14);
   for Selection_Box_Child_Type'Size use Interfaces.C.unsigned_char'Size;


   pragma Import (C, Xm_Selection_Box_Get_Child, "XmSelectionBoxGetChild");

   Apply_Button_Name               : constant String := "Apply";
   Cancel_Button_Name              : constant String := "Cancel";
   Help_Button_Name                : constant String := "Help";
   Items_Label_Name                : constant String := "Items";
   Items_List_Name                 : constant String := "ItemsList";
   Items_List_Scrolled_Window_Name : constant String := "ItemsListSW";
   Ok_Button_Name                  : constant String := "OK";
   Selection_Label_Name            : constant String := "Selection";
   Separator_Name                  : constant String := "Separator";
   Text_Field_Name                 : constant String := "Text";

   for Child_Placement_Type use (Place_Top             => 0,
                                 Place_Above_Selection => 1,
                                 Place_Below_Selection => 2);
   for Child_Placement_Type'Size use Interfaces.C.unsigned_char'Size;

   for Dialog_Type use (Work_Area_Dialog      => 0,
                        Prompt_Dialog         => 1,
                        Selection_Dialog      => 2,
                        Command_Dialog        => 3,
			File_Selection_Dialog => 4);
   for Dialog_Type'Size use Interfaces.C.unsigned_char'Size;



   c_const_Xm_Selection_Box_Widget_Class       : Widget_Class;

   pragma Import (C, c_const_Xm_Selection_Box_Widget_Class, "xmSelectionBoxWidgetClass");

   Xm_Selection_Box_Widget_Class       : constant Widget_Class :=
    c_const_Xm_Selection_Box_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Apply_Callback         : constant Xt_N_Resource_String
      := To_Resource_String ("applyCallback");
   Xm_N_Apply_Label_String     : constant Xt_N_Resource_String
      := To_Resource_String ("applyLabelString");
   Xm_N_Cancel_Callback        : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Cancel_Callback;
   Xm_N_Cancel_Label_String    : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Cancel_Label_String;
   Xm_N_Child_Placement        : constant Xt_N_Resource_String
      := To_Resource_String ("childPlacement");

   Xm_N_Dialog_Type            : constant Xt_N_Resource_String
      := To_Resource_String ("dialogType");

   Xm_N_Help_Label_String      : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Help_Label_String;
   Xm_N_List_Item_Count        : constant Xt_N_Resource_String
      := To_Resource_String ("listItemCount");
   Xm_N_List_Items             : constant Xt_N_Resource_String
      := To_Resource_String ("listItems");
   Xm_N_List_Visible_Item_Count :  constant Xt_N_Resource_String
      := To_Resource_String ("listVisibleItemCount");
   Xm_N_Minimize_Buttons       :  constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Minimize_Buttons;
   Xm_N_Must_Match             :  constant Xt_N_Resource_String
      := To_Resource_String ("mustMatch");
   Xm_N_No_Match_Callback      :  constant Xt_N_Resource_String
      := To_Resource_String ("noMatchCallback");
   Xm_N_OK_Callback            :  constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_OK_Callback;
   Xm_N_OK_Label_String        :  constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_OK_Label_String;
   Xm_N_Selection_Label_String :  constant Xt_N_Resource_String
      := To_Resource_String ("selectionLabelString");
   Xm_N_Text_Accelerators      :  constant Xt_N_Resource_String
      := To_Resource_String ("textAccelerators");
   Xm_N_Text_Columns           :  constant Xt_N_Resource_String
      := To_Resource_String ("textColumns");
   Xm_N_Text_String            :  constant Xt_N_Resource_String
      := To_Resource_String ("textString");

end Xm_Widgets.Manager.Bulletin_Board.Selection_Box;
