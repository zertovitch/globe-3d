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
--          19 May 2001 Vadim Godunko: definition of type Popup_Type and
--                                     Append_Set/Append_Get routines
--          06 Aug 2001 H.-F. Vogt: move Xm_Add_To_Post_From_List from
--                                  Xm_Widgets to here
--                                  add Xm_Remove_From_Post_From_List
--          20 Jan 2002 H.-F. Vogt: Orientation_Type was moved to Xm_Widgets
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Row_Column is

   use type Interfaces.C.short;

   Xm_Row_Column_Widget_Class          : constant Widget_Class;


   type Xm_Row_Column_Callback_Struct is record
      Reason         : Callback_Reason;
      Event          : X_Lib.X_Event_Pointer;
      Wid            : Widget;
      Data           : X_Strings.X_String;
      Callbackstruct : X_Strings.X_String;
   end record;
   pragma Convention (C, Xm_Row_Column_Callback_Struct);

   type Xm_Row_Column_Callback_Struct_Access is
      access all Xm_Row_Column_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Row_Column_Callback_Struct_Access;



   function Xm_Is_Row_Column (W: in Widget) return Boolean;


   function Xm_Create_Row_Column
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Work_Area
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Radio_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Option_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Menu_Bar
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Popup_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Create_Pulldown_Menu
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Option_Label_Gadget (W : in Widget) return Widget;

   function Xm_Option_Button_Gadget (W : in Widget) return Widget;

   procedure Xm_Menu_Position
     (P     : in Widget;
      Event : in X_Lib.X_Event_Pointer);


   procedure Xm_Add_To_Post_From_List
     (Menu             : in Widget;
      Post_From_Widget : in Widget);

   procedure Xm_Remove_From_Post_From_List
     (Menu             : in Widget;
      Post_From_Widget : in Widget);

   function Xm_Get_Posted_From_Widget (W : in Widget) return Widget;
   
   function Xm_Get_Tear_Off_Control (W : in Widget) return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource strings
   --

   -- layout classes

   Xm_N_Adjust_Last            : constant Xt_N_Resource_String;
   Xm_N_Adjust_Margin          : constant Xt_N_Resource_String;
   Xm_N_Entry_Alignment        : constant Xt_N_Resource_String;
   Xm_N_Entry_Border           : constant Xt_N_Resource_String;
   Xm_N_Entry_Vertical_Alignment : constant Xt_N_Resource_String;
   Xm_N_Is_Aligned             : constant Xt_N_Resource_String;
   Xm_N_Margin_Height          : constant Xt_N_Resource_String;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String;
   Xm_N_Num_Columns            : constant Xt_N_Resource_String;

   --  use Xm_Widgets.Orientation_Type for this
   --
   Xm_N_Orientation            : constant Xt_N_Resource_String;

   Xm_N_Packing                : constant Xt_N_Resource_String;

   type Packing_Type is (No_Packing, Pack_Tight, Pack_Column, Pack_None);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Packing_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Packing_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Resize_Height          : constant Xt_N_Resource_String;
   Xm_N_Resize_Width           : constant Xt_N_Resource_String;
   Xm_N_Spacing                : constant Xt_N_Resource_String;

 
   -- non-layout

   Xm_N_Entry_Callback         : constant Xt_N_Resource_String;
   Xm_N_Entry_Class            : constant Xt_N_Resource_String;
   Xm_N_Is_Homogenous          : constant Xt_N_Resource_String;
   Xm_N_Label_String           : constant Xt_N_Resource_String;
   Xm_N_Map_Callback           : constant Xt_N_Resource_String;
   Xm_N_Menu_Accelerator       : constant Xt_N_Resource_String;
   Xm_N_Menu_Help_Widget       : constant Xt_N_Resource_String;
   Xm_N_Menu_History           : constant Xt_N_Resource_String;
   Xm_N_Menu_Post              : constant Xt_N_Resource_String;
   Xm_N_Mnemonic               : constant Xt_N_Resource_String;
   Xm_N_Mnemonic_Char_Set      : constant Xt_N_Resource_String;
   Xm_N_Popup_Enabled          : constant Xt_N_Resource_String;

   --  in Motif prior to 2.0, Xm_N_Popup_Enabled is a Boolean variable
   --  for Ease of usage, we define here nevertheless a different type,
   --  consistent with Motif 2.0+
   --
-- UseMotif2.0 Motif2.1
   type Popup_Type is (Disabled, Keyboard, Automatic, Automatic_Recursive);
-- NotMotif2.0 Motif2.1
--!    type Popup_Type is (Disabled, Keyboard);
-- EndMotif2.0 Motif2.1

   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Popup_Type);

   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Popup_Type);


   Xm_N_Radio_Always_One       : constant Xt_N_Resource_String;
   Xm_N_Radio_Behavior         : constant Xt_N_Resource_String;
   Xm_N_Row_Column_Type        : constant Xt_N_Resource_String;

   type Row_Column_Type is (Work_Area, Menu_Bar, Menu_Pulldown, Menu_Popup, Menu_Option);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Row_Column_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Row_Column_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Sub_Menu_ID            : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Tear_Off_Menu_Activate_Callback : constant Xt_N_Resource_String;
   Xm_N_Tear_Off_Menu_Deactivate_Callback : constant Xt_N_Resource_String;
   Xm_N_Tear_Off_Model         : constant Xt_N_Resource_String;

   type Tear_Off_Model_Type is (Tear_Off_Enabled, Tear_Off_Disabled);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Tear_Off_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Tear_Off_Model_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Tear_Off_Title         : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Unmap_Callback         : constant Xt_N_Resource_String;
   Xm_N_Which_Button           : constant Xt_N_Resource_String;

   --  constraints for children

   Xm_N_Position_Index         : constant Xt_N_Resource_String;

   subtype Position_Index is Interfaces.C.short;
   Last_Position  : constant Position_Index := -1;
   First_Position : constant Position_Index := 0;

private

   for Packing_Type use (No_Packing => 0, Pack_Tight => 1, Pack_Column => 2, Pack_None => 3);
   for Packing_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Popup_Type use (Disabled => 0, Keyboard => 1, Automatic => 2, Automatic_Recursive => 3);
-- NotMotif2.0 Motif2.1
--!    for Popup_Type use (Disabled => 0, Keyboard => 1);
-- EndMotif2.0 Motif2.1
   for Popup_Type'Size use Interfaces.C.unsigned'Size;

   for Row_Column_Type use (Work_Area => 0, Menu_Bar => 1, Menu_Pulldown => 2,
                            Menu_Popup => 3, Menu_Option => 4);
   for Row_Column_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Tear_Off_Model_Type use (Tear_Off_Enabled => 0, Tear_Off_Disabled => 1);
   for Tear_Off_Model_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   pragma Import (C, Xm_Menu_Position, "XmMenuPosition");
   pragma Import (C, Xm_Option_Label_Gadget, "XmOptionLabelGadget");
   pragma Import (C, Xm_Option_Button_Gadget, "XmOptionButtonGadget");
   pragma Import (C, Xm_Add_To_Post_From_List, "XmAddToPostFromList");
   pragma Import (C, Xm_Remove_From_Post_From_List, "XmRemoveFromPostFromList");
   pragma Import (C, Xm_Get_Posted_From_Widget, "XmGetPostedFromWidget");
   pragma Import (C, Xm_Get_Tear_Off_Control, "XmGetTearOffControl");


   c_const_Xm_Row_Column_Widget_Class          : Widget_Class;

   pragma Import (C, c_const_Xm_Row_Column_Widget_Class, "xmRowColumnWidgetClass");

   Xm_Row_Column_Widget_Class          : constant Widget_Class :=
    c_const_Xm_Row_Column_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource strings
   --

   -- layout classes

   Xm_N_Adjust_Last            : constant Xt_N_Resource_String
      := To_Resource_String ("adjustLast");
   Xm_N_Adjust_Margin          : constant Xt_N_Resource_String
      := To_Resource_String ("adjustMargin");
   Xm_N_Entry_Alignment        : constant Xt_N_Resource_String
      := To_Resource_String ("entryAlignment");
   Xm_N_Entry_Border           : constant Xt_N_Resource_String
      := To_Resource_String ("entryBorder");
   Xm_N_Entry_Vertical_Alignment : constant Xt_N_Resource_String
      := To_Resource_String ("entryVerticalAlignment");
   Xm_N_Is_Aligned             : constant Xt_N_Resource_String
      := To_Resource_String ("isAligned");
   Xm_N_Margin_Height          : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Num_Columns            : constant Xt_N_Resource_String
      := To_Resource_String ("numColumns");
   Xm_N_Orientation            : constant Xt_N_Resource_String
      := X_Toolkit.Xt_N_Orientation;

   Xm_N_Packing                : constant Xt_N_Resource_String
      := To_Resource_String ("packing");
   Xm_N_Resize_Height          : constant Xt_N_Resource_String
      := To_Resource_String ("resizeHeight");
   Xm_N_Resize_Width           : constant Xt_N_Resource_String
      := To_Resource_String ("resizeWidth");
   Xm_N_Spacing                : constant Xt_N_Resource_String
      := To_Resource_String ("spacing");

   -- non-layout

   Xm_N_Entry_Callback         : constant Xt_N_Resource_String
      := To_Resource_String ("entryCallback");
   Xm_N_Entry_Class            : constant Xt_N_Resource_String
      := To_Resource_String ("entryClass");
   Xm_N_Is_Homogenous          : constant Xt_N_Resource_String
      := To_Resource_String ("isHomogenous");
   Xm_N_Label_String           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Label_String;
   Xm_N_Map_Callback           : constant Xt_N_Resource_String
      := To_Resource_String ("mapCallback");
   Xm_N_Menu_Accelerator       : constant Xt_N_Resource_String
      := To_Resource_String ("menuAccelerator");
   Xm_N_Menu_Help_Widget       : constant Xt_N_Resource_String
      := To_Resource_String ("menuHelpWidget");
   Xm_N_Menu_History           : constant Xt_N_Resource_String
      := To_Resource_String ("menuHistory");
   Xm_N_Menu_Post              : constant Xt_N_Resource_String
      := To_Resource_String ("menuPost");
   Xm_N_Mnemonic               : constant Xt_N_Resource_String
      := To_Resource_String ("mnemonic");
   Xm_N_Mnemonic_Char_Set      : constant Xt_N_Resource_String
      := To_Resource_String ("mnemonicCharSet");
   Xm_N_Popup_Enabled          : constant Xt_N_Resource_String
      := To_Resource_String ("popupEnabled");
   Xm_N_Radio_Always_One       : constant Xt_N_Resource_String
      := To_Resource_String ("radioAlwaysOne");
   Xm_N_Radio_Behavior         : constant Xt_N_Resource_String
      := To_Resource_String ("radioBehavior");
   Xm_N_Row_Column_Type        : constant Xt_N_Resource_String
      := To_Resource_String ("rowColumnType");

   Xm_N_Sub_Menu_ID            : constant Xt_N_Resource_String
      := To_Resource_String ("subMenuId");
-- UseMotif2.0 Motif2.1
   Xm_N_Tear_Off_Menu_Activate_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("tearOffMenuActivateCallback");
   Xm_N_Tear_Off_Menu_Deactivate_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("tearOffMenuDeactivateCallback");
   Xm_N_Tear_Off_Model         : constant Xt_N_Resource_String
      := To_Resource_String ("tearOffModel");

   Xm_N_Tear_Off_Title         : constant Xt_N_Resource_String
      := To_Resource_String ("tearOffTitle");
-- EndMotif2.0 Motif2.1
   Xm_N_Unmap_Callback         : constant Xt_N_Resource_String
      := To_Resource_String ("unmapCallback");
   Xm_N_Which_Button           : constant Xt_N_Resource_String
      := To_Resource_String ("whichButton");

   Xm_N_Position_Index         : constant Xt_N_Resource_String
      := To_Resource_String ("positionIndex");

end Xm_Widgets.Manager.Row_Column;
