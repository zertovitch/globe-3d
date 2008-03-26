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
--          28 Jan 2002 V. Godunko: mark Xm_Main_Window_SepX and
--                                  Xm_Main_Window_Set_Areas as obsolete for
--                                  Motif 2.x, add Separator_X_Name strings
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Scrolled_Window.Main_Window is
 
   Xm_Main_Window_Widget_Class         : constant Widget_Class;


   function Xm_Is_Main_Window (W : in Widget) return Boolean;


   function Xm_Create_Main_Window
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   --  Xm_Main_Window_SepX is obsolete in Motif 2.x,
   --  use Xt_Name_To_Widget instead
   --
   function Xm_Main_Window_Sep1 (W : in Widget) return Widget;

   function Xm_Main_Window_Sep2 (W : in Widget) return Widget;

   function Xm_Main_Window_Sep3 (W : in Widget) return Widget;

   Separator_1_Name : constant String := "Separator1";
   Separator_2_Name : constant String := "Separator2";
   Separator_3_Name : constant String := "Separator3";
 

   -- obsolete in Motif 2.x
   --
   procedure Xm_Main_Window_Set_Areas
     (W                     : in Widget;
      Menu_Bar              : in Widget;
      Command_Window        : in Widget;
      Horizontal_Scrollbar  : in Widget;
      Vertical_Scrollbar    : in Widget;
      Work_Region           : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Command_Window          : constant Xt_N_Resource_String;
   Xm_N_Command_Window_Location : constant Xt_N_Resource_String;

   -- for Xm_N_Command_Window_Location
   --
   type Command_Window_Location_Type is (Above_Workspace, Below_Workspace);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Command_Window_Location_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Command_Window_Location_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Main_Window_Margin_Height : constant Xt_N_Resource_String;
   Xm_N_Main_Window_Margin_Width  : constant Xt_N_Resource_String;
   Xm_N_Menu_Bar                : constant Xt_N_Resource_String;
   Xm_N_Message_Window          : constant Xt_N_Resource_String;
   Xm_N_Show_Separator          : constant Xt_N_Resource_String;

-- UseMotif2.0 Motif2.1
   -- extension of the contraint resource "child type" for children of
   -- scrolled window widget class

   Menu_Bar       : constant Scrolled_Window_Child_Type;
   Command_Window : constant Scrolled_Window_Child_Type;
   Separator      : constant Scrolled_Window_Child_Type;
   Message_Window : constant Scrolled_Window_Child_Type;

-- EndMotif2.0 Motif2.1

private

   for Command_Window_Location_Type use
     (Above_Workspace => 0, Below_Workspace => 1);
   for Command_Window_Location_Type'Size use Interfaces.C.unsigned_char'Size;

   pragma Import (C, Xm_Main_Window_Sep1, "XmMainWindowSep1");
   pragma Import (C, Xm_Main_Window_Sep2, "XmMainWindowSep2");
   pragma Import (C, Xm_Main_Window_Sep3, "XmMainWindowSep3");
   pragma Import (C, Xm_Main_Window_Set_Areas, "XmMainWindowSetAreas");


   c_const_Xm_Main_Window_Widget_Class         : Widget_Class;

   pragma Import (C, c_const_Xm_Main_Window_Widget_Class, "xmMainWindowWidgetClass");

   Xm_Main_Window_Widget_Class         : constant Widget_Class :=
    c_const_Xm_Main_Window_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Command_Window          : constant Xt_N_Resource_String :=
      To_Resource_String ("commandWindow");
   Xm_N_Command_Window_Location : constant Xt_N_Resource_String :=
      To_Resource_String ("commandWindowLocation");

   Xm_N_Main_Window_Margin_Height    : constant Xt_N_Resource_String :=
      To_Resource_String ("mainWindowMarginHeight");
   Xm_N_Main_Window_Margin_Width     : constant Xt_N_Resource_String :=
      To_Resource_String ("mainWindowMarginWidth");
   Xm_N_Menu_Bar                : constant Xt_N_Resource_String :=
      To_Resource_String ("menuBar");
   Xm_N_Message_Window          : constant Xt_N_Resource_String :=
      To_Resource_String ("messageWindow");
   Xm_N_Show_Separator          : constant Xt_N_Resource_String :=
      To_Resource_String ("showSeparator");

-- UseMotif2.0 Motif2.1
   Menu_Bar       : constant Scrolled_Window_Child_Type := 1;
   Command_Window : constant Scrolled_Window_Child_Type := 4;
   Separator      : constant Scrolled_Window_Child_Type := 5;
   Message_Window : constant Scrolled_Window_Child_Type := 6;
-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Scrolled_Window.Main_Window;
