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
--          July 07, 1998 HFVogt: changed type names to Initial_Window_State,
--                                Audible_Warning_Kind
--
-------------------------------------------------------------------------------

with X_lib.Property,
     X_Toolkit.Shell;
package Xm_Widgets.Shell is


-- ----------------------------------------------------------------------------
--
--  constants representing widget classes
--

   Xm_Dialog_Shell_Widget_Class        : constant Widget_Class;
   Xm_Drag_Over_Shell_Widget_Class     : constant Widget_Class;
-- UseMotif2.0 Motif2.1
   Xm_Grab_Shell_Widget_Class          : constant Widget_Class;
-- EndMotif2.0 Motif2.1
   Xm_Menu_Shell_Widget_Class          : constant Widget_Class;
   Xm_Vendor_Shell_Widget_Class        : constant Widget_Class;

-- ----------------------------------------------------------------------------
--
--  convenience functions for creating widgets
--

   function Xm_Create_Dialog_Shell
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

-- UseMotif2.0 Motif2.1
   function Xm_Create_Grab_Shell
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;
-- EndMotif2.0 Motif2.1

   function Xm_Create_Menu_Shell
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   function Xm_Is_Dialog_Shell    (W : in Widget) return Boolean;
   function Xm_Is_Drag_Over_Shell (W : in Widget) return Boolean;
   function Xm_Is_Menu_Shell      (W : in Widget) return Boolean;
-- UseMotif2.0 Motif2.1
   function Xm_Is_Grab_Shell      (W : in Widget) return Boolean;
-- EndMotif2.0 Motif2.1
   function Xm_Is_Vendor_Shell    (W : in Widget) return Boolean;

   function Xm_Is_Motif_WM_Running (Shell : in Widget) return Boolean;


   -- -------------------------------------------------------------------------
   --
   --  new resource strings
   --

   -- -------------------------------------------------------------------------
   --
   --  Shell
   --
   Xm_N_Allow_Shell_Resize      : constant Xt_N_Resource_String;
   Xm_N_Create_Popup_Child_Proc : constant Xt_N_Resource_String;
   Xm_N_Geometry                : constant Xt_N_Resource_String;
   Xm_N_Override_Redirect       : constant Xt_N_Resource_String;
   Xm_N_Popdown_Callback        : constant Xt_N_Resource_String;
   Xm_N_Popup_Callback          : constant Xt_N_Resource_String;
   Xm_N_Save_Under              : constant Xt_N_Resource_String;
   Xm_N_Visual                  : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  Menu Shell
   --
   Xm_N_Button_Font_List       : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Button_Render_Table    : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Default_Font_List      : constant Xt_N_Resource_String;
   Xm_N_Label_Font_List        : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Label_Render_Table     : constant Xt_N_Resource_String;
   Xm_N_Layout_Direction       : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   --  WM Shell
   --
   Xm_N_Base_Height            : constant Xt_N_Resource_String;
   Xm_N_Base_Width             : constant Xt_N_Resource_String;
   Xm_N_Height_Inc             : constant Xt_N_Resource_String;
   Xm_N_Icon_Mask              : constant Xt_N_Resource_String;
   Xm_N_Icon_Pixmap            : constant Xt_N_Resource_String;
   Xm_N_Icon_Window            : constant Xt_N_Resource_String;
   Xm_N_Icon_X                 : constant Xt_N_Resource_String;
   Xm_N_Icon_Y                 : constant Xt_N_Resource_String;

   -- for Xm_N_Initial_State use a value of Initial_State
   --
   Xm_N_Initial_State          : constant Xt_N_Resource_String;

   subtype Initial_Window_State is
      X_Lib.Property.Window_State range X_Lib.Property.Normal .. X_Lib.Property.Iconic;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Initial_Window_State);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Initial_Window_State);
   pragma Convention (C, Append_Get);


   Xm_N_Input                  : constant Xt_N_Resource_String;
   Xm_N_Max_Aspect_X           : constant Xt_N_Resource_String;
   Xm_N_Max_Aspect_Y           : constant Xt_N_Resource_String;
   Xm_N_Max_Height             : constant Xt_N_Resource_String;
   Xm_N_Max_Width              : constant Xt_N_Resource_String;
   Xm_N_Min_Aspect_X           : constant Xt_N_Resource_String;
   Xm_N_Min_Aspect_Y           : constant Xt_N_Resource_String;
   Xm_N_Min_Height             : constant Xt_N_Resource_String;
   Xm_N_Min_Width              : constant Xt_N_Resource_String;
   Xm_N_Title                  : constant Xt_N_Resource_String;
   Xm_N_Title_Encoding         : constant Xt_N_Resource_String;
   Xm_N_Transient              : constant Xt_N_Resource_String;
   Xm_N_Wait_For_WM            : constant Xt_N_Resource_String;
   Xm_N_Width_Inc              : constant Xt_N_Resource_String;
   Xm_N_Win_Gravity            : constant Xt_N_Resource_String;
   Xm_N_Window_Group           : constant Xt_N_Resource_String;
   Xm_N_WM_Timeout             : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  Vendor Shell
   --
   Xm_N_Audible_Warning        : constant Xt_N_Resource_String;

   type Audible_Warning_Kind is (None, Bell);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Audible_Warning_Kind);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Audible_Warning_Kind);
   pragma Convention (C, Append_Get);


   -- Xm_N_Button_Font_List       : constant Xt_N_Resource_String;
   -- Xm_N_Button_Render_Table    : constant Xt_N_Resource_String;
   -- Xm_N_Default_Font_List      : constant Xt_N_Resource_String;
   Xm_N_Delete_Response        : constant Xt_N_Resource_String;

   type Delete_Response is (Destroy, Unmap, Do_Nothing);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Delete_Response);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Delete_Response);
   pragma Convention (C, Append_Get);

   Xm_N_Input_Method           : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Input_Policy           : constant Xt_N_Resource_String;

   type Input_Policy is (Per_Shell, Per_Widget, Inherit_Policy);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Input_Policy);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Input_Policy);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   Xm_N_Keyboard_Focus_Policy  : constant Xt_N_Resource_String;

   type Keyboard_Focus_Policy is (Explicit, Pointer);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Keyboard_Focus_Policy);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Keyboard_Focus_Policy);
   pragma Convention (C, Append_Get);

   -- Xm_N_Label_Font_List        : constant Xt_N_Resource_String;
   -- Xm_N_Label_Render_Table     : constant Xt_N_Resource_String;
   -- Xm_N_Layout_Direction       : constant Xt_N_Resource_String;
   Xm_N_MWM_Decorations        : constant Xt_N_Resource_String;
   Xm_N_MWM_Functions          : constant Xt_N_Resource_String;
   Xm_N_MWM_Input_Mode         : constant Xt_N_Resource_String;
   Xm_N_MWM_Menu               : constant Xt_N_Resource_String;
   Xm_N_Preedit_Type           : constant Xt_N_Resource_String;
   Xm_N_Shell_Unit_Type        : constant Xt_N_Resource_String;
   Xm_N_Text_Font_List         : constant Xt_N_Resource_String;
   Xm_N_Text_Render_Table      : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Unit_Type              : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Use_Async_Geometry     : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  TransientShell
   --
   Xm_N_Transient_For          : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  TopLevelShell
   --
   Xm_N_Iconic                 : constant Xt_N_Resource_String;
   Xm_N_Icon_Name              : constant Xt_N_Resource_String;
   Xm_N_Icon_Name_Encoding     : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  ApplicationShell
   --
   Xm_N_Argc                   : constant Xt_N_Resource_String;
   Xm_N_Argv                   : constant Xt_N_Resource_String;




private

   for Audible_Warning_Kind use (None => 0, Bell => 1);
   for Audible_Warning_Kind'Size use Interfaces.C.unsigned_char'Size;

   for Delete_Response use (Destroy => 0, Unmap => 1, Do_Nothing => 2);
   for Delete_Response'Size use Interfaces.C.unsigned_char'Size;

   for Keyboard_Focus_Policy use (Explicit => 0, Pointer => 1);
   for Keyboard_Focus_Policy'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Input_Policy use (Per_Shell => 0, Per_Widget => 1, Inherit_Policy => 255);
   for Input_Policy'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   -- a workaround for gnat apparently not (yet) being able to import
   -- constants
   c_const_Xm_Dialog_Shell_Widget_Class        : Widget_Class;
   c_const_Xm_Drag_Over_Shell_Widget_Class     : Widget_Class;
-- UseMotif2.0 Motif2.1
   c_const_Xm_Grab_Shell_Widget_Class          : Widget_Class;
-- EndMotif2.0 Motif2.1
   c_const_Xm_Menu_Shell_Widget_Class          : Widget_Class;
   c_const_Xm_Vendor_Shell_Widget_Class        : Widget_Class;

   pragma Import (C, c_const_Xm_Dialog_Shell_Widget_Class, "xmDialogShellWidgetClass");
   pragma Import (C, c_const_Xm_Drag_Over_Shell_Widget_Class, "xmDragOverShellWidgetClass");
-- UseMotif2.0 Motif2.1
   pragma Import (C, c_const_Xm_Grab_Shell_Widget_Class, "xmGrabShellWidgetClass");
-- EndMotif2.0 Motif2.1
   pragma Import (C, c_const_Xm_Menu_Shell_Widget_Class, "xmMenuShellWidgetClass");
   pragma Import (C, c_const_Xm_Vendor_Shell_Widget_Class, "vendorShellWidgetClass");

   Xm_Dialog_Shell_Widget_Class        : constant Widget_Class :=
    c_const_Xm_Dialog_Shell_Widget_Class;
   Xm_Drag_Over_Shell_Widget_Class     : constant Widget_Class :=
    c_const_Xm_Drag_Over_Shell_Widget_Class;
-- UseMotif2.0 Motif2.1
   Xm_Grab_Shell_Widget_Class          : constant Widget_Class :=
    c_const_Xm_Grab_Shell_Widget_Class;
-- EndMotif2.0 Motif2.1
   Xm_Menu_Shell_Widget_Class          : constant Widget_Class :=
    c_const_Xm_Menu_Shell_Widget_Class;
   Xm_Vendor_Shell_Widget_Class        : constant Widget_Class :=
    c_const_Xm_Vendor_Shell_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   --  resource value strings
   --

   use  X_Toolkit.Shell;

   -- -------------------------------------------------------------------------
   --
   --  Shell
   --
   Xm_N_Allow_Shell_Resize      : constant Xt_N_Resource_String
                                := Xt_N_Allow_Shell_Resize;
   Xm_N_Create_Popup_Child_Proc : constant Xt_N_Resource_String
                                := Xt_N_Create_Popup_Child_Proc;
   Xm_N_Geometry                : constant Xt_N_Resource_String
                                := Xt_N_Geometry;
   Xm_N_Override_Redirect       : constant Xt_N_Resource_String
                                := Xt_N_Override_Redirect;
   Xm_N_Popdown_Callback        : constant Xt_N_Resource_String
                                := Xt_N_Popdown_Callback;
   Xm_N_Popup_Callback          : constant Xt_N_Resource_String
                                := Xt_N_Popup_Callback;
   Xm_N_Save_Under              : constant Xt_N_Resource_String
                                := Xt_N_Save_Under;
   Xm_N_Visual                  : constant Xt_N_Resource_String
                                := Xt_N_Visual;


   -- -------------------------------------------------------------------------
   --
   --  Menu Shell
   --
   Xm_N_Button_Font_List       : constant Xt_N_Resource_String
                               := To_Resource_String ("buttonFontList");
-- UseMotif2.0 Motif2.1
   Xm_N_Button_Render_Table    : constant Xt_N_Resource_String
                               := To_Resource_String ("buttonRenderTable");
-- EndMotif2.0 Motif2.1
   Xm_N_Default_Font_List      : constant Xt_N_Resource_String
                               := To_Resource_String ("defaultFontList");
   Xm_N_Label_Font_List        : constant Xt_N_Resource_String
                               := To_Resource_String ("labelFontList");
-- UseMotif2.0 Motif2.1
   Xm_N_Label_Render_Table     : constant Xt_N_Resource_String
                               := To_Resource_String ("labelRenderTable");
   Xm_N_Layout_Direction       : constant Xt_N_Resource_String
                               := To_Resource_String ("layoutDirection");
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   --  WM Shell
   --
   Xm_N_Base_Height            : constant Xt_N_Resource_String
                               := Xt_N_Base_Height;
   Xm_N_Base_Width             : constant Xt_N_Resource_String
                               := Xt_N_Base_Width;
   Xm_N_Height_Inc             : constant Xt_N_Resource_String
                               := Xt_N_Height_Inc;
   Xm_N_Icon_Mask              : constant Xt_N_Resource_String
                               := Xt_N_Icon_Mask;
   Xm_N_Icon_Pixmap            : constant Xt_N_Resource_String
                               := Xt_N_Icon_Pixmap;
   Xm_N_Icon_Window            : constant Xt_N_Resource_String
                               := Xt_N_Icon_Window;
   Xm_N_Icon_X                 : constant Xt_N_Resource_String
                               := Xt_N_Icon_X;
   Xm_N_Icon_Y                 : constant Xt_N_Resource_String
                               := Xt_N_Icon_Y;
   Xm_N_Initial_State          : constant Xt_N_Resource_String
                               := Xt_N_Initial_State;
   Xm_N_Input                  : constant Xt_N_Resource_String
                               := Xt_N_Input;
   Xm_N_Max_Aspect_X           : constant Xt_N_Resource_String
                               := Xt_N_Max_Aspect_X;
   Xm_N_Max_Aspect_Y           : constant Xt_N_Resource_String
                               := Xt_N_Max_Aspect_Y;
   Xm_N_Max_Height             : constant Xt_N_Resource_String
                               := Xt_N_Max_Height;
   Xm_N_Max_Width              : constant Xt_N_Resource_String
                               := Xt_N_Max_Width;
   Xm_N_Min_Aspect_X           : constant Xt_N_Resource_String
                               := Xt_N_Min_Aspect_X;
   Xm_N_Min_Aspect_Y           : constant Xt_N_Resource_String
                               := Xt_N_Min_Aspect_Y;
   Xm_N_Min_Height             : constant Xt_N_Resource_String
                               := Xt_N_Min_Height;
   Xm_N_Min_Width              : constant Xt_N_Resource_String
                               := Xt_N_Min_Width;
   Xm_N_Title                  : constant Xt_N_Resource_String
                               := Xt_N_Title;
   Xm_N_Title_Encoding         : constant Xt_N_Resource_String
                               := Xt_N_Title_Encoding;
   Xm_N_Transient              : constant Xt_N_Resource_String
                               := Xt_N_Transient;
   Xm_N_Wait_For_WM            : constant Xt_N_Resource_String
                               := Xt_N_Wait_For_WM;
   Xm_N_Width_Inc              : constant Xt_N_Resource_String
                               := Xt_N_Width_Inc;
   Xm_N_Win_Gravity            : constant Xt_N_Resource_String
                               := Xt_N_Win_Gravity;
   Xm_N_Window_Group           : constant Xt_N_Resource_String
                               := Xt_N_Window_Group;
   Xm_N_WM_Timeout             : constant Xt_N_Resource_String
                               := Xt_N_WM_Timeout;


   -- -------------------------------------------------------------------------
   --
   --  VendorShell
   --
   Xm_N_Audible_Warning        : constant Xt_N_Resource_String
                               := To_Resource_String ("audibleWarning");


   -- Xm_N_Button_Font_List       : constant Xt_N_Resource_String := To_Resource_String ("buttonFontList");
   -- Xm_N_Button_Render_Table    : constant Xt_N_Resource_String := To_Resource_String ("buttonRenderTable");
   -- Xm_N_Default_Font_List      : constant Xt_N_Resource_String := To_Resource_String ("defaultFontList");
   Xm_N_Delete_Response        : constant Xt_N_Resource_String
                               := To_Resource_String ("deleteResponse");

   Xm_N_Keyboard_Focus_Policy  : constant Xt_N_Resource_String
                               := To_Resource_String ("keyboardFocusPolicy");

   Xm_N_Input_Method           : constant Xt_N_Resource_String
                               := To_Resource_String ("inputMethod");
-- UseMotif2.0 Motif2.1
   Xm_N_Input_Policy           : constant Xt_N_Resource_String
                               := To_Resource_String ("inputPolicy");

-- EndMotif2.0 Motif2.1

   -- Xm_N_Label_Font_List        : constant Xt_N_Resource_String := To_Resource_String ("labelFontList");
   -- Xm_N_Label_Render_Table     : constant Xt_N_Resource_String := To_Resource_String ("labelRenderTable");
   -- Xm_N_Layout_Direction       : constant Xt_N_Resource_String := To_Resource_String ("layoutDirection");
   Xm_N_MWM_Decorations        : constant Xt_N_Resource_String
                               := To_Resource_String ("mwmDecorations");
   Xm_N_MWM_Functions          : constant Xt_N_Resource_String
                               := To_Resource_String ("mwmFunctions");
   Xm_N_MWM_Input_Mode         : constant Xt_N_Resource_String
                               := To_Resource_String ("mwmInputMode");
   Xm_N_MWM_Menu               : constant Xt_N_Resource_String
                               := To_Resource_String ("mwmMenu");
   Xm_N_Preedit_Type           : constant Xt_N_Resource_String
                               := To_Resource_String ("preeditType");
   Xm_N_Shell_Unit_Type        : constant Xt_N_Resource_String
                               := To_Resource_String ("shellUnitType");
   Xm_N_Text_Font_List         : constant Xt_N_Resource_String
                               := To_Resource_String ("textFontList");
   Xm_N_Text_Render_Table      : constant Xt_N_Resource_String
                               := To_Resource_String ("textRenderTable");
-- UseMotif2.0 Motif2.1
   Xm_N_Unit_Type              : constant Xt_N_Resource_String
                               := Xm_Widgets.Xm_N_Unit_Type;
-- EndMotif2.0 Motif2.1
   Xm_N_Use_Async_Geometry     : constant Xt_N_Resource_String
                               := To_Resource_String ("useAsyncGeometry");


   -- -------------------------------------------------------------------------
   --
   --  TransientShell
   --
   Xm_N_Transient_For          : constant Xt_N_Resource_String
                               := Xt_N_Transient_For;


   -- -------------------------------------------------------------------------
   --
   --  TopLevelShell
   --
   Xm_N_Iconic                 : constant Xt_N_Resource_String
                               := Xt_N_Iconic;
   Xm_N_Icon_Name              : constant Xt_N_Resource_String
                               := Xt_N_Icon_Name;
   Xm_N_Icon_Name_Encoding     : constant Xt_N_Resource_String
                               := Xt_N_Icon_Name_Encoding;


   -- -------------------------------------------------------------------------
   --
   --  ApplicationShell
   --
   Xm_N_Argc                   : constant Xt_N_Resource_String
                               := Xt_N_Argc;
   Xm_N_Argv                   : constant Xt_N_Resource_String
                               := Xt_N_Argv;

end Xm_Widgets.Shell;
