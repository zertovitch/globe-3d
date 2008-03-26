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
--          August 22, 1998 added Resource Strings Client_Leader, Urgency and
--                          Window_Role,
--                          removed Button_Font_List, Default_Font_List,
--                          Label_Font_List, Delete_Response,
--                          Keyboard_Focus_Policy, Mwm_Decorations,
--                          Mwm_Functions, Mwm_Input_Mode, Mwm_Menu,
--                          Shell_Unit_Type, Use_Async_Geometry
--          November 8, 1998 HFVogt: added X11R6.3 conditional defines
--
-------------------------------------------------------------------------------

package X_Toolkit.Shell is

-- ----------------------------------------------------------------------------
--
--  Predefined Shell Widget Classes
--    

   Shell_Widget_Class              : constant Widget_Class;
   Override_Shell_Widget_Class     : constant Widget_Class;
   WM_Shell_Widget_Class           : constant Widget_Class;
   Transient_Shell_Widget_Class    : constant Widget_Class;
   Top_Level_Shell_Widget_Class    : constant Widget_Class;
   Application_Shell_Widget_Class  : constant Widget_Class;
-- UseX11R6 X11R6.3
   Session_Shell_Widget_Class      : constant Widget_Class;
-- EndX11R6 X11R6.3
   
--
-- WM_Shell_Widget_Class and VENDOR_Shell_Widget_Class 
-- should NOT be used by toolbuilders in general
--

   Vendor_Shell_Widget_Class     : constant Widget_Class;


   -- -------------------------------------------------------------------------
   --
   --  resource strings for all shell widgets
   --
   Xt_N_Allow_Shell_Resize  : constant Xt_N_Resource_String;
   Xt_N_Create_Popup_Child_Proc  : constant Xt_N_Resource_String;
   Xt_N_Geometry            : constant Xt_N_Resource_String;
   Xt_N_Override_Redirect   : constant Xt_N_Resource_String;
   Xt_N_Popdown_Callback    : constant Xt_N_Resource_String;
   Xt_N_Popup_Callback      : constant Xt_N_Resource_String;
   Xt_N_Save_Under          : constant Xt_N_Resource_String;
   Xt_N_Visual              : constant Xt_N_Resource_String;

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all WM shell widgets
   --
   Xt_N_Base_Height         : constant Xt_N_Resource_String;
   Xt_N_Base_Width          : constant Xt_N_Resource_String;
-- UseX11R6 X11R6.3
   Xt_N_Client_Leader       : constant Xt_N_Resource_String;
-- EndX11R6 X11R6.3
   Xt_N_Height_Inc          : constant Xt_N_Resource_String;
   Xt_N_Icon_Mask           : constant Xt_N_Resource_String;
   Xt_N_Icon_Pixmap         : constant Xt_N_Resource_String;
   Xt_N_Icon_Window         : constant Xt_N_Resource_String;
   Xt_N_Icon_X              : constant Xt_N_Resource_String;
   Xt_N_Icon_Y              : constant Xt_N_Resource_String;
   Xt_N_Initial_State       : constant Xt_N_Resource_String;
   Xt_N_Input               : constant Xt_N_Resource_String;
   Xt_N_Max_Aspect_X        : constant Xt_N_Resource_String;
   Xt_N_Max_Aspect_Y        : constant Xt_N_Resource_String;
   Xt_N_Max_Height          : constant Xt_N_Resource_String;
   Xt_N_Max_Width           : constant Xt_N_Resource_String;
   Xt_N_Min_Aspect_X        : constant Xt_N_Resource_String;
   Xt_N_Min_Aspect_Y        : constant Xt_N_Resource_String;
   Xt_N_Min_Height          : constant Xt_N_Resource_String;
   Xt_N_Min_Width           : constant Xt_N_Resource_String;
   Xt_N_Title               : constant Xt_N_Resource_String;
   Xt_N_Title_Encoding      : constant Xt_N_Resource_String;
   Xt_N_Transient           : constant Xt_N_Resource_String;
-- UseX11R6 X11R6.3
   Xt_N_Urgency             : constant Xt_N_Resource_String;
-- EndX11R6 X11R6.3
   Xt_N_Wait_For_WM         : constant Xt_N_Resource_String;
   Xt_N_Width_Inc           : constant Xt_N_Resource_String;
   Xt_N_Win_Gravity         : constant Xt_N_Resource_String;
   Xt_N_Window_Group        : constant Xt_N_Resource_String;
-- UseX11R6 X11R6.3
   Xt_N_Window_Role         : constant Xt_N_Resource_String;
-- EndX11R6 X11R6.3
   Xt_N_WM_Timeout          : constant Xt_N_Resource_String;

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all transient shell widgets
   --
   Xt_N_Transient_For       : constant Xt_N_Resource_String;

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all top level shell widgets
   --
   Xt_N_Icon_Name           : constant Xt_N_Resource_String;
   Xt_N_Icon_Name_Encoding  : constant Xt_N_Resource_String;
   Xt_N_Iconic              : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   --  resource strings for all application shell widgets
   --
   Xt_N_Argc                : constant Xt_N_Resource_String;
   Xt_N_Argv                : constant Xt_N_Resource_String;


-- UseX11R6 X11R6.3
   -- -------------------------------------------------------------------------
   --
   --  resource strings for session shell widgets
   --
   Xt_N_Cancel_Callback     : constant Xt_N_Resource_String;
   Xt_N_Clone_Command       : constant Xt_N_Resource_String;
   Xt_N_Connection          : constant Xt_N_Resource_String;
   Xt_N_Current_Directory   : constant Xt_N_Resource_String;
   Xt_N_Die_Callback        : constant Xt_N_Resource_String;
   Xt_N_Discard_Command     : constant Xt_N_Resource_String;
   Xt_N_Environment         : constant Xt_N_Resource_String;
   Xt_N_Error_Callback      : constant Xt_N_Resource_String;
   Xt_N_Interact_Callback   : constant Xt_N_Resource_String;
   Xt_N_Join_Session        : constant Xt_N_Resource_String;
   Xt_N_Program_Path        : constant Xt_N_Resource_String;
   Xt_N_Resign_Command      : constant Xt_N_Resource_String;
   Xt_N_Restart_Command     : constant Xt_N_Resource_String;
   Xt_N_Restart_Style       : constant Xt_N_Resource_String;
   Xt_N_Save_Callback       : constant Xt_N_Resource_String;
   Xt_N_Save_Complete_Callback : constant Xt_N_Resource_String;
   Xt_N_Session_ID          : constant Xt_N_Resource_String;
   Xt_N_Shutdown_Command    : constant Xt_N_Resource_String;
-- EndX11R6 X11R6.3


private

   c_const_Shell_Widget_Class              : Widget_Class;
   c_const_Override_Shell_Widget_Class     : Widget_Class;
   c_const_WM_Shell_Widget_Class           : Widget_Class;
   c_const_Transient_Shell_Widget_Class    : Widget_Class;
   c_const_Top_Level_Shell_Widget_Class    : Widget_Class;
   c_const_Application_Shell_Widget_Class  : Widget_Class;
-- UseX11R6 X11R6.3
   c_const_Session_Shell_Widget_Class      : Widget_Class;
-- EndX11R6 X11R6.3
   c_const_Vendor_Shell_Widget_Class       : Widget_Class;

   pragma Import (C, c_const_Shell_Widget_Class, "shellWidgetClass");
   pragma Import (C, c_const_Override_Shell_Widget_Class, "overrideShellWidgetClass");
   pragma Import (C, c_const_WM_Shell_Widget_Class, "wmShellWidgetClass");
   pragma Import (C, c_const_Transient_Shell_Widget_Class, "transientShellWidgetClass");
   pragma Import (C, c_const_Top_Level_Shell_Widget_Class, "topLevelShellWidgetClass");
   pragma Import (C, c_const_Application_Shell_Widget_Class, "applicationShellWidgetClass");
-- UseX11R6 X11R6.3
   pragma Import (C, c_const_Session_Shell_Widget_Class, "sessionShellWidgetClass");
-- EndX11R6 X11R6.3
   pragma Import (C, c_const_Vendor_Shell_Widget_Class, "vendorShellWidgetClass");

   Shell_Widget_Class              : constant Widget_Class := c_const_Shell_Widget_Class;
   Override_Shell_Widget_Class     : constant Widget_Class := c_const_Override_Shell_Widget_Class;
   WM_Shell_Widget_Class           : constant Widget_Class := c_const_WM_Shell_Widget_Class;
   Transient_Shell_Widget_Class    : constant Widget_Class := c_const_Transient_Shell_Widget_Class;
   Top_Level_Shell_Widget_Class    : constant Widget_Class := c_const_Top_Level_Shell_Widget_Class;
   Application_Shell_Widget_Class  : constant Widget_Class := c_const_Application_Shell_Widget_Class;
-- UseX11R6 X11R6.3
   Session_Shell_Widget_Class      : constant Widget_Class := c_const_Session_Shell_Widget_Class;
-- EndX11R6 X11R6.3
   Vendor_Shell_Widget_Class       : constant Widget_Class := c_const_Vendor_Shell_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   --  resource strings for all shell widgets
   --
   Xt_N_Allow_Shell_Resize  : constant Xt_N_Resource_String
                            := To_Resource_String ("allowShellResize");
   Xt_N_Create_Popup_Child_Proc  : constant Xt_N_Resource_String
                                 := To_Resource_String ("createPopupChildProc");
   Xt_N_Geometry            : constant Xt_N_Resource_String
                            := To_Resource_String ("geometry");
   Xt_N_Override_Redirect   : constant Xt_N_Resource_String
                            := To_Resource_String ("overrideRedirect");
   Xt_N_Popdown_Callback    : constant Xt_N_Resource_String
                            := To_Resource_String ("popdownCallback");
   Xt_N_Popup_Callback      : constant Xt_N_Resource_String
                            := To_Resource_String ("popupCallback");
   Xt_N_Save_Under          : constant Xt_N_Resource_String
                            := To_Resource_String ("saveUnder");
   Xt_N_Visual              : constant Xt_N_Resource_String
                            := To_Resource_String ("visual");

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all WM shell widgets
   --
   Xt_N_Base_Height         : constant Xt_N_Resource_String
                            := To_Resource_String ("baseHeight");
   Xt_N_Base_Width          : constant Xt_N_Resource_String
                            := To_Resource_String ("baseWidth");
-- UseX11R6 X11R6.3
   Xt_N_Client_Leader       : constant Xt_N_Resource_String
                            := To_Resource_String ("clientLeader");
-- EndX11R6 X11R6.3
   Xt_N_Height_Inc          : constant Xt_N_Resource_String
                            := To_Resource_String ("heightInc");
   Xt_N_Icon_Mask           : constant Xt_N_Resource_String
                            := To_Resource_String ("iconMask");
   Xt_N_Icon_Pixmap         : constant Xt_N_Resource_String
                            := To_Resource_String ("iconPixmap");
   Xt_N_Icon_Window         : constant Xt_N_Resource_String
                            := To_Resource_String ("iconWindow");
   Xt_N_Icon_X              : constant Xt_N_Resource_String
                            := To_Resource_String ("iconX");
   Xt_N_Icon_Y              : constant Xt_N_Resource_String
                            := To_Resource_String ("iconY");
   Xt_N_Initial_State       : constant Xt_N_Resource_String
                            := To_Resource_String ("initialState");
   Xt_N_Input               : constant Xt_N_Resource_String
                            := To_Resource_String ("input");
   Xt_N_Max_Aspect_X        : constant Xt_N_Resource_String
                            := To_Resource_String ("maxAspectX");
   Xt_N_Max_Aspect_Y        : constant Xt_N_Resource_String
                            := To_Resource_String ("maxAspectY");
   Xt_N_Max_Height          : constant Xt_N_Resource_String
                            := To_Resource_String ("maxHeight");
   Xt_N_Max_Width           : constant Xt_N_Resource_String
                            := To_Resource_String ("maxWidth");
   Xt_N_Min_Aspect_X        : constant Xt_N_Resource_String
                            := To_Resource_String ("minAspectX");
   Xt_N_Min_Aspect_Y        : constant Xt_N_Resource_String
                            := To_Resource_String ("minAspectY");
   Xt_N_Min_Height          : constant Xt_N_Resource_String
                            := To_Resource_String ("minHeight");
   Xt_N_Min_Width           : constant Xt_N_Resource_String
                            := To_Resource_String ("minWidth");
   Xt_N_Title               : constant Xt_N_Resource_String
                            := To_Resource_String ("title");
   Xt_N_Title_Encoding      : constant Xt_N_Resource_String
                            := To_Resource_String ("titleEncoding");
   Xt_N_Transient           : constant Xt_N_Resource_String
                            := To_Resource_String ("transient");
-- UseX11R6 X11R6.3
   Xt_N_Urgency             : constant Xt_N_Resource_String
                            := To_Resource_String ("urgency");
-- EndX11R6 X11R6.3
   Xt_N_Wait_For_WM         : constant Xt_N_Resource_String
                            := To_Resource_String ("waitforwm");
   Xt_N_Width_Inc           : constant Xt_N_Resource_String
                            := To_Resource_String ("widthInc");
   Xt_N_Win_Gravity         : constant Xt_N_Resource_String
                            := To_Resource_String ("winGravity");
   Xt_N_Window_Group        : constant Xt_N_Resource_String
                            := To_Resource_String ("windowGroup");
-- UseX11R6 X11R6.3
   Xt_N_Window_Role         : constant Xt_N_Resource_String
                            := To_Resource_String ("windowRole");
-- EndX11R6 X11R6.3
   Xt_N_WM_Timeout          : constant Xt_N_Resource_String
                            := To_Resource_String ("wmTimeout");

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all transient shell widgets
   --
   Xt_N_Transient_For       : constant Xt_N_Resource_String
                            := To_Resource_String ("transientFor");

   -- -------------------------------------------------------------------------
   --
   --  resource strings for all top level shell widgets
   --
   Xt_N_Icon_Name           : constant Xt_N_Resource_String
                            := To_Resource_String ("iconName");
   Xt_N_Icon_Name_Encoding  : constant Xt_N_Resource_String
                            := To_Resource_String ("iconNameEncoding");
   Xt_N_Iconic              : constant Xt_N_Resource_String
                            := To_Resource_String ("iconic");


   -- -------------------------------------------------------------------------
   --
   --  resource strings for all application shell widgets
   --
   Xt_N_Argc                : constant Xt_N_Resource_String
                            := To_Resource_String ("argc");
   Xt_N_Argv                : constant Xt_N_Resource_String
                            := To_Resource_String ("argv");


-- UseX11R6 X11R6.3
   -- -------------------------------------------------------------------------
   --
   --  resource strings for session shell widgets
   --
   Xt_N_Cancel_Callback     : constant Xt_N_Resource_String
                            := To_Resource_String ("cancelCallback");
   Xt_N_Clone_Command       : constant Xt_N_Resource_String
                            := To_Resource_String ("cloneCommand");
   Xt_N_Connection          : constant Xt_N_Resource_String
                            := To_Resource_String ("connection");
   Xt_N_Current_Directory   : constant Xt_N_Resource_String
                            := To_Resource_String ("currentDirectory");
   Xt_N_Die_Callback        : constant Xt_N_Resource_String
                            := To_Resource_String ("dieCallback");
   Xt_N_Discard_Command     : constant Xt_N_Resource_String
                            := To_Resource_String ("discardCommand");
   Xt_N_Environment         : constant Xt_N_Resource_String
                            := To_Resource_String ("environment");
   Xt_N_Error_Callback      : constant Xt_N_Resource_String
                            := To_Resource_String ("errorCallback");
   Xt_N_Interact_Callback   : constant Xt_N_Resource_String
                            := To_Resource_String ("interactCallback");
   Xt_N_Join_Session        : constant Xt_N_Resource_String
                            := To_Resource_String ("joinSession");
   Xt_N_Program_Path        : constant Xt_N_Resource_String
                            := To_Resource_String ("programPath");
   Xt_N_Resign_Command      : constant Xt_N_Resource_String
                            := To_Resource_String ("resignCommand");
   Xt_N_Restart_Command     : constant Xt_N_Resource_String
                            := To_Resource_String ("restartCommand");
   Xt_N_Restart_Style       : constant Xt_N_Resource_String
                            := To_Resource_String ("restartStyle");
   Xt_N_Save_Callback       : constant Xt_N_Resource_String
                            := To_Resource_String ("saveCallback");
   Xt_N_Save_Complete_Callback : constant Xt_N_Resource_String
                               := To_Resource_String ("saveCompleteCallback");
   Xt_N_Session_ID          : constant Xt_N_Resource_String
                            := To_Resource_String ("sessionID");
   Xt_N_Shutdown_Command    : constant Xt_N_Resource_String
                            := To_Resource_String ("shutdownCommand");
-- EndX11R6 X11R6.3

end X_Toolkit.Shell;
