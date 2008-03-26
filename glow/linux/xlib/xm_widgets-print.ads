-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
--  This file also copyright (c) 2001 Vadim Godunko                          --
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
--          20 Jun 2001 Vadim Godunko: first definition of this file
--          05 Aug 2001 H.-F. Vogt: add print shell widget
--
-------------------------------------------------------------------------------

with X_Lib.Extensions.Print;

package Xm_Widgets.Print is

-- UseMotif2.1

   --  Xm_Print_Shell is a subclass of Xt_Application_Shell
   --
   Xm_Print_Shell_Widget_Class  : constant Widget_Class;


   type Xm_Print_Shell_Callback_Struct is record
      Reason             : Callback_Reason;
      Event              : X_Lib.X_Event_Pointer;
      Print_Context      : X_Lib.Extensions.Print.Xp_Context_ID;
      Last_Page          : Boolean;   --  in/out!
      Detail             : Xt_Pointer;
   end record;
   pragma Convention (C, Xm_Print_Shell_Callback_Struct);

   type Xm_Print_Shell_Callback_Struct_Access is
      access all Xm_Print_Shell_Callback_Struct;

   --  convert a Pointer (Call_Data of a callback function) into a
   --  callback struct access if possible
   --
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Print_Shell_Callback_Struct_Access;

   --  Callback_Reasons for the callback above
   --
   Cr_Start_Job        : constant Callback_Reason := 70;
   Cr_End_Job          : constant Callback_Reason := 71;
   Cr_Page_Setup       : constant Callback_Reason := 72;
   Cr_PDM_None         : constant Callback_Reason := 73;
   Cr_PDM_Up           : constant Callback_Reason := 74;
   Cr_PDM_Start_Error  : constant Callback_Reason := 75;
   Cr_PDM_Start_VxAuth : constant Callback_Reason := 76;
   Cr_PDM_Start_PxAuth : constant Callback_Reason := 77;
   Cr_PDM_OK           : constant Callback_Reason := 78;
   Cr_PDM_Cancel       : constant Callback_Reason := 79;
   Cr_PDM_Exit_Error   : constant Callback_Reason := 80;

   --  Xm_Is_Print_Shell
   --  test if widget belongs to Xm_Print_Shell_Widget of a subclass thereof
   --
   function Xm_Is_Print_Shell (W : in Widget) return Boolean;


   --  Xm_Print_Setup
   --  create a print shell widget
   --
   function Xm_Print_Setup
     (Video_Widget     : in Widget;
      Print_Screen     : in X_Lib.Screen_Pointer;
      Print_Shell_Name : in String;
      Arglist          : in Arg_List := Null_Arg_List)
      return Widget;

   File_Error : exception;

   --  Xm_Print_To_File
   --  Retrieves and saves data that would normally be printed
   --  by the X Print Server
   --
   procedure Xm_Print_To_File
     (Display     : in X_Lib.Display_Pointer;
      File_Name   : in String;
      Finish_Proc : in X_Lib.Extensions.Print.Xp_Finish_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer);

   --  Xm_Print_Popup_PDM
   --  Send a notification for the Print Dialog Manager to be popped up
   --  (definition in advance of usage)
   --
   procedure Xm_Print_Popup_PDM
     (Print_Shell         : in Widget;
      Video_Transient_For : in Widget);
   pragma Import (C, Xm_Print_Popup_PDM, "XmPrintPopupPDM");

   --  Xm_Redisplay_Widget
   --  Synchronously activates the expose method of a widget to draw
   --  its content
   --
   procedure Xm_Redisplay_Widget (W : in Widget);

   Xm_N_Start_Job_Callback        : constant Xt_N_Resource_String;
   Xm_N_End_Job_Callback          : constant Xt_N_Resource_String;
   Xm_N_Page_Setup_Callback       : constant Xt_N_Resource_String;
   Xm_N_Min_X                     : constant Xt_N_Resource_String;
   Xm_N_Min_Y                     : constant Xt_N_Resource_String;
   Xm_N_Max_X                     : constant Xt_N_Resource_String;
   Xm_N_Max_Y                     : constant Xt_N_Resource_String;
   Xm_N_Default_Pixmap_Resolution : constant Xt_N_Resource_String;
   Xm_N_Pdm_Notification_Callback : constant Xt_N_Resource_String;

private

   c_const_Xm_Print_Shell_Widget_Class : Widget_Class;
   pragma Import (C, c_const_Xm_Print_Shell_Widget_Class, "xmPrintShellWidgetClass");

   Xm_Print_Shell_Widget_Class         : constant Widget_Class
      := c_const_Xm_Print_Shell_Widget_Class;

   Xm_N_Start_Job_Callback        : constant Xt_N_Resource_String :=
      To_Resource_String ("startJobCallback");
   Xm_N_End_Job_Callback          : constant Xt_N_Resource_String :=
      To_Resource_String ("endJobCallback");
   Xm_N_Page_Setup_Callback       : constant Xt_N_Resource_String :=
      To_Resource_String ("pageSetupCallback");
   Xm_N_Min_X                     : constant Xt_N_Resource_String :=
      To_Resource_String ("minX");
   Xm_N_Min_Y                     : constant Xt_N_Resource_String :=
      To_Resource_String ("minY");
   Xm_N_Max_X                     : constant Xt_N_Resource_String :=
      To_Resource_String ("maxX");
   Xm_N_Max_Y                     : constant Xt_N_Resource_String :=
      To_Resource_String ("maxY");
   Xm_N_Default_Pixmap_Resolution : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultPixmapResolution");
   Xm_N_Pdm_Notification_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("pdmNotificationCallback");

   pragma Import (C, Xm_Redisplay_Widget, "XmRedisplayWidget");

-- EndMotif2.1

end Xm_Widgets.Print;
