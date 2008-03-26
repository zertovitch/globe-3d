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

package Xm_Widgets.Primitive.Label.Push_Button is

   Xm_Push_Button_Widget_Class         : constant Widget_Class;
   Xm_Push_Button_Gadget_Class         : constant Widget_Class;


   type Xm_Push_Button_Callback_Struct is record
      Reason      : Callback_Reason;
      Event       : X_Lib.X_Event_Pointer;
      Click_Count : Integer;
   end record;
   pragma Convention (C, Xm_Push_Button_Callback_Struct);

   type Xm_Push_Button_Callback_Struct_Access is
      access all Xm_Push_Button_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Push_Button_Callback_Struct_Access;


   function Xm_Is_Push_Button (W : in Widget) return Boolean;
   function Xm_Is_Push_Button_Gadget (W : in Widget) return Boolean;


   function Xm_Create_Push_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Push_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Activate_Callback       : constant Xt_N_Resource_String;
   Xm_N_Arm_Callback            : constant Xt_N_Resource_String;
   Xm_N_Arm_Color               : constant Xt_N_Resource_String;
   Xm_N_Arm_Pixmap              : constant Xt_N_Resource_String;
   Xm_N_Default_Button_Shadow_Thickness : constant Xt_N_Resource_String;
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String;
   Xm_N_Fill_On_Arm             : constant Xt_N_Resource_String;
   Xm_N_Multi_Click             : constant Xt_N_Resource_String;
   Xm_N_Show_As_Default         : constant Xt_N_Resource_String;


private

   c_const_Xm_Push_Button_Widget_Class         : Widget_Class;
   c_const_Xm_Push_Button_Gadget_Class         : Widget_Class;

   pragma Import (C, c_const_Xm_Push_Button_Widget_Class, "xmPushButtonWidgetClass");
   pragma Import (C, c_const_Xm_Push_Button_Gadget_Class, "xmPushButtonGadgetClass");

   Xm_Push_Button_Widget_Class         : constant Widget_Class :=
    c_const_Xm_Push_Button_Widget_Class;
   Xm_Push_Button_Gadget_Class         : constant Widget_Class :=
    c_const_Xm_Push_Button_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Activate_Callback       : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Activate_Callback;
   Xm_N_Arm_Callback            : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Arm_Callback;
   Xm_N_Arm_Color               : constant Xt_N_Resource_String :=
      To_Resource_String ("armColor");
   Xm_N_Arm_Pixmap              : constant Xt_N_Resource_String :=
      To_Resource_String ("armPixmap");
   Xm_N_Default_Button_Shadow_Thickness : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultButtonShadowThickness");
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Disarm_Callback;
   Xm_N_Fill_On_Arm             : constant Xt_N_Resource_String :=
      To_Resource_String ("fillOnArm");
   Xm_N_Multi_Click             : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Multi_Click;
   Xm_N_Show_As_Default         : constant Xt_N_Resource_String :=
      To_Resource_String ("showAsDefault");

end Xm_Widgets.Primitive.Label.Push_Button;
