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

package Xm_Widgets.Primitive.Label.Cascade_Button is

   Xm_Cascade_Button_Widget_Class      : constant Widget_Class;
   Xm_Cascade_Button_Gadget_Class      : constant Widget_Class;


   function Xm_Is_Cascade_Button (W : in Widget) return Boolean;
   function Xm_Is_Cascade_Button_Gadget (W : in Widget) return Boolean;

   function Xm_Create_Cascade_Button
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Cascade_Button_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   procedure Xm_Cascade_Button_Highlight
     (Button    : in Widget;
      Highlight : in Boolean);

   procedure Xm_Cascade_Button_Gadget_Highlight
     (Button    : in Widget;
      Highlight : in Boolean);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Activate_Callback       : constant Xt_N_Resource_String;
   Xm_N_Cascade_Pixmap          : constant Xt_N_Resource_String;
   Xm_N_Cascading_Callback      : constant Xt_N_Resource_String;
   Xm_N_Mapping_Delay           : constant Xt_N_Resource_String;
   Xm_N_Sub_Menu_Id             : constant Xt_N_Resource_String;

private

   c_const_Xm_Cascade_Button_Widget_Class      : Widget_Class;
   c_const_Xm_Cascade_Button_Gadget_Class      : Widget_Class;

   pragma Import (C, c_const_Xm_Cascade_Button_Widget_Class, "xmCascadeButtonWidgetClass");
   pragma Import (C, c_const_Xm_Cascade_Button_Gadget_Class, "xmCascadeButtonGadgetClass");

   Xm_Cascade_Button_Widget_Class      : constant Widget_Class :=
    c_const_Xm_Cascade_Button_Widget_Class;
   Xm_Cascade_Button_Gadget_Class      : constant Widget_Class :=
    c_const_Xm_Cascade_Button_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Activate_Callback       : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Activate_Callback;
   Xm_N_Cascade_Pixmap          : constant Xt_N_Resource_String :=
      To_Resource_String ("cascadePixmap");
   Xm_N_Cascading_Callback      : constant Xt_N_Resource_String :=
      To_Resource_String ("cascadingCallback");
   Xm_N_Mapping_Delay           : constant Xt_N_Resource_String :=
      To_Resource_String ("mappingDelay");
   Xm_N_Sub_Menu_Id             : constant Xt_N_Resource_String :=
      To_Resource_String ("subMenuId");

end Xm_Widgets.Primitive.Label.Cascade_Button;
