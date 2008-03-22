-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
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
--          06 Aug 2001 Vadim Godunko: move Xm_Change_Color to Xm_Widgets
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--
--  this file contains the classes XmPrimitive  AND  XmGadget
--
-------------------------------------------------------------------------------

with X_Lib;
package Xm_Widgets.Primitive is
 

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Primitive_Widget_Class           : constant Widget_Class;


   function Xm_Is_Primitive (W : in Widget) return Boolean;


   Xm_Gadget_Class                     : constant Widget_Class;


   function Xm_Is_Gadget (W : in Widget) return Boolean;



   -- -------------------------------------------------------------------------
   --
   -- new resource values
   --

   Xm_N_Bottom_Shadow_Color     : constant Xt_N_Resource_String;
   Xm_N_Bottom_Shadow_Pixmap    : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Foreground              : constant Xt_N_Resource_String;
   Xm_N_Help_Callback           : constant Xt_N_Resource_String;
   Xm_N_Highlight_Color         : constant Xt_N_Resource_String;
   Xm_N_Highlight_On_Enter      : constant Xt_N_Resource_String;
   Xm_N_Highlight_Pixmap        : constant Xt_N_Resource_String;
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Layout_Direction        : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1

   Xm_N_Navigation_Type         : constant Xt_N_Resource_String;

   type Navigation_Type is (None,             Tab_Group,
                            Sticky_Tab_Group, Exclusive_Tab_Group);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Navigation_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Navigation_Type);
   pragma Convention (C, Append_Get);


-- UseMotif2.0 Motif2.1
   Xm_N_Popup_Handler_Callback  : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Shadow_Thickness        : constant Xt_N_Resource_String;
   Xm_N_Top_Shadow_Color        : constant Xt_N_Resource_String;
   Xm_N_Top_Shadow_Pixmap       : constant Xt_N_Resource_String;
   Xm_N_Traversal_On            : constant Xt_N_Resource_String;
   Xm_N_Unit_Type               : constant Xt_N_Resource_String;
   Xm_N_User_Data               : constant Xt_N_Resource_String;


   --
   -- for Push_Button, Drawn_Button, Arrow_Button
   --
   type Multi_Click_Type is (Multiclick_Discard, Multiclick_Keep);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Multi_Click_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Multi_Click_Type);
   pragma Convention (C, Append_Get);


private

   for Navigation_Type use (None => 0,             Tab_Group => 1,
                            Sticky_Tab_Group => 2, Exclusive_Tab_Group => 3);
   for Navigation_Type'Size use Interfaces.C.unsigned_char'Size;

   for Multi_Click_Type use (Multiclick_Discard => 0, Multiclick_Keep => 1);
   for Multi_Click_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Primitive_Widget_Class : Widget_Class;
   c_const_Xm_Gadget_Class           : Widget_Class;

   pragma Import (C, c_const_Xm_Primitive_Widget_Class, "xmPrimitiveWidgetClass");
   pragma Import (C, c_const_Xm_Gadget_Class, "xmGadgetClass");

   Xm_Primitive_Widget_Class         : constant Widget_Class :=
    c_const_Xm_Primitive_Widget_Class;
   Xm_Gadget_Class                   : constant Widget_Class :=
    c_const_Xm_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- new resource values
   --

   Xm_N_Bottom_Shadow_Color     : constant Xt_N_Resource_String :=
      To_Resource_String ("bottomShadowColor");
   Xm_N_Bottom_Shadow_Pixmap    : constant Xt_N_Resource_String :=
      To_Resource_String ("bottomShadowPixmap");
-- UseMotif2.0 Motif2.1
   Xm_N_Convert_Callback        : constant Xt_N_Resource_String :=
      To_Resource_String ("convertCallback");
-- EndMotif2.0 Motif2.1
   Xm_N_Foreground              : constant Xt_N_Resource_String :=
      To_Resource_String ("foreground");
   Xm_N_Help_Callback           : constant Xt_N_Resource_String :=
      To_Resource_String ("helpCallback");
   Xm_N_Highlight_Color         : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightColor");
   Xm_N_Highlight_On_Enter      : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightOnEnter");
   Xm_N_Highlight_Pixmap        : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightPixmap");
   Xm_N_Highlight_Thickness     : constant Xt_N_Resource_String :=
      To_Resource_String ("highlightThickness");
-- UseMotif2.0 Motif2.1
   Xm_N_Layout_Direction        : constant Xt_N_Resource_String :=
      To_Resource_String ("layoutDirection");
-- EndMotif2.0 Motif2.1

   Xm_N_Navigation_Type         : constant Xt_N_Resource_String :=
      To_Resource_String ("navigationType");

-- UseMotif2.0 Motif2.1
   Xm_N_Popup_Handler_Callback  : constant Xt_N_Resource_String :=
      To_Resource_String ("popupHandlerCallback");
-- EndMotif2.0 Motif2.1
   Xm_N_Shadow_Thickness        : constant Xt_N_Resource_String :=
      To_Resource_String ("shadowThickness");
   Xm_N_Top_Shadow_Color        : constant Xt_N_Resource_String :=
      To_Resource_String ("topShadowColor");
   Xm_N_Top_Shadow_Pixmap       : constant Xt_N_Resource_String :=
      To_Resource_String ("topShadowPixmap");
   Xm_N_Traversal_On            : constant Xt_N_Resource_String :=
      To_Resource_String ("traversalOn");
   Xm_N_Unit_Type               : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Unit_Type;
   Xm_N_User_Data               : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_User_Data;


   --
   -- resource strings of subpackages of Xm_Widgets.Primitive
   --
   Xm_N_Activate_Callback       : constant Xt_N_Resource_String :=
      To_Resource_String ("activateCallback");
   Xm_N_Alignment               : constant Xt_N_Resource_String :=
      To_Resource_String ("alignment");
   Xm_N_Arm_Callback            : constant Xt_N_Resource_String :=
      To_Resource_String ("armCallback");
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String :=
      To_Resource_String ("disarmCallback");
   Xm_N_Multi_Click             : constant Xt_N_Resource_String :=
      To_Resource_String ("multiClick");
   Xm_N_Spacing                 : constant Xt_N_Resource_String :=
      To_Resource_String ("spacing");


end Xm_Widgets.Primitive;
