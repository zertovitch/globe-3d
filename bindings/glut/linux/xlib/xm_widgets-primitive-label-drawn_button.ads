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

package Xm_Widgets.Primitive.Label.Drawn_Button is

   Xm_Drawn_Button_Widget_Class        : constant Widget_Class;


   type Xm_Drawn_Button_Callback_Struct is record
      Reason      : Callback_Reason;
      Event       : X_Lib.X_Event_Pointer;
      Window      : X_Lib.Window_ID;
      Click_Count : Integer;
   end record;
   pragma Convention (C, Xm_Drawn_Button_Callback_Struct);

   type Xm_Drawn_Button_Callback_Struct_Access is
      access all Xm_Drawn_Button_Callback_Struct;

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Drawn_Button_Callback_Struct_Access;



   function Xm_Is_Drawn_Button (W : in Widget) return Boolean;

   function Xm_Create_Drawn_Button
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
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String;
   Xm_N_Expose_Callback         : constant Xt_N_Resource_String;

   Xm_N_Multi_Click             : constant Xt_N_Resource_String;

   Xm_N_Push_Button_Enabled     : constant Xt_N_Resource_String;
   Xm_N_Resize_Callback         : constant Xt_N_Resource_String;
   Xm_N_Shadow_Type             : constant Xt_N_Resource_String;

   type Shadow_Type is (Shadow_In,      Shadow_Out);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Shadow_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Shadow_Type);
   pragma Convention (C, Append_Get);



private

   for Shadow_Type use (Shadow_In => 7, Shadow_Out => 8);
   for Shadow_Type'Size use Interfaces.C.unsigned_char'Size;

   c_const_Xm_Drawn_Button_Widget_Class        : Widget_Class;

   pragma Import (C, c_const_Xm_Drawn_Button_Widget_Class, "xmDrawnButtonWidgetClass");

   Xm_Drawn_Button_Widget_Class        : constant Widget_Class :=
    c_const_Xm_Drawn_Button_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Activate_Callback       : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Activate_Callback;
   Xm_N_Arm_Callback            : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Arm_Callback;
   Xm_N_Disarm_Callback         : constant Xt_N_Resource_String
      := Xm_Widgets.Primitive.Xm_N_Disarm_Callback;
   Xm_N_Expose_Callback         : constant Xt_N_Resource_String :=
      To_Resource_String ("exposeCallback");

   Xm_N_Multi_Click             : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Multi_Click;

   Xm_N_Push_Button_Enabled     : constant Xt_N_Resource_String :=
      To_Resource_String ("pushButtonEnabled");
   Xm_N_Resize_Callback         : constant Xt_N_Resource_String :=
      To_Resource_String ("resizeCallback");
   Xm_N_Shadow_Type             : constant Xt_N_Resource_String :=
      To_Resource_String ("shadowType");

end Xm_Widgets.Primitive.Label.Drawn_Button;
