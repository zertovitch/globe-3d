-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2002 Hans-Frieder Vogt                                --
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
--          19 May 2001 Vadim Godunko: first definition of this file
--          17 Nov 2001 Vadim Godunko: remove resource Xm_N_Spin_Box_Child_Type
--                                     (already defined in Spin_Box package)
--          12 Jan 2002 H.-F. Vogt: make Spin_Box.Simple only available in
--                                  Motif 2.1 and later
--                                  add a few comments
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Spin_Box.Simple is

-- UseMotif2.1

   Xm_Simple_Spin_Box_Widget_Class     : constant Widget_Class;


   type Xm_Simple_Spin_Box_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      W                : Widget;
      Doit             : Boolean;
      Pos              : Integer;
      Value            : Xm_String;
      Crossed_Boundary : Boolean;
   end record;
   pragma Convention (C, Xm_Simple_Spin_Box_Callback_Struct);

   type Xm_Simple_Spin_Box_Callback_Struct_Access is
      access all Xm_Simple_Spin_Box_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Simple_Spin_Box_Callback_Struct_Access;


   function Xm_Is_Simple_Spin_Box (W : in Widget) return Boolean;

   function Xm_Create_Simple_Spin_Box
     (Parent   : in  Widget;
      Name     : in  Standard.String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   procedure Xm_Simple_Spin_Box_Add_Item
     (W    : in Widget;
      Item : in Xm_String;
      Pos  : in Integer);

   procedure Xm_Simple_Spin_Box_Delete_Pos
     (W   : in Widget;
      Pos : in Integer);

   procedure Xm_Simple_Spin_Box_Set_Item
     (W    : in Widget;
      Item : in Xm_String);

   --
   -- resources only for simple spin box
   --
   --  already defined in Spin_Box:
   --  Xm_N_Arrow_Sensitivity       : constant Xt_N_Resource_String;
   --  Xm_N_Decimal_Points          : constant Xt_N_Resource_String;
   --  Xm_N_Increment_Value         : constant Xt_N_Resource_String;
   --  Xm_N_Maximum_Value           : constant Xt_N_Resource_String;
   --  Xm_N_Minimum_Value           : constant Xt_N_Resource_String;
   --  Xm_N_Num_Values              : constant Xt_N_Resource_String;
   --  Xm_N_Position                : constant Xt_N_Resource_String;
   --  Xm_N_Position_Type           : constant Xt_N_Resource_String;
   --  Xm_N_Spin_Box_Child_Type     : constant Xt_N_Resource_String;
   --  Xm_N_Values                  : constant Xt_N_Resource_String;
   --  Xm_N_Wrap                    : constant Xt_N_Resource_String;

   Xm_N_Columns                 : constant Xt_N_Resource_String;
   Xm_N_Editable                : constant Xt_N_Resource_String;
   Xm_N_Text_Field              : constant Xt_N_Resource_String;

private

   c_const_Xm_Simple_Spin_Box_Widget_Class     : Widget_Class;

   pragma Import (C, c_const_Xm_Simple_Spin_Box_Widget_Class, "xmSimpleSpinBoxWidgetClass");

   Xm_Simple_Spin_Box_Widget_Class     : constant Widget_Class :=
    c_const_Xm_Simple_Spin_Box_Widget_Class;

   --
   -- resources only for simple spin box
   --
   Xm_N_Columns                 : constant Xt_N_Resource_String :=
      To_Resource_String ("columns");
   Xm_N_Editable                : constant Xt_N_Resource_String :=
      To_Resource_String ("editable");
   Xm_N_Text_Field              : constant Xt_N_Resource_String :=
      To_Resource_String ("textField");

   pragma Import (C, Xm_Simple_Spin_Box_Add_Item, "XmSimpleSpinBoxAddItem");
   pragma Import (C, Xm_Simple_Spin_Box_Delete_Pos, "XmSimpleSpinBoxDeletePos");
   pragma Import (C, Xm_Simple_Spin_Box_Set_Item, "XmSimpleSpinBoxSetItem");

-- EndMotif2.1

end Xm_Widgets.Manager.Spin_Box.Simple;
