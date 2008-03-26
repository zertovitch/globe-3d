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
--          20 Jan 2002 H.-F. Vogt: Orientation_Type was moved to Xm_Widgets
--
-------------------------------------------------------------------------------

package Xm_Widgets.Primitive.Separator is
 

   Xm_Separator_Widget_Class           : constant Widget_Class;
   Xm_Separator_Gadget_Class           : constant Widget_Class;


   function Xm_Is_Separator        (W : in Widget) return Boolean;
   function Xm_Is_Separator_Gadget (W : in Widget) return Boolean;


   function Xm_Create_Separator
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Separator_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin                  : constant Xt_N_Resource_String;
   --  use Xm_Widgets.Orientation_Type for this
   --
   Xm_N_Orientation             : constant Xt_N_Resource_String;

   Xm_N_Separator_Type          : constant Xt_N_Resource_String;

   type Separator_Type is
     (No_Line,
      Single_Line,           Double_Line,
      Single_Dashed_Line,    Double_Dashed_Line, 
      Shadow_Etched_In,      Shadow_Etched_Out,
      Shadow_Etched_In_Dash, Shadow_Etched_Out_Dash,
      Invalid_Separator_Type);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Separator_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Separator_Type);
   pragma Convention (C, Append_Get);


private

   for Separator_Type use
     (No_Line => 0,
      Single_Line => 1,           Double_Line => 2,
      Single_Dashed_Line => 3,    Double_Dashed_Line => 4, 
      Shadow_Etched_In => 5,      Shadow_Etched_Out => 6,
      Shadow_Etched_In_Dash => 7, Shadow_Etched_Out_Dash => 8,
      Invalid_Separator_Type => 9);
   for Separator_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Separator_Widget_Class           : Widget_Class;
   c_const_Xm_Separator_Gadget_Class           : Widget_Class;
   
   pragma Import (C, c_const_Xm_Separator_Widget_Class, "xmSeparatorWidgetClass");
   pragma Import (C, c_const_Xm_Separator_Gadget_Class, "xmSeparatorGadgetClass");
   
   Xm_Separator_Widget_Class           : constant Widget_Class :=
    c_const_Xm_Separator_Widget_Class;
   Xm_Separator_Gadget_Class           : constant Widget_Class :=
    c_const_Xm_Separator_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin                  : constant Xt_N_Resource_String :=
      To_Resource_String ("margin");
   Xm_N_Orientation             : constant Xt_N_Resource_String :=
      X_Toolkit.Xt_N_Orientation;
   Xm_N_Separator_Type          : constant Xt_N_Resource_String :=
      To_Resource_String ("separatorType");

end Xm_Widgets.Primitive.Separator;
