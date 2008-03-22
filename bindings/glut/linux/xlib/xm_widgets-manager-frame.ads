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

package Xm_Widgets.Manager.Frame is
 

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Frame_Widget_Class               : constant Widget_Class;


   function Xm_Is_Frame (W : in Widget) return Boolean;


   function Xm_Create_Frame
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;
   Xm_N_Shadow_Type             : constant Xt_N_Resource_String;

   type Shadow_Type is
     (Shadow_Etched_In, Shadow_Etched_Out, Shadow_In, Shadow_Out);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Shadow_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Shadow_Type);
   pragma Convention (C, Append_Get);

   --
   -- geometrical constraints for children of the frame widget
   --
   Xm_N_Child_Type              : constant Xt_N_Resource_String;
   Xm_N_Child_Horizontal_Alignment : constant Xt_N_Resource_String;
   Xm_N_Child_Horizontal_Spacing : constant Xt_N_Resource_String;
   Xm_N_Child_Vertical_Alignment : constant Xt_N_Resource_String;

-- UseMotif2.0 Motif2.1
   type Vertical_Alignment is (Alignment_Baseline_Top,
                               Alignment_Center,
                               Alignment_Baseline_Bottom,
                               Alignment_Widget_Top,
                               Alignment_Widget_Bottom);
   Alignment_Child_Top    : constant Vertical_Alignment;
   Alignment_Child_Bottom : constant Vertical_Alignment;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Vertical_Alignment);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Vertical_Alignment);
   pragma Convention (C, Append_Get);

   -- replacement for Xm_N_Child_Type
   Xm_N_Frame_Child_Type         : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1

   type Frame_Child_Type is
     (Generic_Child, Workarea_Child, Title_Child);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Frame_Child_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Frame_Child_Type);
   pragma Convention (C, Append_Get);

private

   for Shadow_Type use
     (Shadow_Etched_In => 5, Shadow_Etched_Out => 6, Shadow_In => 7, Shadow_Out => 8);
   for Shadow_Type'Size use Interfaces.C.unsigned_char'Size;

   for Frame_Child_Type use
     (Generic_Child => 0, Workarea_Child => 1, Title_Child => 2);
   for Frame_Child_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Vertical_Alignment use (Alignment_Baseline_Top => 0,
                               Alignment_Center => 1,
                               Alignment_Baseline_Bottom => 2,
                               Alignment_Widget_Top => 3,
                               Alignment_Widget_Bottom => 4);
   for Vertical_Alignment'Size use Interfaces.C.unsigned_char'Size;
   Alignment_Child_Top    : constant Vertical_Alignment :=
      Alignment_Widget_Bottom;
   Alignment_Child_Bottom : constant Vertical_Alignment :=
      Alignment_Widget_Top;
-- EndMotif2.0 Motif2.1


   c_const_Xm_Frame_Widget_Class               : Widget_Class;

   pragma Import (C, c_const_Xm_Frame_Widget_Class, "xmFrameWidgetClass");

   Xm_Frame_Widget_Class                : constant Widget_Class :=
    c_const_Xm_Frame_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin_Height           : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Shadow_Type             : constant Xt_N_Resource_String :=
      To_Resource_String ("shadowType");

   --
   -- geometrical constraints for children of the frame widget
   --
   Xm_N_Child_Type              : constant Xt_N_Resource_String :=
      To_Resource_String ("childType");
   Xm_N_Child_Horizontal_Alignment : constant Xt_N_Resource_String :=
      To_Resource_String ("childHorizontalAlignment");
   Xm_N_Child_Horizontal_Spacing : constant Xt_N_Resource_String :=
      To_Resource_String ("childHorizontalSpacing");
   Xm_N_Child_Vertical_Alignment : constant Xt_N_Resource_String :=
      To_Resource_String ("childVerticalAlignment");
-- UseMotif2.0 Motif2.1
   -- replacement for Xm_N_Child_Type
   Xm_N_Frame_Child_Type         : constant Xt_N_Resource_String :=
      To_Resource_String ("frameChildType");
-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Frame;
