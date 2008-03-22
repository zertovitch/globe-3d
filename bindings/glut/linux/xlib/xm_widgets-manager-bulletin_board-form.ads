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
--          19 May 2001 Vadim Godunko: add (previously forgotten) resource
--                                     resizable
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Bulletin_Board.Form is

   Xm_Form_Widget_Class                : constant Widget_Class;

   function Xm_Is_Form (W: in Widget) return Boolean;


   function Xm_Create_Form
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Form_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Fraction_Base          : constant Xt_N_Resource_String;
   Xm_N_Horizontal_Spacing     : constant Xt_N_Resource_String;
   Xm_N_Rubber_Positioning     : constant Xt_N_Resource_String;
   Xm_N_Vertical_Spacing       : constant Xt_N_Resource_String;


   --
   -- geometrical constraints for children of the form widget
   --
   Xm_N_Bottom_Attachment      : constant Xt_N_Resource_String;
   Xm_N_Bottom_Offset          : constant Xt_N_Resource_String;
   Xm_N_Bottom_Position        : constant Xt_N_Resource_String;
   Xm_N_Bottom_Widget          : constant Xt_N_Resource_String;

   Xm_N_Left_Attachment        : constant Xt_N_Resource_String;
   Xm_N_Left_Offset            : constant Xt_N_Resource_String;
   Xm_N_Left_Position          : constant Xt_N_Resource_String;
   Xm_N_Left_Widget            : constant Xt_N_Resource_String;

   Xm_N_Right_Attachment        : constant Xt_N_Resource_String;
   Xm_N_Right_Offset            : constant Xt_N_Resource_String;
   Xm_N_Right_Position          : constant Xt_N_Resource_String;
   Xm_N_Right_Widget            : constant Xt_N_Resource_String;

   Xm_N_Top_Attachment          : constant Xt_N_Resource_String;
   Xm_N_Top_Offset              : constant Xt_N_Resource_String;
   Xm_N_Top_Position            : constant Xt_N_Resource_String;
   Xm_N_Top_Widget              : constant Xt_N_Resource_String;

   -- values for attachment resources
   type Attachment_Type is (Attach_None,
                            Attach_Form,
                            Attach_Opposite_Form,
                            Attach_Widget,
                            Attach_Opposite_Widget,
                            Attach_Position,
                            Attach_Self);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Attachment_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Attachment_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Resizable               : constant Xt_N_Resource_String;
 
private

   for Attachment_Type use (Attach_None            => 0,
                            Attach_Form            => 1,
                            Attach_Opposite_Form   => 2,
                            Attach_Widget          => 3,
                            Attach_Opposite_Widget => 4,
                            Attach_Position        => 5,
                            Attach_Self            => 6);
   for Attachment_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Form_Widget_Class                : Widget_Class;

   pragma Import (C, c_const_Xm_Form_Widget_Class, "xmFormWidgetClass");

   Xm_Form_Widget_Class                : constant Widget_Class :=
    c_const_Xm_Form_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Fraction_Base          : constant Xt_N_Resource_String
      := To_Resource_String ("fractionBase");
   Xm_N_Horizontal_Spacing     : constant Xt_N_Resource_String
      := To_Resource_String ("horizontalSpacing");
   Xm_N_Rubber_Positioning     : constant Xt_N_Resource_String
      := To_Resource_String ("rubberPositioning");
   Xm_N_Vertical_Spacing       : constant Xt_N_Resource_String
      := To_Resource_String ("verticalSpacing");


   --
   -- geometrical constraints for children of the form widget
   --
   Xm_N_Bottom_Attachment      : constant Xt_N_Resource_String
      := To_Resource_String ("bottomAttachment");
   Xm_N_Bottom_Offset          : constant Xt_N_Resource_String
      := To_Resource_String ("bottomOffset");
   Xm_N_Bottom_Position        : constant Xt_N_Resource_String
      := To_Resource_String ("bottomPosition");
   Xm_N_Bottom_Widget          : constant Xt_N_Resource_String
      := To_Resource_String ("bottomWidget");

   Xm_N_Left_Attachment        : constant Xt_N_Resource_String
      := To_Resource_String ("leftAttachment");
   Xm_N_Left_Offset            : constant Xt_N_Resource_String
      := To_Resource_String ("leftOffset");
   Xm_N_Left_Position          : constant Xt_N_Resource_String
      := To_Resource_String ("leftPosition");
   Xm_N_Left_Widget            : constant Xt_N_Resource_String
      := To_Resource_String ("leftWidget");

   Xm_N_Right_Attachment        : constant Xt_N_Resource_String
      := To_Resource_String ("rightAttachment");
   Xm_N_Right_Offset            : constant Xt_N_Resource_String
      := To_Resource_String ("rightOffset");
   Xm_N_Right_Position          : constant Xt_N_Resource_String
      := To_Resource_String ("rightPosition");
   Xm_N_Right_Widget            : constant Xt_N_Resource_String
      := To_Resource_String ("rightWidget");

   Xm_N_Top_Attachment          : constant Xt_N_Resource_String
      := To_Resource_String ("topAttachment");
   Xm_N_Top_Offset              : constant Xt_N_Resource_String
      := To_Resource_String ("topOffset");
   Xm_N_Top_Position            : constant Xt_N_Resource_String
      := To_Resource_String ("topPosition");
   Xm_N_Top_Widget              : constant Xt_N_Resource_String
      := To_Resource_String ("topWidget");

   Xm_N_Resizable               : constant Xt_N_Resource_String
      := To_Resource_String ("resizable");

end Xm_Widgets.Manager.Bulletin_Board.Form;
