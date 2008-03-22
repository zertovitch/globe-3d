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
--          27 Jan 2002 V. Godunko: mark Xm_Message_Box_Get_Child as obsolete
--                                  for Motif 2.x, add name strings for
--                                  children
--          03 Feb 2002 H.-F. Vogt: move definition of children name strings
--                                  into private section of package
--
-------------------------------------------------------------------------------

package Xm_Widgets.Manager.Bulletin_Board.Message_Box is

   Xm_Message_Box_Widget_Class         : constant Widget_Class;


   function Xm_Is_Message_Box (W: in Widget) return Boolean;


   function Xm_Create_Message_Box
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Message_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Error_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Information_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Question_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Warning_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Working_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Template_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   type Message_Box_Child_Type is (None,
                                   Cancel_Button,
                                   Default_Button,
                                   OK_Button,
                                   Help_Button,
                                   Message_Label,
                                   Symbol_Label,
                                   Separator);

   --  in Motif 2.x this subprogram is obsolete, use Xt_Name_To_Widget instead
   --
   function Xm_Message_Box_Get_Child (W      : in Widget;
                                      Child  : in Message_Box_Child_Type)
      return Widget;

   --  widget name for Xt_Name_To_Widget
   --
   Cancel_Button_Name : constant String;
   Help_Button_Name   : constant String;
   Message_Label_Name : constant String;
   Ok_Button_Name     : constant String;
   Separator_Name     : constant String;
   Symbol_Label_Name  : constant String;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Cancel_Callback        : constant Xt_N_Resource_String;
   Xm_N_Cancel_Label_String    : constant Xt_N_Resource_String;
   Xm_N_Default_Button_Type    : constant Xt_N_Resource_String;

   type Default_Button_Type is (Cancel_Button,
                                OK_Button,
                                Help_Button);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Default_Button_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Default_Button_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Dialog_Type            : constant Xt_N_Resource_String;

   type Dialog_Type is (Template_Dialog,
                        Error_Dialog,
			Information_Dialog,
                        Message_Dialog,
			Question_Dialog,
                        Warning_Dialog,
			Working_Dialog);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Dialog_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Dialog_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Help_Label_String      : constant Xt_N_Resource_String;
   -- pass through to the labels alignment
   Xm_N_Message_Alignment      : constant Xt_N_Resource_String;
   Xm_N_Message_String         : constant Xt_N_Resource_String;
   Xm_N_Minimize_Buttons       : constant Xt_N_Resource_String;
   Xm_N_OK_Callback            : constant Xt_N_Resource_String;
   Xm_N_OK_Label_String        : constant Xt_N_Resource_String;

   -- pass through to the icon labels labelPixmap resource
   Xm_N_Symbol_Pixmap          : constant Xt_N_Resource_String;

private

   for Message_Box_Child_Type use (None            => 0,
                                   Cancel_Button   => 2,
                                   Default_Button  => 3,
                                   OK_Button       => 4,
                                   Help_Button     => 7,
                                   Message_Label   => 10,
                                   Symbol_Label    => 12,
                                   Separator       => 14);
   for Message_Box_Child_Type'Size use Interfaces.C.unsigned_char'Size;
   pragma Import (C, Xm_Message_Box_Get_Child, "XmMessageBoxGetChild");

   Cancel_Button_Name : constant String := "Cancel";
   Help_Button_Name   : constant String := "Help";
   Message_Label_Name : constant String := "Message";
   Ok_Button_Name     : constant String := "OK";
   Separator_Name     : constant String := "Separator";
   Symbol_Label_Name  : constant String := "Symbol";

   for Default_Button_Type use (Cancel_Button => 2,
                                OK_Button => 4,
                                Help_Button => 7);
   for Default_Button_Type'Size use Interfaces.C.unsigned_char'Size;

   for Dialog_Type use (Template_Dialog => 0,
                        Error_Dialog => 1,
			Information_Dialog => 2,
                        Message_Dialog => 3,
			Question_Dialog => 4,
                        Warning_Dialog => 5,
			Working_Dialog => 6);
   for Dialog_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Message_Box_Widget_Class         : Widget_Class;

   pragma Import (C, c_const_Xm_Message_Box_Widget_Class, "xmMessageBoxWidgetClass");

   Xm_Message_Box_Widget_Class         : constant Widget_Class :=
    c_const_Xm_Message_Box_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Cancel_Callback        : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Cancel_Callback;
   Xm_N_Cancel_Label_String    : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Cancel_Label_String;
   Xm_N_Default_Button_Type    : constant Xt_N_Resource_String
      := To_Resource_String ("defaultButtonType");

   Xm_N_Dialog_Type            : constant Xt_N_Resource_String
      := To_Resource_String ("dialogType");

   Xm_N_Help_Label_String      : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Help_Label_String;

   -- pass through to the labels alignment
   Xm_N_Message_Alignment      : constant Xt_N_Resource_String
      := To_Resource_String ("messageAlignment");
   Xm_N_Message_String         : constant Xt_N_Resource_String
      := To_Resource_String ("messageString");
   Xm_N_Minimize_Buttons       : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_Minimize_Buttons;
   Xm_N_OK_Callback            : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_OK_Callback;
   Xm_N_OK_Label_String        : constant Xt_N_Resource_String
      := Xm_Widgets.Manager.Bulletin_Board.Xm_N_OK_Label_String;

   -- pass through to the icon labels labelPixmap resource
   Xm_N_Symbol_Pixmap          : constant Xt_N_Resource_String
      := To_Resource_String ("symbolPixmap");

end Xm_Widgets.Manager.Bulletin_Board.Message_Box;
