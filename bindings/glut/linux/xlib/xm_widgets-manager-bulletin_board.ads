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

package Xm_Widgets.Manager.Bulletin_Board is


   Xm_Bulletin_Board_Widget_Class      : constant Widget_Class;

   function Xm_Is_Bulletin_Board (W : in Widget) return Boolean;


   function Xm_Create_Bulletin_Board
    (Parent   : in  Widget;
     Name     : in  String;
     Arglist  : in  Arg_List := Null_Arg_List)
     return Widget;

   function Xm_Create_Bulletin_Board_Dialog
    (Parent   : in  Widget;
     Name     : in  String;
     Arglist  : in  Arg_List := Null_Arg_List)
     return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Allow_Overlap          : constant Xt_N_Resource_String;
   Xm_N_Auto_Unmanage          : constant Xt_N_Resource_String;
   Xm_N_Button_Font_List       : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Button_Render_Table    : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1

   Xm_N_Cancel_Button          : constant Xt_N_Resource_String;
   Xm_N_Default_Button         : constant Xt_N_Resource_String;
   Xm_N_Default_Position       : constant Xt_N_Resource_String;

   Xm_N_Dialog_Style           : constant Xt_N_Resource_String;

   type Dialog_Style_Type is (Modeless, Primary_Application_Modal,
                              Full_Application_Modal, System_Modal);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Dialog_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Dialog_Style_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Dialog_Title           : constant Xt_N_Resource_String;
   Xm_N_Focus_Callback         : constant Xt_N_Resource_String;
   Xm_N_Label_Font_List        : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Label_Render_Table     : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Map_Callback           : constant Xt_N_Resource_String;
   Xm_N_Margin_Height          : constant Xt_N_Resource_String;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String;
   Xm_N_No_Resize              : constant Xt_N_Resource_String;
   Xm_N_Resize_Policy          : constant Xt_N_Resource_String;
   Xm_N_Shadow_Type            : constant Xt_N_Resource_String;


   type Shadow_Type is (Shadow_In,      Shadow_Out);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Shadow_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Shadow_Type);
   pragma Convention (C, Append_Get);



   Xm_N_Text_Font_List         : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Text_Render_Table      : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_Text_Translations      : constant Xt_N_Resource_String;

   --
   -- dialog specific resources

   Xm_N_Unmap_Callback         : constant Xt_N_Resource_String;

private

   for Dialog_Style_Type use (Modeless => 0, Primary_Application_Modal => 1,
                              Full_Application_Modal => 2, System_Modal => 3);
   for Dialog_Style_Type'Size use Interfaces.C.unsigned_char'Size;


   for Shadow_Type use (Shadow_In => 7, Shadow_Out => 8);
   for Shadow_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Bulletin_Board_Widget_Class      : Widget_Class;

   pragma Import (C, c_const_Xm_Bulletin_Board_Widget_Class, "xmBulletinBoardWidgetClass");

   Xm_Bulletin_Board_Widget_Class      : constant Widget_Class :=
    c_const_Xm_Bulletin_Board_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Allow_Overlap          : constant Xt_N_Resource_String
      := To_Resource_String ("allowOverlap");
   Xm_N_Auto_Unmanage          : constant Xt_N_Resource_String
      := To_Resource_String ("autoUnmanage");
   Xm_N_Button_Font_List       : constant Xt_N_Resource_String
      := To_Resource_String ("buttonFontList");
-- UseMotif2.0 Motif2.1
   Xm_N_Button_Render_Table    : constant Xt_N_Resource_String
      := To_Resource_String ("buttonRenderTable");
-- EndMotif2.0 Motif2.1

   Xm_N_Cancel_Button          : constant Xt_N_Resource_String
      := To_Resource_String ("cancelButton");
   Xm_N_Default_Button         : constant Xt_N_Resource_String
      := To_Resource_String ("defaultButton");
   Xm_N_Default_Position       : constant Xt_N_Resource_String
      := To_Resource_String ("defaultPosition");

   Xm_N_Dialog_Style           : constant Xt_N_Resource_String
      := To_Resource_String ("dialogStyle");

   Xm_N_Dialog_Title           : constant Xt_N_Resource_String
      := To_Resource_String ("dialogTitle");
   Xm_N_Focus_Callback         : constant Xt_N_Resource_String
      := To_Resource_String ("focusCallback");
   Xm_N_Label_Font_List        : constant Xt_N_Resource_String
      := To_Resource_String ("labelFontList");
-- UseMotif2.0 Motif2.1
   Xm_N_Label_Render_Table     : constant Xt_N_Resource_String
      := To_Resource_String ("labelRenderTable");
-- EndMotif2.0 Motif2.1
   Xm_N_Map_Callback           : constant Xt_N_Resource_String
      := To_Resource_String ("mapCallback");
   Xm_N_Margin_Height          : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_No_Resize              : constant Xt_N_Resource_String
      := To_Resource_String ("noResize");
   Xm_N_Resize_Policy          : constant Xt_N_Resource_String
      := To_Resource_String ("resizePolicy");
   Xm_N_Shadow_Type            : constant Xt_N_Resource_String
      := To_Resource_String ("shadowType");

   Xm_N_Text_Font_List         : constant Xt_N_Resource_String
      := To_Resource_String ("textFontList");
-- UseMotif2.0 Motif2.1
   Xm_N_Text_Render_Table      : constant Xt_N_Resource_String
      := To_Resource_String ("textRenderTable");
-- EndMotif2.0 Motif2.1
   Xm_N_Text_Translations      : constant Xt_N_Resource_String
      := To_Resource_String ("textTranslations");

   --
   -- dialog specific resources

   Xm_N_Unmap_Callback         : constant Xt_N_Resource_String
      := To_Resource_String ("unmapCallback");


   -- resources necessary for child packages
   --
   Xm_N_Cancel_Callback        : constant Xt_N_Resource_String
      := To_Resource_String ("cancelCallback");
   Xm_N_Cancel_Label_String    : constant Xt_N_Resource_String
      := To_Resource_String ("cancelLabelString");
   Xm_N_Help_Label_String      : constant Xt_N_Resource_String
      := To_Resource_String ("helpLabelString");
   Xm_N_OK_Callback            : constant Xt_N_Resource_String
      := To_Resource_String ("okCallback");
   Xm_N_OK_Label_String        : constant Xt_N_Resource_String
      := To_Resource_String ("okLabelString");
   Xm_N_Minimize_Buttons       : constant Xt_N_Resource_String
      := To_Resource_String ("minimizeButtons");


end Xm_Widgets.Manager.Bulletin_Board;
