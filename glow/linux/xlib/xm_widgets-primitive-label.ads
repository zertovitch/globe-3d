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

package Xm_Widgets.Primitive.Label is
 
   Xm_Label_Widget_Class               : constant Widget_Class;
   Xm_Label_Gadget_Class               : constant Widget_Class;


   function Xm_Is_Label        (W : in Widget) return Boolean;
   function Xm_Is_Label_Gadget (W : in Widget) return Boolean;


   function Xm_Create_Label
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Label_Gadget
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Accelerator             : constant Xt_N_Resource_String;
   Xm_N_Accelerator_Text        : constant Xt_N_Resource_String;

   -- use a value of Xm_Widgets.Alignment
   --
   Xm_N_Alignment               : constant Xt_N_Resource_String;

   Xm_N_Font_List               : constant Xt_N_Resource_String;
   Xm_N_Label_Insensitive_Pixmap : constant Xt_N_Resource_String;
   Xm_N_Label_Pixmap            : constant Xt_N_Resource_String;
   Xm_N_Label_String            : constant Xt_N_Resource_String;

   Xm_N_Label_Type              : constant Xt_N_Resource_String;


   type Label_Type is (Is_Pixmap, Is_String);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Label_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Label_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Margin_Bottom           : constant Xt_N_Resource_String;
   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Left             : constant Xt_N_Resource_String;
   Xm_N_Margin_Right            : constant Xt_N_Resource_String;
   Xm_N_Margin_Top              : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;
   Xm_N_Mnemonic                : constant Xt_N_Resource_String;
   Xm_N_Mnemonic_Char_Set       : constant Xt_N_Resource_String;
   Xm_N_Recompute_Size          : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1
   Xm_N_String_Direction        : constant Xt_N_Resource_String;


private

   for Label_Type use (Is_Pixmap => 1, Is_String => 2);
   for Label_Type'Size use Interfaces.C.unsigned_char'Size;

   c_const_Xm_Label_Widget_Class               : Widget_Class;
   c_const_Xm_Label_Gadget_Class               : Widget_Class;

   pragma Import (C, c_const_Xm_Label_Widget_Class, "xmLabelWidgetClass");
   pragma Import (C, c_const_Xm_Label_Gadget_Class, "xmLabelGadgetClass");

   Xm_Label_Widget_Class               : constant Widget_Class :=
    c_const_Xm_Label_Widget_Class;
   Xm_Label_Gadget_Class               : constant Widget_Class :=
    c_const_Xm_Label_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Accelerator             : constant Xt_N_Resource_String :=
      To_Resource_String ("accelerator");
   Xm_N_Accelerator_Text        : constant Xt_N_Resource_String :=
      To_Resource_String ("acceleratorText");
   Xm_N_Alignment               : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Alignment;
   Xm_N_Font_List               : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Font_List;
   Xm_N_Label_Insensitive_Pixmap : constant Xt_N_Resource_String :=
      To_Resource_String ("labelInsensitivePixmap");
   Xm_N_Label_Pixmap            : constant Xt_N_Resource_String :=
      To_Resource_String ("labelPixmap");
   Xm_N_Label_String            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Label_String;

   Xm_N_Label_Type              : constant Xt_N_Resource_String :=
      To_Resource_String ("labelType");

   Xm_N_Margin_Bottom           : constant Xt_N_Resource_String :=
      To_Resource_String ("marginBottom");
   Xm_N_Margin_Height           : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Left             : constant Xt_N_Resource_String :=
      To_Resource_String ("marginLeft");
   Xm_N_Margin_Right            : constant Xt_N_Resource_String :=
      To_Resource_String ("marginRight");
   Xm_N_Margin_Top              : constant Xt_N_Resource_String :=
      To_Resource_String ("marginTop");
   Xm_N_Margin_Width            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Mnemonic                : constant Xt_N_Resource_String :=
      To_Resource_String ("mnemonic");
   Xm_N_Mnemonic_Char_Set       : constant Xt_N_Resource_String :=
      To_Resource_String ("mnemonicCharSet");
   Xm_N_Recompute_Size          : constant Xt_N_Resource_String :=
      To_Resource_String ("recomputeSize");
-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Render_Table;
-- EndMotif2.0 Motif2.1
   Xm_N_String_Direction        : constant Xt_N_Resource_String :=
      To_Resource_String ("stringDirection");

end Xm_Widgets.Primitive.Label;
