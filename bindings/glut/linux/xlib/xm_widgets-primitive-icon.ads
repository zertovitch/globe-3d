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

package Xm_Widgets.Primitive.Icon is
 
-- UseMotif2.0 Motif2.1

   Xm_Icon_Gadget_Class                : constant Widget_Class;


   function Xm_Is_Icon_Gadget (W : in Widget) return Boolean;


   function Xm_Create_Icon_Gadget
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;                                    


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

-- UseMotif2.1
   -- use a value of Xm_Widgets.Alignment
   --
   Xm_N_Alignment               : constant Xt_N_Resource_String;
-- EndMotif2.1
   Xm_N_Detail                  : constant Xt_N_Resource_String;
   Xm_N_Detail_Count            : constant Xt_N_Resource_String;
   Xm_N_Font_List               : constant Xt_N_Resource_String;
   Xm_N_Label_String            : constant Xt_N_Resource_String;
   Xm_N_Large_Icon_Mask         : constant Xt_N_Resource_String;
   Xm_N_Large_Icon_Pixmap       : constant Xt_N_Resource_String;
-- UseMotif2.1
   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;
-- EndMotif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String;
   Xm_N_Small_Icon_Mask         : constant Xt_N_Resource_String;
   Xm_N_Small_Icon_Pixmap       : constant Xt_N_Resource_String;
-- UseMotif2.1
   Xm_N_Spacing                 : constant Xt_N_Resource_String;
-- EndMotif2.1

   Xm_N_View_Type               : constant Xt_N_Resource_String;

   type View_Type is (Large_Icon, Small_Icon);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     View_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out View_Type);
   pragma Convention (C, Append_Get);

   
   Xm_N_Visual_Emphasis         : constant Xt_N_Resource_String;

   type Visual_Emphasis_Type is (Selected, Not_Selected);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Visual_Emphasis_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Visual_Emphasis_Type);
   pragma Convention (C, Append_Get);

private

   for View_Type use (Large_Icon => 0, Small_Icon => 1);
   for View_Type'Size use Interfaces.C.unsigned_char'Size;

   for Visual_Emphasis_Type use (Selected => 0, Not_Selected => 1);
   for Visual_Emphasis_Type'Size use Interfaces.C.unsigned_char'Size;

   c_const_Xm_Icon_Gadget_Class                : Widget_Class;
   
   pragma Import (C, c_const_Xm_Icon_Gadget_Class, "xmIconGadgetClass");
   
   Xm_Icon_Gadget_Class                : constant Widget_Class :=
    c_const_Xm_Icon_Gadget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

-- UseMotif2.1
   Xm_N_Alignment               : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Alignment;
-- EndMotif2.1
   Xm_N_Detail                  : constant Xt_N_Resource_String :=
      To_Resource_String ("detail");
   Xm_N_Detail_Count            : constant Xt_N_Resource_String :=
      To_Resource_String ("detailCount");
   Xm_N_Font_List               : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Font_List;
   Xm_N_Label_String            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Label_String;
   Xm_N_Large_Icon_Mask         : constant Xt_N_Resource_String :=
      To_Resource_String ("largeIconMask");
   Xm_N_Large_Icon_Pixmap       : constant Xt_N_Resource_String :=
      To_Resource_String ("largeIconPixmap");
-- UseMotif2.1
   Xm_N_Margin_Height           : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Width;
-- EndMotif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Render_Table;
   Xm_N_Small_Icon_Mask         : constant Xt_N_Resource_String :=
      To_Resource_String ("smallIconMask");
   Xm_N_Small_Icon_Pixmap       : constant Xt_N_Resource_String :=
      To_Resource_String ("smallIconPixmap");

-- UseMotif2.1
   Xm_N_Spacing                 : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Spacing;
-- EndMotif2.1

   Xm_N_View_Type               : constant Xt_N_Resource_String :=
      To_Resource_String ("viewType");

   Xm_N_Visual_Emphasis         : constant Xt_N_Resource_String :=
      To_Resource_String ("visualEmphasis");

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Primitive.Icon;
