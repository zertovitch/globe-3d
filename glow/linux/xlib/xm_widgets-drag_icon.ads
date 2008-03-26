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

package Xm_Widgets.Drag_Icon is


   Xm_Drag_Icon_Object_Class : constant Widget_Class;

   -- -------------------------------------------------------------------------
   --
   -- XmIsDragIconObject
   --
   function Xm_Is_Drag_Icon_Object_Class (W : in Widget) return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- XmCreateDragIcon
   --
   function Xm_Create_Drag_Icon
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Attachment             : constant Xt_N_Resource_String;

   type Attachment_Type is
     (Attach_North_West, Attach_North, Attach_North_East,
      Attach_East,
      Attach_South_East, Attach_South, Attach_South_West,
      Attach_West,
      Attach_Center, Attach_Hot);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Attachment_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Attachment_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Depth                  : constant Xt_N_Resource_String;
   Xm_N_Height                 : constant Xt_N_Resource_String;
   Xm_N_Hot_X                  : constant Xt_N_Resource_String;
   Xm_N_Hot_Y                  : constant Xt_N_Resource_String;
   Xm_N_Mask                   : constant Xt_N_Resource_String;
   Xm_N_Offset_X               : constant Xt_N_Resource_String;
   Xm_N_Offset_Y               : constant Xt_N_Resource_String;
   Xm_N_Pixmap                 : constant Xt_N_Resource_String;
   Xm_N_Width                  : constant Xt_N_Resource_String;


private

   for Attachment_Type use
     (Attach_North_West => 0, Attach_North => 1, Attach_North_East => 2,
      Attach_East => 3,
      Attach_South_East => 4, Attach_South => 5, Attach_South_West => 6,
      Attach_West => 7,
      Attach_Center => 8, Attach_Hot => 9);
   for Attachment_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Drag_Icon_Object_Class : Widget_Class;

   pragma Import (C, c_const_Xm_Drag_Icon_Object_Class, "xmDragIconObjectClass");

   Xm_Drag_Icon_Object_Class : constant Widget_Class :=
      c_const_Xm_Drag_Icon_Object_Class;

   Xm_N_Attachment             : constant Xt_N_Resource_String :=
      To_Resource_String ("attachment");
   Xm_N_Depth                  : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Depth;
   Xm_N_Height                 : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Height;
   Xm_N_Hot_X                  : constant Xt_N_Resource_String :=
      To_Resource_String ("hotX");
   Xm_N_Hot_Y                  : constant Xt_N_Resource_String :=
      To_Resource_String ("hotY");
   Xm_N_Mask                   : constant Xt_N_Resource_String :=
      To_Resource_String ("mask");
   Xm_N_Offset_X               : constant Xt_N_Resource_String :=
      To_Resource_String ("offsetX");
   Xm_N_Offset_Y               : constant Xt_N_Resource_String :=
      To_Resource_String ("offsetY");
   Xm_N_Pixmap                 : constant Xt_N_Resource_String :=
      To_Resource_String ("pixmap");
   Xm_N_Width                  : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Width;

end Xm_Widgets.Drag_Icon;
