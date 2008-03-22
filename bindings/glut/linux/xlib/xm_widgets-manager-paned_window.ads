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

package Xm_Widgets.Manager.Paned_Window is

   Xm_Paned_Window_Widget_Class        : constant Widget_Class;

   function Xm_Is_Paned_Window (W : in Widget) return Boolean;


   function Xm_Create_Paned_Window
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;                                           


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin_Height          : constant Xt_N_Resource_String;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   --  use Xm_Widgets.Orientation_Type for this
   --
   Xm_N_Orientation            : constant Xt_N_Resource_String;

-- EndMotif2.0 Motif2.1
   Xm_N_Refigure_Mode          : constant Xt_N_Resource_String;
   Xm_N_Sash_Height            : constant Xt_N_Resource_String;
   Xm_N_Sash_Indent            : constant Xt_N_Resource_String;
   Xm_N_Sash_Shadow_Thickness  : constant Xt_N_Resource_String;
   Xm_N_Sash_Width             : constant Xt_N_Resource_String;
   Xm_N_Separator_On           : constant Xt_N_Resource_String;
   Xm_N_Spacing                : constant Xt_N_Resource_String;

   --
   --  constraint resources
   --
   Xm_N_Allow_Resize           : constant Xt_N_Resource_String;
   Xm_N_Maximum                : constant Xt_N_Resource_String;
   Xm_N_Minimum                : constant Xt_N_Resource_String;
   Xm_N_Position_Index         : constant Xt_N_Resource_String;
   Xm_N_Skip_Adjust            : constant Xt_N_Resource_String;



private

   c_const_Xm_Paned_Window_Widget_Class        : Widget_Class;

   pragma Import (C, c_const_Xm_Paned_Window_Widget_Class, "xmPanedWindowWidgetClass");

   Xm_Paned_Window_Widget_Class        : constant Widget_Class :=
    c_const_Xm_Paned_Window_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Margin_Height          : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width           : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_Margin_Width;
-- UseMotif2.0 Motif2.1
   Xm_N_Orientation            : constant Xt_N_Resource_String := X_Toolkit.Xt_N_Orientation;
-- EndMotif2.0 Motif2.1

   Xm_N_Refigure_Mode          : constant Xt_N_Resource_String := To_Resource_String ("refigureMode");
   Xm_N_Sash_Height            : constant Xt_N_Resource_String := To_Resource_String ("sashHeight");
   Xm_N_Sash_Indent            : constant Xt_N_Resource_String := To_Resource_String ("sashIndent");
   Xm_N_Sash_Shadow_Thickness  : constant Xt_N_Resource_String := To_Resource_String ("sashShadowThickness");
   Xm_N_Sash_Width             : constant Xt_N_Resource_String := To_Resource_String ("sashWidth");
   Xm_N_Separator_On           : constant Xt_N_Resource_String := To_Resource_String ("separatorOn");
   Xm_N_Spacing                : constant Xt_N_Resource_String := To_Resource_String ("spacing");

   --
   --  constraint resources
   --
   Xm_N_Allow_Resize           : constant Xt_N_Resource_String := To_Resource_String ("allowResize");
   Xm_N_Maximum                : constant Xt_N_Resource_String := To_Resource_String ("paneMaximum");
   Xm_N_Minimum                : constant Xt_N_Resource_String := To_Resource_String ("paneMinimum");
   Xm_N_Position_Index         : constant Xt_N_Resource_String := To_Resource_String ("positionIndex");
   Xm_N_Skip_Adjust            : constant Xt_N_Resource_String := To_Resource_String ("skipAdjust");


end Xm_Widgets.Manager.Paned_Window;
