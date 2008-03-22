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

package Xm_Widgets.Manager.Notebook is
 
-- UseMotif2.0 Motif2.1

   -- -------------------------------------------------------------------------
   --
   --  constant representing widget/gadget class
   --

   Xm_Notebook_Widget_Class           : constant Widget_Class;


   type Xm_Notebook_Callback_Struct is record
      Reason             : Callback_Reason;
      Event              : X_Lib.X_Event_Pointer;
      Page_Number        : Integer;
      Page_Widget        : Widget;
      Prev_Page_Number   : Integer;
      Prev_Page_Widget   : Widget;
   end record;
   pragma Convention (C, Xm_Notebook_Callback_Struct);

   type Xm_Notebook_Callback_Struct_Access is
      access all Xm_Notebook_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Notebook_Callback_Struct_Access;


   Xm_Error_Page_Invalid    : exception;
   Xm_Error_Page_Empty      : exception;
   Xm_Error_Page_Duplicated : exception;


   type Xm_Notebook_Page_Info is record
      Page_Number          : Integer;
      Page_Widget          : Widget;
      Status_Area_Widget   : Widget;
      Major_Tab_Widget     : Widget;
      Minor_Tab_Widget     : Widget;
   end record;
   pragma Convention (C, Xm_Notebook_Page_Info);


   function Xm_Is_Notebook (W : in Widget) return Boolean;


   function Xm_Create_Notebook
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   procedure Xm_Notebook_Get_Page_Info
     (Notebook     : in     Widget;
      Page_Number  : in     Integer;
      Page_Info    :    out Xm_Notebook_Page_Info);

   --
   -- special value for Page_Number
   --
   Xm_Unspecified_Page_Number : constant Integer := -32768;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Back_Page_Placement     : constant Xt_N_Resource_String;
   Xm_N_Back_Page_Number        : constant Xt_N_Resource_String;
   Xm_N_Back_Page_Size          : constant Xt_N_Resource_String;
   Xm_N_Back_Page_Background    : constant Xt_N_Resource_String;
   Xm_N_Back_Page_Foreground    : constant Xt_N_Resource_String;

   Xm_N_Binding_Type            : constant Xt_N_Resource_String;

   -- binding type values
   type Binding_Type is (None,
                         Pixmap,
                         Solid,
                         Spiral,
                         Pixmap_Overlap_Only);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Binding_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Binding_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Binding_Pixmap          : constant Xt_N_Resource_String;
   Xm_N_Binding_Width           : constant Xt_N_Resource_String;
   Xm_N_Major_Tab_Spacing       : constant Xt_N_Resource_String;
   Xm_N_Minor_Tab_Spacing       : constant Xt_N_Resource_String;
   Xm_N_Frame_Background        : constant Xt_N_Resource_String;
   Xm_N_Frame_Shadow_Thickness  : constant Xt_N_Resource_String;
   Xm_N_Inner_Margin_Height     : constant Xt_N_Resource_String;
   Xm_N_Inner_Margin_Width      : constant Xt_N_Resource_String;

   Xm_N_Current_Page_Number     : constant Xt_N_Resource_String;
   Xm_N_First_Page_Number       : constant Xt_N_Resource_String;
   Xm_N_Last_Page_Number        : constant Xt_N_Resource_String;
   Xm_N_Page_Changed_Callback   : constant Xt_N_Resource_String;

   Xm_N_Page_Number             : constant Xt_N_Resource_String;
   Xm_N_Notebook_Child_Type     : constant Xt_N_Resource_String;

   -- child type values
   type Notebook_Child_Type is (None,
                                Page,
                                Major_Tab,
                                Minor_Tab,
                                Status_Area,
                                Page_Scroller);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Notebook_Child_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Notebook_Child_Type);
   pragma Convention (C, Append_Get);


private

   for Binding_Type use (None => 0,
                         Pixmap   => 1,
                         Solid    => 2,
                         Spiral   => 3,
                         Pixmap_Overlap_Only => 4);
   for Binding_Type'Size use Interfaces.C.unsigned_char'Size;

   for Notebook_Child_Type use (None           => 0,
                                Page           => 1,
                                Major_Tab      => 2,
                                Minor_Tab      => 3,
                                Status_Area    => 4,
                                Page_Scroller  => 5);
   for Notebook_Child_Type'Size use Interfaces.C.unsigned_char'Size;


   c_const_Xm_Notebook_Widget_Class           : Widget_Class;

   pragma Import (C, c_const_Xm_Notebook_Widget_Class, "xmNotebookWidgetClass");

   Xm_Notebook_Widget_Class           : constant Widget_Class :=
    c_const_Xm_Notebook_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Back_Page_Placement     : constant Xt_N_Resource_String
      := To_Resource_String ("backPagePlacement");
   Xm_N_Back_Page_Number        : constant Xt_N_Resource_String
      := To_Resource_String ("backPageNumber");
   Xm_N_Back_Page_Size          : constant Xt_N_Resource_String
      := To_Resource_String ("backPageSize");
   Xm_N_Back_Page_Background    : constant Xt_N_Resource_String
      := To_Resource_String ("backPageBackground");
   Xm_N_Back_Page_Foreground    : constant Xt_N_Resource_String
      := To_Resource_String ("backPageForeground");

   Xm_N_Binding_Type            : constant Xt_N_Resource_String
      := To_Resource_String ("bindingType");

   Xm_N_Binding_Pixmap          : constant Xt_N_Resource_String
      := To_Resource_String ("bindingPixmap");
   Xm_N_Binding_Width           : constant Xt_N_Resource_String
      := To_Resource_String ("bindingWidth");
   Xm_N_Major_Tab_Spacing       : constant Xt_N_Resource_String
      := To_Resource_String ("majorTabSpacing");
   Xm_N_Minor_Tab_Spacing       : constant Xt_N_Resource_String
      := To_Resource_String ("minorTabSpacing");
   Xm_N_Frame_Background        : constant Xt_N_Resource_String
      := To_Resource_String ("frameBackground");
   Xm_N_Frame_Shadow_Thickness  : constant Xt_N_Resource_String
      := To_Resource_String ("frameShadowThickness");
   Xm_N_Inner_Margin_Height     : constant Xt_N_Resource_String
      := To_Resource_String ("innerMarginHeight");
   Xm_N_Inner_Margin_Width      : constant Xt_N_Resource_String
      := To_Resource_String ("innerMarginWidth");

   Xm_N_Current_Page_Number     : constant Xt_N_Resource_String
      := To_Resource_String ("currentPageNumber");
   Xm_N_First_Page_Number       : constant Xt_N_Resource_String
      := To_Resource_String ("firstPageNumber");
   Xm_N_Last_Page_Number        : constant Xt_N_Resource_String
      := To_Resource_String ("lastPageNumber");
   Xm_N_Page_Changed_Callback   : constant Xt_N_Resource_String
      := To_Resource_String ("pageChangedCallback");

   Xm_N_Page_Number             : constant Xt_N_Resource_String
      := To_Resource_String ("pageNumber");
   Xm_N_Notebook_Child_Type     : constant Xt_N_Resource_String
      := To_Resource_String ("notebookChildType");

-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Notebook;
