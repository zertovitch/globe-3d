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
--                        corrected typo for Xm_N_Enable_Unselectable_Drag
--
-------------------------------------------------------------------------------

package Xm_Widgets.Display is


-- UseMotif2.0 Motif2.1
   type Xm_Drag_Start_Callback_Struct is record
      Reason : Callback_Reason;
      Event  : X_Lib.X_Event_Pointer;
      W      : Widget;
      Doit   : Boolean;
   end record;
   pragma Convention (C, Xm_Drag_Start_Callback_Struct);

   type Xm_Drag_Start_Callback_Struct_Access is
      access all Xm_Drag_Start_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Drag_Start_Callback_Struct_Access;


   type Xm_Display_Callback_Struct is record
      Reason        : Callback_Reason;
      Event         : X_Lib.X_Event_Pointer;
      Rendition     : Xm_Rendition;
      Font_Name     : X_Strings.X_String;
      Render_Table  : Xm_Render_Table;
      Tag           : Xm_String_Tag;
   end record;
   pragma Convention (C, Xm_Display_Callback_Struct);

   type Xm_Display_Callback_Struct_Access is
      access all Xm_Display_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Display_Callback_Struct_Access;

-- EndMotif2.0 Motif2.1



   -- ------------------------------------------------------------------------
   --
   --  XmDisplay Widget
   --
   function Xm_Get_Xm_Display (Display : in X_Lib.Display_Pointer) return Widget;


   -- -------------------------------------------------------------------------
   --
   -- resources
   --
-- UseMotif2.0 Motif2.1
   Xm_N_Default_Button_Emphasis       : constant Xt_N_Resource_String;

   type Default_Button_Emphasis_Type is
     (External_Highlight, Internal_Highlight);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Default_Button_Emphasis_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Default_Button_Emphasis_Type);
   pragma Convention (C, Append_Get);

-- EndMotif2.0 Motif2.1

   Xm_N_Default_Virtual_Bindings      : constant Xt_N_Resource_String;
   Xm_N_Drag_Initiator_Protocol_Style : constant Xt_N_Resource_String;
   Xm_N_Drag_Receiver_Protocol_Style  : constant Xt_N_Resource_String;

   type Drag_Protocol_Style_Type is
     (None, Drop_Only,
      Prefer_Preregister, Preregister,
      Prefer_Dynamic, Dynamic,
      Prefer_Receiver);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drag_Protocol_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Drag_Protocol_Style_Type);
   pragma Convention (C, Append_Get);

-- UseMotif2.0 Motif2.1
   Xm_N_Drag_Start_Callback           : constant Xt_N_Resource_String;
   Xm_N_Enable_Btn1_Transfer          : constant Xt_N_Resource_String;

   type Enable_Btn1_Transfer_Type is 
     (Off, Button2_Adjust, Button2_Transfer);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Enable_Btn1_Transfer_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Enable_Btn1_Transfer_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Enable_Button_Tab             : constant Xt_N_Resource_String;
   Xm_N_Enable_Drag_Icon              : constant Xt_N_Resource_String;
   Xm_N_Enable_Etched_In_Menu         : constant Xt_N_Resource_String;
   Xm_N_Enable_Toggle_Color           : constant Xt_N_Resource_String;
   Xm_N_Enable_Toggle_Visual          : constant Xt_N_Resource_String;
   Xm_N_Enable_Unselectable_Drag      : constant Xt_N_Resource_String;
   Xm_N_Enable_Warp                   : constant Xt_N_Resource_String;

   type Enable_Warp_Type is (On, Off);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Enable_Warp_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Enable_Warp_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Motif_Version                 : constant Xt_N_Resource_String;
   Xm_N_No_Font_Callback              : constant Xt_N_Resource_String;
   Xm_N_No_Rendition_Callback         : constant Xt_N_Resource_String;
   Xm_N_User_Data                     : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1


private

-- UseMotif2.0 Motif2.1
   for Default_Button_Emphasis_Type use
     (External_Highlight => 0, Internal_Highlight => 1);
   for Default_Button_Emphasis_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1

   for Drag_Protocol_Style_Type use
     (None => 0, Drop_Only => 1,
      Prefer_Preregister => 2, Preregister => 3,
      Prefer_Dynamic => 4, Dynamic => 5,
      Prefer_Receiver => 6);
   for Drag_Protocol_Style_Type'Size use Interfaces.C.unsigned_char'Size;

-- UseMotif2.0 Motif2.1
   for Enable_Btn1_Transfer_Type use
     (Off => 0, Button2_Adjust => 1, Button2_Transfer => 2);
   for Enable_Btn1_Transfer_Type'Size use Interfaces.C.unsigned_char'Size;

   for Enable_Warp_Type use (On => 0, Off => 1);
   for Enable_Warp_Type'Size use Interfaces.C.unsigned_char'Size;
-- EndMotif2.0 Motif2.1



   pragma Import (C, Xm_Get_Xm_Display, "XmGetXmDisplay");

-- UseMotif2.0 Motif2.1
   Xm_N_Default_Button_Emphasis       : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultButtonEmphasis");
-- EndMotif2.0 Motif2.1
   Xm_N_Default_Virtual_Bindings      : constant Xt_N_Resource_String :=
      To_Resource_String ("defaultVirtualBindings");
   Xm_N_Drag_Initiator_Protocol_Style : constant Xt_N_Resource_String :=
      To_Resource_String ("dragInitiatorProtocolStyle");
   Xm_N_Drag_Receiver_Protocol_Style  : constant Xt_N_Resource_String :=
      To_Resource_String ("dragReceiverProtocolStyle");
-- UseMotif2.0 Motif2.1
   Xm_N_Drag_Start_Callback           : constant Xt_N_Resource_String :=
      To_Resource_String ("dragStartCallback");
   Xm_N_Enable_Btn1_Transfer          : constant Xt_N_Resource_String :=
      To_Resource_String ("enableBtn1Transfer");
   Xm_N_Enable_Button_Tab             : constant Xt_N_Resource_String :=
      To_Resource_String ("enableButtonTab");
   Xm_N_Enable_Drag_Icon              : constant Xt_N_Resource_String :=
      To_Resource_String ("enableDragIcon");
   Xm_N_Enable_Etched_In_Menu         : constant Xt_N_Resource_String :=
      To_Resource_String ("enableEtchedInMenu");
   Xm_N_Enable_Toggle_Color           : constant Xt_N_Resource_String :=
      To_Resource_String ("enableToggleColor");
   Xm_N_Enable_Toggle_Visual          : constant Xt_N_Resource_String :=
      To_Resource_String ("enableToggleVisual");
   Xm_N_Enable_Unselectable_Drag      : constant Xt_N_Resource_String :=
      To_Resource_String ("enableUnselectableDrag");
   Xm_N_Enable_Warp                   : constant Xt_N_Resource_String :=
      To_Resource_String ("enableWarp");
   Xm_N_Motif_Version                 : constant Xt_N_Resource_String :=
      To_Resource_String ("motifVersion");
   Xm_N_No_Font_Callback              : constant Xt_N_Resource_String :=
      To_Resource_String ("noFontCallback");
   Xm_N_No_Rendition_Callback         : constant Xt_N_Resource_String :=
      To_Resource_String ("noRenditionCallback");
   Xm_N_User_Data                     : constant Xt_N_Resource_String
      := Xm_Widgets.Xm_N_User_Data;
-- EndMotif2.0 Motif2.1

end Xm_Widgets.Display;
