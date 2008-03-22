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
--          19 May 2001 Vadim Godunko: implement Xm_Get_Drag_Context
--          09 Feb 2002 H.-F. Vogt: delete pragma Convention (C,...) for
--                                  renamed Append_Get (for GNAT 3.14p
--                                  compatibility)
--
-------------------------------------------------------------------------------

with Xm_Widgets.Display,
     Xm_Widgets.Drop_Site_Manager;
package Xm_Widgets.Drag_Context is

   package Display     renames Xm_Widgets.Display;
   package Drop_Site_M renames Xm_Widgets.Drop_Site_Manager;


   Xm_Drag_Context_Class     : constant Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- XmIsDragContext
   --
   function Xm_Is_Drag_Context (W : in Widget) return Boolean;



   Motif_Drop_String   : constant String := "_MOTIF_DROP";
   Drag_Failure_String : constant String := "_MOTIF_DRAG_FAILURE";
   Drag_Success_String : constant String := "_MOTIF_DRAG_SUCCESS";


   type Completion_Status_Type is (Failure, Success);


   type Drag_Drop_Callback_Reason is
     (Top_Level_Enter,    Top_Level_Leave,
      Drag_Motion,
      Drop_Site_Enter,    Drop_Site_Leave,
      Drop_Start,         Drop_Finish,
      Drag_Drop_Finish,   Operation_Changed);
   for Drag_Drop_Callback_Reason use
     (Top_Level_Enter => 0,    Top_Level_Leave => 1,
      Drag_Motion => 2,
      Drop_Site_Enter => 3,    Drop_Site_Leave => 4,
      Drop_Start => 5,         Drop_Finish => 6,
      Drag_Drop_Finish => 7,   Operation_Changed => 8);


   type Top_Level_Enter_Callback_Struct_Part is record
      Screen              : X_Lib.Screen_Pointer;
      Window              : X_Lib.Window_ID;
      X, Y                : X_Lib.Position;
      Drag_Protocol_Style : Display.Drag_Protocol_Style_Type;
      ICC_Handle          : X_Lib.Atom;
   end record;

   type Top_Level_Leave_Callback_Struct_Part is record
      Screen              : X_Lib.Screen_Pointer;
      Window              : X_Lib.Window_ID;
   end record;

   type Drag_Motion_Callback_Struct_Part is record
      Operation        : Drop_Site_M.Operation_Type;
      Operations       : Drop_Site_M.Operations_Type;
      Drop_Site_Status : Drop_Site_M.Status_Type;
      X, Y             : X_Lib.Position;
   end record;

   subtype Drop_Site_Enter_Callback_Struct_Part is
      Drag_Motion_Callback_Struct_Part;

   type Drop_Site_Leave_Callback_Struct_Part is null record;

   type Drop_Start_Callback_Struct_Part is record
      Operation        : Drop_Site_M.Operation_Type;
      Operations       : Drop_Site_M.Operations_Type;
      Drop_Site_Status : Drop_Site_M.Status_Type;
      Drop_Action      : Drop_Site_M.Drop_Action_Type;
      X, Y             : X_Lib.Position;
      Window           : X_Lib.Window_ID;
      ICC_Handle       : X_Lib.Atom;
   end record;


   type Drop_Finish_Callback_Struct_Part is record
      Operation        : Drop_Site_M.Operation_Type;
      Operations       : Drop_Site_M.Operations_Type;
      Drop_Site_Status : Drop_Site_M.Status_Type;
      Drop_Action      : Drop_Site_M.Drop_Action_Type;
      Compl_Status     : Completion_Status_Type;        -- IN/OUT member
   end record;

   type Drag_Drop_Finish_Callback_Struct_Part is null record;

   type Operation_Changed_Callback_Struct_Part is record
      Operation        : Drop_Site_M.Operation_Type;
      Operations       : Drop_Site_M.Operations_Type;
      Drop_Site_Status : Drop_Site_M.Status_Type;
   end record;


   type Drag_Drop_Callback_Struct (Reason : Drag_Drop_Callback_Reason) is record
      Event       : X_Lib.X_Event_Pointer;
      Time_Stamp  : X_Lib.Server_Time;
      case Reason is
         when Top_Level_Enter =>
            Top_Level_Enter_Part   : Top_Level_Enter_Callback_Struct_Part;
         when Top_Level_Leave =>
            Top_Level_Leave_Part   : Top_Level_Leave_Callback_Struct_Part;
         when Drag_Motion =>
            Drag_Motion_Part       : Drag_Motion_Callback_Struct_Part;
         when Drop_Site_Enter =>
            Drop_Site_Enter_Part   : Drop_Site_Enter_Callback_Struct_Part;
         when Drop_Site_Leave =>
            Drop_Site_Leave_Part   : Drop_Site_Leave_Callback_Struct_Part;
         when Drop_Start =>
            Drop_Start_Part        : Drop_Start_Callback_Struct_Part;
         when Drop_Finish =>
            Drop_Finish_Part       : Drop_Finish_Callback_Struct_Part;
         when Drag_Drop_Finish =>
            Drag_Drop_Finish_Part  : Drag_Drop_Finish_Callback_Struct_Part;
         when Operation_Changed =>
            Operation_Changed_Part : Operation_Changed_Callback_Struct_Part;
      end case;
   end record;
   pragma Convention (C, Drag_Drop_Callback_Struct);
   type Drag_Drop_Callback_Struct_Access is
      access all Drag_Drop_Callback_Struct;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access (some members are IN/OUT) if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drag_Drop_Callback_Struct_Access;


   type Any_ICC_Callback_Struct is record
      Reason      : Drag_Drop_Callback_Reason;
      Event       : X_Lib.X_Event_Pointer;
      Time_Stamp  : X_Lib.Server_Time;
   end record;
   pragma Convention (C, Any_ICC_Callback_Struct);

   type Any_ICC_Callback_Struct_Access is
      access all Any_ICC_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Any_ICC_Callback_Struct_Access;



   -- -------------------------------------------------------------------------
   --
   -- XmGetDragContext
   --
   function Xm_Get_Drag_Context
     (Ref_Widget : in Widget;
      Timestamp  : in X_Lib.Server_Time)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- XmDragStart
   --
   function Xm_Drag_Start
     (W        : in  Widget;
      Event    : in  X_Lib.X_Event_Pointer;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- XmDragCancel
   --
   procedure Xm_Drag_Cancel (Drag_Context : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- XmTargetsAreCompatible
   --
   function Xm_Targets_Are_Compatible
     (Dpy            : in X_Lib.Display_Pointer;
      Export_Targets : in X_Lib.Atom_Array;
      Import_Targets : in X_Lib.Atom_Array)
      return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Blend_Model            : constant Xt_N_Resource_String :=
      To_Resource_String ("blendModel");

   type Blend_Model_Type is
     (Blend_All, Blend_State_Source, Blend_Just_Source, Blend_None);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Blend_Model_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Blend_Model_Type);
   pragma Convention (C, Append_Get);



   Xm_N_Client_Data            : constant Xt_N_Resource_String;
   Xm_N_Convert_Proc           : constant Xt_N_Resource_String;
   Xm_N_Cursor_Background      : constant Xt_N_Resource_String;
   Xm_N_Cursor_Foreground      : constant Xt_N_Resource_String;
   Xm_N_Drag_Drop_Finish_Callback : constant Xt_N_Resource_String;
   Xm_N_Drag_Motion_Callback   : constant Xt_N_Resource_String;
   Xm_N_Drag_Operations        : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drop_Site_M.Operations_Type)
      renames Drop_Site_M.Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Drop_Site_M.Operations_Type)
      renames Drop_Site_M.Append_Get;


   Xm_N_Drop_Finish_Callback   : constant Xt_N_Resource_String;
   Xm_N_Drop_Site_Enter_Callback : constant Xt_N_Resource_String;
   Xm_N_Drop_Site_Leave_Callback : constant Xt_N_Resource_String;
   Xm_N_Drop_Start_Callback    : constant Xt_N_Resource_String;
   Xm_N_Export_Targets         : constant Xt_N_Resource_String;

   -- if you use this procedure, don't set Xm_N_Num_Export_Targets, it will
   -- be set automatically
   procedure Append_Set_Export_Targets
    (List  : in out Arg_List;
     Value : in     X_Lib.Atom_Array);

   Xm_N_Incremental            : constant Xt_N_Resource_String;
   Xm_N_Invalid_Cursor_Foreground : constant Xt_N_Resource_String;
   Xm_N_None_Cursor_Foreground : constant Xt_N_Resource_String;
   Xm_N_Num_Export_Targets     : constant Xt_N_Resource_String;
   Xm_N_Operation_Changed_Callback : constant Xt_N_Resource_String;
   Xm_N_Operation_Cursor_Icon  : constant Xt_N_Resource_String;
   Xm_N_Source_Cursor_Icon     : constant Xt_N_Resource_String;
   Xm_N_Source_Pixmap_Icon     : constant Xt_N_Resource_String;
   Xm_N_State_Cursor_Icon      : constant Xt_N_Resource_String;
   Xm_N_Top_Level_Enter_Callback : constant Xt_N_Resource_String;
   Xm_N_Top_Level_Leave_Callback : constant Xt_N_Resource_String;
   Xm_N_Valid_Cursor_Foreground : constant Xt_N_Resource_String;




private

   for Blend_Model_Type use
     (Blend_All => 0, Blend_State_Source => 1, Blend_Just_Source => 2,
      Blend_None => 3);
   for Blend_Model_Type'Size use Interfaces.C.unsigned_char'Size;


   pragma Import (C, Xm_Get_Drag_Context, "XmGetDragContext");
   pragma Import (C, Xm_Drag_Cancel, "XmDragCancel");


   c_const_Xm_Drag_Context_Class     : Widget_Class;

   pragma Import (C, c_const_Xm_Drag_Context_Class, "xmDragContextClass");

   Xm_Drag_Context_Class     : constant Widget_Class :=
      c_const_Xm_Drag_Context_Class;

   for Completion_Status_Type use (Failure => 0, Success => 1);
   for Completion_Status_Type'Size use Interfaces.C.unsigned_char'Size;


   Xm_N_Client_Data            : constant Xt_N_Resource_String :=
      To_Resource_String ("clientData");
   Xm_N_Convert_Proc           : constant Xt_N_Resource_String :=
      To_Resource_String ("convertProc");
   Xm_N_Cursor_Background      : constant Xt_N_Resource_String :=
      To_Resource_String ("cursorBackground");
   Xm_N_Cursor_Foreground      : constant Xt_N_Resource_String :=
      To_Resource_String ("cursorForeground");
   Xm_N_Drag_Drop_Finish_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("dragDropFinishCallback");
   Xm_N_Drag_Motion_Callback   : constant Xt_N_Resource_String :=
      To_Resource_String ("dragMotionCallback");
   Xm_N_Drag_Operations        : constant Xt_N_Resource_String :=
      To_Resource_String ("dragOperations");

   Xm_N_Drop_Finish_Callback   : constant Xt_N_Resource_String :=
      To_Resource_String ("dropFinishCallback");
   Xm_N_Drop_Site_Enter_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("dropSiteEnterCallback");
   Xm_N_Drop_Site_Leave_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("dropSiteLeaveCallback");
   Xm_N_Drop_Start_Callback    : constant Xt_N_Resource_String :=
      To_Resource_String ("dropStartCallback");
   Xm_N_Export_Targets         : constant Xt_N_Resource_String :=
      To_Resource_String ("exportTargets");

   Xm_N_Incremental            : constant Xt_N_Resource_String :=
      To_Resource_String ("incremental");
   Xm_N_Invalid_Cursor_Foreground : constant Xt_N_Resource_String :=
      To_Resource_String ("invalidCursorForeground");
   Xm_N_None_Cursor_Foreground : constant Xt_N_Resource_String :=
      To_Resource_String ("NoneCursorForeground");
   Xm_N_Num_Export_Targets     : constant Xt_N_Resource_String :=
      To_Resource_String ("numExportTargets");
   Xm_N_Operation_Changed_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("operationChangedCallback");
   Xm_N_Operation_Cursor_Icon  : constant Xt_N_Resource_String :=
      To_Resource_String ("operationCursorIcon");
   Xm_N_Source_Cursor_Icon     : constant Xt_N_Resource_String :=
      To_Resource_String ("sourceCursorIcon");
   Xm_N_Source_Pixmap_Icon     : constant Xt_N_Resource_String :=
      To_Resource_String ("sourcePixmapIcon");
   Xm_N_State_Cursor_Icon      : constant Xt_N_Resource_String :=
      To_Resource_String ("stateCursorIcon");
   Xm_N_Top_Level_Enter_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("topLevelEnterCallback");
   Xm_N_Top_Level_Leave_Callback : constant Xt_N_Resource_String :=
      To_Resource_String ("topLevelLeaveCallback");
   Xm_N_Valid_Cursor_Foreground : constant Xt_N_Resource_String :=
      To_Resource_String ("validCursorForeground");

end Xm_Widgets.Drag_Context;
