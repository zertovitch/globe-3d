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

package Xm_Widgets.Drop_Site_Manager is

   use X_Lib;

   Xm_Drop_Site_Manager_Object_Class : constant Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- XmIsDropSiteManager
   --
   function Xm_Is_Drop_Site_Manager (W : in Widget) return Boolean;


   -- use the correct names as documented in DropSMgr.h
   type Status_Type is (No_Drop_Site, Drop_Site_Invalid, Drop_Site_Valid);

   -- Operations available for Drag and Drop
   type Operation_Type is (No_Op, Move, Copy, Link);
   for  Operation_Type use (No_Op => 0, Move => 1, Copy => 2, Link => 4);
   for  Operation_Type'Size use Interfaces.C.unsigned_char'Size;


   type Operations_Type is record
      Move  : Boolean;
      Copy  : Boolean;
      Link  : Boolean;
   end record;
   for Operations_Type use record
-- UseLittleEndian
      Move  at 0 range 0 .. 0;
      Copy  at 0 range 1 .. 1;
      Link  at 0 range 2 .. 2;
-- NotLittleEndian
--!       Move  at 0 range 7 .. 7;
--!       Copy  at 0 range 6 .. 6;
--!       Link  at 0 range 5 .. 5;
-- EndLittleEndian
   end record;
   pragma Pack (Operations_Type);
   for  Operations_Type'Size use Interfaces.C.unsigned_char'Size;


   type Drop_Action_Type is (Drop, Drop_Help, Drop_Cancel, Drop_Interrupt);
   for Drop_Action_Type use
     (Drop => 0, Drop_Help => 1, Drop_Cancel => 2, Drop_Interrupt => 3);
   for Drop_Action_Type'Size use Interfaces.C.unsigned_char'Size;


   subtype Drop_Site_Drop_Action_Type is
      Drop_Action_Type range Drop .. Drop_Help;


   type Drop_Site_Callback_Reason is
     (Drop_Site_Leave_Message, Drop_Site_Enter_Message,
      Drop_Site_Motion_Message, Drop_Message, Operation_Changed);
   for Drop_Site_Callback_Reason use
     (Drop_Site_Leave_Message => 1, Drop_Site_Enter_Message => 2,
      Drop_Site_Motion_Message => 3, Drop_Message => 4, Operation_Changed => 8);


   type Drag_Proc_Callback_Struct is record
      Reason       : Drop_Site_Callback_Reason;
      Event        : X_Lib.X_Event_Pointer;
      Time_Stamp   : X_Lib.Server_Time;
      Drag_Context : Widget;
      X, Y         : X_Lib.Position;
      Status       : Status_Type;      -- IN/OUT member
      Operation    : Operation_Type;   -- IN/OUT member
      Operations   : Operations_Type;  -- IN OUT member
      Animate      : Boolean;          -- OUT member
   end record;
   pragma Convention (C, Drag_Proc_Callback_Struct);
   for Drag_Proc_Callback_Struct'Size use 192;
   type Drag_Proc_Callback_Struct_Access is
      access all Drag_Proc_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access (some members are IN/OUT) if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drag_Proc_Callback_Struct_Access;


   type Drop_Proc_Callback_Struct is record
      Reason       : Drop_Site_Callback_Reason;
      Event        : X_Lib.X_Event_Pointer;
      Time_Stamp   : X_Lib.Server_Time;
      Drag_Context : Widget;
      X, Y         : X_Lib.Position;
      Status       : Status_Type;                -- IN/OUT member
      Operation    : Operation_Type;             -- IN/OUT member
      Operations   : Operations_Type;            -- IN/OUT member
      Drop_Action  : Drop_Site_Drop_Action_Type; -- IN/OUT member
   end record;
   pragma Convention (C, Drop_Proc_Callback_Struct);
   for Drop_Proc_Callback_Struct'Size use 192;
   type Drop_Proc_Callback_Struct_Access is
      access all Drop_Proc_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access (some members are IN/OUT) if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drop_Proc_Callback_Struct_Access;



   type Xm_Drop_Site_Visuals_Rec is record
      Background           : Pixel;
      Foreground           : Pixel;
      Top_Shadow_Color     : Pixel;
      Top_Shadow_Pixmap    : Pixmap_ID;
      Bottom_Shadow_Color  : Pixel;
      Bottom_Shadow_Pixmap : Pixmap_ID;
      Shadow_Thickness     : Dimension;
      Highlight_Color      : Pixel;
      Highlight_Pixmap     : Pixmap_ID;
      Highlight_Thickness  : Dimension;
      Border_Width         : Dimension;
   end record;
   pragma Convention (C, Xm_Drop_Site_Visuals_Rec);

   type Xm_Drop_Site_Visuals_Rec_Access is
      access all Xm_Drop_Site_Visuals_Rec;


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteRegister
   --
   procedure Xm_Drop_Site_Register
     (W       : in Widget;
      Args    : in Arg_List := Null_Arg_List);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteUnregister
   --
   procedure Xm_Drop_Site_Unregister (W : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteRegistered
   --
   function Xm_Drop_Site_Registered (W : in Widget) return Boolean;


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteStartUpdate
   --
   procedure Xm_Drop_Site_Start_Update (Ref_Widget : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteUpdate
   --
   procedure Xm_Drop_Site_Update
     (Enclosing_Widget : in Widget;
      Args             : in Arg_List := Null_Arg_List);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteEndUpdate
   --
   procedure Xm_Drop_Site_End_Update (Ref_Widget : in Widget);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteRetrieve
   --
   procedure Xm_Drop_Site_Retrieve
     (Enclosing_Widget : in Widget;
      Args             : in Arg_List := Null_Arg_List);


   -- -------------------------------------------------------------------------
   --
   -- Xm_Drop_Site_Query_Stacking_Order
   --
   Error_Query_Stacking_Order : exception;
   procedure Xm_Drop_Site_Query_Stacking_Order
     (W        : in Widget;
      Parent   : out Widget;
      Children : out Widget_List);


   -- -------------------------------------------------------------------------
   --
   -- Xm_Drop_Site_Configure_Stacking_Order
   --

   type Stack_Position_Type is (Above, Below);

   procedure Xm_Drop_Site_Configure_Stacking_Order
     (W          : in Widget;
      Sibling    : in Widget;
      Stack_Mode : in Stack_Position_Type);


   -- -------------------------------------------------------------------------
   --
   -- Xm_Drop_Site_Get_Active_Visuals
   --
   function Xm_Drop_Site_Get_Active_Visuals (W : in Widget) return Xm_Drop_Site_Visuals_Rec_Access;

   
   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Animation_Mask         : constant Xt_N_Resource_String;
   Xm_N_Animation_Pixmap       : constant Xt_N_Resource_String;
   Xm_N_Animation_Pixmap_Depth : constant Xt_N_Resource_String;
   Xm_N_Animation_Style        : constant Xt_N_Resource_String;

   type Animation_Style_Type is (None, Pixmap, Shadow_In, Shadow_Out, Highlight);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Animation_Style_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Animation_Style_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Drag_Proc              : constant Xt_N_Resource_String;
   Xm_N_Drop_Proc              : constant Xt_N_Resource_String;
   Xm_N_Drop_Rectangles        : constant Xt_N_Resource_String;
   Xm_N_Drop_Site_Activity     : constant Xt_N_Resource_String;

   type Drop_Site_Activity_Type is (Active, Inactive, Ignore);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drop_Site_Activity_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Drop_Site_Activity_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Drop_Site_Operations   : constant Xt_N_Resource_String;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Operations_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Operations_Type);
   pragma Convention (C, Append_Get);


   Xm_N_Drop_Site_Type         : constant Xt_N_Resource_String;

   type Drop_Site_Type is (Simple, Composite,
                           Simple_Clip_Only, Composite_Clip_Only);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drop_Site_Type);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Drop_Site_Type);
   pragma Convention (C, Append_Get);

   Xm_N_Import_Targets         : constant Xt_N_Resource_String;
   Xm_N_Num_Drop_Rectangles    : constant Xt_N_Resource_String;
   Xm_N_Num_Import_Targets     : constant Xt_N_Resource_String;

private

   for Status_Type use (No_Drop_Site => 1, Drop_Site_Invalid => 2,
                        Drop_Site_Valid => 3);
   for Status_Type'Size use Interfaces.C.unsigned_char'Size;

   for Stack_Position_Type use (Above => 0, Below => 1);
   for Stack_Position_Type'Size use Cardinal'Size;

   for Animation_Style_Type use (None => 0, Pixmap => 1, Shadow_In => 2, Shadow_Out => 3, Highlight => 4);
   for Animation_Style_Type'Size use Interfaces.C.unsigned_char'Size;

   for Drop_Site_Activity_Type use (Active => 0, Inactive => 1, Ignore => 2);
   for Drop_Site_Activity_Type'Size use Interfaces.C.unsigned_char'Size;

   for Drop_Site_Type use (Simple => 0, Composite => 1,
                           Simple_Clip_Only => 128, Composite_Clip_Only => 129);
   for Drop_Site_Type'Size use Interfaces.C.unsigned_char'Size;


   pragma Import (C, Xm_Drop_Site_Unregister, "XmDropSiteUnregister");
   pragma Import (C, Xm_Drop_Site_Start_Update, "XmDropSiteStartUpdate");
   pragma Import (C, Xm_Drop_Site_End_Update, "XmDropSiteEndUpdate");
   pragma Import (C, Xm_Drop_Site_Configure_Stacking_Order, "XmDropSiteConfigureStackingOrder");
   pragma Import (C, Xm_Drop_Site_Get_Active_Visuals, "XmDropSiteGetActiveVisuals");

   c_const_Xm_Drop_Site_Manager_Object_Class : Widget_Class;

   pragma Import (C, c_const_Xm_Drop_Site_Manager_Object_Class, "xmDropSiteManagerObjectClass");

   Xm_Drop_Site_Manager_Object_Class : constant Widget_Class :=
      c_const_Xm_Drop_Site_Manager_Object_Class;

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Animation_Mask         : constant Xt_N_Resource_String :=
      To_Resource_String ("animationMask");
   Xm_N_Animation_Pixmap       : constant Xt_N_Resource_String :=
      To_Resource_String ("animationPixmap");
   Xm_N_Animation_Pixmap_Depth : constant Xt_N_Resource_String :=
      To_Resource_String ("animationPixmapDepth");
   Xm_N_Animation_Style        : constant Xt_N_Resource_String :=
      To_Resource_String ("animationStyle");

   Xm_N_Drag_Proc              : constant Xt_N_Resource_String :=
      To_Resource_String ("dragProc");
   Xm_N_Drop_Proc              : constant Xt_N_Resource_String :=
      To_Resource_String ("dropProc");
   Xm_N_Drop_Rectangles        : constant Xt_N_Resource_String :=
      To_Resource_String ("dropRectangles");
   Xm_N_Drop_Site_Activity     : constant Xt_N_Resource_String :=
      To_Resource_String ("dropSiteActivity");

   Xm_N_Drop_Site_Operations   : constant Xt_N_Resource_String :=
      To_Resource_String ("dropSiteOperations");

   Xm_N_Drop_Site_Type         : constant Xt_N_Resource_String :=
      To_Resource_String ("dropSiteType");

   Xm_N_Import_Targets         : constant Xt_N_Resource_String :=
      To_Resource_String ("importTargets");
   Xm_N_Num_Drop_Rectangles     : constant Xt_N_Resource_String :=
      To_Resource_String ("numDropRectangles");
   Xm_N_Num_Import_Targets      : constant Xt_N_Resource_String :=
      To_Resource_String ("numImportTargets");

end Xm_Widgets.Drop_Site_Manager;
