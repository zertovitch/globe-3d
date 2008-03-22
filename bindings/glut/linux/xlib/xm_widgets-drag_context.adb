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

with X_Toolkit.Internal,
     Ada.Unchecked_Conversion;
package body Xm_Widgets.Drag_Context is

-- ----------------------------------------------------------------------------
--
--   D R A G   C O N T E X T
--

   -- -------------------------------------------------------------------------
   --
   -- XmIsDragContext
   --
   function Xm_Is_Drag_Context (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Drag_Context_Class);
   end Xm_Is_Drag_Context;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access (some members are IN/OUT) if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drag_Drop_Callback_Struct_Access is

      type Callback_Reason_Pointer is
         access all Drag_Drop_Callback_Reason;

      function Go_Reason   is
         new Ada.Unchecked_Conversion (Xt_Pointer, Callback_Reason_Pointer);
      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Drag_Drop_Callback_Struct_Access);
      CS_Reason  : Callback_Reason_Pointer;
      use X_Lib;
   begin
      -- test if Pointer is Null Pointer
      if Pointer = Null_Xt_Pointer then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      -- test if reason is valid
      CS_Reason  := Go_Reason (Pointer);
      if not (CS_Reason.all in Drag_Drop_Callback_Reason'Range) then
         raise Xm_Error_Invalid_Callback_Reason;
      end if;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Callback_Struct;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Any_ICC_Callback_Struct_Access is

      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Any_ICC_Callback_Struct_Access);
      use X_Lib;
   begin
      -- test if Pointer is Null Pointer
      if Pointer = Null_Xt_Pointer then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Callback_Struct;


   -- -------------------------------------------------------------------------
   --
   -- XmDragStart
   --
   function Xm_Drag_Start
     (W        : in  Widget;
      Event    : in  X_Lib.X_Event_Pointer;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmDragStart
        (W        : in  Widget;
         Event    : in  X_Lib.X_Event_Pointer;
         Args     : in  X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal) return Widget;
      pragma Import (C, XmDragStart, "XmDragStart");
   begin
      return XmDragStart (W, Event,
                          X_Toolkit.Internal.Hook (Arglist),
                          Cardinal (Length (Arglist)));
   end Xm_Drag_Start;


   -- -------------------------------------------------------------------------
   --
   -- XmTargetsAreCompatible
   --
   function Xm_Targets_Are_Compatible
     (Dpy            : in X_Lib.Display_Pointer;
      Export_Targets : in X_Lib.Atom_Array;
      Import_Targets : in X_Lib.Atom_Array)
      return Boolean is
      function XmTargetsAreCompatible
        (Dpy                : in X_Lib.Display_Pointer;
         Export_Targets     : in System.Address;
         Num_Export_Targets : in Cardinal;
         Import_Targets     : in System.Address;
         Num_Import_Targets : in Cardinal)
         return Xt_Boolean;
      pragma Import (C, XmTargetsAreCompatible, "XmTargetsAreCompatible");
   begin
      return To_Boolean (XmTargetsAreCompatible (Dpy,
                                                 Export_Targets (Export_Targets'First)'Address,
                                                 Cardinal (Export_Targets'Length),
                                                 Import_Targets (Import_Targets'First)'Address,
                                                 Cardinal (Import_Targets'Length)));
   end Xm_Targets_Are_Compatible;


   function To_Integer is
      new Ada.Unchecked_Conversion (Blend_Model_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Blend_Model_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Blend_Model_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;


   procedure Append_Set_Export_Targets
    (List  : in out Arg_List;
     Value : in     X_Lib.Atom_Array) is
   begin
      -- there should be an error if a null array is given in value
      if Value'Length > 0 then
         Append_Set (List => List,
                     Name => Xm_N_Export_Targets,
                     Value => Value (Value'First)'Address);
         Append_Set (List => List,
                     Name => Xm_N_Num_Export_Targets,
                     Value => Integer (Value'Length));
      end if;
   end Append_Set_Export_Targets;

end Xm_Widgets.Drag_Context;
