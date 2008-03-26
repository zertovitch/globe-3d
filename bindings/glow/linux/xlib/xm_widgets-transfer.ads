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

with Generic_List_Types;
package Xm_Widgets.Transfer is

   type Transfer_Status_Type is (Succeed, Fail, Continue, Default);

   type Operation_Type is (Move, Copy, Link, Other);

   type Selection_Type is (Default, Incremental, Persist, Snapshot, Transact);

   type Converting_Flags is record
      Same     : Boolean;
      Transact : Boolean;
      Partial  : Boolean;
   end record;
   for Converting_Flags use record
-- UseLittleEndian
      Same     at 0 range 0 .. 0;
      Transact at 0 range 1 .. 1;
      Partial  at 0 range 2 .. 2;
-- NotLittleEndian
--!       Same      at 0 range 2 .. 2;
--!       Transact  at 0 range 1 .. 1;
--!       Partial   at 0 range 0 .. 0;
-- EndLittleEndian
   end record;
   for Converting_Flags'Size use 3;

   Converting_None : constant Converting_Flags := (others => False);


   type Convert_Status_Type is (Default, More, Merge, Refuse, Done);

   type Transfer_ID_Type is private;


   type Xm_Convert_Callback_Struct is record
      Reason        : Callback_Reason;
      Event         : X_Lib.X_Event_Pointer;
      Selection     : X_Lib.Atom;
      Target        : X_Lib.Atom;
      Source_Data,
      Location_Data : Xt_Pointer;
      Flags         : Converting_Flags;
      Parm          : Xt_Pointer;
      Parm_Format   : X_Lib.Data_Format_Type;
      Parm_Length   : Interfaces.C.unsigned_long;
      Parm_Type     : X_Lib.Atom;
      Status        : Convert_Status_Type;     --  IN/OUT member
      Value         : Xt_Pointer;              --  IN/OUT member
      Value_Type    : X_Lib.Atom;              --  IN/OUT member
      Format        : X_Lib.Data_Format_Type;  --  IN/OUT member
      Length        : Interfaces.C.unsigned_long;           --  IN/OUT member
   end record;
   type Xm_Convert_Callback_Struct_Access is
      access all Xm_Convert_Callback_Struct;

   -- try to interpret the generic Pointer Xt_Pointer as
   -- an access value to the callback struct
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Convert_Callback_Struct_Access;


   type Xm_Destination_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      Selection        : X_Lib.Atom;
      Op               : Operation_Type;
      Flags            : Converting_Flags;
      Transfer_Id      : Transfer_ID_Type;
      Destination_Data : Xt_Pointer;
      Location_Data    : Xt_Pointer;
      Time             : X_Lib.Server_Time;
   end record;
   type Xm_Destination_Callback_Struct_Access is
      access all Xm_Destination_Callback_Struct;

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Destination_Callback_Struct_Access;


   type Xm_Selection_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      Selection        : X_Lib.Atom;
      Target           : X_Lib.Atom;
      Target_Type      : X_Lib.Atom;
      Transfer_Id      : Transfer_ID_Type;
      Flags            : Selection_Type;
      Remaining        : Integer;
      Value            : Xt_Pointer;
      Length           : Interfaces.C.unsigned_long;
      Format           : X_Lib.Data_Format_Type;
   end record;
   type Xm_Selection_Callback_Struct_Access is
      access all Xm_Selection_Callback_Struct;

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Selection_Callback_Struct_Access;


   type Xm_Transfer_Done_Callback_Struct is record
      Reason           : Callback_Reason;
      Event            : X_Lib.X_Event_Pointer;
      Selection        : X_Lib.Atom;
      Transfer_Id      : Transfer_ID_Type;
      Status           : Transfer_Status_Type;
      Client_Data      : Xt_Pointer;
   end record;
   type Xm_Transfer_Done_Callback_Struct_Access is
      access all Xm_Transfer_Done_Callback_Struct;


   -- convert callback handling
   --
   type Xm_Convert_Callback_Proc is access procedure
     (W         : in     Widget;
      Closure   : in     Xt_Pointer;
      Call_Data : in out Xm_Convert_Callback_Struct);
   pragma Convention (C, Xm_Convert_Callback_Proc);


   type Convert_Callback_Rec is record
      Callback  : Xm_Convert_Callback_Proc;
      Closure   : Xt_Pointer;
   end record;

   Null_Convert_Callback_Rec : constant Convert_Callback_Rec :=
      (Callback  => null,
       Closure   => Null_Xt_Pointer);

   type Convert_Callback_List is private;
   Null_Convert_Callback_List : constant Convert_Callback_List;

   function Length (List : in Convert_Callback_List) return Natural;

   procedure Append (List : in out Convert_Callback_List;
                     Rec  : in     Convert_Callback_Rec);

   procedure Append (List      : in out Convert_Callback_List;
                     Callback  : in     Xm_Convert_Callback_Proc;
		     Closure   : in     Xt_Pointer);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Convert_Callback_List);


   procedure Xt_Add_Callback
     (To          : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Convert_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer);

   procedure Xt_Remove_Callback
     (From        : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Convert_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer);



   -- destination callback handling
   --
   type Xm_Destination_Callback_Proc is access procedure
     (W         : in     Widget;
      Closure   : in     Xt_Pointer;
      Call_Data : in out Xm_Destination_Callback_Struct);
   pragma Convention (C, Xm_Destination_Callback_Proc);


   type Destination_Callback_Rec is record
      Callback  : Xm_Destination_Callback_Proc;
      Closure   : Xt_Pointer;
   end record;

   Null_Destination_Callback_Rec : constant Destination_Callback_Rec :=
      (Callback  => null,
       Closure   => Null_Xt_Pointer);

   type Destination_Callback_List is private;
   Null_Destination_Callback_List : constant Destination_Callback_List;

   function Length (List : in Destination_Callback_List) return Natural;

   procedure Append (List : in out Destination_Callback_List;
                     Rec  : in     Destination_Callback_Rec);

   procedure Append (List      : in out Destination_Callback_List;
                     Callback  : in     Xm_Destination_Callback_Proc;
		     Closure   : in     Xt_Pointer);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Destination_Callback_List);


   procedure Xt_Add_Callback
     (To          : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Destination_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer);

   procedure Xt_Remove_Callback
     (From        : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Destination_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer);



   type Xm_Selection_Finished_Proc is access procedure
     (W         : in     Widget;
      Op        : in     Operation_Type;
      Call_Data : in out Xm_Transfer_Done_Callback_Struct);
   pragma Convention (C, Xm_Selection_Finished_Proc);


   -- completes an already-initiated data transfer
   --
   procedure Xm_Transfer_Done
     (Transfer_Id : in Transfer_ID_Type;
      Status      : in Transfer_Status_Type);


   -- 
   procedure Xm_Transfer_Value
     (Transfer_Id : in Transfer_ID_Type;
      Target      : in X_Lib.Atom;
      Proc        : in Xt_Callback_Proc;
      Client_Data : in Xt_Pointer;
      Timestamp   : in X_Lib.Server_Time);


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is an 8 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_8_Array_Type;
      Parm_Type   : in X_Lib.Atom);


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is a 16 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_16_Array_Type;
      Parm_Type   : in X_Lib.Atom);


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is a 32 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_32_Array_Type;
      Parm_Type   : in X_Lib.Atom);


   -- begins a MULTIPLE transfer
   --
   procedure Xm_Transfer_Start_Request (Transfer_Id : in Transfer_ID_Type);


   -- marks the end of a MULTIPLE request started by Xm_Transfer_Start_Request
   --
   procedure Xm_Transfer_Send_Request
     (Transfer_Id : in Transfer_ID_Type;
      Timestamp   : in X_Lib.Server_Time);


private

   type Transfer_ID_Type is new Xt_Pointer;

   for Xm_Convert_Callback_Struct use record
      Reason        at  0 range  0 .. 31;
      Event         at  4 range  0 .. 31;
      Selection     at  8 range  0 .. 31;
      Target        at 12 range  0 .. 31;
      Source_Data   at 16 range  0 .. 31;
      Location_Data at 20 range  0 .. 31;
-- UseLittleEndian
      Flags         at 24 range  0 ..  2;
-- NotLittleEndian
--!       Flags         at 24 range 29 .. 31;
-- EndLittleEndian
      Parm          at 28 range  0 .. 31;
      Parm_Format   at 32 range  0 .. 31;
      Parm_Length   at 36 range  0 .. 31;
      Parm_Type     at 40 range  0 .. 31;
      Status        at 44 range  0 .. 31;
      Value         at 48 range  0 .. 31;
      Value_Type    at 52 range  0 .. 31;
      Format        at 56 range  0 .. 31;
      Length        at 60 range  0 .. 31;
   end record;
   pragma Convention (C, Xm_Convert_Callback_Struct);

   for Xm_Destination_Callback_Struct use record
      Reason           at  0 range  0 .. 31;
      Event            at  4 range  0 .. 31;
      Selection        at  8 range  0 .. 31;
      Op               at 12 range  0 .. 31;
-- UseLittleEndian
      Flags            at 16 range  0 ..  2;
-- NotLittleEndian
--!       Flags            at 16 range 29 .. 31;
-- EndLittleEndian
      Transfer_Id      at 20 range  0 .. 31;
      Destination_Data at 24 range  0 .. 31;
      Location_Data    at 28 range  0 .. 31;
      Time             at 32 range  0 .. 31;
   end record;
   pragma Convention (C, Xm_Destination_Callback_Struct);

   for Xm_Selection_Callback_Struct use record
      Reason           at  0 range  0 .. 31;
      Event            at  4 range  0 .. 31;
      Selection        at  8 range  0 .. 31;
      Target           at 12 range  0 .. 31;
      Target_Type      at 16 range  0 .. 31;
      Transfer_Id      at 20 range  0 .. 31;
-- UseLittleEndian
      Flags            at 24 range  0 ..  2;
-- NotLittleEndian
--!       Flags            at 24 range 29 .. 31;
-- EndLittleEndian
      Remaining        at 28 range  0 .. 31;
      Value            at 32 range  0 .. 31;
      Length           at 36 range  0 .. 31;
      Format           at 40 range  0 .. 31;
   end record;
   pragma Convention (C, Xm_Selection_Callback_Struct);

   pragma Convention (C, Xm_Transfer_Done_Callback_Struct);

   for Transfer_Status_Type use (Succeed => 0, Fail => 1, Continue => 2, Default => 3);
   for Operation_Type use (Move => 1, Copy => 2, Link => 3, Other => 4);
   for Selection_Type use (Default => 0, Incremental => 1, Persist => 2, Snapshot => 3, Transact => 4);
   for Convert_Status_Type use (Default => 0, More => 1, Merge => 2, Refuse => 3, Done => 4);


   package Convert_Callback_Lists is
      new Generic_List_Types (Convert_Callback_Rec);
   type Convert_Callback_List is
      new Convert_Callback_Lists.Unbounded_List;

   Null_Convert_Callback_List : constant Convert_Callback_List
      := Convert_Callback_List (Convert_Callback_Lists.Null_Unbounded_List);


   package Destination_Callback_Lists is
      new Generic_List_Types (Destination_Callback_Rec);
   type Destination_Callback_List is
      new Destination_Callback_Lists.Unbounded_List;

   Null_Destination_Callback_List : constant Destination_Callback_List
      := Destination_Callback_List (Destination_Callback_Lists.Null_Unbounded_List);



   pragma Import (C, Xm_Transfer_Done, "XmTransferDone");
   pragma Import (C, Xm_Transfer_Value, "XmTransferValue");
   pragma Import (C, Xm_Transfer_Start_Request, "XmTransferStartRequest");
   pragma Import (C, Xm_Transfer_Send_Request, "XmTransferSendRequest");

end Xm_Widgets.Transfer;
