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

package Xm_Widgets.Drop_Transfer is

   Xm_Drop_Transfer_Object_Class : constant Widget_Class;

   -- -------------------------------------------------------------------------
   --
   -- XmIsDropTransfer
   --
   function Xm_Is_Drop_Transfer (W : in Widget) return Boolean;


   type Xm_Drop_Transfer_Entry_Rec is record
      Client_Data : Xt_Pointer;
      Target      : X_Lib.Atom;
   end record;
   pragma Convention (C, Xm_Drop_Transfer_Entry_Rec); 

   type Xm_Drop_Transfer_Entry_Array is
      array (Natural range <>) of aliased Xm_Drop_Transfer_Entry_Rec;


   -- -------------------------------------------------------------------------
   --
   -- XmDropTransferStart
   --
   function Xm_Drop_Transfer_Start
     (Ref_Widget : in Widget;
      Args       : in Arg_List := Null_Arg_List)
      return Widget;


   -- -------------------------------------------------------------------------
   --
   -- XmDropTransferAdd
   --
   procedure Xm_Drop_Transfer_Add
     (W         : in Widget;
      Transfers : in Xm_Drop_Transfer_Entry_Array);


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Drop_Transfers         : constant Xt_N_Resource_String;

   -- use the following function to set drop transfers
   -- the resource Xm_N_Num_Drop_Transfers is automatically set!
   procedure Append_Set_Drop_Transfers
    (List  : in out Arg_List;
     Value : in     Xm_Drop_Transfer_Entry_Array);

   -- Xm_N_Incremental            : constant Xt_N_Resource_String;
   Xm_N_Num_Drop_Transfers     : constant Xt_N_Resource_String;
   Xm_N_Transfer_Proc          : constant Xt_N_Resource_String;
   Xm_N_Transfer_Status        : constant Xt_N_Resource_String;

   -- values for Xm_N_Transfer_Status
   Success : constant Boolean := True;
   Failure : constant Boolean := False;

private

   c_const_Xm_Drop_Transfer_Object_Class     : Widget_Class;

   pragma Import (C, c_const_Xm_Drop_Transfer_Object_Class, "xmDropTransferObjectClass");

   Xm_Drop_Transfer_Object_Class : constant Widget_Class :=
      c_const_Xm_Drop_Transfer_Object_Class;

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Drop_Transfers         : constant Xt_N_Resource_String :=
      To_Resource_String ("dropTransfers");

   -- Xm_N_Incremental            : constant Xt_N_Resource_String :=
   --    To_Resource_String ("incremental");
   Xm_N_Num_Drop_Transfers     : constant Xt_N_Resource_String :=
      To_Resource_String ("numDropTransfers");
   Xm_N_Transfer_Proc          : constant Xt_N_Resource_String :=
      To_Resource_String ("transferProc");
   Xm_N_Transfer_Status        : constant Xt_N_Resource_String :=
      To_Resource_String ("transferStatus");

end Xm_Widgets.Drop_Transfer;
