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

with X_Toolkit.Internal;
package body Xm_Widgets.Drop_Transfer is

   -- -------------------------------------------------------------------------
   --
   -- XmIsDropTransfer
   --
   function Xm_Is_Drop_Transfer (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Drop_Transfer_Object_Class);
   end Xm_Is_Drop_Transfer;


   -- -------------------------------------------------------------------------
   --
   -- XmDropTransferStart
   --
   function Xm_Drop_Transfer_Start
     (Ref_Widget : in Widget;
      Args       : in Arg_List := Null_Arg_List)
      return Widget is
      function XmDropTransferStart
        (Ref_Widget : in Widget;
         Args       : in X_Toolkit.Internal.Arg_Rec_Access;
         Arg_Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmDropTransferStart, "XmDropTransferStart");
   begin
      return XmDropTransferStart (Ref_Widget,
                                  X_Toolkit.Internal.Hook (Args),
                                  Cardinal (Length (Args)));
   end Xm_Drop_Transfer_Start;


   -- -------------------------------------------------------------------------
   --
   -- Xm_Drop_Transfer_Add
   --
   procedure Xm_Drop_Transfer_Add
     (W         : in Widget;
      Transfers : in Xm_Drop_Transfer_Entry_Array) is
      procedure XmDropTransferAdd
        (W             : in Widget;
         Transfers     : in System.Address;
         Num_Transfers : in Cardinal);
      pragma Import (C, XmDropTransferAdd, "XmDropTransferAdd");
   begin
      if Transfers'Length > 0 then
         XmDropTransferAdd (W, Transfers'Address, Cardinal (Transfers'Length));
      end if;
   end Xm_Drop_Transfer_Add;


   procedure Append_Set_Drop_Transfers
    (List  : in out Arg_List;
     Value : in     Xm_Drop_Transfer_Entry_Array) is
   begin
      -- there should be an error if a null array is given in value
      if Value'Length > 0 then
         Append_Set (List => List,
                     Name => Xm_N_Drop_Transfers,
                     Value => Value (Value'First)'Address);
         Append_Set (List => List,
                     Name => Xm_N_Num_Drop_Transfers,
                     Value => Integer (Value'Length));
      end if;
   end Append_Set_Drop_Transfers;



end Xm_Widgets.Drop_Transfer;
