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

with Ada.Unchecked_Conversion,
     Interfaces.C.Pointers,
     X_Toolkit.Internal;
package body Xm_Widgets.Drop_Site_Manager is

   -- -------------------------------------------------------------------------
   --
   -- XmIsDropSiteManager
   --
   function Xm_Is_Drop_Site_Manager (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Drop_Site_Manager_Object_Class);
   end Xm_Is_Drop_Site_Manager;
   pragma Inline (Xm_Is_Drop_Site_Manager);


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drag_Proc_Callback_Struct_Access is

      type Callback_Reason_Pointer is
         access all Drop_Site_Callback_Reason;

      function Go_Reason   is
         new Ada.Unchecked_Conversion (Xt_Pointer, Callback_Reason_Pointer);
      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Drag_Proc_Callback_Struct_Access);
      CS_Reason  : Callback_Reason_Pointer;
   begin
      -- test if Pointer is Null Pointer
      if Pointer = Null_Xt_Pointer then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      -- test if reason is valid
      CS_Reason  := Go_Reason (Pointer);
      case CS_Reason.all is
         when Drop_Site_Leave_Message | Drop_Site_Enter_Message |
              Drop_Site_Motion_Message | Operation_Changed =>
            null; -- OK
	 when others =>
            raise Xm_Error_Invalid_Callback_Reason;
      end case;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Callback_Struct;


   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Drop_Proc_Callback_Struct_Access is

      type Callback_Reason_Pointer is
         access all Drop_Site_Callback_Reason;

      function Go_Reason   is
         new Ada.Unchecked_Conversion (Xt_Pointer, Callback_Reason_Pointer);
      function Go_Callback is
         new Ada.Unchecked_Conversion (Xt_Pointer, Drop_Proc_Callback_Struct_Access);
      CS_Reason  : Callback_Reason_Pointer;
   begin
      -- test if Pointer is Null Pointer
      if Pointer = Null_Xt_Pointer then
         raise Xm_Error_No_Callback_Struct_Pointer;
      end if;
      -- test if reason is valid
      CS_Reason  := Go_Reason (Pointer);
      case CS_Reason.all is
         when Drop_Message =>
            null; -- OK
	 when others =>
            raise Xm_Error_Invalid_Callback_Reason;
      end case;
      return Go_Callback (Pointer);
   exception
      when others =>
         raise Xm_Error_No_Callback_Struct_Pointer;
   end To_Callback_Struct;


   -- -------------------------------------------------------------------------
   --
   -- Xm_Drop_Site_Register
   --
   procedure Xm_Drop_Site_Register
     (W       : in Widget;
      Args    : in Arg_List := Null_Arg_List) is
      procedure XmDropSiteRegister
        (W       : in Widget;
         Args    : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal);
      pragma Import (C, XmDropSiteRegister, "XmDropSiteRegister");
   begin
      XmDropSiteRegister (W,
                          X_Toolkit.Internal.Hook (Args),
                          Cardinal (Length (Args)));
   end Xm_Drop_Site_Register;


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteRegistered
   --
   function Xm_Drop_Site_Registered (W : in Widget) return Boolean is
      function XmDropSiteRegistered (W : in Widget) return X_Toolkit.Xt_Boolean;
      pragma Import (C, XmDropSiteRegistered, "XmDropSiteRegistered");
   begin
      return To_Boolean (XmDropSiteRegistered (W));
   end Xm_Drop_Site_Registered;
   pragma Inline (Xm_Drop_Site_Registered);


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteStartUpdate
   --
   procedure Xm_Drop_Site_Update
     (Enclosing_Widget : in Widget;
      Args             : in Arg_List := Null_Arg_List) is
      procedure XmDropSiteUpdate
        (W       : in Widget;
         Args    : in X_Toolkit.Internal.Arg_Rec_Access;
         Count   : in Cardinal);
      pragma Import (C, XmDropSiteUpdate, "XmDropSiteUpdate");
   begin
      XmDropSiteUpdate (Enclosing_Widget,
                        X_Toolkit.Internal.Hook (Args),
                        Cardinal (Length (Args)));
   end Xm_Drop_Site_Update;


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteRetrieve
   --
   procedure Xm_Drop_Site_Retrieve
     (Enclosing_Widget : in Widget;
      Args             : in Arg_List := Null_Arg_List) is
      procedure XmDropSiteRetrieve
        (W       : in Widget;
         Args    : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal);
      pragma Import (C, XmDropSiteRetrieve, "XmDropSiteRetrieve");
   begin
      XmDropSiteRetrieve (Enclosing_Widget,
                          X_Toolkit.Internal.Hook (Args),
                          Cardinal (Length (Args)));
   end Xm_Drop_Site_Retrieve;


   -- -------------------------------------------------------------------------
   --
   -- XmDropSiteQueryStackingOrder
   --
   procedure Xm_Drop_Site_Query_Stacking_Order
     (W        : in Widget;
      Parent   : out Widget;
      Children : out Widget_List) is
      type Our_Widget_Array is array (Cardinal range <>) of aliased Widget;
      package Widget_L_Pointers is new Interfaces.C.Pointers (Cardinal,
                                                              Widget,
                                                              Our_Widget_Array,
                                                              Null_Widget);
      Hook        : Widget_L_Pointers.Pointer;
      Num_Widgets : Cardinal;

      function XmDropSiteQueryStackingOrder
        (W            : in Widget;
         Parent       : in System.Address;
         Children     : in System.Address;
         Num_Children : in System.Address) return Integer;
      pragma Import (C, XmDropSiteQueryStackingOrder, "XmDropSiteQueryStackingOrder");
   begin
      if XmDropSiteQueryStackingOrder (W, Parent'Address, Hook'Address, Num_Widgets'Address) /= 0 then
         raise Error_Query_Stacking_Order;
      else
         declare
            W_List : constant Our_Widget_Array (1 .. Num_Widgets)
               := Widget_L_Pointers.Value (Hook, Interfaces.C.ptrdiff_t (Num_Widgets));
         begin
            Children := Null_Widget_List;
            for I in 1 .. Num_Widgets loop
               Append (Children, W_List (I));
            end loop;
         end;
      end if;
   end Xm_Drop_Site_Query_Stacking_Order;


   function To_Integer is
      new Ada.Unchecked_Conversion (Animation_Style_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Animation_Style_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Animation_Style_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



   function To_Integer is
      new Ada.Unchecked_Conversion (Drop_Site_Activity_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drop_Site_Activity_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Drop_Site_Activity_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



   function To_Uns_Char is
      new Ada.Unchecked_Conversion (Operations_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Operations_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Uns_Char (Value)));
   end Append_Set;


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Operations_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;



   function To_Integer is
      new Ada.Unchecked_Conversion (Drop_Site_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Drop_Site_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Drop_Site_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);



end Xm_Widgets.Drop_Site_Manager;
