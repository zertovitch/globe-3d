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
--  HISTORY:
--          June 20, 1998 begin of history
--
-------------------------------------------------------------------------------

--  package which implements C style char** handling

with System.Address_To_Access_Conversions;

package body Interfaces.C.Strings.String_Lists is

   package String_List_Access is
      new System.Address_To_Access_Conversions (chars_ptr);

   Elmt_Size : constant size_t :=
                 (chars_ptr'Size
                   + System.Storage_Unit - 1) / System.Storage_Unit;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "+"
     (Left  : chars_ptr_array_ptr;
      Right : size_t)
      return chars_ptr_array_ptr;
   pragma Inline ("+");

   procedure Increment
     (Left  : in out chars_ptr_array_ptr);
   pragma Inline (Increment);

   function C_Malloc (Size : size_t) return chars_ptr_array_ptr;
   pragma Import (C, C_Malloc, "malloc");

   procedure C_Free (Address : chars_ptr_array_ptr);
   pragma Import (C, C_Free, "free");

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : in chars_ptr_array_ptr;
      Right : in size_t)
      return chars_ptr_array_ptr is
   begin
      return Left + chars_ptr_array_ptr (Elmt_Size * Right);
   end "+";

   procedure Increment
     (Left  : in out chars_ptr_array_ptr) is
   begin
      Left := Left + chars_ptr_array_ptr (Elmt_Size);
   end Increment;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out chars_ptr_array_ptr) is
      Tmp_Item      : chars_ptr_array_ptr := Item;
      Tmp_Chars_Ptr : chars_ptr;
   begin
      if Item = Null_Array_Ptr then
         return;
      end if;

      loop
         Tmp_Chars_Ptr :=
            String_List_Access.To_Pointer (To_Address (Tmp_Item)).all;
         if Tmp_Chars_Ptr /= Null_Ptr then
            Free (Tmp_Chars_Ptr);
            Increment (Tmp_Item);
         else
            exit;
         end if;
      end loop;
      C_Free (Item);
      Item := Null_Array_Ptr;
   end Free;

   procedure Free
     (Item   : in out chars_ptr_array_ptr;
      Length : in     size_t) is
      Tmp_Item      : chars_ptr_array_ptr := Item;
      Tmp_Chars_Ptr : chars_ptr;
   begin
      if Item = Null_Array_Ptr then
         return;
      end if;

      for I in 1 .. Length loop
         Tmp_Chars_Ptr :=
            String_List_Access.To_Pointer (To_Address (Tmp_Item)).all;
         Free (Tmp_Chars_Ptr);
         Increment (Tmp_Item);
      end loop;
      C_Free (Item);
      Item := Null_Array_Ptr;
   end Free;

   function New_Chars_Ptr_Array
     (Ary         : in chars_ptr_array;
      Append_Null : in Boolean := True)
      return chars_ptr_array_ptr is
      Pointer, Tmp_Pointer : chars_ptr_array_ptr;
      Len       : size_t;
      Append_N  : Boolean := False;
   begin
      if Ary'Length = 0 then
         return Null_Array_Ptr;
      end if;
      Len := Elmt_Size * Ary'Length;
      if Append_Null and then Ary (Ary'Last) /= Null_Ptr then
         Len      := Len + Elmt_Size;
         Append_N := True;
      end if;
      Pointer     := C_Malloc (Len);
      Tmp_Pointer := Pointer;
      for I in Ary'Range loop
         String_List_Access.To_Pointer (To_Address (Tmp_Pointer)).all :=
            Ary (I);
         Increment (Tmp_Pointer);
      end loop;
      if Append_N then
         String_List_Access.To_Pointer (To_Address (Tmp_Pointer)).all :=
            Null_Ptr;
      end if;
      return Pointer;
   end New_Chars_Ptr_Array;

   function Value
     (Item   : in chars_ptr_array_ptr;
      Offset : in size_t)
      return chars_ptr is
   begin
      return String_List_Access.To_Pointer (To_Address (Item + Offset)).all;
   end Value;

   procedure Update
     (Item      : in out chars_ptr_array_ptr;
      Offset    : in     size_t;
      New_Value : in     chars_ptr) is

      Ptr : String_List_Access.Object_Pointer;
   begin
      if Item = Null_Array_Ptr then
         raise Dereference_Error;
      end if;
      Ptr := String_List_Access.To_Pointer (To_Address (Item + Offset));
      Free (Ptr.all);
      Ptr.all := New_Value;
   end Update;

   function Length (Item : in chars_ptr_array_ptr) return size_t is
      Item_Index : size_t := 0;
   begin
      if Item = Null_Array_Ptr then
         raise Dereference_Error;
      end if;

      loop
         if String_List_Access.To_Pointer (To_Address (Item +
                                                      Item_Index)).all =
            Null_Ptr then
            return Item_Index;
         end if;

         Item_Index := Item_Index + 1;
      end loop;
   end Length;

   function Value (Item : in chars_ptr_array_ptr) return chars_ptr_array is
   begin
      return Value (Item, Length (Item));
   end Value;

   function Value
     (Item   : in chars_ptr_array_ptr;
      Length : in     size_t)
      return chars_ptr_array is
      Ary : chars_ptr_array (1 .. Length);
      Tmp_Item   : chars_ptr_array_ptr := Item;
   begin
      for I in 1 .. Length loop
         Ary (I) :=
            String_List_Access.To_Pointer (To_Address (Tmp_Item)).all;
         Increment (Tmp_Item);
      end loop;
      return Ary;
   end Value;

   function Duplicate
     (Item     : in chars_ptr_array_ptr)
      return chars_ptr_array_ptr is
   begin
      if Item = Null_Array_Ptr then
         return Null_Array_Ptr;
      else
         return Duplicate (Item, Length (Item));
      end if;
   end Duplicate;

   function Duplicate
     (Item     : in chars_ptr_array_ptr;
      Length   : in size_t)
      return chars_ptr_array_ptr is
      Pointer,
      Source,
      Target : chars_ptr_array_ptr;
   begin
      if Length = 0 then
         return Null_Array_Ptr;
      end if;
      Pointer  := C_Malloc (Elmt_Size * Length);
      Target   := Pointer;
      Source   := Item;
      for I in 1 .. Length loop
         String_List_Access.To_Pointer (To_Address (Target)).all :=
            New_Char_Array (Value (String_List_Access.To_Pointer (
            To_Address (Source)).all));
         Increment (Target);
         Increment (Source);
      end loop;
      return Pointer;
   end Duplicate;

end Interfaces.C.Strings.String_Lists;
