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

with System.Storage_Elements;
package Interfaces.C.Strings.String_Lists is
pragma Preelaborate (String_Lists);

   type chars_ptr_array_ptr is private;

   Null_Array_Ptr : constant chars_ptr_array_ptr;

   --  allocate string list and copy the chars_ptrs
   --  (no new allocation of chars_ptrs)
   --
   function New_Chars_Ptr_Array
     (Ary         : in chars_ptr_array;
      Append_Null : in Boolean := True)
      return chars_ptr_array_ptr;

   --  return the chars_ptr at offset
   --  (NO CHECK if offset is valid)
   --
   function Value
     (Item   : in chars_ptr_array_ptr;
      Offset : in size_t)
      return chars_ptr;

   --  replace the value at offset with new_value
   --  (NO CHECK if offset is valid)
   --
   procedure Update
     (Item      : in out chars_ptr_array_ptr;
      Offset    : in     size_t;
      New_Value : in     chars_ptr);

   --  handle null-terminated arrays (i.e. with a trailing Null_Ptr)
   --
   procedure Free (Item : in out chars_ptr_array_ptr);

   function Length (Item : in chars_ptr_array_ptr) return size_t;

   function Value (Item : in chars_ptr_array_ptr) return chars_ptr_array;

   --  create a duplicate of Item
   --
   function Duplicate
     (Item     : in chars_ptr_array_ptr)
      return chars_ptr_array_ptr;

   --  handle unterminated arrays (i.e. without a trailing Null_Ptr)
   --
   procedure Free
     (Item   : in out chars_ptr_array_ptr;
      Length : in     size_t);

   function Value
     (Item   : in chars_ptr_array_ptr;
      Length : in size_t)
      return chars_ptr_array;

   --  create a duplicate of Item
   --
   function Duplicate
     (Item     : in chars_ptr_array_ptr;
      Length   : in size_t)
      return chars_ptr_array_ptr;

private

   type chars_ptr_array_ptr is new System.Storage_Elements.Integer_Address;

   Null_Array_Ptr : constant chars_ptr_array_ptr := 0;

end Interfaces.C.Strings.String_Lists;
