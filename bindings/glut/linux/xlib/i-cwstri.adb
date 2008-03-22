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

--  package which implements C style wchar_t* handling like
--  the interfaces.c.strings package

with System; use System;
with System.Address_To_Access_Conversions;

package body Interfaces.C.Wstrings is

   --  factor by which an address offset has to be multiplied to get
   --  the address of the next element
   Wide_Factor  : constant := wchar_t'Size / char'Size;

   package WChar_Access is new Address_To_Access_Conversions (wchar_t);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Peek (From : wchars_ptr) return wchar_t;
   pragma Inline (Peek);
   --  Given a chars_ptr value, obtain referenced character

   procedure Poke (Value : wchar_t; Into : wchars_ptr);
   pragma Inline (Poke);
   --  Given a wchars_ptr, modify referenced Wide Character value

   function "+" (Left : wchars_ptr; Right : size_t) return wchars_ptr;
   pragma Inline ("+");
   --  Address arithmetic on wchars_ptr value

   function Position_Of_Nul (Into : wchar_array) return size_t;
   --  Returns position of the first Nul in Into or Into'Last + 1 if none

   function C_Malloc (Size : size_t) return wchars_ptr;
   pragma Import (C, C_Malloc, "__gnat_malloc");

   procedure C_Free (Address : wchars_ptr);
   pragma Import (C, C_Free, "free");

   ---------
   -- "+" --
   ---------

   function "+" (Left : wchars_ptr; Right : size_t) return wchars_ptr is
   begin
      return Left + wchars_ptr (Wide_Factor * Right);
   end "+";

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out wchars_ptr) is
   begin
      if Item = Null_WPtr then
         return;
      end if;

      C_Free (Item);
      Item := Null_WPtr;
   end Free;

   --------------------
   -- New_Char_Array --
   --------------------

   function New_Char_Array (Chars : in wchar_array) return wchars_ptr is
      Index   : size_t;
      Pointer : wchars_ptr;
   begin
      --  Get index of position of null. If Index > Chars'last, nul is absent
      --  and must be added explicitly.

      Index := Position_Of_Nul (Into => Chars);
      Pointer := C_Malloc (Wide_Factor * (Index - Chars'First + 1));

      --  If nul is present, transfer string up to and including it.

      if Index <= Chars'Last then
         Update (Item   => Pointer,
                 Offset => 0,
                 Chars  => Chars (Chars'First .. Index),
                 Check  => False);
      else
         --  If original string has no nul, transfer whole string and add
         --  terminator explicitly.

         Update (Item   => Pointer,
                 Offset => 0,
                 Chars  => Chars,
                 Check  => False);
         Poke (wide_nul,
               into => Pointer + size_t '(Chars'Length) + size_t'(1));
      end if;

      return Pointer;
   end New_Char_Array;

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : in Wide_String) return wchars_ptr is
   begin
      return New_Char_Array (To_C (Str));
   end New_String;

   ----------
   -- Peek --
   ----------

   function Peek (From : wchars_ptr) return wchar_t is
      use WChar_Access;
   begin
      return To_Pointer (Address (To_Address (From))).all;
   end Peek;

   ----------
   -- Poke --
   ----------

   procedure Poke (Value : wchar_t; Into : wchars_ptr) is
      use WChar_Access;
   begin
      To_Pointer (Address (To_Address (Into))).all := Value;
   end Poke;

   ---------------------
   -- Position_Of_Nul --
   ---------------------

   function Position_Of_Nul (Into : wchar_array) return size_t is
   begin
      for J in Into'Range loop
         if Into (J) = wide_nul then
            return J;
         end if;
      end loop;

      return Into'Last + 1;
   end Position_Of_Nul;

   ------------
   -- Strlen --
   ------------

   function Strlen (Item : in wchars_ptr) return size_t is
      Item_Index : size_t := 0;

   begin
      if Item = Null_WPtr then
         raise Dereference_Error;
      end if;

      loop
         if Peek (Item + Item_Index) = wide_nul then
            return Item_Index;
         end if;

         Item_Index := Item_Index + 1;
      end loop;
   end Strlen;

   ------------------
   -- To_Chars_Ptr --
   ------------------

   function To_Chars_Ptr
     (Item      : in wchar_array_access;
      Nul_Check : in Boolean := False)
      return      wchars_ptr
   is
   begin
      if Item = null then
         return Null_WPtr;
      elsif Nul_Check
        and then Position_Of_Nul (Into => Item.all) > Item'Last
      then
         raise Terminator_Error;
      else
         return To_Integer (Item (Item'First)'Address);
      end if;
   end To_Chars_Ptr;

   ------------
   -- Update --
   ------------

   procedure Update
     (Item   : in wchars_ptr;
      Offset : in size_t;
      Chars  : in wchar_array;
      Check  : Boolean := True)
   is
      Index : wchars_ptr := Item + Wide_Factor*Offset;

   begin
      if Check and then Offset + Chars'Length  > Strlen (Item) then
         raise Update_Error;
      end if;

      for J in Chars'Range loop
         Poke (Chars (J), Into => Index);
         Index := Index + size_t'(1);
      end loop;
   end Update;

   procedure Update
     (Item   : in wchars_ptr;
      Offset : in size_t;
      Str    : in Wide_String;
      Check  : in Boolean := True)
   is
   begin
      Update (Item, Offset, To_C (Str), Check);
   end Update;

   -----------
   -- Value --
   -----------

   function Value (Item : in wchars_ptr) return wchar_array is
      Result : wchar_array (0 .. Strlen (Item));

   begin
      if Item = Null_WPtr then
         raise Dereference_Error;
      end if;

      --  Note that the following loop will also copy the terminating Nul

      for J in Result'Range loop
         Result (J) := Peek (Item + J);
      end loop;

      return Result;
   end Value;

   function Value
     (Item   : in wchars_ptr;
      Length : in size_t)
      return   wchar_array
   is
      Result : wchar_array (0 .. Length - 1);

   begin
      if Item = Null_WPtr then
         raise Dereference_Error;
      end if;

      for J in Result'Range loop
         Result (J) := Peek (Item + J);
         if Result (J) = wide_nul then
            return Result (0 .. J);
         end if;
      end loop;

      return Result;
   end Value;

   function Value (Item : in wchars_ptr) return Wide_String is
   begin
      return To_Ada (Value (Item));
   end Value;

   function Value
     (Item : in wchars_ptr;
      Length : in size_t)
      return Wide_String is
   begin
      return To_Ada (Value (Item, Length));
   end Value;


end Interfaces.C.Wstrings;
