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

with System.Storage_Elements;

package Interfaces.C.Wstrings is
pragma Preelaborate (Wstrings);

   type wchar_array_access is access all wchar_array;

   type wchars_ptr is private;

   type wchars_ptr_array is array (size_t range <>) of wchars_ptr;

   Null_WPtr : constant wchars_ptr;

   function To_Chars_Ptr
     (Item      : in wchar_array_access;
      Nul_Check : in Boolean := False)
      return      wchars_ptr;

   function New_Char_Array (Chars : in wchar_array) return wchars_ptr;

   function New_String (Str : in Wide_String) return wchars_ptr;

   procedure Free (Item : in out wchars_ptr);

   Dereference_Error : exception;

   function Value (Item : in wchars_ptr) return wchar_array;

   function Value
     (Item   : in wchars_ptr;
      Length : in size_t)
      return   wchar_array;

   function Value (Item : in wchars_ptr) return Wide_String;

   function Value
     (Item   : in wchars_ptr;
      Length : in size_t)
      return   Wide_String;

   function Strlen (Item : in wchars_ptr) return size_t;

   procedure Update
     (Item   : in wchars_ptr;
      Offset : in size_t;
      Chars  : in wchar_array;
      Check  : Boolean := True);

   procedure Update
     (Item   : in wchars_ptr;
      Offset : in size_t;
      Str    : in Wide_String;
      Check  : in Boolean := True);

   Update_Error : exception;

private
   type wchars_ptr is new System.Storage_Elements.Integer_Address;

   Null_WPtr : constant wchars_ptr := 0;

end Interfaces.C.Wstrings;
