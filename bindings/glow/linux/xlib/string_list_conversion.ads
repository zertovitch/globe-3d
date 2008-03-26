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

with Interfaces.C.Strings.String_Lists,
     String_List;
package String_List_Conversion is


   subtype Chars_Ptr_List_Type is Interfaces.C.Strings.String_Lists.chars_ptr_array_ptr;
   Null_Chars_Ptr_List : constant Chars_Ptr_List_Type := Interfaces.C.Strings.String_Lists.Null_Array_Ptr;

   subtype Index_Type is Interfaces.C.size_t;

   function To_Chars_Ptr_Array (Str_List    : in String_List.Element_Access_List;
                                Append_Null : in Boolean := False)
                                return Interfaces.C.Strings.Chars_Ptr_Array;


   function To_Chars_Ptr_List (Str_List    : in String_List.Element_Access_List;
                               Append_Null : in Boolean := False)
                               return Chars_Ptr_List_Type;



   function To_String_Access_List (Item : in Interfaces.C.Strings.Chars_Ptr_Array)
      return String_List.Element_Access_List;


   -- if we only have a C-style pointer to the first element of a
   -- Null-terminated array
   function To_String_Access_List (Ref : in Chars_Ptr_List_Type)
      return String_List.Element_Access_List;

   procedure Free (Ref : in out Chars_Ptr_List_Type);


   -- if we only have a C-style pointer to the first element of a
   -- not terminated array, which size is known
   function To_String_Access_List
     (Ref    : in Chars_Ptr_List_Type;
      Length : in Index_Type)
      return String_List.Element_Access_List;

   procedure Free
     (Ref    : in out Chars_Ptr_List_Type;
      Length : in     Index_Type);


end String_List_Conversion;
