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

package body String_List_Conversion is

use Interfaces.C,
    Interfaces.C.Strings;


   function To_Chars_Ptr_Array (Str_List    : in String_List.Element_Access_List;
                                Append_Null : in Boolean := False)
                                return Chars_Ptr_Array is
      Str_Array : constant String_List.Element_Array := String_List.To_Element_Array (Str_List);
      Last_Idx : Natural;
   begin
      if Append_Null then
         Last_Idx := Str_Array'Last + 1;
      else
         Last_Idx := Str_Array'Last;
      end if;
      declare
         Ret : Chars_Ptr_Array (Index_Type (Str_Array'First) .. Index_Type (Last_Idx));
      begin
         for I in Str_Array'Range loop
            Ret (Index_Type (I)) := New_String (Str_Array (I).all);
         end loop;
         if Append_Null then
            Ret (Index_Type (Last_Idx)) := Null_Ptr;
         end if;
         return Ret;
      end;
   end To_Chars_Ptr_Array;


   function To_Chars_Ptr_List (Str_List    : in String_List.Element_Access_List;
                               Append_Null : in Boolean := False)
                               return Chars_Ptr_List_Type is
      Ch_Arry : Chars_Ptr_Array := To_Chars_Ptr_Array (Str_List, Append_Null);
   begin
      return Interfaces.C.Strings.String_Lists.New_Chars_Ptr_Array (Ch_Arry, Append_Null => False);
   end To_Chars_Ptr_List;
   pragma Inline (To_Chars_Ptr_List);



   function To_String_Access_List (Item : in Chars_Ptr_Array)
      return String_List.Element_Access_List is
      Last_Idx : Index_Type;
   begin
      -- look if NULL_PTR-Terminated, if yes, remove NULL_PTR
      Last_Idx := Item'Last;
      for I in Item'Range loop
         if Item (I) = Null_Ptr then
            Last_Idx := Index_Type (I) - 1;
            exit;
         end if;
      end loop;
      declare
         Ret : String_List.Element_Array (Natural (Item'First) .. Natural (Last_Idx));
      begin
         for I in Ret'Range loop
            Ret (I) := new String'(Value (Item (Index_Type (I))));
         end loop;
         return String_List.To_Element_Access_List (Ret);
      end;
   end To_String_Access_List;


   -- if we only have a C-style pointer to the first element of a
   -- Null-terminated array

   function To_String_Access_List (Ref : in Chars_Ptr_List_Type) return
      String_List.Element_Access_List is
   begin
      return To_String_Access_List (Interfaces.C.Strings.String_Lists.Value (Ref));
   end To_String_Access_List;


   procedure Free (Ref : in out Chars_Ptr_List_Type) is
   begin
      Interfaces.C.Strings.String_Lists.Free (Ref);
   end Free;
   pragma Inline (Free);



   -- if we only have a C-style pointer to the first element of a
   -- not terminated array, which size is known

   function To_String_Access_List (Ref    : in Chars_Ptr_List_Type;
                                   Length : in Index_Type)
      return String_List.Element_Access_List is
   begin
      -- we need to prevent exception Dereference_Error
      if Length <= 0 then
         return String_List.Null_Element_Access_List;
      else
         return To_String_Access_List (Interfaces.C.Strings.String_Lists.Value (Ref, Length));
      end if;
   end To_String_Access_List;


   procedure Free
     (Ref    : in out Chars_Ptr_List_Type;
      Length : in     Index_Type) is
   begin
      Interfaces.C.Strings.String_Lists.Free (Ref, Length);
   end Free;
   pragma Inline (Free);

end String_List_Conversion;
