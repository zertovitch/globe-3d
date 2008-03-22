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
--          September 5, 1998 first version of package
--                            (extracted from X_Lib)
--
-------------------------------------------------------------------------------

with Interfaces.C.Pointers,
     Interfaces.C.Strings;
package body X_Lib.Extensions is


   function X_List_Extensions (Display : in X_Lib.Display_Pointer)
      return String_List.Element_Access_List is

      type C_String_Array is array (Natural range <>) of aliased Interfaces.C.Strings.Chars_Ptr;
      package String_Pointers is new
         Interfaces.C.Pointers (Natural,
                                Interfaces.C.Strings.Chars_Ptr,
                                C_String_Array,
                                Interfaces.C.Strings.Null_Ptr);

      function XListExtensions (Display : in X_Lib.Display_Pointer;
                                N_Extensions : in System.Address) return String_Pointers.Pointer;
      pragma Import (C, XListExtensions, "XListExtensions");
      procedure XFreeExtensionList (List : in String_Pointers.Pointer);
      pragma Import (C, XFreeExtensionList, "XFreeExtensionList");

      Hook : String_Pointers.Pointer;
      Number : Integer;

      Return_List : String_List.Element_Access_List;
   begin
      Hook := XListExtensions (Display, Number'Address);
      declare
         List : constant C_String_Array (1 .. Number) := String_Pointers.Value (Hook, Interfaces.C.Ptrdiff_T (Number));
      begin
         for I in List'Range loop
            String_List.Append (Return_List, Interfaces.C.Strings.Value (List (I)));
         end loop;
      end;
      XFreeExtensionList (Hook); -- we don't need the list any more
      return Return_List;
   end X_List_Extensions;


   -- query if extension "Name" exists. if not, raise
   -- Extension_Not_Existent_Error
   --
   procedure X_Query_Extension (Display      : in X_Lib.Display_Pointer;
                                Name         : in String;
                                Major_Opcode :    out Opcode_Type;
                                First_Event  :    out Event_Type;
                                First_Error  :    out Error_Code_Type) is
      function XQueryExtension
        (Display      : in X_Lib.Display_Pointer;
         Name         : in System.Address;
         Major_Opcode : in System.Address;
         First_Event  : in System.Address;
         First_Error  : in System.Address)
	 return X_Boolean;
      pragma Import (C, XQueryExtension, "XQueryExtension");

      Name_String     : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Name, Append_Nul => True);
      First_Error_Int : Integer;
   begin
      if XQueryExtension (Display, Name_String'Address, Major_Opcode'Address,
                          First_Event'Address,
                          First_Error_Int'Address) = X_Boolean'(False) then
         raise Extension_Not_Existent_Error;
      end if;
      First_Error := Error_Code_Type (First_Error_Int);
   end X_Query_Extension;


end X_Lib.Extensions;
