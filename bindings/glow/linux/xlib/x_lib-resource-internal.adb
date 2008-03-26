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

with Ada.Unchecked_Deallocation;
package body X_Lib.Resource.Internal is


   function To_Xrm_Option_Desc_List (Opt_List    : in Option_Description_List)
      return Xrm_Option_Desc_List is

      Ret_Array : Xrm_Option_Array (1 .. Length (Opt_List));

      use Ada.Strings.Unbounded;
   begin
      for I in Ret_Array'Range loop
         declare
            Opt_Rec : constant Option_Description_Record := Xrm_Option_Lists.Element (Xrm_Option_Lists.Element_Access_List (Opt_List), I);
         begin
            Ret_Array (I).Option_Name := New_String (To_String (Opt_Rec.Option_Name));
            Ret_Array (I).Specifier   := New_String (To_String (Opt_Rec.Specifier));
            Ret_Array (I).Option_Kind := Opt_Rec.Option_Kind;
            if Ret_Array (I).Option_Kind = Option_No_Arg then
               Ret_Array (I).Option_Value := Opt_Rec.Option_Value;
            else
               Ret_Array (I).Option_Value := Null_X_Pointer;
            end if;
         end;
      end loop;
      return new Xrm_Option_Array'(Ret_Array);
   end To_Xrm_Option_Desc_List;


   procedure Free (Object : in out Xrm_Option_Desc_List) is
      procedure Dealloc is new Ada.Unchecked_Deallocation (Xrm_Option_Array, Xrm_Option_Desc_List);
   begin
      for I in Object.all'Range loop
         Free (Object (I).Option_Name);
         Free (Object (I).Specifier);
      end loop;
      Dealloc (Object);
   end Free;


end X_Lib.Resource.Internal;
