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

with Interfaces.C.Strings;
use  Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

with Ada.Strings,
     Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;
package body X_Lib.Resource is


   function Length (Object : in Option_Description_List) return Natural is
   begin
      return Xrm_Option_Lists.Length (Xrm_Option_Lists.Element_Access_List (Object));
   end Length;
   pragma Inline (Length);

-- ----------------------------------------------------------------------------
--
--   Comparison Routines
--
   function "=" (Left, Right : in Option_Description_Record) return Boolean is
   begin
      if Left.Option_Kind = Right.Option_Kind then
         if Left.Option_Kind = Option_No_Arg and then
            Left.Option_Value /= Right.Option_Value then
               return False;
         end if;
         return Left.Option_Name = Right.Option_Name and then
                Left.Specifier   = Right.Specifier;
      else
         return False;
      end if;
   end "=";


   function "=" (Left, Right : in Option_Description_List) return Boolean is
   begin
      return Xrm_Option_Lists."=" (Xrm_Option_Lists.Element_Access_List (Left),
                                   Xrm_Option_Lists.Element_Access_List (Right));
   end "=";
   pragma Inline ("=");



-- ----------------------------------------------------------------------------
--
--   Concatenation Routines
--

   function "&" (Left, Right : in Option_Description_List)
         return Option_Description_List is
   begin
      return Option_Description_List (Xrm_Option_Lists."&" (Xrm_Option_Lists.Element_Access_List (Left),
                                                            Xrm_Option_Lists.Element_Access_List (Right)));
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Option_Description_List;
                 Right : in Option_Description_Record)
         return Option_Description_List is
   begin
      return Option_Description_List (Xrm_Option_Lists."&" (Xrm_Option_Lists.Element_Access_List (Left),
                                                            Right));
   end "&";
   pragma Inline ("&");


   procedure Append (List   : in out Option_Description_List;
                     Object : in     Option_Description_List) is
   begin
      Xrm_Option_Lists.Append (Xrm_Option_Lists.Element_Access_List (List),
                               Xrm_Option_Lists.Element_Access_List (Object));
   end Append;
   pragma Inline (Append);


   procedure Append (List   : in out Option_Description_List;
                     Object : in     Option_Description_Record) is
   begin
      Xrm_Option_Lists.Append (Xrm_Option_Lists.Element_Access_List (List),
                               Object);
   end Append;
   pragma Inline (Append);


   procedure Append (List         : in out Option_Description_List;
                     Option_Kind  : in     Xrm_Option_Kind;
                     Option_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
                     Option_Spec  : in     Ada.Strings.Unbounded.Unbounded_String;
                     Option_Value : in     X_Pointer := Null_X_Pointer) is
      New_Object : Option_Description_Record (Option_Kind);
   begin
      New_Object.Option_Name := Option_Name;
      New_Object.Specifier   := Option_Spec;
      if Option_Kind = Option_No_Arg then
         New_Object.Option_Value := Option_Value;
      end if;
      Append (List, New_Object);
   end Append;


   procedure Append (List         : in out Option_Description_List;
                     Option_Kind  : in     Xrm_Option_Kind;
                     Option_Name  : in     String;
                     Option_Spec  : in     String;
                     Option_Value : in     X_Pointer := Null_X_Pointer) is
   begin
      Append (List => List,
              Option_Kind => Option_Kind,
              Option_Name => To_Unbounded_String (Option_Name),
              Option_Spec => To_Unbounded_String (Option_Spec),
              Option_Value => Option_Value);
   end Append;


end X_Lib.Resource;
