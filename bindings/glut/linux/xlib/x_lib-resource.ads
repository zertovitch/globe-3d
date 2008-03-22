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

with Ada.Strings.Unbounded,
     Generic_List_Access_Types;

package X_Lib.Resource is

   type Xrm_Option_Kind is (Option_No_Arg, 
                            Option_Is_Arg, 
                            Option_Sticky_Arg, 
                            Option_Separate_Arg, 
                            Option_Resource_Arg, 
                            Option_Skip_Arg, 
                            Option_Skip_Line,
                            Option_Skip_N_Args);
   for Xrm_Option_Kind use (Option_No_Arg        => 0,
                            Option_Is_Arg        => 1, 
                            Option_Sticky_Arg    => 2, 
                            Option_Separate_Arg  => 3, 
                            Option_Resource_Arg  => 4, 
                            Option_Skip_Arg      => 5, 
                            Option_Skip_Line     => 6,
                            Option_Skip_N_Args   => 7);


   type Option_Description_Record (Option_Kind : Xrm_Option_Kind) is record
      Option_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Specifier    : Ada.Strings.Unbounded.Unbounded_String;
      case Option_Kind is
         when Option_No_Arg =>
            Option_Value : X_Pointer;
         when others =>
            null;
      end case;
   end record;
   type Option_Description_Record_Access is access all Option_Description_Record;


   type Option_Description_List is private;

   Null_Option_Description_List : constant Option_Description_List;


   function Length (Object : in Option_Description_List) return Natural;


-- ----------------------------------------------------------------------------
--
--   Comparison Routines
--

   function "=" (Left, Right : in Option_Description_Record) return Boolean;

   function "=" (Left, Right : in Option_Description_List) return Boolean;


-- ----------------------------------------------------------------------------
--
--   Concatenation Routines
--

   function "&" (Left, Right : in Option_Description_List)
         return Option_Description_List;

   function "&" (Left  : in Option_Description_List;
                 Right : in Option_Description_Record)
         return Option_Description_List;

   procedure Append (List   : in out Option_Description_List;
                     Object : in     Option_Description_List);

   procedure Append (List   : in out Option_Description_List;
                     Object : in     Option_Description_Record);

   procedure Append (List         : in out Option_Description_List;
                     Option_Kind  : in     Xrm_Option_Kind;
                     Option_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
                     Option_Spec  : in     Ada.Strings.Unbounded.Unbounded_String;
                     Option_Value : in     X_Pointer := Null_X_Pointer);

   procedure Append (List         : in out Option_Description_List;
                     Option_Kind  : in     Xrm_Option_Kind;
                     Option_Name  : in     String;
                     Option_Spec  : in     String;
                     Option_Value : in     X_Pointer := Null_X_Pointer);

private

   package Xrm_Option_Lists is
      new Generic_List_Access_Types (Option_Description_Record, Option_Description_Record_Access);

   type Option_Description_List is new Xrm_Option_Lists.Element_Access_List;

   Null_Option_Description_List : constant Option_Description_List := Option_Description_List (Xrm_Option_Lists.Null_Element_Access_List);

end X_Lib.Resource;
