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

with Ada.Finalization;

generic
   type Element_Type (<>) is private;
   type Element_Access_Type is access all Element_Type;
package Generic_List_Access_Types is


   type Element_Array is
         array (Natural range <>) of Element_Access_Type;


   type Element_Access_List is private;

   Null_Element_Access_List : constant Element_Access_List;

   procedure Free (Item : in out Element_Access_Type);


   Invalid_Element_Error : exception;


   function Length (Object : in Element_Access_List) return Natural;


-- ----------------------------------------------------------------------------
--
--   Conversion Routines
--

   function To_Element_Access_List (Source : in Element_Array)
      return Element_Access_List;

   function To_Element_Array       (Source : in Element_Access_List)
      return Element_Array;


-- ----------------------------------------------------------------------------
--
--   Element Access
--

   function Element
     (Source  : in Element_Access_List;
      Index   : in Natural)
      return Element_Type;

   procedure Replace_Element
     (Source  : in out Element_Access_List;
      Index   : in Natural;
      By      : in Element_Type);

   function Slice
     (Source  : in Element_Access_List;
      Low     : in Positive;
      High    : in Natural)
      return Element_Array;


-- ----------------------------------------------------------------------------
--
--   Comparison Routines
--

   function "=" (Left, Right : in Element_Access_List) return Boolean;


-- ----------------------------------------------------------------------------
--
--   Concatenation Routines
--

   function "&" (Left, Right : in Element_Access_List)
         return Element_Access_List;

   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Array)
         return Element_Access_List;

   function "&" (Left  : in Element_Array;
                 Right : in Element_Access_List)
         return Element_Access_List;

   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Access_Type)
         return Element_Access_List;

   function "&" (Left  : in Element_Access_Type;
                 Right : in Element_Access_List)
         return Element_Access_List;

   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Type)
         return Element_Access_List;

   function "&" (Left  : in Element_Type;
                 Right : in Element_Access_List)
         return Element_Access_List;


   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Access_List);

   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Array);

   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Access_Type);

   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Type);

private

   use  Ada.Finalization;

   type Element_Array_Access is access all Element_Array;

   procedure Free (Object : in out Element_Array_Access);


   type Element_Access_List is new Ada.Finalization.Controlled with record
      List : Element_Array_Access := null;
   end record;

   procedure Initialize (Object : in out Element_Access_List);
   procedure Adjust     (Object : in out Element_Access_List);
   procedure Finalize   (Object : in out Element_Access_List);

   Null_Element_Access_List : constant Element_Access_List :=
      (Controlled with List => new Element_Array (1 .. 0));

end Generic_List_Access_Types;
