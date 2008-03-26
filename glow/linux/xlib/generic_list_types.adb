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

with Ada.Unchecked_Deallocation,
     Ada.Unchecked_Conversion;
package body Generic_List_Types is


   procedure Deallocate is
      new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   -- -------------------------------------------------------------------------
   --
   --  Initialization
   --
   procedure Initialize (Object : in out Unbounded_List) is
   begin
      Object.List := Null_Unbounded_List.List;
   end Initialize;
   pragma Inline (Initialize);


   -- -------------------------------------------------------------------------
   --
   --  Adjust
   --
   procedure Adjust     (Object : in out Unbounded_List) is
   begin
      if Object.List /= Null_Unbounded_List.List then
         Object.List := new Element_Array'(Object.List.all);
      end if;
   end Adjust;
   pragma Inline (Adjust);


   -- -------------------------------------------------------------------------
   --
   --  Free Storage
   --
   procedure Free (Object : in out Element_Array_Access) is
   begin
      Deallocate (Object);
   end Free;
   pragma Inline (Free);


   -- -------------------------------------------------------------------------
   --
   --  Finalization
   --
   procedure Finalize   (Object : in out Unbounded_List) is
   begin
      if Object.List /= Null_Unbounded_List.List then
         Free (Object.List);
      end if;
   end Finalize;
   pragma Inline (Finalize);



   -- -------------------------------------------------------------------------
   --
   --    Length
   --
   function Length (Object : in Unbounded_List) return Natural is
   begin
      return Object.List.all'Length;
   end Length;
   pragma Inline (Length);


-- ----------------------------------------------------------------------------
--
--   Conversion Routines
--

   -- -------------------------------------------------------------------------
   --
   --
   --
   function To_Unbounded_List (Source : in Element_Array)
         return Unbounded_List is
   begin
      return (Controlled with List => new Element_Array'(Source));
   end To_Unbounded_List;
   pragma Inline (To_Unbounded_List);


   function To_Element_Array          (Source : in Unbounded_List)
         return Element_Array is
   begin
      return Source.List.all;
   end To_Element_Array;
   pragma Inline (To_Element_Array);


-- ----------------------------------------------------------------------------
--
--   Element Access
--

   function Element
     (Source  : in Unbounded_List;
      Index   : in Natural)
      return Element_Type is
   begin
      return Source.List.all (Index);
   end Element;


   procedure Replace_Element
     (Source  : in out Unbounded_List;
      Index   : in Natural;
      By      : in Element_Type) is
   begin
      if Index in Source.List.all'Range then
         Source.List.all (Index) := By;
      else
         raise Constraint_Error;
      end if;
   end Replace_Element;


   function Slice
     (Source  : in Unbounded_List;
      Low     : in Positive;
      High    : in Natural)
      return Element_Array is

      Result   : Element_Array (1 .. High - Low + 1);
   begin
      Result := Source.List.all (Low .. High);
      return Result;
   end Slice;


-- ----------------------------------------------------------------------------
--
--   Comparison Routines
--

   function "=" (Left, Right : in Unbounded_List) return Boolean is
      Offset    : constant Integer := Right.List.all'First -
                                      Left.List.all'First;
   begin
      if Left.List.all'Length = Right.List.all'Length then
         for I in Left.List.all'Range loop
            if Left.List (I) /= Right.List (Offset + I) then
               return False;
            end if;
         end loop;
         return True;
      else
         return False;
      end if;
   end "=";


-- ----------------------------------------------------------------------------
--
--   Concatenation Routines
--

   function "&" (Left, Right : in Unbounded_List)
         return Unbounded_List is
      L_Length : constant Integer := Left.List.all'Length;
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Unbounded_List;

   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left.List.all;
      Result.List.all (L_Length + 1 .. Length) := Right.List.all;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Unbounded_List;
                 Right : in Element_Array)
         return Unbounded_List is
      L_Length : constant Integer := Left.List.all'Length;
      R_Length : constant Integer := Right'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Unbounded_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left.List.all;
      Result.List.all (L_Length + 1 .. Length) := Right;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Array;
                 Right : in Unbounded_List)
         return Unbounded_List is
      L_Length : constant Integer := Left'Length;
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Unbounded_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left;
      Result.List.all (L_Length + 1 .. Length) := Right.List.all;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Unbounded_List;
                 Right : in Element_Type)
         return Unbounded_List is
      L_Length : constant Integer := Left.List.all'Length;
      Length   : constant Integer := L_Length + 1;
      Result   : Unbounded_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)        := Left.List.all;
      Result.List.all (L_Length + 1)         := Right;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Type;
                 Right : in Unbounded_List)
         return Unbounded_List is
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := R_Length + 1;
      Result   : Unbounded_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1)           := Left;
      Result.List.all (2 .. Length) := Right.List.all;
      return Result;
   end "&";
   pragma Inline ("&");


   procedure Append (List    : in out Unbounded_List;
                     Which   : in     Unbounded_List) is
      Ret_List : Element_Array (1 .. List.List.all'Length +
                                     Which.List.all'Length);
   begin
      Ret_List (1 .. List.List.all'Length) := List.List.all;
      Ret_List (List.List.all'Length + 1 .. Ret_List'Last)  := Which.List.all;
      --  don't free our reference value
      if List.List /= Null_Unbounded_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;


   procedure Append (List    : in out Unbounded_List;
                     Which   : in     Element_Type) is
      Ret_List : Element_Array (1 .. List.List.all'Length + 1);
   begin
      Ret_List (1 .. List.List.all'Length) := List.List.all;
      Ret_List (List.List.all'Length + 1)  := Which;
      --  don't free our reference value
      if List.List /= Null_Unbounded_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;



   function Hook (List  : in Unbounded_List) return Element_Access is
   begin
      if List /= Null_Unbounded_List then
         return List.List.all (List.List.all'First)'Access;
      else
         return null;
      end if;
   end Hook;


end Generic_List_Types;
