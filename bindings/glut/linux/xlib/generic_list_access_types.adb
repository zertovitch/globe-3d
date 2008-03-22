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
package body Generic_List_Access_Types is


   procedure Deallocate is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access_Type);

   procedure Free (Item : in out Element_Access_Type) is
   begin
      Deallocate (Item);
   end Free;
   pragma Inline (Free);

   procedure Deallocate is
      new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   -- -------------------------------------------------------------------------
   --
   --  Initialization
   --
   procedure Initialize (Object : in out Element_Access_List) is
   begin
      Object.List := Null_Element_Access_List.List;
   end Initialize;


   -- -------------------------------------------------------------------------
   --
   --  Adjust
   --
   procedure Adjust     (Object : in out Element_Access_List) is
   begin
      if Object.List /= Null_Element_Access_List.List then
         Object.List := new Element_Array'(Object.List.all);
         for I in Object.List.all'Range loop
            Object.List (I) := new Element_Type'(Object.List (I).all);
         end loop;
      end if;
   end Adjust;


   -- -------------------------------------------------------------------------
   --
   --  Free Storage
   --
   procedure Free (Object : in out Element_Array_Access) is
   begin
      for I in Object.all'Range loop
         Free (Object (I));
      end loop;
      Deallocate (Object);
   end Free;


   -- -------------------------------------------------------------------------
   --
   --  Finalization
   --
   procedure Finalize   (Object : in out Element_Access_List) is
   begin
      if Object.List /= Null_Element_Access_List.List then
         Free (Object.List);
      end if;
   end Finalize;



   -- -------------------------------------------------------------------------
   --
   --    Length
   --
   function Length (Object : in Element_Access_List) return Natural is
   begin
      return Object.List.all'Length;
   end Length;


-- ----------------------------------------------------------------------------
--
--   Conversion Routines
--

   -- -------------------------------------------------------------------------
   --
   --
   --
   function To_Element_Access_List (Source : in Element_Array)
         return Element_Access_List is
      Return_Value : Element_Access_List;
      Our_Source   : Element_Array (1 .. Source'Length);
      J            : Natural := 0;
   begin
      --  first check if every element is valid
      for I in Source'Range loop
         if Source (I) = null then
            raise Invalid_Element_Error;
         else
            J := J + 1;
            Our_Source (J) := Source (I);
         end if;
      end loop;
      --  everythings seems ok
      Return_Value.List := new Element_Array'(Our_Source);
      for I in Return_Value.List.all'Range loop
         Return_Value.List (I) := new Element_Type'(Our_Source (I).all);
      end loop;
      return Return_Value;
   end To_Element_Access_List;


   function To_Element_Array          (Source : in Element_Access_List)
         return Element_Array is
      Return_Value : Element_Array (Source.List.all'Range) := Source.List.all;
   begin
      for I in Return_Value'Range loop
         Return_Value (I) := new Element_Type'(Source.List (I).all);
      end loop;
      return Return_Value;
   end To_Element_Array;


-- ----------------------------------------------------------------------------
--
--   Element Access
--

   function Element
     (Source  : in Element_Access_List;
      Index   : in Natural)
      return Element_Type is
   begin
      return Source.List.all (Index).all;
   end Element;


   procedure Replace_Element
     (Source  : in out Element_Access_List;
      Index   : in Natural;
      By      : in Element_Type) is
   begin
      if Index in Source.List.all'Range then
         Free (Source.List.all (Index));
         Source.List.all (Index) := new Element_Type'(By);
      else
         raise Constraint_Error;
      end if;
   end Replace_Element;


   function Slice
     (Source  : in Element_Access_List;
      Low     : in Positive;
      High    : in Natural)
      return Element_Array is

      Result   : Element_Array (1 .. High - Low + 1);
   begin
      Result := Source.List.all (Low .. High);
      for I in Result'Range loop
         Result (I) := new Element_Type'(Result (I).all);
      end loop;
      return Result;
   end Slice;


-- ----------------------------------------------------------------------------
--
--   Comparison Routines
--

   function "=" (Left, Right : in Element_Access_List) return Boolean is
      Offset : constant Integer := Right.List.all'First - Left.List.all'First;
   begin
      if Left.List.all'Length = Right.List.all'Length then
         for I in Left.List.all'Range loop
            if Left.List (I).all /= Right.List (Offset + I).all then
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

   function "&" (Left, Right : in Element_Access_List)
         return Element_Access_List is
      L_Length : constant Integer := Left.List.all'Length;
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Element_Access_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left.List.all;
      Result.List.all (L_Length + 1 .. Length) := Right.List.all;
      --  copy not only the reference, but also the value
      for I in Result.List.all'Range loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Array)
         return Element_Access_List is
      L_Length : constant Integer := Left.List.all'Length;
      R_Length : constant Integer := Right'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Element_Access_List;
   begin
      --  first check if every element of the element array is valid
      for I in Right'Range loop
         if Right (I) = null then
            raise Invalid_Element_Error;
         end if;
      end loop;
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left.List.all;
      Result.List.all (L_Length + 1 .. Length) := Right;
      --  copy not only the reference, but also the value
      for I in Result.List.all'Range loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Array;
                 Right : in Element_Access_List)
         return Element_Access_List is
      L_Length : constant Integer := Left'Length;
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Result   : Element_Access_List;
   begin
      --  first check if every element of the element array is valid
      for I in Left'Range loop
         if Left (I) = null then
            raise Invalid_Element_Error;
         end if;
      end loop;
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)          := Left;
      Result.List.all (L_Length + 1 .. Length) := Right.List.all;
      --  copy not only the reference, but also the value
      for I in Result.List.all'Range loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Access_Type)
         return Element_Access_List is
      L_Length : constant Integer := Left.List.all'Length;
      Length   : constant Integer := L_Length + 1;
      Result   : Element_Access_List;
   begin
      if Right = null then
         raise Invalid_Element_Error;
      end if;
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)  := Left.List.all;
      Result.List.all (L_Length + 1)   := Right;
      --  copy not only the reference, but also the value
      for I in 1 .. Length loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Access_Type;
                 Right : in Element_Access_List)
         return Element_Access_List is
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := R_Length + 1;
      Result   : Element_Access_List;
   begin
      if Left = null then
         raise Invalid_Element_Error;
      end if;
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1)           := Left;
      Result.List.all (2 .. Length) := Right.List.all;
      --  copy not only the reference, but also the value
      for I in 1 .. Length loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Access_List;
                 Right : in Element_Type)
         return Element_Access_List is
      L_Length : constant Integer := Left.List.all'Length;
      Length   : constant Integer := L_Length + 1;
      Result   : Element_Access_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1 .. L_Length)        := Left.List.all;
      --  copy not only the reference, but also the value
      for I in 1 .. L_Length loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      Result.List.all (L_Length + 1) := new Element_Type'(Right);
      return Result;
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Element_Type;
                 Right : in Element_Access_List)
         return Element_Access_List is
      R_Length : constant Integer := Right.List.all'Length;
      Length   : constant Integer := R_Length + 1;
      Result   : Element_Access_List;
   begin
      Result.List := new Element_Array (1 .. Length);
      Result.List.all (1)           := new Element_Type'(Left);
      Result.List.all (2 .. Length) := Right.List.all;
      --  copy not only the reference, but also the value
      for I in 2 .. Length loop
         Result.List.all (I) := new Element_Type'(Result.List.all (I).all);
      end loop;
      return Result;
   end "&";
   pragma Inline ("&");


   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Access_List) is
      L_Length : constant Natural := List.List.all'Length;
      R_Length : constant Natural := Element.List.all'Length;
      Length   : constant Integer := L_Length + R_Length;
      Ret_List : Element_Array (1 .. Length);
   begin
      --  copy only the reference, not the value
      Ret_List (1 .. L_Length)          := List.List.all;
      Ret_List (L_Length + 1 .. Length) := Element.List.all;
      --  we have to copy the values of our new Elements
      for I in L_Length + 1 .. Length loop
         Ret_List (I) := new Element_Type'(Ret_List (I).all);
      end loop;
      --  don't free our reference value
      if List.List /= Null_Element_Access_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;


   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Array) is
      LLength  : constant Natural := List.List.all'Length;
      Ret_List : Element_Array (1 .. LLength + Element'Length);
   begin
      --  first check if every element of the element array is valid
      for I in Element'Range loop
         if Element (I) = null then
            raise Invalid_Element_Error;
         end if;
      end loop;
      --  copy only the reference, not the value
      Ret_List (1 .. LLength) := List.List.all;
      Ret_List (LLength + 1 .. LLength + Element'Length) := Element;
      --  we have to copy the values of our new Elements
      for I in LLength + 1 .. LLength + Element'Length loop
         Ret_List (I) := new Element_Type'(Ret_List (I).all);
      end loop;
      --  don't free our reference value
      if List.List /= Null_Element_Access_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;


   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Access_Type) is
      LLength  : constant Natural := List.List.all'Length;
      Ret_List : Element_Array (1 .. LLength + 1);
   begin
      if Element = null then
         raise Invalid_Element_Error;
      end if;
      --  copy only the reference, not the value
      Ret_List (1 .. LLength) := List.List.all;
      Ret_List (LLength + 1)  := Element;
      --  don't free our reference value
      if List.List /= Null_Element_Access_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;


   procedure Append (List    : in out Element_Access_List;
                     Element : in     Element_Type) is
      LLength  : constant Natural := List.List.all'Length;
      Ret_List : Element_Array (1 .. LLength + 1);
   begin
      --  copy only the reference, not the value
      Ret_List (1 .. LLength) := List.List.all;
      Ret_List (LLength + 1)  := new Element_Type'(Element);
      --  don't free our reference value
      if List.List /= Null_Element_Access_List.List then
         Deallocate (List.List);
      end if;
      List.List := new Element_Array'(Ret_List);
   end Append;



end Generic_List_Access_Types;
