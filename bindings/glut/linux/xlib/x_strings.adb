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

with Interfaces.C,
     Unchecked_Conversion,
     Unchecked_Deallocation;

package body X_Strings is

   --  for address operation may be used Interfaces.C.Pointers

   use type Interfaces.C.unsigned_long;

   -- some conversion routines
   function To_Long_Uns is
      new Unchecked_Conversion (X_String, Interfaces.C.unsigned_long);
   function To_X_String is
      new Unchecked_Conversion (Interfaces.C.unsigned_long, X_String);


   procedure Next_Char (Pointer : in out X_String) is
   begin
      Pointer := To_X_String (To_Long_Uns (Pointer) + 1);
   end Next_Char;
   pragma Inline (Next_Char);


   procedure Prev_Char (Pointer : in out X_String) is
   begin
      Pointer := To_X_String (To_Long_Uns (Pointer) - 1);
   end Prev_Char;
   pragma Inline (Prev_Char);



   procedure Free (What : in out X_String) is
   begin
      if What /= Null_X_String then
         declare
            subtype Specific_String is String (1 .. Length(What) + 1);
            type Specific_String_Access is access Specific_String;
            pragma Controlled (Specific_String_Access);
            procedure Deallocate_X_String is new Unchecked_Deallocation (Specific_String, Specific_String_Access);
            function To_String is new Unchecked_Conversion (X_String, Specific_String_Access);
            Temp_String : Specific_String_Access := To_String (What);
         begin
            Deallocate_X_String (Temp_String);
            What := Null_X_String;
         end;
      end if;
   end Free;



   function Is_Equal (Left, Right : in X_String) return Boolean is
      Left_Len  : constant Natural := Length (Left);
      Right_Len : constant Natural := Length (Right);
   begin
      if Left_Len /= Right_Len then
         return False;
      elsif Left_Len = 0 then  -- both arguments have no length
         return True;
      else
         declare
            Left_Acc  : X_String := Left;
            Right_Acc : X_String := Right;
         begin
            for I in 1 .. Left_Len loop
               if Left_Acc.all = Right_Acc.all then
                  Next_Char (Left_Acc);
                  Next_Char (Right_Acc);
               else
                  return False;
               end if;
            end loop;
            return True;
         end;
      end if;
   end Is_Equal;


   function Length (Item : in X_String) return Natural is
      Indx    : Natural := 0;
      Ch      : Character;
      Mom_Acc : X_String := Item;
   begin
      if Item = Null_X_String then
         return 0;
      else
         -- look for the terminating Null
         loop
            Ch := Mom_Acc.all;
            if Ch /= Character'Val (0) then
               Indx := Indx + 1;
               Next_Char (Mom_Acc);
            else
               exit;
            end if;
         end loop;
         return Indx;
      end if;
   end Length;


   function "&" (Left, Right : in X_String) return X_String is
      Left_Len  : constant Natural := Length (Left);
      Right_Len : constant Natural := Length (Right);
   begin
      if Left_Len+Right_Len > 0 then
         declare
            Str       : String (1 .. Left_Len+Right_Len);
            Mom_Acc   : X_String;
            Arry_Indx : Natural := 1;
         begin
            Mom_Acc := Left;
            for I in 1 .. Left_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            Mom_Acc := Right;
            for I in 1 .. Right_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            return To_X_String (Str);
         end;
      else
         return Null_X_String;
      end if;
   end "&";


   function "&" (Left : in X_String; Right : in String) return X_String is
      Left_Len  : constant Natural := Length (Left);
      Right_Len : constant Natural := Right'Length;
   begin
      if Left_Len+Right_Len > 0 then
         declare
            Str        : String (1 .. Left_Len+Right_Len);
            Mom_Acc    : X_String;
            Arry_Indx  : Natural := 1;
         begin
            Mom_Acc := Left;
            for I in 1 .. Left_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            for I in Right'Range loop
               Str (Arry_Indx) := Right (I);
               Arry_Indx       := Arry_Indx + 1;
            end loop;
            return To_X_String (Str);
         end;
      else
         return Null_X_String;
      end if;
   end "&";


   function "&" (Left : in X_String; Right : in Character) return X_String is
      Left_Len  : constant Natural := Length (Left);
   begin
      declare
         Str        : String (1 .. Left_Len+1);
         Mom_Acc    : X_String;
         Arry_Indx  : Natural := 1;
      begin
         Mom_Acc := Left;
         for I in 1 .. Left_Len loop
            Str (Arry_Indx) := Mom_Acc.all;
            Arry_Indx       := Arry_Indx + 1;
            Next_Char (Mom_Acc);
         end loop;
         Str (Arry_Indx)   := Right;
         return To_X_String (Str);
      end;
   end "&";


   function "&" (Left : in String;    Right : in X_String) return X_String is
      Left_Len  : constant Natural := Left'Length;
      Right_Len : constant Natural := Length (Right);
   begin
      if Left_Len+Right_Len > 0 then
         declare
            Str        : String (1 .. Left_Len+Right_Len);
            Mom_Acc    : X_String;
            Arry_Indx  : Natural := 1;
         begin
            for I in Left'Range loop
               Str (Arry_Indx) := Left (I);
               Arry_Indx       := Arry_Indx + 1;
            end loop;
            Mom_Acc := Right;
            for I in 1 .. Right_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            return To_X_String (Str);
         end;
      else
         return Null_X_String;
      end if;
   end "&";


   function "&" (Left : in Character; Right : in X_String) return X_String is
      Right_Len : constant Natural := Length (Right);
   begin
      declare
         Str        : String (1 .. Right_Len+1);
         Mom_Acc    : X_String;
         Arry_Indx  : Natural := 1;
      begin
         Str (Arry_Indx) := Left;
         Arry_Indx       := Arry_Indx + 1;
         Mom_Acc := Right;
         for I in 1 .. Right_Len loop
            Str (Arry_Indx) := Mom_Acc.all;
            Arry_Indx       := Arry_Indx + 1;
            Next_Char (Mom_Acc);
         end loop;
         return To_X_String (Str);
      end;
   end "&";





   procedure Append (To : in out X_String; Item : in X_String) is
      Left_Len  : constant Natural := Length (To);
      Right_Len : constant Natural := Length (Item);
   begin
      if Right_Len > 0 then
         declare
            Str       : String (1 .. Left_Len+Right_Len);
            Mom_Acc   : X_String;
            Arry_Indx : Natural := 1;
         begin
            Mom_Acc := To;
            for I in 1 .. Left_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            Mom_Acc := Item;
            for I in 1 .. Right_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            Free (To);
            To              := To_X_String (Str);
         end;
      end if;
   end Append;


   procedure Append (To : in out X_String; Item : in String) is
      Left_Len  : constant Natural := Length (To);
      Right_Len : constant Natural := Item'Length;
   begin
      if Right_Len > 0 then
         declare
            Str       : String (1 .. Left_Len+Right_Len);
            Mom_Acc   : X_String;
            Arry_Indx : Natural := 1;
         begin
            Mom_Acc := To;
            for I in 1 .. Left_Len loop
               Str (Arry_Indx) := Mom_Acc.all;
               Arry_Indx       := Arry_Indx + 1;
               Next_Char (Mom_Acc);
            end loop;
            for I in Item'Range loop
               Str (Arry_Indx) := Item (I);
               Arry_Indx       := Arry_Indx + 1;
            end loop;
            Free (To);
            To              := To_X_String (Str);
         end;
      end if;
   end Append;



   procedure Append (To : in out X_String; Item : in Character) is
      Left_Len  : constant Natural := Length (To);
   begin
      declare
         Str       : String (1 .. Left_Len+1);
         Mom_Acc   : X_String;
         Arry_Indx : Natural := 1;
      begin
         Mom_Acc := To;
         for I in 1 .. Left_Len loop
            Str (Arry_Indx) := Mom_Acc.all;
            Arry_Indx       := Arry_Indx + 1;
            Next_Char (Mom_Acc);
         end loop;
         Str (Arry_Indx)   := Item;
         Free (To);
         To              := To_X_String (Str);
      end;
   end Append;



   function To_X_String (Item : in String) return X_String is
      subtype Specific_String is String (1 .. Item'Length + 1);
      type Specific_String_Access is access Specific_String;
      pragma Controlled (Specific_String_Access);
      function New_String is new Unchecked_Conversion (Specific_String_Access,
                                                        X_String);
   begin
      if Item'Length > 0 then
         return New_String (new Specific_String'(Item & Character'Val (0)));
      else
         return Null_X_String;
      end if;
   end To_X_String;


   function To_String (Item : in X_String) return String is
      Indx    : Natural := 0;
      Ch      : Character;
      Mom_Acc : X_String := Item;
   begin
      if Item = Null_X_String then
         return String'("");
      else
         loop
            Ch := Mom_Acc.all;
            exit when Ch = Character'Val (0);
               Indx := Indx + 1;
               Next_Char (Mom_Acc);
         end loop;
         declare
            Buffer : String (1 .. Indx);
         begin
            -- Mom_Acc shows to a Character'Val (0) now!
            for I in reverse 1 .. Indx loop
               Prev_Char (Mom_Acc);
               Buffer (I) := Mom_Acc.all;
            end loop;
            return Buffer;
         end;
      end if;
   end To_String;



end X_Strings;
