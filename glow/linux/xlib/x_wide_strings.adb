-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
--  This file also copyright (c) 2001 Vadim Godunko                          --
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
--          22 Jun 2001 Vadim Godunko (purely his patch, no changes)
--
-------------------------------------------------------------------------------

package body X_Wide_Strings is

   use Interfaces.C;
   use Interfaces.C.Wstrings;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : in X_Wide_String) return X_Wide_String is
      L : constant wchar_array := Value (Left);
      R : constant wchar_array := Value (Right);
   begin
      return New_Char_Array (L (L'First .. L'Last - 1) & R);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : in X_Wide_String;
      Right : in Wide_String)
      return X_Wide_String
   is
      L : constant wchar_array := Value (Left);
   begin
      return New_Char_Array (L (L'First .. L'Last) & To_C (Right));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : in X_Wide_String;
      Right : in Wide_Character)
      return X_Wide_String
   is
      L : constant wchar_array := Value (Left);
   begin
      return New_Char_Array (L (L'First .. L'Last) & To_C (Right));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : in Wide_String;
      Right : in X_Wide_String)
      return X_Wide_String
   is
   begin
      return New_Char_Array (To_C (Left, False) & Value (Right));
   end "&";

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : in Wide_Character;
      Right : in X_Wide_String)
      return X_Wide_String
   is
   begin
      return New_Char_Array (To_C (Left) & Value (Right));
   end "&";

   ------------
   -- Append --
   ------------

   procedure Append (To : in out X_Wide_String; Item : in X_Wide_String) is
      Tmp : X_Wide_String := To & Item;
   begin
      Free (To);
      To := Tmp;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (To : in out X_Wide_String; Item : in Wide_String) is
      Tmp : X_Wide_String := To & Item;
   begin
      Free (To);
      To := Tmp;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (To : in out X_Wide_String; Item : in Wide_Character) is
      Tmp : X_Wide_String := To & Item;
   begin
      Free (To);
      To := Tmp;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (What : in out X_Wide_String) is
   begin
      Interfaces.C.Wstrings.Free (wchars_ptr (What));
   end Free;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Left, Right : in X_Wide_String) return Boolean is
      Left_Length : constant Natural := Length (Left);
   begin
      if Left = Right then
         return True;
      elsif Left_Length /= Length (Right) then
         return False;
      elsif Left_Length = 0 then
         return True;
      else
         declare
            Left_Value  : constant wchar_array := Value (Left);
            Right_Value : constant wchar_array := Value (Right);
         begin
            return Left_Value = Right_Value;
         end;
      end if;
   end Is_Equal;

   ------------
   -- Length --
   ------------

   function Length (Item : in X_Wide_String) return Natural is
   begin
      return Natural (Strlen (Item));
   end Length;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Item : in X_Wide_String) return Wide_String is
   begin
      return Value (Item);
   end To_Wide_String;

   ----------------------
   -- To_X_Wide_String --
   ----------------------

   function To_X_Wide_String (Item : in Wide_String) return X_Wide_String is
   begin
      return New_String (Item);
   end To_X_Wide_String;

end X_Wide_Strings;
