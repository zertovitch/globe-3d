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

with Interfaces.C.Wstrings;

package X_Wide_Strings is

   type X_Wide_String is private;
   Null_X_Wide_String : constant X_Wide_String;

   function Is_Equal (Left, Right : in X_Wide_String) return Boolean;

   function Length (Item : in X_Wide_String) return Natural;

   procedure Free (What : in out X_Wide_String);

   function "&" (Left, Right : in X_Wide_String) return X_Wide_String;
   function "&" (Left : in X_Wide_String; Right : in Wide_String) return X_Wide_String;
   function "&" (Left : in X_Wide_String; Right : in Wide_Character) return X_Wide_String;
   function "&" (Left : in Wide_String; Right : in X_Wide_String) return X_Wide_String;
   function "&" (Left : in Wide_Character; Right : in X_Wide_String) return X_Wide_String;

   procedure Append (To : in out X_Wide_String; Item : in X_Wide_String);
   procedure Append (To : in out X_Wide_String; Item : in Wide_String);
   procedure Append (To : in out X_Wide_String; Item : in Wide_Character);

   function To_X_Wide_String (Item : in Wide_String) return X_Wide_String;

   function To_Wide_String (Item : in X_Wide_String) return Wide_String;


private

--   type X_Wide_String is access all Interfaces.C.wchar_t;
   type X_Wide_String is new Interfaces.C.Wstrings.wchars_ptr;
   Null_X_Wide_String : constant X_Wide_String :=
      X_Wide_String (Interfaces.C.Wstrings.Null_WPtr);

end X_Wide_Strings;
