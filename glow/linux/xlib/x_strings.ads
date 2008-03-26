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

package X_Strings is
pragma Preelaborate (X_Strings);

   type X_String is private;
   Null_X_String : constant X_String;


   function Is_Equal (Left, Right : in X_String) return Boolean;

   function Length (Item : in X_String) return Natural;

   procedure Free (What : in out X_String);


   function "&" (Left, Right : in X_String) return X_String;
   function "&" (Left : in X_String;  Right : in String) return X_String;
   function "&" (Left : in X_String;  Right : in Character) return X_String;
   function "&" (Left : in String;    Right : in X_String) return X_String;
   function "&" (Left : in Character; Right : in X_String) return X_String;

   procedure Append (To : in out X_String; Item : in X_String);
   procedure Append (To : in out X_String; Item : in String);
   procedure Append (To : in out X_String; Item : in Character);


   function To_X_String (Item : in String) return X_String;

   function To_String (Item : in X_String) return String;

private

   type X_String is access all Character;
   Null_X_String : constant X_String := null;

end X_Strings;
