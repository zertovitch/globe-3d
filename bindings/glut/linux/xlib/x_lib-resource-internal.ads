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

package X_Lib.Resource.Internal is

   -- this is how Xlib sees the option description record
   --
   type Xrm_Option_Desc_Rec is record
      Option_Name  : Chars_Ptr;
      Specifier    : Chars_Ptr;
      Option_Kind  : Xrm_Option_Kind;
      Option_Value : X_Pointer;
   end record;

   Null_Option_Desc_Rec : constant Xrm_Option_Desc_Rec :=
        (Option_Name => Null_Ptr,
         Specifier   => Null_Ptr,
         Option_Kind => Option_No_Arg,
         Option_Value => Null_X_Pointer);
         

   type Xrm_Option_Array is array (Positive range <>) of Xrm_Option_Desc_Rec;

   type Xrm_Option_Desc_List is access all Xrm_Option_Array;

   -- we care for the allocation and deallocation
   pragma Controlled (Xrm_Option_Desc_List);

   function To_Xrm_Option_Desc_List (Opt_List    : in Option_Description_List)
      return Xrm_Option_Desc_List;


   procedure Free (Object : in out Xrm_Option_Desc_List);

end X_Lib.Resource.Internal;
