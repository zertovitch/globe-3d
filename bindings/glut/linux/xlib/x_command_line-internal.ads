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

-------------------------------------------------------------------------------
--
--  this package is PRIVATE for the X11/M*tif-Binding and should NOT be used
--  directly by an application
--
--  it is subject to change without notice!
--
-------------------------------------------------------------------------------

with System;
package X_Command_Line.Internal is

-- ----------------------------------------------------------------------------
--
--  Direct Access to the internal representation of the Argument vector
--
--                 B E   C A R E F U L
--

   -- -------------------------------------------------------------------------
   --
   --  get the address of the internally stored argument vector
   --  (equivalent to the char **argv in C)
   --  should not be used normally
   --  it is necessary for the toolkit initialization
   --  routines of the X Toolkit Intrinsics
   --
   function Get_Argument_Hook return String_List_Conversion.Chars_Ptr_List_Type;
   function Get_Argument_Hook
     (Arg : in Argument_Vector_Type)
      return String_List_Conversion.Chars_Ptr_List_Type;

   -- -------------------------------------------------------------------------
   --
   --  tell the command line package that the number of arguments has changed
   --  VERY DANGEROUS, because the new number of arguments can't be checked!
   --
   procedure Actualize_Arguments
     (New_Number : in String_List_Conversion.Index_Type);
   procedure Actualize_Arguments
     (Arg        : in out Argument_Vector_Type;
      New_Number : in     String_List_Conversion.Index_Type);


end X_Command_Line.Internal;
