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
--                                                                           --
--  This package replaces part of the package Ada.Command_Line, because we   --
--  need to pass the argument vector to the toolkit initialization routines, --
--  where it is eventually modified                                          --
--                                                                           --
--  Every action on the argument vector is therefore made with a copy of the --
--  original vector                                                          --
--                                                                           --
-------------------------------------------------------------------------------

with System,
     String_List,
     String_List_Conversion;
package X_Command_Line is


   -- -------------------------------------------------------------------------
   --
   --  the type we store the argument vector
   --  only restricted access is allowed
   --
   type Argument_Vector_Type is limited private;


   -- -------------------------------------------------------------------------
   --
   --  get/set the complete argument vector
   --
   procedure Get_Arguments (Arg : in out Argument_Vector_Type);
   procedure Set_Arguments (Arg : in Argument_Vector_Type);

   -- error to be raised if the arguments to be set aren't defined
   --
   Empty_Argument_Vector_Error : exception;


   -- -------------------------------------------------------------------------
   --
   --  get the number of command-line arguments (in contrary to C,
   --  the command name is not included here)
   --
   function Argument_Count return Natural;
   function Argument_Count (Arg : in Argument_Vector_Type) return Natural;

   -- -------------------------------------------------------------------------
   --
   --  return the command-line arguments
   --
   function Argument (Number : in Positive) return String;
   function Argument
     (Arg    : in Argument_Vector_Type;
      Number : in Positive)
      return String;

   -- -------------------------------------------------------------------------
   --
   --  return the command name of the executable being run
   --
   function Command_Name return String;
   function Command_Name (Arg : in Argument_Vector_Type) return String;



   -- -------------------------------------------------------------------------
   --
   --  change the command name as seen by the routines which use this package
   --
   procedure Set_Command_Name (Name : in String);
   procedure Set_Command_Name
     (Arg  : in out Argument_Vector_Type;
      Name : in String);

   -- error to be raised if the command name given is a null string
   --
   Empty_Command_Name_Error : exception;


-- ----------------------------------------------------------------------------
--
--     get/set all arguments (NOT the command name) represented as a
--     string list for easier modification
--

   -- -------------------------------------------------------------------------
   --
   --  get the command line arguments
   --
   function Get_All_Arguments return String_List.Element_Access_List;
   function Get_All_Arguments (Arg  : in Argument_Vector_Type) return String_List.Element_Access_List;


   -- -------------------------------------------------------------------------
   --
   --  set the command line arguments
   --
   procedure Set_All_Arguments (String_Args : in String_List.Element_Access_List);
   procedure Set_All_Arguments
     (Arg         : in out Argument_Vector_Type;
      String_Args : in     String_List.Element_Access_List);



private

   type Argument_Vector_Type is record
      Args     : String_List_Conversion.Chars_Ptr_List_Type := String_List_Conversion.Null_Chars_Ptr_List;
      Num_Args : String_List_Conversion.Index_Type := 0;
   end record;

   Command_Line_Arguments : Argument_Vector_Type;

end X_Command_Line;
