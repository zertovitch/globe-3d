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

with Ada.Command_Line,
     Interfaces.C.Strings.String_Lists;

package body X_Command_Line is

   use Interfaces.C,
       Interfaces.C.Strings;


   -- -------------------------------------------------------------------------
   --
   --  get/set the complete argument vector
   --
   procedure Get_Arguments (Arg : in out Argument_Vector_Type) is
   begin
      -- if the argument vector is already set, remove its contents
      Interfaces.C.Strings.String_Lists.Free (Arg.Args, Arg.Num_Args);

      -- make a copy of the argument vector stored in Command_Line_Arguments
      Arg := (Args     => Interfaces.C.Strings.String_Lists.Duplicate (Command_Line_Arguments.Args, Command_Line_Arguments.Num_Args),
              Num_Args => Command_Line_Arguments.Num_Args);
   end Get_Arguments;


   procedure Set_Arguments (Arg : in Argument_Vector_Type) is
      Target           : String_List_Conversion.Chars_Ptr_List_Type;
      Args_Hook_Backup : String_List_Conversion.Chars_Ptr_List_Type
                       := Command_Line_Arguments.Args;
      Num_Args_Backup  : String_List_Conversion.Index_Type
                       := Command_Line_Arguments.Num_Args;
   begin
      if Interfaces.C.Strings.String_Lists."/=" (Arg.Args, String_List_Conversion.Null_Chars_Ptr_List) and then
         Interfaces.C."/=" (Arg.Num_Args, 0) then
         -- make a copy of the argument vector stored in Arg
         Target := Interfaces.C.Strings.String_Lists.Duplicate (Arg.Args, Arg.Num_Args);
         -- set the new argument vector
         Command_Line_Arguments := (Args     => Target,
                                    Num_Args => Arg.Num_Args);
         -- free the previous argument vector
         Interfaces.C.Strings.String_Lists.Free (Args_Hook_Backup, Num_Args_Backup);
      else
         raise Empty_Argument_Vector_Error;
      end if;
   end Set_Arguments;



   function Argument_Count return Natural is
   begin
      return Natural (Command_Line_Arguments.Num_Args) - 1; -- dont count the first argument,
                                                            -- this is the command name
   end Argument_Count;
   pragma Inline (Argument_Count);


   function Argument_Count (Arg : in Argument_Vector_Type) return Natural is
   begin
      return Natural (Arg.Num_Args) - 1; -- dont count the first argument,
                                         -- this is the command name
   end Argument_Count;
   pragma Inline (Argument_Count);


   -- return the command-line arguments
   --
   function Argument (Number : in Positive) return String is
   begin
      if Number >= Positive (Command_Line_Arguments.Num_Args) then
         raise Constraint_Error;
      end if;
      return Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Command_Line_Arguments.Args, String_List_Conversion.Index_Type (Number)));
   end Argument;

   function Argument
     (Arg    : in Argument_Vector_Type;
      Number : in Positive)
      return String is
   begin
      if Number >= Positive (Arg.Num_Args) then
         raise Constraint_Error;
      end if;
      return Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Arg.Args, String_List_Conversion.Index_Type (Number)));
   end Argument;


   -- return the command-name
   --
   function Command_Name return String is
   begin
      return Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Command_Line_Arguments.Args, String_List_Conversion.Index_Type (0)));
   end Command_Name;
   pragma Inline (Command_Name);

   function Command_Name (Arg : in Argument_Vector_Type) return String is
   begin
      return Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Arg.Args, String_List_Conversion.Index_Type (0)));
   end Command_Name;
   pragma Inline (Command_Name);



   -- -------------------------------------------------------------------------
   --
   --  change the command name as seen by the routines which use this package
   --
   procedure Set_Command_Name (Name : in String) is
   begin
      if Name'Length > 0 then
         Interfaces.C.Strings.String_Lists.Update (Command_Line_Arguments.Args,
	                                           0,
						   Interfaces.C.Strings.New_String (Name));
      else
         raise Empty_Command_Name_Error;
      end if;
   end Set_Command_Name;


   procedure Set_Command_Name
     (Arg  : in out Argument_Vector_Type;
      Name : in String) is
   begin
      if Name'Length > 0 then
         Interfaces.C.Strings.String_Lists.Update (Arg.Args,
	                                           0,
						   Interfaces.C.Strings.New_String (Name));
      else
         raise Empty_Command_Name_Error;
      end if;
   end Set_Command_Name;


   -- -------------------------------------------------------------------------
   --
   --  get the command line arguments
   --
   function Get_All_Arguments return String_List.Element_Access_List is
      All_Args : String_List.Element_Access_List;
   begin
      -- skip the command name!
      for I in 1 .. Command_Line_Arguments.Num_Args-1 loop
         String_List.Append (All_Args,
	    Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Command_Line_Arguments.Args, I)));
      end loop;
      return All_Args;
   end Get_All_Arguments;


   function Get_All_Arguments (Arg  : in Argument_Vector_Type) return String_List.Element_Access_List is
      All_Args : String_List.Element_Access_List;
   begin
      -- skip the command name!
      for I in 1 .. Arg.Num_Args-1 loop
         String_List.Append (All_Args,
	    Interfaces.C.Strings.Value (Interfaces.C.Strings.String_Lists.Value (Arg.Args, I)));
      end loop;
      return All_Args;
   end Get_All_Arguments;



   -- -------------------------------------------------------------------------
   --
   --  set the command line arguments
   --
   procedure Set_All_Arguments (String_Args : in String_List.Element_Access_List) is
      Len              : Natural := String_List.Length (String_Args);
      Args_Hook_Backup : String_List_Conversion.Chars_Ptr_List_Type
                       := Command_Line_Arguments.Args;
      Num_Args_Backup  : String_List_Conversion.Index_Type
                       := Command_Line_Arguments.Num_Args;
      New_Argv         : chars_ptr_array (0 .. String_List_Conversion.Index_Type (Len));
   begin
      -- think of the command name!
      New_Argv (0) := Interfaces.C.Strings.New_String (Command_Name);
      -- it is easier simply to copy the command name
      for I in 1 .. Len loop
         New_Argv (String_List_Conversion.Index_Type (I)) := Interfaces.C.Strings.New_String (String_List.Element (String_Args, I));
      end loop;

      -- set the new argument vector
      Command_Line_Arguments := (Args     => Interfaces.C.Strings.String_Lists.New_Chars_Ptr_Array (New_Argv, False),
                                 Num_Args => String_List_Conversion.Index_Type (Len + 1));
      -- free the previous argument vector
      Interfaces.C.Strings.String_Lists.Free (Args_Hook_Backup, Num_Args_Backup);
   end Set_All_Arguments;


   procedure Set_All_Arguments
     (Arg         : in out Argument_Vector_Type;
      String_Args : in     String_List.Element_Access_List) is
      Len              : Natural := String_List.Length (String_Args);
      Args_Hook_Backup : String_List_Conversion.Chars_Ptr_List_Type
                       := Arg.Args;
      Num_Args_Backup  : String_List_Conversion.Index_Type
                       := Arg.Num_Args;
      New_Argv         : chars_ptr_array (0 .. String_List_Conversion.Index_Type (Len));
   begin
      -- think of the command name!
      New_Argv (0) := Interfaces.C.Strings.New_String (Command_Name (Arg));
      -- it is easier simply to copy the command name
      for I in 1 .. Len loop
         New_Argv (String_List_Conversion.Index_Type (I)) := Interfaces.C.Strings.New_String (String_List.Element (String_Args, I));
      end loop;

      -- set the new argument vector
      Arg := (Args     => Interfaces.C.Strings.String_Lists.New_Chars_Ptr_Array (New_Argv, False),
              Num_Args => String_List_Conversion.Index_Type (Len + 1));
      -- free the previous argument vector
      Interfaces.C.Strings.String_Lists.Free (Args_Hook_Backup, Num_Args_Backup);
   end Set_All_Arguments;



   procedure Initialize_Command_Line_Arguments is
      Argc     : Natural := Ada.Command_Line.Argument_Count;
      New_Argv : chars_ptr_array (0 .. String_List_Conversion.Index_Type (Argc));
   begin
      New_Argv (0) := Interfaces.C.Strings.New_String (Ada.Command_Line.Command_Name);
      for I in 1 .. Argc loop
         New_Argv (String_List_Conversion.Index_Type (I)) := Interfaces.C.Strings.New_String (Ada.Command_Line.Argument (I));
      end loop;
      Command_Line_Arguments := (Args     => Interfaces.C.Strings.String_Lists.New_Chars_Ptr_Array (New_Argv, False),
                                 Num_Args => New_Argv'Length);
   end Initialize_Command_Line_Arguments;


begin
   Initialize_Command_Line_Arguments;
end X_Command_Line;
