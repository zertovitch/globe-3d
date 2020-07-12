with YYParse;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;
with Doom3_IO;

with Doom3_Help;

procedure D3A is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: d3a [option] [input_file]" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "D3A translates a Doom 3 or Quake 4 processed map file" );
    Put_Line( Standard_Error, "(.proc) into an Ada source code of a GLOBE_3D object builder." );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "options:" );
    Put_Line( Standard_Error, " -P[retty] : pretty output" );
    Put_Line( Standard_Error, " -J[unk]   : junk directories of texture names" );
    Put_Line( Standard_Error, " -A[reas]  : consider areas only, junk other models" );
  end Syntax;

begin
  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'Length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/')
      then
        case u_arg(2) is
          when 'P' =>
            Doom3_Help.pretty:= True;
          when 'J' =>
            Doom3_Help.junk_dirs:= True;
          when 'A' =>
            Doom3_Help.areas_only:= True;
          when others =>  -- includes "-h", "/?" etc.
            Syntax;
            return;
        end case;
      else -- no option
        if Inp_Opened then          -- Two inputs ?!
          Doom3_IO.Close_Input;
          Syntax;
          return;
        else
          Doom3_Help.argument_pos_source:= i;
          begin
            Doom3_IO.Open_Input (fname => arg);
            Inp_Opened := True;
            Put_Line(Standard_Error,
              "D3A from '" & arg & "' to Standard_Output" );
          exception
            when Name_Error =>
              Put_Line( Standard_Error, "Input file '" & arg &
                "' not found." );
              Syntax;
              return;
          end;
        end if;
      end if;
    end;
  end loop;

  if not Inp_Opened then
    Put_Line (Standard_Error,
      "D3A from Standard_Input to Standard_Output. Rerun ""D3A -h"" for help." );
  end if;

  Doom3_Help.has_input := Inp_Opened;

  Doom3_Help.Ada_Begin;

  YYParse;

  if Inp_Opened then
    Doom3_IO.Close_Input;
  end if;

end D3A;
