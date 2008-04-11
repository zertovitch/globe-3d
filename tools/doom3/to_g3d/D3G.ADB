with YYParse;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;
with Doom3_IO;

with Doom3_Help;

procedure D3G is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: d3g [option] input_file" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "D3G translates a Doom 3 or Quake 4 processed map file" );
    Put_Line( Standard_Error, "(.proc) into GLOBE_3D objects (.g3d), bsp (.bsp)." );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "options:" );
    Put_Line( Standard_Error, " -J[unk]   : junk directories of texture names" );
    Put_Line( Standard_Error, " -A[reas]  : consider areas only, junk other models" );
  end Syntax;

begin
  for i in 1..Argument_Count loop
    declare
      arg: String:= Argument(i);
      u_arg: String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
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
            Put_Line(Standard_error,
              "D3G from '" & arg & "." );
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

  if not Inp_opened then
    Put_Line(Standard_error,"Missing input file!");
    Syntax;
    return;
  end if;

  Doom3_Help.has_input:= inp_opened;

  Doom3_Help.D3G_Init;

  YYParse;

  if Inp_Opened then
    Doom3_IO.Close_Input;
  end if;

end D3G;
