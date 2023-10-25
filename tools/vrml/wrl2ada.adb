with YYParse;

with Ada.Command_Line,
     Ada.Characters.Handling,
     Ada.Text_IO;

with VRML_Help,
     VRML_IO;

procedure Wrl2Ada is

  use Ada.Command_Line, Ada.Characters.Handling, Ada.Text_IO;

  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line (Standard_Error, "Syntax: wrl2ada [-p[retty]] [input_file]");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Wrl2Ada translates a VRML scene file (.vrml, .wrl)");
    Put_Line (Standard_Error, "into an Ada source code of a GLOBE_3D object builder.");
    New_Line (Standard_Error);
  end Syntax;

begin
  for i in 1 .. Argument_Count loop
    declare
      arg   : constant String := Argument (i);
      u_arg : constant String := To_Upper (arg);
    begin
      if u_arg'Length > 1 and then u_arg (1) in '-' | '/' then
        case u_arg (2) is
          when 'P' =>
            VRML_Help.pretty := True;
          when others =>  --  includes "-h" option, for "help"
            Syntax;
            return;
        end case;
      else  --  no option
        if Inp_Opened then          --  Two inputs ?!
          VRML_IO.Close_Input;
          Syntax;
          return;
        else
          begin
            VRML_IO.Open_Input (fname => arg);
            Inp_Opened := True;
            Put_Line
              (Standard_Error,
               "Wrl2Ada from '" & arg & "' to Standard_Output");
          exception
            when Name_Error =>
              Put_Line
                (Standard_Error, "Input file '" & arg &
                 "' not found.");
              Syntax;
              return;
          end;
        end if;
      end if;
    end;
  end loop;

  if not Inp_Opened then
    Put_Line
      (Standard_Error,
       "Wrl2Ada from Standard_Input to Standard_Output. Rerun ""Wrl2Ada -h"" for help.");
  end if;

  VRML_Help.has_input := Inp_Opened;

  YYParse;

  if Inp_Opened then
    VRML_IO.Close_Input;
  end if;

end Wrl2Ada;
