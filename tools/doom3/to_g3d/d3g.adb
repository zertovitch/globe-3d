with YYParse;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;
with Doom3_IO;
with GLOBE_3D, GLOBE_3D.Math;

with Doom3_Help;

procedure D3G is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: d3g [option] input_file" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "D3G translates a Doom 3 or Quake 4 processed map file" );
    Put_Line( Standard_Error, "(.proc) into GLOBE_3D objects (.g3d) and bsp (.bsp) files." );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "options:" );
    Put_Line( Standard_Error, " -J[unk]      : junk directories of texture names" );
    Put_Line( Standard_Error, " -A[reas]     : consider areas only, junk other models" );
    Put_Line( Standard_Error, " -Carea-number: center on a certain area" );
    Put_Line( Standard_Error, " -S(x,y,z)    : shifts centering by (x,y,z)" );
  end Syntax;

  procedure Set_new_centre(ps: String) is
    use GLOBE_3D.Math;
    p: GLOBE_3D.Point_3D;
    j1,j2: Natural;
  begin
    j1:= ps'First;
    for d in Doom3_Help.main_centre'Range loop
      j2:= j1;
      while
        j2 <= ps'Last and then
        not (ps(j2) = ',' or ps(j2) = ';' or ps(j2) = ')')
      loop
        j2:= j2 + 1;
      end loop;
      p(d):= Doom3_Help.Real'Value(ps(j1..j2-1));
      j1:= j2+1;
    end loop;
    Doom3_Help.main_centre:= Doom3_Help.main_centre + p;
  end Set_new_centre;


begin
  for i in 1..Argument_Count loop
    declare
      arg  : constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
          when 'J' =>
            Doom3_Help.junk_dirs:= True;
          when 'A' =>
            Doom3_Help.areas_only:= True;
          when 'S' =>
            if arg'length < 4 then
              Syntax;
              return;
            end if;
            if arg(3) = '(' then -- -S(x,y,z)
              Set_new_centre(arg(4..arg'Last));
            else
              Syntax;
              return;
            end if;
          when 'C' =>            -- -Carea-number
            Doom3_Help.area_centering:= Integer'Value(arg(3..arg'Last));
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
              "D3G from '" & arg & "'." );
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
