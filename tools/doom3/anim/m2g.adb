with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;

with MD5_Help, md5_io, YYParse;

procedure M2G is
begin
  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
    begin
      MD5_Help.Reset_globals;
      md5_io.Open_Input (fname => arg);
      Put_Line(Current_Error,
        "m2g: parsing '" & arg & "'." );
      YYParse;
    exception
      when Name_Error =>
        Put_Line( Current_Error, "Input file '" & arg & "' not found." );
        return;
    end;
  end loop;
end;

