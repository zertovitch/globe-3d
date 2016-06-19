--  https://en.wikipedia.org/wiki/Wavefront_.obj_file

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure O2G is
  o, m: File_Type;
begin
  if Argument_Count = 0 then
    Put_Line(Standard_Error, "Syntax: o2g model[.obj]");
  else
    declare
      o_name: constant String:= Argument(1);
    begin
      if Index(".obj", o_name) = 0 then
        Open(o, In_File, o_name & ".obj");
      else
        Open(o, In_File, o_name);
      end if;
      -- !! parsing happens here :-)
      Close(o);
    end;
  end if;        
end O2G;
