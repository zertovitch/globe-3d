with Ada.Text_IO;                       use Ada.Text_IO;

-- Project specific:
with MD5_Help;

procedure YYError (s: in String) is
begin
  Put_Line(Current_Error,s);
  raise MD5_Help.Syntax_Error;
end YYError;
