with Ada.Text_IO;                       use Ada.Text_IO;

procedure YYError (s: in String) is
  syntax_error : exception;
begin
  Put_Line(s);
  Put_Line(Standard_Error,s);
  raise syntax_error;
end YYError;
