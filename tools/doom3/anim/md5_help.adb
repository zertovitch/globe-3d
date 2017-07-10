with Ada.Text_IO;                       use Ada.Text_IO;

package body MD5_Help is 

  procedure YY_Accept is
  begin
    null;
  end YY_Accept;

  procedure YY_Abort is
  begin
    Put_Line(Current_Error, "YY_Abort");
  end;

  procedure YY_Terminate is
  begin
    Put_Line(Current_Error, "YY_Terminate");
  end;
  
  procedure MD5_Comment(s: String) is
  begin
    null;
  end;

  procedure Reset_globals is
  begin
    linenum:= 0;
  end;

end;