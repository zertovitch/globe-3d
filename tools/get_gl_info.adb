with GL, GLUT;
with Ada.Text_IO; use Ada.Text_IO;

procedure Get_GL_Info is
  f: File_Type;
  use GL, GLUT;
  GLUT_options: constant GLUT.Unsigned:=
    GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH or GLUT.MULTISAMPLE;
  procedure Put_Split(s: String) is
  begin
    for i in s'Range loop
      if s(i)=' ' then
        New_Line(f);
      else
        Put(f, s(i));
      end if;
    end loop;
  end Put_Split;
begin
  GLUT.Init;
  GLUT.InitDisplayMode( GLUT_options );
  if GLUT.CreateWindow( "Test" ) = 0 then return; end if;
  Create(f, Out_File,"gl_info.txt");
  Put_Line(f,"GL.GetString:");
  for i in GL.StringEnm loop
    Put(f, "  " & GL.StringEnm'Image(i) & ": [");
    if i = GL.EXTENSIONS then
      Put_Split( GL.GetString(i) );
    else
      Put(f, GL.GetString(i));
    end if;
    Put_Line(f, "]");
  end loop;
  Put(f,"GLUT.Get(WINDOW_NUM_SAMPLES):" &
      Integer'Image(GLUT.Get(GLUT.WINDOW_NUM_SAMPLES)));
  Close(f);
end Get_GL_Info;
