with GLUT, System;

package body GLUT_2D is

  GLUT_char: constant array(Font_type) of System.Address:=
  (
    Screen_9_BY_15 => glut.BITMAP_9_BY_15,
    Screen_8_BY_13 => glut.BITMAP_8_BY_13,
    TIMES_ROMAN_10 => glut.BITMAP_TIMES_ROMAN_10,
    TIMES_ROMAN_24 => glut.BITMAP_TIMES_ROMAN_24,
    HELVETICA_10   => glut.BITMAP_HELVETICA_10,
    HELVETICA_12   => glut.BITMAP_HELVETICA_12,
    HELVETICA_18   => glut.BITMAP_HELVETICA_18
  );

  procedure Text_output(
    s    : String;
    font : Font_type
  )
  is
  begin
    for i in s'Range loop
      null; -- tbd: GLUT.BitmapCharacter(GLUT_char(font), Character'Pos(s(i)));
    end loop;
  end Text_output;

  procedure Push_3D_set_2D(main_size_x, main_size_y : GL.SizeI) is
    use GL;
  begin
    -- Push current matrix mode and viewport attributes.
    GL.PushAttrib(GL.TRANSFORM_BIT or GL.VIEWPORT_BIT);
    GL.MatrixMode(GL.PROJECTION);
    GL.PushMatrix; -- In GL.PROJECTION mode, the stack depth is at least 2
    GL.LoadIdentity;
    GL.Ortho(
      0.0, GL.Double(main_size_x), GL.Double(main_size_y), 0.0,
      GL.Double'(-1.0), 1.0);
    GL.MatrixMode(GL.MODELVIEW);
    GL.PushMatrix; -- In GL.MODELVIEW mode, the stack depth is at least 32
    GL.LoadIdentity;
  end Push_3D_set_2D;

  procedure Pop_3D is
  begin
    GL.MatrixMode(GL.MODELVIEW);
    GL.PopMatrix;
    GL.MatrixMode(GL.PROJECTION);
    GL.PopMatrix;
    GL.PopAttrib;
  end Pop_3D;

  procedure Text_output(
    x,y         : GL.Int;
    main_size_x,
    main_size_y : GL.SizeI;
    s           : String;
    font        : Font_type
  )
  is
  begin
    Push_3D_set_2D(main_size_x, main_size_y);
    GL.RasterPos(x,y);
    Text_Output(s,font);
    Pop_3D;
  end Text_output;

  procedure Text_output(
    p    : GL.Double_vector_3D;
    s    : String;
    font : Font_type
  )
  is
  begin
    GL.PushMatrix;
    GL.Translate(p);
    GL.RasterPos(0,0);
    Text_Output(s,font);
    GL.PopMatrix;
  end Text_output;

  procedure Put_Image(
    Image_ID    : Integer;
    x,y         : GL.Int;
    size_x,
    size_y      : GL.Int;
    main_size_x,
    main_size_y : GL.SizeI
  )
  is
  begin
    --  fx:= GL.Float(size_x) / GL.Float(main_size_x);
    --  fy:= GL.Float(size_y) / GL.Float(main_size_y);
    Push_3D_set_2D(main_size_x, main_size_y);
    GL.Translate(GL.Double(x),GL.Double(y),0.0);
    -- GL.Enable( GL.TEXTURE_2D );
    GL.BindTexture(GL.TEXTURE_2D, GL.uint(Image_ID));
    GL.GL_Begin(GL.QUADS);
    GL.TexCoord(0.0,0.0);
    GL.Vertex(0,size_y);
    GL.TexCoord(1.0,0.0);
    GL.Vertex(size_x,size_y);
    GL.TexCoord(1.0,1.0);
    GL.Vertex(size_x,0);
    GL.TexCoord(0.0,1.0);
    GL.Vertex(0,0);
    GL.GL_End;
    Pop_3D;
  end Put_Image;

end GLUT_2D;
