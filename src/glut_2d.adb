with GLUT, System;

package body GLUT_2D is

  GLUT_char : constant array (Font_Type) of System.Address :=
   (Screen_9_by_15 => GLUT.BITMAP_9_BY_15,
    Screen_8_by_13 => GLUT.BITMAP_8_BY_13,
    Times_Roman_10 => GLUT.BITMAP_TIMES_ROMAN_10,
    Times_Roman_24 => GLUT.BITMAP_TIMES_ROMAN_24,
    Helvetica_10   => GLUT.BITMAP_HELVETICA_10,
    Helvetica_12   => GLUT.BITMAP_HELVETICA_12,
    Helvetica_18   => GLUT.BITMAP_HELVETICA_18);

  procedure Text_Output
    (s    : String;
     font : Font_Type)
  is
  begin
    for i in s'Range loop
      GLUT.BitmapCharacter (GLUT_char (font), Character'Pos (s (i)));
    end loop;
  end Text_Output;

  procedure Push_3D_set_2D (main_size_x, main_size_y : GL.Sizei) is
    use GL;
  begin
    --  Push current matrix mode and viewport attributes.
    GL.PushAttrib (GL.TRANSFORM_BIT or GL.VIEWPORT_BIT);
    GL.MatrixMode (GL.PROJECTION);
    GL.PushMatrix;  --  In GL.PROJECTION mode, the stack depth is at least 2
    GL.LoadIdentity;
    GL.Ortho
      (0.0, GL.Double (main_size_x), GL.Double (main_size_y), 0.0,
       GL.Double'(-1.0), 1.0);
    GL.MatrixMode (GL.MODELVIEW);
    GL.PushMatrix;  --  In GL.MODELVIEW mode, the stack depth is at least 32
    GL.LoadIdentity;
  end Push_3D_set_2D;

  procedure Pop_3D is
  begin
    GL.MatrixMode (GL.MODELVIEW);
    GL.PopMatrix;
    GL.MatrixMode (GL.PROJECTION);
    GL.PopMatrix;
    GL.PopAttrib;
  end Pop_3D;

  procedure Text_Output
    (x, y        : GL.Int;
     main_size_x,
     main_size_y : GL.Sizei;
     s           : String;
     font        : Font_Type
  )
  is
  begin
    Push_3D_set_2D (main_size_x, main_size_y);
    GL.RasterPos (x, y);
    Text_Output (s, font);
    Pop_3D;
  end Text_Output;

  procedure Text_Output
    (p    : GL.Double_Vector_3D;
     s    : String;
     font : Font_Type)
  is
  begin
    GL.PushMatrix;
    GL.Translate (p);
    GL.RasterPos (0, 0);
    Text_Output (s, font);
    GL.PopMatrix;
  end Text_Output;

  procedure Put_Image
    (Image_ID    : Integer;
     x, y        : GL.Int;
     size_x,
     size_y      : GL.Int;
     main_size_x,
     main_size_y : GL.Sizei
  )
  is
  begin
    --  fx:= GL.Float(size_x) / GL.Float(main_size_x);
    --  fy:= GL.Float(size_y) / GL.Float(main_size_y);
    Push_3D_set_2D (main_size_x, main_size_y);
    GL.Translate (GL.Double (x), GL.Double (y), 0.0);
    --  GL.Enable( GL.TEXTURE_2D );
    GL.BindTexture (GL.Texture_2D, GL.Uint (Image_ID));
    GL.GL_Begin (GL.QUADS);
    GL.TexCoord (0.0, 0.0);
    GL.Vertex (0, size_y);
    GL.TexCoord (1.0, 0.0);
    GL.Vertex (size_x, size_y);
    GL.TexCoord (1.0, 1.0);
    GL.Vertex (size_x, 0);
    GL.TexCoord (0.0, 1.0);
    GL.Vertex (0, 0);
    GL.GL_End;
    Pop_3D;
  end Put_Image;

end GLUT_2D;
