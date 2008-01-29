-- Some 2D GL utility functions based on GL, GLUT

with GL;

package GLUT_2D is

  ----------
  -- Text --
  ----------

  type Font_type is(
    Screen_9_by_15,
    Screen_8_by_13,
    Times_Roman_10,
    Times_Roman_24,
    Helvetica_10,
    Helvetica_12,
    Helvetica_18
  );

  procedure Text_output(
    s    : String;
    font : Font_type
  );

  -- Text output from 2D, screen coordinates
  procedure Text_output(
    x,y         : GL.Int;
    main_size_x,
    main_size_y : GL.SizeI;
    s           : String;
    font        : Font_type
  );

  -- Text output from 3D coordinates
  procedure Text_output(
    p    : GL.Double_vector_3D;
    s    : String;
    font : Font_type
  );

  -----------
  -- Image --
  -----------

  procedure Put_Image(
    Image_ID    : Integer;
    x,y         : GL.Int;
    size_x,
    size_y      : GL.Int;
    main_size_x,
    main_size_y : GL.SizeI
  );

end GLUT_2D;