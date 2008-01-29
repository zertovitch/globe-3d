with GLOBE_3D.Math;

package body GLOBE_3D.Wire_frame is

  procedure Display_one(o: in out Wired_3D) is
  begin
    -- Call parent method:
    Display_one(Object_3D(o));
    -- Display the wires:
    if o.wire /= null then
      GL.PushMatrix;
      GL.Translate( o.centre );
      Math.Multiply_GL_Matrix(o.rotation);
      for i in o.wire'Range loop
        GL.Color(o.wire(i).colour);
        GL.GL_Begin(GL.LINES);
        GL.Vertex(o.point(o.wire(i).P1));
        GL.Vertex(o.point(o.wire(i).P2));
        GL.GL_End;
      end loop;
      GL.PopMatrix;
    end if;
  end Display_one;

end GLOBE_3D.Wire_frame;