with GLOBE_3D.Math;

package body GLOBE_3D.Wire_Frame is

  overriding procedure Destroy (o : in out Wired_3D) is
    procedure Dispose is
      new Ada.Unchecked_Deallocation (Segment_Array, p_Segment_Array);
  begin
    --  Destroy the wires:
    Dispose (o.wire);
    --  Call parent method:
    Destroy (Object_3D (o));
  end Destroy;

  overriding procedure Display_One (o : in out Wired_3D) is
  begin
    --  Call parent method:
    Display_One (Object_3D (o));
    --  Display the wires:
    if o.wire /= null then
      GL.PushMatrix;
      GL.Translate (o.centre);
      Math.Multiply_GL_Matrix (o.rotation);
      for i in o.wire'Range loop
        GL.Color (o.wire (i).colour);
        GL.GL_Begin (GL.LINES);
        GL.Vertex (o.point (o.wire (i).P1));
        GL.Vertex (o.point (o.wire (i).P2));
        GL.GL_End;
      end loop;
      GL.PopMatrix;
    end if;
  end Display_One;

end GLOBE_3D.Wire_Frame;
