package GLOBE_3D.Wire_frame is

  type Segment is record
    P1, P2: Positive; -- Indices to o.Point
    colour: GL.RGBA_Color;
  end record;

  type Segment_array is array(Positive range <>) of Segment;
  type p_Segment_array is access Segment_array;

  type Wired_3D is new Object_3D with record
    wire: p_Segment_array:= null;
  end record;

  type p_Wired_3D is access Wired_3D;

  type Wired_3D_array is array(Positive range <>) of p_Wired_3D;
  type p_Wired_3D_array is access Wired_3D_array;

  overriding procedure Display_one(o: in out Wired_3D);

end GLOBE_3D.Wire_frame;
