--  Wired_3D adds a wire-frame shape to a standard 3D object.

package GLOBE_3D.Wire_Frame is

  type Segment is record
    P1, P2 : Positive;  --  Indices to o.Point
    colour : GL.RGBA_Color;
  end record;

  type Segment_Array is array (Positive range <>) of Segment;
  type p_Segment_Array is access Segment_Array;

  type Wired_3D is new Object_3D with record
    wire : p_Segment_Array := null;
  end record;

  type p_Wired_3D is access Wired_3D;

  type Wired_3D_Array is array (Positive range <>) of p_Wired_3D;
  type p_Wired_3D_Array is access Wired_3D_Array;

  overriding procedure Destroy (o : in out Wired_3D);

  overriding procedure Display_One (o : in out Wired_3D);

end GLOBE_3D.Wire_Frame;
