with GL;

with GLOBE_3D.Random_Extrusions,
     GL.Math;

with Ada.Numerics;                      use Ada.Numerics;

package body Extruded_Surface is

  procedure Create
    (object     : in out GLOBE_3D.p_Object_3D;
     scale      :        GLOBE_3D.Real;
     centre     :        GLOBE_3D.Point_3D;
     grid       : in     Positive;
     surface    : in     Kind_of_surface;
     max_u3     : in     GLOBE_3D.Real;
     iterations : in     Natural;
     hor_tex,
     ver_tex    : in     GLOBE_3D.Image_ID;
     tiling_hu,
     tiling_hv,
     tiling_vu,
     tiling_vv  : in     Positive)
  is
    use GLOBE_3D;

  procedure Geo (u : in Point_3D; x : out Point_3D) is
    use GL.Math, GLOBE_3D.REF, GL;
    phi, theta, sin_phi : Real;
  begin
    case surface is
      when square =>
        x := scale * u;
      when sphere =>
        theta := 2.0 * Pi * u (0);
        phi := Pi * (1.0 - u (1));
        sin_phi := Sin (phi);
        x := scale * (1.0 + u (2)) * (sin_phi * Cos (theta), sin_phi * Sin (theta), Cos (phi));
    end case;
  end Geo;

  package Gex is new GLOBE_3D.Random_extrusions (Geo);

  last_point      : Natural;
  mesh            : p_Point_3D_Array :=
                      new Point_3D_Array (1 .. ((grid + 1)**2) * 3);
  poly            : p_Face_Array :=
                      new Face_Array (1 .. ((grid + 1)**2) * 6 * 2);
  --  Worst case: each cell has a S and W neighbour of
  --  different height; triangular tiling
  last_face       : Natural;
  random_initiator : constant := 0;  --  0 -> time-dependent seed
  thu : constant Real := Real (tiling_hu);
  thv : constant Real := Real (tiling_hv);
  tvu : constant Real := Real (tiling_vu);
  tvv : constant Real := Real (tiling_vv);

  begin
    Gex.Extrude_on_Rectangle
       --  (0.039,0.396), (0.672,0.119), (0.959,0.35), (0.333,0.926),
       --  -- ^ Testing a non-parallel texture-edge mapping
       --  --   (quadrilatère quelconque - quad_qcq.bmp)
      ((0.0, 0.0), (thu, 0.0), (thu, thv), (0.0, thv),
       (0.0, 0.0), (tvu, 0.0), (tvu, tvv), (0.0, tvv),
       grid, grid,
       hor_tex, ver_tex,
       max_u3, -- since Random is uniform, expected height 2x that value
       iterations,
       last_point,
       mesh.all,
       last_face,
       poly.all,
       random_initiator);

    object := new Object_3D (Max_points => last_point, Max_faces => last_face);
    object.point := mesh (1 .. last_point);
    object.face  := poly (1 .. last_face);
    object.centre := centre;
    Dispose (mesh);
    Dispose (poly);
  end Create;

end Extruded_Surface;
