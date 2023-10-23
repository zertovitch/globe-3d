--  Algorithm to generate a Sci-Fi-style extruded surface
--  Copyright (c) Gautier de Montmollin 2006
--  CH-8810 Horgen
--  SWITZERLAND
--  Permission granted to use the herein contained algorithm for any purpose,
--  provided this copyright note remains attached and unmodified.
--
--  Change log:
--  xx-May-2006: - !! split a quad into triangles if not flat
--               - !! identify / remove degenerate faces, e.g. on a sphere
--                    (triangle ok, less -> erase!)
--  26-May-2006: optimized search for duplicate points
--  24-May-2006: added explicit bound checks
--  14-May-2006: created

with GL.Math;

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

package body GLOBE_3D.Random_Extrusions is

  seed : Generator;

  procedure Extrude_on_Rectangle
    (HT1, HT2, HT3, HT4 : in     Map_Idx_Pair;   --  Texture edges, horizontal surface
     VT1, VT2, VT3, VT4 : in     Map_Idx_Pair;   --  Texture edges, vertical surfaces
     grid_1, grid_2     : in     Positive;
     T_ID, V_ID         : in     Image_ID;       --  ID's of plane and vertical texture
     max_u3             : in     Real;
     iterations         : in     Natural;
     last_point         :    out Natural;
     mesh               :    out Point_3D_Array;
     last_face          :    out Natural;
     poly               :    out Face_Array;
     random_initiator   : in     Integer := 0)   --  default 0 -> time-dependent seed
  is
    use GL, GL.Math;
    po, fa : Natural := 0;
    face_proto : Face_Type;  --  takes defaults values

    --  grid: 0 |---|---|...|---|---| n+2; we don't touch the main face's border
    --  cell:     0   1       n  n+1
    elevation : array (0 .. grid_1 + 1, 0 .. grid_2 + 1) of Real :=
      (others => (others => 0.0));  --  Elevation of the middle of a cell
    --  Temporary data used to search faster existing points:
    type Point_Stack is array (1 .. 36) of Positive;  --  16 should be sufficient...
    --  without redundancies: 16; with redundancies: 36
    point_touching : array (elevation'Range (1), elevation'Range (2)) of Point_Stack;
    total_points_touching :
      array (elevation'Range (1), elevation'Range (2)) of Natural := (others => (others => 0));

    procedure Register (e1n, e2n : Integer; P_idx : Positive) is
      e1, e2 : Integer;
      t : Natural;
    begin
      e1 := e1n mod (grid_1 + 2);
      e2 := e2n mod (grid_2 + 2);
      t := total_points_touching (e1, e2);
      for i in reverse 1 .. t loop
        if point_touching (e1, e2)(i) = P_idx then -- already in stack
          return;
        end if;
      end loop;
      total_points_touching (e1, e2) := t + 1;
      --  if t+1 > Point_Stack'Last then raise Constraint_Error; end if;
      point_touching (e1, e2)(t + 1) := P_idx;
    end Register;

    procedure Do_Face
      (P1, P2, P3, P4 : Point_3D;
       tex            : Map_Idx_Pair_4_Array;
       tex_ID         : Image_ID;
       cell1, cell2   : Natural)
    is
      P : array (1 .. 4) of Point_3D;
      vtx : GLOBE_3D.Index_Array (1 .. 4);
      pt_idx : Natural;
      found : Boolean;
      degen : Natural := 0;
      last_degen_vtx : Positive;

      procedure Register_Proto is
      begin
        fa := fa + 1;
        if fa > poly'Last then raise Constraint_Error; end if;
        --  ^ useful if someone has disabled range checks...
        poly (fa) := face_proto;
      end Register_Proto;

    begin
      Geometric_Mapping (P1, P (1));
      Geometric_Mapping (P2, P (2));
      Geometric_Mapping (P3, P (3));
      Geometric_Mapping (P4, P (4));
      for pt in P'Range loop
        found := False;
        --  Look in the stack of registered points:
        for op in reverse 1 .. total_points_touching (cell1, cell2) loop
          pt_idx := point_touching (cell1, cell2)(op);
          if Almost_zero (Norm2 (P (pt) - mesh (pt_idx))) then  --  exists already
            vtx (pt) := pt_idx;
            found := True;
          end if;
        end loop;
        if not found then  --  Add a vertex when non existing
          po := po + 1;
          if po > mesh'Last then raise Constraint_Error; end if;
          --  ^ useful if we disable range checks...
          mesh (po) := P (pt);
          vtx (pt) := po;
          for i in -1 .. 1 loop
            for j in -1 .. 1 loop
              Register (cell1 + i, cell2 + j, po);
            end loop;
          end loop;
        end if;
      end loop;
      face_proto.texture_edge_map := tex;
      face_proto.texture := tex_ID;
      --  Check degenerate faces
      for i in 1 .. 4 loop
        for j in i + 1 .. 4 loop
          if vtx (i) = vtx (j) then
            degen := degen + 1;
            last_degen_vtx := j;
          end if;
        end loop;
      end loop;
      case degen is
        when 0 =>       --  Quadrilateral
          --  !! check if flat, otherwise make 2 triangles!
          face_proto.P := vtx;
          Register_Proto;
        when 1 =>       --  Triangle
          vtx (last_degen_vtx) := 0;
          face_proto.P := vtx;
          Register_Proto;
        when others =>  --  Less than 3 distinct vertices -> no face at all !
          return;
      end case;
    end Do_Face;

    e : Real := 0.0;
    sc_1 : constant Real := 1.0 / Real (grid_1 + 2);
    sc_2 : constant Real := 1.0 / Real (grid_2 + 2);
    p_1, p_2, l_1, l_2 : Positive;
    xa, xb, ya, yb, en : Real;
    width_factor : Float;
    ta, tb : Map_Idx_Pair;

  begin
    face_proto.skin := coloured_texture;
    face_proto.colour := (0.5, 0.5, 0.5);
    face_proto.whole_texture := False;
    if random_initiator /= 0 then
      Reset (seed, random_initiator);
    end if;
    --  Generate elevation map by covering it with rectangle layers
    for i in reverse 1 .. iterations loop
      p_1 := 1 + Integer (Float (grid_1 - 2) * Random (seed) + 0.5);
      p_2 := 1 + Integer (Float (grid_2 - 2) * Random (seed) + 0.5);
      width_factor := Float (i) / Float (iterations);
      --  ^ cover with decreasing widths
      l_1 := Integer (Float (grid_1 - p_1 - 1) * Random (seed) * width_factor + 0.5);
      l_2 := Integer (Float (grid_2 - p_2 - 1) * Random (seed) * width_factor + 0.5);
      --  e:= e + Real(Random(seed))*max_u3/Real(iterations);
      --  ^ converges to a square of height max_u3 :-(
      e := Real (Random (seed)) * max_u3;
      for r_1 in reverse 0 .. l_1 loop
        for r_2 in reverse 0 .. l_2 loop
          elevation (p_1 + r_1, p_2 + r_2) := e;
        end loop;
      end loop;
    end loop;
    --  Create the mesh
    for e1 in reverse elevation'Range (1) loop
      for e2 in reverse elevation'Range (2) loop
        e := elevation (e1, e2);
        xa := Real (e1) * sc_1;
        xb := Real (e1 + 1) * sc_1;
        ya := Real (e2) * sc_2;
        yb := Real (e2 + 1) * sc_2;
        ta.U := HT1.U + xa * (HT2.U - HT1.U) + ya * (xa * (HT3.U - HT2.U) + (1.0 - xa) * (HT4.U - HT1.U));
        ta.V := HT1.V + xa * (HT2.V - HT1.V) + ya * (xa * (HT3.V - HT2.V) + (1.0 - xa) * (HT4.V - HT1.V));
        tb.U := HT1.U + xb * (HT2.U - HT1.U) + yb * (xb * (HT3.U - HT2.U) + (1.0 - xb) * (HT4.U - HT1.U));
        tb.V := HT1.V + xb * (HT2.V - HT1.V) + yb * (xb * (HT3.V - HT2.V) + (1.0 - xb) * (HT4.V - HT1.V));
        --  The horizontal face
        Do_Face
          ((xa, ya, e), (xb, ya, e), (xb, yb, e), (xa, yb, e),
           (ta, (tb.U, ta.V), tb, (ta.U, tb.V)),
           T_ID,
           e1, e2);
        --
        --  Now the funny part: the vertical faces!
        --
        if iterations > 0 and  -- <- possible to generate no extrusion at all!
           e1 > 0 and e2 > 0
        then
          --
          --     seen from above:    _|_|_ yb
          --  -> southern neighbour  _|_|_ ya
          --                          |^|
          --
          en := elevation (e1, e2 - 1);
          if Almost_zero (e - en) then
            null; -- do nothing, there is no face to add
          else
            if e > en then  --  neighbour has a lower elevation: face visible from south
              ta.U := VT1.U + xa * (VT2.U - VT1.U) + en * (xa * (VT3.U - VT2.U) + (1.0 - xa) * (VT4.U - VT1.U));
              ta.V := VT1.V + xa * (VT2.V - VT1.V) + en * (xa * (VT3.V - VT2.V) + (1.0 - xa) * (VT4.V - VT1.V));
              tb.U := VT1.U + xb * (VT2.U - VT1.U) + e  * (xb * (VT3.U - VT2.U) + (1.0 - xb) * (VT4.U - VT1.U));
              tb.V := VT1.V + xb * (VT2.V - VT1.V) + e  * (xb * (VT3.V - VT2.V) + (1.0 - xb) * (VT4.V - VT1.V));
              Do_Face
                ((xa, ya, en), (xb, ya, en), (xb, ya, e), (xa, ya, e),
                 (ta, (tb.U, ta.V), tb, (ta.U, tb.V)),
                 V_ID,
                 e1, e2);
            else            --  neighbour has a higher elevation: face visible from north
              ta.U := VT2.U + xb * (VT1.U - VT2.U) + e  * (xb * (VT4.U - VT1.U) + (1.0 - xb) * (VT3.U - VT2.U));
              ta.V := VT2.V + xb * (VT1.V - VT2.V) + e  * (xb * (VT4.V - VT1.V) + (1.0 - xb) * (VT3.V - VT2.V));
              tb.U := VT2.U + xa * (VT1.U - VT2.U) + en * (xa * (VT4.U - VT1.U) + (1.0 - xa) * (VT3.U - VT2.U));
              tb.V := VT2.V + xa * (VT1.V - VT2.V) + en * (xa * (VT4.V - VT1.V) + (1.0 - xa) * (VT3.V - VT2.V));
              Do_Face
                ((xb, ya, e), (xa, ya, e), (xa, ya, en), (xb, ya, en),
                 (ta, (tb.U, ta.V), tb, (ta.U, tb.V)),
                 V_ID,
                 e1, e2);
            end if;
          end if;
          --
          --     seen from above:    _|_|_
          --  -> western neighbour  >_|_|_
          --                          | |
          --
          en := elevation (e1 - 1, e2);
          if Almost_zero (e - en) then
            null; -- do nothing, there is no face to add
          else
            if e > en then  --  neighbour has a lower elevation: face visible from west
              ta.U := VT2.U + yb * (VT1.U - VT2.U) + en * (yb * (VT4.U - VT1.U) + (1.0 - yb) * (VT3.U - VT2.U));
              ta.V := VT2.V + yb * (VT1.V - VT2.V) + en * (yb * (VT4.V - VT1.V) + (1.0 - yb) * (VT3.V - VT2.V));
              tb.U := VT2.U + ya * (VT1.U - VT2.U) + e  * (ya * (VT4.U - VT1.U) + (1.0 - ya) * (VT3.U - VT2.U));
              tb.V := VT2.V + ya * (VT1.V - VT2.V) + e  * (ya * (VT4.V - VT1.V) + (1.0 - ya) * (VT3.V - VT2.V));
              Do_Face
                ((xa, yb, en), (xa, ya, en), (xa, ya, e), (xa, yb, e),
                 (ta, (tb.U, ta.V), tb, (ta.U, tb.V)),
                 V_ID,
                 e1, e2);
            else            --  neighbour has a higher elevation: face visible from east
              ta.U := VT1.U + ya * (VT2.U - VT1.U) + e  * (ya * (VT3.U - VT2.U) + (1.0 - ya) * (VT4.U - VT1.U));
              ta.V := VT1.V + ya * (VT2.V - VT1.V) + e  * (ya * (VT3.V - VT2.V) + (1.0 - ya) * (VT4.V - VT1.V));
              tb.U := VT1.U + yb * (VT2.U - VT1.U) + en * (yb * (VT3.U - VT2.U) + (1.0 - yb) * (VT4.U - VT1.U));
              tb.V := VT1.V + yb * (VT2.V - VT1.V) + en * (yb * (VT3.V - VT2.V) + (1.0 - yb) * (VT4.V - VT1.V));
              Do_Face
                ((xa, ya, e), (xa, yb, e), (xa, yb, en), (xa, ya, en),
                 (ta, (tb.U, ta.V), tb, (ta.U, tb.V)),
                 V_ID,
                 e1, e2);
            end if;
          end if;
          --
          --  Eastern and northern neighbours: treated on next step
          --  as western and southern cases.
        end if;
      end loop;
    end loop;
    last_point := po;
    last_face  := fa;
  end Extrude_on_Rectangle;

begin
  Reset (seed);
end GLOBE_3D.Random_Extrusions;
