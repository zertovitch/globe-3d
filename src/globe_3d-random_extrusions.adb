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


with GLOBE_3D.Math;

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

package body GLOBE_3D.Random_extrusions is

  seed: Generator;

  procedure Extrude_on_rectangle(
    T1,T2,T3,T4     :  in Map_idx_pair;  -- Texture edges, horizontal surface
    V1,V2,V3,V4     :  in Map_idx_pair;  -- Texture edges, vertical surfaces
    grid_1,grid_2   :  in Positive;
    T_ID, V_ID      :  in Image_ID;      -- ID's of plane and vertical texture
    max_u3          :  in Real;
    iterations      :  in Natural;
    last_point      : out Natural;
    mesh            : out Point_3D_array;
    last_face       : out Natural;
    poly            : out Face_array;
    random_initiator:  in Integer:= 0    -- default 0 -> time-dependent seed
  )
  is
    use GL, GLOBE_3D.Math;
    po, fa: Natural:= 0;
    face_proto : Face_type; -- takes defaults values

    -- grid: 0 |---|---|...|---|---| n+2; we don't touch the main face's border
    -- cell:     0   1       n  n+1
    elevation: array(0..grid_1+1, 0..grid_2+1) of Real:=
      (others => (others=> 0.0));  -- elevation of the middle of a cell
    -- Temporary data used to search faster existing points:
    type Point_stack is array(1..36) of Positive; -- 16 should be sufficient...
    -- without redundancies: 16; with redundancies: 36
    point_touching: array(elevation'Range(1),elevation'Range(2)) of Point_stack;
    total_points_touching: array(elevation'Range(1),elevation'Range(2)) of Natural:= (others => (others=> 0));

    procedure Register(e1n,e2n: Integer; P_idx: Positive) is
      e1,e2: Integer;
      t: Natural;
    begin
      e1:= e1n mod (grid_1+2);
      e2:= e2n mod (grid_2+2);
      t:= total_points_touching(e1,e2);
      for i in reverse 1..t loop
        if point_touching(e1,e2)(i)= P_idx then -- already in stack
          return;
        end if;
      end loop;
      total_points_touching(e1,e2):= t+1;
      -- if t+1 > Point_Stack'Last then raise Constraint_Error; end if;
      point_touching(e1,e2)(t+1):= P_idx;
    end Register;

    procedure Do_Face(
      P1,P2,P3,P4 : Point_3D;
      tex         : Map_idx_pair_4_array;
      tex_ID      : Image_ID;
      cell1,cell2 : Natural
    )
    is
      P: array(1..4) of Point_3D;
      vtx: GLOBE_3D.Index_array(1..4);
      pt_idx: Natural;
      found: Boolean;
      degen: Natural:= 0;
      last_degen_vtx: Positive;
      procedure Register_proto is
      begin
        fa:= fa+1;
        if fa > poly'Last then raise Constraint_Error; end if;
        -- ^ useful if we disable range checks...
        poly(fa):= face_proto;
      end REgister_proto;
    begin
      Geometric_mapping(P1,P(1));
      Geometric_mapping(P2,P(2));
      Geometric_mapping(P3,P(3));
      Geometric_mapping(P4,P(4));
      for pt in P'Range loop
        found:= False;
        -- Look in the stack of registered points:
        for op in reverse 1..total_points_touching(cell1,cell2) loop
          pt_idx:= point_touching(cell1,cell2)(op);
          if Almost_zero(Norm2(P(pt)-mesh(pt_idx))) then -- exists already
            vtx(pt):= pt_idx;
            found:= True;
          end if;
        end loop;
        if not found then -- add a point when non existing
          po:= po + 1;
          if po > mesh'Last then raise Constraint_Error; end if;
          -- ^ useful if we disable range checks...
          mesh(po):= P(pt);
          vtx(pt):= po;
          for i in -1..1 loop
            for j in -1..1 loop
              Register(cell1+i,cell2+j,po);
            end loop;
          end loop;
        end if;
      end loop;
      face_proto.texture_edge_map:= tex;
      face_proto.texture:= tex_ID;
      -- Check degenerate faces
      for i in 1..4 loop
        for j in i+1..4 loop
          if vtx(i)=vtx(j) then
            degen:= degen + 1;
            last_degen_vtx:= j;
          end if;
        end loop;
      end loop;
      case degen is
        when 0 => -- quadrilatere
          -- !! check if flat, otherwise make 2 triangles!
          face_proto.P:= vtx;
          Register_proto;
        when 1 => -- triangle
          vtx(last_degen_vtx):= 0;
          face_proto.P:= vtx;
          Register_proto;
        when others =>
          return;
      end case;
    end Do_Face;

    e: Real:= 0.0;
    sc_1: constant Real:= 1.0 / Real(grid_1+2);
    sc_2: constant Real:= 1.0 / Real(grid_2+2);
    p_1, p_2, l_1, l_2: Positive;
    xa,xb,ya,yb, en: Real;
    width_factor: Float;
    ta,tb: Map_idx_pair;

  begin
    face_proto.skin:= coloured_texture;
    face_proto.colour:= (0.5,0.5,0.5);
    face_proto.whole_texture:= False;
    if random_initiator /= 0 then
      Reset(seed, random_initiator);
    end if;
    -- Generate elevation map by covering it with rectangle layers
    for i in reverse 1..iterations loop
      p_1:= 1 + Integer(Float(grid_1-2)*Random(seed)+0.5);
      p_2:= 1 + Integer(Float(grid_2-2)*Random(seed)+0.5);
      width_factor:= Float(i)/Float(iterations);
      -- ^ cover with decreasing widths
      l_1:= Integer(Float(grid_1-p_1-1)*Random(seed)*width_factor+0.5);
      l_2:= Integer(Float(grid_2-p_2-1)*Random(seed)*width_factor+0.5);
      -- e:= e + Real(Random(seed))*max_u3/Real(iterations);
      -- ^ converges to a square of height max_u3 :-(
      e:= Real(Random(seed))*max_u3;
      for r_1 in reverse 0..l_1 loop
        for r_2 in reverse 0..l_2 loop
          elevation(p_1+r_1,p_2+r_2):= e;
        end loop;
      end loop;
    end loop;
    -- Create the mesh
    for e1 in reverse elevation'Range(1) loop
      for e2 in reverse elevation'Range(2) loop
        e:= elevation(e1,e2);
        xa:= Real(e1)*sc_1;
        xb:= Real(e1+1)*sc_1;
        ya:= Real(e2)*sc_2;
        yb:= Real(e2+1)*sc_2;
        ta.u:= T1.u + xa * (T2.u-T1.u) + ya * (xa * (T3.u-T2.u) + (1.0-xa) * (T4.u-T1.u));
        ta.v:= T1.v + xa * (T2.v-T1.v) + ya * (xa * (T3.v-T2.v) + (1.0-xa) * (T4.v-T1.v));
        tb.u:= T1.u + xb * (T2.u-T1.u) + yb * (xb * (T3.u-T2.u) + (1.0-xb) * (T4.u-T1.u));
        tb.v:= T1.v + xb * (T2.v-T1.v) + yb * (xb * (T3.v-T2.v) + (1.0-xb) * (T4.v-T1.v));
        -- The horizontal face
        Do_Face(
          (xa,ya,e), (xb,ya,e), (xb,yb,e), (xa,yb,e),
          (ta,(tb.u,ta.v),tb,(ta.u,tb.v)),
          T_ID,
          e1,e2
        );
        --
        -- Now the funny part: the vertical faces!
        --
        if iterations > 0 and -- <- possible to generate no extrusion at all!
           e1 > 0 and e2 > 0 then
          --
          --    seen from above:    _|_|_ yb
          -- -> southern neighbour  _|_|_ ya
          --                         |^|
          --
          en:= elevation(e1,e2-1);
          if Almost_zero(e-en) then
            null; -- do nothing, there is no face to add
          else
            if e > en then -- neighbour has a lower elevation: face visible from south
              ta.u:= V1.u + xa * (V2.u-V1.u) + en * (xa * (V3.u-V2.u) + (1.0-xa) * (V4.u-V1.u));
              ta.v:= V1.v + xa * (V2.v-V1.v) + en * (xa * (V3.v-V2.v) + (1.0-xa) * (V4.v-V1.v));
              tb.u:= V1.u + xb * (V2.u-V1.u) + e  * (xb * (V3.u-V2.u) + (1.0-xb) * (V4.u-V1.u));
              tb.v:= V1.v + xb * (V2.v-V1.v) + e  * (xb * (V3.v-V2.v) + (1.0-xb) * (V4.v-V1.v));
              Do_Face(
                (xa,ya,en), (xb,ya,en), (xb,ya,e), (xa,ya,e),
                (ta,(tb.u,ta.v),tb,(ta.u,tb.v)),
                V_ID,
                e1,e2
              );
            else           -- neighbour has a higher elevation: face visible from north
              ta.u:= V2.u + xb * (V1.u-V2.u) + e  * (xb * (V4.u-V1.u) + (1.0-xb) * (V3.u-V2.u));
              ta.v:= V2.v + xb * (V1.v-V2.v) + e  * (xb * (V4.v-V1.v) + (1.0-xb) * (V3.v-V2.v));
              tb.u:= V2.u + xa * (V1.u-V2.u) + en * (xa * (V4.u-V1.u) + (1.0-xa) * (V3.u-V2.u));
              tb.v:= V2.v + xa * (V1.v-V2.v) + en * (xa * (V4.v-V1.v) + (1.0-xa) * (V3.v-V2.v));
              Do_Face(
                (xb,ya,e), (xa,ya,e), (xa,ya,en), (xb,ya,en),
                (ta,(tb.u,ta.v),tb,(ta.u,tb.v)),
                V_ID,
                e1,e2
              );
            end if;
          end if;
          --
          --    seen from above:    _|_|_
          -- -> western neighbour  >_|_|_
          --                         | |
          --
          en:= elevation(e1-1,e2);
          if Almost_zero(e-en) then
            null; -- do nothing, there is no face to add
          else
            if e > en then -- neighbour has a lower elevation: face visible from west
              ta.u:= V2.u + yb * (V1.u-V2.u) + en * (yb * (V4.u-V1.u) + (1.0-yb) * (V3.u-V2.u));
              ta.v:= V2.v + yb * (V1.v-V2.v) + en * (yb * (V4.v-V1.v) + (1.0-yb) * (V3.v-V2.v));
              tb.u:= V2.u + ya * (V1.u-V2.u) + e  * (ya * (V4.u-V1.u) + (1.0-ya) * (V3.u-V2.u));
              tb.v:= V2.v + ya * (V1.v-V2.v) + e  * (ya * (V4.v-V1.v) + (1.0-ya) * (V3.v-V2.v));
              Do_Face(
                (xa,yb,en), (xa,ya,en), (xa,ya,e), (xa,yb,e),
                (ta,(tb.u,ta.v),tb,(ta.u,tb.v)),
                V_ID,
                e1,e2
              );
            else           -- neighbour has a higher elevation: face visible from east
              ta.u:= V1.u + ya * (V2.u-V1.u) + e  * (ya * (V3.u-V2.u) + (1.0-ya) * (V4.u-V1.u));
              ta.v:= V1.v + ya * (V2.v-V1.v) + e  * (ya * (V3.v-V2.v) + (1.0-ya) * (V4.v-V1.v));
              tb.u:= V1.u + yb * (V2.u-V1.u) + en * (yb * (V3.u-V2.u) + (1.0-yb) * (V4.u-V1.u));
              tb.v:= V1.v + yb * (V2.v-V1.v) + en * (yb * (V3.v-V2.v) + (1.0-yb) * (V4.v-V1.v));
              Do_Face(
                (xa,ya,e), (xa,yb,e), (xa,yb,en), (xa,ya,en),
                (ta,(tb.u,ta.v),tb,(ta.u,tb.v)),
                V_ID,
                e1,e2
              );
            end if;
          end if;
          --
          -- -> eastern and northern neighbours: treated on next step
          --    as western and southern cases
        end if;
      end loop;
    end loop;
    last_point:= po;
    last_face := fa;
  end Extrude_on_rectangle;

begin
  Reset(seed);
end GLOBE_3D.Random_extrusions;
