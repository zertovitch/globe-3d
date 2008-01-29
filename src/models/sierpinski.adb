with GL, GLOBE_3D.Math;

-- with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body Sierpinski is

  -----------------
  -- Create_Cube --
  -----------------

  procedure Create_Cube(
    object       : in out GLOBE_3D.p_Object_3D;
    scale        :        GLOBE_3D.Real;
    centre       :        GLOBE_3D.Point_3D;
    texture      :        Cubic_Face_texture;
    tiled        :        Boolean;
    fractal_level:        Natural
  )
  is
    use GL, GLOBE_3D, GLOBE_3D.Math;

    side: constant Integer:= 3**fractal_level;
    -- We put a layer of empty cubes all around the main one.
    subtype Extended_side_range is Integer range -1..side;
    subtype Side_range is Extended_side_range range 0..side-1;
    subtype Comparison_side_range is Extended_side_range range 0..side;

    filled: array( Extended_side_range,
                   Extended_side_range,
                   Extended_side_range
                 ) of Boolean:=
      (others => (others => (others => False)));

    procedure Fill(x0,y0,z0: Natural; level: Natural) is
      l1: Natural;
      o: Natural;
    begin
      if level=0 then
        filled(x0,y0,z0):= True;
      else
        l1:= level-1;
        o:= 3 ** l1;
        for x in 0..2 loop
          for y in 0..2 loop
            if not (x=1 and y=1) then
              for z in 0..2 loop
                if not ((x=1 and z=1) or (y=1 and z=1)) then
                  Fill(x0+x*o,y0+y*o,z0+z*o,l1);
                end if;
              end loop;
            end if;
          end loop;
        end loop;
      end if;
    end Fill;

    scale_2: constant Real:= scale / Real(side);
    offset : constant Real:= 0.5 * Real(side);

    type Ipoint_3D is array(1..3) of Extended_side_range;

    procedure Trans(i: Ipoint_3D; p: out Point_3D) is
    begin
      p:= scale_2 * (Real(i(1))-offset,Real(i(2))-offset,Real(i(3))-offset);
    end Trans;

    point  : p_Point_3D_array:=
               new Point_3D_array(1..((1+3**fractal_level)**3));
    face   : p_Face_array:=
               new Face_array(1..6*(3**fractal_level)**3);
    face_proto : Face_type; -- takes defaults values

    po, fa: Natural:= 0;

    type Ipoint_2D is array(1..2) of Natural;
    tex_scale: constant Real:= 1.0 / Real(side);

    -- the following is to optimize the search of existing points:
    type Index_stack is array(1..27*6) of Natural;
    touching: array( Extended_side_range,
                     Extended_side_range,
                     Extended_side_range
                 ) of Index_stack:=
      (others => (others => (others => (others=> 0))));

    procedure Register(i,j,k: Integer; idx: Positive) is
    begin
      if i > Extended_side_range'Last or
         j > Extended_side_range'Last or
         k > Extended_side_range'Last then
        return;
      end if;
      for s in Index_stack'Range loop
        if touching(i,j,k)(s) = idx then -- already in stack
          return;
        elsif touching(i,j,k)(s) = 0 then
          touching(i,j,k)(s):= idx;
          return;
        end if;
      end loop;
      raise Program_Error; -- cannot have more points in stack
    end Register;

    procedure Do_Face(
      cube_id,
      iP1,iP2,iP3,iP4: Ipoint_3D;
      it1,it2,it3,it4: Ipoint_2D
    )
   is
      P: array(1..4) of Point_3D;
      vtx: GLOBE_3D.Index_array(1..4);
      idx: Natural;
    begin
      Trans(iP1,P(1));
      Trans(iP2,P(2));
      Trans(iP3,P(3));
      Trans(iP4,P(4));
      for pt in P'Range loop
        vtx(pt):= 0;
        for op in Index_stack'Range loop
          idx:= touching(cube_id(1),cube_id(2),cube_id(3))(op);
          if idx = 0 then
            -- no more point in stack
            exit;
          elsif Almost_zero(Norm2(P(pt)-point(idx))) then
            -- exists already
            vtx(pt):= idx;
            exit;
          end if;
        end loop;
        if idx = 0 then
          -- create new point
          po:= po + 1;
          point(po):= P(pt);
          vtx(pt):= po;
          -- add point index to cube's stack
          for i in -1..1 loop
            for j in -1..1 loop
              for k in -1..1 loop
                Register(cube_id(1)+i, cube_id(2)+j, cube_id(3)+k, po);
              end loop;
            end loop;
          end loop;
        end if;
      end loop;
      face_proto.P:= vtx;
      if not tiled then
        face_proto.texture_edge_map:=
          ((tex_scale * Real(it1(1)), tex_scale * Real(it1(2))),
           (tex_scale * Real(it2(1)), tex_scale * Real(it2(2))),
           (tex_scale * Real(it3(1)), tex_scale * Real(it3(2))),
           (tex_scale * Real(it4(1)), tex_scale * Real(it4(2))));
      end if;
      fa:= fa+1;
      face(fa):= face_proto;
    end Do_Face;

  generic
    with function Pm(p: Ipoint_3D) return Ipoint_3D; -- permutation
    front, back: Cubic_Face_count;
  procedure Pave;
  procedure Pave is
  begin
    for i in Side_range loop
      for j in Side_range loop
        for k in Comparison_side_range loop
          -- filled components don't need to be permuted (symmetric)
          if filled(i,j,k) = not filled(i,j,k-1) then
            -- There is a face to display
            if filled(i,j,k) then
              -- * full-empty
              face_proto.texture:= texture(front);
              Do_Face(
                Pm((i,j,k)),
                Pm((i,j,k)),Pm((i,j+1,k)),Pm((i+1,j+1,k)),Pm((i+1,j,k)),
                (i,j),(i,j+1),(i+1,j+1),(i+1,j)
              );
            else
              -- * empty-full
              face_proto.texture:= texture(back);
              Do_Face(
                Pm((i,j,k)),
                Pm((i+1,j,k)),Pm((i+1,j+1,k)),Pm((i,j+1,k)),Pm((i,j,k)),
                (i+1,j),(i+1,j+1),(i,j+1),(i,j)
              );
            end if;
          end if;
        end loop;
      end loop;
    end loop;
  end Pave;

  function Id(p: Ipoint_3D) return Ipoint_3D is
  begin
    return p;
  end Id;

  function Pm1(p: Ipoint_3D) return Ipoint_3D is
  begin
    return (p(3),p(1),p(2));
  end Pm1;

  function Pm2(p: Ipoint_3D) return Ipoint_3D is
  begin
    return (p(2),p(3),p(1));
  end Pm2;

  procedure Pave_z is new Pave(Id,  5,6);
  procedure Pave_y is new Pave(Pm1, 1,3);
  procedure Pave_x is new Pave(Pm2, 2,4);

  begin
    Fill(0,0,0, fractal_level);
    --  -- Test: display 1st layer...
    --  for x in filled'Range(1) loop
    --    for y in filled'Range(2) loop
    --      if filled(x,y,0) then
    --        Put('x');
    --      else
    --        Put(' ');
    --      end if;
    --    end loop;
    --    New_Line;
    --  end loop;

    face_proto.skin:= material_texture;
    face_proto.whole_texture:= tiled;
    face_proto.repeat_U:= 1;
    face_proto.repeat_V:= 1;

    Pave_x;
    Pave_y;
    Pave_z;

    object:= new Object_3D(po,fa);
    object.point:= point(1..po);
    object.face := face(1..fa);

    object.centre:= centre;
    Set_name(object.all,
        "Sierpinski cube-sponge, fractal level" &
        Integer'Image(fractal_level)
    );
    Dispose(point);
    Dispose(face);
  end Create_Cube;

end Sierpinski;
