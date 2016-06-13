--  3D model of a "brick" of a space station, seen from inside.
--  Copyright (c) Gautier de Montmollin 2005
--  CH-8810 Horgen
--  SWITZERLAND
--  Permission granted to use the herein contained 3D model for any purpose,
--  provided this copyright note remains attached and unmodified.

with GL, GL.Math;

package body Brick is

  procedure Create(
    object : in out GLOBE_3D.p_Object_3D;
    scale  :        GLOBE_3D.Real;
    centre :        GLOBE_3D.Point_3D;
    kind   :        Brick_kind;
    opening:        Cubic_Face_set;    -- needed opening on which face
    portal :    out Cubic_Face_index;  -- corresponding connecting faces
    texture:        Cubic_Face_texture
  )
  is
    use GL, GL.Math, GLOBE_3D;

    type Ipoint is array(1..3) of Natural;

    scale_2: constant Real:= scale / 3.0;

    procedure Trans(i: Ipoint; p: out Point_3D) is
    begin
      p:= scale_2 * (Real(i(1))-1.5,Real(i(2))-1.5,Real(i(3))-1.5);
    end Trans;

    point  : Point_3D_array(1..6*16);
    face   : Face_array(1..6*9);
    face_proto : Face_type; -- takes defaults values

    po, fa: Natural:= 0;

    ----------------
    -- Kind: cube --
    ----------------

    procedure Cube is

      procedure Do_Face( iP1,iP2,iP3,iP4: Ipoint) is
        P: array(1..4) of Point_3D;
        vtx: GLOBE_3D.Index_array(1..4);
      begin
        Trans(iP1,P(1));
        Trans(iP2,P(2));
        Trans(iP3,P(3));
        Trans(iP4,P(4));
        for pt in P'Range loop
          vtx(pt):= 0;
          for op in 1..po loop
            if Almost_zero(Norm2(P(pt)-point(op))) then -- exists already
              vtx(pt):= op;
            end if;
          end loop;
          if vtx(pt)= 0 then
            po:= po + 1;
            point(po):= P(pt);
            vtx(pt):= po;
          end if;
        end loop;
        face_proto.P:= vtx;
        fa:= fa+1;
        face(fa):= face_proto;
      end Do_Face;

    begin
      face_proto.skin:= texture_only;
      face_proto.repeat_U:= 1;
      face_proto.repeat_V:= 1;
      for dir in Cubic_Face_count loop
        face_proto.texture:= texture(dir);
        for r in 0..2 loop
          for s in 0..2 loop
            case dir is
              when 1 => Do_Face( (r+1,0,s), (r,0,s), (r,0,s+1), (r+1,0,s+1) );
              when 2 => Do_Face( (3,r+1,s), (3,r,s), (3,r,s+1), (3,r+1,s+1) );
              when 3 => Do_Face( (r,3,s), (r+1,3,s), (r+1,3,s+1), (r,3,s+1) );
              when 4 => Do_Face( (0,r,s), (0,r+1,s), (0,r+1,s+1), (0,r,s+1) );
              when 5 => Do_Face( (r,s,0), (r+1,s,0), (r+1,s+1,0), (r,s+1,0) );
              when 6 => Do_Face( (r+1,s,3), (r,s,3), (r,s+1,3), (r+1,s+1,3) );
            end case;
            if r=1 and s=1 then
              if opening(dir) then
                face(fa).skin:= invisible;
              end if;
              portal(dir):= fa;
            end if;
          end loop;
        end loop;
      end loop;
    end Cube;

  begin
    case kind is
      when cube  => Cube;
      when cross => null; --!!
    end case;

    object:= new Object_3D(po,fa);
    object.point:= point(1..po);
    object.face := face(1..fa);

    object.centre:= centre;
    Set_name(object.all, "Space station brick");

  end Create;

end Brick;
