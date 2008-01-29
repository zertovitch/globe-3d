--  Space vehicle #002 3D model. Copyright (c) Gautier de Montmollin 1999-2000
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use the herein contained 3D model for any purpose,
--  provided this copyright note remains attached and unmodified.

with GL;

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

package body Vehic002 is

  procedure Create(
    object: in out GLOBE_3D.p_Object_3D;
    scale :        GLOBE_3D.Real;
    centre:        GLOBE_3D.Point_3D;
    metal_door,
    metal_surface,
    bumped_blue  : GLOBE_3D.Image_id
  )
  is

    use GL, GLOBE_3D;

    po, fa: Natural:= 0;
    c1,c2,c3,c4: Positive;
    x,y,z: Real;
    p: Point_3D;
    rx,ry,fx: Real;
    nth: constant:= 13;
    ntv: constant:= 121;
    seed: Generator;

    face_0 : Face_type; -- takes defaults values
    point  : constant p_Point_3D_array:= new Point_3D_array(1..(nth+1)*ntv*4);
    face   : constant p_Face_array:= new Face_array(1..nth*ntv*4 + 2*ntv*ntv);

  begin
    Reset(seed);

    for tv in 0..ntv loop
     rx:= 2.0*Real(tv)/Real(ntv)-1.0;
     x:= scale*rx;
     fx:= 0.15 + 0.002 * (3.0*(rx*1.55+0.2)**5-16.0*(rx**2)-5.0*(rx**4)+1.2*rx);
     z:= scale*fx;
     for co in 1..4 loop
      for th in 1..nth loop
        ry:= 2.0*Real(th-1)/Real(nth)-1.0;
        ry:= ry * fx;
        y:= scale*ry;
        case co is
          when 1=> p:= (x,-y,-z);
          when 2=> p:= (x,-z, y);
          when 3=> p:= (x, y, z);
          when 4=> p:= (x, z,-y);
        end case;
        if tv>=1 then
          c1:= (co-1)*nth + th;
          c2:= c1 mod (4*nth) + 1;
          c3:= c2 + 4*nth;
          c4:= c1 + 4*nth;
          c1:= c1 + 4*nth*(tv-1);
          c2:= c2 + 4*nth*(tv-1);
          c3:= c3 + 4*nth*(tv-1);
          c4:= c4 + 4*nth*(tv-1);
          fa:= fa+1;
          face_0.P:= (c1,c4,c3,c2);
          if th in 2..nth-1 then
           face_0.skin:= colour_only;
           face_0.colour:= (0.3,0.3,0.3+0.4*Real(Random(seed)));
          else
            face_0.skin:= coloured_texture;
            face_0.colour:= (0.2,0.2,0.2+0.2*Real(Random(seed)));
            face_0.texture:= bumped_blue;
            face_0.repeat_U:= 3;
            face_0.repeat_V:= 3;
          end if;
          face(fa):= face_0;
        end if;
        po:= po + 1;
        point(po):= p;
      end loop;
     end loop;
    end loop;
    fa:= fa+1;
    face_0.P:= (1,1+nth,1+2*nth,1+3*nth);
    face_0.skin:= texture_only;
    face_0.texture:= metal_door;
    face_0.repeat_U:= 1;
    face_0.repeat_V:= 1;
    face(fa):= face_0;
    --
    fa:= fa+1;
    c1:= 1+ntv*4*nth;

    face_0.P:= (c1,c1+3*nth,c1+2*nth,c1+nth);
    face_0.skin:= texture_only;
    face_0.texture:= metal_surface;
    face_0.repeat_U:= 5;
    face_0.repeat_V:= 5;
    face(fa):= face_0;

    object:= new Object_3D(po,fa);
    object.point:= point(1..po);
    object.face := face(1..fa);

    object.centre:= centre;
    Set_name(object.all, "vehicle_002");
  end Create;

end Vehic002;
