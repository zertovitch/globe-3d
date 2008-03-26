
with glow.Windows;    use glow.Windows;       -- tbd: clean this up re textures !
with GL;
with GLOBE_3D.Math;
with Ada.Numerics;    use Ada.Numerics;



package body Box is

   package g3d renames GLOBE_3D;



  procedure Create (object  : in out GLOBE_3D.p_Object_3D;
                    Sides   :        GLOBE_3D.Vector_3D := (1.0, 1.0, 1.0);
                    scale   :        GLOBE_3D.Real      := 1.0;
                    centre  :        GLOBE_3D.Point_3D  := (0.0, 0.0, 0.0))
   is
      use GLOBE_3D, GL, GLOBE_3D.REF, GLOBE_3D.Math;

      function Basic_face (P      : G3D.Index_array;
                           texture: glow.windows.Texture_id;
                           colour : GL.RGB_Color;
                           repeat : Positive)       return Face_type
      is
         f: Face_type; -- takes defaults values
         alpha : GL.Double:= 1.0;
      begin
         f.P       := P;
         f.skin    := coloured_texture;
         f.texture := Texture_id'Pos(texture);
         f.colour  := colour;
         f.alpha   := alpha;
         f.repeat_U:= repeat;
         f.repeat_V:= repeat;
         return f;
      end Basic_face;


      half_Width  : constant g3d.Real := Scale * Sides (0) / 2.0;
      half_Height : constant g3d.Real := Scale * Sides (1) / 2.0;
      half_Depth  : constant g3d.Real := Scale * Sides (2) / 2.0;

   begin

      object:= new G3D.Object_3D (Max_points => 8,  Max_faces => 6);

      object.centre:= Centre;

      object.point:= ((-half_Width, -half_Height, -half_Depth),
                      (-half_Width,  half_Height, -half_Depth),
                      ( half_Width,  half_Height, -half_Depth),
                      ( half_Width, -half_Height, -half_Depth),
                      (-half_Width, -half_Height,  half_Depth),
                      (-half_Width,  half_Height,  half_Depth),
                      ( half_Width,  half_Height,  half_Depth),
                      ( half_Width, -half_Height,  half_Depth));

      object.face:= (Basic_face ((3,2,6,7), face1, (1.0,0.0,0.0), 1),
                     Basic_face ((4,3,7,8), face2, (0.0,1.0,0.0), 2),
                     Basic_face ((8,7,6,5), face3, (0.0,0.0,1.0), 3),
                     Basic_face ((1,4,8,5), face4, (1.0,1.0,0.0), 4),
                     Basic_face ((2,1,5,6), face5, (0.0,1.0,1.0), 5),
                     Basic_face ((3,4,1,2), face6, (1.0,0.0,1.0), 6));

      Set_name (object.all, "a box");

  end Create;


end Box;
