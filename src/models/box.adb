with GL;
with GLOBE_3D.Textures;

package body Box is

  procedure Create (object  : in out GLOBE_3D.p_Object_3D;
                    Sides   :        GLOBE_3D.Vector_3D := (1.0, 1.0, 1.0);
                    scale   :        GLOBE_3D.Real      := 1.0;
                    centre  :        GLOBE_3D.Point_3D  := (0.0, 0.0, 0.0))
   is
      use GLOBE_3D;

      function Basic_face (P       : GLOBE_3D.Index_Array;
                           texture : String;
                           colour  : GL.RGB_Color;
                           repeat  : Positive)       return Face_Type
      is
         f : Face_Type; -- takes defaults values
         alpha : constant GL.Double := 1.0;
         new_id : Image_ID;
      begin
         f.P        := P;
         f.skin     := coloured_texture;
         GLOBE_3D.Textures.Add_Texture_Name (texture, new_id);
         f.texture  := new_id;
         f.colour   := colour;
         f.alpha    := alpha;
         f.repeat_U := repeat;
         f.repeat_V := repeat;
         return f;
      end Basic_face;

      use GL;

      half_width  : constant GLOBE_3D.Real := scale * Sides (0) / 2.0;
      half_height : constant GLOBE_3D.Real := scale * Sides (1) / 2.0;
      half_depth  : constant GLOBE_3D.Real := scale * Sides (2) / 2.0;

   begin

      object := new GLOBE_3D.Object_3D (Max_points => 8,  Max_faces => 6);

      object.centre := centre;

      object.point := ((-half_width, -half_height, -half_depth),
                       (-half_width,  half_height, -half_depth),
                        (half_width,  half_height, -half_depth),
                        (half_width, -half_height, -half_depth),
                       (-half_width, -half_height,  half_depth),
                       (-half_width,  half_height,  half_depth),
                        (half_width,  half_height,  half_depth),
                        (half_width, -half_height,  half_depth));

      object.face := (Basic_face ((3, 2, 6, 7), "face1", (1.0, 0.0, 0.0), 1),
                      Basic_face ((4, 3, 7, 8), "face2", (0.0, 1.0, 0.0), 2),
                      Basic_face ((8, 7, 6, 5), "face3", (0.0, 0.0, 1.0), 3),
                      Basic_face ((1, 4, 8, 5), "face4", (1.0, 1.0, 0.0), 4),
                      Basic_face ((2, 1, 5, 6), "face5", (0.0, 1.0, 1.0), 5),
                      Basic_face ((3, 4, 1, 2), "face6", (1.0, 0.0, 1.0), 6));

      Set_Name (object.all, "a box");

  end Create;

end Box;
