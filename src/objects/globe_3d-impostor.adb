with GLOBE_3D.Textures,
     GLOBE_3D.Math;

with glut.Windows; use glut.Windows;
with gl.Errors;
with GLU;

with ada.Text_IO;  use ada.Text_IO;



package body GLOBE_3D.Impostor is


   package G3DT renames GLOBE_3D.Textures;
   package G3DM renames GLOBE_3D.Math;




   procedure destroy (o : in out Impostor)
   is
      use gl.Geometry, gl.Skins;
   begin
      free (o.skinned_Geometry.Geometry);
      free (o.skinned_Geometry.Skin);
      free (o.skinned_Geometry.Veneer);
   end;




   procedure free (o : in out p_Impostor)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Impostor'Class, p_Impostor);
   begin
      if o /= null then
         destroy (o.all);
      end if;

      deallocate (o);
   end;




   function get_Target (O : in Impostor) return p_Visual
   is
   begin
      return o.Target;
   end;





   procedure set_Target (o : in out Impostor;   Target : in p_Visual)
   is
      use GL,  gl.Skins, gl.Geometry;
   begin
      o.Target     := Target;
      o.is_Terrain := Target.is_Terrain;

      Target.pre_Calculate;

      -- set o.skinned_Geometry.geometry.vertices & indices
      --
      declare
         Width : gl.Double := Target.bounds.sphere_Radius * 1.00;
      begin
         o.Quads.Vertices (1) := (-Width, -Width, 0.0);
         o.Quads.Vertices (2) := ( Width, -Width, 0.0);
         o.Quads.Vertices (3) := ( Width,  Width, 0.0);
         o.Quads.Vertices (4) := (-Width,  Width, 0.0);
      end;

      o.Quads.all.set_vertex_Id (1, 1,  1);      -- tbd: the '.all' required for gnat gpl06 ... not required in gpl07.
      o.Quads.all.set_vertex_Id (1, 2,  2);
      o.Quads.all.set_vertex_Id (1, 3,  3);
      o.Quads.all.set_vertex_Id (1, 4,  4);


      -- create the veneer, if necessary
      --
      if o.skinned_Geometry.Veneer = null then
         --o.skinned_Geometry.Veneer := o.skinned_Geometry.Skin.new_Veneer (o.Quads.all);
         o.skinned_Geometry.Veneer := o.skinned_Geometry.Skin.new_Veneer (o.skinned_geometry.Geometry.all);
      end if;

      --o.bounding_sphere_Radius := bounding_sphere_Radius (o.Quads.vertex_Pool.all);
      --o.Bounds := o.skinned_Geometry.Geometry.Bounds;
   end;



   -- update trigger configuration
   --

   procedure set_freshen_count_update_trigger_Mod (o : in out Impostor;   To : in Positive)
   is
   begin
      o.freshen_count_update_trigger_Mod := Counter (To);
   end;



   function get_freshen_count_update_trigger_Mod (o : in     Impostor) return Positive
   is
   begin
      return Positive (o.freshen_count_update_trigger_Mod);
   end;





   procedure set_size_update_trigger_Delta (o : in out Impostor;   To : in Positive)
   is
   begin
      o.size_update_trigger_Delta := gl.SizeI (To);
   end;



   function get_size_update_trigger_Delta (o : in     Impostor) return Positive
   is
   begin
      return Positive (o.size_update_trigger_Delta);
   end;






   function general_Update_required (o : access Impostor;   the_Camera       : in p_Camera;
                                                    the_pixel_Region : pixel_Region) return Boolean
   is
      use GL, Globe_3D.Math;
      Camera_has_moved : Boolean  :=  the_Camera.clipper.eye_Position /= o.prior_camera_Position;
      Target_has_moved : Boolean  :=  o.Target.Centre                 /= o.prior_target_Position;
   begin
      o.freshen_Count := o.freshen_Count + 1;

      if o.freshen_Count > o.freshen_count_update_trigger_Mod then
         return True;
      end if;


      if         Camera_has_moved
        and then abs (Angle (the_Camera.clipper.eye_Position, o.prior_target_Position, o.prior_camera_Position)) > to_Radians (degrees => 15.0)
      then
         return True;
      end if;


      if         Target_has_moved
        and then abs (Angle (o.target.Centre, o.prior_camera_Position, o.prior_target_Position)) > to_Radians (degrees => 15.0)
      then
         return True;
      end if;


      if         o.prior_pixel_Region.Width  >  40                -- ignore target rotation triggered updates when target is small on screen
        and then o.prior_pixel_Region.Height >  40                --
        and then o.prior_target_Rotation     /= o.target.Rotation
      then
         return True;
      end if;


      return False;
   end;





   function size_Update_required (o : access Impostor;   the_pixel_Region : in pixel_Region) return Boolean
   is
      use GL;
   begin
      return         abs (the_pixel_Region.Width  - o.prior_Width_Pixels)  > o.size_update_trigger_Delta
             or else abs (the_pixel_Region.Height - o.prior_Height_pixels) > o.size_update_trigger_Delta;
   end;








   function get_pixel_Region (o : access Impostor'Class;   the_Camera : in globe_3d.p_Camera) return pixel_Region
   is
      use GL, globe_3d.Math;

      target_Centre            : Vector_3d    := the_Camera.world_Rotation * (o.Target.Centre - the_Camera.clipper.eye_Position);
      target_lower_Left        : Vector_3d    := target_Centre - (o.Target.bounds.sphere_Radius, o.Target.bounds.sphere_Radius, 0.0);

      target_Centre_proj       : Vector_4d    := the_Camera.Projection_Matrix * target_Centre;
      target_Lower_Left_proj   : Vector_4d    := the_Camera.Projection_Matrix * target_lower_Left;

      target_Centre_norm       : Vector_3d    := (target_Centre_proj (0) / target_Centre_proj (3),
                                                  target_Centre_proj (1) / target_Centre_proj (3),
                                                  target_Centre_proj (2) / target_Centre_proj (3));
      target_Lower_Left_norm   : Vector_3d    := (target_Lower_Left_proj (0) / target_Lower_Left_proj (3),
                                                  target_Lower_Left_proj (1) / target_Lower_Left_proj (3),
                                                  target_Lower_Left_proj (2) / target_Lower_Left_proj (3));

      target_Centre_norm_0to1  : Vector_3d    := (target_Centre_norm (0)     * 0.5 + 0.5,
                                                  target_Centre_norm (1)     * 0.5 + 0.5,
                                                  target_Centre_norm (2)     * 0.5 + 0.5);
      target_Lower_Left_norm_0to1 : Vector_3d := (target_Lower_Left_norm (0) * 0.5 + 0.5,
                                                  target_Lower_Left_norm (1) * 0.5 + 0.5,
                                                  target_Lower_Left_norm (2) * 0.5 + 0.5);

      viewport_Width           : Integer      := the_Camera.clipper.main_Clipping.x2 - the_Camera.clipper.main_Clipping.x1 + 1;
      viewport_Height          : Integer      := the_Camera.clipper.main_Clipping.y2 - the_Camera.clipper.main_Clipping.y1 + 1;

      Width                    : Real         := 2.0  *  Real (viewport_Width) * (target_Centre_norm_0to1 (0) - target_Lower_Left_norm_0to1 (0));
      Width_pixels             : gl.Sizei     := gl.Sizei (  Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (0) + Width)
                                                           - Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (0))
                                                           + 1);

      Height                   : Real         := 2.0  *  Real (viewport_Height) * (target_Centre_norm_0to1 (1) - target_Lower_Left_norm_0to1 (1));
      Height_pixels            : gl.Sizei     := gl.Sizei (  Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (1) + Height)
                                                           - Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (1))
                                                           + 1);
   begin
      o.all.target_camera_Distance := Norm (target_Centre);   -- nb: cache distance from camera to target.

      return (x      => gl.Int (target_Lower_Left_norm_0to1 (0) * Real (Viewport_Width)),
              y      => gl.Int (target_Lower_Left_norm_0to1 (1) * Real (viewport_Height)),
              width  => Width_pixels,
              height => Height_pixels);
   end;





   procedure update (o            : in out Impostor;
                     the_Camera   : in     p_Camera;
                     texture_Pool : in     gl.textures.p_Pool)
   is
      use GL, GL.Textures;

      Width_size               : gl.textures.Size := to_Size (Natural (o.current_Width_pixels));
      Height_size              : gl.textures.Size := to_Size (Natural (o.current_Height_pixels));

      texture_Width            : gl.sizei         := gl.sizei (power_of_2_Ceiling (Natural (o.current_Width_pixels)));
      texture_Height           : gl.sizei         := gl.sizei (power_of_2_Ceiling (Natural (o.current_Height_pixels)));

      GL_Error : Boolean;
   begin
      o.prior_pixel_Region    := (o.current_copy_X, o.current_copy_Y,  o.current_Width_pixels, o.current_Height_pixels);
      o.prior_Width_pixels    := o.current_Width_pixels;
      o.prior_Height_pixels   := o.current_Height_pixels;
      o.prior_target_Rotation := o.target.Rotation;
      o.prior_target_Position := o.target.Centre;
      o.prior_camera_Position := the_Camera.clipper.Eye_Position;

      gl.ClearColor (0.0,  0.0,  0.0,   0.0);
      render        ((1 => o.Target),  the_Camera.all); -- render the target for subsequent copy to impostor texture.


      declare -- set texture coordinates for the veneer.
         use gl.Skins;
         the_Veneer : p_Veneer_transparent_unlit_textured := p_Veneer_transparent_unlit_textured (o.skinned_Geometry.Veneer);

         X_first    : Real := o.expand_X;
         Y_first    : Real := o.expand_Y;
         X_last     : Real := Real (o.current_Width_pixels)  / Real (texture_Width)  - X_First;
         Y_last     : Real := Real (o.current_Height_pixels) / Real (texture_Height) - Y_First;
      begin
         the_Veneer.texture_Coordinates := (1 => (s => X_first,     t => Y_first),
                                            2 => (s => X_last,      t => Y_first),
                                            3 => (s => X_last,      t => Y_last),
                                            4 => (s => X_first,     t => Y_last));
      end;


      if        Width_size  /= gl.textures.Size_width  (o.skin.Texture)
        or else Height_size /= gl.textures.Size_height (o.skin.Texture)
      then
         free (texture_Pool.all,  o.skin.Texture);
         o.skin.all.Texture := new_Texture (texture_Pool,  Natural (texture_Width),  Natural (texture_Height));
      end if;


      enable (o.skin.all.Texture);


      gl.CopyTexSubImage2D (gl.TEXTURE_2D,  0,
                            o.current_copy_x_Offset, o.current_copy_y_Offset,
                            o.current_copy_X,        o.current_copy_Y,
                            o.current_copy_Width,    o.current_copy_Height);

      gl.Errors.log (error_occurred => gl_Error);

      if gl_Error then
         put_Line ("x_Offset: " & gl.Int'image (o.current_copy_x_Offset) & " ********");
         put_Line ("y_Offset: " & gl.Int'image (o.current_copy_y_Offset));

         put_Line ("start x: " & gl.Int'image (o.current_copy_X));
         put_Line ("start y: " & gl.Int'image (o.current_copy_Y));

         put_Line ("copy width: "  & gl.sizei'image (o.current_copy_Width));
         put_Line ("copy height: " & gl.sizei'image (o.current_copy_Height));

         put_Line ("width_pixels: "  & gl.sizei'image (o.current_Width_pixels));
         put_Line ("height_pixels: " & gl.sizei'image (o.current_Height_pixels));

         put_Line ("width_size: "  & gl.textures.size'image (Width_size));
         put_Line ("height_size: " & gl.textures.size'image (Height_size));

         put_Line ("texture width: "  & gl.sizei'image (texture_Width));
         put_Line ("texutre height: " & gl.sizei'image (texture_Height));
      end if;

      o.never_Updated := False;
      o.freshen_Count := 0;
   end;





   procedure freshen (o : in out Impostor'Class;   the_Camera   : in     globe_3d.p_Camera;
                                                   texture_Pool : in     gl.Textures.p_Pool;
                                                   is_Valid     :    out Boolean            )
   is
      update_Required : Boolean := o.Update_required (the_Camera);    -- nb: caches current update info
   begin
      if update_Required then
         o.update (the_Camera, texture_Pool);
      end if;

      is_Valid := o.is_Valid;
   end;





   function target_camera_Distance (o : in Impostor'Class) return Real
   is
   begin
      return o.target_camera_Distance;
   end;




   function is_Valid (o : in Impostor'Class) return Boolean
   is
   begin
      return o.is_Valid;
   end;




   function never_Updated (o : in Impostor'Class) return Boolean
   is
   begin
      return o.never_Updated;
   end;




   function frame_Count_since_last_update (o : in Impostor'Class) return Natural
   is
   begin
      return Natural (o.freshen_Count);
   end;





   function skinned_Geometrys (o : in Impostor) return gl.skinned_geometry.skinned_Geometrys
   is
   begin
      return (1 => o.skinned_Geometry);
   end;






   function face_Count (o : in Impostor) return Natural
   is
   begin
      return 1;
   end;




   procedure Display (o : in out Impostor;   clip : in     Clipping_data)
   is
   begin
      null;   -- actual display is done by the renderer (ie glut.Windows), which requests all skinned Geometry's
              -- and then applies 'gl state' sorting for performance, before drawing.
   end Display;




   procedure set_Alpha (o    : in out Impostor;   Alpha : in gl.Double)
   is
   begin
      null;   -- tbd
   end;




   function Bounds (o : in     Impostor) return gl.geometry.Bounds_record
   is
   begin
      return o.skinned_geometry.Geometry.Bounds;
   end;





   function  is_Transparent (o    : in Impostor) return Boolean
   is
   begin
      return True;   -- tbd: - if using gl alpha test, depth sorting is not needed apparently.
                     --        in which case this could be set to False, and treated as a non-transparent in g3d.render.
                     --        may then be faster (?).
                     --      - seems to make little difference ... test with different vid card.
   end;




   function Skin (o : access Impostor) return gl.skins.p_Skin_transparent_unlit_textured
   is
   begin
      return gl.skins.p_Skin_transparent_unlit_textured (o.skinned_geometry.skin);
   end;




   function Quads (o : in Impostor) return gl.geometry.primitives.p_Quads
   is
      use gl.Geometry.Primitives, gl.geometry.primal;
   begin
      return p_Quads (p_primal_Geometry (o.skinned_geometry.Geometry).Primitive);
   end;






   -- note: only old, unused code folows (may be useful) ...
   --


   -- tbd: enable_rotation is no good for impostors, since they must be aligned with the viewport
   --      it might be useful for general billboards however !
   --
   procedure enable_Rotation (o : in Impostor;   camera_Site : in Vector_3D)
   is
      use globe_3d.Math, globe_3d.REF, GL;
      lookAt       : Vector_3D := (0.0,  0.0,  1.0);
      objToCamProj : Vector_3D := Normalized ((camera_Site (0) - o.Centre (0),  0.0,  camera_Site (2) - o.Centre (2)));
      upAux        : Vector_3D := lookAt * objToCamProj;
      angleCosine  : gl.Double := lookAt * objToCamProj;
   begin
      if    angleCosine > -0.9999
        and angleCosine <  0.9999
      then
         gl.Rotate (arcCos (angleCosine) * 180.0 / 3.14,   upAux (0), upAux (1), upAux (2));
      end if;

      declare
         objToCam : Vector_3D := Normalized ((camera_Site (0) - o.Centre (0),
                                              camera_Site (1) - o.Centre (1),
                                              camera_Site (2) - o.Centre (2)));
      begin
         angleCosine := objToCamProj * objToCam;

         if    angleCosine > -0.9999
           and angleCosine <  0.9999
         then
            if objToCam (1) < 0.0 then
               gl.Rotate (arcCos (angleCosine) * 180.0 / 3.14,   1.0, 0.0, 0.0);
            else
               gl.Rotate (arcCos (angleCosine) * 180.0 / 3.14,  -1.0, 0.0, 0.0);
            end if;
         end if;
      end;

   end;
   --
   -- based on lighthouse3d billboard example.


end GLOBE_3D.Impostor;

