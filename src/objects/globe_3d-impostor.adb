--  with GLOBE_3D.Textures,
with GLOBE_3D.Math;
with GL.Math;

--  with GLUT.Windows; use GLUT.Windows;
with GL.Errors;
--  with GLU;

with Ada.Text_IO;  use Ada.Text_IO;

package body GLOBE_3D.Impostor is

   --  package G3DT renames GLOBE_3D.Textures;
   --  package G3DM renames GLOBE_3D.Math;

   procedure destroy (o : in out Impostor)
   is
      use GL.Geometry, GL.Skins;
   begin
      free (o.skinned_Geometry.Geometry);
      free (o.skinned_Geometry.Skin);
      free (o.skinned_Geometry.Veneer);
   end;

   procedure free (o : in out p_Impostor)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Impostor'Class, p_Impostor);
   begin
      if o /= null then
         destroy (o.all);
      end if;

      deallocate (o);
   end;

   function get_Target (o : in Impostor) return p_Visual
   is
   begin
      return o.Target;
   end;

   procedure set_Target (o : in out Impostor;   Target : in p_Visual)
   is
      use GL,  GL.Skins, GL.Geometry;
   begin
      o.Target     := Target;
      o.is_Terrain := Target.is_Terrain;

      Target.Pre_calculate;

      -- set o.skinned_Geometry.geometry.vertices & indices
      --
      declare
        Width : constant GL.Double := Target.Bounds.sphere_Radius * 1.00;
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
         o.skinned_Geometry.Veneer := o.skinned_Geometry.Skin.new_Veneer (o.skinned_Geometry.Geometry.all);
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
      o.size_update_trigger_Delta := GL.Sizei (To);
   end;

   function get_size_update_trigger_Delta (o : in     Impostor) return Positive
   is
   begin
      return Positive (o.size_update_trigger_Delta);
   end;

   function general_Update_required (o : access Impostor;   the_Camera       : in p_Camera;
                                                            the_pixel_Region : in pixel_Region) return Boolean
   is
   pragma Unreferenced (the_pixel_Region);
      use GL, GLOBE_3D.Math, GL.Math;
      Camera_has_moved : constant Boolean  :=  the_Camera.clipper.eye_position /= o.prior_camera_Position;
      Target_has_moved : constant Boolean  :=  o.Target.centre                 /= o.prior_target_Position;
   begin
      o.freshen_Count := o.freshen_Count + 1;

      if o.freshen_Count > o.freshen_count_update_trigger_Mod then
         return True;
      end if;

      if         Camera_has_moved
        and then abs (Angle (the_Camera.clipper.eye_position, o.prior_target_Position, o.prior_camera_Position)) > to_Radians (Degrees => 15.0)
      then
         return True;
      end if;

      if         Target_has_moved
        and then abs (Angle (o.Target.centre, o.prior_camera_Position, o.prior_target_Position)) > to_Radians (Degrees => 15.0)
      then
         return True;
      end if;

      if         o.prior_pixel_Region.Width  >  40                -- ignore target rotation triggered updates when target is small on screen
        and then o.prior_pixel_Region.Height >  40                --
        and then o.prior_target_Rotation     /= o.Target.rotation
      then
         return True;
      end if;

      return False;
   end;

   function size_Update_required (o : access Impostor;   the_pixel_Region : in pixel_Region) return Boolean
   is
      use GL;
   begin
      return         abs (the_pixel_Region.Width  - o.prior_Width_pixels)  > o.size_update_trigger_Delta
             or else abs (the_pixel_Region.Height - o.prior_Height_pixels) > o.size_update_trigger_Delta;
   end;

   function get_pixel_Region (o : access Impostor'Class;   the_Camera : in GLOBE_3D.p_Camera) return pixel_Region
   is
      use GL, GLOBE_3D.Math, GL.Math;

      target_Centre            : constant Vector_3D    := the_Camera.world_rotation * (o.Target.centre - the_Camera.clipper.eye_position);
      target_lower_Left        : constant Vector_3D    := target_Centre - (o.Target.Bounds.sphere_Radius, o.Target.Bounds.sphere_Radius, 0.0);

      target_Centre_proj       : constant Vector_4D    := the_Camera.projection_matrix * target_Centre;
      target_Lower_Left_proj   : constant Vector_4D    := the_Camera.projection_matrix * target_lower_Left;

      target_Centre_norm       : constant Vector_3D    :=
                                                 (target_Centre_proj (0) / target_Centre_proj (3),
                                                  target_Centre_proj (1) / target_Centre_proj (3),
                                                  target_Centre_proj (2) / target_Centre_proj (3));
                                                  target_Lower_Left_norm   : constant Vector_3D    := (target_Lower_Left_proj (0) / target_Lower_Left_proj (3),
                                                  target_Lower_Left_proj (1) / target_Lower_Left_proj (3),
                                                  target_Lower_Left_proj (2) / target_Lower_Left_proj (3));

      target_Centre_norm_0to1  : constant Vector_3D    :=
                                                 (target_Centre_norm (0)     * 0.5 + 0.5,
                                                  target_Centre_norm (1)     * 0.5 + 0.5,
                                                  target_Centre_norm (2)     * 0.5 + 0.5);
      target_Lower_Left_norm_0to1 : constant Vector_3D :=
                                                 (target_Lower_Left_norm (0) * 0.5 + 0.5,
                                                  target_Lower_Left_norm (1) * 0.5 + 0.5,
                                                  target_Lower_Left_norm (2) * 0.5 + 0.5);

      viewport_Width           : constant Integer      := the_Camera.clipper.main_clipping.X2 - the_Camera.clipper.main_clipping.X1 + 1;
      viewport_Height          : constant Integer      := the_Camera.clipper.main_clipping.Y2 - the_Camera.clipper.main_clipping.Y1 + 1;

      Width                    : constant Real         := 2.0  *  Real (viewport_Width) * (target_Centre_norm_0to1 (0) - target_Lower_Left_norm_0to1 (0));
      Width_pixels             : constant GL.Sizei     := GL.Sizei (  Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (0) + Width)
                                                           - Integer (Real (viewport_Width) * target_Lower_Left_norm_0to1 (0))
                                                           + 1);

      Height                   : constant Real         := 2.0  *  Real (viewport_Height) * (target_Centre_norm_0to1 (1) - target_Lower_Left_norm_0to1 (1));
      Height_pixels            : constant GL.Sizei     := GL.Sizei (  Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (1) + Height)
                                                           - Integer (Real (viewport_Height) * target_Lower_Left_norm_0to1 (1))
                                                           + 1);
   begin
      o.all.target_camera_Distance := Norm (target_Centre);   -- nb: cache distance from camera to target.

      return (X      => GL.Int (target_Lower_Left_norm_0to1 (0) * Real (viewport_Width)),
              Y      => GL.Int (target_Lower_Left_norm_0to1 (1) * Real (viewport_Height)),
              Width  => Width_pixels,
              Height => Height_pixels);
   end;

   procedure update (o            : in out Impostor;
                     the_Camera   : in     p_Camera;
                     texture_Pool : in     GL.Textures.p_Pool)
   is
      use GL, GL.Textures;

Width_size               : constant GL.Textures.Size := to_Size (Natural (o.current_Width_pixels));
Height_size              : constant GL.Textures.Size := to_Size (Natural (o.current_Height_pixels));

texture_Width            : constant GL.Sizei         := power_of_2_Ceiling (Natural (o.current_Width_pixels));
texture_Height           : constant GL.Sizei         := power_of_2_Ceiling (Natural (o.current_Height_pixels));

      GL_Error : Boolean;
   begin
      o.prior_pixel_Region    := (o.current_copy_X, o.current_copy_Y,  o.current_Width_pixels, o.current_Height_pixels);
      o.prior_Width_pixels    := o.current_Width_pixels;
      o.prior_Height_pixels   := o.current_Height_pixels;
      o.prior_target_Rotation := o.Target.rotation;
      o.prior_target_Position := o.Target.centre;
      o.prior_camera_Position := the_Camera.clipper.eye_position;

      GL.ClearColor (0.0,  0.0,  0.0,   0.0);
      Render        ((1 => o.Target),  the_Camera.all); -- render the target for subsequent copy to impostor texture.

      declare -- set texture coordinates for the veneer.
         use GL.Skins;
         the_Veneer : constant p_Veneer_transparent_unlit_textured := p_Veneer_transparent_unlit_textured (o.skinned_Geometry.Veneer);

         X_first    : constant Real := o.expand_X;
         Y_first    : constant Real := o.expand_Y;
         X_last     : constant Real := Real (o.current_Width_pixels)  / Real (texture_Width)  - X_first;
         Y_last     : constant Real := Real (o.current_Height_pixels) / Real (texture_Height) - Y_first;
      begin
         the_Veneer.texture_Coordinates := (1 => (S => X_first,     T => Y_first),
                                            2 => (S => X_last,      T => Y_first),
                                            3 => (S => X_last,      T => Y_last),
                                            4 => (S => X_first,     T => Y_last));
      end;

      if        Width_size  /= GL.Textures.Size_width  (o.Skin.Texture)
        or else Height_size /= GL.Textures.Size_height (o.Skin.Texture)
      then
         free (texture_Pool.all,  o.Skin.Texture);
         o.Skin.all.Texture := new_Texture (texture_Pool,  Natural (texture_Width),  Natural (texture_Height));
      end if;

      enable (o.Skin.all.Texture);

      GL.CopyTexSubImage2D (GL.TEXTURE_2D,  0,
                            o.current_copy_X_Offset, o.current_copy_Y_Offset,
                            o.current_copy_X,        o.current_copy_Y,
                            o.current_copy_Width,    o.current_copy_Height);

      GL.Errors.Log (error_Occurred => GL_Error);

      if GL_Error then
         Put_Line ("x_Offset: " & GL.Int'Image (o.current_copy_X_Offset) & " ********");
         Put_Line ("y_Offset: " & GL.Int'Image (o.current_copy_Y_Offset));

         Put_Line ("start x: " & GL.Int'Image (o.current_copy_X));
         Put_Line ("start y: " & GL.Int'Image (o.current_copy_Y));

         Put_Line ("copy width: "  & GL.Sizei'Image (o.current_copy_Width));
         Put_Line ("copy height: " & GL.Sizei'Image (o.current_copy_Height));

         Put_Line ("width_pixels: "  & GL.Sizei'Image (o.current_Width_pixels));
         Put_Line ("height_pixels: " & GL.Sizei'Image (o.current_Height_pixels));

         Put_Line ("width_size: "  & GL.Textures.Size'Image (Width_size));
         Put_Line ("height_size: " & GL.Textures.Size'Image (Height_size));

         Put_Line ("texture width: "  & GL.Sizei'Image (texture_Width));
         Put_Line ("texutre height: " & GL.Sizei'Image (texture_Height));
      end if;

      o.never_Updated := False;
      o.freshen_Count := 0;
   end;

   procedure freshen (o : in out Impostor'Class;   the_Camera   : in     GLOBE_3D.p_Camera;
                                                   texture_Pool : in     GL.Textures.p_Pool;
                                                   is_Valid     :    out Boolean            )
   is
      update_Required : constant Boolean := o.update_Required (the_Camera);    -- nb: caches current update info
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

   function skinned_Geometries (o : in Impostor) return GL.Skinned_Geometry.Skinned_Geometries
   is
   begin
      return (1 => o.skinned_Geometry);
   end;

   function face_Count (o : in Impostor) return Natural
   is
   pragma Unreferenced (o);
   begin
      return 1;
   end;

   procedure Display (o : in out Impostor;   clip : in     Clipping_data)
   is
   begin
      null;   -- actual display is done by the renderer (ie glut.Windows), which requests all skinned Geometry's
              -- and then applies 'gl state' sorting for performance, before drawing.
   end Display;

   procedure set_Alpha (o    : in out Impostor;   Alpha : in GL.Double)
   is
   begin
      null;   -- tbd
   end;

   function Bounds (o : in     Impostor) return GL.Geometry.Bounds_record
   is
   begin
      return o.skinned_Geometry.Geometry.Bounds;
   end;

   function  is_Transparent (o    : in Impostor) return Boolean
   is
   pragma Unreferenced (o);
   begin
      return True;   -- tbd: - if using gl alpha test, depth sorting is not needed apparently.
                     --        in which case this could be set to False, and treated as a non-transparent in g3d.render.
                     --        may then be faster (?).
                     --      - seems to make little difference ... test with different vid card.
   end;

   function Skin (o : access Impostor) return GL.Skins.p_Skin_transparent_unlit_textured
   is
   begin
      return GL.Skins.p_Skin_transparent_unlit_textured (o.skinned_Geometry.Skin);
   end;

   function Quads (o : in Impostor) return GL.Primitive.p_Quads
   is
      use GL.Primitive, GL.Geometry.VA;
   begin
      return p_Quads (p_primal_Geometry (o.skinned_Geometry.Geometry).Primitive);
   end;

   -- note: only old, unused code folows (may be useful) ...
   --

   -- tbd: enable_rotation is no good for impostors, since they must be aligned with the viewport
   --      it might be useful for general billboards however !
   --
   procedure enable_Rotation (o : in Impostor;   camera_Site : in Vector_3D)
   is
      use GLOBE_3D.Math, GL.Math, GLOBE_3D.REF, GL;
      lookAt       : constant Vector_3D := (0.0,  0.0,  1.0);
      objToCamProj : constant Vector_3D := Normalized ((camera_Site (0) - o.centre (0),  0.0,  camera_Site (2) - o.centre (2)));
      upAux        : constant Vector_3D := lookAt * objToCamProj;
      angleCosine  : GL.Double := lookAt * objToCamProj;
   begin
      if    angleCosine > -0.9999
        and angleCosine <  0.9999
      then
         GL.Rotate (Arccos (angleCosine) * 180.0 / 3.14,   upAux (0), upAux (1), upAux (2));
      end if;

      declare
      objToCam : constant Vector_3D := Normalized ((camera_Site (0) - o.centre (0),
                                              camera_Site (1) - o.centre (1),
                                              camera_Site (2) - o.centre (2)));
      begin
         angleCosine := objToCamProj * objToCam;

         if    angleCosine > -0.9999
           and angleCosine <  0.9999
         then
            if objToCam (1) < 0.0 then
               GL.Rotate (Arccos (angleCosine) * 180.0 / 3.14,   1.0, 0.0, 0.0);
            else
               GL.Rotate (Arccos (angleCosine) * 180.0 / 3.14,  -1.0, 0.0, 0.0);
            end if;
         end if;
      end;

   end;
   pragma Unreferenced (enable_Rotation);
   --
   -- based on lighthouse3d billboard example.

end GLOBE_3D.Impostor;
