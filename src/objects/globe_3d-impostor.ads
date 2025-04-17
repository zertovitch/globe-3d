-------------------------------------------------------------------------
--  GLOBE_3D.Impostor
--
--  Copyright (c) Rod Kay 2007 .. 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GLOBE_3D.Skinned_Visuals;

with
     GL.Primitive,
     GL.Geometry.VA,
     GL.Textures,
     GL.Skins,
     GL.Skinned_Geometry;

package GLOBE_3D.Impostor is

   -- A Globe_3D Object which contains a 2d image of another Globe_3d 'Visual'.

   type Impostor         is abstract new Skinned_Visuals.Skinned_Visual with private;
   type p_Impostor       is access all Impostor'Class;
   type p_Impostor_array is array (Positive range <>) of p_Impostor;

   procedure set_Target (o : in out Impostor;   Target : in Skinned_Visuals.p_Skinned_Visual);
   function  get_Target (o : in     Impostor)        return Skinned_Visuals.p_Skinned_Visual;

   function update_Required (o : access Impostor;   the_Camera  : in p_Camera) return Boolean   is abstract;
   --
   -- nb: Caches current pixel_Region as a side-effect.

   function is_Valid (o : in Impostor'Class) return Boolean;
   --
   -- True when rendered target has width and height greater than 0.

   function never_Updated (o : in Impostor'Class) return Boolean;
   --
   -- True when 'update' has never been called for the impostor.

   function frame_Count_since_last_update (o : in Impostor'Class) return Natural;

   procedure freshen (o : in out Impostor'Class;   the_Camera   : in     GLOBE_3D.p_Camera;
                                                   texture_Pool : in     GL.Textures.p_Pool;
                                                   is_Valid     :    out Boolean);
   --
   -- Takes a new snapshot of the target Visual.

   function target_camera_Distance (o : in Impostor'Class) return Real;
   --
   -- Returns the distance from the camera to the target, when 'update_required' was last called.

   -- Update Trigger Configuration
   --

   procedure set_freshen_count_update_trigger_Mod (o : in out Impostor;   To : in Positive);
   function  get_freshen_count_update_trigger_Mod (o : in     Impostor)    return Positive;
   --
   -- Periodic freshen trigger.

   procedure set_size_update_trigger_Delta        (o : in out Impostor;   To : in Positive);
   function  get_size_update_trigger_Delta        (o : in     Impostor)    return Positive;
   --
   -- Update due to change in size of targets pixel rectangle.

   -- Base Impostor Class
   --

   overriding
   function  is_Transparent     (o : in     Impostor) return Boolean;
   overriding
   procedure set_Alpha          (o : in out Impostor;   Alpha  : in GL.Double);

   overriding
   function  skinned_Geometries (o : in     Impostor) return GL.Skinned_Geometry.Skinned_Geometries;
   overriding
   function  face_Count         (o : in     Impostor) return Natural;
   overriding
   function  Bounds             (o : in     Impostor) return GL.Geometry.Bounds_record;

   procedure update (o : in out Impostor;   the_Camera                    : in     p_Camera;
                                            texture_Pool                  : in     GL.Textures.p_Pool);
   --
   -- Renders the impostor to a cleared framebuffer and copies the image to the impostors texture.

   overriding
   procedure Display (o : in out Impostor;   clip : in     Clipping_Data);

   -- Destruction
   --
   overriding
   procedure destroy (o : in out Impostor);
   procedure free    (o : in out p_Impostor);

private

   function Skin (o : access Impostor) return GL.Skins.p_Skin_transparent_unlit_textured;
   --
   -- Convenience function to allow brief access to dynamically typed 'skinned_Geometry.Skin' component.

   function Quads (o : in Impostor) return GL.Primitive.p_Quads;
   --
   -- Convenience function to allow brief access to the 'skinned_Geometry.Geometry' component.

   type pixel_Region is
      record
         X, Y          : GL.Int;
         Width, Height : GL.Sizei;
      end record;

   function get_pixel_Region (o : access Impostor'Class;   the_Camera : in GLOBE_3D.p_Camera) return pixel_Region;
   --
   -- Calculate and return the smallest rectangular screen region which encloses the target, when rendered by the_Camera.

   function general_Update_required      (o : access Impostor;   the_Camera       : in p_Camera; -- tbd: rename general_Update_required !
                                                         the_pixel_Region : in pixel_Region) return Boolean;

   function size_Update_required (o : access Impostor;   the_pixel_Region : in pixel_Region) return Boolean;

   type Counter is mod 2**32;

   type Impostor is abstract new Skinned_Visuals.Skinned_Visual with
      record
         Target                           : Skinned_Visuals.p_Skinned_Visual;
         skinned_Geometry                 : GL.Skinned_Geometry.Skinned_Geometry
                                          := (Geometry => new GL.Geometry.VA.primal_Geometry'
                                                                (Bounds    => GL.Geometry.null_Bounds,
                                                                 Primitive => GL.Primitive.new_Quads (quad_Count => 1).all'Access),
                                              Skin     => new GL.Skins.Skin_transparent_unlit_textured,
                                              Veneer   => null);

         freshen_Count                    : Counter  := 0;
         freshen_count_update_trigger_Mod : Counter  := 150;

         size_update_trigger_Delta        : GL.Sizei := 2;
         expand_X, expand_Y               : Real     := 0.01;     -- tbd: trying small expansion to rid 'border flicker'.

         is_Valid                         : Boolean  := True;     -- True when rendered target has both width and height > 0.
                                                                  -- (nb: Always true for simple impostors)
         never_Updated                    : Boolean  := True;

         -- Current State
         --
         current_pixel_Region    : pixel_Region;

         current_Width_pixels,
         current_Height_pixels   : GL.Sizei;
         current_copy_X_Offset,
         current_copy_Y_Offset   : GL.Int  := 0;
         current_copy_X,
         current_copy_Y          : GL.Int;
         current_copy_Width,
         current_copy_Height     : GL.Sizei;

         target_camera_Distance  : Real;

         -- Prior State
         --
         prior_pixel_Region    : pixel_Region := (X => 0, Y => 0,  Width => GL.Sizei'First,  Height => GL.Sizei'First);
         prior_Width_pixels    : GL.Sizei     := 0;
         prior_Height_pixels   : GL.Sizei     := 0;

         prior_target_Rotation : Matrix_33    := Id_33;                 -- tbd: use quaternion or euler angles, instead ?
         prior_target_Position : Vector_3D    := (0.0,  0.0,  0.0);

         prior_camera_Position : Vector_3D    := (1.0,  1.0,  1.0);
      end record;

end GLOBE_3D.Impostor;
