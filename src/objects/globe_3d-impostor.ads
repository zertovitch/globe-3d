-------------------------------------------------------------------------
--  GLOBE_3D - GL-based, real-time, 3D engine
--
--  Copyright (c) Gautier de Montmollin/Rod Kay 2007
--  CH-8810 Horgen
--  SWITZERLAND
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with gl.Geometry.Primitives,  gl.geometry.primal, gl.Textures,  gl.Skins,  gl.skinned_Geometry;
with globe_3d.Math;



package GLOBE_3D.Impostor is

   -- a Globe_3D Object which contains a 2d image of another Globe_3d 'Visual'.


   type Impostor         is abstract new Visual with private;
   type p_Impostor       is access all Impostor'Class;
   type p_Impostor_array is array (Positive range <>) of p_Impostor;


   procedure set_Target (o : in out Impostor;   Target : in p_Visual);
   function  get_Target (o : in     Impostor                        ) return p_Visual;




   function update_Required (o : access Impostor;   the_Camera  : in     globe_3d.p_Camera) return Boolean   is abstract;
   --
   -- nb: caches current pixel_Region as a side-effect.


   function is_Valid (o : in Impostor'Class) return Boolean;
   --
   -- true when rendered target has width and height greater than 0.


   function never_Updated (o : in Impostor'Class) return Boolean;
   --
   -- true when 'update' has never been called for the impostor.


   function frame_Count_since_last_update (o : in Impostor'Class) return Natural;


   procedure freshen (o : in out Impostor'Class;   the_Camera   : in     globe_3d.p_Camera;
                                                   texture_Pool : in     gl.Textures.p_Pool;
                                                   is_Valid     :    out Boolean            );
   --
   -- takes a new snapshot of the target Visual.



   function target_camera_Distance (o : in Impostor'Class) return Real;
   --
   -- returns the distance from the camera to the target, when 'update_required' was last called.


   -- update trigger configuration
   --

   procedure set_freshen_count_update_trigger_Mod (o : in out Impostor;   To : in Positive);
   function  get_freshen_count_update_trigger_Mod (o : in     Impostor)                     return Positive;
   --
   -- periodic freshen trigger.

   procedure set_size_update_trigger_Delta        (o : in out Impostor;   To : in Positive);
   function  get_size_update_trigger_Delta        (o : in     Impostor)                     return Positive;
   --
   -- update due to change in size of targets pixel rectangle.



   -- base class subprograms
   --

   function  is_Transparent    (o : in     Impostor) return Boolean;
   procedure set_Alpha         (o : in out Impostor;   Alpha  : in gl.Double);

   function  skinned_Geometrys (o : in     Impostor) return gl.skinned_geometry.skinned_Geometrys;
   function  face_Count        (o : in     Impostor) return Natural;
   function  Bounds            (o : in     Impostor) return gl.geometry.Bounds_record;


   procedure update (o : in out Impostor;   the_Camera                    : in     p_Camera;
                                            texture_Pool                  : in     gl.textures.p_Pool);
   --
   -- renders the impostor to a cleared framebuffer and copies the image to the impostors texture.



   procedure Display (o : in out Impostor;   clip : in     Clipping_data);



   -- destruction
   --
   procedure destroy (o : in out Impostor);
   procedure free    (o : in out p_Impostor);





private


   function Skin (o : access Impostor) return gl.skins.p_Skin_transparent_unlit_textured;
   --
   -- convenience function to allow brief access to dynamically typed 'skinned_Geometry.Skin' component.


   function Quads (o : in Impostor) return gl.geometry.primitives.p_Quads;
   --
   -- convenience function to allow brief access to the 'skinned_Geometry.Geometry' component.




   type pixel_Region is
      record
         X, Y          : gl.Int;
         Width, Height : gl.SizeI;
      end record;


   function get_pixel_Region (o : access Impostor'Class;   the_Camera : in globe_3d.p_Camera) return pixel_Region;
   --
   -- calculate and return the smallest rectangular screen region which encloses the target, when rendered by the_Camera.


   function general_Update_required      (o : access Impostor;   the_Camera       : in p_Camera; -- tbd: rename general_Update_required !
                                                         the_pixel_Region : in pixel_Region) return Boolean;

   function size_Update_required (o : access Impostor;   the_pixel_Region : in pixel_Region) return Boolean;




   type Counter is mod 2**32;


   type Impostor is abstract new Visual with
      record
         Target                           : p_Visual;
         --skinned_Geometry      : gl.skinned_geometry.skinned_Geometry := (geometry => gl.geometry.new_Quads (1, lit => False).all'access,
         skinned_Geometry                 : gl.skinned_geometry.skinned_Geometry
                                          := (geometry => new gl.geometry.primal.primal_Geometry'
                                                                (bounds    => gl.geometry.null_Bounds,
                                                                 primitive => gl.geometry.primitives.new_Quads (quad_count => 1).all'access),
                                              skin      => new gl.skins.Skin_transparent_unlit_textured,
                                              veneer    => null);

         freshen_Count                    : Counter  := 0;
         freshen_count_update_trigger_Mod : Counter  := 150;

         size_update_trigger_Delta        : gl.SizeI := 2;
         expand_X, expand_Y               : Real     := 0.01; -- tbd: trying small expansion to rid 'border flicker'.

         is_Valid                         : Boolean  := True;     -- true when rendered target has both width and height > 0.
                                                                  -- (nb: always true for simple impostors)
         never_Updated                    : Boolean  := True;

         -- current state
         --
         current_pixel_Region    : pixel_Region;

         current_Width_pixels,
         current_Height_pixels   : gl.Sizei;
         current_copy_X_Offset,
         current_copy_Y_Offset   : gl.Int  := 0;
         current_copy_X,
         current_copy_Y          : gl.Int;
         current_copy_Width,
         current_copy_Height     : gl.SizeI;

         target_camera_Distance  : Real;

         -- prior state
         --
         prior_pixel_Region    : pixel_Region := (x => 0, y => 0,  width => gl.SizeI'First,  height => gl.SizeI'First);
         prior_Width_pixels    : gl.Sizei     := 0;
         prior_Height_pixels   : gl.Sizei     := 0;

         prior_target_Rotation : Matrix_33    := Id_33;                 -- tbd: use quaternion or euler angles, instead ?
         prior_target_Position : Vector_3D    := (0.0,  0.0,  0.0);

         prior_camera_Position : Vector_3D    := (1.0,  1.0,  1.0);
      end record;


end GLOBE_3D.Impostor;
