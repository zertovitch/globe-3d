with GLOBE_3D.Textures,
     GLOBE_3D.Math;

with glut.Windows; use glut.Windows;
with gl.Errors;
with GLU;



package body GLOBE_3D.Impostor.Simple is


   package G3DT renames GLOBE_3D.Textures;
   package G3DM renames GLOBE_3D.Math;




   procedure pre_Calculate (o : in out Impostor)
   is
   begin
      null;
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





   function update_Required (o : access Impostor;      the_Camera           : in     globe_3d.p_Camera) return Boolean
   is
      use GL, GLOBE_3D.Math;
      world_Rotation_original : Globe_3D.Matrix_33 := the_Camera.world_Rotation;
   begin
      o.current_Camera_look_at_Rotation := look_at (the_Camera.clipper.eye_Position,     -- look directly at target so it will be rendered
                                                    o.Target.Centre,                     -- in the centre of the viewport.
                                                    (the_Camera.world_Rotation (2, 1), the_Camera.world_Rotation (2, 2), the_Camera.world_Rotation (2, 3)));
      the_Camera.world_Rotation         := o.current_Camera_look_at_Rotation;
      o.current_pixel_Region            := o.get_pixel_Region (the_Camera);

      declare
         use gl.Textures, globe_3d.Math;
         use type gl.Double;
         update_Required : Boolean := o.general_Update_required (the_Camera, o.current_pixel_Region);
      begin
         if         not update_Required
           and then o.size_Update_required (o.current_pixel_Region)
         then
            update_Required := True;
         end if;

         if Update_required then
            o.current_Width_pixels  := o.current_pixel_Region.Width;       -- cache current state.
            o.current_Height_pixels := o.current_pixel_Region.Height;

            o.current_copy_X        := o.current_pixel_Region.X;
            o.current_copy_Y        := o.current_pixel_Region.Y;

            o.current_copy_Width    := o.current_pixel_Region.Width;
            o.current_copy_Height   := o.current_pixel_Region.Height;
         end if;

         the_Camera.world_Rotation := world_Rotation_original;

         return update_Required;
      end;
   end;





   procedure update (o : in out Impostor;   the_Camera   : in     p_Camera;
                                            texture_Pool : in     gl.textures.p_Pool)
   is
      use GL, GLOBE_3D.Math;
      world_Rotation_original : Globe_3D.Matrix_33 := the_Camera.world_Rotation;
   begin
      the_Camera.world_Rotation := o.current_Camera_look_at_Rotation;
      globe_3d.impostor.Impostor (o).update (the_Camera, texture_Pool);   -- base class 'update'
      the_Camera.world_Rotation := world_Rotation_original;
   end;



end GLOBE_3D.Impostor.Simple;

