with
      GLOBE_3D.Math;

package body GLOBE_3D.Impostor.Simple is

   overriding
   procedure pre_Calculate (o : in out Impostor)
   is
   begin
      null;
   end pre_Calculate;

   procedure free (o : in out p_Impostor)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Impostor'Class, p_Impostor);
   begin
      if o /= null then
         destroy (o.all);
      end if;

      deallocate (o);
   end free;

   overriding
   function update_Required (o : access Impostor;   the_Camera : in GLOBE_3D.p_Camera) return Boolean
   is
      use GL, GLOBE_3D.Math;
      world_Rotation_original : constant GLOBE_3D.Matrix_33 := the_Camera.world_rotation;
   begin
      o.current_Camera_look_at_Rotation := Look_at (the_Camera.clipper.eye_position,     -- Look directly at target so it will be rendered
                                                    o.Target.centre,                     -- in the centre of the viewport.
                                                    (the_Camera.world_rotation (2, 1), the_Camera.world_rotation (2, 2), the_Camera.world_rotation (2, 3)));
      the_Camera.world_rotation         := o.current_Camera_look_at_Rotation;
      o.current_pixel_Region            := o.get_pixel_Region (the_Camera);

      declare
         update_Required : Boolean := o.general_Update_required (the_Camera, o.current_pixel_Region);
      begin
         if         not update_Required
           and then o.size_Update_required (o.current_pixel_Region)
         then
            update_Required := True;
         end if;

         if update_Required then
            o.current_Width_pixels  := o.current_pixel_Region.Width;       -- Cache current state.
            o.current_Height_pixels := o.current_pixel_Region.Height;

            o.current_copy_X        := o.current_pixel_Region.X;
            o.current_copy_Y        := o.current_pixel_Region.Y;

            o.current_copy_Width    := o.current_pixel_Region.Width;
            o.current_copy_Height   := o.current_pixel_Region.Height;
         end if;

         the_Camera.world_rotation := world_Rotation_original;

         return update_Required;
      end;
   end update_Required;

   overriding
   procedure update (o : in out Impostor;   the_Camera   : in p_Camera;
                                            texture_Pool : in GL.Textures.p_Pool)
   is
     world_Rotation_original : constant GLOBE_3D.Matrix_33 := the_Camera.world_rotation;
   begin
      the_Camera.world_rotation := o.current_Camera_look_at_Rotation;
      GLOBE_3D.Impostor.Impostor (o).update (the_Camera, texture_Pool);   -- Call base class update().
      the_Camera.world_rotation := world_Rotation_original;
   end update;

end GLOBE_3D.Impostor.Simple;
