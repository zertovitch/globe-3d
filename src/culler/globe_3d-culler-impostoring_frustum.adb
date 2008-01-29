
with globe_3d.Impostor.simple;
with globe_3d.Impostor.terrain;
with globe_3d.Math;               use globe_3d.Math;

with Ada_Containers_Generic_Array_Sort;

with ada.text_IO;       use ada.text_IO;
with ada.exceptions;    use ada.exceptions;
with ada.unchecked_Deallocation;




package body globe_3d.Culler.impostoring_frustum is




   procedure add (Self : in out Culler;   the_Visual : in globe_3d.p_Visual)
   is
      new_sprite_Set : sprite_Set_view := new sprite_Set;
   begin

      new_sprite_Set.Visual   := the_Visual;

      if the_Visual.is_Terrain then
         new_sprite_Set.Impostor := new impostor.terrain.Impostor;
      else
         new_sprite_Set.Impostor := new impostor.simple.Impostor;
         new_sprite_Set.Impostor.set_size_update_trigger_Delta        (to => 10);
         new_sprite_Set.Impostor.set_freshen_count_update_trigger_Mod (to => 250);
      end if;

      new_sprite_Set.Impostor.set_Target (the_Visual);

      self.object_sprite_set_Map.insert (the_Visual, new_Sprite_Set);
   end;




   procedure rid (Self : in out Culler;   the_Visual : in globe_3d.p_Visual)
   is
   begin
      free (self.object_sprite_set_Map.Element (the_Visual));
      self.object_sprite_set_Map.delete (the_Visual);
   end;




   function  object_Count (Self : in Culler) return Natural    -- tbd: should use ada.containers.Count_type instead of Natural ?
   is
   begin
      return Natural (self.object_sprite_set_Map.Length);
   end;




   procedure evolve (Self : in out Culler;
                     By   : in     Real)
   is
      all_Objects         : physics_object_sprite_set_Maps.Map    renames self.object_sprite_set_Map;
      Cursor              : physics_object_sprite_set_Maps.Cursor :=      First (all_Objects);

      type visible_Object is
         record
            Visual         : p_Visual;
            sprite_Set     : sprite_Set_view;
            apparent_Size  : Real;
         end record;

      visible_Objects : array (1 .. Natural (Length (all_Objects))) of visible_Object;
      Last            : Natural                                        := 0;
      the_Object      : p_Visual;

      Frustum : gl.frustums.Plane_array := self.Viewer.Camera.frustum_Planes;
   begin

      -- apply 'frustum' and 'apparent size' culling
      --
      while has_Element (Cursor) loop

         the_Object := Element (Cursor).Visual;

         declare
            use gl.Frustums;
            the_Size      : Real := the_Object.bounds.sphere_Radius;
            the_Distance  : Real := Norm (self.viewer.camera.clipper.eye_Position - the_Object.Centre);
            apparent_Size : Real := the_Size / the_Distance;

            function is_visible_for_plane (Which : in gl.frustums.plane_Id) return Boolean
            is
               the_Site       : Vector_3D renames the_Object.Centre;
               plane_Distance : Real              :=   Frustum (Which) (0) * the_Site (0)
                                                     + Frustum (Which) (1) * the_Site (1)
                                                     + Frustum (Which) (2) * the_Site (2)
                                                     + Frustum (Which) (3);
            begin
               return plane_Distance + the_Size > 0.0;
            end;

         begin
            --if apparent_Size > 0.004 then
            if         apparent_Size > 0.0012        -- tbd: make '0.0012' user-settable
              and then is_visible_for_plane (Left)
              and then is_visible_for_plane (Right)
              and then is_visible_for_plane (High)
              and then is_visible_for_plane (Low)
            then
               Last                                  := Last + 1;
               visible_Objects (Last).Visual := the_Object;
               visible_Objects (Last).sprite_Set     := Element (self.object_sprite_set_Map, the_Object);
               visible_Objects (Last).apparent_Size  := apparent_Size;
            end if;

         end;

         next (Cursor);
      end loop;


      -- find which level of detail sprite or imposter is used, for each physics object.
      --
      declare
         the_Sprites                : Visual_array (1 .. Last);
         transposed_camera_Attitude : Matrix_33               := Transpose (self.Viewer.Camera.world_Rotation);
         new_Last                   : Natural                 := 0;

      begin
         for Each in self.impostor_load_Slots'range loop
            self.impostor_load_Slots (Each).impostors_Count := 0;        -- empty each slot's contents.
         end loop;

         self.Viewer.enable;                       -- for multi-window operation (tbd: check this is needed)

         for Each in 1 .. Last loop
            declare
               the_Object : visible_Object renames visible_Objects (Each);
            begin
               if the_Object.apparent_Size < 0.0625 then   -- use impostor (tbd: make '0.0625' user-settable)
                  declare
                     impostor_Target   : p_Visual            renames the_Object.sprite_Set.Visual;
                     the_Impostor      : impostor.p_Impostor renames the_Object.sprite_Set.Impostor;
                  begin
                     declare
                        Impostor_update_required : Boolean := the_Impostor.update_Required (self.viewer.Camera'access);
                        Impostor_is_valid        : Boolean := the_Impostor.is_Valid;
                        Impostor_never_updated   : Boolean := the_Impostor.never_Updated;
                     begin
                        if Impostor_is_valid then

                           if Impostor_update_required then

                              if Impostor_never_updated then
                                 the_Impostor.update (self.viewer.Camera'access,  self.Texture_Pool'unchecked_access);     -- do immediate update to generate initial texture.
                              else
                                 declare  -- add impostor to appropriate load balancing slot.
                                    target_face_Count : Positive := impostor_Target.face_Count;

                                    function Slot_Id return Positive is
                                    begin
                                       for Each in self.impostor_load_Slots.all'range loop
                                          if target_face_Count <= self.impostor_load_Slots (Each).max_Faces then
                                             return Each;
                                          end if;
                                       end loop;
                                       raise program_Error;  -- self.impostor_load_Slots is not valid !   (tbd: use better exception ?)
                                    end;

                                    the_Slot : impostor_load_Balancer.Slot renames self.impostor_load_Slots (Slot_Id);
                                 begin
                                    the_Slot.impostors_Count                     := the_Slot.impostors_Count + 1;
                                    the_Slot.Impostors (the_Slot.impostors_Count) := the_Impostor;
                                 end;
                              end if;

                           end if;


                           the_Impostor.Centre   := the_Object.Visual.Centre;
                           the_Impostor.Rotation := transposed_camera_Attitude;

                           new_Last               := new_Last + 1;
                           the_Sprites (new_Last) := the_Impostor.all'access;
                        end if;
                     end;
                  end;

               else   -- don't use impostor
                  new_Last               := new_Last + 1;
                  the_Sprites (new_Last) := the_Object.Visual;
               end if;
            end;
         end loop;


         -- do the load balanced impostor updates
         --

         for Each in self.impostor_load_Slots'range loop
            declare
               the_Slot    : impostor_load_Balancer.Slot renames self.impostor_load_Slots (Each);
               num_Updates : Natural                          := Natural'Min (the_Slot.max_Updates, the_Slot.impostors_Count);

               function "<" (L, R : in impostor.p_Impostor) return Boolean
               is
               begin
                  return   L.target_camera_Distance - Real (L.frame_Count_since_last_update)  -- subtracting 'frame count' allows distant
                         < R.target_camera_Distance - Real (L.frame_Count_since_last_update); -- targets a chance of update (tbd: need some sort of user-settable scale param to allow for very large scales (space/etc)).
               end "<";

               --procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
               procedure sort is new Ada_Containers_Generic_Array_Sort (Positive,
                                                                        impostor.p_Impostor,
                                                                        impostor.p_Impostor_array);
            begin
               sort (the_Slot.Impostors (1 .. the_Slot.impostors_Count));

               for Each in 1 .. num_Updates loop
                  the_slot.Impostors (Each).update (self.viewer.Camera'access, self.texture_Pool'unchecked_access);
                  -- tbd: would 'flush' improve performance here ?
               end loop;
            end;
         end loop;


         self.Viewer.freshen (time_step => By,
                              extras    => the_Sprites (1 .. new_Last));
      end;

      self.frame_Count := self.frame_Count + 1;
   end;







   -- sprite_Set
   --

   procedure destroy (Self : in out sprite_Set)
   is
      use Impostor;
   begin
      free (self.Impostor);
   end;





   procedure free    (Self : in     sprite_Set_view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (sprite_Set, sprite_Set_view);
      Pad : sprite_Set_view := Self;
   begin
      destroy (Self.all);
      deallocate (Pad);
   end;



end globe_3d.Culler.impostoring_frustum;



