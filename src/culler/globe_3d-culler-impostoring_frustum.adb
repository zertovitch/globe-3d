
with GLOBE_3D.Impostor.Simple;
with GLOBE_3D.Impostor.Terrain;
with GLOBE_3D.Math;               use GLOBE_3D.Math;
with GL.Math;

with Ada.Containers.Generic_Array_Sort;

with Ada.Unchecked_Deallocation;

package body GLOBE_3D.Culler.Impostoring_frustum is

   procedure add (Self : in out Culler;   the_Visual : in GLOBE_3D.p_Visual)
   is
      new_sprite_Set : constant sprite_Set_view := new sprite_Set;
   begin

      new_sprite_Set.Visual   := the_Visual;

      if the_Visual.is_Terrain then
         new_sprite_Set.Impostor := new Impostor.Terrain.Impostor;
      else
         new_sprite_Set.Impostor := new Impostor.Simple.Impostor;
         new_sprite_Set.Impostor.set_size_update_trigger_Delta        (To => 10);
         new_sprite_Set.Impostor.set_freshen_count_update_trigger_Mod (To => 250);
      end if;

      new_sprite_Set.Impostor.set_Target (the_Visual);

      Self.object_sprite_set_Map.Insert (the_Visual, new_sprite_Set);
   end;

   procedure rid (Self : in out Culler;   the_Visual : in GLOBE_3D.p_Visual)
   is
   begin
      free (Self.object_sprite_set_Map.Element (the_Visual));
      Self.object_sprite_set_Map.Delete (the_Visual);
   end;

   function  object_Count (Self : in Culler) return Natural    -- tbd: should use ada.containers.Count_type instead of Natural ?
   is
   begin
      return Natural (Self.object_sprite_set_Map.Length);
   end;

   function vanish_point_size_Min (Self : in     Culler'Class) return Real
   is
   begin
      return Self.vanish_point_size_Min;
   end;

   procedure vanish_point_size_Min_is (Self : in out Culler'Class;   Now : in Real)
   is
   begin
      Self.vanish_point_size_Min := Now;
   end;

   function impostor_size_Min (Self : in     Culler'Class) return Real
   is
   begin
      return Self.impostor_size_Min;
   end;

   procedure impostor_size_Min_is (Self : in out Culler'Class;   Now : in Real)
   is
   begin
      Self.impostor_size_Min := Now;
   end;

   function frustum_culling_Enabled (Self : in     Culler'Class) return Boolean
   is
   begin
      return Self.frustum_culling_Enabled;
   end;

   procedure frustum_culling_Enabled_is (Self : in out Culler'Class;   Now : in Boolean)
   is
   begin
      Self.frustum_culling_Enabled := Now;
   end;

   procedure evolve (Self : in out Culler;
                     By   : in     Real)
   is
      all_Objects         : physics_object_sprite_set_Maps.Map    renames Self.object_sprite_set_Map;
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

      Frustum : constant GL.Frustums.plane_Array := Self.Viewer.Camera.frustum_planes;
   begin

      -- apply 'frustum' and 'apparent size' culling
      --
      while Has_Element (Cursor) loop

         the_Object := Element (Cursor).Visual;

         declare
            use GL.Frustums, GL.Math;
            the_Size      : constant Real := the_Object.Bounds.sphere_Radius;
            the_Distance  : constant Real := Norm (Self.Viewer.Camera.clipper.eye_position - the_Object.centre);
            apparent_Size : constant Real := the_Size / the_Distance;

            function is_visible_for_plane (Which : in GL.Frustums.plane_Id) return Boolean
            is
               the_Site       : Vector_3D renames the_Object.centre;
               plane_Distance : constant Real              :=   Frustum (Which) (0) * the_Site (0)
                                                     + Frustum (Which) (1) * the_Site (1)
                                                     + Frustum (Which) (2) * the_Site (2)
                                                     + Frustum (Which) (3);
            begin
               return plane_Distance + the_Size > 0.0;
            end;

         begin
            if         apparent_Size > Self.vanish_point_size_Min
              and then (        not Self.frustum_culling_Enabled
                        or else (         is_visible_for_plane (Left)
                                 and then is_visible_for_plane (Right)
                                 and then is_visible_for_plane (High)
                                 and then is_visible_for_plane (Low)))
            then
               Last                                 := Last + 1;
               visible_Objects (Last).Visual        := the_Object;
               visible_Objects (Last).sprite_Set    := Element (Self.object_sprite_set_Map, the_Object);
               visible_Objects (Last).apparent_Size := apparent_Size;
            end if;

         end;

         Next (Cursor);
      end loop;

      -- find whether visual or imposter is used, for each object.
      --
      declare
         the_Sprites                : Visual_array (1 .. Last);
         transposed_camera_Attitude : constant Matrix_33               := Transpose (Self.Viewer.Camera.world_rotation);
         new_Last                   : Natural                 := 0;

      begin
         for Each in Self.impostor_load_Slots'Range loop
            Self.impostor_load_Slots (Each).impostors_Count := 0;        -- empty each slot's contents.
         end loop;

         Self.Viewer.enable;                       -- for multi-window operation (tbd: check this is needed)

         for Each in 1 .. Last loop
            declare
               the_Object : visible_Object renames visible_Objects (Each);
            begin
               if the_Object.apparent_Size < Self.impostor_size_Min then   -- use impostor
                  declare
                     impostor_Target   : p_Visual            renames the_Object.sprite_Set.Visual;
                     the_Impostor      : Impostor.p_Impostor renames the_Object.sprite_Set.Impostor;
                  begin
                     declare
                        Impostor_update_required : constant Boolean := the_Impostor.update_Required (Self.Viewer.Camera'Access);
                        Impostor_is_valid        : constant Boolean := the_Impostor.is_Valid;
                        Impostor_never_updated   : constant Boolean := the_Impostor.never_Updated;
                     begin
                        if Impostor_is_valid then

                           if Impostor_update_required then

                              if Impostor_never_updated then
                                 the_Impostor.update (Self.Viewer.Camera'Access,  Self.texture_Pool'Unchecked_Access);     -- do immediate update to generate initial texture.
                              else
                                 declare  -- add impostor to appropriate load balancing slot.
                                    target_face_Count : constant Positive := impostor_Target.Face_Count;

                                    function Slot_Id return Positive is
                                    begin
                                       for Each in Self.impostor_load_Slots.all'Range loop
                                          if target_face_Count <= Self.impostor_load_Slots (Each).max_Faces then
                                             return Each;
                                          end if;
                                       end loop;
                                       raise Program_Error;  -- self.impostor_load_Slots is not valid !   (tbd: use better exception ?)
                                    end;

                                    the_Slot : impostor_load_Balancer.Slot renames Self.impostor_load_Slots (Slot_Id);
                                 begin
                                    the_Slot.impostors_Count                     := the_Slot.impostors_Count + 1;
                                    the_Slot.Impostors (the_Slot.impostors_Count) := the_Impostor;
                                 end;
                              end if;

                           end if;

                           the_Impostor.centre   := the_Object.Visual.centre;
                           the_Impostor.rotation := transposed_camera_Attitude;

                           new_Last               := new_Last + 1;
                           the_Sprites (new_Last) := the_Impostor.all'Access;
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

         for Each in Self.impostor_load_Slots'Range loop
            declare
               the_Slot    : impostor_load_Balancer.Slot renames Self.impostor_load_Slots (Each);
               num_Updates : constant Natural                          := Natural'Min (the_Slot.max_Updates, the_Slot.impostors_Count);

               function "<" (L, R : in Impostor.p_Impostor) return Boolean
               is
               begin
                  return   L.target_camera_Distance - Real (L.frame_Count_since_last_update)  -- subtracting 'frame count' allows distant
                         < R.target_camera_Distance - Real (L.frame_Count_since_last_update); -- targets a chance of update (tbd: need some sort of user-settable scale param to allow for very large scales (space/etc)).
               end "<";

               --procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
               procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
                                                                        Impostor.p_Impostor,
                                                                        Impostor.p_Impostor_array);
            begin
               sort (the_Slot.Impostors (1 .. the_Slot.impostors_Count));

               for Each in 1 .. num_Updates loop
                  the_Slot.Impostors (Each).update (Self.Viewer.Camera'Access, Self.texture_Pool'Unchecked_Access);
                  -- tbd: would 'flush' improve performance here ?
               end loop;
            end;
         end loop;

         Self.Viewer.freshen (time_Step => By,
                              Extras    => the_Sprites (1 .. new_Last));
      end;

      Self.frame_Count := Self.frame_Count + 1;
   end;

   -- sprite_Set
   --

   procedure destroy (Self : in out sprite_Set)
   is
      use Impostor;
   begin
      free (Self.Impostor);
   end;

   procedure free    (Self : in     sprite_Set_view)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (sprite_Set, sprite_Set_view);
      Pad : sprite_Set_view := Self;
   begin
      destroy (Self.all);
      deallocate (Pad);
   end;

end GLOBE_3D.Culler.Impostoring_frustum;
