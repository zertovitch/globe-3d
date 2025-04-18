-------------------------------------------------------------------------
--  GLOBE_3D.Culler.Impostoring_frustum
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GLOBE_3D.Impostor;
with GL.Textures;

with Ada.Containers.Hashed_Maps;

package GLOBE_3D.Culler.Impostoring_frustum is

   type Culler   is new GLOBE_3D.Culler.Culler with private;
   type p_Culler is access all Culler'Class;

   overriding
   procedure add (Self : in out Culler;   the_Visual : in Skinned_Visuals.p_Skinned_Visual);
   overriding
   procedure rid (Self : in out Culler;   the_Visual : in Skinned_Visuals.p_Skinned_Visual);

   overriding
   function  object_Count (Self : in     Culler) return Natural;
   overriding
   procedure evolve       (Self : in out Culler;   By : in     Real);    -- tbd: rename 'freshen' ?

   function  vanish_point_size_Min    (Self : in     Culler'Class) return Real;
   procedure vanish_point_size_Min_is (Self : in out Culler'Class;   Now : in Real);
   --
   --  visuals whose projected size falls below this minimum will not be displayed.

   function  impostor_size_Min    (Self : in     Culler'Class) return Real;
   procedure impostor_size_Min_is (Self : in out Culler'Class;   Now : in Real);
   --
   --  visuals whose projected size falls below this minimum will be displayed as impostors.

   function  frustum_culling_Enabled    (Self : in     Culler'Class) return Boolean;
   procedure frustum_culling_Enabled_is (Self : in out Culler'Class;   Now : in Boolean);

private

   type sprite_Set is tagged
      record
         Visual   : Skinned_Visuals.p_Skinned_Visual;
         Impostor : GLOBE_3D.Impostor.p_Impostor;
      end record;

   type sprite_Set_view is access all sprite_Set;
   type sprite_Set_views is array (Positive range <>) of sprite_Set_view;

   procedure destroy (Self : in out sprite_Set);
   procedure free    (Self : in     sprite_Set_view);

   function Hash (Self : in Skinned_Visuals.p_Skinned_Visual) return Ada.Containers.Hash_Type;

   package physics_object_sprite_set_Maps is new ada.containers.hashed_Maps
     (Skinned_Visuals.p_Skinned_Visual,
      sprite_Set_view,
      hash            => Hash,
      equivalent_keys => Skinned_Visuals."=");
   use physics_object_sprite_set_Maps;

   package impostor_load_Balancer is

      type Slot is
         record
            max_Faces     : Positive;
            max_Updates   : Positive;

            Impostors       : Impostor.p_Impostor_array (1 .. 10_000);
            impostors_Count : Natural                              := 0;
         end record;

      type Slots is array (Positive range <>) of Slot;
      type p_Slots is access all Slots;

   end impostor_load_Balancer;

   default_Slots : aliased impostor_load_Balancer.Slots := (1 => (max_Faces =>  100,           max_Updates =>  20,  others => <>),
                                                            2 => (max_Faces => 1000,           max_Updates =>  15,  others => <>),
                                                            3 => (max_Faces => Positive'Last,  max_Updates =>  12,  others => <>));
                                                            --
                                                            --  tbd: tune default_Slots to reasonable defaults.

   type Culler is new GLOBE_3D.Culler.Culler with
      record
         countDown               :         Natural                           := 0;
         frame_Count             :         Natural                           := 0;

         vanish_point_size_Min   :         Real                              := 0.0012;
         impostor_size_Min       :         Real                              := 0.0625;
         frustum_culling_Enabled :         Boolean                           := True;

         object_sprite_set_Map   :         physics_object_sprite_set_Maps.Map;
         impostor_load_Slots     :         impostor_load_Balancer.p_Slots    := default_Slots'Access;

         texture_Pool            : aliased GL.Textures.Pool;
      end record;

end GLOBE_3D.Culler.Impostoring_frustum;
