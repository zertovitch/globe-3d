-------------------------------------------------------------------------
--  GL.Skins - appearance of the surfaces of geometry primitives.
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Geometry,
     GL.Textures,
     GL.Materials;

package GL.Skins is

   type Veneer is abstract tagged null record;   -- Contains skin data specific to a particular geometric primitive.

   type p_Veneer is access all Veneer'Class;

   procedure destroy (Self : in out Veneer);
   procedure free    (Self : in out p_Veneer);

   procedure enable (Self : in out Veneer)   is abstract;

   -- 'Skin': base of skin subclasses.

   type Skin is abstract tagged
      record
         null;
      end record;

   type p_Skin is access all Skin'Class;
   type Skins  is array (Positive range <>) of p_Skin;

   procedure destroy (Self : in out Skin);
   procedure free    (Self : in out p_Skin);

   function  new_Veneer (Self : in     Skin;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer   is abstract;

   procedure enable         (Self : in out Skin)                  is abstract;
   function  is_Transparent (Self : in     Skin) return Boolean   is abstract;

   null_Skins : constant Skins (1 .. 0) := (others => null);

   -- Skin: opaque unlit mono_color
   --

   type Skin_opaque_unlit_mono_color is new Skin with
      record
         Color : RGB_Color;
      end record;

   overriding
   function  new_Veneer     (Self : in     Skin_opaque_unlit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer;
   overriding
   procedure enable         (Self : in out Skin_opaque_unlit_mono_color);
   overriding
   function  is_Transparent (Self : in     Skin_opaque_unlit_mono_color) return Boolean;

   -- Skin: opaque lit mono_color
   --

   type Veneer_opaque_lit_mono_color (max_Normals : GL.Geometry.vertex_Id) is new Veneer with
      record
         Normals : GL.Geometry.Normal_array (1 .. max_Normals);
      end record;

   overriding
   procedure enable (Self : in out Veneer_opaque_lit_mono_color);

   type Skin_opaque_lit_mono_color is new Skin with
      record
         Material : GL.Materials.Material_type := Materials.neutral_material;
      end record;

   overriding
   function  new_Veneer (Self : in     Skin_opaque_lit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer;
   overriding
   procedure enable     (Self : in out Skin_opaque_lit_mono_color);

   overriding
   function  is_Transparent (Self : in     Skin_opaque_lit_mono_color) return Boolean;

   -- Skin: transparent unlit textured (used by 'impostor's)  -- tbd: get rid of 'transparent' since might not be !
   --

   type Veneer_transparent_unlit_textured (num_Coordinates : GL.Geometry.vertex_Id) is new Veneer with
      record
         texture_Coordinates : GL.Textures.Coordinate_2D_array (1 .. num_Coordinates);
      end record;

   type p_Veneer_transparent_unlit_textured is access all Veneer_transparent_unlit_textured'Class;

   overriding
   procedure enable (Self : in out Veneer_transparent_unlit_textured);

   type Skin_transparent_unlit_textured is new Skin with
      record
         Texture              : GL.Textures.Object;
         coordinate_Generator : GL.Textures.p_coordinate_Generator;
      end record;

   type p_Skin_transparent_unlit_textured is access all Skin_transparent_unlit_textured;

   overriding
   procedure destroy (Self : in out Skin_transparent_unlit_textured);

   overriding
   function  new_Veneer     (Self : in     Skin_transparent_unlit_textured;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer;
   overriding
   procedure enable         (Self : in out Skin_transparent_unlit_textured);
   overriding
   function  is_Transparent (Self : in     Skin_transparent_unlit_textured) return Boolean;

   -- ... other common skin specialisations ...
   -- ...

   -- standard skins
   --

   green_Skin     : p_Skin := new GL.Skins.Skin_opaque_unlit_mono_color'(Color => (red   => 1.0,
                                                                                   green => 1.0,
                                                                                   blue  => 1.0));

   lit_green_Skin : p_Skin := new GL.Skins.Skin_opaque_lit_mono_color;  -- tbd: set to a green colour (defaults to neutral grey atm :)

   -- ... other standard skins

end GL.Skins;
