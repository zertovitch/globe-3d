-------------------------------------------------------------------------
--  GL.Skins - appearance of the surfaces of geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with gl.Geometry, gl.Textures;
with gl.Buffer.texture_coords;
with gl.Buffer.normals;



package gl.Skins is



   type Veneer is abstract tagged null record;   -- contains skin data specific to a particular geometric primitive.

   type p_Veneer is access all Veneer'Class;


   procedure destroy (Self : in out Veneer);
   procedure free    (Self : in out p_Veneer);

   procedure enable (Self : in out Veneer)   is abstract;




   -- 'Skin': base of skin class


   type Skin is abstract tagged
      record
         null;
      end record;

   type p_Skin is access all Skin'Class;
   type Skins is array (Positive range <>) of p_Skin;


   procedure destroy (Self : in out Skin);
   procedure free    (Self : in out p_Skin);


   function  new_Veneer (Self : in     Skin;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer   is abstract;

   procedure enable         (Self : in out Skin)                  is abstract;
   function  is_Transparent (Self : in     Skin) return Boolean   is abstract;



   null_Skins : constant Skins (1 .. 0) := (others => null);





   -- Skin: opaque unlit mono_color
   --

   type Skin_opaque_unlit_mono_color is new Skin with
      record
         Color : RGB_Color;
      end record;

   function  new_Veneer     (Self : in     Skin_opaque_unlit_mono_color;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer;
   procedure enable         (Self : in out Skin_opaque_unlit_mono_color);
   function  is_Transparent (Self : in     Skin_opaque_unlit_mono_color) return Boolean;





   -- Skin: opaque lit mono_color
   --

   type Veneer_opaque_lit_mono_color (max_Normals : gl.geometry.vertex_Id) is new Veneer with
      record
         Normals : gl.geometry.Normal_array (1 .. max_Normals);
      end record;

   procedure enable (Self : in out Veneer_opaque_lit_mono_color);



   type Material_type is record               -- tbd: factor out into own package.
      ambient,
      diffuse,
      specular,
      emission  : GL.Material_Float_vector;
      shininess : GL.Float; -- 0.0 .. 128.0
   end record;

   neutral_material : constant Material_type:= (ambient =>        (0.2, 0.2, 0.2, 1.0),
                                                diffuse =>        (0.8, 0.8, 0.8, 1.0),
                                                specular =>       (0.0, 0.0, 0.0, 1.0),
                                                emission =>       (0.0, 0.0, 0.0, 1.0),
                                                shininess =>      0.0);                 -- ^ the values are GL defaults.

   function  is_Transparent (Self : in     Material_type) return Boolean;





   type Skin_opaque_lit_mono_color is new Skin with
      record
         Material : Material_type := neutral_material;
      end record;

   function  new_Veneer (Self : in     Skin_opaque_lit_mono_color;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer;
   procedure enable     (Self : in out Skin_opaque_lit_mono_color);

   function  is_Transparent (Self : in     Skin_opaque_lit_mono_color) return Boolean;




   -- Skin: transparent unlit textured (used by 'impostor's)  -- tbd: get rid of 'transparent' since might not be !
   --


   type Veneer_transparent_unlit_textured (num_Coordinates : gl.geometry.vertex_Id) is new Veneer with
      record
         texture_Coordinates : gl.textures.Coordinate_2D_array (1 .. num_Coordinates);
      end record;

   type p_Veneer_transparent_unlit_textured is access all Veneer_transparent_unlit_textured'Class;

   procedure enable (Self : in out Veneer_transparent_unlit_textured);





   type Skin_transparent_unlit_textured is new Skin with
      record
         Texture              : gl.Textures.Object;
         coordinate_Generator : gl.textures.p_coordinate_Generator;
      end record;

   type p_Skin_transparent_unlit_textured is access all Skin_transparent_unlit_textured;


   procedure destroy (Self : in out Skin_transparent_unlit_textured);

   function  new_Veneer     (Self : in     Skin_transparent_unlit_textured;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer;
   procedure enable         (Self : in out Skin_transparent_unlit_textured);
   function  is_Transparent (Self : in     Skin_transparent_unlit_textured) return Boolean;




   -- Skin: unlit textured vbo
   --


   type Veneer_unlit_textured_vbo is new Veneer with
      record
         --texture_Coordinates : gl.Buffer.vertex_buffer_Object;
         texture_Coordinates : gl.Buffer.texture_coords.Object;
      end record;

   type p_Veneer_unlit_textured_vbo is access all Veneer_unlit_textured_vbo'Class;


   procedure enable (Self : in out Veneer_unlit_textured_vbo);


   -- tbd: 'destroy' for veneers !




   type Skin_unlit_textured_vbo is new Skin with
      record
         Texture : gl.Textures.Object;
      end record;

   type p_Skin_unlit_textured_vbo is access all Skin_unlit_textured_vbo;


   procedure destroy (Self : in out Skin_unlit_textured_vbo);

   function  new_Veneer (Self : in     Skin_unlit_textured_vbo;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer;
   procedure enable     (Self : in out Skin_unlit_textured_vbo);


   function  is_Transparent (Self : in     Skin_unlit_textured_vbo) return Boolean;



   -- ... other common skin specialisations ...
   -- ...





   -- standard skins
   --

   green_Skin     : p_Skin := new gl.skins.Skin_opaque_unlit_mono_color'(color => (red   => 1.0,
                                                                                   green => 1.0,
                                                                                   blue  => 1.0));

   lit_green_Skin : p_Skin := new gl.skins.Skin_opaque_lit_mono_color;  -- tbd: set to a green colour (defaults to neutral grey atm :)


   -- ... other standard skins


end gl.Skins;


-- tbd: use consistent naming for Max_* vs Num_*
