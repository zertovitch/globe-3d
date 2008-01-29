with GLOBE_3D.tri_Mesh;
with GLOBE_3D.Sprite;

with gl.Textures;

with ada.unchecked_Deallocation;



package Terrain.vbo is

   use Globe_3d;


   -- sprite (uses vertex buffer objects)
   --

   procedure Create (Object      : in out sprite.p_Sprite;
                     png_Heights : in     String;                             -- filename of the PNG heightmap image
                     Scale       : in     Vector_3D;
                     base_Texture : in     String                              -- filename of the ground texture.
                    );


   function Create (tga_Heights   : in     String;
                    texture_Image : in     String;
                    tile_Width    : in     Positive  := 32;
                    tile_Depth    : in     Positive  := 32;
                    base_Centre   : in     Vector_3D := (0.0, 0.0, 0.0);       -- Central point of the bottom of the terrain.
                    Scale         : in     Vector_3D := (1.0, 1.0, 1.0)) return sprite.p_sprite_Grid;






   -------------------------------------


   function new_terrain_Sprite return sprite.p_Sprite;






end Terrain.vbo;
