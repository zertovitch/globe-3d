with GLOBE_3D.tri_Mesh;



package Terrain.simple is  -- tbd: rename 'Terrain_trimesh' !

   use Globe_3d;


   -- tbd: currently broken !!

   procedure Create (Object      : in out tri_mesh.p_tri_Mesh;
                     png_Heights : in     String;                             -- filename of the PNG heightmap image
                     Scale       : in     Vector_3D);


   function  Create (png_Heights   : in     String;                           -- filename of the PNG heightmap image
                     texture_Image : in     String;                           -- filename of the BMP or TGA texture image
                     tile_Width    : in     Positive  := 32;
                     tile_Depth    : in     Positive  := 32;
                     base_Centre   : in     Vector_3D := (0.0, 0.0, 0.0);     -- position of centre of terrain, at height '0.0'.
                     Scale         : in     Vector_3D := (1.0, 1.0, 1.0)) return tri_mesh.p_tri_Mesh_Grid;


end Terrain.simple;
