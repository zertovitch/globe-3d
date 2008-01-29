with GL,
     gl.Textures,
     gl.Skins;                                 use GL, gl.Textures;
with gl.Buffer.vertex,
     gl.Buffer.indices,
     gl.Buffer.texture_coords;
with gl.Geometry.VBO;
with gl.skinned_Geometry;
with gl.IO;

with GLOBE_3D.Math;                            use GLOBE_3D.Math;


with Ada.Numerics;                             use Ada.Numerics;
with ada.text_io;                              use ada.text_io;



package body Terrain.vbo is


   -- tbd: this package uses a 'Sprite', whereas a 'triMesh.vbo' would probably be more appropriate.






   function new_terrain_Sprite return sprite.p_Sprite
   is
      the_Sprite : sprite.p_Sprite := new globe_3d.sprite.Sprite (max_geometrys => 1);
   begin
      the_Sprite.is_Terrain := True;
      return the_Sprite;
   end;





   procedure Heights_are (Self : in sprite.p_Sprite;   Now      : in     height_Map;
                                                       Scale    : in     GLOBE_3D.Vector_3D;
                                                       Y_Offset :    out Real)
   is
      use GLOBE_3D, GL, GL.Geometry, GL.Geometry.vbo, GL.Buffer, GL.Buffer.vertex, GLOBE_3D.REF, GLOBE_3D.Math, globe_3d.Sprite;

      the_Geometry : gl.geometry.vbo.p_vbo_Geometry := new gl.geometry.vbo.vbo_Geometry;

      vertex_Count : vertex_Id      := vertex_Id (Now.row_Count * Now.column_Count);
      the_Vertices : p_Vertex_array := new gl.geometry.vertex_array'(1 .. vertex_Count => <>);

   begin
      the_Geometry.primitive_Id := gl.TRIANGLES;

      -- vertices
      --
      set (the_Vertices.all,   from_height_map => Now,
                               scale           => scale,
                               height_offset   => Y_Offset);
      the_Geometry.Vertices     := to_Buffer (the_Vertices,  usage => GL.STATIC_DRAW);
      the_Geometry.vertex_Count := gl.SizeI  (vertex_Count);

      the_Geometry.Bounds := Bounds (the_Vertices.all);
      self.Bounds         := the_Geometry.Bounds;

      free (the_Vertices);      -- nb: using new/free to avoid storage_Error with large heightmaps.


      -- indices
      --
      declare
         use gl.Buffer.indices;
         index_Count : constant gl.positive_uInt  := gl.positive_uInt ((Now.row_Count - 1) * (Now.column_Count - 1) * 2) * 3;
         the_Indices :          p_vertex_id_Array := new vertex_id_Array (1 .. index_Count);
         Last        :          gl.positive_uInt  := 1;
      begin
         the_Geometry.indices_Count := gl.SizeI (index_Count);

         for Row in 1 .. Now.row_Count - 1 loop
            for Col in 1 .. Now.column_Count - 1 loop
               declare
                  use gl.Geometry;
                  NW : vertex_Id := Vertex_Id_for (Now,   Row,     Col    );
                  SW : vertex_Id := Vertex_Id_for (Now,   Row + 1, Col    );
                  NE : vertex_Id := Vertex_Id_for (Now,   Row,     Col + 1);
                  SE : vertex_Id := Vertex_Id_for (Now,   Row + 1, Col + 1);
               begin
                                      the_Indices (Last) := NW - 1;
                  Last := Last + 1;   the_Indices (Last) := SW - 1;
                  Last := Last + 1;   the_Indices (Last) := NE - 1;

                  Last := Last + 1;   the_Indices (Last) := NE - 1;
                  Last := Last + 1;   the_Indices (Last) := SW - 1;
                  Last := Last + 1;   the_Indices (Last) := SE - 1;
                  Last := Last + 1;
               end;
            end loop;
         end loop;

         pragma assert (Last - 1 = Index_Count);

         the_Geometry.Indices := to_Buffer (the_Indices,  usage => GL.STATIC_DRAW);

         free (the_Indices);
      end;


      self.skinned_Geometrys (1).Geometry := the_Geometry.all'access;
      self.skinned_geometry_Count         := 1;
   end;






   procedure Texture_is (Self : in sprite.p_Sprite;   Now                 : in     gl.textures.Object;
                                                      texture_Transform_s : in     gl.textures.texture_Transform;
                                                      texture_Transform_t : in     gl.textures.texture_Transform)
   is
      use gl.Textures, gl.Skins, gl.Geometry.vbo;

      the_skinned_Geometry : gl.skinned_Geometry.skinned_Geometry renames self.skinned_geometrys (1);

      the_Skin       : p_Skin_unlit_textured_vbo         := new Skin_unlit_textured_vbo'(texture => Now);
      the_Vertices   : gl.geometry.vertex_Array     renames vbo_Geometry (the_skinned_Geometry.Geometry.all).Vertices.get;
      texture_Coords : gl.textures.p_Coordinate_2D_array := to_texture_Coordinates_xz (the_Vertices, texture_Transform_s,
                                                                                                     texture_Transform_t);
   begin
      the_skinned_Geometry.Skin   := the_Skin.all'access;
      the_skinned_Geometry.Veneer := the_Skin.all.new_Veneer (for_geometry => the_skinned_Geometry.Geometry.all);

      declare
         use gl.Buffer.texture_coords;
         the_Veneer : gl.skins.Veneer_unlit_textured_vbo'Class renames gl.skins.Veneer_unlit_textured_vbo'Class (the_skinned_Geometry.Veneer.all);
      begin
         the_Veneer.texture_Coordinates := to_Buffer (texture_Coords, usage => GL.STATIC_DRAW);
      end;

      free (texture_Coords);
   end;






   procedure create (Object              :    out globe_3d.sprite.p_Sprite;
                     the_height_Map      : in     height_Map;
                     scale               : in     GLOBE_3D.Vector_3D;
                     the_Texture         : in     gl.textures.Object;
                     texture_Transform_s : in     gl.textures.texture_Transform;
                     texture_Transform_t : in     gl.textures.texture_Transform;
                     Y_Offset            :    out Real)
   is
      use GLOBE_3D, GL, GL.Geometry, GL.Geometry.vbo, GL.Buffer, GL.Buffer.vertex, GLOBE_3D.REF, GLOBE_3D.Math, globe_3d.Sprite;

   begin
      Object            := new globe_3d.sprite.Sprite (max_geometrys => 1);
      Object.is_Terrain := True;

      Heights_are (Object, the_Height_Map, Scale, Y_Offset);
      Texture_is  (Object, the_Texture, texture_Transform_s, texture_Transform_t);
   end create;






   procedure Create (object         : in out GLOBE_3D.sprite.p_Sprite;
                     png_Heights    : in     String;
                     scale          : in     GLOBE_3D.Vector_3D;
                     base_Texture   : in     String)
   is
      use GLOBE_3D, GL, GLOBE_3D.REF, GLOBE_3D.Math;

      the_height_Map : height_Map                    := to_Height_Map (to_Matrix (png_Heights));

      Width          : Real                          := Real (the_height_Map.column_Count - 1) * Scale (0);
      Depth          : Real                          := Real (the_height_Map.row_Count    - 1) * Scale (2);

      ground_Texture : gl.textures.Object            := new_Texture (image_Filename => base_Texture);
      transform_s    : gl.textures.texture_Transform := (offset => 0.5 * Scale (0)  +  Width / 2.0,
                                                         scale  => 1.0 / Width                     );
      transform_t    : gl.textures.texture_Transform := (offset => 0.5 * Scale (2)  +  Depth / 2.0,
                                                         scale  => 1.0 / Depth                     );
      Y_Offset : Real;

   begin
      create (object, the_Height_Map, Scale, ground_Texture, Transform_s, Transform_t,  Y_Offset);
   end Create;





   function Create (tga_Heights   : in     String;
                    texture_Image : in     String;
                    tile_Width    : in     Positive  := 32;
                    tile_Depth    : in     Positive  := 32;
                    base_Centre   : in     Vector_3D := (0.0, 0.0, 0.0);
                    Scale         : in     Vector_3D := (1.0, 1.0, 1.0)) return sprite.p_sprite_Grid
   is
      use globe_3d.Sprite;
      the_Matrix : Matrix := to_Matrix (tga_Heights);

      total_Width    : Real       := Real (the_Matrix'Length (2) - 1);
      total_Depth    : Real       := Real (the_Matrix'Length (1) - 1);


      function Grid_last (total_Size, tile_Size : in Positive) return Positive
      is
         Last : Positive := 1  +  (total_Size - tile_Size) / (tile_Size - 1);
      begin
         if (total_Size - tile_Size) mod (tile_Size - 1) /= 0 then
            Last := Last + 1;
         end if;
         return Last;
      end;

      the_heightmap_Grid  : height_map_Grid (1 .. Grid_last (the_Matrix'Length (1), tile_Depth),
                                             1 .. Grid_last (the_Matrix'Length (2), tile_Width));

      the_sprite_Grid     : p_sprite_Grid (the_heightmap_Grid'range (1),
                                           the_heightmap_Grid'range (2));
      ground_Texture      : gl.textures.Object := new_Texture (image_Filename => texture_Image);

   begin
      -- create each element heightmap for the_heightmap_Grid.
      --
      declare
         row_First, row_Last,
         col_First, col_Last  : Integer; -- row and col ranges for each submatrix.
      begin
         for Row in the_sprite_Grid'Range (1) loop
            row_First := (tile_Depth - 1) * (Row - 1) + 1;
            row_Last  := integer'Min (row_First + (tile_Depth - 1),  the_Matrix'Last (1));

            for Col in the_sprite_Grid'Range (2) loop
               col_First := (tile_Width - 1) * (Col - 1) + 1;
               col_Last  := integer'Min (col_First + (tile_Width - 1),  the_Matrix'Last (2));

               the_heightmap_Grid (Row, Col) := new height_Map' (to_height_Map (sub_Matrix (the_Matrix, row_First, row_Last,
                                                                                                        col_First, col_Last)));
            end loop;
         end loop;
      end;

      -- create the Sprite for each grid element
      --
      declare
         site_X_offset,
         site_Z_offset : Real := 0.0;
         site_Y_Offset : Real;
      begin
         for Row in the_sprite_Grid'Range (1) loop
            site_X_offset := 0.0;

            for Col in the_sprite_Grid'Range (2) loop
               create (the_sprite_Grid (Row, Col),
                       the_heightmap_Grid (Row, Col).all,
                       Scale,
                       ground_Texture,
                       texture_transform_s => (offset => (0.5 + Real (tile_Width - 1) / 2.0) * Scale (0)  +  site_X_offset,
                                               scale  => 1.0 / ((total_Width) * Scale (0))),
                       texture_transform_t => (offset => (0.5 + Real (tile_Depth - 1) / 2.0) * Scale (2)  +  site_Z_offset,
                                               scale  => 1.0 / ((total_Depth) * Scale (2))),
                       y_offset            => site_Y_Offset);

               declare
                  the_Site : vector_3d  := (site_X_offset - (total_Width / 2.0) * Scale (0),
                                            site_Y_Offset,
                                            site_Z_offset - (total_Depth / 2.0) * Scale (2));
               begin
                  the_sprite_Grid (Row, Col).Centre := the_Site + base_Centre;
               end;


               if Col /= the_sprite_Grid'Last (2) then
                  site_X_offset := site_X_offset + Width (the_heightmap_Grid (Row, Col    ).all) * Scale (0) / 2.0
                                                 + Width (the_heightmap_Grid (Row, Col + 1).all) * Scale (0) / 2.0;
               end if;
            end loop;


            if Row /= the_sprite_Grid'Last (1) then
               site_Z_offset := site_Z_offset + Depth (the_heightmap_Grid (Row,     1).all) * Scale (2) / 2.0
                                              + Depth (the_heightmap_Grid (Row + 1, 1).all) * Scale (2) / 2.0;
            end if;
         end loop;
      end;


      -- clean up
      --
      for Row in the_sprite_Grid'Range (1) loop
         for Col in the_sprite_Grid'Range (2) loop
            free (the_heightmap_Grid (Row, Col));
         end loop;
      end loop;


      return the_sprite_Grid;
   end Create;



end Terrain.vbo;
