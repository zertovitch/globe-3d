with GL,
     GL.Textures,
     GL.Skins;                                 use GL, GL.Textures;
with GL.Buffer.Vertex,
     GL.Buffer.Indices,
     GL.Buffer.Texture_coords;
with GL.Geometry.VBO;
with GL.Skinned_Geometry;
--  with GL.IO;

with GLOBE_3D.Math;                            use GLOBE_3D.Math;
with GL.Math;
--  with Ada.Numerics;                             use Ada.Numerics;
--  with Ada.Text_IO;                              use Ada.Text_IO;

package body Terrain.VBO is

   -- tbd: this package uses a 'Sprite', whereas a 'triMesh.vbo' would probably be more appropriate.

   function new_terrain_Sprite return Sprite.p_Sprite
   is
      the_Sprite : constant Sprite.p_Sprite := new GLOBE_3D.Sprite.Sprite (max_Geometries => 1);
   begin
      the_Sprite.is_Terrain := True;
      return the_Sprite;
   end;

   procedure Heights_are (Self : in Sprite.p_Sprite;   Now      : in     height_Map;
                                                       Scale    : in     GLOBE_3D.Vector_3D;
                                                       Y_Offset :    out Real)
   is
      use GL.Geometry, GL.Geometry.VBO, GL.Buffer, GL.Buffer.Vertex, GLOBE_3D.REF, GLOBE_3D.Sprite;

      the_Geometry : constant GL.Geometry.VBO.p_vbo_Geometry := new GL.Geometry.VBO.vbo_Geometry;

      vertex_Count : constant vertex_Id      := vertex_Id (Now.row_Count * Now.column_Count);
      the_Vertices : p_Vertex_array := new GL.Geometry.Vertex_array'(1 .. vertex_Count => <>);

   begin
      the_Geometry.primitive_Id := GL.TRIANGLES;

      -- vertices
      --
      set (the_Vertices.all,   from_height_Map => Now,
                               scale           => Scale,
                               height_Offset   => Y_Offset);
      the_Geometry.Vertices     := to_Buffer (the_Vertices,  Usage => GL.STATIC_DRAW);
      the_Geometry.vertex_Count := GL.Sizei  (vertex_Count);

      the_Geometry.Bounds := Bounds (the_Vertices.all);
      Self.Bounds         := the_Geometry.Bounds;

      free (the_Vertices);      -- nb: using new/free to avoid storage_Error with large heightmaps.

      -- indices
      --
      declare
         use GL.Buffer.Indices;
         index_Count : constant GL.positive_uInt  := GL.positive_uInt ((Now.row_Count - 1) * (Now.column_Count - 1) * 2) * 3;
         the_Indices :          p_vertex_Id_array := new vertex_Id_array (1 .. index_Count);
         Last        :          GL.positive_uInt  := 1;
      begin
         the_Geometry.indices_Count := GL.Sizei (index_Count);

         for Row in 1 .. Now.row_Count - 1 loop
            for Col in 1 .. Now.column_Count - 1 loop
               declare
                  --  use GL.Geometry;
                  NW : constant vertex_Id := Vertex_Id_for (Now,   Row,     Col    );
                  SW : constant vertex_Id := Vertex_Id_for (Now,   Row + 1, Col    );
                  NE : constant vertex_Id := Vertex_Id_for (Now,   Row,     Col + 1);
                  SE : constant vertex_Id := Vertex_Id_for (Now,   Row + 1, Col + 1);
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

         pragma Assert (Last - 1 = index_Count);

         the_Geometry.Indices := to_Buffer (the_Indices,  Usage => GL.STATIC_DRAW);

         free (the_Indices);
      end;

      Self.skinned_Geometries (1).Geometry := the_Geometry.all'Access;
      Self.skinned_geometry_Count         := 1;
   end;

   procedure Texture_is (Self : in Sprite.p_Sprite;   Now                 : in     GL.Textures.Object;
                                                      texture_Transform_s : in     GL.Textures.texture_Transform;
                                                      texture_Transform_t : in     GL.Textures.texture_Transform)
   is
      use GL.Skins, GL.Geometry.VBO;

      the_skinned_Geometry : GL.Skinned_Geometry.Skinned_Geometry renames Self.skinned_Geometries (1);

      the_Skin       : constant p_Skin_unlit_textured_vbo         := new Skin_unlit_textured_vbo'(Texture => Now);
      the_Vertices   : GL.Geometry.Vertex_array     renames vbo_Geometry (the_skinned_Geometry.Geometry.all).Vertices.get;
      texture_Coords : GL.Textures.p_Coordinate_2D_array := to_texture_Coordinates_xz (the_Vertices, texture_Transform_s,
                                                                                                     texture_Transform_t);
   begin
      the_skinned_Geometry.Skin   := the_Skin.all'Access;
      the_skinned_Geometry.Veneer := the_Skin.all.new_Veneer (for_Geometry => the_skinned_Geometry.Geometry.all);

      declare
         use GL.Buffer.Texture_coords;
         the_Veneer : GL.Skins.Veneer_unlit_textured_vbo'Class renames GL.Skins.Veneer_unlit_textured_vbo'Class (the_skinned_Geometry.Veneer.all);
      begin
         the_Veneer.texture_Coordinates := to_Buffer (texture_Coords, Usage => GL.STATIC_DRAW);
      end;

      free (texture_Coords);
   end;

   procedure create (Object              :    out GLOBE_3D.Sprite.p_Sprite;
                     the_height_Map      : in     height_Map;
                     scale               : in     GLOBE_3D.Vector_3D;
                     the_Texture         : in     GL.Textures.Object;
                     texture_Transform_s : in     GL.Textures.texture_Transform;
                     texture_Transform_t : in     GL.Textures.texture_Transform;
                     Y_Offset            :    out Real)
   is
      use GL.Geometry, GL.Geometry.VBO, GL.Buffer, GL.Buffer.Vertex, GLOBE_3D.REF, GLOBE_3D.Sprite;

   begin
      Object            := new GLOBE_3D.Sprite.Sprite (max_Geometries => 1);
      Object.is_Terrain := True;

      Heights_are (Object, the_height_Map, scale, Y_Offset);
      Texture_is  (Object, the_Texture, texture_Transform_s, texture_Transform_t);
   end create;

   procedure Create (Object         : in out GLOBE_3D.Sprite.p_Sprite;
                     png_Heights    : in     String;
                     Scale          : in     GLOBE_3D.Vector_3D;
                     base_Texture   : in     String)
   is
      use GLOBE_3D.REF;

      the_height_Map : constant height_Map                    := to_Height_Map (to_Matrix (png_Heights));

      Width          : constant Real                          := Real (the_height_Map.column_Count - 1) * Scale (0);
      Depth          : constant Real                          := Real (the_height_Map.row_Count    - 1) * Scale (2);

      ground_Texture : constant GL.Textures.Object            := new_Texture (image_Filename => base_Texture);
      transform_s    : constant GL.Textures.texture_Transform := (Offset => 0.5 * Scale (0)  +  Width / 2.0,
                                                         Scale  => 1.0 / Width                     );
      transform_t    : constant GL.Textures.texture_Transform := (Offset => 0.5 * Scale (2)  +  Depth / 2.0,
                                                         Scale  => 1.0 / Depth                     );
      Y_Offset : Real;

   begin
      create (Object, the_height_Map, Scale, ground_Texture, transform_s, transform_t,  Y_Offset);
   end Create;

   function Create (tga_Heights   : in     String;
                    texture_Image : in     String;
                    tile_Width    : in     Positive  := 32;
                    tile_Depth    : in     Positive  := 32;
                    base_Centre   : in     Vector_3D := (0.0, 0.0, 0.0);
                    Scale         : in     Vector_3D := (1.0, 1.0, 1.0)) return Sprite.p_sprite_Grid
   is
      use GLOBE_3D.Sprite;
      the_Matrix : constant Matrix := to_Matrix (tga_Heights);

      total_Width    : constant Real       := Real (the_Matrix'Length (2) - 1);
      total_Depth    : constant Real       := Real (the_Matrix'Length (1) - 1);

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

      the_sprite_Grid     : p_sprite_Grid (the_heightmap_Grid'Range (1),
                                           the_heightmap_Grid'Range (2));
      ground_Texture      : constant GL.Textures.Object := new_Texture (image_Filename => texture_Image);

   begin
      -- create each element heightmap for the_heightmap_Grid.
      --
      declare
         row_First, row_Last,
         col_First, col_Last  : Integer; -- row and col ranges for each submatrix.
      begin
         for Row in the_sprite_Grid'Range (1) loop
            row_First := (tile_Depth - 1) * (Row - 1) + 1;
            row_Last  := Integer'Min (row_First + (tile_Depth - 1),  the_Matrix'Last (1));

            for Col in the_sprite_Grid'Range (2) loop
               col_First := (tile_Width - 1) * (Col - 1) + 1;
               col_Last  := Integer'Min (col_First + (tile_Width - 1),  the_Matrix'Last (2));

               the_heightmap_Grid (Row, Col) := new height_Map' (to_Height_Map (sub_Matrix (the_Matrix, row_First, row_Last,
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
                       texture_Transform_s => (Offset => (0.5 + Real (tile_Width - 1) / 2.0) * Scale (0)  +  site_X_offset,
                                               Scale  => 1.0 / ((total_Width) * Scale (0))),
                       texture_Transform_t => (Offset => (0.5 + Real (tile_Depth - 1) / 2.0) * Scale (2)  +  site_Z_offset,
                                               Scale  => 1.0 / ((total_Depth) * Scale (2))),
                       Y_Offset            => site_Y_Offset);

               declare
                  use GL.Math;
                  the_Site : constant Vector_3D  :=
                    (site_X_offset - (total_Width / 2.0) * Scale (0),
                     site_Y_Offset,
                     site_Z_offset - (total_Depth / 2.0) * Scale (2));
               begin
                  the_sprite_Grid (Row, Col).centre := the_Site + base_Centre;
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

end Terrain.VBO;
