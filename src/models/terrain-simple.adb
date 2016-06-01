--
--  NB: this package is broken so far !!
--

with GL, GL.Textures; use GL;
with GLOBE_3D.Math;   use GLOBE_3D.Math;
with GLOBE_3D.tri_Mesh.vertex_array;

with Ada.Numerics;    use Ada.Numerics;
with Ada.Text_IO;     use Ada.Text_IO;

package body Terrain.Simple is

   function to_tri_Mesh (the_height_Map : in     height_Map;
                         scale          : in     GLOBE_3D.Vector_3D) return tri_Mesh.p_tri_Mesh
   is
      use GLOBE_3D.REF, GLOBE_3D.tri_Mesh;

      object        : p_tri_Mesh := new GLOBE_3D.tri_Mesh.vertex_array.tri_Mesh '(max_points => the_height_Map.Width       * the_height_Map.Depth,
                                                                                  max_faces  => (the_height_Map.Width - 1) * (the_height_Map.Depth - 1) * 2,
                                                                                  others => <>);
      Face_last     : Natural             := 0;
      height_Offset : Real                := the_height_Map.min_Height + (the_height_Map.max_Height - the_height_Map.min_Height) / 2.0;
      MidPoint      : Vector_3D           := ((Real (the_height_Map.Width - 1) / 2.0),
                                              height_Offset,
                                              (Real (the_height_Map.Depth - 1) / 2.0));

      procedure apply_scaling (the_Vertex : in out Vector_3D) is
      begin
         the_Vertex (0) := the_Vertex (0) * scale (0);
         the_Vertex (1) := the_Vertex (1) * scale (1);
         the_Vertex (2) := the_Vertex (2) * scale (2);
      end apply_scaling;

      function Vertex_Id_for (Row, Col : in Positive) return Positive is
      begin
         return (Row - 1) * the_height_Map.Width  +  Col;
      end Vertex_Id_for;

      Skirt : constant := 0.007;  -- tbd: make configurable ?

   begin
      object.Skin          := texture_only;
      object.alpha         := 1.0;
      object.height_Offset := scale (1) * height_Offset;

      for Row in the_height_Map.Heights'Range (1) loop
         for Col in the_height_Map.Heights'Range (2) loop
            declare
               the_Point : Vector_3D renames object.Point (Vertex_Id_for (Row, Col));
            begin
               the_Point :=   (Real (Col) - 1.0,          -- '- 1.0' adjusts for '1 based' indexing.
                               the_height_Map.Heights (Row, Col),
                               Real (Row) - 1.0)
                            -  MidPoint;

               apply_scaling (the_Point);

               if    Row = 1                                   then the_Point (2) := the_Point (2) - Skirt;
               elsif Row = the_height_Map.Heights'Last (1) - 1 then the_Point (2) := the_Point (2) + Skirt;
               end if;

               if    Col = 1                                   then the_Point (0) := the_Point (0) - Skirt;
               elsif Col = the_height_Map.Heights'Last (2) - 1 then the_Point (0) := the_Point (0) + Skirt;
               end if;
            end;
         end loop;
      end loop;

      for Row in 1 .. the_height_Map.Depth - 1 loop
         for Col in 1 .. the_height_Map.Width - 1 loop
            declare
               NW : Positive := Vertex_Id_for (Row,     Col);
               SW : Positive := Vertex_Id_for (Row + 1, Col);
               NE : Positive := Vertex_Id_for (Row,     Col + 1);
               SE : Positive := Vertex_Id_for (Row + 1, Col + 1);
            begin
               Face_last := Face_last + 1;
               declare
                  the_face_Indices : GLOBE_3D.Index_array renames object.face_Indices (Face_last);
               begin
                  the_face_Indices (1) := NW;
                  the_face_Indices (2) := SW;
                  the_face_Indices (3) := NE;
               end;

               Face_last := Face_last + 1;
               declare
                  the_face_Indices : GLOBE_3D.Index_array renames object.face_Indices (Face_last);
               begin
                  the_face_Indices (1) := NE;
                  the_face_Indices (2) := SW;
                  the_face_Indices (3) := SE;
               end;
            end;
         end loop;
      end loop;

      object.is_Terrain := True;

      return object;
   end to_tri_Mesh;

   procedure Create (Object      : in out GLOBE_3D.tri_Mesh.p_tri_Mesh;
                     png_Heights : in     String;
                     Scale       : in     GLOBE_3D.Vector_3D)
   is
      use GLOBE_3D, GL, GLOBE_3D.REF, GLOBE_3D.Math;

      the_height_Map : height_Map := to_Height_Map (to_Matrix (png_Heights));
   begin
      Object      := to_tri_Mesh (the_height_Map, Scale);
      Object.Skin := material_only;
   end Create;

   function Create (png_Heights   : in     String;
                    texture_Image : in     String;
                    tile_Width    : in     Positive  := 32;
                    tile_Depth    : in     Positive  := 32;
                    base_Centre   : in     Vector_3D := (0.0, 0.0, 0.0);
                    Scale         : in     Vector_3D := (1.0, 1.0, 1.0)) return tri_Mesh.p_tri_Mesh_grid
   is
      the_Matrix : Matrix := to_Matrix (png_Heights);

      function Grid_last (total_Size, tile_Size : in Positive) return Positive
      is
         Last : Positive := 1  +  (total_Size - tile_Size) / (tile_Size - 1);
      begin
         if (total_Size - tile_Size) mod (tile_Size - 1) /= 0 then
            Last := Last + 1;
         end if;
         return Last;
      end;

      the_trimesh_Grid     : tri_Mesh.p_tri_Mesh_grid (1 .. Grid_last (the_Matrix'Length (1), tile_Depth),
                                                       1 .. Grid_last (the_Matrix'Length (2), tile_Width));
      row_First, row_Last,
      col_First, col_Last  : Integer; -- row and col ranges for each submatrix.

   begin
      -- create each grid element tri_Mesh
      --
      for Row in the_trimesh_Grid'Range (1) loop

         row_First := (tile_Depth - 1) * (Row - 1) + 1;
         row_Last  := Integer'Min (row_First + (tile_Depth - 1),  the_Matrix'Last (1));

         for Col in the_trimesh_Grid'Range (2) loop
            col_First := (tile_Width - 1) * (Col - 1) + 1;
            col_Last := Integer'Min (col_First + (tile_Width - 1),  the_Matrix'Last (2));

            declare
               the_height_Map : height_Map := to_Height_Map (sub_Matrix (the_Matrix, row_First, row_Last,
                                                                                     col_First, col_Last));
            begin
               the_trimesh_Grid (Row, Col) := to_tri_Mesh (the_height_Map,  Scale);
               the_trimesh_Grid (Row, Col).Pre_calculate;
            end;
         end loop;

      end loop;

      -- set position (site) and texture coords of the grid element objects
      --
      declare
         use GL.Textures;
         the_Texture   : GL.Textures.Object := new_Texture (image_Filename => texture_Image);

         site_X_offset : Real;
         site_Z_offset : Real := 0.0;
      begin
         for Row in the_trimesh_Grid'Range (1) loop
            site_X_offset := 0.0;

            for Col in the_trimesh_Grid'Range (2) loop
               declare
                  use tri_Mesh;
                  the_Trimesh    : p_tri_Mesh := the_trimesh_Grid (Row, Col);

                  total_Width    : Real       := Real (the_Matrix'Length (2));
                  total_Depth    : Real       := Real (the_Matrix'Length (1));

                  the_Site       : Vector_3D  := (site_X_offset              - (total_Width / 2.0) * Scale (0),
                                                  the_Trimesh.height_Offset,
                                                  site_Z_offset              - (total_Depth / 2.0) * Scale (2));
               begin
                  the_Trimesh.centre := the_Site + base_Centre;
                  the_Trimesh.set_Texture (to          => the_Texture,
                                           transform_s => (offset => (0.5 + Real (tile_Width - 1) / 2.0) * Scale (0)  +  site_X_offset,
                                                           scale  => 1.0 / ((total_Width - 1.0) * Scale (0))),
                                           transform_t => (offset => (0.5 + Real (tile_Depth - 1) / 2.0) * Scale (2)  +  site_Z_offset,
                                                           scale  => 1.0 / ((total_Depth - 1.0) * Scale (2))));
               end;

               if Col /= the_trimesh_Grid'Last (2) then
                  site_X_offset := site_X_offset + the_trimesh_Grid (Row, Col    ).Width / 2.0
                                                 + the_trimesh_Grid (Row, Col + 1).Width / 2.0;
               end if;
            end loop;

            if Row /= the_trimesh_Grid'Last (1) then
               site_Z_offset := site_Z_offset + the_trimesh_Grid (Row,     1).Depth / 2.0
                                              + the_trimesh_Grid (Row + 1, 1).Depth / 2.0;
            end if;
         end loop;
      end;

      return the_trimesh_Grid;
   end Create;

end Terrain.Simple;
