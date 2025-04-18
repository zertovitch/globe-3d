with
     GL.IO,
     GL.Math;

package body Terrain is

   --  todo: This package uses a 'Sprite', whereas a 'triMesh.vbo' would probably be more appropriate.

   function to_Matrix (tga_Heights : in String) return Matrix
   is
      use GL.IO;

      the_Image   : GL.IO.Image                  := Load (tga_Heights);
      the_Pixels  : constant GL.IO.Byte_grid     := To_greyscale_pixels (the_Image);
      the_Heights : Matrix (the_Pixels'Range (1),
                            the_Pixels'Range (2));
   begin
      for Row in the_Heights'Range (1) loop
         for Col in the_Heights'Range (2) loop
            the_Heights (Row, Col) := Real (the_Pixels (Row, Col));
         end loop;
      end loop;

      Free (the_Image.data);

      return the_Heights;
   end to_Matrix;

   procedure flip_Vertically (Self : in out Matrix)
   is
      Pad : constant Matrix := Self;
   begin
      for Row in Self'Range (1)
      loop
         for Col in Self'Range (2)
         loop
            Self (Row, Col) := Pad (Self'Last (1) - Row + 1,  Col);
         end loop;
      end loop;
   end flip_Vertically;

   function to_Height_Map (the_Matrix : in Matrix) return height_Map
   is
      the_height_Map : height_Map (column_Count => the_Matrix'Length (2),
                                   row_Count    => the_Matrix'Length (1));
   begin
      for Row in the_Matrix'Range (1) loop
         for Col in the_Matrix'Range (2) loop
            the_height_Map.Heights (Row, Col) := the_Matrix (Row, Col);

            the_height_Map.min_Height := Real'Min (the_height_Map.min_Height,  the_Matrix (Row, Col));
            the_height_Map.max_Height := Real'Max (the_height_Map.max_Height,  the_Matrix (Row, Col));
         end loop;
      end loop;

      return the_height_Map;
   end to_Height_Map;

   function Width  (Self : in height_Map) return Real
   is
   begin
      return Real (Self.column_Count - 1);
   end Width;

   function Depth (Self : in height_Map) return Real
   is
   begin
      return Real (Self.row_Count - 1);
   end Depth;

   function Vertex_Id_for (the_height_Map : in height_Map;
                           Row, Col       : in Positive) return GL.Geometry.vertex_Id is
   begin
      return GL.Geometry.vertex_Id ((Row - 1) * the_height_Map.column_Count  +  Col);
   end Vertex_Id_for;

   procedure set (the_Vertices    : in out GL.Geometry.Vertex_array;
                  from_height_Map : in     height_Map;
                  scale           : in     GLOBE_3D.Vector_3D;
                  height_Offset   :    out Real)
   is
      use GL;

      the_height_Offset : constant Real               := from_height_Map.min_Height + (from_height_Map.max_Height - from_height_Map.min_Height) / 2.0;
      MidPoint          : constant GL.Geometry.Vertex := ((Real (from_height_Map.column_Count - 1) / 2.0),
                                                          the_height_Offset,
                                                          (Real (from_height_Map.row_Count - 1) / 2.0));

      procedure apply_scaling (the_Vertex : in out Vector_3D) is
      begin
         the_Vertex (0) := the_Vertex (0) * scale (0);
         the_Vertex (1) := the_Vertex (1) * scale (1);
         the_Vertex (2) := the_Vertex (2) * scale (2);
      end apply_scaling;

   begin
      for Row in from_height_Map.Heights'Range (1) loop
         for Col in from_height_Map.Heights'Range (2) loop
            declare
               use GL.Math;
               the_Point : GL.Geometry.Vertex renames the_Vertices (Vertex_Id_for (from_height_Map,  Row, Col));
            begin
               the_Point := GL.Geometry.Vertex'(Real (Col) - 1.0,                     -- '- 1.0' adjusts for '1 based' indexing.
                                                from_height_Map.Heights (Row, Col),
                                                Real (Row) - 1.0)
                            - MidPoint;

               apply_scaling (the_Point);
            end;
         end loop;
      end loop;

      height_Offset := scale (1) * the_height_Offset;
   end set;

end Terrain;
