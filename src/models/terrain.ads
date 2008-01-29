with GLOBE_3D.tri_Mesh;
with GLOBE_3D.Sprite;

with gl.Textures;
with gl.Geometry;

with ada.unchecked_Deallocation;



package Terrain is

   use Globe_3d;


   function to_Matrix (tga_Heights : in String) return Matrix;



   type height_Map (row_Count, column_Count : Positive) is
      record
         Heights     : Matrix (1 .. row_Count, 1 .. column_Count);
         min_Height  : Real                            := Real'Last;
         max_Height  : Real                            := Real'First;
      end record;

   type p_height_Map    is access all height_Map;
   type height_map_Grid is array (Positive range <>, Positive range <>) of p_height_Map;


   procedure free is new ada.unchecked_Deallocation (height_Map, p_height_Map);


   function to_Height_Map (the_Matrix : in Matrix) return height_Map;


   function Width (Self : in height_Map) return Real;
   function Depth (Self : in height_Map) return Real;





private

   function Vertex_Id_for (the_height_Map : in height_Map;
                           Row, Col       : in Positive   ) return gl.geometry.vertex_Id;


   procedure set (the_Vertices    : in out gl.geometry.vertex_Array;
                  from_height_Map : in     height_Map;
                  scale           : in     GLOBE_3D.Vector_3D;
                  height_Offset   :    out Real);

end Terrain;
