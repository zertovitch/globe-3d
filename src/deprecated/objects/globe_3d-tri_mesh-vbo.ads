-------------------------------------------------------------------------
--  GLOBE_3D.tri_Mesh.VBO
--
--  Copyright (c) Rod Kay 2007+
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Skinned_Geometry,
     GL.Geometry.VBO;

package GLOBE_3D.tri_Mesh.VBO is

   --  A triangle mesh Visual, using an openGL 'vertex buffer object' for geometry.
   --
   type tri_Mesh is new GLOBE_3D.tri_Mesh.tri_Mesh with
      record
         skinned_Geometry : GL.Skinned_Geometry.Skinned_Geometry
           := (Geometry  => new GL.Geometry.VBO.vbo_Geometry'
                                  (GL.Geometry.Geometry with
                                   primitive_Id  => GL.TRIANGLES,
                                   vertex_Count  => 0,
                                   indices_Count => 0,
                                   others        => <>),
               Skin      => null,
               Veneer    => null);
      end record;

   type p_tri_Mesh       is access all tri_Mesh'Class;
   type p_tri_Mesh_array is array (Positive range <>) of p_tri_Mesh;
   type p_tri_Mesh_grid  is array (Positive range <>, Positive range <>) of p_tri_Mesh;

   overriding
   procedure destroy        (o : in out tri_Mesh);
   overriding
   procedure Pre_calculate  (o : in out tri_Mesh);

   overriding
   procedure Display        (o : in out tri_Mesh;   clip : in     Clipping_Data);

   overriding
   procedure set_Alpha      (o : in out tri_Mesh;   Alpha : in GL.Double);
   overriding
   function  is_Transparent (o : in     tri_Mesh) return Boolean;

   overriding
   procedure set_Vertices (Self : in out tri_Mesh;   To : access GL.Geometry.Vertex_array);
   overriding
   procedure set_Indices  (Self : in out tri_Mesh;   To : access GL.Geometry.vertex_Id_array);

   overriding
   function skinned_Geometries (o : in     tri_Mesh) return GL.Skinned_Geometry.Skinned_Geometries;
   overriding
   function face_Count         (o : in     tri_Mesh) return Natural;
   overriding
   function Bounds             (o : in     tri_Mesh) return GL.Geometry.Bounds_record;

   overriding
   procedure Skin_is           (o : in out tri_Mesh;   Now : in GL.Skins.p_Skin);

end GLOBE_3D.tri_Mesh.VBO;
