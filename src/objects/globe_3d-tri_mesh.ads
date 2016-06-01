-------------------------------------------------------------------------
--  GLOBE_3D.tri_Mesh
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Textures, GL.Skins;

package GLOBE_3D.tri_Mesh is

   -- triangle mesh Object base class
   --

   type tri_Mesh is abstract new Visual with
      record
         null;
      end record;

   type p_tri_Mesh       is access all tri_Mesh'Class;
   type p_tri_Mesh_array is array (Positive range <>) of p_tri_Mesh;
   type p_tri_Mesh_grid  is array (Positive range <>, Positive range <>) of p_tri_Mesh;

   procedure set_Vertices (Self : in out tri_Mesh;   To : access GL.Geometry.Vertex_array)    is abstract;
   procedure set_Indices  (Self : in out tri_Mesh;   To : access GL.Geometry.vertex_Id_array) is abstract;

   procedure Skin_is (o : in out tri_Mesh;   Now : in GL.Skins.p_Skin) is abstract;

private

   procedure dummy;

end GLOBE_3D.tri_Mesh;
