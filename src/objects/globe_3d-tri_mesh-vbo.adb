--  with GLOBE_3D.Options,
--       GLOBE_3D.Textures,
--       GLOBE_3D.Math;

with GL.Buffer.Vertex;
with GL.Buffer.Indices;
--  with GL.Buffer.Texture_coords;

--  with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Unchecked_Conversion;

--  with System;

package body GLOBE_3D.tri_Mesh.VBO is

  --  use GLOBE_3D.Options;

  --  package G3DT renames GLOBE_3D.Textures;
  --  package G3DM renames GLOBE_3D.Math;

   procedure destroy (o: in out tri_Mesh)
   is
      use GL.Skinned_Geometry;
   begin
      destroy (o.skinned_Geometry);
   end;

   function skinned_Geometries (o : in tri_Mesh) return GL.Skinned_Geometry.Skinned_Geometries
   is
   begin
      return (1 => o.skinned_Geometry);
   end;

   procedure set_Alpha ( o    : in out tri_Mesh;   Alpha : in GL.Double)
   is
   begin
      null;    -- tbd:
   end;

   function  is_Transparent (o    : in tri_Mesh) return Boolean
   is
      --  use type GL.Double;
   begin
      return o.skinned_Geometry.Skin.is_Transparent;
   end;

   procedure Pre_calculate (o: in out tri_Mesh)
   is
      --  use GL, G3DM;
   begin
      null;  -- tbd:
   end Pre_calculate;

   procedure Display (o      : in out tri_Mesh;
                      clip   : in     Clipping_data)
   is
   begin
      null;
   end Display;

   procedure set_Vertices (Self : in out tri_Mesh;   To : access GL.Geometry.Vertex_array)
   is
      use GL.Buffer.Vertex, GL.Geometry, GL.Geometry.VBO;

      the_Geometry : GL.Geometry.VBO.vbo_Geometry renames GL.Geometry.VBO.vbo_Geometry (Self.skinned_Geometry.Geometry.all);
   begin
      the_Geometry.Vertices     := to_Buffer (To, Usage => GL.STATIC_DRAW);  -- tbd: check usage
      the_Geometry.vertex_Count := GL.Sizei  (To'Length);

      the_Geometry.Bounds := Bounds (To.all);
   end;

   procedure set_Indices  (Self : in out tri_Mesh;   To : access GL.Geometry.vertex_Id_array)
   is
      use GL.Buffer.Indices, GL.Geometry, GL.Geometry.VBO;
      the_Geometry : GL.Geometry.VBO.vbo_Geometry renames GL.Geometry.VBO.vbo_Geometry (Self.skinned_Geometry.Geometry.all);
   begin
      the_Geometry.indices_Count := GL.Sizei (To'Length);
      the_Geometry.Indices       := to_Buffer (To, Usage => GL.STATIC_DRAW);
   end;

   procedure Skin_is (o : in out tri_Mesh;   Now : in GL.Skins.p_Skin)
   is
   begin
      o.skinned_Geometry.Skin   := Now;
      o.skinned_Geometry.Veneer := Now.all.new_Veneer (for_Geometry => o.skinned_Geometry.Geometry.all);
   end;

   function face_Count (o : in tri_Mesh) return Natural
   is
      use GL;
      the_Geometry : GL.Geometry.VBO.vbo_Geometry renames GL.Geometry.VBO.vbo_Geometry (o.skinned_Geometry.Geometry.all);
   begin
      return Natural (the_Geometry.indices_Count / 3);
   end;

   function Bounds (o : in tri_Mesh) return GL.Geometry.Bounds_record
   is
   begin
      return o.skinned_Geometry.Geometry.Bounds;
   end;

end GLOBE_3D.tri_Mesh.VBO;
