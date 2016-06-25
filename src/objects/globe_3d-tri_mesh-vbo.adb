with
     GL.Buffer.Vertex,
     GL.Buffer.Indices,
     GL.Extended,
     Ada.Unchecked_Conversion;
use
     GL.Extended;

package body GLOBE_3D.tri_Mesh.VBO is

   overriding
   procedure destroy (o: in out tri_Mesh)
   is
      use GL.Skinned_Geometry;
   begin
      destroy (o.skinned_Geometry);
   end;

   overriding
   function skinned_Geometries (o : in tri_Mesh) return GL.Skinned_Geometry.Skinned_Geometries
   is
   begin
      return (1 => o.skinned_Geometry);
   end;

   overriding
   procedure set_Alpha (o : in out tri_Mesh;   Alpha : in GL.Double)
   is
   begin
      null;    -- todo
   end;

   overriding
   function  is_Transparent (o : in tri_Mesh) return Boolean
   is
   begin
      return o.skinned_Geometry.Skin.is_Transparent;
   end;

   overriding
   procedure Pre_calculate (o: in out tri_Mesh)
   is
   begin
      null;  -- todo
   end Pre_calculate;

   overriding
   procedure Display (o      : in out tri_Mesh;
                      clip   : in     Clipping_data)
   is
   begin
      null;
   end Display;

   overriding
   procedure set_Vertices (Self : in out tri_Mesh;   To : access GL.Geometry.Vertex_array)
   is
      use GL.Buffer.Vertex,
          GL.Geometry,
          GL.Geometry.VBO;

      the_Geometry : GL.Geometry.VBO.vbo_Geometry
             renames GL.Geometry.VBO.vbo_Geometry (Self.skinned_Geometry.Geometry.all);
   begin
      the_Geometry.Vertices     := to_Buffer (To, Usage => STATIC_DRAW);  -- todo: check usage
      the_Geometry.vertex_Count := GL.Sizei  (To'Length);

      the_Geometry.Bounds := Bounds (To.all);
   end;

   overriding
   procedure set_Indices  (Self : in out tri_Mesh;   To : access GL.Geometry.vertex_Id_array)
   is
      use GL.Buffer.Indices,
          GL.Geometry,
          GL.Geometry.VBO;
      the_Geometry : GL.Geometry.VBO.vbo_Geometry
             renames GL.Geometry.VBO.vbo_Geometry (Self.skinned_Geometry.Geometry.all);
   begin
      the_Geometry.indices_Count := GL.Sizei (To'Length);
      the_Geometry.Indices       := to_Buffer (To, Usage => STATIC_DRAW);
   end;

   overriding
   procedure Skin_is (o : in out tri_Mesh;   Now : in GL.Skins.p_Skin)
   is
   begin
      o.skinned_Geometry.Skin   := Now;
      o.skinned_Geometry.Veneer := Now.all.new_Veneer (for_Geometry => o.skinned_Geometry.Geometry.all);
   end;

   overriding
   function face_Count (o : in tri_Mesh) return Natural
   is
      use GL;
      the_Geometry : GL.Geometry.VBO.vbo_Geometry
             renames GL.Geometry.VBO.vbo_Geometry (o.skinned_Geometry.Geometry.all);
   begin
      return Natural (the_Geometry.indices_Count / 3);
   end;

   overriding
   function Bounds (o : in tri_Mesh) return GL.Geometry.Bounds_record
   is
   begin
      return o.skinned_Geometry.Geometry.Bounds;
   end;

end GLOBE_3D.tri_Mesh.VBO;
