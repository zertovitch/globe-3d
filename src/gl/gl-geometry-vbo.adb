-------------------------------------------------------------------------
--  GL.Geometry
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Geometry.VBO is

   use GL.Buffer;

   overriding
   function  primitive_Id (Self : in     vbo_Geometry) return GL.ObjectTypeEnm
   is
   begin
      return Self.primitive_Id;
   end primitive_Id;

   overriding
   function  vertex_Count (Self : in     vbo_Geometry) return GL.Geometry.vertex_Id
   is
   begin
      return vertex_Id (Self.vertex_Count);
   end vertex_Count;

   overriding
   function  indices_Count (Self : in     vbo_Geometry) return GL.positive_uInt
   is
   begin
      return GL.positive_uInt (Self.indices_Count);
   end indices_Count;

   overriding
   function  Bounds (Self : in     vbo_Geometry) return GL.Geometry.Bounds_record
   is
   begin
      return Self.Bounds;
   end Bounds;

   overriding
   procedure draw (Self : in     vbo_Geometry)
   is
   begin
      Self.Vertices.enable;
      GL.VertexPointer (3, GL_DOUBLE, 0, null);

      Self.Indices.enable;

      GL.EnableClientState  (GL.VERTEX_ARRAY);

      GL.DrawElements       (Self.primitive_Id, Self.indices_Count, GL.UNSIGNED_INT, null);
      GL.DisableClientState (GL.VERTEX_ARRAY);
   end draw;

   --  Modified by zheng, 2011.1.20
   overriding
   function Vertices (Self : in     vbo_Geometry) return GL.Geometry.Vertex_array
   is
      self_buf : aliased vbo_Geometry := Self;
   begin
      return self_buf.Vertices.get;
   end Vertices;

   --  Modified by zheng, 2011.1.20
   overriding
   function Indices (Self : in     vbo_Geometry) return GL.Geometry.vertex_Id_array
   is
      self_buf   : aliased vbo_Geometry    := Self;
      gl_Indices :         vertex_Id_array := self_buf.Indices.get;
   begin
      increment (gl_Indices);
      return gl_Indices;
   end Indices;

   overriding
   procedure destroy (Self : in out vbo_Geometry)
   is
   begin
      destroy (Self.Vertices);
      destroy (Self.Indices);
   end destroy;

end GL.Geometry.VBO;
