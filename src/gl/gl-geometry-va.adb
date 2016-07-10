-------------------------------------------------------------------------
--  GL.Geometry
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Geometry.VA is

   overriding
   function  primitive_Id (Self : in     primal_Geometry) return GL.ObjectTypeEnm
   is
   begin
      return Self.Primitive.primitive_Id;
   end primitive_Id;

   overriding
   function  vertex_Count (Self : in     primal_Geometry) return GL.Geometry.vertex_Id
   is
   begin
      return Self.Primitive.Vertices'Length;
   end vertex_Count;

   overriding
   function  indices_Count (Self : in     primal_Geometry) return GL.positive_uInt
   is
   begin
      return Self.Primitive.Indices'Length;
   end indices_Count;

   overriding
   function Bounds (Self : in     primal_Geometry) return GL.Geometry.Bounds_record
   is
   begin
      return Bounds (Self.Primitive.Vertices.all);
   end Bounds;

   overriding
   function Vertices (Self : in     primal_Geometry) return GL.Geometry.Vertex_array
   is
   begin
      return Self.Primitive.Vertices.all;
   end Vertices;

   procedure set_Vertices  (Self : in out primal_Geometry;   To : access GL.Geometry.Vertex_array)
   is
   begin
      Self.Primitive.set_Vertices (To => To);
      Self.Bounds := Bounds (Self.Primitive.Vertices.all);
   end set_Vertices;

   overriding
   function Indices (Self : in     primal_Geometry) return GL.Geometry.vertex_Id_array
   is
      the_Indices : GL.Geometry.vertex_Id_array := Self.Primitive.Indices.all;
   begin
      increment (the_Indices);
      return the_Indices;
   end Indices;

   procedure set_Indices   (Self : in out primal_Geometry;   To : access GL.Geometry.vertex_Id_array)
   is
   begin
      Self.Primitive.set_Indices (To => To);
   end set_Indices;

   overriding
   procedure draw (Self : in     primal_Geometry)
   is
   begin
      Self.Primitive.Draw;
   end draw;

   overriding
   procedure destroy (Self : in out primal_Geometry)
   is
      use Primitive;
   begin
      free (Self.Primitive);
   end destroy;

end GL.Geometry.VA;
