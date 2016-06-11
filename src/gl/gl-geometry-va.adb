-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body GL.Geometry.VA is

   function  primitive_Id (Self : in     primal_Geometry) return GL.ObjectTypeEnm
   is
   begin
      return Self.Primitive.primitive_Id;
   end;

   function  vertex_Count  (Self : in     primal_Geometry) return GL.Geometry.vertex_Id
   is
   begin
      return Self.Primitive.Vertices'Length;
   end;

   function  indices_Count (Self : in     primal_Geometry) return GL.positive_uInt
   is
   begin
      return Self.Primitive.Indices'Length;
   end;

   function Bounds (Self : in     primal_Geometry) return GL.Geometry.Bounds_record
   is
   begin
      return Bounds (Self.Primitive.Vertices.all);
   end;

   function Vertices (Self : in     primal_Geometry) return GL.Geometry.Vertex_array
   is
   begin
      return Self.Primitive.Vertices.all;
   end;

   procedure set_Vertices  (Self : in out primal_Geometry;   To : access GL.Geometry.Vertex_array)
   is
   begin
      Self.Primitive.set_Vertices (To => To);
      Self.Bounds := Bounds (Self.Primitive.Vertices.all);
   end;

   function Indices (Self : in     primal_Geometry) return GL.Geometry.vertex_Id_array
   is
      the_Indices : GL.Geometry.vertex_Id_array := Self.Primitive.Indices.all;
   begin
      increment (the_Indices);
      return the_Indices;
   end;

   procedure set_Indices   (Self : in out primal_Geometry;   To : access GL.Geometry.vertex_Id_array)
   is
   begin
      Self.Primitive.set_Indices (To => To);
   end;

   procedure Draw (Self : in     primal_Geometry)
   is
   begin
      Self.Primitive.Draw;
   end;

   procedure destroy (Self : in out primal_Geometry)
   is
      use Primitive;
   begin
      free (Self.Primitive);
   end;

end GL.Geometry.VA;
