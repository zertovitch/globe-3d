-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_functions;
with Ada.Text_IO; use Ada.Text_IO;



package body ogl.Geometry.primal is



   function  primitive_Id (Self : in     primal_Geometry) return gl.ObjectTypeEnm
   is
   begin
      return self.primitive.primitive_Id;
   end;




   function  vertex_Count  (Self : in     primal_Geometry) return gl.geometry.vertex_Id
   is
   begin
      return self.primitive.Vertices'Length;
   end;




   function  indices_Count (Self : in     primal_Geometry) return gl.positive_uInt
   is
   begin
      return self.primitive.Indices'Length;
   end;




   function Bounds (Self : in     primal_Geometry) return gl.geometry.Bounds_record
   is
   begin
      return Bounds (self.Primitive.Vertices.all);
   end;



   function Vertices (Self : in     primal_Geometry) return gl.geometry.Vertex_array
   is
   begin
      return self.primitive.Vertices.all;
   end;



   procedure set_Vertices  (Self : in out primal_Geometry;   To : access gl.geometry.Vertex_array)
   is
   begin
      self.primitive.set_Vertices (to => To);
      self.Bounds := Bounds (self.primitive.Vertices.all);
   end;





   function Indices (Self : in     primal_Geometry) return gl.geometry.vertex_Id_array
   is
      the_Indices : gl.geometry.vertex_Id_array := self.primitive.Indices.all;
   begin
      increment (the_Indices);
      return the_Indices;
   end;


   procedure set_Indices   (Self : in out primal_Geometry;   To : access gl.geometry.vertex_Id_array)
   is
   begin
      self.primitive.set_Indices (to => To);
   end;





   procedure Draw (Self : in     primal_Geometry)
   is
   begin
      self.Primitive.draw;
   end;



   procedure destroy (Self : in out primal_Geometry)
   is
      use Primitives;
   begin
      free (self.Primitive);
   end;




end gl.Geometry.primal;
