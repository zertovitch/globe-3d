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



package body ogl.Geometry.VBO is


   use gl.Buffer;




   function  primitive_Id (Self : in     vbo_Geometry) return gl.ObjectTypeEnm
   is
   begin
      return self.primitive_Id;
   end;




   function  vertex_Count  (Self : in     vbo_Geometry) return gl.geometry.vertex_Id
   is
   begin
      return vertex_Id (self.vertex_Count);
   end;



   function  indices_Count (Self : in     vbo_Geometry) return gl.positive_uInt
   is
   begin
      return gl.positive_uInt (self.indices_Count);
   end;




   function  Bounds (Self : in     vbo_Geometry) return gl.geometry.Bounds_record
   is
   begin
      return self.Bounds;
   end;




   procedure draw (Self : in     vbo_Geometry)
   is
   begin
      self.Vertices.enable;
      gl.vertexPointer (3, GL_DOUBLE, 0, null);

      self.Indices.enable;

      gl.enableClientState  (gl.VERTEX_ARRAY);

      gl.drawElements       (self.primitive_Id,  self.indices_Count, gl.UNSIGNED_INT, null);
      gl.disableClientState (gl.VERTEX_ARRAY);
   end;





   function Vertices (Self : in     vbo_Geometry) return gl.geometry.Vertex_array
   is
   begin
      return self.Vertices.get;
   end;




   function Indices (Self : in     vbo_Geometry) return gl.geometry.vertex_Id_array
   is
      gl_Indices : vertex_Id_array := self.Indices.get;
   begin
      increment (gl_Indices);
      return gl_Indices;
   end;




   procedure destroy (Self : in out vbo_Geometry)
   is
   begin
      destroy (self.Vertices);
      destroy (self.Indices);
   end;


end gl.Geometry.VBO;
