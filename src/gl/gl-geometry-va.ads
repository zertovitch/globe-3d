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

with GL.Primitive;

package GL.Geometry.VA
--
-- Provides a Geometry subclass which uses Vertex Array based primitives.
--
is

   type primal_Geometry is new Geometry with
      record
         Primitive : GL.Primitive.p_Primitive;
      end record;

   type p_primal_Geometry is access all primal_Geometry;

   function  primitive_Id  (Self : in     primal_Geometry) return GL.ObjectTypeEnm;

   function  vertex_Count  (Self : in     primal_Geometry)        return GL.Geometry.vertex_Id;
   function  Vertices      (Self : in     primal_Geometry)        return GL.Geometry.Vertex_array;
   procedure set_Vertices  (Self : in out primal_Geometry;   To : access GL.Geometry.Vertex_array);

   function  indices_Count (Self : in     primal_Geometry)        return GL.positive_uInt;
   function  Indices       (Self : in     primal_Geometry)        return GL.Geometry.vertex_Id_array;
   procedure set_Indices   (Self : in out primal_Geometry;   To : access GL.Geometry.vertex_Id_array);

   function  Bounds        (Self : in     primal_Geometry) return GL.Geometry.Bounds_record;

   procedure draw          (Self : in     primal_Geometry);
   procedure destroy       (Self : in out primal_Geometry);

end GL.Geometry.VA;
