-------------------------------------------------------------------------
--  GL.Geometry.VBO
--
--  Copyright (c) Rod Kay 2007 .. 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with
     GL.Buffer.Vertex,
     GL.Buffer.Indices;

package GL.Geometry.VBO
--
-- Provides a Geometry subclass which uses VBO based primitives.
--
is

   -- Vertex Buffer Object Geometry
   --

   type vbo_Geometry is new Geometry with
      record
         primitive_Id  :         GL.ObjectTypeEnm;
         vertex_Count  :         GL.Sizei;
         indices_Count :         GL.Sizei;
         Vertices      : aliased GL.Buffer.Vertex.Object;
         Indices       : aliased GL.Buffer.Indices.Object;    -- Indices of 'Vertices' which describe the primitive geometry.
      end record;

   type p_vbo_Geometry is access all vbo_Geometry;

   function  primitive_Id  (Self : in     vbo_Geometry) return GL.ObjectTypeEnm;

   function  vertex_Count  (Self : in     vbo_Geometry) return GL.Geometry.vertex_Id;
   function  Vertices      (Self : in     vbo_Geometry) return GL.Geometry.Vertex_array;

   function  indices_Count (Self : in     vbo_Geometry) return GL.positive_uInt;
   function  Indices       (Self : in     vbo_Geometry) return GL.Geometry.vertex_Id_array;

   function  Bounds        (Self : in     vbo_Geometry) return GL.Geometry.Bounds_record;

   procedure draw          (Self : in     vbo_Geometry);
   procedure destroy       (Self : in out vbo_Geometry);

end GL.Geometry.VBO;
