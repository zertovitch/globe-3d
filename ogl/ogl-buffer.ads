-------------------------------------------------------------------------
--  GL.Geometry - GL vertex buffer Object
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with oGL.Geometry;
with oGL.Textures;



package ogl.Buffer is


   subtype vbo_Name is gl.uInt;     -- an openGL vertex buffer 'name', which is a natural integer.


   -- buffer object
   --
   type Object is abstract tagged private;

   procedure enable  (Self : in     Object'Class);
   procedure destroy (Self : in out Object'Class);

   function VBO_Target (Self : in Object) return gl.VBO_Target is abstract;




   -- 'array' and 'element array' base classes
   --

   type array_Object         is new Object with private;
   type element_array_Object is new Object with private;



   -- refer to child packages, for specific buffers:
   --
   -- - gl.Buffer.vertex
   -- - gl.Buffer.texture_coords
   -- - gl.Buffer.normals
   -- - gl.Buffer.indices
   --
   -- (tbd: pixel pack/unpack buffers)



   no_platform_Support : exception;
   --
   -- raised by buffer 'Map' functions when OS platform does not support GL Buffer objects.



private

   type Object is abstract tagged
      record
         Name   : aliased vbo_Name := 0;
         Length :         Positive;
      end record;



   function vbo_Target (Self : in array_Object)         return gl.VBO_Target;
   function vbo_Target (Self : in element_array_Object) return gl.VBO_Target;



   type array_Object         is new Object with null record;
   type element_array_Object is new Object with null record;



   type vertex_buffer_Object is new array_Object with null record;


   -- support

   procedure verify_Name (Self : in out Object'Class);


end ogl.Buffer;
