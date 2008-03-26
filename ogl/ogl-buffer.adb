-------------------------------------------------------------------------
--  GL.Geometry - GL vertex buffer Object
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with ogl.Errors;

with Ada.Numerics.Generic_Elementary_functions;
with Ada.Text_IO; use Ada.Text_IO;

with System;


package body ogl.Buffer is




   -- 'Name' support
   --

   function new_vbo_Name return vbo_Name
   is
      the_Name : aliased vbo_Name;
   begin
      gl.genBuffers (1,  the_Name'unchecked_access);
      return the_Name;
   end;




   procedure free (the_vbo_Name : in vbo_Name)
   is
      the_Name : aliased vbo_Name := the_vbo_Name;
   begin
      gl.deleteBuffers (1, the_Name'unchecked_access);
   end;





   -- object
   --

   procedure verify_Name (Self : in out Object'Class)
   is
   begin
      if self.Name = 0 then
         self.Name := new_vbo_Name;
      end if;
   end;



   procedure enable (Self : in Object'Class)
   is
   begin
      pragma Assert (self.Name > 0);

      gl.bindBuffer (vbo_Target (Self),  self.Name);
   end;



   procedure destroy (Self : in out Object'Class)
   is
   begin
      gl.bindBuffer    (vbo_Target (Self), 0);
      gl.DeleteBuffers (1, self.Name'unchecked_access);
   end;





   -- array object
   --

   function vbo_Target (Self : in array_Object) return gl.VBO_Target
   is
   begin
      return gl.ARRAY_BUFFER;
   end;



   -- element array object
   --

   function vbo_Target (Self : in element_array_Object) return gl.VBO_Target
   is
   begin
      return gl.ELEMENT_ARRAY_BUFFER;
   end;





--     -- texture coordinates
--     --
--
--     procedure set_texture_Coordinates (Self : in out vertex_buffer_Object;   To : access gl.textures.Coordinate_2D_array)
--     is
--        use type gl.SizeIPtr;
--     begin
--        verify_Name (Self);
--
--        gl.bindBuffer (gl.ARRAY_BUFFER,  self.Name);
--        gl.bufferData (gl.ARRAY_BUFFER,  To.all'size / 8,
--                                         to_Pointer (To (To'First).S'access),
--                                         GL.STATIC_DRAW);                        -- tbd: make this a parameter.
--     end;




end ogl.Buffer;
