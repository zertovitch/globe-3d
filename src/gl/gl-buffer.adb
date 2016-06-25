-------------------------------------------------------------------------
--  GL.Geometry - GL vertex buffer Object
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Buffer is

   -- VBO 'Name' Support
   --

   function new_vbo_Name return vbo_Name
   is
      the_Name : aliased vbo_Name;
   begin
      GenBuffers (1,  the_Name'Unchecked_Access);
      return the_Name;
   end;

   procedure free (the_vbo_Name : in vbo_Name) is
      the_Name : aliased vbo_Name := the_vbo_Name;
   begin
      DeleteBuffers (1, the_Name'Unchecked_Access);
   end;
   pragma Unreferenced (free);

   -- Object
   --

   procedure verify_Name (Self : in out Object'Class)
   is
   begin
      if Self.Name = 0 then
         Self.Name := new_vbo_Name;
      end if;
   end;

   procedure enable (Self : in Object'Class)
   is
   begin
      pragma Assert (Self.Name > 0);

      BindBuffer (VBO_Target (Self),  Self.Name);
   end;

   procedure destroy (Self : in out Object'Class)
   is
   begin
      BindBuffer    (VBO_Target (Self), 0);
      DeleteBuffers (1, Self.Name'Unchecked_Access);
   end;

   -- Array Object
   --

   overriding
   function vbo_Target (Self : in array_Object) return VBO_Target_Type
   is
   pragma Unreferenced (Self);
   begin
      return ARRAY_BUFFER;
   end;

   -- Element Array Object
   --

   overriding
   function vbo_Target (Self : in element_array_Object) return VBO_Target_Type
   is
   pragma Unreferenced (Self);
   begin
      return ELEMENT_ARRAY_BUFFER;
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

end GL.Buffer;
