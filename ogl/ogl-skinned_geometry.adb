-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

--with GLUT;

--with Ada.Text_IO;
--with Ada.Numerics.Generic_Elementary_functions;
--with Ada.Strings.Unbounded;
--with Ada.Unchecked_Deallocation;


package body ogl.skinned_Geometry is


   use ogl.Geometry, ogl.Skins;

   procedure destroy (Self : in out skinned_Geometry)
   is
   begin
      free (self.Geometry);
      free (self.Skin);
      free (self.Veneer);
   end;


end ogl.skinned_Geometry;
