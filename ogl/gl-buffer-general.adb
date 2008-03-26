-------------------------------------------------------------------------
--  GL.Geometry - GL vertex buffer Object
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with gl.Errors;

with Ada.Numerics.Generic_Elementary_functions;
with Ada.Text_IO; use Ada.Text_IO;

with System;


package body gl.Buffer.general is

   use Element_Pointers;


   function to_gl_Pointer      is new ada.unchecked_Conversion (Element_Pointers.Pointer, gl.Pointer);
   function to_element_Pointer is new ada.unchecked_Conversion (gl.Pointer,               Element_Pointers.Pointer);



   -- vertex buffer object
   --


   function to_Buffer (From : access Element_Array;   Usage : VBO_Usage) return Object
   is
      use type gl.SizeIPtr;
      new_Buffer : Object;
   begin
      verify_Name (new_Buffer);
      new_Buffer.Length := From'Length;

      enable (new_Buffer);
      gl.bufferData (vbo_Target (new_Buffer),  From.all'size / 8,
                                               to_gl_Pointer (From (From'First)'access),
                                               Usage);
      return new_Buffer;
   end;





   procedure set (Self : in out Object;   Position : in Positive := 1;
                                                   To       : in Element_Array)
   is
      use type gl.SizeIPtr;
      new_Vertices        : aliased Element_Array := To;
      Vertex_Size_in_bits : constant Natural                 := To (To'First)'Size;
   begin
      enable (Self);
      gl.bufferSubData (vbo_Target (Self),  offset => gl.IntPtr ((Position - 1) * Vertex_Size_in_bits / 8),
                                            size   => new_Vertices'size / 8,
                                            data   => to_gl_Pointer (new_Vertices (new_Vertices'First)'unchecked_access));
      gl.errors.log;
   end;





   function  get (Self   : access    Object) return Element_Array
   is
      use gl.Geometry, gl.Buffer;

      the_Map      : read_only_Map'Class renames Map (Self);
      the_Vertices : constant Element_Array            := get (the_Map, Index'First, self.Length);
   begin
      release (the_Map);
      return the_Vertices;
   end;





   -- memory Maps
   --


   procedure release (Self : in    memory_Map)
   is
      Status : constant GL_Boolean := unmapBuffer (self.vbo_Target);
   begin
      if Status /= GL_True then
         raise Corrupt_Buffer;
      end if;
   end;




   function  get (Self : in memory_Map;   Position : in Index) return Element
   is
      use Element_Pointers, interfaces.C;
      Start : constant element_pointers.Pointer := self.Data + ptrDiff_t (Position - 1);
   begin
      return Value (Start, 1) (1);
   end;



   function  get (Self : in memory_Map;   Position : in Index;
                                          Count    : in Positive              ) return Element_Array
   is
      use Element_Pointers, interfaces.C;
      Start : constant element_pointers.Pointer := self.Data + ptrDiff_t (Position - 1);
   begin
      return Value (Start, ptrDiff_t (Count));
   end;




   procedure set (Self : in     memory_Map;   Position : in     Index;
                                              To       : access Element)
   is
      use gl.Geometry, Element_Pointers, interfaces.C;
   begin
      copy_Array (element_Pointers.Pointer (To),  self.Data + ptrDiff_t (Position - 1),  1);
   end;



   procedure set (Self : in     memory_Map;   Position : in Index;
                                              To       : in Element)
   is
      the_Vertex : aliased Element := To;
   begin
      set (Self, Position, to => the_Vertex'unchecked_access);
   end;




   -- read-only

   function  Map (Self : access Object) return read_only_Map'Class
   is
      use gl.Geometry;
      the_Map : read_only_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (mapBuffer (vbo_Target (Self.all),  gl.READ_ONLY));
      if the_Map.Data = null then
         raise gl.buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := vbo_Target (Self.all);

      return the_Map;
   end;



   function  get (Self : in read_only_Map;   Position : in Index) return Element
   is
   begin
      return get (memory_Map (Self), Position);
   end;


   function  get (Self : in read_only_Map;   Position : in Index;
                                                    Count    : in Positive              ) return Element_Array
   is
   begin
      return get (memory_Map (Self), Position, Count);
   end;





   -- write-only

   function Map (Self : access Object) return write_only_Map'Class
   is
      use gl.Geometry;
      the_Map : write_only_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (mapBuffer (vbo_Target (Self.all), gl.WRITE_ONLY));
      if the_Map.Data = null then
         raise gl.buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := vbo_Target (Self.all);

      return the_Map;
   end;



   procedure set (Self : in     write_only_Map;   Position : in     Index;
                                                         To       : access Element)
   is
   begin
      set (memory_Map (Self), Position, To);
   end;


   procedure set (Self : in     write_only_Map;   Position : in Index;
                                                         To       : in Element)
   is
   begin
      set (memory_Map (Self), Position, To);
   end;





   -- read-write

   function Map (Self : access Object) return read_write_Map'Class
   is
      use gl.Geometry;
      the_Map : read_write_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (mapBuffer (vbo_Target (Self.all), gl.READ_WRITE));
      if the_Map.Data = null then
         raise gl.buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := vbo_Target (Self.all);

      return the_Map;
   end;



   function  get (Self : in read_write_Map;   Position : in Index) return Element
   is
   begin
      return get (memory_Map (Self), Position);
   end;


   function  get (Self : in read_write_Map;   Position : in Index;
                                                     Count    : in Positive              ) return Element_Array
   is
   begin
      return get (memory_Map (Self), Position, Count);
   end;



   procedure set (Self : in     read_write_Map;   Position : in     Index;
                                                         To       : access Element)
   is
   begin
      set (memory_Map (Self), Position, To);
   end;


   procedure set (Self : in     read_write_Map;   Position : in Index;
                                                         To       : in Element)
   is
   begin
      set (memory_Map (Self), Position, To);
   end;



end gl.Buffer.general;
