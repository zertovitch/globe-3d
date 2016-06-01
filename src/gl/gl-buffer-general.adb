-------------------------------------------------------------------------
--  GL.Buffer.General
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Errors;
--  with GL.Geometry;

--  with Ada.Numerics.Generic_Elementary_Functions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with System;

package body GL.Buffer.General is

   use Element_Pointers;

   function to_gl_Pointer      is new Ada.Unchecked_Conversion (Element_Pointers.Pointer, GL.pointer);
   function to_element_Pointer is new Ada.Unchecked_Conversion (GL.pointer,               Element_Pointers.Pointer);

   -- vertex buffer object
   --

   function to_Buffer (From : access Element_Array;   Usage : VBO_Usage) return Object
   is
      use type GL.sizeiPtr;
      new_Buffer : Object;
   begin
      verify_Name (new_Buffer);
      new_Buffer.Length := From'Length;

      enable (new_Buffer);
      GL.BufferData (VBO_Target (new_Buffer),  From.all'Size / 8,
                                               to_gl_Pointer (From (From'First)'Access),
                                               Usage);
      return new_Buffer;
   end;

   procedure set (Self : in out Object;   Position : in Positive := 1;
                                                   To       : in Element_Array)
   is
      use type GL.sizeiPtr;
      new_Vertices        : aliased Element_Array := To;
      Vertex_Size_in_bits : constant Natural                 := To (To'First)'Size;
   begin
      enable (Self);
      GL.BufferSubData (VBO_Target (Self),  offset => GL.intPtr ((Position - 1) * Vertex_Size_in_bits / 8),
                                            size   => new_Vertices'Size / 8,
                                            data   => to_gl_Pointer (new_Vertices (new_Vertices'First)'Unchecked_Access));
      GL.Errors.Log;
   end;

   function  get (Self   : access    Object) return Element_Array
   is
      use GL.Buffer;  --  GL.Geometry

      the_Map      : read_only_Map'Class renames Map (Self);
      the_Vertices : constant Element_Array            := get (the_Map, Index'First, Self.Length);
   begin
      release (the_Map);
      return the_Vertices;
   end;

   -- memory Maps
   --

   procedure release (Self : in    memory_Map)
   is
      Status : constant GL_Boolean := UnmapBuffer (Self.vbo_Target);
   begin
      if Status /= GL_TRUE then
         raise Corrupt_Buffer;
      end if;
   end;

   function  get (Self : in memory_Map;   Position : in Index) return Element
   is
      use Interfaces.C;  --  Element_Pointers
      Start : constant Element_Pointers.Pointer := Self.Data + ptrdiff_t (Position - 1);
   begin
      return Value (Start, 1) (1);
   end;

   function  get (Self : in memory_Map;   Position : in Index;
                                          Count    : in Positive              ) return Element_Array
   is
      use Interfaces.C;  --  Element_Pointers
      Start : constant Element_Pointers.Pointer := Self.Data + ptrdiff_t (Position - 1);
   begin
      return Value (Start, ptrdiff_t (Count));
   end;

   procedure set (Self : in     memory_Map;   Position : in     Index;
                                              To       : access Element)
   is
      use Interfaces.C;  --  GL.Geometry, Element_Pointers
   begin
      Copy_Array (Element_Pointers.Pointer (To),  Self.Data + ptrdiff_t (Position - 1),  1);
   end;

   procedure set (Self : in     memory_Map;   Position : in Index;
                                              To       : in Element)
   is
      the_Vertex : aliased Element := To;
   begin
      set (Self, Position, To => the_Vertex'Unchecked_Access);
   end;

   -- read-only

   function  Map (Self : access Object) return read_only_Map'Class
   is
      --  use GL.Geometry;
      the_Map : read_only_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (MapBuffer (VBO_Target (Self.all),  GL.READ_ONLY));
      if the_Map.Data = null then
         raise GL.Buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := VBO_Target (Self.all);

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
      --  use GL.Geometry;
      the_Map : write_only_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (MapBuffer (VBO_Target (Self.all), GL.WRITE_ONLY));
      if the_Map.Data = null then
         raise GL.Buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := VBO_Target (Self.all);

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
      --  use GL.Geometry;
      the_Map : read_write_Map;
   begin
      enable (Self.all);

      the_Map.Data := to_element_Pointer (MapBuffer (VBO_Target (Self.all), GL.READ_WRITE));
      if the_Map.Data = null then
         raise GL.Buffer.no_platform_Support;
      end if;

      the_Map.Last       := Index (Self.Length);
      the_Map.vbo_Target := VBO_Target (Self.all);

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

end GL.Buffer.General;
