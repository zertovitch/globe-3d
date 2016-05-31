-------------------------------------------------------------------------
--  gl.Buffer.general - a generic for producing the various types of openGL vertex buffer objects.
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Interfaces.C.Pointers;

generic
   type base_Object is new GL.Buffer.Object with private;

   type Index         is mod <>;
   type Element       is private;
   type Element_Array is array (Index range <>) of aliased Element;

package GL.Buffer.general is

   type Object is new base_Object with private;

   function  to_Buffer (From : access Element_Array;   Usage : VBO_Usage) return Object;

   procedure set (Self : in out Object;   Position : in Positive     := 1;    -- tbd: make this raise 'constraint_Error' instead of openGL_Error, when bounds are violated.
                                          To       : in Element_Array);
   function  get (Self : access Object) return Element_Array;

   -- buffer memory map
   --

   type memory_Map is abstract tagged private;

   procedure release (Self : in    memory_Map);
   --
   -- 'release' must be called to release the buffers data back to the GL server.
   --
   -- May raise Corrupt_Buffer if the Buffer has become corrupt since the data
   -- was initially mapped. This can occur for system-specific reasons that affect the availability of graphics memory,
   -- such as screen mode changes. In such situations, the data store contents are undefined, and an application
   -- reinitialize the data store.
   --
   Corrupt_Buffer : exception;

   type read_only_Map  is new memory_Map with private;

   function  Map (Self : access Object) return read_only_Map'Class;

   function  get (Self : in read_only_Map;   Position : in Index) return Element;
   function  get (Self : in read_only_Map;   Position : in Index;
                                             Count    : in Positive) return Element_Array;

   type write_only_Map is new memory_Map with private;

   function  Map (Self : access Object) return write_only_Map'Class;

   procedure set (Self : in write_only_Map;   Position : in     Index;
                                              To       : access Element);
   procedure set (Self : in write_only_Map;   Position : in     Index;
                                              To       : in     Element);

   type read_write_Map is new memory_Map with private;

   function  Map (Self : access Object) return read_write_Map'Class;

   function  get (Self : in read_write_Map;   Position : in Index) return Element;
   function  get (Self : in read_write_Map;   Position : in Index;
                                              Count    : in Positive) return Element_Array;

   procedure set (Self : in read_write_Map;   Position : in     Index;
                                              To       : access Element);
   procedure set (Self : in read_write_Map;   Position : in     Index;
                                              To       : in     Element);

private

   type Object is new base_Object with null record;

   default_Terminator : Element;     -- no 'i.c.Pointers' subprogram is called which uses this, so a default 'Element' should suffice.

   package Element_Pointers is new interfaces.C.Pointers (Index, Element, Element_Array, default_Terminator);

   type memory_Map is abstract tagged
      record
         vbo_Target : GL.VBO_Target;

         Data : Element_Pointers.Pointer;
         Last : Index;
      end record;

   type read_only_Map  is new memory_Map with null record;
   type write_only_Map is new memory_Map with null record;
   type read_write_Map is new memory_Map with null record;

end GL.Buffer.general;
