-------------------------------------------------------------------------
--  GL.Primitive - GL geometry primitives
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Geometry;

package GL.Primitive is

   type Primitive is abstract tagged
      record
         Vertices      : GL.Geometry.p_Vertex_array;
         owns_Vertices : Boolean;

         Indices       : GL.Geometry.p_vertex_Id_array;
      end record;

   type p_Primitive is access all Primitive'Class;
   type Primitives  is array (Positive range <>) of p_Primitive;

   procedure destroy (Self : in out Primitive);
   procedure free    (Self : in out p_Primitive);

   function  primitive_Id (Self : in     Primitive) return GL.ObjectTypeEnm    is abstract;
   procedure Draw         (Self : access Primitive'Class);

   procedure set_Vertices  (Self : in out Primitive;   To : access GL.Geometry.Vertex_array);
   procedure set_Indices   (Self : in out Primitive;   To : access GL.Geometry.vertex_Id_array);

   null_Primitives : constant Primitives (1 .. 0) := (others => null);

   --  nb: - For 'create_*' functions below: if 'Vertices' parameter is null, a new Vertex array of appropriate size is created.
   --        This allows construction of primitives using shared vertices from a common vertex array.

   --  Points

   type Points is new Primitive with null record;

   function create_Points (point_Count : in GL.Geometry.vertex_Id;   Vertices : GL.Geometry.p_Vertex_array := null) return Points;

   overriding
   function primitive_Id  (Self : in Points) return GL.ObjectTypeEnm;

   --  Lines

   type Lines is new Primitive with null record;

   function create_Lines (line_Count : in Natural;   Vertices : GL.Geometry.p_Vertex_array := null) return Lines;

   overriding
   function primitive_Id   (Self : in     Lines) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     Lines;   Line   : in Positive;                     -- The line number.
                                                  Vertex : in Positive)                     -- 1 or 2.
                                                                        return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out Lines;   Line   : in Positive;                     -- The line number.
                                                   Vertex : in Positive;                     -- 1 or 2.
                                                   To     : in GL.Geometry.vertex_Id);

   --  Line Strip

   type line_Strip is new Primitive with null record;

   function create_line_Strip (line_Count : in Natural;   Vertices : GL.Geometry.p_Vertex_array := null) return line_Strip;

   overriding
   function primitive_Id   (Self : in     line_Strip) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     line_Strip;   Line   : in Positive;                 -- The line number.
                                                        Vertex : in Positive)                 -- 1 or 2.
                                                                              return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out line_Strip;   Line   : in Positive;                 -- The line number.
                                                        Vertex : in Positive;                 -- 1 or 2.
                                                        To     : in GL.Geometry.vertex_Id);

   --  Line Loop

   type line_Loop is new Primitive with null record;

   function create_line_Loop (line_Count : in Natural;   Vertices : GL.Geometry.p_Vertex_array := null) return line_Loop;

   overriding
   function primitive_Id   (Self : in     line_Loop) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     line_Loop;   Line   : in Positive;                  -- The line number.
                                                       Vertex : in Positive)                  -- 1 or 2.
                                                                             return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out line_Loop;   Line   : in Positive;                  -- The line number.
                                                       Vertex : in Positive;                  -- 1 or 2.
                                                       To     : in GL.Geometry.vertex_Id);

   --  Triangles

   type Triangles is new Primitive with null record;

   type p_Triangles is access all Triangles'Class;

   function create_Triangles (triangle_Count : in Natural;   Vertices : in     GL.Geometry.p_Vertex_array) return Triangles;
   function new_Triangles    (triangle_Count : in Natural;   Vertices : in     GL.Geometry.p_Vertex_array) return p_Triangles;

   overriding
   function primitive_Id   (Self : in     Triangles) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     Triangles;   Triangle : in Positive;                -- The triangle ordinal number.
                                                       Vertex   : in Positive)                -- 1, 2 or 3.
                                                                               return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out Triangles;   Triangle : in Positive;                -- The triangle ordinal number.
                                                       Vertex   : in Positive;                -- 1, 2 or 3.
                                                       To       : in GL.Geometry.vertex_Id);

   --  Triangle Strip

   type triangle_Strip is new Primitive with null record;

   type p_triangle_Strip is access all triangle_Strip'Class;

   type triangle_Strips is array (Positive range <>) of p_triangle_Strip;

   function create_triangle_Strip (triangle_Count : in Natural;   vertices : in GL.Geometry.p_Vertex_array) return triangle_Strip'Class;
   function    new_triangle_Strip (triangle_Count : in Natural;   vertices : in GL.Geometry.p_Vertex_array) return p_triangle_Strip;

   overriding
   function primitive_Id   (Self : in     triangle_Strip) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     triangle_Strip;   Triangle : in Positive;                     -- The triangle ordinal number.
                                                            Vertex   : in Positive)                     -- 1, 2 or 3.
                                                                                    return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out triangle_Strip;   Triangle : in Positive;                     -- The triangle ordinal number.
                                                            Vertex   : in Positive;                     -- 1, 2 or 3.
                                                            To       : in GL.Geometry.vertex_Id);

   --  Triangle Fan

   type triangle_Fan is new Primitive with null record;

   function create_triangle_Fan (triangle_Count : in Natural;   vertices : in GL.Geometry.p_Vertex_array) return triangle_Fan;

   overriding
   function primitive_Id  (Self : in     triangle_Fan) return GL.ObjectTypeEnm;

   function get_vertex_Id (Self : in     triangle_Fan;   Triangle : in Positive;                 -- The triangle ordinal number.
                                                         Vertex   : in Positive)                 -- 1, 2 or 3.
                                                                                 return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out triangle_Fan;   Triangle : in Positive;                -- The triangle ordinal number.
                                                          Vertex   : in Positive;                -- 1, 2 or 3.
                                                          To       : in GL.Geometry.vertex_Id);

   --  Quads

   type Quads is new Primitive with null record;
   type p_Quads is access all Quads'Class;

   function create_Quads  (quad_Count : in Natural;   Vertices : in GL.Geometry.p_Vertex_array := null) return Quads;
   function new_Quads     (quad_Count : in Natural;   Vertices : in GL.Geometry.p_Vertex_array := null) return p_Quads;

   overriding
   function primitive_Id  (Self : in     Quads) return GL.ObjectTypeEnm;

   function get_vertex_Id (Self : in Quads;   Quad   : in Positive;                        -- The quad's ordinal number.
                                              Vertex : in Positive)                        -- 1, 2, 3 or 4.
                                                                    return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out Quads;   Quad   : in Positive;                   -- The quad's ordinal number.
                                                   Vertex : in Positive;                   -- 1, 2, 3 or 4.
                                                   To     : in GL.Geometry.vertex_Id);

   --  Quad Strip

   type quad_Strip is new Primitive with null record;

   function create_quad_Strip (quad_Count : in Natural;   Vertices : in     GL.Geometry.p_Vertex_array) return quad_Strip;

   overriding
   function primitive_Id   (Self : in     quad_Strip) return GL.ObjectTypeEnm;

   function get_vertex_Id  (Self : in     quad_Strip;   Quad   : in Positive;                -- The quad's ordinal number.
                                                        Vertex : in Positive)                -- 1, 2, 3 or 4.
                                                                              return GL.Geometry.vertex_Id;

   procedure set_vertex_Id (Self : in out quad_Strip;   Quad   : in Positive;                -- The quad's ordinal number.
                                                        Vertex : in Positive;                -- 1, 2, 3 or 4.
                                                        To     : in GL.Geometry.vertex_Id);

   --  Polygon

   type Polygon is new Primitive with null record;

   function create_Polygon (vertex_Count : in Natural;   Vertices : in     GL.Geometry.p_Vertex_array) return Polygon;

   overriding
   function primitive_Id  (Self : in     Polygon) return GL.ObjectTypeEnm;

end GL.Primitive;
