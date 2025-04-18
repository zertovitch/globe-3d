-------------------------------------------------------------------------
--  GL.Primitive
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Unchecked_Deallocation,
     GL.Geometry, GL.Extended;

use GL.Geometry, GL.Extended;

package body GL.Primitive is

   procedure destroy (Self : in out Primitive)
   is
   begin
      if Self.owns_Vertices then
         free (Self.Vertices);
      end if;

      free (Self.Indices);
   end destroy;

   procedure free (Self : in out p_Primitive)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Primitive'Class, p_Primitive);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end free;

   procedure Draw (Self : access Primitive'Class)
   is
   begin
      BindBuffer (ARRAY_BUFFER, 0);           -- Disable 'vertex buffer objects'.
      BindBuffer (ELEMENT_ARRAY_BUFFER, 0);   -- Disable 'vertex buffer objects' indices.

      GL.EnableClientState (GL.VERTEX_ARRAY);
      GL.VertexPointer     (3, GL_DOUBLE,  0,  GL.to_Pointer (Self.Vertices (1)(0)'Unchecked_Access));
      GL.DrawElements      (primitive_Id (Self.all),
                            Self.Indices'Length,
                            GL.UNSIGNED_INT,
                            to_gl_Pointer (Self.Indices (1)'Unchecked_Access));
   end Draw;

   procedure set_Vertices  (Self : in out Primitive;   To : access GL.Geometry.Vertex_array)
   is
   begin
      if Self.Vertices = null then
         Self.Vertices      := new Geometry.Vertex_array'(To.all);
         Self.owns_Vertices := True;

      elsif Self.Vertices'Length = To'Length then
         Self.Vertices.all := To.all;
      else
         free (Self.Vertices);
         Self.Vertices      := new Geometry.Vertex_array'(To.all);
         Self.owns_Vertices := True;
      end if;
   end set_Vertices;

   procedure set_Indices   (Self : in out Primitive;   To : access GL.Geometry.vertex_Id_array)
   is
   begin
      if Self.Indices = null then
         Self.Indices := new vertex_Id_array'(To.all);

      elsif Self.Indices'Length = To'Length then
         Self.Indices.all := To.all;
      else
         free (Self.Indices);
         Self.Indices := new vertex_Id_array'(To.all);
      end if;
   end set_Indices;

   --  'Points'

   function create_Points (point_Count : in vertex_Id;   Vertices : p_Vertex_array := null) return Points
   is
   begin
      if Vertices = null then
         return (Vertices         => new Geometry.Vertex_array (1 .. point_Count),
                 owns_Vertices    => True,
                 Indices          => new vertex_Id_array (1 .. positive_uInt (point_Count)));
      else
         return (Vertices         => Vertices,
                 owns_Vertices    => False,
                 Indices          => new vertex_Id_array (1 .. positive_uInt (point_Count)));
      end if;
   end create_Points;

   overriding
   function primitive_Id (Self : in Points) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.POINTS;
   end primitive_Id;

   --  'Lines'
   --

   function create_Lines (line_Count : in Natural;   Vertices : p_Vertex_array := null) return Lines
   is
      indices_Count : constant positive_uInt := positive_uInt (2 * line_Count);
   begin
      if Vertices = null then
         return (Vertices      => new Geometry.Vertex_array (1 .. 2 * vertex_Id (line_Count)),
                 owns_Vertices => True,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (Vertices      => Vertices,
                 owns_Vertices => False,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end create_Lines;

   overriding
   function primitive_Id (Self : in Lines) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.LINES;
   end primitive_Id;

   function get_vertex_Id (Self : in Lines;   Line   : in Positive;
                                              Vertex : in Positive)
                                                                      return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (2 * (Line - 1)  +  Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out Lines;   Line   : in Positive;
                                                   Vertex : in Positive;
                                                   To     : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (2 * (Line - 1)  +  Vertex)) := To - 1;
   end set_vertex_Id;

   --  'line Strip'
   --

   function create_line_Strip (line_Count : in Natural;   Vertices : p_Vertex_array := null) return line_Strip
   is
      indices_Count : constant positive_uInt := positive_uInt (line_Count + 1);
   begin
      if Vertices = null then
         return (Vertices      => new Geometry.Vertex_array (1 .. vertex_Id (line_Count) + 1),
                 owns_Vertices => True,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (Vertices      => Vertices,
                 owns_Vertices => False,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end create_line_Strip;

   overriding
   function primitive_Id (Self : in line_Strip) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.LINE_STRIP;
   end primitive_Id;

   function get_vertex_Id (Self : in line_Strip;   Line   : in Positive;
                                                   Vertex : in Positive)
                                                                         return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (Line - 1  +  Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out line_Strip;   Line   : in Positive;
                                                        Vertex : in Positive;
                                                        To     : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (Line - 1  +  Vertex)) := To - 1;
   end set_vertex_Id;

   --  'line Loop'
   --

   function create_line_Loop (line_Count : in Natural;   Vertices : p_Vertex_array := null) return line_Loop
   is
      indices_Count : constant positive_uInt :=  positive_uInt (line_Count) + 1;
   begin
      if Vertices = null then
         return (Vertices => new Geometry.Vertex_array (1 .. vertex_Id (line_Count) + 1),
                 owns_Vertices => True,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (Vertices      => Vertices,
                 owns_Vertices => False,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end create_line_Loop;

   overriding
   function primitive_Id (Self : in line_Loop) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.LINE_LOOP;
   end primitive_Id;

   function get_vertex_Id (Self : in line_Loop;   Line   : in Positive;
                                                  Vertex : in Positive) return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (Line - 1  +  Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out line_Loop;   Line   : in Positive;
                                                       Vertex : in Positive;
                                                       To     : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (Line - 1  +  Vertex)) := To - 1;
   end set_vertex_Id;

   --  'Triangles'
   --

   function create_Triangles (triangle_Count : in Natural;   Vertices : p_Vertex_array) return Triangles
   is
   begin
       return (Vertices      => Vertices,
               owns_Vertices => False,
               Indices       => new vertex_Id_array (1 .. 3 * positive_uInt (triangle_Count)));
   end create_Triangles;

   function new_Triangles (triangle_Count : in Natural;   Vertices : in     p_Vertex_array) return p_Triangles
   is
   begin
      return new Triangles'(create_Triangles (triangle_Count, Vertices));
   end new_Triangles;

   overriding
   function primitive_Id (Self : in Triangles) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.TRIANGLES;
   end primitive_Id;

   function get_vertex_Id (Self : in Triangles;   Triangle : in Positive;
                                                  Vertex   : in Positive) return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (3 * (Triangle - 1)  +  Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out Triangles;   Triangle : in Positive;
                                                       Vertex   : in Positive;
                                                       To       : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (3 * (Triangle - 1)  +  Vertex)) := To - 1;
   end set_vertex_Id;

   --  'triangle Strip'
   --

   function create_triangle_Strip (triangle_Count : in Natural;   vertices : p_Vertex_array) return triangle_Strip'Class
   is
      the_Strip : triangle_Strip; -- (max_indices => positive_uInt (triangle_Count) + 2);
   begin
      the_Strip.Vertices      := vertices;
      the_Strip.owns_Vertices := False;
      the_Strip.Indices       := new vertex_Id_array (1 .. positive_uInt (triangle_Count) + 2);

      return the_Strip;
   end create_triangle_Strip;

   function new_triangle_Strip (triangle_Count : in Natural;   vertices : p_Vertex_array) return p_triangle_Strip
   is
   begin
      return new triangle_Strip'Class'(create_triangle_Strip (triangle_Count, vertices));
   end new_triangle_Strip;

   overriding
   function primitive_Id (Self : in triangle_Strip) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.TRIANGLE_STRIP;
   end primitive_Id;

   function get_vertex_Id (Self : in triangle_Strip;   Triangle : in Positive;
                                                       Vertex   : in Positive) return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (Triangle + Vertex - 1)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out triangle_Strip;   Triangle : in Positive;
                                                            Vertex   : in Positive;
                                                            To       : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (Triangle + Vertex - 1)) := To - 1;
   end set_vertex_Id;

   --  'triangle Fan'
   --

   function create_triangle_Fan (triangle_Count : in Natural;   vertices : p_Vertex_array) return triangle_Fan
   is
   begin
      return (Vertices      => vertices,
              owns_Vertices => False,
              Indices       => new vertex_Id_array (1 .. positive_uInt (triangle_Count) + 2));
   end create_triangle_Fan;

   overriding
   function primitive_Id (Self : in triangle_Fan) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.TRIANGLE_FAN;
   end primitive_Id;

   function get_vertex_Id (Self : in triangle_Fan;   Triangle : in Positive;
                                                     Vertex   : in Positive) return vertex_Id
   is
   begin
      if Vertex = 1 then
         return Self.Indices (1);
      else
         return Self.Indices (positive_uInt (Triangle + Vertex - 1)) + 1;
      end if;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out triangle_Fan;   Triangle : in Positive;
                                                          Vertex   : in Positive;
                                                          To       : in vertex_Id)
   is
   begin
      if Vertex = 1 then
         Self.Indices (1) := To;
      else
         Self.Indices (positive_uInt (Triangle + Vertex - 1)) := To - 1;
      end if;
   end set_vertex_Id;

   --  'Quads'
   --

   function create_Quads (quad_Count : in Natural;   Vertices : p_Vertex_array := null) return Quads
   is
      indices_Count : constant positive_uInt := 4 * positive_uInt (quad_Count);
   begin
      if Vertices = null then
         return (Vertices      => new Geometry.Vertex_array (1 .. vertex_Id (4 * quad_Count)),
                 owns_Vertices => True,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (Vertices      => Vertices,
                 owns_Vertices => False,
                 Indices       => new vertex_Id_array (1 .. indices_Count));
      end if;

   end create_Quads;

   function new_Quads (quad_Count : in Natural;   Vertices : p_Vertex_array := null) return p_Quads
   is
   begin
      return new Quads'(create_Quads (quad_Count, Vertices));
   end new_Quads;

   overriding
   function primitive_Id (Self : in Quads) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.QUADS;
   end primitive_Id;

   function get_vertex_Id (Self : in Quads;   Quad   : in Positive;
                                              Vertex : in Positive) return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (4 * (Quad - 1)  +  Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out Quads;   Quad     : in Positive;
                                                   Vertex   : in Positive;
                                                   To       : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (4 * (Quad - 1)  +  Vertex)) := To - 1;
   end set_vertex_Id;

   --  'quad Strip'
   --

   function create_quad_Strip (quad_Count : in Natural;   Vertices : p_Vertex_array) return quad_Strip
   is
   begin
      return (Vertices      => Vertices,
              owns_Vertices => False,
              Indices       => new vertex_Id_array (1 .. 2 * positive_uInt (quad_Count)  +  2));
   end create_quad_Strip;

   overriding
   function primitive_Id (Self : in quad_Strip) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.QUAD_STRIP;
   end primitive_Id;

   function get_vertex_Id (Self : in quad_Strip;   Quad     : in Positive;
                                                   Vertex   : in Positive) return vertex_Id
   is
   begin
      return Self.Indices (positive_uInt (2 * (Quad - 1) + Vertex)) + 1;
   end get_vertex_Id;

   procedure set_vertex_Id (Self : in out quad_Strip;   Quad     : in Positive;
                                                        Vertex   : in Positive;
                                                        To       : in vertex_Id)
   is
   begin
      Self.Indices (positive_uInt (2 * (Quad - 1) + Vertex)) := To - 1;
   end set_vertex_Id;

   --  'Polygon'

   function create_Polygon (vertex_Count : in Natural;   Vertices : p_Vertex_array) return Polygon
   is
   begin
      return (Vertices      => Vertices,
              owns_Vertices => False,
              Indices       => new vertex_Id_array (1 .. positive_uInt (vertex_Count)));
   end create_Polygon;

   overriding
   function primitive_Id (Self : in Polygon) return GL.ObjectTypeEnm
   is
      pragma Unreferenced (Self);
   begin
      return GL.POLYGON;
   end primitive_Id;

end GL.Primitive;
