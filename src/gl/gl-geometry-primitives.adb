-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_functions;
with Ada.Text_IO; use Ada.Text_IO;



package body gl.geometry.Primitives is



   procedure destroy (Self : in out Primitive)
   is
   begin
      if self.owns_Vertices then
         free (self.Vertices);
      end if;

      free (self.Indices);
   end;





   procedure free (Self : in out p_Primitive)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Primitive'Class, p_Primitive);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;




   procedure Draw (Self : access Primitive'Class)
   is
   begin
      gl.bindBuffer    (gl.ARRAY_BUFFER, 0);                            -- disable 'vertex buffer objects'
      gl.bindBuffer    (gl.ELEMENT_ARRAY_BUFFER, 0);                    -- disable 'vertex buffer objects' indices


      gl.enableClientState (gl.VERTEX_ARRAY);
      gl.vertexPointer     (3, GL_DOUBLE,  0,  gl.to_Pointer (self.Vertices (1)(0)'unchecked_access));
      gl.drawElements      (primitive_Id (Self.all),
                            self.Indices'Length,
                            gl.UNSIGNED_INT,
                            to_void_Pointer (self.Indices (1)'unchecked_access));
   end;




   procedure set_Vertices  (Self : in out Primitive;   To : access gl.geometry.Vertex_array)
   is
   begin
      if self.Vertices = null then
         self.Vertices      := new Vertex_array' (To.all);
         self.owns_Vertices := True;

      elsif self.Vertices'Length = To'Length then
         self.Vertices.all := To.all;
      else
         free (self.Vertices);
         self.Vertices      := new Vertex_array' (To.all);
         self.owns_Vertices := True;
      end if;
   end;





   procedure set_Indices   (Self : in out Primitive;   To : access gl.geometry.vertex_Id_array)
   is
   begin
      if self.Indices = null then
         self.Indices := new vertex_Id_array' (To.all);

      elsif self.Indices'Length = To'Length then
         self.Indices.all := To.all;
      else
         free (self.Indices);
         self.Indices := new vertex_Id_array' (To.all);
      end if;
   end;






   -- 'Points'


   function create_Points (point_Count : in vertex_Id;   Vertices : p_vertex_array := null) return Points
   is
   begin
      if Vertices = null then
         return (vertices         => new Vertex_array    (1 .. point_Count),
                 owns_vertices    => True,
                 indices          => new vertex_Id_array (1 .. positive_uInt (point_Count)));
      else
         return (vertices         => Vertices,
                 owns_vertices    => False,
                 indices          => new vertex_Id_array (1 .. positive_uInt (point_Count)));
      end if;
   end;




   function primitive_Id (Self : in Points) return gl.ObjectTypeEnm
   is
   begin
      return gl.POINTS;
   end;







   -- 'Lines'
   --

   function create_Lines (line_Count : in Natural;   Vertices : p_Vertex_array := null) return Lines
   is
      indices_Count : positive_uInt := positive_uInt (2 * line_Count);
   begin
      if Vertices = null then
         return (vertices      => new Vertex_array (1 .. 2 * vertex_Id (line_Count)),
                 owns_vertices => True,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (vertices      => Vertices,
                 owns_vertices => False,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end;



   function primitive_Id (Self : in Lines) return gl.ObjectTypeEnm
   is
   begin
      return gl.LINES;
   end;





   function get_vertex_Id (Self : in Lines;   Line   : in Positive;
                                              Vertex : in Positive)
                                                                      return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (2 * (Line - 1)  +  Vertex)) + 1;
   end;



   procedure set_vertex_Id (Self : in out Lines;   Line   : in Positive;
                                                   Vertex : in Positive;
                                                   To     : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (2 * (Line - 1)  +  Vertex)) := To - 1;
   end;





   -- 'line Strip'
   --


   function create_line_Strip (line_Count : in Natural;   Vertices : p_Vertex_array := null) return line_Strip
   is
      indices_Count : positive_uInt := positive_uInt (line_Count + 1);
   begin
      if Vertices = null then
         return (vertices      => new Vertex_array (1 .. vertex_Id (line_Count) + 1),
                 owns_vertices => True,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (vertices      => Vertices,
                 owns_vertices => False,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end;




   function primitive_Id (Self : in line_Strip) return gl.ObjectTypeEnm
   is
   begin
      return gl.LINE_STRIP;
   end;





   function get_vertex_Id (Self : in line_Strip;   Line   : in Positive;
                                                   Vertex : in Positive)
                                                                         return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (Line - 1  +  Vertex)) + 1;
   end;



   procedure set_vertex_Id (Self : in out line_Strip;   Line   : in Positive;
                                                        Vertex : in Positive;
                                                        To     : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (Line - 1  +  Vertex)) := To - 1;
   end;




   -- 'line Loop'
   --


   function create_line_Loop (line_Count : in Natural;   Vertices: p_Vertex_array := null) return line_Loop
   is
      indices_Count : positive_uInt :=  positive_uInt (line_Count) + 1;
   begin
      if Vertices = null then
         return (vertices => new Vertex_array (1 .. vertex_Id (line_Count) + 1),
                 owns_vertices => True,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      else
         return (vertices      => Vertices,
                 owns_vertices => False,
                 indices       => new vertex_Id_array (1 .. indices_Count));
      end if;
   end;




   function primitive_Id (Self : in line_Loop) return gl.ObjectTypeEnm
   is
   begin
      return gl.LINE_LOOP;
   end;





   function get_vertex_Id (Self : in line_Loop;   Line   : in Positive;
                                                  Vertex : in Positive)
                                                                         return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (Line - 1  +  Vertex)) + 1;
   end;



   procedure set_vertex_Id (Self : in out line_Loop;   Line   : in Positive;
                                                       Vertex : in Positive;
                                                       To     : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (Line - 1  +  Vertex)) := To - 1;
   end;




   -- 'Triangles'
   --


   function create_Triangles (triangle_Count : in Natural;   Vertices : p_Vertex_array) return Triangles
   is
   begin
       return (vertices      => Vertices,
               owns_vertices => False,
               indices       => new vertex_Id_array (1 .. 3 * positive_uInt (triangle_Count)));
   end;




   function new_Triangles    (triangle_Count : in Natural;   Vertices : in     p_vertex_array) return p_Triangles
   is
   begin
      return new Triangles'(create_Triangles (triangle_Count, Vertices));
   end;




   function primitive_Id (Self : in Triangles) return gl.ObjectTypeEnm
   is
   begin
      return gl.TRIANGLES;
   end;





   function get_vertex_Id (Self : in Triangles;   Triangle : in Positive;
                                                  Vertex   : in Positive)
                                                                          return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (3 * (Triangle - 1)  +  Vertex)) + 1;
   end;




   procedure set_vertex_Id (Self : in out Triangles;   Triangle : in Positive;
                                                       Vertex   : in Positive;
                                                       To       : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (3 * (Triangle - 1)  +  Vertex)) := To - 1;
   end;





   -- 'triangle Strip'
   --


   function create_triangle_Strip (triangle_Count : in Natural;   Vertices : p_Vertex_array) return triangle_Strip'Class
   is
      the_Strip : triangle_Strip; -- (max_indices => positive_uInt (triangle_Count) + 2);
   begin
      the_Strip.Vertices      := Vertices;
      the_Strip.owns_Vertices := False;
      the_Strip.Indices       := new vertex_Id_array (1 .. positive_uInt (triangle_Count) + 2);

      return the_Strip;
   end;





   function new_triangle_Strip (triangle_Count : in Natural;   Vertices: p_Vertex_array) return p_triangle_Strip
   is
   begin
      return new triangle_Strip'Class'(create_triangle_Strip (triangle_Count, Vertices));
   end;





   function primitive_Id (Self : in triangle_Strip) return gl.ObjectTypeEnm
   is
   begin
      return gl.TRIANGLE_STRIP;
   end;





   function get_vertex_Id (Self : in triangle_Strip;   Triangle : in Positive;
                                                       Vertex   : in Positive)
                                                                              return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (Triangle + Vertex - 1)) + 1;
   end;



   procedure set_vertex_Id (Self : in out triangle_Strip;   Triangle : in Positive;
                                                            Vertex   : in Positive;
                                                            To       : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (Triangle + Vertex - 1)) := To - 1;
   end;





   -- 'triangle Fan'
   --


   function create_triangle_Fan (triangle_Count : in Natural;   Vertices: p_Vertex_array) return triangle_Fan
   is
   begin
      return (vertices      => Vertices,
              owns_vertices => False,
              indices       => new vertex_Id_array (1 .. positive_uInt (triangle_Count) + 2));
   end;





   function primitive_Id (Self : in triangle_Fan) return gl.ObjectTypeEnm
   is
   begin
      return gl.TRIANGLE_FAN;
   end;





   function get_vertex_Id (Self : in triangle_Fan;   Triangle : in Positive;
                                                     Vertex   : in Positive)
                                                                              return vertex_Id
   is
   begin
      if Vertex = 1 then
         return self.Indices (1);
      else
         return self.Indices (positive_uInt (Triangle + Vertex - 1)) + 1;
      end if;
   end;



   procedure set_vertex_Id (Self : in out triangle_Fan;   Triangle : in Positive;
                                                          Vertex   : in Positive;
                                                          To       : in vertex_Id)
   is
   begin
      if Vertex = 1 then
         self.Indices (1) := To;
      else
         self.Indices (positive_uInt (Triangle + Vertex - 1)) := To - 1;
      end if;
   end;





   -- 'Quads'
   --


   function create_Quads (quad_Count : in Natural;   Vertices: p_Vertex_array := null) return Quads
   is
      indices_Count : positive_uInt := 4 * positive_uInt (quad_Count);
   begin
      if Vertices = null then
         return (vertices      => new Vertex_array (1 .. vertex_Id (4 * quad_Count)),
                 owns_vertices => True,
                 indices       => new vertex_Id_array ( 1 .. indices_Count));
      else
         return (vertices      => Vertices,
                 owns_vertices => False,
                 indices       => new vertex_Id_array ( 1 .. indices_Count));
      end if;

   end;






   function new_Quads (quad_Count : in Natural;   Vertices: p_Vertex_array := null) return p_Quads
   is
   begin
      return new Quads'(create_Quads (quad_Count, Vertices));
   end;





   function primitive_Id (Self : in Quads) return gl.ObjectTypeEnm
   is
   begin
      return gl.QUADS;
   end;





   function get_vertex_Id (Self : in Quads;   Quad   : in Positive;
                                              Vertex : in Positive)
                                                                          return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (4 * (Quad - 1)  +  Vertex)) + 1;
   end;



   procedure set_vertex_Id (Self : in out Quads;   Quad     : in Positive;
                                                   Vertex   : in Positive;
                                                   To       : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (4 * (Quad - 1)  +  Vertex)) := To - 1;
   end;





   -- 'quad Strip'
   --


   function create_quad_Strip (quad_Count : in Natural;   Vertices: p_Vertex_array) return quad_Strip
   is
   begin
      return (vertices      => Vertices,
              owns_vertices => False,
              indices       => new vertex_Id_array (1 .. 2 * positive_uInt (quad_Count)  +  2));
   end;





   function primitive_Id (Self : in quad_Strip) return gl.ObjectTypeEnm
   is
   begin
      return gl.QUAD_STRIP;
   end;





   function get_vertex_Id (Self : in quad_Strip;   Quad     : in Positive;
                                                   Vertex   : in Positive)
                                                                              return vertex_Id
   is
   begin
      return self.Indices (positive_uInt (2 * (Quad - 1) + Vertex)) + 1;
   end;



   procedure set_vertex_Id (Self : in out quad_Strip;   Quad     : in Positive;
                                                        Vertex   : in Positive;
                                                        To       : in vertex_Id)
   is
   begin
      self.Indices (positive_uInt (2 * (Quad - 1) + Vertex)) := To - 1;
   end;






   -- 'Polygon'


   function create_Polygon (vertex_Count : in Natural;   Vertices: p_Vertex_array) return Polygon
   is
   begin
      return (vertices      => Vertices,
              owns_vertices => False,
              indices       => new vertex_Id_array (1 .. positive_uInt (vertex_Count)));
   end;






   function primitive_Id (Self : in Polygon) return gl.ObjectTypeEnm
   is
   begin
      return gl.POLYGON;
   end;



end gl.geometry.Primitives;
