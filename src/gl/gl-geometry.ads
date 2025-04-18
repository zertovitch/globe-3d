-------------------------------------------------------------------------
--  GL.Geometry - Base Geometry class.
--
--  Copyright (c) Rod Kay 2007 .. 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Interfaces.C.Pointers;

package GL.Geometry is

   --  Planes
   --

   type Plane is array (0 .. 3) of aliased GL.Double;   -- A general plane in equation form (tbd: use  1 .. 4  ?)

   procedure normalise (the_Plane : in out Plane);

   --  Bounds
   --

   type Extent is
      record
         Min, Max : GL.Double;
      end record;

   function Max (L, R : in Extent) return Extent;

   type axis_aligned_bounding_Box is
      record
         X_Extent : Extent;    -- Extents in object space.
         Y_Extent : Extent;
         Z_Extent : Extent;
      end record;

   function Max (L, R : in axis_aligned_bounding_Box) return axis_aligned_bounding_Box;

   type Bounds_record is   -- tbd: better name ... 'type Bounds' conflicts with 'Bounds' trait function. ... sphere_box_Bounds ?
      record
         sphere_Radius : GL.Double;
         Box           : axis_aligned_bounding_Box;
      end record;

   null_Bounds : constant Bounds_record := (sphere_Radius => 0.0,
                                            Box           => (X_Extent => (Min => GL.Double'Last,
                                                                           Max => GL.Double'First),
                                                              Y_Extent => (Min => GL.Double'Last,
                                                                           Max => GL.Double'First),
                                                              Z_Extent => (Min => GL.Double'Last,
                                                                           Max => GL.Double'First)));
   function Max (L, R : in Bounds_record) return Bounds_record;

   --  Vertices
   --

   --  vertex_Id (an index into a vertex_Array)
   --

   type   vertex_Id is new GL.Uint;
   type p_vertex_Id is access all vertex_Id;

   subtype positive_Vertex_Id is vertex_Id range 1 .. vertex_Id'Last;

   function to_gl_Pointer   is new Ada.Unchecked_Conversion (p_vertex_Id, GL.pointer);   -- tbd: one of these is redundant.

   type   vertex_Id_array is array (GL.positive_uInt range <>) of aliased vertex_Id;
   type p_vertex_Id_array is access all vertex_Id_array;

   procedure increment (Self : in out vertex_Id_array);
   procedure decrement (Self : in out vertex_Id_array);

   procedure free (vert_Id_array : in out p_vertex_Id_array);

   --  Vertex
   --

   subtype Vertex is GL.Double_Vector_3D;                             -- tbd: can gl.Double_vector_3D use '1'-based indexing ?

   null_Vertex : constant Vertex := (GL.Double'Last, GL.Double'Last, GL.Double'Last); -- tbd: use NAN instead of 'Last ?

   function Image (Self : in     Vertex) return String;

   type    Vertex_array is array (positive_Vertex_Id range <>) of aliased Vertex;
   type  p_Vertex_array is access all Vertex_array;

   function Bounds (Self       : in     Vertex_array)    return GL.Geometry.Bounds_record;
   function Bounds (Vertices   : in     Vertex_array;
                    Indices    : in     vertex_Id_array) return GL.Geometry.Bounds_record;

   function Image  (Self       : in     Vertex_array)    return String;

   procedure free  (Vert_array : in out p_Vertex_array);

   package vertex_pointers is new interfaces.c.Pointers (positive_Vertex_Id, Vertex, Vertex_array, (others => gl.Double'Last));
   subtype p_Vertex        is     vertex_pointers.Pointer;

   function to_p_Vertex    is new Ada.Unchecked_Conversion (GL.pointer, p_Vertex);

   --  Lighting Normals
   --

   subtype Normal       is GL.Double_Vector_3D;
   type    Normals      is array (Positive           range <>) of aliased Normal;
   type    Normal_array is array (positive_Vertex_Id range <>) of aliased Normal;

   --  Base Geometry Class
   --

   type Geometry is abstract tagged
      record
         Bounds : Bounds_record;
      end record;

   type p_Geometry is access all Geometry'Class;

   function  primitive_Id   (Self : in     Geometry) return GL.ObjectTypeEnm             is abstract;

   function  vertex_Count   (Self : in     Geometry) return GL.Geometry.vertex_Id        is abstract;
   function  Vertices       (Self : in     Geometry) return GL.Geometry.Vertex_array     is abstract;

   function  indices_Count  (Self : in     Geometry) return GL.positive_uInt             is abstract;
   function  Indices        (Self : in     Geometry) return GL.Geometry.vertex_Id_array  is abstract;

   function  Bounds         (Self : in     Geometry) return GL.Geometry.Bounds_record    is abstract;

   function  vertex_Normals (Self : in     Geometry'Class) return Normal_array;
   function  face_Count     (Self : in     Geometry'Class) return Natural;
   --
   --  For point primitives, each point is considered a 'face'.
   --  For line  primitives, each line  is considered a 'face'.

   procedure draw    (Self : in     Geometry)   is abstract;

   procedure destroy (Self : in out Geometry)   is abstract;
   procedure free    (Self : in out p_Geometry);

end GL.Geometry;
