-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with ada.unchecked_Deallocation;
with interfaces.c.Pointers;
with ada.unchecked_Conversion;


package ogl.Geometry is


   -- planes
   --

   type Plane is array (0 .. 3) of aliased gl.Double;   -- a general plane in equation form (tbd: use  1 .. 4  ?)

   procedure normalise (the_Plane : in out Plane);



   -- bounds
   --

   type Extent is
      record
         Min, Max : gl.Double;
      end record;

   function Max (L, R : in Extent) return Extent;



   type axis_aligned_bounding_Box is
      record
         X_Extent : Extent;    -- extents in object space
         Y_Extent : Extent;
         Z_Extent : Extent;
      end record;

   function Max (L, R : in axis_aligned_bounding_Box) return axis_aligned_bounding_Box;




   type Bounds_record is   -- tbd: better name ... 'type Bounds' conflicts with 'Bounds' trait function. ... sphere_box_Bounds ?
      record
         sphere_Radius : gl.Double;
         Box           : axis_aligned_bounding_Box;
      end record;

   null_Bounds : constant Bounds_record := (sphere_radius => 0.0,
                                            box           => (x_extent => (min => gl.Double'last,
                                                                           max => gl.Double'first),
                                                              y_extent => (min => gl.Double'last,
                                                                           max => gl.Double'first),
                                                              z_extent => (min => gl.Double'last,
                                                                           max => gl.Double'first)));

   function Max (L, R : in Bounds_record) return Bounds_record;




   -- vertices
   --


   -- vertex Id (an index into a vertex_Array)
   --

   type   vertex_Id       is new gl.uInt;
   type p_vertex_Id       is access all vertex_Id;

   type   vertex_Id_array is array (gl.positive_uInt range <>) of aliased vertex_Id;
   type p_vertex_Id_array is access all vertex_Id_array;

   function to_gl_Pointer is new ada.unchecked_Conversion (p_vertex_Id, gl.Pointer);


   procedure increment (Self : in out vertex_Id_array);
   procedure decrement (Self : in out vertex_Id_array);

   function  to_void_Pointer is new ada.unchecked_Conversion   (p_vertex_Id,     gl.Pointer);
   procedure free            is new ada.unchecked_Deallocation (vertex_Id_array, p_vertex_Id_array);



   subtype positive_Vertex_Id is vertex_Id range 1 .. vertex_Id'Last;


   -- vertex
   --

   subtype Vertex       is gl.Double_vector_3D;                             -- tbd: can gl.Double_vector_3D use '1'-based indexing ?
   type    Vertex_array is array (positive_Vertex_Id range <>) of aliased Vertex;
   type  p_Vertex_array is access all Vertex_array;

   package vertex_pointers is new interfaces.c.Pointers (positive_Vertex_Id, Vertex, Vertex_array, (others => gl.Double'Last));
   subtype p_Vertex is vertex_pointers.Pointer;

   function to_p_Vertex is new ada.unchecked_Conversion (gl.Pointer, p_Vertex);
   function Image (Self : in     Vertex) return String;

   null_Vertex : constant Vertex := (gl.Double'last, gl.Double'last, gl.Double'last); -- tbd: use NAN instead of 'Last ?



   procedure free is new ada.unchecked_Deallocation (Vertex_array, p_Vertex_array);

   function Bounds (Self : in     Vertex_array) return ogl.geometry.Bounds_record;
   function Image  (Self : in     Vertex_array) return String;


   function Bounds (Vertices : in     Vertex_array;   Indices : in   vertex_Id_array) return ogl.geometry.Bounds_record;



   -- lighting normals
   --

   subtype Normal       is gl.Double_vector_3D;
   type    Normals      is array (Positive           range <>) of aliased Normal;
   type    Normal_array is array (positive_vertex_Id range <>) of aliased Normal;  -- tbd: rename vertex_Normal_array





   -- abstract base geometry class
   --

   type Geometry is abstract tagged
      record
         Bounds : Bounds_record;
      end record;


   type p_Geometry is access all Geometry'Class;


   function  primitive_Id  (Self : in     Geometry      ) return gl.ObjectTypeEnm          is abstract;

   function  vertex_Count  (Self : in     Geometry      ) return ogl.geometry.vertex_Id     is abstract;
   function  Vertices      (Self : in     Geometry      ) return ogl.geometry.Vertex_array  is abstract;

   function  indices_Count (Self : in     Geometry      ) return gl.positive_uInt             is abstract;
   function  Indices       (Self : in     Geometry      ) return ogl.geometry.vertex_Id_array  is abstract;

   function  Bounds        (Self : in     Geometry      ) return ogl.geometry.Bounds_record is abstract;

   function  face_Count    (Self : in     Geometry'Class) return Natural;
   --
   -- for point primitives, each point is considered a 'face'.
   -- for line  primitives, each line  is considered a 'face'.


   procedure Draw          (Self : in     Geometry)                                  is abstract;

   function  vertex_Normals (Self : in     Geometry'Class) return Normal_array;


   procedure destroy (Self : in out Geometry)                                        is abstract;
   procedure free    (Self : in out p_Geometry);




end ogl.Geometry;
