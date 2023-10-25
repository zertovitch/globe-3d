package VRML_Help is

  has_input : Boolean := False;

  pretty : Boolean := False;

  blurb : String := "Converted by Wrl2Ada";

  procedure YY_Accept;
  procedure YY_Abort;
  procedure YY_Terminate;

  max_sepa : constant := 1000;

  linenum : Integer := 0;
  type Sepa_item_count is array (1 .. max_sepa) of Natural;

  sepa_points : Sepa_item_count := (others => 0);
  --  ^ # points for this separator
  sepa_polys : Sepa_item_count := (others => 0);
  --  ^ # polygons for this separator

  sepa_count : Natural := 0;  --  Total # of separators

  idx_last_sepa : Natural := 0;

  flag_group : Boolean;

  indent : Natural := 0;

  subtype Real is Float;

  type Point_3D is array (0 .. 2) of Real;

  last_pt : Point_3D;

  type Material_Float_Vector is array (0 .. 3) of aliased Real;

  type Material_Type is record
    ambient,
    diffuse,
    specular,
    emission  : Material_Float_Vector;
    shininess : Real;  --  0.0 .. 128.0
  end record;

  default_material : constant Material_Type :=
    (ambient   =>  (0.2, 0.2, 0.2, 1.0),
     diffuse   =>  (0.8, 0.8, 0.8, 1.0),
     specular  =>  (0.0, 0.0, 0.0, 1.0),
     emission  =>  (0.0, 0.0, 0.0, 1.0),
     shininess =>  25.6);

  sepa_matos_defined : array (1 .. max_sepa) of Boolean := (others => False);

  current_matos : Material_Type := default_material;

  last_color : Material_Float_Vector;

  function Image (r : Real; force : Boolean := False) return String;
  function Coords (p : Point_3D) return String;
  function RGBA (p : Material_Float_Vector) return String;

  procedure VRML_Info (s : String);
  procedure VRML_Comment (s : String);
  procedure Ada_Comment (s : String);

  procedure Ada_Put (s : String);
  procedure Ada_Put_Line (s : String);
  procedure Ada_New_Line;

  procedure Reset_Index_Grouping;
  procedure Point_Index (i : Integer);

  --  *---------------------------------------------------------------------*
  --  *  vrml.h
  --  *
  --  * Project: SICS DIVE
  --  * Copyright: SICS
  --  * Implemented by: Emmanuel Frécon and Olof Hagsand
  --  *
  --  *---------------------------------------------------------------------*

  --  Text justification
  type VRML_Justification is
    (VRML_LEFT,
     VRML_CENTER,
     VRML_RIGHT);

  --  Parts of cones and cylinder
  VRML_SIDES  : constant := 2**0;
  VRML_BOTTOM : constant := 2**1;
  VRML_TOP    : constant := 2**2;
  VRML_ALL    : constant := (VRML_SIDES + VRML_BOTTOM + VRML_TOP);

  --  Font family
  type VRML_Font_Family is
    (VRML_SERIF,
     VRML_SANS,
     VRML_TYPEWRITER);

  --  Font styles
  VRML_NONE   : constant := 2**0;
  VRML_BOLD   : constant := 2**1;
  VRML_ITALIC : constant := 2**2;

  --  Bindings
  type VRML_Binding is
    (VRML_DEFAULT,
     VRML_OVERALL,
     VRML_PER_PART,
     VRML_PER_PART_INDEXED,
     VRML_PER_FACE,
     VRML_PER_FACE_INDEXED,
     VRML_PER_VERTEX,
     VRML_PER_VERTEX_INDEXED);

  --  Culling
  type VRML_culling is (
    VRML_ON,
    VRML_OFF,
    VRML_AUTO
  );

  --  FILE FORMAT/DEFAULTS
  --       ShapeHints {
  --            vertexOrdering  UNKNOWN_ORDERING      # SFEnum
  --            shapeType       UNKNOWN_SHAPE_TYPE    # SFEnum
  --            faceType        CONVEX                # SFEnum
  --            creaseAngle     0.5                   # SFFloat
  --       }

  --  The ShapeHints node also affects how default normals are generated.
  --  When an IndexedFaceSet has to generate default normals, it uses the
  --  creaseAngle field to determine which edges should be smoothly shaded
  --  and which ones should have a sharp crease. The crease angle is the
  --  angle between surface normals on adjacent polygons. For example, a
  --  crease angle of .5 radians (the default value) means that an edge
  --  between two adjacent polygonal faces will be smooth shaded if the
  --  normals to the two faces form an angle that is less than .5 radians
  --  (about 30 degrees). Otherwise, it will be faceted.

--  Vertex ordering
  type VRML_Vertex_Ordering is
    (VRML_UNKNOWN_ORDERING,
     VRML_CLOCKWISE,
     VRML_COUNTERCLOCKWISE);

  --  Shape Type
  type VRML_Shape_Type is
    (VRML_UNKNOWN_SHAPE_TYPE,
     VRML_SOLID);

  --  Face type
  type VRML_face_type is (
    VRML_UNKNOWN_FACE_TYPE,
    VRML_CONVEX
  );

  type ShapeHints is record
    ordering     : VRML_Vertex_Ordering;
    shape        : VRML_Shape_Type;
    face         : VRML_face_type;
    crease_angle : Real;
  end record;

  default_shape_hints : constant ShapeHints :=
   (ordering     => VRML_UNKNOWN_ORDERING,
    shape        => VRML_UNKNOWN_SHAPE_TYPE,
    face         => VRML_CONVEX,
    crease_angle => 0.5);

  current_shape_hints : ShapeHints := default_shape_hints;

  --  Wrap
  type VRML_Wrap is
    (VRML_REPEAT,
     VRML_CLAMP);

end VRML_Help;
