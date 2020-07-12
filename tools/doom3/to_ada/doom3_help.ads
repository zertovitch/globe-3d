with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Doom3_Help is

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  has_input: Boolean:= False;

  pretty    : Boolean:= False;
  junk_dirs : Boolean:= False;
  areas_only: Boolean:= False;

  blurb: String:= "Converted by D3A";

  procedure YY_Accept;
  procedure YY_Abort;
  procedure YY_Terminate;

  indent : Natural:= 0;
  linenum : Integer:= 0;

  argument_pos_source: Natural:= 0;

  subtype Real is Long_Float;

  type Point_3D is array(0..2) of Real;

  last_pt: Point_3D;
  last_U, last_V, last_d: Real;

  num_vertices, num_indices, vertex_offset: Natural;

  surface_count: Natural;

  total_points, total_faces: Natural;
  d3_total_points, d3_total_faces: Natural;

  v1,v2,v3: Natural;

  procedure Set_current_texture(name_with_quotes: String);
  function Get_current_texture return String;

  consider_current_model: Boolean;

  procedure Add_Model(name_with_quotes: String);

  function Current_Model_name return String;

  procedure Reset_surfaces;

  procedure Add_surface(
    name_with_quotes: String;
    npoints: Natural;
    nfaces : Natural
  );

  function Get_surface_texture_name(nb: Natural) return String;
  function Get_surface_npoints(nb: Natural) return Natural;
  function Get_surface_nfaces(nb: Natural) return Natural;

  iap_pos, iap_neg, iap_points: Integer;
  procedure Add_IAP;

  -- old vrml stuff

  type Material_Float_vector is array(0..3) of aliased Real;

  type Material_type is record
    ambient,
    diffuse,
    specular,
    emission  : Material_Float_vector;
    shininess : Real; -- 0.0 .. 128.0
  end record;

  default_material   : constant Material_type:= (
            ambient =>        (0.2, 0.2, 0.2, 1.0),
            diffuse =>        (0.8, 0.8, 0.8, 1.0),
            specular =>       (0.0, 0.0, 0.0, 1.0),
            emission =>       (0.0, 0.0, 0.0, 1.0),
            shininess =>       25.6);

  current_matos: Material_type:= default_material;

  last_color: Material_Float_vector;

  function Image( i: Integer ) return String;
  function Image( r: Real ) return String;
  function Coords( p: Point_3D ) return String;
  function RGBA( p: Material_Float_vector ) return String;

  triangle_count: Natural;
  procedure Ada_Put_Triangle;

  procedure Ada_Begin;

  procedure Doom3_Info(s: String);
  procedure Doom3_Comment(s: String);
  procedure Ada_Comment(s: String);

  procedure Ada_Put(i: Integer);
  procedure Ada_Put(s: String);
  procedure Ada_Put_Line(s: String);
  procedure Ada_New_Line;

  procedure Ada_Build_Model_Header;
  procedure Ada_Build_Model_Footer;

end Doom3_Help;
