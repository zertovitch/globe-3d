--  Helper functions for the Doom3 .proc parser.

with GLOBE_3D, GL.Materials;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Doom3_Help is

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  has_input: Boolean:= False;

  junk_dirs : Boolean:= False;
  areas_only: Boolean:= False;

  procedure YY_Accept;
  procedure YY_Abort;
  procedure YY_Terminate;

  indent : Natural:= 0;
  linenum : Integer:= 0;

  argument_pos_source: Natural:= 0;

  main_centre: GLOBE_3D.Point_3D:= (0.0, 0.0, 0.0);

  area_centering: Integer:= -1;

  procedure Compute_Averages;

  subtype Real is GLOBE_3D.Real;

  last_pt: GLOBE_3D.Point_3D;
  last_U, last_V, last_d: GLOBE_3D.Real;

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

  procedure Set_current_surface_current_point(p: GLOBE_3D.Point_3D);
  procedure Set_current_surface_current_uv(uv: GLOBE_3D.Map_idx_pair);
  procedure Set_current_surface_current_triangle;

  function Get_surface_texture_name(nb: Natural) return String;
  function Get_surface_npoints(nb: Natural) return Natural;
  function Get_surface_nfaces(nb: Natural) return Natural;

  iap_pos, iap_neg, iap_points: Integer;
  iap_curr_point: Integer;

  procedure Add_IAP;
  procedure Add_IAP_Vertex;

  procedure Include_portals_to_areas;

  default_material   : constant GL.Materials.Material_type:= (
            ambient =>        (0.2, 0.2, 0.2, 1.0),
            diffuse =>        (0.8, 0.8, 0.8, 1.0),
            specular =>       (0.0, 0.0, 0.0, 1.0),
            emission =>       (0.0, 0.0, 0.0, 1.0),
            shininess =>       25.6);

  current_matos: GL.Materials.Material_type:= default_material;

  last_color: GL.Material_Float_vector;

  current_BSP_node: Integer:= -1;
  pos_BSP_child,
  neg_BSP_child   : Integer;

  procedure Allocate_BSP_farm(number: Natural);
  procedure Process_BSP_Node;

  procedure D3G_Init;

  --  All "surfaces" have been parsed, we can build the object corresponding to the D3 model
  procedure Build_Model;

  procedure Doom3_Comment(s: String); -- dummy

end Doom3_Help;
