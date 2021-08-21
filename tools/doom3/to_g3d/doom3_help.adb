with GLOBE_3D.Aux, GLOBE_3D.IO, GLOBE_3D.BSP, GL.Math;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

package body Doom3_Help is

  function Image( i: Integer ) return String is
  begin
    return Trim(Integer'Image(i),Both);
  end Image;

  function Strip_quotes(s:String) return String is
    f, l: Natural;
  begin
    if s="" then
      return "";
    else
      if s(s'First)='"' then
        f:= s'First+1;
      else
        f:= s'First;
      end if;
      if s(s'Last)='"' then
        l:= s'Last-1;
      else
        l:= s'Last;
      end if;
      return s(f..l);
    end if;
  end Strip_quotes;

  function Junk(s:String) return String is
    l: Natural:= s'First;
  begin
    for i in s'Range loop
      if s(i)='/' or s(i)='\' then
        l:= i+1;
      end if;
    end loop;
    return s(l..s'Last);
  end Junk;

  function Optional_Junk(s:String) return String is
  begin
    if junk_dirs then
      return Junk(s);
    else
      return s;
    end if;
  end Optional_Junk;

  function pkg return String;

  --------------------------------------------------------------------------------------
  --  Map file : origins of models are not in the .proc file but in the .map file...  --
  --------------------------------------------------------------------------------------

  map_file_found: Boolean:= False;

  package Model_Origins is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => GL.Double_Vector_3D,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => GL.Math.Identical);

  origin_cat: Model_Origins.Map;

  procedure Parse_map_file is
    --  We parse only the following line pairs in the .map file:
    --    "model" "func_static_54265"
    --    "origin" "20 -1178 1397"
    f: File_Type;
    P_id, P: GL.Double_Vector_3D;
    model: Unbounded_String;
    map_file_name: constant String:= pkg & ".map";
    prev_line_model: Boolean:= False;
  begin
    Put_Line(Standard_Error, "Parsing map file: " & map_file_name);
    Open(f, In_File, map_file_name);
    while not End_Of_File(f) loop
      declare
        s: constant String:= Get_Line(f);
      begin
        if s'Length >= 10 then
          if s(s'First..s'First+8) = """model"" """ then     --  e.g. "model" "func_static_754"
            model:= U(s(s'First+9..s'Last-1));
            prev_line_model:= True;
          elsif s(s'First..s'First+9)= """origin"" """ and prev_line_model then  -- e.g. "origin" "128 0 744"
            declare
              vec: constant String:= s(s'First+10..s'Last-1);
              j: Natural:= 0;
              dim : Natural:= 0;
            begin
              j:= vec'First;
              for i in vec'Range loop
                if vec(i)=' ' then
                  P_id(dim):= Real'Value(vec(j..i-1));
                  j:= i+1;
                  dim:= dim + 1;
                end if;
              end loop;
              P_id(2):= Real'Value(vec(j..vec'Last));
            end;
            P:= (P_id(1), P_id(2), P_id(0));  -- id Software to GL coord. conversion
            origin_cat.Include (model, P);
            Put_Line(Standard_Error, "Model " & To_String(model) & " has origin " & GLOBE_3D.Aux.Coords(P));
            prev_line_model:= False;
          else
            prev_line_model:= False;
          end if;
        end if;
      end;
    end loop;
    Close(f);
    map_file_found:= True;
  exception
    when Name_Error =>
      Put_Line(Standard_Error,
        "*** Caution: map file: " & map_file_name &
        " not found; models except areas won't have a correct origin.");
  end Parse_map_file;

  ------------
  -- Models --
  ------------

  type Model is record
    name               : Unbounded_String;
    area               : Integer              := -1;
    portals_to_be_added: Natural              := 0;
    --  Last defined; used for IAP post-processing:
    last_point,
    last_face          : Natural              := 0;
    obj                : GLOBE_3D.p_Object_3D := null;
    avg_point          : GLOBE_3D.Point_3D    := (0.0, 0.0, 0.0);
    origin             : GLOBE_3D.Point_3D    := (0.0, 0.0, 0.0);
  end record;

  model_stack: array(1..10_000) of Model;
  model_top: Natural:= 0;

  -- area#(0...) refers to model #(>=1)
  area_stack: array(0..10_000) of Positive;
  area_top: Integer:= -1;

  procedure Add_Model(name_with_quotes: String) is
    un: Unbounded_String;
    name: constant String:= Strip_quotes(name_with_quotes);
    is_area: Boolean:= False;
  begin
    model_top:= model_top + 1;
    declare
      mt: Model renames model_stack(model_top);
    begin
      mt.area:= -1;
      if name(name'First)='_' then
        un:= U(name(name'First+1..name'Last));
        if name'Length > 4 and then
           name(name'First+1..name'First+4)="area"
        then
          area_top:= area_top + 1;
          mt.area:= area_top;
          area_stack(area_top):= model_top;
          is_area:= True;
        else
          Put_Line( Standard_Error,
            "** unknown case with model " & name &
             ": starts with '_' but not area."
          );
        end if;
      else
        un:= U(name);
      end if;
      mt.name:= un;
    end;
    if areas_only and not is_area then
      model_top:= model_top - 1;
      consider_current_model:= False;
    else
      consider_current_model:= True;
    end if;
  end Add_Model;

  function Current_Model_name return String is
  begin
    return S(model_stack(model_top).name);
  end Current_Model_name;

  -- Current texture - see also Surfaces

  current_texture: Unbounded_String;

  procedure Set_current_texture(name_with_quotes: String) is
    name: constant String:= Optional_Junk(Strip_quotes(name_with_quotes));
  begin
    current_texture:= U(name);
  end Set_current_texture;

  function Get_current_texture return String is
  begin
    return S(current_texture);
  end Get_current_texture;

  function pkg return String is
  begin
    if has_input then
      declare
        s: constant String:= Argument(argument_pos_source);
      begin
        for i in s'Range loop
          if s(i)='.' then -- skip file extension
            return s(s'First..i-1);
          end if;
        end loop;
        return s;
      end;
    else
      return "Doom3_level";
    end if;
  end pkg;

  -----------------------
  -- Texture catalogue --
  -----------------------
  -- Same textures appear numerous times in a .proc file, then
  -- it is difficult to figure out how many and which ones they are
  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right : p_Dir_node;
    name        : String(1..name_len);
  end record;

  catalogue: p_Dir_node:= null;

  procedure Insert( name: String;
                    node: in out p_Dir_node ) is
  begin
    if node = null then
      node:= new Dir_node'
        ( (name_len => name'Length,
           left => null, right => null,
           name => name) );
    elsif name > node.name then
      Insert( name, node.right );
    elsif name < node.name then
      Insert( name, node.left );
    else
      null; -- Duplicate_name;
    end if;
  end Insert;

  procedure Write_catalogue is
    f: File_Type;
    n: Natural:= 0;
    type Style_kind is
     (Ada_enum, Unzip_list,
      Unzip_cmd1,
      Unzip_cmd2,
      add_suffix1,
      add_suffix2,
      copy_fakes);

    function File_suffix(style: Style_kind) return String is
    begin
      case style is
        when Ada_enum =>
          return "_enum.ada";
        when Unzip_list =>
          return "_unzip_list.txt";
        when Unzip_cmd1 =>
          return "_unzip1.cmd";
        when Unzip_cmd2 =>
          return "_unzip2.cmd";
        when add_suffix1 =>
          return "_add_tex_suffix1.cmd";
        when add_suffix2 =>
          return "_add_tex_suffix2.cmd";
        when copy_fakes =>
          return "_copy_fakes.cmd";
      end case;
    end File_suffix;

    junk_opt: String(1..2):= "  ";

    procedure Traverse( p: p_Dir_node; style: Style_kind ) is
    begin
      if p /= null then
        Traverse(p.left,style);
        declare
          tex: constant String:= p.name(1..p.name_len-2); -- remove the "_d", added at insertion
        begin
          case style is
            when Ada_enum   =>
              declare
                s: constant String:= Junk(p.name);
              begin
                if Col(f)+s'Length > 75 then New_Line(f); else Put(f,' '); end if;
                Put(f, Junk(p.name) & ',');
              end;
            when Unzip_list =>
              Put_Line(f, tex & ".*");
              Put_Line(f, p.name & ".*");  --  name, with "_d" for diffuse
              Put_Line(f, tex & "_s.*");   --  name, with "_s" for specular
            when Unzip_cmd1  =>
              if junk_dirs then
                Put_Line(f,"unzip -o ..\d3tex.zip " & p.name & ".tga");
                Put_Line(f,"unzip -o ..\d3tex.zip " & tex & ".tga");
                Put_Line(f,"unzip -o ..\d3tex.zip " & tex & "_s.tga");  --  Specular
              else
                -- unused variant
                Put_Line(f,"unzip C:\Transferts\Doom3map\pak004.zip " & p.name & "*");
              end if;
            when Unzip_cmd2  =>
              Put_Line(f,"unzip -o ..\palettex.zip " & p.name & ".*");
              Put_Line(f,"unzip -o ..\palettex.zip " & tex & ".*");
              Put_Line(f,"unzip -o ..\palettex.zip " & tex & "_s.*");  --  Specular
            when add_suffix1 =>
              --  Example: if not exist black_d.tga  ren black.tga black_d.tga
              Put_Line (f, "if not exist " & p.name & ".tga  ren " & tex & ".tga " & p.name & ".tga");
            when copy_fakes =>
              Put_Line (f, "if not exist " & p.name & ".tga  if not exist " & p.name & ".bmp  copy ..\_fake.bmp " & p.name & ".bmp");
            when add_suffix2 =>
              --  Remove unused textures (bump, etc.)
              Put_Line(f,"if exist " & tex & ".tga       del " & tex & ".tga");
              Put_Line(f,"if exist " & tex & "_bmp.tga   del " & tex & "_bmp.tga");
              Put_Line(f,"if exist " & tex & "_h.tga     del " & tex & "_h.tga");
              Put_Line(f,"if exist " & tex & "_local.tga del " & tex & "_local.tga");
              --  Prefer .bmp over .tga if both exist (typically, .bmp from palettex.zip and .tga from d3tex.zip)
              Put_Line(f,"if exist " & p.name & ".bmp      del " & p.name & ".tga");
              Put_Line(f,"if exist " & tex    & "_s.bmp    del " & tex & "_s.tga");
          end case;
        end;
        n:= n + 1;
        Traverse(p.right,style);
      end if;
    end Traverse;

  begin
    if junk_dirs then
      junk_opt:= "-j";
    end if;
    for style in Style_kind loop
      n:= 0;
      Create(f,Out_File, pkg & "_textures" & File_suffix(style));
      Traverse(catalogue,style);
      Close(f);
    end loop;
    Put_Line(
      Standard_Error,
      "*** Total: " & Image(n) & " distinct textures"
    );
  end Write_catalogue;

  --------------
  -- Surfaces --
  --------------

  use GLOBE_3D;

  type p_Map_idx_pair_array is access Map_idx_pair_array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation( Map_idx_pair_array, p_Map_idx_pair_array);

  type p_Index_array is access Index_array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation( Index_array, p_Index_array);

  type Surface is record
    texture_name : Unbounded_String;
    npoints      : Natural;
    nfaces       : Natural;
    point        : GLOBE_3D.p_Point_3D_array;
    uv           : p_Map_idx_pair_array;
    tri          : p_Face_array; -- only tri(t).P used there!
    tri_d3       : p_Face_array; -- tri_d3(t).P has Doom 3 indices
    d3_pt_to_pt  : p_Index_array; -- The Doom 3 points are redundant
    curr_pt      : Natural;
    curr_d3_pt   : Natural;
    curr_uv      : Natural;
    curr_tri     : Natural;
  end record;

  surface_stack: array(1..10_000) of Surface;

  surface_top: Natural:= 0;

  procedure Reset_surfaces is
  begin
    for i in 1..surface_top loop
      Dispose(surface_stack(i).point);
      Dispose(surface_stack(i).uv);
      Dispose(surface_stack(i).tri);
      Dispose(surface_stack(i).tri_d3);
      Dispose(surface_stack(i).d3_pt_to_pt);
    end loop;
    surface_top:= 0;
    surface_count:= 0;
  end Reset_surfaces;

  procedure Add_surface(
    name_with_quotes: String;
    npoints: Natural;
    nfaces : Natural
  )
  is
    base_name: constant String:= Optional_Junk(Strip_quotes(name_with_quotes));
    name: constant String:= base_name & "_d";
  begin
    --  Insert the name with "diffuse" suffix (the image itself)
    Insert(name, catalogue);
    surface_top:= surface_top + 1;
    declare
      st: Surface renames surface_stack(surface_top);
    begin
      st.texture_name:= U(name);
      st.npoints:= npoints;
      st.nfaces:=  nfaces;
      st.point      := new Point_3D_array(1..npoints);
      st.d3_pt_to_pt:= new Index_array(1..npoints);
      st.uv         := new Map_idx_pair_array(1..npoints);
      st.tri        := new Face_array(1..nfaces);
      st.tri_d3     := new Face_array(1..nfaces);
      st.curr_pt   := 0;
      st.curr_d3_pt:= 0;
      st.curr_uv   := 0;
      st.curr_tri  := 0;
    end;
  end Add_surface;

  procedure Set_current_surface_current_point(p: Point_3D) is
    st: Surface renames surface_stack(surface_top);
    use GL.Math;
  begin
    st.curr_d3_pt:= st.curr_d3_pt + 1;
    for i in 1..st.curr_pt loop
      if Almost_zero(Norm(st.point(i) - p)) then
        --  Put_Line( Standard_Error,
        --    "Duplicate point: pt #" & Integer'Image(i) &
        --    " is same as intended for #" & Integer'Image(st.curr_pt)
        --  );
        total_points:= total_points - 1;
        st.d3_pt_to_pt(st.curr_d3_pt):= i;
        return;
      end if;
    end loop;
    st.curr_pt:= st.curr_pt + 1;
    st.point(st.curr_pt):= p;
    st.d3_pt_to_pt(st.curr_d3_pt):= st.curr_pt;
  end Set_current_surface_current_point;

  procedure Set_current_surface_current_uv(uv: GLOBE_3D.Map_idx_pair) is
    st: Surface renames surface_stack(surface_top);
  begin
    st.curr_uv:= st.curr_uv + 1;
    st.uv(st.curr_uv):= uv;
  end Set_current_surface_current_uv;

  procedure Set_current_surface_current_triangle is
    st: Surface renames surface_stack(surface_top);
    w1: constant Integer:= st.d3_pt_to_pt(v1+1);
    w2: constant Integer:= st.d3_pt_to_pt(v2+1);
    w3: constant Integer:= st.d3_pt_to_pt(v3+1);
    use GL.Math;
  begin
    if (v1=v2 or v2=v3 or v1=v3 or        -- <- original idx same
        w1=w2 or w2=w3 or w1=w3) or else  -- <- distinct vertices same
        Almost_zero(                      -- <- 2 vertices aligned,
          Norm((st.point(w1)-st.point(w2)) * (st.point(w1)-st.point(w3)))
        )
    then -- our triangle is not a triangle (stands on a line!)
      -- Put_Line( Standard_Error, "Degenerated triangle detected" );
      total_faces:= total_faces - 1;
      return;
    end if;
    st.curr_tri:= st.curr_tri + 1;
    st.tri(st.curr_tri).P:= (w1,w2,w3,Integer'Last);
    st.tri_d3(st.curr_tri).P:= (v1,v2,v3,Integer'Last);
  end Set_current_surface_current_triangle;

  function Get_surface_texture_name(nb: Natural) return String is
  begin
    return S(surface_stack(nb).texture_name);
  end Get_surface_texture_name;

  function Get_surface_npoints(nb: Natural) return Natural is
  begin
    return surface_stack(nb).npoints;
  end Get_surface_npoints;

  function Get_surface_nfaces(nb: Natural) return Natural is
  begin
    return surface_stack(nb).nfaces;
  end Get_surface_nfaces;

  ------------------------------
  -- IAP - Inter Area Portals --
  ------------------------------

  type IAP is record
    iap_pos, iap_neg, iap_points: Integer;
    points: Point_3D_array(1..4);
  end record;

  curr_IAP_points: Point_3D_array(1..4);

  IAP_stack: array(1..10_000) of IAP;
  IAP_top: Natural:= 0;

  invalid_iap: exception;

  procedure Add_IAP is
  begin
    IAP_top:= IAP_top + 1;
    declare
      p: IAP renames IAP_stack(IAP_top);
    begin
      p.iap_pos:= iap_pos;
      p.iap_neg:= iap_neg;
      p.iap_points:= iap_points;
      if iap_points not in p.points'Range then
        raise invalid_iap;
      end if;
      p.points:= curr_IAP_points;
    end;
  end Add_IAP;

  procedure Add_IAP_Vertex is
  begin
    iap_curr_point:= iap_curr_point + 1;
    if iap_curr_point not in 1..4 then
      raise invalid_iap;
    end if;
    curr_IAP_points(iap_curr_point):= last_pt;
  end Add_IAP_Vertex;

  max_faces, max_points: Natural:= 0;

  face_0      : GLOBE_3D.Face_type; -- takes defaults values
  face_portal : GLOBE_3D.Face_type; -- prototype for portals

  --  Statistics
  whole_pts, whole_tris, whole_d3_pts, whole_d3_tris: Natural:= 0;

  --  All "surfaces" have been parsed, we can build the object corresponding to the D3 model
  procedure Build_Model is
    p,f, p_mem: Natural:= 0;
    local_vertex_number: Integer;
    d3_vertex_number: Integer;
    m: Model renames model_stack(model_top);
  begin
    m.obj:= new Object_3D(total_points,total_faces);
    max_faces := Integer'Max(max_faces, total_faces);
    max_points:= Integer'Max(max_points,total_points);
    New_Line( Standard_Error );
    Put_Line( Standard_Error,
      "   -->" & Integer'Image(total_points) &
      " distinct points, of" & Integer'Image(d3_total_points)
    );
    Put_Line( Standard_Error,
      "   and" & Integer'Image(total_faces) &
      " non-degenerated faces, of" & Integer'Image(d3_total_faces) &
      " i.e. " & Integer'Image(d3_total_faces - total_faces) &
      " are degenerated"
    );
    whole_pts := whole_pts + total_points;
    whole_tris:= whole_tris + total_faces;
    whole_d3_pts := whole_d3_pts + d3_total_points;
    whole_d3_tris:= whole_d3_tris + d3_total_faces;

    for i in 1..surface_count loop
      p_mem:= p;
      for ps in 1..surface_stack(i).curr_pt loop
        p:= p + 1;
        m.obj.point(p):= surface_stack(i).point(ps);
      end loop;
      for fs in 1..surface_stack(i).curr_tri loop
        for s in 1..3 loop
          local_vertex_number:=
            surface_stack(i).tri(fs).P(4-s);
          d3_vertex_number:=
            surface_stack(i).tri_d3(fs).P(4-s);
            -- vertex nb for this surface; (4-s) to invert orientation
          face_0.P(s):= local_vertex_number + p_mem; -- add the offset!
          -- Set the texture coordinates
          face_0.texture_edge_map(s):= surface_stack(i).uv(
            d3_vertex_number + 1
          );
        end loop;
        f:= f + 1;
        model_stack(model_top).obj.face(f):= face_0;
--         put_line(
--            "surface " & i'img & " face " &
--            f'img & "texture: " & Get_surface_texture_name(i)
--         );
        declare
          tex_d: constant String:= Get_surface_texture_name(i);
          tex: constant String:= tex_d(tex_d'First..tex_d'Last-2); -- remove the "_d"
          use GLOBE_3D.Aux;
        begin
          Texture_name_hint(m.obj.all, f, tex_d);
          Specular_name_hint(m.obj.all, f, tex & "_s");
        exception
          when Constraint_Error =>
            raise Constraint_Error with "Texture ident too long. Try with -j (no directory names).";
        end;
      end loop;
    end loop;
    model_stack(model_top).last_point:= p;
    model_stack(model_top).last_face:= f;
    model_stack(model_top).obj.centre:= main_centre;
    Set_name(model_stack(model_top).obj.all, pkg & "_$_" & Current_Model_name);
    Reset_surfaces;
  end Build_Model;

  --  Portals must be inserted as supplemental (transparent) faces of areas.
  --  This procedure does only enlargement of objects and access reallocation.

  procedure Include_portals_to_areas is

    procedure Increment_number_of_portals(area: Integer) is
    begin
      if area in 0..area_top then
        model_stack(area_stack(area)).portals_to_be_added:=
          model_stack(area_stack(area)).portals_to_be_added + 1;
      end if;
    end Increment_number_of_portals;

  begin
    if IAP_top=0 then
      return;
    end if;
    -- Define_IAPs;
    for i in 1..IAP_top loop
      -- We will have to add a portal to both sides' areas
      Increment_number_of_portals(IAP_stack(i).iap_pos);
      Increment_number_of_portals(IAP_stack(i).iap_neg);
    end loop;
    -- Now, re-allocate model objects by adding new points and faces
    for a in 0..area_top loop
      declare
        m: Model renames model_stack(area_stack(a));
        new_obj: p_Object_3D;
        p,f: Natural;
      begin
        if m.portals_to_be_added > 0 then
          p:= m.obj.Max_points;
          f:= m.obj.Max_faces;
          new_obj:= new Object_3D(
            Max_points => p + 4*m.portals_to_be_added,
            Max_faces  => f +   m.portals_to_be_added
          );
          -- Clone common part:
          new_obj.ID                   := m.obj.ID;
          new_obj.centre               := m.obj.centre;
          new_obj.point(1..p)          := m.obj.point;
          new_obj.face(1..f)           := m.obj.face;
          new_obj.face_internal(1..f)  := m.obj.face_internal;
          -- Dispose(m.obj);
          m.obj:= new_obj;
        end if;
      end;
    end loop;
    --  Details of the portals and linking between objects are
    --  done later with Complete_area_with_portals.
  end Include_portals_to_areas;

  procedure Complete_area_with_portals(model_nb: Positive) is
    p,f: Natural:= 0;
    m: Model renames model_stack(model_nb);
    area: constant Natural:= m.area; -- supposed >= 0

    procedure Include_Portals(iap_nb, side_area, other_side: Integer; reversed: Boolean) is
    begin
      if side_area /= area then
        return;
      end if;
      p:= m.last_point;
      f:= m.last_face;
      -- Add the 4 new vertices:
      m.obj.point(p+1..p+4):= IAP_stack(iap_nb).points;
      -- Define the face:
      if reversed then
        face_portal.P:= (p+4, p+3, p+2, p+1);
      else
        face_portal.P:= (p+1, p+2, p+3, p+4);
      end if;
      -- ** Direct connection (needs all areas being created):
      face_portal.connecting:= model_stack(area_stack(other_side)).obj;
      --  Clone invisible portal face:
      m.obj.face(f+1):= face_portal;
      --  There are 4 more points, 1 more face:
      p:= p + 4;
      f:= f + 1;
      m.last_point:= p;
      m.last_face:= f;
    end Include_Portals;

  begin
    for i in 1..IAP_top loop
      Include_Portals(i, IAP_stack(i).iap_pos, IAP_stack(i).iap_neg, True);
      Include_Portals(i, IAP_stack(i).iap_neg, IAP_stack(i).iap_pos, False);
    end loop;
  end Complete_area_with_portals;

  type BSP_farm is array(Natural range <>) of GLOBE_3D.BSP.p_BSP_node;

  type p_BSP_farm is access BSP_farm;

  farm: p_BSP_farm:= null;

  procedure Allocate_BSP_farm(number: Natural) is
  begin
    farm:= new BSP_farm(0..number-1);
    for i in farm'Range loop
      farm(i):= new GLOBE_3D.BSP.BSP_node;
    end loop;
    current_BSP_node:= -1;
  end Allocate_BSP_farm;

  procedure Process_BSP_Node is
    use GLOBE_3D.BSP, GL.Math, GL;
    n: BSP_node renames farm(current_BSP_node).all;
  begin
    -- In .proc files:
    --
    --  /* node format is: ( planeVector ) positiveChild negativeChild */
    --  /* a child number of 0 is an opaque, solid area */
    --  /* negative child numbers are areas: (-1-child) */
    --  /* node 0 */ ( 1 0 0 -1024 ) 1 1684
    --  /* node 1 */ ( 1 0 0 -2048 ) 2 576
    --  /* node 2 */ ( 0 1 0 -2048 ) 3 16
    --
    -- NB: node 0 cannot be a child, then...
    --
    if pos_BSP_child > 0 then
      n.front_child:= farm(pos_BSP_child);
    elsif pos_BSP_child < 0 then
      n.front_leaf:= model_stack(area_stack(-1-pos_BSP_child)).obj;
    end if;
    if neg_BSP_child > 0 then
      n.back_child:= farm(neg_BSP_child);
    elsif neg_BSP_child < 0 then
      n.back_leaf:=  model_stack(area_stack(-1-neg_BSP_child)).obj;
    end if;
    n.normal:= last_pt;
    n.distance:= last_d - main_centre * n.normal;
  end Process_BSP_Node;

  procedure Compute_Averages is
    use GL, GL.Math;
  begin
    for i in 1..model_top loop
      declare
        m: Model renames model_stack(i);
        s: Point_3D;
      begin
        s:= (0.0, 0.0, 0.0);
        if m.last_point > 0 then
          for p in 1..m.last_point loop
            s:= s + m.obj.point(p);
          end loop;
          s:= s * (1.0 / Real(m.last_point));
        end if;
        m.avg_point:= s;
      end;
    end loop;
    if area_centering >= 0 then
      main_centre:= main_centre - model_stack(area_stack(area_centering)).avg_point;
      for i in 1..model_top loop -- we need to correct that on each object's centre.
        model_stack(i).obj.centre:= main_centre;
      end loop;
    end if;
  end Compute_Averages;

  --  Other statistics
  whole_merged_triangles: Natural:= 0;

  function Merge_triangles_trace(o: Object_3D) return Object_3D is
    o2: constant Object_3D:= GLOBE_3D.Aux.Merge_triangles(o);
    -- Added 2016-06-14.
  begin
    whole_merged_triangles:= whole_merged_triangles + (o.Max_faces - o2.Max_faces);
    return o2;
  end Merge_triangles_trace;

  procedure Write_Summary(log_name, root_name: String) is
    use GLOBE_3D.Aux;
    log: File_Type;
  begin
    Create(log, Out_File, log_name);
    if not map_file_found then
      Put_Line(log,
        "*** Caution: map file: " & root_name &
        ".map not found; models except areas haven't a correct origin."
      );
    end if;
    Put_Line(log, "Coordinates of main imposed centre: " & Coords(main_centre));
    if area_centering >= 0 then
      Put_Line(log, "Centered on area #: " & Integer'Image(area_centering));
    end if;
    New_Line(log);
    Put_Line(log,
      "All vertices, distinct per model" & Integer'Image(whole_pts) &
      " from" & Integer'Image(whole_d3_pts) &
      ", i.e." & Integer'Image((100*whole_pts)/whole_d3_pts) &
      "% were distinct"
    );
    Put_Line(log,
      "All valid triangles" & Integer'Image(whole_tris) &
      " from" & Integer'Image(whole_d3_tris) &
      ", i.e." & Integer'Image((100*whole_tris)/whole_d3_tris) &
      "% were valid"
    );
    Put_Line(log,
      "Triangle pairs merged into rectangles" & Integer'Image(whole_merged_triangles) &
      " from" & Integer'Image(whole_tris) &
      ", i.e." & Integer'Image((100*whole_merged_triangles)/whole_tris) &
      "% triangle to rectangle merges"
    );
    New_Line(log);
    for i in 1..model_top loop
      declare
        m: Model renames model_stack(i);
        so: p_Object_3D_list:= m.obj.sub_objects;
      begin
        Put_Line(log, "Model: " & Trim(m.obj.ID, Right));
        Put_Line(log, "=====");
        Put_Line(log,
          "  Polygon(s):" &
          Integer'Image(m.last_face) &
          " and" &
          Integer'Image(m.portals_to_be_added) &
          " portal(s)");
        Put_Line(log,
          "  Vertices:" &
          Integer'Image(m.last_point) &
          " and" &
          Integer'Image(m.portals_to_be_added * 4) &
          " for portals");
        Put_Line(log, "  Coordinates of average non-translated model vertex: " &
          Coords(m.avg_point));
        Put_Line(log, "  Coordinates of model centre: " & Coords(m.obj.centre));
        Put_Line(log, "  Sub-objects:" & Boolean'Image(so /= null));
        while so /= null loop
          Put_Line(log, "    " & Trim(so.objc.ID, Right));
          so:= so.next;
        end loop;
      end;
      New_Line(log);
    end loop;
    Close(log);
  end Write_Summary;

  --  Here the parsing is finished.
  --  Time to dump everything into files and produce a nice report.
  --
  procedure YY_Accept is
    target_area: p_Object_3D:= null;
    use GL.Math, Model_Origins;
    c: Cursor;
  begin
    if farm = null then
      -- Some custom levels like the Reims Cathedral have no BSP
      null;
    else
      Put_Line(Standard_Error, "Writing BSP tree (.bsp).");
      GLOBE_3D.IO.Save_file(pkg, farm(0)); -- Save BSP tree.
    end if;
    --  Include_portals_to_areas : already done at beginning of BSP parsing
    --  (see doom3.pry) because the BSP tree uses access types.
    for i in 1..model_top loop
      if model_stack(i).portals_to_be_added > 0 then
        Complete_area_with_portals(i);
      end if;
    end loop;
    --  We need to parse the map file for finding model origins.
    Parse_map_file;
    if map_file_found then  --  without origins, non-area models are bogus (0-centered)
      --  Include other models as sub-objects in areas
      for i in 1..model_top loop
        if model_stack(i).area < 0 then
          c:= origin_cat.Find(model_stack(i).name);
          if c /= No_Element then
            model_stack(i).obj.centre:= model_stack(i).obj.centre + Element(c);
          end if;
          if farm = null then
            --  No BSP, and in principle, one area (we could search fo best fit if more areas)
            if area_top >= 0 then
              target_area:= model_stack(area_stack(0)).obj;
            end if;
          else
            GLOBE_3D.BSP.Locate(model_stack(i).avg_point + model_stack(i).obj.centre, farm(0), target_area);
          end if;
          if target_area /= null then
            Put_Line(Standard_Error,
              "Sub-object " & Trim(model_stack(i).obj.ID, Both) &
              " is put into area: " & target_area.ID
            );
            --  Insert model in front of sub-object list.
            target_area.sub_objects:=
              new Object_3D_list'(
                objc => model_stack(i).obj,
                next => target_area.sub_objects);
          end if;
        end if;
      end loop;
    end if;
    for i in 1..model_top loop
      Put_Line(Standard_Error,
        "Writing object file (.g3d) for: " &
        Trim(model_stack(i).obj.ID, Right) & '.'
      );
      --  Save each object
      GLOBE_3D.IO.Save_file(
        Merge_triangles_trace (model_stack(i).obj.all)
      );
    end loop;
    Put_Line(Standard_Error, "Writing texture catalogue (.txt).");
    Write_catalogue; -- Save a list of textures
    Put_Line(Standard_Error, "Writing a summary (.log).");
    Write_Summary(pkg & ".log", pkg);
  end YY_Accept;

  procedure D3G_Init is
  begin
    face_0.skin:= texture_only;
    face_0.whole_texture:= False;
    face_0.P(4):= 0; -- we have triangles, P(4) is set to 0
    face_0.texture_edge_map:= ((0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0));
    face_portal.skin:= invisible;
  end D3G_Init;

  procedure YY_Abort is
  begin
    null;
  end YY_Abort;

  procedure YY_Terminate is
  begin
    null;
  end YY_Terminate;

  procedure Doom3_Comment(s: String) is -- dummy
  begin
    null;
  end Doom3_Comment;

end Doom3_Help;
