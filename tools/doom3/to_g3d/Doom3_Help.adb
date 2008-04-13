with GLOBE_3D.IO;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
-- with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Doom3_Help is

  function Image( i: Integer ) return String is
  begin
    return Trim(Integer'Image(i),both);
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

  ------------
  -- Models --
  ------------

  type Model is record
    name               : Unbounded_String;
    area               : Integer:= -1;
    portals_to_be_added: Natural:= 0;
    last_point,
    last_face          : Natural:= 0;
    -- last defined; used for IAP post-processing
    obj                : GLOBE_3D.p_Object_3D;
  end record;

  model_stack: array(1..10_000) of Model;
  model_top: Natural:= 0;

  -- area#(0...) refers to model #(>=1)
  area_stack: array(0..10_000) of Positive;
  area_top: Integer:= -1;

  current_area_number: Integer:= -1;

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
          current_area_number:= current_area_number + 1;
          mt.area:= current_area_number;
          area_top:= area_top + 1;
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
  end;

  -- Current texture - see also Surfaces

  current_texture: Unbounded_String;

  procedure Set_current_texture(name_with_quotes: String) is
    name: constant String:= Optional_Junk(Strip_quotes(name_with_quotes));
  begin
    current_texture:= U(name);
  end;

  function Get_current_texture return String is
  begin
    return S(current_texture);
  end;

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
  type dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: natural) is record
    left, right : p_dir_node;
    name        : string(1..name_len);
  end record;

  catalogue: p_Dir_node:= null;

  procedure Insert( name: String;
                    node: in out p_dir_node ) is
  begin
    if node = null then
      node:= new dir_node'
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
    use Ada.Text_IO;
    f: File_type;
    n: Natural:= 0;
    type Style_kind is (Ada_enum, Unzip_list, Unzip_cmd);
    junk_opt: String(1..2):= "  ";

    procedure Traverse( p: p_dir_node; style: Style_kind ) is
    begin
      if p /= null then
        Traverse(p.left,style);
        case style is
          when Ada_enum   =>
            declare
              s: constant String:= Junk(p.name);
            begin
              if Col(f)+s'Length > 75 then New_Line(f); else Put(f,' '); end if;
              Put(f,junk(p.name) & ',');
            end;
          when Unzip_list => Put_Line(f,p.name & "*");
          when Unzip_cmd  => Put_Line(f,"unzip " & junk_opt & " pak004.zip " & p.name & "*");
        end case;
        n:= n + 1;
        Traverse(p.right,style);
      end if;
    end Traverse;

  begin
    if junk_dirs then
      junk_opt:= "-j";
    end if;
    Create(f,out_file, pkg & "_textures.txt");
    for style in Style_kind loop
      Put_Line(f,"***" & Style_kind'Image(style) & ':');
      case style is
        when Ada_Enum => null;
        when Unzip_list =>
          Put_Line(f,"7zip e -i@list.txt pak004.zip");
        when Unzip_cmd =>
          null;
      end case;
      Traverse(catalogue,style);
      New_Line(f);
    end loop;
    Put_Line(f, "*** Total: " & Image(n) & " distinct textures");
    Close(f);
  end Write_catalogue;

  --------------
  -- Surfaces --
  --------------

  use GLOBE_3D;

  type p_Map_idx_pair_array is access Map_idx_pair_array;

  procedure Dispose is
    new Ada.Unchecked_Deallocation( Map_idx_pair_array, p_Map_idx_pair_array);

  type Surface is record
    texture_name : Unbounded_String;
    npoints      : Natural;
    nfaces       : Natural;
    point        : GLOBE_3D.p_Point_3D_array;
    uv           : p_Map_idx_pair_array;
    tri          : p_Face_array; -- only tri(t).P used there!
    curr_pt      : Natural;
    curr_uv      : Natural;
    curr_tri     : Natural;
  end record;

  surface_stack: array(1..10_000) of Surface;

  surface_top: Natural:= 0;

  procedure Reset_surfaces is
    use GLOBE_3D;
  begin
    for i in 1..surface_top loop
      Dispose(surface_stack(i).point);
      Dispose(surface_stack(i).uv);
      Dispose(surface_stack(i).tri);
    end loop;
    surface_top:= 0;
    surface_count:= 0;
  end;

  procedure Add_surface(
    name_with_quotes: String;
    npoints: Natural;
    nfaces : Natural
  )
  is
    use GLOBE_3D;
    base_name: constant String:= Optional_Junk(Strip_quotes(name_with_quotes));
    name: constant String:= base_name & "_d";
  begin
    Insert(name, catalogue);
    -- insert the name with "diffuse" suffix (the image itself)
    surface_top:= surface_top + 1;
    declare
      st: Surface renames surface_stack(surface_top);
    begin
      st.texture_name:= U(name);
      st.npoints:= npoints;
      st.nfaces:=  nfaces;
      st.point:= new Point_3D_array(1..npoints);
      st.uv   := new Map_idx_pair_array(1..npoints);
      st.tri  := new Face_array(1..nfaces);
      st.curr_pt  := 0;
      st.curr_uv  := 0;
      st.curr_tri := 0;
    end;
  end;

  procedure Set_current_surface_current_point(p: Point_3D) is
    st: Surface renames surface_stack(surface_top);
  begin
    st.curr_pt:= st.curr_pt + 1;
    st.point(st.curr_pt):= P;
  end;

  procedure Set_current_surface_current_uv(uv: GLOBE_3D.Map_idx_pair) is
    st: Surface renames surface_stack(surface_top);
  begin
    st.curr_uv:= st.curr_uv + 1;
    st.uv(st.curr_uv):= uv;
  end;

  procedure Set_current_surface_current_triangle is
    st: Surface renames surface_stack(surface_top);
  begin
    st.curr_tri:= st.curr_tri + 1;
    st.tri(st.curr_tri).P:= (v1,v2,v3,Integer'Last);
  end;

  function Get_surface_texture_name(nb: Natural) return String is
  begin
    return S(surface_stack(nb).texture_name);
  end;

  function Get_surface_npoints(nb: Natural) return Natural is
  begin
    return surface_stack(nb).npoints;
  end;

  function Get_surface_nfaces(nb: Natural) return Natural is
  begin
    return surface_stack(nb).nfaces;
  end;

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

  procedure Build_Model is
    p,f, p_mem: Natural:= 0;
    doom3_vertex_number: Integer;
    use GLOBE_3D;
  begin
    model_stack(model_top).obj:= new Object_3D(total_points,total_faces);
    max_faces := Integer'Max(max_faces, total_faces);
    max_points:= Integer'Max(max_points,total_points);
    for i in 1..surface_count loop
      p_mem:= p;
      for ps in surface_stack(i).point'Range loop
        p:= p + 1;
        model_stack(model_top).obj.point(p):= surface_stack(i).point(ps);
      end loop;
      for fs in surface_stack(i).tri'Range loop
        for s in 1..3 loop
          doom3_vertex_number:=
            surface_stack(i).tri(fs).P(4-s);
            -- vertex nb for this surface; (4-s) to invert orientation
          face_0.P(s):= doom3_vertex_number + (p_mem+1); -- add the offset!
          -- Set the texture coordinates
          face_0.texture_edge_map(s):= surface_stack(i).uv(
            doom3_vertex_number + 1
          );
        end loop;
        f:= f + 1;
        model_stack(model_top).obj.face(f):= face_0;
--         put_line(
--            "surface " & i'img & " face " &
--            f'img & "texture: " & Get_surface_texture_name(i)
--         );
        Texture_name_hint(model_stack(model_top).obj.all, f, Get_surface_texture_name(i));
      end loop;
    end loop;
    model_stack(model_top).last_point:= p;
    model_stack(model_top).last_face:= f;
    model_stack(model_top).obj.centre:= main_centre;
    Set_name(model_stack(model_top).obj.all, pkg & "_$_" & Current_Model_name);
    Reset_surfaces;
  end Build_Model;

  -- Portals must be inserted as supplemental (transparent) faces of areas

  procedure Include_portals_to_areas is

    procedure Increment_number_of_portals(area: Integer) is
    begin
      if area in 0..area_top then
        model_stack(area_stack(area)).portals_to_be_added:=
          model_stack(area_stack(area)).portals_to_be_added + 1;
      end if;
    end;

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
          p:= m.obj.max_points;
          f:= m.obj.max_faces;
          new_obj:= new Object_3D(
            Max_points => p + 4*m.portals_to_be_added,
            Max_faces  => f  +   m.portals_to_be_added
          );
          -- Clone common part:
          Visual(new_obj.all)          := Visual(m.obj.all);
          new_obj.point(1..p)          := m.obj.point;
          new_obj.face(1..f)           := m.obj.face;
          new_obj.face_invariant(1..f) := m.obj.face_invariant;
          -- Dispose(m.obj);
          m.obj:= new_obj;
        end if;
      end;
    end loop;
    -- Details and linking are done with Complete_area_with_portals later.
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
      m.obj.face(f+1):= face_portal;
      p:= p + 4;
      f:= f + 1;
      m.last_point:= p;
      m.last_face:= f;
    end Include_Portals;

  begin
    for i in 1..iap_top loop
      Include_Portals(i, iap_stack(i).iap_pos, iap_stack(i).iap_neg, True);
      Include_Portals(i, iap_stack(i).iap_neg, iap_stack(i).iap_pos, False);
    end loop;
  end Complete_area_with_portals;

  bypass_portals: constant Boolean:= False;

  procedure YY_Accept is
    nb_points, nb_polys: Natural;
  begin
    Include_portals_to_areas;
    for i in 1..model_top loop
      if not (bypass_portals or model_stack(i).portals_to_be_added = 0) then
        Complete_area_with_portals(i);
      end if;
      Put_Line(Standard_Error,
        "Writing object file: " &
        model_stack(i).obj.ID
      );
      GLOBE_3D.IO.Save_file(model_stack(i).obj.all);
    end loop;
    Write_catalogue;
  end YY_Accept;

  procedure D3G_Init is
    use GL;
  begin
    main_centre:= -- !!
       (-1468.0+1294.0,+616.0,-1754.0-770.0); -- Delta4 magic!
    face_0.skin:= texture_only;
    face_0.whole_texture:= False;
    face_0.P(4):= 0; -- we have triangles, P(4) is set to 0
    face_0.texture_edge_map:= ((0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0));
    face_portal.skin:= invisible;
  end;

  procedure YY_Abort is
  begin
    null;
  end;

  procedure YY_Terminate is
  begin
    null;
  end;

  procedure Doom3_Comment(s: String) is -- dummy
  begin
    null;
  end;

end Doom3_Help;
