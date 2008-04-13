with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;
-- with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body Doom3_Help is

  package RIO is new Ada.Text_IO.Float_IO(Real);

  function Fac_trim( s: String ) return String is
  begin
    if pretty then
      return s;
    else
      return Trim(s, Both);
    end if;
  end Fac_trim;

  -- Minimal represention of a floating-point value
  -- accepted by an Ada compiler

  function Short_Float_Image( t: String ) return String is
    p,e,l: Natural;
    function Expo( ex: String ) return String is
      b: Natural;
    begin
      if ex'Length<=0 then
        return "";
      else
        b:= ex'Last+1;
        for i in ex'First+1..ex'Last loop
          if ex(i) = '+' or ex(i) = '0' then
            null;
          else
            b:= i;
          end if;
        end loop;
        if b = ex'Last+1 then -- E+000 : useless
          return "";
        else
          return 'e' & ex(b..ex'Last);
        end if;
      end if;
    --exception when others=> return ex;
    end Expo;
  begin
    e:= t'Last+1; -- a priori, no exponent
    for i in t'Range loop
      case t(i) is
        when '.' => p:= i;
        when 'e'|'E' => e:= i;
        when others => null;
      end case;
    end loop;
    l:= t'Last;
    -- preserve "1.x" (3 characters)
    for i in reverse p+2..e-1 loop
      if t(i)='0' then l:= i-1; else exit; end if;
    end loop;
    return t(t'First..l) & Expo(t(e..t'Last));
  end Short_Float_Image;

  function Image( r: Real ) return String is
    s: String(1..26);
  begin
    -- Try with no exponent
    RIO.Put(s,r,14,0);
    return Short_Float_Image(Trim(s,left));
  exception
    when Ada.Text_IO.Layout_Error =>
      -- With exponent etc.
      return Short_Float_Image(Real'Image(r));
  end Image;

  function Image( i: Integer ) return String is
  begin
    return Trim(Integer'Image(i),both);
  end Image;

  function Coords( p: Point_3D ) return String is
  begin
    return '(' & Image(p(0)) &
           ',' & Image(p(1)) &
           ',' & Image(p(2)) &
           ')';
  end Coords;

  function RGBA( p: Material_Float_vector ) return String is
  begin
    return '(' & Image(p(0)) &
           ',' & Image(p(1)) &
           ',' & Image(p(2)) &
           ',' & Image(p(3)) &
           ')';
  end RGBA;

  ---------- Internal Put's

  big_space: constant String(1..1024):= (others=> ' ');

  indent_block: constant:= 2;

  procedure iPut(s: String; as_comment: Boolean);

  procedure iNew_Line is
  begin
    Ada.Text_IO.New_Line;
    if pretty then
      iPut(big_space(1..indent * indent_block), as_comment=> False);
    end if;
  end iNew_Line;

  procedure iPut(s: String; as_comment: Boolean) is
    pr: Boolean:= False;
  begin
    if as_comment then
      for i in s'Range loop
        case s(i) is
          when ASCII.CR | ASCII.LF =>
            if not pr then -- avoid pairs
              iNew_Line;
              iPut("-- ", as_comment => False);
            end if;
            pr:= True;
          when others =>
            Ada.Text_IO.Put(s(i));
            pr:= False;
        end case;
      end loop;
    else
      Ada.Text_IO.Put(s);
    end if;
  end iPut;

  procedure iPut_Line(s: String; as_comment: Boolean) is
  begin
    if Pretty or not as_comment then
      iPut(s, as_comment);
    end if; -- else: discard comment
    iNew_Line;
  end iPut_Line;

  procedure Ada_Put_Triangle is
  begin
    Ada_Put("(");
    Ada_Put( v1 ); Ada_Put(",");
    Ada_Put( v2 ); Ada_Put(",");
    Ada_Put( v3 );
    Ada_Put(")");
  end;

  procedure Ada_Comment(s: String) is
  begin
    iPut_Line("-- " & s, as_comment => True);
  end;

  procedure Doom3_Info(s: String) is
  begin
    if pretty then
      Ada_Comment("Doom3 Info: [" & s & ']');
    end if;
  end;

  procedure Doom3_Comment(s: String) is
  begin
    if pretty then
      Ada_Comment("Doom3: [" & s & ']');
    end if;
  end;

  procedure Ada_Put(s: String) is
  begin
    iPut(s, as_comment => False);
  end;

  procedure Ada_Put(i: Integer) is
  begin
    Ada_Put(Image(i));
  end;

  procedure Ada_Put_Line(s: String) is
  begin
    iPut_Line(s, as_comment => False);
  end;

  procedure Ada_New_Line is
  begin
    iNew_Line;
  end;

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
          Ada_Put(
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
    Ada_Comment("Total: " & Image(n) & " distinct textures");
    Close(f);
  end Write_catalogue;

  --------------
  -- Surfaces --
  --------------

  type Surface is record
    texture_name : Unbounded_String;
    npoints      : Natural;
    nfaces       : Natural;
  end record;

  surface_stack: array(1..10_000) of Surface;

  surface_top: Natural:= 0;

  procedure Reset_surfaces is
  begin
    surface_top:= 0;
    surface_count:= 0;
  end;

  procedure Add_surface(
    name_with_quotes: String;
    npoints: Natural;
    nfaces : Natural
  )
  is
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
    end;
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
  end record;

  IAP_stack: array(1..10_000) of IAP;
  IAP_top: Natural:= 0;

  procedure Add_IAP is
  begin
    IAP_top:= IAP_top + 1;
    IAP_stack(IAP_top):= (iap_pos, iap_neg, iap_points);
  end Add_IAP;

  procedure Complete_area_with_portals(model_nb: Positive) is
    p,f: Natural:= 0;
    area: constant Natural:= model_stack(model_nb).area; -- supposed >= 0
    model_name: constant String:= S(model_stack(model_nb).name);

    function Area_name(area_nb: Integer) return String is
    begin
      -- !! return S(model_stack(area_stack(area_nb)).name)
      for i in 1..model_top loop
        if model_Stack(i).area = area_nb then
          return S(model_stack(i).name);
        end if;
      end loop;
      return "";
    end Area_name;

    procedure Include_Portals(iap_nb, side_area, other_side: Integer; reversed: Boolean) is
    begin
      if side_area /= area then
        return;
      end if;
      p:= model_stack(model_nb).last_point;
      f:= model_stack(model_nb).last_face;
      Ada_Put_Line(
        model_name &
        ".point(" &
        Image(p+1)&
        ".." & Image(p+4) & "):= (IAP("
        & Image(iap_nb) & ").point);"
      );
      if reversed then
        Ada_Put_Line(
          "face_portal.P:= ("&
          Image(p+4) & ',' & Image(p+3) & ',' &
          Image(p+2) & ',' & Image(p+1) & ");"
        );
      else
        Ada_Put_Line(
          "face_portal.P:= ("&
          Image(p+1) & ',' & Image(p+2) & ',' &
          Image(p+3) & ',' & Image(p+4) & ");"
        );
      end if;
      -- ** Direct connection (needs all areas being created):
      Ada_Put_Line("face_portal.connecting:= " & Area_name(other_side) & ";");
      -- ** Named connection:
      --  Ada_Put_Line("Portal_name_hint(" &
      --    model_name &".all," & Image(f+1) & ",""" &
      --    Area_name(other_side) & """);"
      --  );
      Ada_Put_Line( model_name & ".face(" & Image(f+1) & "):= face_portal;"
      );
      p:= p + 4;
      f:= f + 1;
      model_stack(model_nb).last_point:= p;
      model_stack(model_nb).last_face:= f;
    end Include_Portals;

  begin
    for i in 1..iap_top loop
      Include_Portals(i, iap_stack(i).iap_pos, iap_stack(i).iap_neg, True);
      Include_Portals(i, iap_stack(i).iap_neg, iap_stack(i).iap_pos, False);
    end loop;
  end Complete_area_with_portals;

  ------------------------------
  -- Source headers / footers --
  ------------------------------

  procedure Ada_Build_Model_Header is
  begin
    Ada_New_Line;
    Ada_Put_Line(Current_Model_name & ": p_Object_3D;");
    Ada_New_Line;
    indent:= indent + 1;
    Ada_Put(
      "procedure Build_" & Current_Model_name &
      "(area_number: Integer;" &
      " portals_to_be_added: Integer; centre: Point_3D) is"
    );
    Ada_Comment("area_number = -1 if not an area");
  end Ada_Build_Model_Header;

  max_faces, max_points: Natural:= 0;

  procedure Ada_Build_Model_Footer is
    p,f: Natural:= 0;
  begin
    indent:= indent - 1;
    if total_points > 0 then
      Ada_Put_Line("doom3_vertex_number: Integer;");
    end if;
    indent:= indent + 1;
    Ada_Put("begin");
    Ada_Comment("Build_" & Current_Model_name );
    Ada_Put_Line(
      Current_Model_name &
      ":= new Object_3D(" &
      Image(total_points) & "+4*portals_to_be_added," &
      Image(total_faces)  & "+portals_to_be_added" &
      ");"
    );
    max_faces:= Integer'Max(max_faces,total_faces);
    max_points:= Integer'Max(max_points,total_points);
    for i in 1..surface_count loop
      Ada_Comment("add 'surface #" & Image(i-1) & ''');
      indent:= indent + 1;
      Ada_Put_Line("for p in 1.." & Image(Get_surface_npoints(i)) & " loop");
      indent:= indent - 1;
      Ada_Put_Line(
        Current_Model_name &
        ".point(p+" &
        Image(p) & "):= Surface_" & Image(i-1) & "_vertices(p).pt;"
      );
      Ada_Put_Line("end loop;");
      indent:= indent + 1;
      Ada_Put_Line("for f in 1.." & Image(Get_surface_nfaces(i)) & " loop");
      indent:= indent + 1;
      Ada_Put_Line("for s in 1..3 loop");
      Ada_Put_Line(
        "doom3_vertex_number:= " &
        "Surface_" & Image(i-1) & "_triangles(f)(4-s);"
        -- vertex nb for this surface; (4-s) to invert orientation
      );
      Ada_Put_Line(
        "face_0.P(s):= " &
        "doom3_vertex_number" &
        " + " & Image(p+1) & -- add the offset!
        ';'
      );
      indent:= indent - 1;
      -- Set the texture coordinates
      Ada_Put_Line(
        "face_0.texture_edge_map(s):= Surface_" &
        Image(i-1) & "_vertices(" &
        "doom3_vertex_number + 1" &
         ").uv;"
      );
      Ada_Put_Line("end loop;");
      Ada_Put_Line(
        Current_Model_name &
        ".face(f+" & Image(f) & "):= face_0;"
      );
      indent:= indent - 1;
      Ada_Put_Line(
        "Texture_name_hint(" &
        Current_Model_name &
        ".all,f+" & Image(f) & ", """ &
        Get_surface_texture_name(i) &
        """);"
      );
      Ada_Put_Line("end loop;");
      p:= p + Get_surface_npoints(i);
      f:= f + Get_surface_nfaces(i);
    end loop;
    model_stack(model_top).last_point:= p;
    model_stack(model_top).last_face:= f;
    Ada_Put_Line(Current_Model_name & ".centre:= centre;");
    Ada_Put_Line(
      "Set_name(" & Current_Model_name & ".all,""" &
      pkg & "_$_" & Current_Model_name & """);"
    );
    Reset_surfaces;
    indent:= indent - 1;
    if pretty then
      Ada_Put_Line("end Build_" & Current_Model_name & ';' );
    else
      Ada_Put_Line("end;" );
    end if;
  end Ada_Build_Model_Footer;

  ---------

  procedure Ada_create is
  begin
    indent:= 1;
    Ada_New_Line;
    indent:= 2;
    Ada_Put_Line("procedure Create(");
    Ada_Put_Line("group   : in out GLOBE_3D.p_Object_3D_array;");
    Ada_Put_Line("BSP_tree: in out GLOBE_3D.BSP.p_BSP_node;");
    indent:= 1;
    Ada_Put_Line("centre  : in     GLOBE_3D.Point_3D");
    Ada_Put(")");
  end Ada_create;

  procedure Ada_Begin is
    pretty_mem: Boolean:= pretty;
  begin
    pretty:= True; -- For the spec. we want a pretty one !
    Ada_Put_Line("with GLOBE_3D, GLOBE_3D.BSP;");
    Ada_New_Line;
    Ada_Put_Line("package " & pkg &" is");
    Ada_Create;
    indent:= 0;
    Ada_Put_Line(";");
    Ada_New_Line;
    Ada_Put_Line("end " & pkg & ';');
    pretty:= pretty_mem;
    Ada_Put_Line("with GL, GLOBE_3D.Math, GL.Materials;");
    Ada_New_Line;
    indent:= 1;
    Ada_Put_Line("package body " & pkg &" is");
    Ada_Put_Line("-- Pretty output: " & Boolean'Image(pretty));
    Ada_Put_Line("-- Junk items' directories: " & Boolean'Image(junk_dirs));
    Ada_Put_Line("-- Areas only: " & Boolean'Image(areas_only));
    Ada_Put_Line("use GL, GLOBE_3D, GLOBE_3D.Math, GL.Materials;");
    Ada_New_Line;
    Ada_Put_Line("face_0 : Face_type; -- takes defaults values");
    Ada_Put_Line("face_portal : Face_type; -- prototype for portals");
    Ada_Put_Line("subtype Idx_3_array is GLOBE_3D.Index_array(1..3);");
    Ada_Put_Line("type Triangle_array is array(Positive range <>) of Idx_3_array;");
    Ada_Put_Line("type Surface_vertex_info is record pt: Point_3D; uv: Map_idx_pair; end record;");
    Ada_Put_Line("type Surface_vertex_array is array( Positive range <> ) of Surface_vertex_info;");
    Ada_New_Line;
    indent:= indent + 1;
    Ada_Put_Line("type IAP_type is record");
    Ada_Put_Line("positive_side, negative_side: Integer;");
    indent:= indent - 1;
    Ada_Put_Line("point: Point_3D_array(1..4);");
    Ada_Put_Line("end record;");
    Ada_Put_Line("type IAP_array is array( Positive range <> ) of IAP_type;");
    Ada_Put_Line("type p_IAP_array is access IAP_array;");
    Ada_Put("IAP: p_IAP_array;");
    Ada_Comment("We need this declaration early, hence the access");
    Ada_New_Line;
    indent:= indent + 1;
    Ada_Put_Line("type D3_BSP_node_type is record");
    Ada_Put_Line("normal: Vector_3D; distance: Real;");
    indent:= indent - 1;
    Ada_Put_Line("positive_child, negative_child: Integer;");
    Ada_Put_Line("end record;");
    Ada_Put_Line("type D3_BSP_node_array is array( Natural range <> ) of D3_BSP_node_type;");
    Ada_New_Line;
  end;

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
    Ada_Put_Line("Define_IAPs;");
    for i in 1..IAP_top loop
      -- We will have to add a portal to both sides' areas
      Increment_number_of_portals(IAP_stack(i).iap_pos);
      Increment_number_of_portals(IAP_stack(i).iap_neg);
    end loop;
  end Include_portals_to_areas;

  bypass_portals: constant Boolean:= False;

  procedure YY_Accept is
    nb_points, nb_polys: Natural;
  begin
    Ada_Create;
    indent:= 1;
    Ada_New_Line;
    indent:= 1;
    Ada_Put_Line("is");
    indent:= 1;
    nb_points:= 0;
    nb_polys := 0;
    indent:= 2;
    Ada_Put_Line("begin");
    Ada_Put_Line("group:= new Object_3D_array(1.." & Image(model_top) & ");");
    Ada_Put_Line("face_0.skin:= texture_only;");
    Ada_Put_Line("face_0.whole_texture:= False;");
    Ada_Put_Line("face_0.P(4):= 0;"); -- we have triangles, P(4) is set to 0
    Ada_Put_Line("face_0.texture_edge_map:= ((0.0,0.0),(0.0,0.0),(0.0,0.0),(0.0,0.0));");
    Ada_Put_Line("face_portal.skin:= invisible;");
    Include_portals_to_areas;

    for i in 1..model_top loop
      if bypass_portals then
        model_stack(i).portals_to_be_added:= 0;
      end if;
      Ada_Put_Line(
        "Build_" & S(model_stack(i).name) &
        '(' &
        Image(model_stack(i).area) &
        ',' &
        Image(model_stack(i).portals_to_be_added) &
        ",centre);"
      );
      Ada_Put_Line(
        "group(" &
        image(i) & "):= " & S(model_stack(i).name) &';'
      );
    end loop;
    -- All models and then also areas are allocated, we can link the
    -- portals directly (not through names)
    for i in 1..model_top loop
      if model_stack(i).portals_to_be_added > 0 then
        Complete_area_with_portals(i);
      end if;
    end loop;
    -- BSP nodes
    Ada_Put_Line("declare");
    Ada_Put_Line("bsp: array(D3_BSP_node'Range) of GLOBE_3D.BSP.p_BSP_Node;");
    Ada_Put_Line("area_to_model: constant array(0.."& Image(area_top) & ") of Positive:= (");
    for i in 0..area_top loop
      Ada_Put(Image(area_stack(i)));
      if i<area_top then Ada_Put_Line(","); end if;
    end loop;
    Ada_Put_Line(");");
    indent:= indent + 1;
    Ada_Put_Line("begin");
    Ada_Put_Line("for i in bsp'Range loop");
    Ada_Put_Line("bsp(i):= new GLOBE_3D.BSP.BSP_Node;");
    Ada_Put_Line("bsp(i).distance:= D3_BSP_node(i).distance - centre * D3_BSP_node(i).normal;");
    Ada_Put_Line("bsp(i).normal:= D3_BSP_node(i).normal;");
    indent:= indent - 1;
    Ada_Put_Line("bsp(i).node_id:= i;");
    Ada_Put_Line("end loop;");
    indent:= indent + 1;
    Ada_Put_Line("for i in bsp'Range loop");
    Ada_Put_Line("if D3_BSP_node(i).positive_child > 0 then");
    Ada_Put_Line("bsp(i).front_child:= bsp(D3_BSP_node(i).positive_child);");
    Ada_Put_Line("elsif D3_BSP_node(i).positive_child < 0 then");
    Ada_Put_Line("bsp(i).front_leaf:= group(area_to_model(-1-D3_BSP_node(i).positive_child));");
    Ada_Put_Line("end if;");
    Ada_Put_Line("if D3_BSP_node(i).negative_child > 0 then");
    Ada_Put_Line("bsp(i).back_child:= bsp(D3_BSP_node(i).negative_child);");
    Ada_Put_Line("elsif D3_BSP_node(i).negative_child < 0 then");
    Ada_Put_Line("bsp(i).back_leaf:= group(area_to_model(-1-D3_BSP_node(i).negative_child));");
    indent:= indent - 1;
    Ada_Put_Line("end if;");
    Ada_Put_Line("end loop;");
    Ada_Put_Line("BSP_tree:= bsp(bsp'First);");
    Ada_Put_Line("end;");
    indent:= 1;
    Ada_New_Line;
    indent:= 0;
    Ada_Put_Line("end Create;");
    Ada_Put_Line("end " & pkg & ';');
    Ada_comment("Largest amount of faces for an object (model) :" & Image(max_faces));
    Ada_comment("Largest amount of points for an object (model) :" & Image(max_points));
    Write_catalogue;
    Ada_comment(blurb);
  end YY_Accept;

  procedure YY_Abort is
  begin
    null;
  end;

  procedure YY_Terminate is
  begin
    null;
  end;

end Doom3_Help;
