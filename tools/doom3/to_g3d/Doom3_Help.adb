with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;
-- with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body Doom3_Help is

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
    l: Natural;
  begin
    if not junk_dirs then
      return s;
    else
      l:= s'First;
      for i in s'Range loop
        if s(i)='/' or s(i)='\' then
          l:= i+1;
        end if;
      end loop;
      return s(l..s'Last);
    end if;
  end Junk;

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

  current_area_number: Integer:= -1;

  procedure Add_Model(name_with_quotes: String) is
    un: Unbounded_String;
    name: constant String:= Strip_quotes(name_with_quotes);
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
        end if;
      else
        un:= U(name);
      end if;
      mt.name:= un;
    end;
  end Add_Model;

  function Current_Model_name return String is
  begin
    return S(model_stack(model_top).name);
  end;

  -- Current texture - see also Surfaces

  current_texture: Unbounded_String;

  procedure Set_current_texture(name_with_quotes: String) is
    name: constant String:= Junk(Strip_quotes(name_with_quotes));
  begin
    current_texture:= U(name);
  end;

  function Get_current_texture return String is
  begin
    return S(current_texture);
  end;

  -----------------------
  -- Texture catalogue --
  -----------------------
  -- Same textures appear numerous times in a .proc file, then
  -- it is difficult to figure out how many and which ones they are
  type dir_node;
  type p_dir_node is access dir_node;

  type dir_node(name_len: natural) is record
    left, right : p_dir_node;
    name        : string(1..name_len);
  end record;

  catalogue: p_dir_node:= null;

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

    procedure Traverse( p: p_dir_node ) is
    begin
      if p /= null then
        Traverse(p.left);
        Put_Line(f,p.name);
        n:= n + 1;
        Traverse(p.right);
      end if;
    end Traverse;

  begin
    Create(f,out_file, "textures.txt");
    Traverse(catalogue);
    New_Line(f);
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
  end;

  procedure Add_surface(
    name_with_quotes: String;
    npoints: Natural;
    nfaces : Natural
  )
  is
    name: constant String:= Junk(Strip_quotes(name_with_quotes));
  begin
    Insert(name,catalogue);
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

  -- Portals must be inserted as supplemental (transparent) faces of areas

  bypass_portals: constant Boolean:= False;

  procedure YY_Accept is
    nb_points, nb_polys: Natural;
  begin
    Write_catalogue;
  end YY_Accept;

  procedure D3G_Init is
  begin
    null;
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
