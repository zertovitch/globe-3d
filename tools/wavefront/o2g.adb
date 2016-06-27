--  https://en.wikipedia.org/wiki/Wavefront_.obj_file

with GLOBE_3D;                          use GLOBE_3D;
with GLOBE_3D.Aux;                      use GLOBE_3D.Aux;
with GLOBE_3D.IO;                       use GLOBE_3D.IO;

with GL, GL.Math;

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress;                      use Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;        use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure O2G is
  --
  mat_idx: Natural:= 0;
  --
  type Mat_pack is record
    diffuse_texture  : Unbounded_String;
    specular_texture : Unbounded_String;
  end record;
  --
  mat_stack: array(1..10_000) of Mat_pack;
  --
  package Material_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Positive,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

  mat_cat: Material_Maps.Map;

  package Tex_Vectors is new Ada.Containers.Vectors(Positive, Unbounded_String);
  
  tex_list: Tex_Vectors.Vector;

  function No_ext(s: String) return String is
    l: Natural:= s'Last;
  begin
    for i in reverse s'Range loop
      if s(i)='.' then
        l:= i-1;
      end if; 
    end loop;
    return s(s'First..l);
  end;

  --  # 3ds Max Wavefront OBJ Exporter puts some garbage in front of lines...
  function My_trim(s: String) return String is
    f: Natural:= S'First;
  begin
    for i in s'Range loop
      case s(i) is
        when ' ' | ASCII.HT =>
          f:= i+1;
        when others =>
          exit;
      end case;
    end loop;
    return s(f .. s'Last);
  end;

  archive : Zip_Create_info;

  procedure Reg_tex(t_name: String) is
    u_t_name: constant Unbounded_String:= To_Unbounded_String(t_name);
    use Tex_Vectors;
    c: Cursor;
  begin
    c:= tex_list.Find(u_t_name);
    if c = No_Element then
      tex_list.Append(u_t_name);
    end if;
  end Reg_tex;


  procedure Pack_textures is
    procedure Pack_texture(c: Tex_Vectors.Cursor) is
      t_name: constant String:= To_String(Tex_Vectors.Element(c));
    begin
      Add_File(archive, t_name);
    exception
      when Name_Error =>
        Put_Line(Current_Error, "*** Warning: texture file not found: " & t_name);
    end Pack_texture;
  begin
    tex_list.Iterate(Pack_texture'Access);
  end Pack_textures;

  procedure Load_mat_lib(m_name: String) is
    m: File_Type;
    mat_name: Unbounded_String;
  begin
    Put_Line("  Loading material library: " & m_name);
    Add_File(archive, m_name);
    Open(m, In_File, m_name);
    while not End_Of_File(m) loop
      declare
        l: constant String:= My_trim(Get_Line(m));
      begin
        if l'Length >= 7 then
          declare
            l7: constant String:= l(l'First..l'First+6);
            r7: constant String:= l(l'First+7 .. l'Last);
            r7n: constant String:= No_ext(r7);
          begin
            if l7 = "newmtl " then
              mat_name:= To_Unbounded_String(r7);
              Put_Line("    New material: " & r7);
              mat_idx:= mat_idx + 1;
              mat_cat.Include(mat_name, mat_idx);
            elsif l7 = "map_Kd " then
              Put_Line("      Diffuse texture : " & r7n);
              mat_stack(mat_idx).diffuse_texture:= To_Unbounded_String(r7n);
              Reg_tex(r7);
            elsif l7 = "map_Ks " then
              Put_Line("      Specular texture: " & r7n);
              mat_stack(mat_idx).specular_texture:= To_Unbounded_String(r7n);
            end if;
          end;
        end if;
      end;
    end loop;
    Close(m);
  exception
    when Name_Error =>
      Put_Line(Current_Error, "  *** Warning: material file not found: " & m_name);
  end Load_mat_lib;
  --
  vertices, faces, UV_pts, total_lines, pass_2_lines, pass_3_lines: Natural:= 0;
  --
  --  Pass 1
  --
  procedure Count_items(o_name: String) is
    o: File_Type;
  begin
    Open(o, In_File, o_name);
    while not End_Of_File(o) loop
      declare
        l: constant String:= Get_Line(o);
      begin
        total_lines:= total_lines + 1;
        if l'Length >= 2 then
          declare
            l2: constant String:= l(l'First..l'First+1);
          begin
            if l2 = "v " then
              vertices:= vertices + 1;
            elsif l2 = "f " then
              faces:= faces + 1;
            elsif l2 = "vt" then
              UV_pts:= UV_pts + 1;
            end if;
          end;
          if l'Length >= 7 and then l(l'First..l'First+6) = "mtllib " then
            Load_mat_lib(l(l'First+7 .. l'Last));
          end if;
        end if;
      end;
    end loop;
    Close(o);
  end Count_items;

  type UV_buffer is array(Natural range <>) of Map_idx_pair;

  --
  --  Pass 2
  --
  procedure Acquire_texture_points(o_name: String; uvs: out UV_buffer) is
    o: File_Type;
    tex_vertex: Natural:= 0;
  begin
    Open(o, In_File, o_name);
    while not End_Of_File(o) loop
      declare
        l: constant String:= Get_Line(o);
      begin
        pass_2_lines:= pass_2_lines + 1;
        if l'Length >= 3 and then l(l'First..l'First+2) = "vt " then
          --  vt -0.6834 0.5959
          tex_vertex:= tex_vertex + 1;
          declare
            vec: constant String:= l(l'First+3..l'Last) & ' ';
            j: Natural:= 0;
            dim : Positive:= 1;
            UV: Map_idx_pair;
            x: Real;
          begin
            j:= vec'First;
            for i in vec'Range loop
              if vec(i)=' ' then
                x:= Real'Value(vec(j..i-1));
                if dim = 1 then
                  UV.U:= x;
                else
                  UV.V:= x;
                end if;
                j:= i+1;
                dim:= dim + 1;
              end if;
            end loop;
            uvs(tex_vertex):= UV;
          end;
        end if;
      end;
    end loop;
    Close(o);
  end Acquire_texture_points;

  main_centre: GL.Double_Vector_3D:= (0.0, 0.0, 0.0);
  scale: Real:= 1.0;

  --
  --  Pass 3
  --
  procedure Acquire_object(o_name: String; uvs: UV_buffer; x: out Object_3D) is
    o: File_Type;
    vertex, face: Natural:= 0;
    current_face: Face_type;
    current_mat_idx: Natural:= 0;
    use GL.Math;
  begin
    --  Face properties may change upon "usemtl" commands.
    current_face.whole_texture:= False;
    current_face.skin:= texture_only;
    --
    Open(o, In_File, o_name);
    while not End_Of_File(o) loop
      declare
        l: constant String:= Get_Line(o);
      begin
        pass_3_lines:= pass_3_lines + 1;
        if l'Length >= 2 then
          declare
            l2: constant String:= l(l'First..l'First+1);
          begin
            if l2 = "v " then
              --  v 3.357995 0.455045 2.236485
              vertex:= vertex + 1;
              declare
                vec: constant String:= l(l'First+2..l'Last) & ' ';
                j: Natural:= 0;
                dim : Natural:= 0;
                P: GL.Double_Vector_3D;
              begin
                j:= vec'First;
                for i in vec'Range loop
                  if vec(i)=' ' and then i > vec'First then
                    P(dim):= Real'Value(vec(j..i-1));
                    j:= i+1;
                    dim:= dim + 1;
                  end if;
                end loop;
                x.point(vertex):=  scale * P;
              end;
            elsif l2 = "f " then
              --  f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3 ....
              --           ^ normal vector (ignored)
              --       ^ vertex in texture
              --    ^ vertex in 3D space
              --  Examples
              --  ========
              --  f 5939/3607/2716 10364/3609/2715 10520/3608/2715 10519/3606/2716
              --  f 16//11 12//11 11//11 15//11
              face:= face + 1;
              declare
                fd: constant String:= My_trim(l(l'First+2..l'Last) & ' ');
                j: Natural:= 0;
                dim : Positive:= 1;
                f: Face_type:= current_face;
                type Kind_Type is (
                  Vertex_Indices, 
                  Vertex_Texture_Coordinate_Indices, 
                  Vertex_Normal_Indices
                );
                kind: Kind_Type:= Vertex_Indices;
                idx: Natural;
              begin
                f.P(4):= 0;
                f.texture_edge_map(4):= (0.0, 0.0);
                j:= fd'First;
                for i in fd'Range loop
                  if fd(i) not in '0'..'9' then
                    idx:= Integer'Value('0' & fd(j..i-1));
                    --  Trick: we add 0 in front for the case of an empty string 
                    --  (between the "//" in "f 16//11")
                    case kind is
                      when Vertex_Indices =>
                        if dim > 4 then 
                          raise Constraint_Error with "dim > 4 not supported";
                        end if;
                        f.P(dim):= idx;
                      when Vertex_Texture_Coordinate_Indices =>
                        if idx = 0 then
                          f.whole_texture:= True;
                        else
                          f.whole_texture:= False;
                          f.texture_edge_map(dim):= uvs(idx);
                        end if;
                      when Vertex_Normal_Indices =>
                        null;  --  Needless, GLOBE_3D recomputes that.
                    end case;
                    j:= i+1;
                    case fd(i) is
                      when ' ' =>
                        dim:= dim + 1;
                        kind:= Vertex_Indices;
                      when '/' =>
                        kind:= Kind_Type'Succ(kind);
                      when others =>
                        null;  --  error in .obj file
                    end case;
                  end if;
                end loop;
                x.face(face):= f;
                Texture_name_hint(x, face, To_String(mat_stack(current_mat_idx).diffuse_texture));
                Specular_name_hint(x, face, To_String(mat_stack(current_mat_idx).specular_texture));
              end;
            end if;
          end;
          if l'Length >= 7 and then l(l'First..l'First+6) = "usemtl " then
            declare
              r7: constant Unbounded_String:= To_Unbounded_String(l(l'First+7 .. l'Last));
            begin
              Put_Line(
                "  Line" & Integer'Image(pass_3_lines) & 
                ": switching to material: " & To_String(r7));
              current_mat_idx:= mat_cat.Element(r7);
              if mat_stack(current_mat_idx).diffuse_texture = "" then
                current_face.skin:= colour_only;  
                current_face.colour:= (0.5, 0.1, 0.1); --  !! used colours parsed in .mtl
              else
                current_face.skin:= texture_only;
                current_face.whole_texture:= False;
              end if;
            end;
          end if;
        end if;
      end;
    end loop;
    Close(o);
  end Acquire_object;

  procedure Translate(o_name: String) is
    model_name: constant String:= o_name(o_name'First..o_name'Last-4);
    --  "delta4g1_$_area1"; --  for test
    zip_file : aliased File_ZipStream; -- Archive is a file
    cmd: File_Type;
    function Argument_Chain(n: Natural:= 1) return String is
    begin
      if n > Argument_Count then
        return "";
      else
        return Argument(n) & ' ' & Argument_Chain(n => n+1);
      end if;
    end Argument_Chain;
  begin
    Put_Line("Model name: " & model_name);
    --  For convenience, make a Zip archive with the .g3d object, textures, and original data
    Create (archive, zip_file'Unchecked_Access, model_name & ".zip", Deflate_1 );
    Count_items(o_name);
    Put_Line(
      "First pass (sizes) done," &
      Integer'Image(vertices) & " vertices," &
      Integer'Image(faces)    & " faces," &
      Integer'Image(UV_pts)   & " texture points."
    );
    declare
      x: Object_3D(vertices, faces);
      uvs: UV_buffer(1..UV_pts);
    begin
      Acquire_texture_points(o_name, uvs);
      Put_Line("Second pass (acquisition of texture points) done.");
      Acquire_object(o_name, uvs, x);
      Set_name(x, model_name);
      declare
        opti: Object_3D:= Merge_triangles(x);
      begin
        opti.centre:= main_centre;
        Put_Line("Saving object with reduced merged faces:" & 
          Integer'Image(faces) &" ->" & 
          Integer'Image(opti.Max_faces)
        );
        Save_file(model_name & ".g3d", opti);
      end;
    end;
    Put_Line("Third pass done.");
    Put_Line("Archiving now the .g3d object, textures, and original data into a zip file.");
    Add_File(archive, model_name & ".g3d");
    Add_File(archive, o_name);
    Pack_textures;
    --  Create a command line script for lazy Windows users, for viewing the object.
    Create(cmd, Out_File, model_name & ".cmd");
    Put_Line(cmd, "GLOBE_3D_Demo.exe -load=" & model_name);
    Close(cmd);
    Add_File(archive, model_name & ".cmd");
    --  Create a virtual "readme" file.
    Add_String(archive,
      "O2G (see ./tools/wavefront)" & ASCII.LF &
      "Wavefront Object (.obj), with Materials (.mtl), to GLOBE_3D object translator." & 
      ASCII.LF & ASCII.LF &
      "O2G was invoked with the following command line: o2g " & ASCII.LF &
      Argument_Chain,
      model_name & "_readme.txt", 
      Creation_time => Zip.Convert(Ada.Calendar.Clock)
    );
    Finish (archive);
    Put_Line("Zip archive created. Name: " & Name(archive));
  end Translate;

  procedure Syntax is
  begin
    Put_Line(Current_Error, "Syntax: o2g model[.obj]");
    New_Line(Current_Error );
    Put_Line(Current_Error, "Options:" );
    Put_Line(Current_Error, "   -S(x,y,z)    : shifts centering by (x,y,z)");
    Put_Line(Current_Error, "   -Kscale      : set scaling factor");
    New_Line;
    Put_Line(Current_Error, "Output: model.g3d, model.zip");
  end Syntax;

  procedure Set_new_centre(ps: String) is
    use GL.Math;
    p: GLOBE_3D.Point_3D;
    j1,j2: Natural;
  begin
    j1:= ps'First;
    for d in main_centre'Range loop
      j2:= j1;
      while
        j2 <= ps'Last and then
        not (ps(j2) = ',' or ps(j2) = ';' or ps(j2) = ')')
      loop
        j2:= j2 + 1;
      end loop;
      p(d):= Real'Value(ps(j1..j2-1));
      j1:= j2+1;
    end loop;
    main_centre:= main_centre + p;
  end Set_new_centre;

begin
  Put_Line("O2G");
  Put_Line("Wavefront Object (.obj), with Materials (.mtl), to GLOBE_3D object translator");
  if Argument_Count = 0 then
    Syntax;
    return;
  end if;
  
  for i in 1..Argument_Count loop
    declare
      arg  : constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
          when 'S' =>
            if arg'length < 4 then
              Syntax;
              return;
            end if;
            if arg(3) = '(' then -- -S(x,y,z)
              Set_new_centre(arg(4..arg'Last));
            else
              Syntax;
              return;
            end if;
          when 'K' =>            -- -Kscale
            scale:= Real'Value(arg(3..arg'Last));
          when others =>  -- includes "-h", "/?" etc.
            Syntax;
            return;
        end case;
      else -- no option
        declare
          o_name: constant String:= Argument(i);
        begin
          if Index(o_name, ".obj") = 0 then
            Translate(o_name & ".obj");
          else
            Translate(o_name);
          end if;
        end;
      end if;
    end;        
  end loop;
--  exception
--    when e: others =>
--      Raise_Exception(Exception_Identity(e), Exception_Message(e) & 
--        " Pass 1:" & Integer'Image(total_lines) &
--        " Pass 2:" & Integer'Image(pass_2_lines) &
--        " Pass 3:" & Integer'Image(pass_3_lines)
--      );
end O2G;
