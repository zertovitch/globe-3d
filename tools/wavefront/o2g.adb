--  https://en.wikipedia.org/wiki/Wavefront_.obj_file

with GLOBE_3D;                          use GLOBE_3D;
with GLOBE_3D.Aux;                      use GLOBE_3D.Aux;
with GLOBE_3D.IO;                       use GLOBE_3D.IO;
with GL;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
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
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Positive,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

  mat_cat: Material_Maps.Map;

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

  procedure Load_mat_lib(m_name: String) is
    m: File_Type;
    mat_name: Unbounded_String;
  begin
    Put_Line("  Loading material library: " & m_name);
    Open(m, In_File, m_name);
    while not End_Of_File(m) loop
      declare
        l: constant String:= Get_Line(m);
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
      Put_Line(Current_Error, "  *** Warning, file not found: " & m_name);
  end Load_mat_lib;
  --
  vertices, faces, UV_pts: Natural:= 0;
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
        if l'Length >= 2 then
          declare
            l2: constant String:= l(l'First..l'First+2);
          begin
            if l2 = "vt " then
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
        end if;
      end;
    end loop;
    Close(o);
  end Acquire_texture_points;

  --
  --  Pass 3
  --
  procedure Acquire_object(o_name: String; uvs: UV_buffer; x: out Object_3D) is
    o: File_Type;
    vertex, face: Natural:= 0;
    current_face: Face_type;
    current_mat_idx: Natural:= 0;
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
                  if vec(i)=' ' then
                    P(dim):= Real'Value(vec(j..i-1));
                    j:= i+1;
                    dim:= dim + 1;
                  end if;
                end loop;
                x.point(vertex):= P;
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
                fd: constant String:= l(l'First+2..l'Last) & ' ';
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
              Put_Line("  Switch to material: " & To_String(r7));
              current_mat_idx:= mat_cat.Element(r7);
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
  begin
    Put_Line("Model name: " & model_name);
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
        opti: constant Object_3D:= Merge_triangles(x);
      begin
        Put_Line("Saving object with reduced merged faces:" & 
          Integer'Image(faces) &" ->" & 
          Integer'Image(opti.Max_faces)
        );
        Save_file(model_name & ".g3d", opti);
      end;
    end;
    Put_Line("Third pass done.");
  end Translate;

begin
  Put_Line("O2G");
  Put_Line("Wavefront Object (.obj), with Materials (.mtl), to GLOBE_3D object translator");
  if Argument_Count = 0 then
    Put_Line(Current_Error, "Syntax: o2g model[.obj]");
  else
    declare
      o_name: constant String:= Argument(1);
    begin
      if Index(o_name, ".obj") = 0 then
        Translate(o_name & ".obj");
      else
        Translate(o_name);
      end if;
    end;
  end if;        
end O2G;
