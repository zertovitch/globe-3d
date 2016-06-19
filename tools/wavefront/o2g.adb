--  https://en.wikipedia.org/wiki/Wavefront_.obj_file

with GLOBE_3D;                          use GLOBE_3D;
with GLOBE_3D.Aux;                      use GLOBE_3D.Aux;
with GLOBE_3D.IO;                       use GLOBE_3D.IO;
with GL;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure O2G is
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
  begin
    --  !! Will change upon "usemtl" commands
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
              --  f 5939/3607/2716 10364/3609/2715 10520/3608/2715 10519/3606/2716
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
                idx: Positive;
              begin
                f.P(4):= 0;
                f.texture_edge_map(4):= (0.0, 0.0);
                j:= fd'First;
                for i in fd'Range loop
                  if fd(i) not in '0'..'9' then
                    idx:= Integer'Value(fd(j..i-1));
                    case kind is
                      when Vertex_Indices =>
                        if dim > 4 then 
                          raise Constraint_Error with "dim > 4 not supported";
                        end if;
                        f.P(dim):= idx;
                      when Vertex_Texture_Coordinate_Indices =>
                        f.texture_edge_map(dim):= uvs(idx);
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
                Texture_name_hint(x, face, "face1"); --  !! temp, should track "usemtl"
              end;
            end if;
          end;
        end if;
      end;
    end loop;
    Close(o);
  end Acquire_object;

  procedure Translate(o_name: String) is
    object_name: constant String:= "delta4g1_$_area1"; --  !! for test
      -- o_name(o_name'First..o_name'Last-4);
  begin
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
      Set_name(x, object_name);
      Save_file(object_name & ".g3d", x);
    end;
    Put_Line("Third pass done.");
  end Translate;

begin
  if Argument_Count = 0 then
    Put_Line(Standard_Error, "Syntax: o2g model[.obj]");
  else
    declare
      o_name: constant String:= Argument(1);
    begin
      if Index(".obj", o_name) = 0 then
        Translate(o_name & ".obj");
      else
        Translate(o_name);
      end if;
    end;
  end if;        
end O2G;
