--  https://en.wikipedia.org/wiki/Wavefront_.obj_file

with GLOBE_3D;                          use GLOBE_3D;
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

  --
  --  Pass 2
  --
  procedure Acquire_object(o_name: String; x: out Object_3D) is
    o: File_Type;
    vertex, face: Natural:= 0;
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
                f: Face_type;
                type Kind_Type is (
                  Vertex_Indices, 
                  Vertex_Texture_Coordinate_Indices, 
                  Vertex_Normal_Indices
                );
                kind: Kind_Type:= Vertex_Indices;
              begin
                j:= fd'First;
                for i in fd'Range loop
                  if fd(i) not in '0'..'9' then
                    case kind is
                      when Vertex_Indices =>
                        if dim > 4 then 
                          raise Constraint_Error with "dim > 4 not supported";
                        end if;
                        f.P(dim):= Integer'Value(fd(j..i-1));
                      when Vertex_Texture_Coordinate_Indices =>
                        null; -- tbd !!
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
              end;
            elsif l2 = "vt" then
              null; -- !!
            end if;
          end;
        end if;
      end;
    end loop;
    Close(o);
  end Acquire_object;

  procedure Translate(o_name: String) is
  begin
    Count_items(o_name);
    Put_Line(
      "First pass done," &
      Integer'Image(vertices) & " vertices," &
      Integer'Image(faces)    & " faces," &
      Integer'Image(UV_pts)   & " texture points."
    );
    declare
      x: Object_3D(vertices, faces);
    begin
      Acquire_object(o_name, x);
    end;
    Put_Line("Second pass done.");
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
