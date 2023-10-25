with Ada.Command_Line,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

package body VRML_Help is

  use Ada.Command_Line, Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Unbounded;

  package RIO is new Ada.Text_IO.Float_IO (Real);

  function Fac_Trim (s : String; force : Boolean := False) return String is
  begin
    if pretty and not force then
      return s;
    else
      return Trim (s, Both);
    end if;
  end Fac_Trim;

  function Image (r : Real; force : Boolean := False) return String is
    s : String (1 .. 11);
  begin
    RIO.Put (s, r, 6, 0);
    declare
      t : constant String := Fac_Trim (s, force);
      l : Natural := t'Last;
      p : Natural;
    begin
      for i in t'Range loop
        if t (i) = '.' then p := i; exit; end if;
      end loop;
      --  preserve "1.x" (3 characters)
      for i in reverse p + 2 .. t'Last loop
        if t (i) = '0' then l := i - 1; else exit; end if;
      end loop;
      return t (1 .. l);
    end;
  exception
    when Ada.Text_IO.Layout_Error =>
      return Fac_Trim (Real'Image (r), force);
  end Image;

  function Coords (p : Point_3D) return String is
  begin
    return '(' & Image (p (0), force => True) &
           ',' & Image (p (1)) &
           ',' & Image (p (2)) &
           ')';
  end Coords;

  function RGBA (p : Material_Float_Vector) return String is
  begin
    return '(' & Image (p (0), force => True) &
           ',' & Image (p (1)) &
           ',' & Image (p (2)) &
           ',' & Image (p (3)) &
           ')';
  end RGBA;

  ----------  Internal Put's

  big_space : constant String (1 .. 1024) := (others => ' ');

  indent_block : constant := 2;
  table_area : Boolean := False;

  procedure iPut (s : String; as_comment : Boolean);

  buffer : Unbounded_String;

  procedure Flush is
  begin
    Ada.Text_IO.Put (Trim (To_String (buffer), Right));
    buffer := Null_Unbounded_String;
  end Flush;

  procedure iNew_Line is
  begin
    Flush;
    Ada.Text_IO.New_Line;
    if pretty or not table_area then
      iPut (big_space (1 .. indent * indent_block), as_comment => False);
    end if;
  end iNew_Line;

  procedure iPut (s : String; as_comment : Boolean) is
    pr : Boolean := False;
  begin
    if as_comment then
      for i in s'Range loop
        case s (i) is
          when ASCII.CR | ASCII.LF =>
            if not pr then  --  avoid pairs
              iNew_Line;
              iPut ("--  ", as_comment => False);
            end if;
            pr := True;
          when others =>
            buffer := buffer & s (i);
            pr := False;
        end case;
      end loop;
    else
      buffer := buffer & s;
    end if;
  end iPut;

  procedure iPut_Line (s : String; as_comment : Boolean) is
  begin
    iPut (s, as_comment);
    iNew_Line;
  end iPut_Line;

  ---------

  procedure Ada_Create is
  begin
    indent := 1;
    Ada_New_Line;
    indent := 2;
    Ada_Put_Line ("procedure Create");
    Ada_Put_Line ("(object  : in out GLOBE_3D.p_Object_3D;");
    Ada_Put_Line (" scaling :        GLOBE_3D.Real;");
    indent := 1;
    Ada_Put      (" centre  :        GLOBE_3D.Point_3D)");
  end Ada_Create;

  function Pkg return String is
  begin
    if has_input then
      for ai in 1 .. Argument_Count loop
        declare
          s : constant String := Argument (ai);
        begin
          if s (s'First) not in '/' | '-' then  --  Skip options
            for i in s'Range loop
              if s (i) = '.' then  --  skip file extension
                return s (s'First .. i - 1);
              end if;
            end loop;
            return s;
          end if;
        end;
      end loop;
    end if;
    return "VRML_Scene";
  end Pkg;

  procedure YY_Accept is
    nb_points, nb_polys : Natural;
  begin
    table_area := False;
    Ada_Create;
    indent := 1;
    Ada_New_Line;
    indent := 2;
    Ada_Put_Line ("is");
    indent := 1;
    Ada_Put_Line ("face_0 : Face_Type;  --  Takes defaults values");
    nb_points := 0;
    nb_polys := 0;
    for i in 1 .. sepa_count loop
      nb_points := nb_points + sepa_points (i);
      nb_polys := nb_polys  + sepa_polys (i);
    end loop;
    indent := 2;
    Ada_Put_Line ("begin");
    indent := 3;
    Ada_Put_Line ("object :=");
    indent := 2;
    Ada_Put_Line ("new Object_3D (Max_points =>" & Integer'Image (nb_points) &
                 ", Max_faces =>" & Integer'Image (nb_polys) &
                 ");");
    Ada_Put_Line ("object.centre := centre;");
    if has_input then
      Ada_Put_Line ("Set_Name (object.all, """ & Pkg & """);");
    else
      Ada_Put_Line ("Set_Name (object.all, ""World translated from VRML"");");
    end if;
    Ada_Put_Line ("face_0.skin := material_only;");

    --  VRML DEFAULTS:
    --     Material {
    --          ambientColor   0.2 0.2 0.2    # MFColor
    --          diffuseColor   0.8 0.8 0.8    # MFColor
    --          specularColor  0 0 0          # MFColor
    --          emissiveColor  0 0 0          # MFColor
    --          shininess      0.2            # MFFloat
    --          transparency   0              # MFFloat
    --     }

    Ada_Put_Line ("face_0.material := VRML_Defaults;");

    nb_points := 0;
    nb_polys  := 0;
    for i in 1 .. sepa_count loop
      Ada_Comment ("Creating separator #" & i'Image);
      if sepa_points (i) > 0 then
        indent := indent + 1;
        Ada_Put_Line ("if Almost_Zero (scaling - 1.0) then");
        indent := indent - 1;
        Ada_Put_Line (
          "object.point (" &
          Trim (Integer'Image (nb_points + 1), Left) &
          " .." &
          Integer'Image (nb_points + sepa_points (i)) &
          ") := coord_" & Trim (i'Image, Left) &
          ';'
        );
        indent := indent + 1;
        Ada_Put_Line ("else");
        indent := indent + 1;
        Ada_Put_Line ("for p in 1 .."  & Integer'Image (sepa_points (i)) & " loop");
        indent := indent - 1;
        Ada_Put_Line
          ("object.point (" & Trim (nb_points'Image, Left) &
           " + p) := scaling * coord_" & Trim (i'Image, Left) & " (p);");
        indent := indent - 1;
        Ada_Put_Line ("end loop;");
        Ada_Put_Line ("end if;");
      end if;
      if sepa_matos_defined (i) then
        Ada_Put_Line
          ("face_0.material := matos_" & Trim (i'Image, Left) & ';');
      end if;
      if sepa_polys (i) > 0 then
        indent := indent + 1;
        Ada_Put_Line ("for f in 1 .." & Integer'Image (sepa_polys (i)) & " loop");
        Ada_Put_Line
          ("face_0.P := idx_" & Trim (i'Image, Left) & " (f);");
        indent := indent - 1;
        Ada_Put_Line
          ("object.face (" & Trim (nb_polys'Image, Left) & " + f) := face_0;");
        Ada_Put_Line ("end loop;");
      end if;

      nb_points := nb_points + sepa_points (i);
      nb_polys  := nb_polys  + sepa_polys (i);
    end loop;

    indent := 0;
    Ada_Put_Line ("end Create;");
    Ada_Put_Line ("end " & Pkg & ';');
    Ada_Comment (blurb);
  end YY_Accept;

  procedure YY_Abort is
  begin
    null;
  end YY_Abort;

  procedure YY_Terminate is
  begin
    null;
  end YY_Terminate;

  procedure Ada_Comment (s : String) is
  begin
    iPut_Line ("--  " & s, as_comment => True);
  end Ada_Comment;

  procedure VRML_Info (s : String) is
  begin
    if s'Length > 0
      and then s (s'Last) in ASCII.CR | ASCII.LF
    then
      VRML_Info (s (s'First .. s'Last - 1));
    else
      Ada_Comment ("VRML Info: [" & s & ']');
    end if;
  end VRML_Info;

  header_done : Boolean := False;

  procedure VRML_Comment (s : String) is
  begin
    if s'Length > 0
      and then s (s'Last) in ASCII.CR | ASCII.LF
    then
      VRML_Comment (s (s'First .. s'Last - 1));
    else
      Ada_Comment ("VRML: [" & s & ']');
      if s'Length >= 4
        and then s (s'First .. s'First + 4) = "#VRML"
        and then not header_done
      then
        Ada_Put_Line ("with GLOBE_3D;");
        Ada_New_Line;
        Ada_Put_Line ("package " & Pkg & " is");
        Ada_Create;
        Ada_Put (";");
        indent := 0;
        Ada_New_Line;
        Ada_New_Line;
        Ada_Put_Line ("end " & Pkg & ';');
        Ada_Put_Line ("with GL.Materials, GL.Math;");
        Ada_New_Line;
        Ada_Put_Line ("package body " & Pkg & " is");
        Ada_Put_Line ("  --  Pretty output: " & Boolean'Image (pretty));
        Ada_New_Line;
        Ada_Put_Line ("  use GL, GL.Materials, GL.Math, GLOBE_3D;");
        Ada_New_Line;
        header_done := True;
        table_area := True;
      end if;
    end if;
  end VRML_Comment;

  procedure Ada_Put (s : String) is
  begin
    iPut (s, as_comment => False);
  end Ada_Put;

  procedure Ada_Put_Line (s : String) is
  begin
    iPut_Line (s, as_comment => False);
  end Ada_Put_Line;

  procedure Ada_New_Line is
  begin
    iNew_Line;
  end Ada_New_Line;

  ig : Integer := 0;

  procedure Reset_Index_Grouping is
  begin
    ig := 0;
  end Reset_Index_Grouping;

  W2A_Not_supported_Polygon : exception;

  point_buffer : array (1 .. 4) of Natural;
  idx_point_buffer : Natural := 0;

  procedure Point_Index (i : Integer) is

    procedure Store (v : Natural) is
    begin
      idx_point_buffer := idx_point_buffer + 1;
      point_buffer (idx_point_buffer) := v;
    end Store;

  begin
    if ig = 0 then
      Ada_Put ("(");
    end if;
    ig := ig + 1;
    if (ig > 4 + 1) or (ig = 4 + 1 and i /= -1) then
      --  at least a pentagon
      raise W2A_Not_supported_Polygon;
    elsif ig = 4 + 1 then
      null;  --  just ignore the -1
    else
      if i = -1 then
        Store (0);  --  "dead" edge
      else
        Store (i + 1 + idx_last_sepa);
      end if;
    end if;
    if i = -1 then
      ig := 0;
      case current_shape_hints.ordering is
        when VRML_CLOCKWISE =>
          for pt in reverse 1 .. idx_point_buffer loop
            Ada_Put (Fac_Trim (point_buffer (pt)'Image, force => pt = idx_point_buffer));
            if pt > 1 then
              Ada_Put (",");
            end if;
          end loop;
        when VRML_UNKNOWN_ORDERING |
             VRML_COUNTERCLOCKWISE =>
          for pt in 1 .. idx_point_buffer loop
            Ada_Put (Fac_Trim (point_buffer (pt)'Image, force => pt = 1));
            if pt < idx_point_buffer then
              Ada_Put (",");
            end if;
          end loop;
      end case;
      idx_point_buffer := 0;
      Ada_Put (")");
      flag_group := True;
      sepa_polys (sepa_count) := sepa_polys (sepa_count) + 1;
    else
      flag_group := False;
    end if;
  end Point_Index;

end VRML_Help;
