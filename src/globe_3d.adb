with GLOBE_3D.Options,
     GLOBE_3D.Textures,
     GLOBE_3D.Math,
     GLOBE_3D.Portals,
     GLOBE_3D.Aux;

with GL.Math,
     GL.Simple_text;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

package body GLOBE_3D is

  use GLOBE_3D.Options;

  package G3DT renames GLOBE_3D.Textures;
  package G3DM renames GLOBE_3D.Math;

   -- normal support
   --

   procedure Add_Normal_of_3p (o             : in     Object_3D'Class;
                               Pn0, Pn1, Pn2 : in     Integer;
                               N             : in out Vector_3D) is
      use GL, G3DM, GL.Math, GLOBE_3D.Aux;

      function Params return String is
      begin
         return
           " Object: " & Trim(o.ID,Right) &
         " Pn0=" & Integer'Image(Pn0) &
         " Pn1=" & Integer'Image(Pn1) &
         " Pn2=" & Integer'Image(Pn2);
      end Params;
      N_contrib: Vector_3D;
   begin
      if Pn0=0 or Pn1=0 or Pn2=0 then return; end if;
      N_contrib:= (o.point(Pn1)-o.point(Pn0))*(o.point(Pn2)-o.point(Pn0)) ;
      if strict_geometry and then Almost_zero(Norm2(N_contrib)) then
        raise zero_normal with
           Params &
           " P0=" & Coords(o.point(Pn0)) &
           " P1=" & Coords(o.point(Pn1)) &
           " P2=" & Coords(o.point(Pn2)) &
           " Nc=" & Coords(N_contrib);
      end if;
      N:= N + N_contrib;
   exception
      when e: others =>
         Raise_Exception(
                         Exception_Identity(e),
                         Exception_Message(e) & Params
                        );
   end Add_Normal_of_3p;

  -- 'Visual'
  --

  procedure Free (o     : in out p_Visual)
  is
     procedure deallocate is new Ada.Unchecked_Deallocation (Visual'Class, p_Visual);
  begin
     Destroy (o.all);
     deallocate (o);
  end Free;

  function Width  (o: in Visual'class) return Real
  is
  begin
     return Bounds (o).Box.X_Extent.Max - Bounds (o).Box.X_Extent.Min;
  end Width;

  function Height  (o: in Visual'class) return Real
  is
  begin
     return Bounds (o).Box.Y_Extent.Max - Bounds (o).Box.Y_Extent.Min;
  end Height;

  function Depth  (o: in Visual'class) return Real
  is
  begin
     return Bounds (o).Box.Z_Extent.Max - Bounds (o).Box.Z_Extent.Min;
  end Depth;

  -- 'Object_3D'
  --

  -- object validation
  --

  procedure Check_object(o: Object_3D) is

    use GL.Math, G3DM;

    procedure Check_faces is

      procedure Check(f,v: Integer) is
      pragma Inline(Check);
      begin
        if v < 0 or else v > o.Max_points then
          raise bad_vertex_number with
            o.ID & " face="   & Integer'Image(f) &
                   " vertex=" & Integer'Image(v);
        end if;
      end Check;

      procedure Check_duplicate(f,Pn1,Pn2: Integer) is
      pragma Inline(Check_duplicate);
      begin
        -- Skip "dead" edge (triangle), 30-Dec-2001
        if Pn1=0 or else Pn2=0 then return; end if;
        -- Detect same point number
        if Pn1=Pn2 then
          raise duplicated_vertex with o.ID & " in face"   & Integer'Image(f);
        end if;
        -- Detect same point coordinates (tolerated in an object,
        -- although inefficient, but harms as vertex of the same face!)

        if Almost_zero(Norm2(o.point(Pn1) - o.point(Pn2))) then
          raise duplicated_vertex_location with o.ID & " in face" & Integer'Image(f);
        end if;
      end Check_duplicate;

    begin
      for fa in o.face'Range loop
        for edge_num in 1..4 loop
          Check( fa, o.face(fa).P(edge_num) );
          for other_edge in edge_num+1 .. 4 loop
            Check_duplicate( fa, o.face(fa).P(edge_num),
                                 o.face(fa).P(other_edge) );
          end loop;
        end loop;
      end loop; -- fa
    end Check_faces;

  begin
    Check_faces;
  end Check_object;

  procedure Check_textures(o: Object_3D) is
  begin
    for f in o.face'Range loop
      if is_textured(o.face(f).skin) and then not Textures.Valid_texture_ID(o.face(f).texture) then
        raise Textures.Undefined_texture_ID with
            Trim(o.ID, Right) &
            " face="   & Integer'Image(f) &
            " skin="   & Skin_type'Image(o.face(f).skin) &
            " texture_id=" & Image_ID'Image(o.face(f).texture);
      end if;
    end loop;
  end Check_textures;

  --------------------------------------------
  -- Object initialization (1x in its life) --
  --------------------------------------------

  overriding procedure Pre_calculate(o: in out Object_3D) is
    use GL, GL.Math, G3DM, GLOBE_3D.Aux;

    N: Vector_3D;
    length_N : Real;

    procedure Calculate_face_internals(
      fa:  in Face_type;
      fi: out Face_internal_type
    )
    is
      l: Natural:= 0;
      quadri_edge:  array(fa.P'Range) of Natural;
      ex_U, ex_V: Real;
    begin
      l:= 0;
      for qe in fa.P'Range loop
        if fa.P(qe) /= 0 then
          l:= l + 1;
          quadri_edge(l):= qe; -- if triangle, "map" edge on a quadri
          fi.P_compact(l):= fa.P(qe);
        end if;
      end loop;
      if l in Edge_count then
        fi.last_edge:= l;
      else
        raise bad_edge_number with o.ID & " edge=" & Integer'Image(l);
      end if;
      -- * Face invariant : Textured face: extremities
      for e in 1..l loop
        if fa.whole_texture then
          ex_U:= Real(fa.repeat_U);
          ex_V:= Real(fa.repeat_V);
          case quadri_edge(e) is
            when 1 => fi.UV_extrema(e):= (0.0, 0.0 );  --  bottom, left  4--<--3
            when 2 => fi.UV_extrema(e):= (ex_U,0.0 );  --  bottom, right |     |
            when 3 => fi.UV_extrema(e):= (ex_U,ex_V);  --  top, right    1-->--2
            when 4 => fi.UV_extrema(e):= (0.0, ex_V);  --  top, left
            when others => null;
          end case;
        else
          -- Just copy the mapping, but in compact form for triangles:
          fi.UV_extrema(e):= fa.texture_edge_map(quadri_edge(e));
        end if;
      end loop;
      -- * Face invariant : Normal of unrotated face
      N:= (0.0, 0.0, 0.0);
      case fi.last_edge is
        when 3 =>
          Add_Normal_of_3p(o,
            fi.P_compact(1),
            fi.P_compact(2),
            fi.P_compact(3),
            N
          );
        when 4 =>
          Add_Normal_of_3p(o, fa.P(1), fa.P(2), fa.P(4), N);
          --  We sum other normals for not perfectly flat faces,
          --  in order to have a convenient average...
          Add_Normal_of_3p(o, fa.P(2), fa.P(3), fa.P(1), N);
          Add_Normal_of_3p(o, fa.P(3), fa.P(4), fa.P(2), N);
          Add_Normal_of_3p(o, fa.P(4), fa.P(1), fa.P(3), N);
      end case;
      length_N:= Norm( N );
      if Almost_zero(length_N) then
        if strict_geometry then
          raise zero_summed_normal;
        else
          fi.normal:= N; -- 0 vector !
        end if;
      else
        fi.normal:= (1.0 / length_N) * N;
      end if;
    end Calculate_face_internals;

    adjacent_faces: array(o.point'Range) of Natural:= (others => 0);
    pf: Natural;
    length: Real;

  begin --Pre_calculate
    if full_check_objects then
      Check_object(o);
    end if;

    for i in o.face'Range loop
      begin
        -- Geometry
        Calculate_face_internals( o.face(i), o.face_internal(i) );
        -- Disable blending when alphas are = 1
        case o.face(i).skin is
          when material_only | material_texture =>
            o.face_internal(i).blending:= Is_to_blend(o.face(i).material);
          when colour_only | coloured_texture | texture_only=>
            o.face_internal(i).blending:= Is_to_blend(o.face(i).alpha);
          when invisible =>
            o.face_internal(i).blending:= False;
        end case;
        o.transparent:= o.transparent or o.face_internal(i).blending;
      exception
        when zero_summed_normal =>
          raise zero_summed_normal with o.ID & " face=" & Integer'Image(i);
      end;
    end loop;

    declare
      use GLOBE_3D.REF;
      max_Norm2 : Real := 0.0;
    begin
      o.bounds.Box.X_Extent.Min := Real'Last;   o.bounds.Box.X_Extent.Max := Real'First;
      o.bounds.Box.Y_Extent.Min := Real'Last;   o.bounds.Box.Y_Extent.Max := Real'First;
      o.bounds.Box.Z_Extent.Min := Real'Last;   o.bounds.Box.Z_Extent.Max := Real'First;

      for p in o.point'Range loop
        o.edge_vector(p)          := (0.0,0.0,0.0);
        max_Norm2                 := Real'Max (Norm2 (o.point (p)),  max_Norm2);

        o.bounds.Box.X_Extent.Min := Real'Min (o.bounds.Box.X_Extent.Min,  o.point (p)(0));  -- tbd: set extents and bounding sphere radius in
        o.bounds.Box.X_Extent.Max := Real'Max (o.bounds.Box.X_Extent.Max,  o.point (p)(0));  --      common procedure for 'object_base' class.
        o.bounds.Box.Y_Extent.Min := Real'Min (o.bounds.Box.Y_Extent.Min,  o.point (p)(1));
        o.bounds.Box.Y_Extent.Max := Real'Max (o.bounds.Box.Y_Extent.Max,  o.point (p)(1));
        o.bounds.Box.Z_Extent.Min := Real'Min (o.bounds.Box.Z_Extent.Min,  o.point (p)(2));
        o.bounds.Box.Z_Extent.Max := Real'Max (o.bounds.Box.Z_Extent.Max,  o.point (p)(2));
      end loop;

      o.bounds.sphere_Radius := Sqrt (max_Norm2);
    end;

    -- Calculate edge vectors.
    --   Naive algorithm: for each point, scan all faces to see
    --   if they are adjacent. It took #points * #faces steps.
    --   -> better algorithm here: 2 * #points + 4 * #faces. (22-Jan-2006)
    for f in o.face'Range loop
      for p in o.face(f).P'Range loop
        pf:= o.face(f).P(p);
        if pf /= 0 then
          adjacent_faces(pf):= adjacent_faces(pf) + 1;
          o.edge_vector(pf):= o.edge_vector(pf) + o.face_internal(f).normal;
        end if;
      end loop;
    end loop;
    for p in o.point'Range loop
      if adjacent_faces(p) = 0 then
        if strict_geometry then
          -- Strict approach: detect any unmatched point:
          raise point_unmatched with
            Trim(o.ID, Right) &
            " point " & Integer'Image(p) &
            " belongs to none of the object's face";
        end if;
      else
        length:= Norm( o.edge_vector(p) );
        if not Almost_zero(length) then
          o.edge_vector(p):= (1.0/length) * o.edge_vector(p);
        end if;
      end if;
    end loop;

    -- Ooof. Now we can certify:
    o.pre_calculated:= True;
  end Pre_calculate;

  procedure Arrow(P: Point_3D; D: Vector_3D) is
    use GL, GL.Math, G3DM;
    V,V1,V2: Vector_3D;
  begin
    if Almost_zero(Norm2(D)) then
      return;
    end if;
    V:= (D(1),-D(0),0.0);         -- an orthogonal, or zero
    if Almost_zero(Norm2(V)) then -- bad luck, it is zero
      V:= (0.0,-D(2),D(1));       -- 2nd try
    end if;
    V:= (0.2/Norm(V)) * V;
    V1:= 0.7*D + V;
    V2:= 0.7*D - V;
    GL_Begin(GL.LINES);
    Vertex(P+D);    Vertex(P);
    Vertex(P+D);    Vertex(P+V1);
    Vertex(P+D);    Vertex(P+V2);
    GL_End;
  end Arrow;

  shiny_material :
    constant GL.Materials.Material_type:=
      (ambient =>        (0.1, 0.1, 0.1, 1.0),
       diffuse =>        (0.1, 0.1, 0.1, 1.0),
       specular =>       (0.8, 0.8, 0.8, 1.0),
       emission =>       (0.0, 0.0, 0.0, 1.0),
       shininess =>      50.0);  --  77: Chrome, 96: Glass

  -------------
  -- Display --
  -------------

  procedure Display_one(o: in out Object_3D) is
  -- Display only this object and not connected objects
  -- out: object will be initialized if not yet

    --

    --
    -- Display face routine which is optimized to produce a shorter list
    -- of GL commands. Runs slower then the original Display face routine
    -- yet needs to be executed only once.
    --
    -- Uwe R. Zimmer, July 2011
    --
    package Display_face_optimized is
      procedure Display_face (First_Face : Boolean; fa: Face_type; fi: in out Face_internal_type);
      procedure Display_specular (fa: Face_type; fi: Face_internal_type);
    private
      Previous_face          : Face_type;
      Previous_face_internal : Face_internal_type;
      Previous_specular_face : Face_type;
    end Display_face_optimized;

    package body Display_face_optimized is
      use GL, GL.Materials;

      procedure Draw_polygon (fa: Face_type; fi: Face_internal_type) is
      begin
        case fi.last_edge is
          when 3 => GL_Begin( TRIANGLES );
          when 4 => GL_Begin( QUADS );
        end case;
        for i in 1..fi.last_edge loop
          if is_textured(fa.skin) then
            TexCoord(fi.UV_extrema(i).U, fi.UV_extrema(i).V);
          end if;
          Normal(o.edge_vector(fi.P_compact(i)));
          Vertex(o.point(fi.P_compact(i)));
        end loop;
        GL_End;
      end Draw_polygon;

      procedure Display_face (First_Face : Boolean; fa: Face_type; fi: in out Face_internal_type) is
        blending_hint: Boolean;

        procedure Display_texture_label(name: Ident; p: Point_3D) is
          use GL.Simple_text;
        begin
          Disable( TEXTURE_2D );
          Text_output(p, name, (0.7, 0.7, 0.9, 1.0), 5.0, Sans_Serif);
          Enable( TEXTURE_2D );
        end Display_texture_label;

      begin -- Display_face

        if fa.skin = invisible then
          Previous_face          := fa;
          Previous_face_internal := fi;
          return;
        end if;

        ------------------------------
        --  1) Set Face's Material  --
        ------------------------------

        if First_Face
          or else Previous_face.skin = invisible
          or else fa.skin /= Previous_face.skin
          or else (fa.skin = Previous_face.skin
                   and then is_material(fa.skin)
                   and then not Identical(fa.material, Previous_face.material))
        then
          case fa.skin is
            when material_only | material_texture =>
              Disable(COLOR_MATERIAL);
              Set_Material(fa.material);
            when invisible =>
              null;  --  NB: this case doesn't happen since procedure was quitted before
            when others =>
              Set_Material(neutral_material);
          end case;
        end if;

        ----------------------------
        --  2) Set Face's Colour  --
        ----------------------------

        if First_Face
          or else Previous_face.skin = invisible
          or else fa.skin /= Previous_face.skin
        then
          case fa.skin is
            when material_only | material_texture =>
              null; -- done above
            when colour_only | coloured_texture =>
              Enable(COLOR_MATERIAL);
              ColorMaterial(FRONT_AND_BACK, AMBIENT_AND_DIFFUSE);
            when texture_only =>
              Disable(COLOR_MATERIAL);
            when invisible =>
              null;
          end case;
        end if;

        if is_coloured(fa.skin) and then
          (First_Face
             or else Previous_face.skin = invisible
             or else not (GL.Math.Identical(fa.colour, Previous_face.colour) and then
                          GL.Math.Almost_zero(fa.alpha - Previous_face.alpha))
           )
        then
          Color(
                red   => fa.colour.red,
                green => fa.colour.green,
                blue  => fa.colour.blue,
                alpha => fa.alpha
               );
        end if;

        -----------------------------
        --  3) Set Face's Texture  --
        -----------------------------

        if is_textured(fa.skin) then
          G3DT.Check_2D_texture(fa.texture, blending_hint);
          if blending_hint then
            fi.blending:= True;
            -- 13-Oct-2006: override the decision made at Pre_calculate.
            -- If texture data contains an alpha layer, we switch on transparency.
          end if;
        end if;

        if First_Face
          or else Previous_face.skin = invisible
          or else is_textured(fa.skin) /= is_textured(Previous_face.skin)
        then
          case fa.skin is
            when texture_only | coloured_texture | material_texture =>
              Enable( TEXTURE_2D );
            when colour_only | material_only =>
              Disable( TEXTURE_2D );
            when invisible =>
              null;
          end case;
        end if;

        if is_textured(fa.skin) and then
          (First_Face
             or else not
                ( --  In this case we don't need to bind again the same image ID
                  is_textured(Previous_face.skin) and then
                  fa.texture = Previous_face.texture
                )
          )
        then
          BindTexture( TEXTURE_2D, GL.Uint(Image_ID'Pos(fa.texture)+1) );
        end if;

        ------------------------------------------
        --  Set Face's Blending / Transparency  --
        ------------------------------------------

        if First_Face
          or else Previous_face.skin = invisible
          or else fi.blending /= Previous_face_internal.blending
        then
          if fi.blending then
            Enable( BLEND ); -- See 4.1.7 Blending
            BlendFunc( sfactor => SRC_ALPHA,
                       dfactor => ONE_MINUS_SRC_ALPHA );
            -- Disable( DEPTH_TEST );
            -- Disable( CULL_FACE );
          else
            Disable( BLEND );
            -- Enable( DEPTH_TEST );
            -- Enable( CULL_FACE );
            -- CullFace( BACK );
          end if;
        end if;

        --------------------------
        --  Now, draw the face  --
        --------------------------

        --  Texture (diffuse) is drawn here:
        Draw_polygon(fa, fi);
        if show_texture_labels then
          Display_texture_label(fi.texture_name, o.point(fi.P_compact(1)));
        end if;

        Previous_face          := fa;
        Previous_face_internal := fi;
      end Display_face;

      procedure Display_specular (fa: Face_type; fi: Face_internal_type) is
        blending_hint: Boolean;
      begin
        --  Specular map (the optional "glossy" or "shiny" image) is drawn here:
        if is_textured(fa.skin) and then fa.specular_map /= null_image then
          G3DT.Check_2D_texture(fa.specular_map, blending_hint);
          if fa.specular_map /= Previous_specular_face.specular_map then
            BindTexture( TEXTURE_2D, GL.Uint(Image_ID'Pos(fa.specular_map)+1) );
          end if;
          --  NB: display only works when setting GL.DepthFunc(GL.LEQUAL)
          --  Default is GL.LESS, and thus only first texture per face will be drawn.
          Draw_polygon(fa, fi);
          Previous_specular_face := fa;
        end if;
      end Display_specular;

    end Display_face_optimized;

    procedure Display_normals is
      use GL, GL.Math, G3DM;
      C: Vector_3D;
    begin
      GL.Color( 0.5, 0.5, 1.0, 1.0);
      -- show pseudo (average) normals at edges:
      for e in o.point'Range loop
        Arrow(o.point(e), arrow_inflator * o.edge_vector(e));
      end loop;
      GL.Color( 1.0, 1.0, 0.5, 1.0);
      -- show normals of faces:
      for f in o.face'Range loop
        C:= (0.0,0.0,0.0);
        for i in 1..o.face_internal(f).last_edge loop
          C:= C+o.point(o.face_internal(f).P_compact(i));
        end loop;
        C:= (1.0/Real(o.face_internal(f).last_edge)) * C;
        Arrow(C, arrow_inflator * o.face_internal(f).normal);
      end loop;
    end Display_normals;

    procedure Set_for_specular is
      use GL, GL.Materials;
    begin
      Disable(COLOR_MATERIAL);
      Set_Material(shiny_material);
      Enable( BLEND );
      BlendFunc( sfactor => ONE, dfactor => ONE );
    end Set_for_specular;

    use GL, G3DM;

  begin -- Display_one

    if not o.pre_calculated then
      Pre_calculate(o);
    end if;

    -- GL.Extended.BindBuffer    (GL.ARRAY_BUFFER, 0);             -- disable 'vertex buffer objects'
    -- GL.Extended.BindBuffer    (GL.ELEMENT_ARRAY_BUFFER, 0);     -- disable 'vertex buffer objects' indices

    --      gl.disableClientState (gl.TEXTURE_COORD_ARRAY);
    --      gl.disable    (ALPHA_TEST);
    GL.Enable (LIGHTING);

    GL.PushMatrix; -- 26-May-2006: instead of rotating/translating back
    GL.Translate( o.centre );
    Multiply_GL_Matrix(o.rotation);

    --  List preparation phase.
    case o.List_Status is
      when No_List | No_List_Optimized | Is_List =>
        null;
      when Generate_List =>
        o.List_Id := Integer(GL.GenLists(1));
        GL.NewList (GL.Uint (o.List_Id), COMPILE_AND_EXECUTE);
    end case;

    --  List generation phase or execution.
    case o.List_Status is
      when No_List =>
        for f in o.face'Range loop
          Display_face_optimized.Display_face(True, o.face(f), o.face_internal(f));
          --  We mimic the old, direct, Display_face with redundant color, material, etc.
          --  instructions by passing True for First_Face.
        end loop;
        Set_for_specular;
        for f in o.face'Range loop
          Display_face_optimized.Display_specular(o.face(f), o.face_internal(f));
        end loop;
      when No_List_Optimized | Generate_List =>
        for f in o.face'Range loop
          Display_face_optimized.Display_face(f = o.face'First, o.face(f), o.face_internal(f));
        end loop;
        Set_for_specular;
        for f in o.face'Range loop
          Display_face_optimized.Display_specular(o.face(f), o.face_internal(f));
        end loop;
      when Is_List =>
        GL.CallList (GL.Uint (o.List_Id));
    end case;

    --  Close list - if any.
    case o.List_Status is
      when No_List | No_List_Optimized | Is_List =>
        null;
      when Generate_List  =>
        GL.EndList;
        if GL.GetError = OUT_OF_MEMORY then
          o.List_Status := No_List;
        else
          o.List_Status := Is_List;
          GL.CallList (GL.Uint (o.List_Id));  --  First display of the freshly generated list.
        end if;
    end case;

    if show_normals then
      GL.Disable( GL.LIGHTING );
      GL.Disable( GL.TEXTURE_2D );
      Display_normals;
      GL.Enable( GL.LIGHTING ); -- mmmh...
    end if;

    GL.PopMatrix; -- 26-May-2006: instead of rotating/translating back
    --  GL.Rotate( o.auto_rotation(2),  0.0,  0.0, -1.0 );
    --  GL.Rotate( o.auto_rotation(1),  0.0, -1.0,  0.0 );
    --  GL.Rotate( o.auto_rotation(0), -1.0,  0.0,  0.0 );

    --  GL.Translate( -o.centre );
  end Display_one;

  overriding procedure Display(
    o          : in out Object_3D;
    clip       : in     Clipping_data
  )
  is

    use GLOBE_3D.Portals;

    procedure Display_clipped(
      o            : in out Object_3D'Class;
      clip_area    : in     Clipping_area;
      portal_depth : in     Natural
    )
    is
      procedure Try_portal(f: Positive) is
        use G3DM, GL, GL.Math;
        dot_product: Real;
        plane_to_eye: Vector_3D; -- vector from any point in plane to the eye
        bounding_of_face, intersection_clip_and_face: Clipping_area;
        success, non_empty_intersection: Boolean;
      begin
        dot_product:= o.face_internal(f).normal * clip.view_direction;
        --  Culling #1: check if portal is in field of view's "dead angle"
        if dot_product < clip.max_dot_product then
          plane_to_eye:=
            clip.eye_position -
            --  We just choose any point of the face.
            (o.point(o.face_internal(f).P_compact(1)) + o.centre)
          ;
          dot_product:= plane_to_eye * o.face_internal(f).normal;
          --  Culling #2: check if we are on the right side of the portal
          --  dot_product = signed distance to the plane
          --  NB: this ignores o.auto_rotation !
          if dot_product > 0.0 then
            Find_bounding_box( o, f, bounding_of_face, success );
            if success then
              Intersect( clip_area, bounding_of_face,
                         intersection_clip_and_face, non_empty_intersection );
            else
              -- in doubt, draw with the present clipping
              intersection_clip_and_face:= clip_area;
              non_empty_intersection:= True;
            end if;
            --  Culling #3: clipping rectangle
            if non_empty_intersection then
              --  Recursion happens here:
              Display_clipped(
                o            => o.face(f).connecting.all,
                clip_area    => intersection_clip_and_face,
                portal_depth => portal_depth + 1
              );
            end if;
          end if;
        end if;
      end Try_portal;

      so: p_Object_3D_list;

    begin -- Display_clipped
      if not o.pre_calculated then
        Pre_calculate(o);
      end if;
      --
      -- a/ Display connected objects which are visible through o's faces
      --    This is where recursion happens
      if (not filter_portal_depth) or else -- filter_portal_depth: test/debug
         portal_depth <= 6
      then
        for f in o.face'Range loop
          if o.face(f).connecting /= null and then
             --  Prevent infinite recursion on rare cases where
             --  object A or B is not convex, and A and B see each other
             --  and the culling by clipping cannot stop the recursion:
             --  (e.g. origin2.proc, tomb.proc)
             not o.face_internal(f).portal_seen
             --  NB: drawing [different parts of] the same object several times
             --  is right, since portions can be seen through different portals,
             --  but walking more than once through the same *portal* with
             --  this algorithm is wrong, causing infinite recursion.
          then
            o.face_internal(f).portal_seen := True;
            --  Recursively calls Display_clipped for objects visible through face f.
            Try_portal(f);
          end if;
        end loop;
      end if;
      -- b/ Display the object itself
      if (not filter_portal_depth) or else -- filter_portal_depth: test/debug
         (portal_depth = 1 or portal_depth = 5)
      then
        -- The graphical clipping (Scissor) gives various effects
        -- - almost no speedup on the ATI Radeon 9600 Pro (hardware)
        -- - factor: ~ Sqrt(clipped surface ratio) with software GL
        if portal_depth > 0 then
          GL.Enable(GL.SCISSOR_TEST);
          GL.Scissor(
            x      => GL.Int(clip_area.X1),
            y      => GL.Int(clip_area.Y1),
            width  => GL.Sizei(clip_area.X2 - clip_area.X1+1),
            height => GL.Sizei(clip_area.Y2 - clip_area.Y1+1)
          );
        else
          GL.Disable(GL.SCISSOR_TEST);
        end if;
        if portal_tracking then
          info_b_ntl2:= info_b_ntl2 + 1;
          info_b_ntl3:= Natural'Max(portal_depth, info_b_ntl3);
        end if;
        Display_one(o);
        so:= o.sub_objects;
        while so /= null loop
          Display_one(so.objc.all);  -- No portals, sub-obj recursion in this call - may want it.
          so:= so.next;
        end loop;
      end if;
      if show_portals and then portal_depth > 0 then
        Draw_boundary(clip.main_clipping, clip_area, portal_depth);
      end if;
    end Display_clipped;

    procedure Reset_portal_seen(o: in out Object_3D'Class) is
    begin
      for f in o.face'Range loop
        if o.face_internal(f).portal_seen then
          o.face_internal(f).portal_seen := False;
          Reset_portal_seen(o.face(f).connecting.all);
        end if;
      end loop;
    end Reset_portal_seen;

  begin
    if portal_tracking then
      info_b_ntl2:= 0; -- count amount of objects displayed, not distinct
      info_b_ntl3:= 0; -- records max depth
    end if;
    Display_clipped( o, clip_area => clip.main_clipping, portal_depth => 0 );
    Reset_portal_seen(o);
  end Display;

  function "+" (a, b: Map_idx_pair) return Map_idx_pair is
  begin
    return (a.U + b.U, a.V + b.V);
  end;

  function "-" (a, b: Map_idx_pair) return Map_idx_pair is
  begin
    return (a.U - b.U, a.V - b.V);
  end;

  function "*" (l: GL.Double; p: Map_idx_pair) return Map_idx_pair is
  begin
    return (l * p.U, l * p.V);
  end;

  function Identical(a, b: Map_idx_pair) return Boolean is
    use GL.Math;
  begin
    return
      Almost_zero(a.U-b.U) and then Almost_zero(a.V-b.V);
  end;

  function Is_textured_specular(fa: Face_type) return Boolean is
  begin
    return is_textured(fa.skin) and then fa.specular_map /= null_image;
  end;

  overriding procedure Destroy (o : in out Object_3D) is
    ol, ol_prev: p_Object_3D_list:= o.sub_objects;
    procedure Dispose is new Ada.Unchecked_Deallocation (Object_3D_list, p_Object_3D_list);
  begin
    while ol /= null loop
      Free(p_Visual(ol.objc));  --  Sub-object will be destroyed first - then, sub-sub-objects etc.
      ol_prev:= ol;
      ol:= ol.next;
      Dispose(ol_prev);
    end loop;
    if o.List_Status = Is_List then
      GL.DeleteLists (GL.Uint (o.List_Id), 1);
    end if;
  end Destroy;

  overriding procedure Set_Alpha(o: in out Object_3D; Alpha : in GL.Double) is
  begin
    for f in o.face'Range loop
      o.face(f).alpha := Alpha;
    end loop;
  end Set_Alpha;

  overriding function Is_Transparent(o: in Object_3D) return Boolean is
  begin
    return o.transparent;
  end Is_Transparent;

  overriding function Face_Count(o: in Object_3D) return Natural is
  begin
    return o.Max_faces;
  end Face_Count;

  overriding function  Bounds(o: in Object_3D) return GL.Geometry.Bounds_record is
  begin
    return o.bounds;
  end Bounds;

  overriding function Skinned_Geometries (o : in Object_3D) return GL.Skinned_Geometry.Skinned_Geometries
  is
  pragma Unreferenced (o);
  begin
     return GL.Skinned_Geometry.null_skinned_geometries;
  end Skinned_Geometries;

  -- Lighting support.
  --

  -- lights: array( Light_ident ) of Light_definition;
  light_defined: array( Light_ident ) of Boolean:= (others => False);

  procedure Define(which: Light_ident; as: Light_definition) is
    id: constant GL.LightIDEnm:= GL.LightIDEnm'Val(which-1);
    use GL;
  begin
    -- lights(which):= as;
    Light( id, POSITION, as.position );
    Light( id, AMBIENT,  as.ambient  );
    Light( id, DIFFUSE,  as.diffuse  );
    Light( id, SPECULAR, as.specular );
    light_defined(which):= True;
  end Define;

  procedure Switch_lights(on: Boolean) is
  begin
    for l in Light_ident loop
      Switch_light(l,on);
    end loop;
  end Switch_lights;

  function Server_id(which: Light_ident) return GL.ServerCapabilityEnm is
  begin
    return GL.ServerCapabilityEnm'Val(GL.ServerCapabilityEnm'Pos(GL.LIGHT0) + which - 1);
  end Server_id;

  procedure Switch_light(which: Light_ident; on: Boolean) is
  begin
    if light_defined(which) then
      if on then
        GL.Enable( Server_id(which) );
      else
        GL.Disable( Server_id(which) );
      end if;
    end if;
  end Switch_light;

  function Is_light_switched(which: Light_ident) return Boolean is
  begin
    return Boolean'Val(GL.IsEnabled(Server_id(which)));
  end Is_light_switched;

  procedure Reverse_light_switch(which: Light_ident) is
  begin
    Switch_light(which, not Is_light_switched(which));
  end Reverse_light_switch;

  ------------------
  -- Resource I/O --
  ------------------

  procedure Load_if_needed( zif: in out Zip.Zip_info; name: String) is
  begin
    if not Zip.Is_loaded(zif) then
      begin
        Zip.Load( zif, name );
      exception
        when Zip.Zip_file_open_Error => -- Try with lower case:
          Zip.Load( zif, To_Lower(name) );
      end;
    end if;
  end Load_if_needed;

  procedure Set_local_data_name(s: String) is
  begin
    if Zip.Is_loaded( zif_level ) then
      Zip.Delete( zif_level );
    end if;
    -- ^ Possible resource name change -> need this, will be reloaded on next use
    level_data_name:= U(s);
    if not Zip.Exists(s) then
      raise data_file_not_found with s;
    end if;
  end Set_local_data_name;

  procedure Set_global_data_name(s: String) is
  begin
    if Zip.Is_loaded( zif_global ) then
      Zip.Delete( zif_global );
    end if;
    -- ^ Possible resource name change -> need this, will be reloaded on next use
    global_data_name:= U(s);
    if not Zip.Exists(s) then
      raise data_file_not_found with s;
    end if;
  end Set_global_data_name;

  procedure Set_name(o: in out Visual'class; new_name: String) is
  begin
    if new_name'Length > Ident'Length then
      raise Constraint_Error with "Visual identifier is too long, maximum is" & Integer'Image(Ident'Length);
    end if;
    o.ID:= empty;
    o.ID(1..new_name'Length):= new_name;
  end Set_name;

  function Get_name(o: Visual'class) return String is
  begin
    return Trim(o.ID,Right);
  end Get_name;

  procedure Rebuild_links(
    o           : in out Object_3D'Class; -- object to be relinked
    neighbouring: in     Map_of_Visuals;  -- neighbourhood
    tolerant_obj: in     Boolean;         -- tolerant on missing objects
    tolerant_tex: in     Boolean;         -- tolerant on missing textures
    tolerant_spc: in     Boolean          -- tolerant on missing specular maps
  )
  is
    use Visuals_Mapping, Ident_Vectors;
    c: Visuals_Mapping.Cursor;
    cv: Ident_Vectors.Cursor;
    id: Ident;
    --
    procedure Relink_specular(fa: in out Face_type; fi: Face_internal_type) is
    begin
      fa.specular_map:= Textures.Texture_ID( fi.specular_name );
    exception
      when Textures.Texture_name_not_found =>
        if tolerant_spc then
          fa.specular_map:= null_image;
        else
          raise;
        end if;
    end Relink_specular;
    --
  begin
    for f in o.face'Range loop
      --  1/ Find texture IDs:
      if is_textured(o.face(f).skin) and then o.face_internal(f).texture_name /= empty then
        begin
          o.face(f).texture:= Textures.Texture_ID( o.face_internal(f).texture_name );
          if o.face_internal(f).specular_name /= empty then
            Relink_specular(o.face(f), o.face_internal(f));
          end if;
        exception
          when Textures.Texture_name_not_found =>
            if tolerant_tex then
              o.face(f).texture:= null_image;
              o.face(f).skin:= material_only;
            else
              raise;
            end if;
        end;
      end if;
      --  2/ Connections through portals:
      if o.face_internal(f).connect_name /= empty then
        c:= neighbouring.Find(U(o.face_internal(f).connect_name));
        if c = Visuals_Mapping.No_Element then
          -- Key not found
          if tolerant_obj then
            o.face(f).connecting:= null;
          else
            raise
              Portal_connection_failed with
              "For object name [" & Trim(o.ID,Right) & "], looking for object [" &
              Trim(o.face_internal(f).connect_name,Right) & ']';
          end if;
        else
          o.face(f).connecting:= p_Object_3D(Element(c));
        end if;
      end if;
    end loop;
    --  for id of o.sub_obj_ids loop  --  Ada 2012 shortcut notation
    cv:= o.sub_obj_ids.First;
    while Has_Element(cv) loop
      id:= Element(cv);
      c:= neighbouring.Find(U(id));
      if c = Visuals_Mapping.No_Element then
        -- Key not found
        if tolerant_obj then
          null;
        else
          raise
            Sub_object_connection_failed with
              "For object name [" & Trim(o.ID, Right) & "], looking for object [" &
              Trim(id, Right) & ']';
        end if;
      else
        o.sub_objects:= new Object_3D_list'(
          objc => p_Object_3D(Element(c)),
          next => o.sub_objects
        );
        Rebuild_links(
          p_Object_3D(Element(c)).all, neighbouring, tolerant_obj, tolerant_tex, tolerant_spc
        );
      end if;
      Next(cv);
    end loop;
  end Rebuild_links;

   function empty_map return Map_of_Visuals is
     thing: Map_of_Visuals;
   begin
     Visuals_Mapping.Map(thing):= Visuals_Mapping.Empty_Map;
     return thing;
   end empty_map;

   procedure Add( to_map: in out Map_of_Visuals; what: p_Visual ) is
    pos: Visuals_Mapping.Cursor;
    success: Boolean;
   begin
      Visuals_Mapping.Insert(
        Visuals_Mapping.Map(to_map),
        U(what.ID),
        what,
        pos,
        success
      );
     if not success then -- A.18.4. 45/2
       raise Duplicate_name with what.ID;
     end if;
   end Add;

   function Map_of( va: Visual_array ) return Map_of_Visuals is
     res: Map_of_Visuals:= empty_map;
   begin
     -- Perhaps Reserve_Capacity would be good here ??
     for i in va'Range loop
       Add(res, va(i));
     end loop;
     return res;
   end Map_of;

end GLOBE_3D;
