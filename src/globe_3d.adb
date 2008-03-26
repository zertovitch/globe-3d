with GLOBE_3D.Options,
     GLOBE_3D.Textures,
     GLOBE_3D.Math,
     GLOBE_3D.Portals;

with oGL.Errors;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with ada.text_io; use ada.text_io;

with ada.unchecked_Conversion;

with System.Storage_Elements;
with Ada_Containers_Generic_Array_Sort;


package body GLOBE_3D is

  use GLOBE_3D.Options;

  package G3DT renames GLOBE_3D.Textures;
  package G3DM renames GLOBE_3D.Math;

  function Image( r: Real ) return String is
    s: String(1..10);
  begin
    RIO.Put(s,r,4,0);
    return s;
  exception
    when Ada.Text_IO.Layout_Error =>
      return Real'Image(r);
  end Image;

  function Coords( p: Point_3D ) return String is
  begin
    return '(' & Image(p(0)) &
           ',' & Image(p(1)) &
           ',' & Image(p(2)) &
           ')';
  end Coords;








   -- normal support
   --

   procedure Add_Normal_of_3p (o             : in     Object_3d'Class;
                               Pn0, Pn1, Pn2 : in     Integer;
                               N             : in out Vector_3D) is
      use GL, G3dm;

      function Params return String is
      begin
         return
           " Object: " & Trim(o.id,right) &
         " Pn0=" & Integer'Image(Pn0) &
         " Pn1=" & Integer'Image(Pn1) &
         " Pn2=" & Integer'Image(Pn2);
      end Params;
      N_contrib: Vector_3D;
   begin
      if Pn0=0 or Pn1=0 or Pn2=0 then return; end if;
      N_contrib:= (o.point(Pn1)-o.point(Pn0))*(o.point(Pn2)-o.point(Pn0)) ;
      if strict_geometry and then Almost_zero(Norm2(N_contrib)) then
         raise_exception( zero_normal'Identity,
                         Params &
                         " P0=" & Coords(o.point(Pn0)) &
                         " P1=" & Coords(o.point(Pn1)) &
                         " P2=" & Coords(o.point(Pn2)) &
                         " Nc=" & Coords(N_contrib)
                        );
      end if;
      N:= N + N_contrib;
   exception
      when e: others =>
         raise_exception(
                         Exception_Identity(e),
                         Exception_Message(e) & Params
                        );
   end Add_Normal_of_3p;




   -- blending support
   --

   function Is_to_blend(m: GL.Double) return Boolean is
      use GL, G3DM;
    begin
      return not Almost_zero(m-1.0);
    end Is_to_blend;


    function Is_to_blend(m: GL.Float) return Boolean is
      use GL, G3DM;
    begin
      return not Almost_zero(m-1.0);
    end Is_to_blend;


    function Is_to_blend(m: GL.Material_Float_vector) return Boolean is
    begin
      return Is_to_blend(m(3));
    end Is_to_blend;


    function Is_to_blend(m: Material_type) return Boolean  is
    begin
      return
        Is_to_blend(m.ambient) or
        Is_to_blend(m.diffuse) or
        Is_to_blend(m.specular);
        -- m.emission, m.shininess not relevant
    end Is_to_blend;



   -- material support
   --

   procedure Set_Material(m: Material_type) is
      use GL;
   begin
      Material(FRONT_AND_BACK, AMBIENT,   m.ambient);
      Material(FRONT_AND_BACK, DIFFUSE,   m.diffuse);
      Material(FRONT_AND_BACK, SPECULAR,  m.specular);
      Material(FRONT_AND_BACK, EMISSION,  m.emission);
      Material(FRONT_AND_BACK, SHININESS, m.shininess);
   end Set_Material;





   -- 'Visual'
   --

   procedure free (o     : in out p_Visual)
   is
      procedure deallocate is new ada.unchecked_deallocation (Visual'Class, p_Visual);
   begin
      destroy (o.all);
      free (o);
   end;





   function skinned_Geometrys (o : in Visual) return ogl.skinned_geometry.skinned_Geometrys
   is
   begin
      return ogl.skinned_geometry.null_skinned_Geometrys;
   end;



   function Width  (o: in Visual'class) return Real
   is
   begin
      return bounds (o).box.X_Extent.Max - bounds (o).box.X_Extent.Min;
   end;


   function Height  (o: in Visual'class) return Real
   is
   begin
      return bounds (o).box.Y_Extent.Max - bounds (o).box.Y_Extent.Min;
   end;


   function Depth  (o: in Visual'class) return Real
   is
   begin
      return bounds (o).box.Z_Extent.Max - bounds (o).box.Z_Extent.Min;
   end;





   -- 'Object_base'
   --

--     function face_Count (o : in Object_base) return Natural
--     is
--     begin
--        return o.Max_faces;
--     end;





   -- 'Object_3D'
   --

   -- object validation
   --

  procedure Check_object(o: Object_3D) is

    use G3DM;

    procedure Check_faces is

      procedure Check(f,v: Integer) is
      pragma Inline(Check);
      begin
        if v < 0 or else v > o.max_points then
          raise_exception(bad_vertex_number'Identity,
               o.id & " face="   & Integer'Image(f) &
                      " vertex=" & Integer'Image(v));
        end if;
      end Check;

      procedure Check_duplicate(f,Pn1,Pn2: Integer) is
      pragma Inline(Check_duplicate);
      begin
        -- Skip "dead" edge (triangle), 30-Dec-2001
        if Pn1=0 or else Pn2=0 then return; end if;
        -- Detect same point number
        if Pn1=Pn2 then
          raise_exception(duplicated_vertex'Identity,
               o.id & " in face "   & Integer'Image(f) );
        end if;
        -- Detect same point coordinates (tolerated in an object,
        -- although inefficient, but harms as vertex of the same face!)

        if Almost_zero(Norm2(o.point(Pn1) - o.point(Pn2))) then
          raise_exception(duplicated_vertex_location'Identity,
               o.id & " in face "   & Integer'Image(f) );
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

  --------------------------------------------
  -- Object initialization (1x in its life) --
  --------------------------------------------

  procedure Pre_calculate(o: in out Object_3D) is
    use GL, G3DM;

    N: Vector_3D;
    length_N : Real;


    procedure Calculate_face_invariants(
      fa:  in Face_type;
      fi: out Face_invariant_type
    ) is
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
        raise_exception( bad_edge_number'Identity, o.id );
      end if;
      -- * Face invariant : Textured face: extremities
      for e in 1..l loop
        if fa.whole_texture then
          ex_U:= Real(fa.repeat_U);
          ex_V:= Real(fa.repeat_V);
          case quadri_edge(e) is
            when 1=> fi.UV_extrema(e):= (0.0, 0.0 ); -- bottom, left  4--<--3
            when 2=> fi.UV_extrema(e):= (ex_U,0.0 ); -- bottom, right |     |
            when 3=> fi.UV_extrema(e):= (ex_U,ex_V); -- top, right    1-->--2
            when 4=> fi.UV_extrema(e):= (0.0, ex_V); -- top, left
            when others => null;
          end case;
        else
          -- Just copy the mapping, but in compact form for triangles:
          fi.UV_extrema(e):= fa.texture_edge_map(quadri_edge(e));
        end if;
      end loop;
      -- * Face invariant : Normal of unrotated face
      N:= (0.0, 0.0, 0.0);
      Add_Normal_of_3p(o, fa.P(1), fa.P(2), fa.P(4), N);
      -- We sum other normals for not too flat faces,
      -- in order to have a convenient average
      Add_Normal_of_3p(o, fa.P(2), fa.P(3), fa.P(1), N);
      Add_Normal_of_3p(o, fa.P(3), fa.P(4), fa.P(2), N);
      Add_Normal_of_3p(o, fa.P(4), fa.P(1), fa.P(3), N);
      -- NB: it also fits for triangles !
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
    end Calculate_face_invariants;


    adjacent_faces: array(o.point'Range) of Natural:= (others => 0);
    pf: Natural;
    length: Real;

  begin --Pre_calculate
    if full_check_objects then Check_object(o); end if;

    for i in o.face'Range loop
      begin
        -- Geometry
        Calculate_face_invariants( o.face(i), o.face_invariant(i) );
        -- Disable blending when alphas are = 1
        case o.face(i).skin is
          when material_only | material_texture =>
            o.face_invariant(i).blending:= Is_to_blend(o.face(i).material);
          when colour_only | coloured_texture | texture_only=>
            o.face_invariant(i).blending:= Is_to_blend(o.face(i).alpha);
          when invisible =>
            o.face_invariant(i).blending:= False;
        end case;
      exception
        when zero_summed_normal =>
              raise_exception( zero_summed_normal'Identity,
               o.id & " face=" & Integer'Image(i));
      end;
    end loop;

    -- Calculate edge vectors.
    --   Naive algorithm: for each point, scan all faces to see
    --   if they are adjacent. It took #points * #faces steps.
    --   -> better algorithm here: 2 * #points + 4 * #faces. (22-Jan-2006)

    declare
      use globe_3d.REF;
      max_Norm2 : Real := 0.0;
    begin
      o.bounds.box.X_Extent.Min := Real'Last;   o.bounds.box.X_Extent.Max := Real'First;
      o.bounds.box.Y_Extent.Min := Real'Last;   o.bounds.box.Y_Extent.Max := Real'First;
      o.bounds.box.Z_Extent.Min := Real'Last;   o.bounds.box.Z_Extent.Max := Real'First;

      for p in o.point'Range loop
        o.edge_vector(p)          := (0.0,0.0,0.0);
        max_Norm2                 := Real'max (Norm2 (o.Point (p)),  max_Norm2);

        o.bounds.box.X_Extent.Min := Real'Min (o.bounds.box.X_Extent.Min,  o.Point (p)(0));  -- tbd: set extents and bounding sphere radius in
        o.bounds.box.X_Extent.Max := Real'Max (o.bounds.box.X_Extent.Max,  o.Point (p)(0));  --      common procedure for 'object_base' class.
        o.bounds.box.Y_Extent.Min := Real'Min (o.bounds.box.Y_Extent.Min,  o.Point (p)(1));
        o.bounds.box.Y_Extent.Max := Real'Max (o.bounds.box.Y_Extent.Max,  o.Point (p)(1));
        o.bounds.box.Z_Extent.Min := Real'Min (o.bounds.box.Z_Extent.Min,  o.Point (p)(2));
        o.bounds.box.Z_Extent.Max := Real'Max (o.bounds.box.Z_Extent.Max,  o.Point (p)(2));
      end loop;

      o.bounds.sphere_Radius := sqRt (max_Norm2);
    end;

    for f in o.face'Range loop
      for p in o.face(f).P'Range loop
        pf:= o.face(f).P(p);
        if o.face(f).P(p) /= 0 then
          adjacent_faces(pf):= adjacent_faces(pf) + 1;
          o.edge_vector(pf):= o.edge_vector(pf) + o.face_invariant(f).normal;
        end if;
      end loop;
      if is_textured(o.face(f).skin) and then
         not Textures.Valid_texture_ID(o.face(f).texture) then
        raise_exception(Textures.Texture_out_of_range'Identity,
           o.id & " face="   & Integer'Image(f) &
                  " skin="   & Skin_Type'Image(o.face(f).skin) &
                  " texture_id=" & Image_ID'Image(o.face(f).texture));
      end if;
    end loop;
    for p in o.point'Range loop
      if adjacent_faces(p) = 0 then
        if strict_geometry then
          -- Strict approach: detect any unmatched point:
          raise_exception(point_unmatched'Identity,
            o.id & " pt="&Integer'Image(p));
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
    use GL,G3DM;
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

  neutral_material_already_set: Boolean:= False;





  -------------
  -- Display --
  -------------

  procedure Display_one(o: in out Object_3D) is
  -- Display only this object and not connected objects
  -- out: object will be initialized if not yet

    procedure Display_face(fa: access Face_type; fi: in out Face_invariant_type) is
      use GL;
      blending_hint: Boolean;

    begin -- Display_face
      if fa.skin = invisible then
        return;
      end if;

      --------------
      -- Material --
      --------------

      case fa.skin is
        when material_only | material_texture =>
          Disable(COLOR_MATERIAL);
          Set_Material(fa.material);
          neutral_material_already_set:= False;
        when others =>
          -- Avoid setting again and again the neutral material
          if not neutral_material_already_set then
            Set_Material(neutral_material);
            neutral_material_already_set:= True;
          end if;
      end case;

      ------------
      -- Colour --
      ------------

      case fa.skin is
        when material_only | material_texture =>
          null; -- done above
        when colour_only | coloured_texture =>
          Enable(COLOR_MATERIAL);
          ColorMaterial(FRONT_AND_BACK, AMBIENT_AND_DIFFUSE);
          Color(
            red   => fa.colour.red,
            green => fa.colour.green,
            blue  => fa.colour.blue,
            alpha => fa.alpha
          );
        when texture_only =>
          Disable(COLOR_MATERIAL);
        when invisible =>
          null;
      end case;

      -------------
      -- Texture --
      -------------

      case fa.skin is
        when texture_only | coloured_texture | material_texture =>
          Enable( TEXTURE_2D );
          G3DT.Bind_2D_texture( fa.texture, blending_hint );
          if blending_hint then
            fi.blending:= True;
            -- 13-Oct-2006: override decision made at Pre_calculate
            -- if texture data contains an alpha layer
          end if;
        when colour_only | material_only =>
          Disable( TEXTURE_2D );
        when invisible =>
          null;
      end case;

      -----------------------------
      -- Blending / transparency --
      -----------------------------

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

      -------------
      -- Drawing --
      -------------

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

    end Display_face;



    procedure Display_normals is
      use GL,G3DM;
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
        for i in 1..o.face_invariant(f).last_edge loop
          C:= C+o.point(o.face_invariant(f).P_compact(i));
        end loop;
        C:= (1.0/Real(o.face_invariant(f).last_edge)) * C;
        Arrow(C, arrow_inflator * o.face_invariant(f).normal);
      end loop;
    end Display_normals;


    use GL, G3DM;


  begin -- Display_one

    if not o.pre_calculated then
      Pre_calculate(o);
    end if;

    gl.bindBuffer    (gl.ARRAY_BUFFER, 0);             -- disable 'vertex buffer objects'
    gl.bindBuffer    (gl.ELEMENT_ARRAY_BUFFER, 0);     -- disable 'vertex buffer objects' indices

--      gl.disableClientState (gl.TEXTURE_COORD_ARRAY);
--      gl.disable    (ALPHA_TEST);
    gl.enable (Lighting);


    GL.PushMatrix; -- 26-May-2006: instead of rotating/translating back
    GL.Translate( o.centre );
    Multiply_GL_Matrix(o.rotation);
    -- pre-30-May-2006:
    --  GL.Rotate( o.auto_rotation(0), 1.0, 0.0, 0.0 );
    --  GL.Rotate( o.auto_rotation(1), 0.0, 1.0, 0.0 );
    --  GL.Rotate( o.auto_rotation(2), 0.0, 0.0, 1.0 );

    -- Display the object in its own frame of reference
    for f in o.face'Range loop
       Display_face(o.face(f)'access, o.face_invariant(f));
    end loop;

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







  procedure Display(
    o    : in out Object_3D;
    clip : in     Clipping_data
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
        use G3DM, GL;
        dp: Real;
        plane_to_eye: Vector_3D; -- vector from any point in plane to the eye
        bounding_of_face, intersection_clip_and_face: Clipping_area;
        success, non_empty_intersection: Boolean;
      begin
        -- Culling #1: check if portal is in vield of view's "dead angle"
        dp:= o.face_invariant(f).normal * clip.view_direction;
        if dp < clip.max_dot_product then
          -- Culling #2: check if we are on the right side of the portal
          -- NB: ignores o.auto_rotation !
          plane_to_eye:=
            clip.eye_position -
            (o.point(o.face_invariant(f).P_compact(1))+o.centre)
          ;
          dp:= plane_to_eye * o.face_invariant(f).normal;
          -- dp = signed distance to the plane
          if dp > 0.0 then
            -- Culling #3: clipping rectangle
            Find_bounding_box( o, f, bounding_of_face, success );
            if success then
              Intersect( clip_area, bounding_of_face,
                         intersection_clip_and_face, non_empty_intersection );
            else
              -- in doubt, draw with the present clipping
              intersection_clip_and_face:= clip_area;
              non_empty_intersection:= True;
            end if;
            if non_empty_intersection then
              Display_clipped(
                o            => o.face(f).connecting.all,
                clip_area    => intersection_clip_and_face,
                portal_depth => portal_depth + 1
              );
            end if;
          end if;
        end if;
      end Try_portal;

    begin
      if not o.pre_calculated then
        Pre_calculate(o);
      end if;
      -- a/ Display connected objects which are visible through o's faces
      if (not filter_portal_depth) or else -- filter_portal_depth: test/debug
         portal_depth in 0..6
      then
        for f in o.face'Range loop
          if o.face(f).connecting /= null then
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
            x      => GL.Int(clip_area.x1),
            y      => GL.Int(clip_area.y1),
            width  => GL.SizeI(clip_area.x2 - clip_area.x1+1),
            height => GL.SizeI(clip_area.y2 - clip_area.y1+1)
          );
        else
          GL.Disable(GL.SCISSOR_TEST);
        end if;
        info_b_ntl2:= info_b_ntl2 + 1;
        Display_one(o);
      end if;
      if show_portals and then portal_depth > 0 then
        Draw_boundary(clip.main_clipping, clip_area);
      end if;
    end Display_clipped;

  begin
    info_b_ntl2:= 0; -- count amount of objects displayed, not distinct
    Display_clipped( o, clip_area => clip.main_clipping, portal_depth => 0 );
  end Display;


--
--
--
--    procedure Display(
--      o    : in out Object_3D;
--      Camera : in     globe_3d.Camera'class
--    )
--    is
--
--      use GLOBE_3D.Portals;
--
--      procedure Display_clipped(
--        o            : in out Object_3D'Class;
--        clip_area    : in     Clipping_area;
--        portal_depth : in     Natural
--      )
--      is
--        procedure Try_portal(f: Positive) is
--          use G3DM, GL;
--          dp: Real;
--          plane_to_eye: Vector_3D; -- vector from any point in plane to the eye
--          bounding_of_face, intersection_clip_and_face: Clipping_area;
--          success, non_empty_intersection: Boolean;
--        begin
--          -- Culling #1: check if portal is in vield of view's "dead angle"
--          dp:= o.face_invariant(f).normal * Camera.clipper.view_direction;
--          if dp < Camera.clipper.max_dot_product then
--            -- Culling #2: check if we are on the right side of the portal
--            -- NB: ignores o.auto_rotation !
--            plane_to_eye:=
--              Camera.clipper.eye_position -
--              (o.point(o.face_invariant(f).P_compact(1))+o.centre)
--            ;
--            dp:= plane_to_eye * o.face_invariant(f).normal;
--            -- dp = signed distance to the plane
--            if dp > 0.0 then
--              -- Culling #3: clipping rectangle
--              Find_bounding_box( o, f, bounding_of_face, success );
--              if success then
--                Intersect( clip_area, bounding_of_face,
--                           intersection_clip_and_face, non_empty_intersection );
--              else
--                -- in doubt, draw with the present clipping
--                intersection_clip_and_face:= clip_area;
--                non_empty_intersection:= True;
--              end if;
--              if non_empty_intersection then
--                Display_clipped(
--                  o            => o.face(f).connecting.all,
--                  clip_area    => intersection_clip_and_face,
--                  portal_depth => portal_depth + 1
--                );
--              end if;
--            end if;
--          end if;
--        end Try_portal;
--
--      begin
--        if not o.pre_calculated then
--          Pre_calculate(o);
--        end if;
--        -- a/ Display connected objects which are visible through o's faces
--        if (not filter_portal_depth) or else -- filter_portal_depth: test/debug
--           portal_depth in 0..6
--        then
--          for f in o.face'Range loop
--            if o.face(f).connecting /= null then
--              Try_portal(f);
--            end if;
--          end loop;
--        end if;
--        -- b/ Display the object itself
--        if (not filter_portal_depth) or else -- filter_portal_depth: test/debug
--           (portal_depth = 1 or portal_depth = 5)
--        then
--          -- The graphical clipping (Scissor) gives various effects
--          -- - almost no speedup on the ATI Radeon 9600 Pro (hardware)
--          -- - factor: ~ Sqrt(clipped surface ratio) with software GL
--          if portal_depth > 0 then
--            GL.Enable(GL.SCISSOR_TEST);
--            GL.Scissor(
--              x      => GL.Int(clip_area.x1),
--              y      => GL.Int(clip_area.y1),
--              width  => GL.SizeI(clip_area.x2 - clip_area.x1+1),
--              height => GL.SizeI(clip_area.y2 - clip_area.y1+1)
--            );
--          else
--            GL.Disable(GL.SCISSOR_TEST);
--          end if;
--          info_b_ntl2:= info_b_ntl2 + 1;
--          Display_one(o);
--        end if;
--        if show_portals and then portal_depth > 0 then
--          Draw_boundary(Camera.clipper.main_clipping, clip_area);
--        end if;
--      end Display_clipped;
--
--    begin
--      info_b_ntl2:= 0; -- count amount of objects displayed, not distinct
--      Display_clipped( o, clip_area => Camera.clipper.main_clipping, portal_depth => 0 );
--    end Display;



   procedure destroy (o : in out Object_3D)
   is
   begin
      null;
   end;




   procedure set_Alpha ( o    : in out Object_3D;   Alpha : in gl.Double)
   is
   begin
      for f in o.face'Range loop
         o.face (f).alpha := Alpha;
      end loop;
   end;



   function  is_Transparent (o    : in Object_3D) return Boolean
   is
   begin
      return True; -- tbd: do proper check in pre_calculate and cache result (ie check faces for transparency or portals)
   end;



   function face_Count (o : in Object_3D) return Natural
   is
   begin
      return o.Max_faces;
   end;



   function  Bounds         (o : in Object_3D) return ogl.geometry.Bounds_record
   is
   begin
      return o.Bounds;
   end;




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

  prec_a360    : constant:= 10000;
  r_prec_a360  : constant:= 10000.0;
  i_r_prec_a360: constant:= 1.0 / r_prec_a360;

  procedure Angles_modulo_360( v: in out Vector_3D )is
    use GL;
  begin
    for i in v'Range loop
      v(i):=
        GL.Double(Integer(r_prec_a360 * v(i)) mod (360*prec_a360))
        * i_r_prec_a360;
    end loop;
  end Angles_modulo_360;

  ------------------
  -- Resource I/O --
  ------------------

  procedure Load_if_needed( zif: in out Zip.Zip_info; name: String) is
  begin
    if not Zip.Is_loaded(zif) then
      begin
        Zip.Load( zif, name );
      exception
        when Zip.Zip_file_open_error => -- Try with lower case:
          Zip.Load( zif, To_Lower(name) );
      end;
    end if;
  end Load_if_needed;

  procedure Set_level_data_name(s: String) is
  begin
    if Zip.Is_loaded( zif_level ) then
      Zip.Delete( zif_level );
    end if;
    -- ^ Possible resource name change -> need this, will be reloaded on next use
    level_data_name:= Ada.Strings.Unbounded.To_Unbounded_String(s);
  end Set_level_data_name;

  procedure Set_global_data_name(s: String) is
  begin
    if Zip.Is_loaded( zif_global ) then
      Zip.Delete( zif_global );
    end if;
    -- ^ Possible resource name change -> need this, will be reloaded on next use
    global_data_name:= Ada.Strings.Unbounded.To_Unbounded_String(s);
  end Set_global_data_name;

  procedure Set_name(o: in out Visual'class; new_name: String) is
  begin
    if new_name'Length > Ident'Length then
      raise Constraint_Error;
    end if;
    o.ID:= empty;
    o.ID(1..new_name'Length):= new_name;
  end Set_name;

  function Get_name(o: Visual'class) return String is
  begin
    return Trim(o.id,right);
  end Get_name;

  procedure Rebuild_links(
    o           : in out Object_3D;   -- object to be relinked
    neighbouring: in Object_3D_array; -- neighbourhood
    tolerant_obj: in Boolean;         -- tolerant on missing objects
    tolerant_tex: in Boolean          -- tolerant on missing textures
  ) is
    found: Boolean;
  begin
    for f in o.face'Range loop
      -- 1/ Find texture IDs:
      if is_textured(o.face(f).skin) and then
         o.face_invariant(f).texture_name /= empty
      then
        begin
          o.face(f).texture:=
            Textures.Texture_ID( o.face_invariant(f).texture_name );
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
      -- 2/ Connections through portals:
      if o.face_invariant(f).connect_name /= empty then
        found:= False;
        for i in neighbouring'Range loop
          if neighbouring(i).ID = o.face_invariant(f).connect_name then
            o.face(f).connecting:= neighbouring(i);
            found:= True;
            exit;
          end if;
        end loop;
        if not found then
          if tolerant_obj then
            o.face(f).connecting:= null;
          else
            raise_exception(
              Portal_connection_failed'Identity,
              "For object name [" & Trim(o.ID,right) &
              "], looking for [" &
              Trim(o.face_invariant(f).connect_name,right)
              & ']'
            );
          end if;
        end if;
      end if;
    end loop;
  end Rebuild_links;

  procedure Texture_name_hint(
    o   : in out Object_3D;
    face:        Positive;
    name:        String
  )
  is
  begin
--      ada.text_io.put_Line ("NAME: '" & name & "'");

    if name'Length > Ident'Length then raise Constraint_Error; end if;
    o.face_invariant(face).texture_name:= empty;
    o.face_invariant(face).texture_name(1..name'Length):= name;
  end Texture_name_hint;

  procedure Portal_name_hint(
    o   : in out Object_3D;
    face:        Positive;
    name:        String
  )
  is
  begin
    if name'Length > Ident'Length then raise Constraint_Error; end if;
    o.face_invariant(face).connect_name:= empty;
    o.face_invariant(face).connect_name(1..name'Length):= name;
  end Portal_name_hint;




   -- camera
   --

   procedure set_frustum_Planes (Self : in out Camera'Class)   -- tbd: this seems buggy still :/ ... needs testing !!
   is
      use GL, oGL.Frustums, globe_3d.Math;

      the_Planes  : ogl.frustums.plane_Array renames self.frustum_Planes;

      nc, fc      : Vector_3D;
      aux, normal : Vector_3D;
      X, Y, Z     : Vector_3D;
      p, l, u     : Vector_3D;
      the_Point   : Vector_3D;

   begin
      p (0) := self.Clipper.eye_position (0);
      p (1) := self.Clipper.eye_position (1);
      p (2) := self.Clipper.eye_position (2);

      l (0) := p (0) + self.Clipper.view_Direction (0);
      l (1) := p (1) + self.Clipper.view_Direction (1);
      l (2) := p (2) + self.Clipper.view_Direction (2);

      u (0) := self.world_Rotation (2, 1);
      u (1) := self.world_Rotation (2, 2);
      u (2) := self.world_Rotation (2, 3);


      -- compute the Z axis of camera this axis points in the opposite direction from the looking direction
      Z := p - l;
      Z := normalized (Z);

      -- X axis of camera with given "up" vector and Z axis
      X := u * Z;
      X := normalized (X);

      -- the real "up" vector is the cross product of Z and X
      Y := Z * X;

      -- compute the centers of the near and far planes
      nc := p - Z * self.near_plane_Distance;
      fc := p - Z * self.far_plane_Distance;

      -- Near
      --
      the_Planes (Near) (0) := -Z (0);
      the_Planes (Near) (1) := -Z (1);
      the_Planes (Near) (2) := -Z (2);
      the_Planes (Near) (3) := -Z * nc;

      -- Far
      --
      the_Planes (Far) (0) := Z (0);
      the_Planes (Far) (1) := Z (1);
      the_Planes (Far) (2) := Z (2);
      the_Planes (Far) (3) := Z * fc;

      -- High/Top
      --
      aux       := (nc + Y * self.near_plane_Height) - p;
      aux       := normalized (aux);
      normal    := aux * X;
      the_Point := nc + Y * self.near_plane_Height;

      the_Planes (High) (0) := Normal (0);
      the_Planes (High) (1) := Normal (1);
      the_Planes (High) (2) := Normal (2);
      the_Planes (High) (3) := normal * the_Point;

      -- Low/Bottom
      --
      aux       := (nc - Y * self.near_plane_Height) - p;
      aux       := normalized (aux);
      normal    := X * aux;
      the_Point := nc - Y * self.near_plane_Height;

      the_Planes (Low) (0) := Normal (0);
      the_Planes (Low) (1) := Normal (1);
      the_Planes (Low) (2) := Normal (2);
      the_Planes (Low) (3) := Normal * the_Point;

      -- Left
      --
      aux       := (nc - X * self.near_plane_Width) - p;
      aux       := normalized (aux);
      normal    := Aux * Y;
      the_Point := nc - X * self.near_plane_Width;

      the_Planes (Left) (0) := Normal (0);
      the_Planes (Left) (1) := Normal (1);
      the_Planes (Left) (2) := Normal (2);
      the_Planes (Left) (3) := Normal * the_Point;

      -- Right
      --
      aux       := (nc + X * self.near_plane_Width) - p;
      aux       := normalized (aux);
      normal    := Y * Aux;
      the_Point := nc + X * self.near_plane_Width;

      the_Planes (Right) (0) := Normal (0);
      the_Planes (Right) (1) := Normal (1);
      the_Planes (Right) (2) := Normal (2);
      the_Planes (Right) (3) := Normal * the_Point;


      normalise (the_Planes);
   end;





   ----------------------------------------
   -- tbd: has been moved (for the moment) external to 'render' for performance, but this makes package task unsafe !
   --
   --
      type visual_Geometry is
         record
            Visual   : p_Visual;
            Geometry : ogl.skinned_geometry.skinned_Geometry;
         end record;
      pragma Convention (C, visual_Geometry);  -- using convention pragma to disable default initialization (for performance)

      type visual_Geometrys is array (Positive range <>) of visual_Geometry;
      pragma Convention (C, visual_Geometrys);  -- using convention pragma to disable default initialization (for performance)

   all_Geometrys     : visual_geometrys (1 .. 80_000);   pragma Convention (C, all_Geometrys);  -- tbd: this is slow !
   --
   --------------------------------------


   procedure render (the_Visuals : in Visual_array;   the_Camera : in Camera)
   is
      use GL, REF, G3DM;

      all_Transparents  : globe_3d.Visual_array (1 .. 10_000);
      transparent_Count : Natural                           := 0;

      geometry_Count    : Natural                       := 0;   -- for 'all_Geometrys' array.

      current_Skin      : ogl.skins.p_Skin;

   begin
      -- prepare openGL to display visuals.
      --
      Clear    (COLOR_BUFFER_BIT or DEPTH_BUFFER_BIT);
      Enable   (DEPTH_TEST);

      Enable   (LIGHTING);                               -- enable lighting for G3D.Display in 'separate Visuals' (obsolete).
      Enable   (CULL_FACE);
      CullFace (BACK);

      MatrixMode    (MODELVIEW);
      Set_GL_Matrix (the_Camera.world_rotation);
      Translate     (-the_Camera.Clipper.eye_Position (0),  -the_Camera.Clipper.eye_Position (1),  -the_Camera.Clipper.eye_Position (2));

      PushMatrix;

      -- separate Visuals
      --
      for Each in the_Visuals'range loop
         declare
            the_Visual       : Visual'Class                          renames the_Visuals (Each).all;
            visual_Geometrys : ogl.skinned_geometry.skinned_Geometrys renames skinned_Geometrys (the_visual);
         begin
            if is_transparent (the_Visual) then
               transparent_Count                    := transparent_Count + 1;
               all_Transparents (transparent_Count) := the_Visual'access;
            else
               for Each in visual_Geometrys'Range loop
                  geometry_Count                          := geometry_Count + 1;
                  all_Geometrys (geometry_Count).Visual   := the_Visual'access;
                  all_Geometrys (geometry_Count).Geometry := visual_Geometrys (Each);
               end loop;

               Display (the_Visuals (Each).all,  the_Camera.Clipper);
            end if;
         end;
      end loop;


      ogl.Errors.log;


      -- display all opaque geometries, sorted by gl geometry primitive kind and skin.
      --
      declare
         function "<" (L, R : in visual_geometry) return Boolean
         is
            use ogl.Geometry, System.Storage_Elements;
         begin
            if primitive_Id (L.Geometry.Geometry.all)  =  primitive_Id (R.Geometry.Geometry.all) then   -- tbd: find better naming scheme to avoid '.Geometry.Geometry.'
               return to_Integer (L.Geometry.Skin.all'address)  <  to_Integer (R.Geometry.Skin.all'address); -- tbd: check this is safe/portable

            elsif primitive_Id (L.Geometry.Geometry.all)  <  primitive_Id (R.Geometry.Geometry.all) then
               return True;

            else -- L.Geometry.primitive_Id > R.Geometry.primitive_Id
               return False;
            end if;
         end "<";

         --procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
         procedure sort is new Ada_Containers_Generic_Array_Sort (Positive,
                                                                  visual_geometry,
                                                                  visual_geometrys);
         use oGL.Skins, oGL.Geometry, oGL.Skinned_Geometry;

         current_Visual : p_Visual;

      begin
         if geometry_Count > 1 then
            sort (all_Geometrys (1 .. geometry_Count));
         end if;

         GL.PushMatrix;

         for Each in 1 .. geometry_Count loop

            if all_Geometrys (Each).Geometry.Skin /= current_Skin then
               current_Skin := all_Geometrys (Each).Geometry.Skin;
               enable (current_Skin.all);
               ogl.Errors.log;
            end if;

            if all_Geometrys (Each).Geometry.Veneer /= null then
               enable (all_Geometrys (Each).Geometry.Veneer.all);
               ogl.Errors.log;
            end if;


            if all_Geometrys (Each).Visual = current_Visual then
               draw (all_Geometrys (Each).Geometry.Geometry.all);
               ogl.Errors.log;
            else
               GL.PopMatrix;
               GL.PushMatrix;
               GL.Translate       (all_Geometrys (Each).Visual.centre);
               Multiply_GL_Matrix (all_Geometrys (Each).Visual.rotation);

               draw (all_Geometrys (Each).Geometry.Geometry.all);
               ogl.Errors.log;

               current_Visual := all_Geometrys (Each).Visual;
            end if;

         end loop;

         GL.PopMatrix;
      end;

      ogl.Errors.log;

      -- display all transparent visuals, sorted from far to near.
      --
      declare
         use Math;

         function "<" (L, R : in globe_3d.p_Visual) return Boolean -- tbd : ugh move expensive calcs outside
         is
         begin
            return L.Centre_camera_space (2) < R.Centre_camera_space (2);  -- nb: in camera space, negative Z is forward, so use '<'.
         end;

         --procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
         procedure sort is new Ada_Containers_Generic_Array_Sort (Positive,
                                                                  globe_3d.p_Visual,
                                                                  globe_3d.Visual_array);
      begin
         for Each in 1 .. transparent_Count loop  -- pre-calculate each visuals Centre in camera space.
            all_Transparents (Each).Centre_camera_space :=   the_Camera.world_Rotation
                                                           * (all_Transparents (Each).Centre - the_Camera.Clipper.eye_Position);
         end loop;

         if transparent_Count > 1 then
            sort (all_Transparents (1 .. transparent_Count));
         end if;

         gl.depthMask (gl_False);  -- make depth buffer read-only, for correct transparency

         Enable    (LIGHTING);   -- ensure lighting is enabled for G3D.Display of transparents (obsolete).
         Enable    (BLEND);
         BlendFunc (sfactor => ONE,
                    dfactor => ONE_MINUS_SRC_ALPHA);

         for Each in 1 .. transparent_Count loop
            declare
               the_Visual       : Visual'Class                          renames all_Transparents (Each).all;
               visual_Geometrys : constant ogl.skinned_geometry.skinned_Geometrys      := skinned_Geometrys (the_visual); -- tbd: apply ogl state sorting here ?
            begin
               display (the_Visual,  the_Camera.clipper);
               ogl.Errors.log;

               for Each in visual_Geometrys'Range loop
                  declare
                     use ogl.Skins, ogl.Geometry;
                     the_Geometry : ogl.skinned_geometry.skinned_Geometry renames visual_Geometrys (Each);
                  begin

                     if the_Geometry.Skin /= current_Skin then
                        current_Skin := the_Geometry.Skin;
                        enable (current_Skin.all);
                        ogl.Errors.log;
                     end if;

                     if the_Geometry.Veneer /= null then
                        enable (the_Geometry.Veneer.all);
                        ogl.Errors.log;
                     end if;

                     GL.PushMatrix;

                     GL.Translate       (the_Visual.centre);
                     Multiply_GL_Matrix (the_Visual.rotation);

                     draw (the_Geometry.Geometry.all);
                     ogl.Errors.log;

                     GL.PopMatrix;
                  end;
               end loop;

            end;
         end loop;

         gl.depthMask (gl_True);
      end;


      PopMatrix;

      ogl.Errors.log;      -- tbd: for debug only
   end render;



end GLOBE_3D;

