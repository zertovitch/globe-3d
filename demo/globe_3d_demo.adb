------------------------------------------------------------------------------
--  File:            GLOBE_3D_Demo.adb
--  Description:     A small demo for GLOBE_3D
--  Copyright (c) Gautier de Montmollin 2002, 2005, 2006, 2008, 2010
------------------------------------------------------------------------------

with GL,
     GL.IO,
     GL.Materials;

with GLOBE_3D,
     GLOBE_3D.IO,
     GLOBE_3D.BSP,
     GLOBE_3D.Options,
     GLOBE_3D.Math,
     GLOBE_3D.Textures,
     GLOBE_3D.Software_Anti_Aliasing,
     GLOBE_3D.Stars_sky,
     GLOBE_3D.Collision_detection;

with GLU, GLUT.Devices, GLUT_2D;

with Actors, Game_control;

---------------
-- 3D models --
---------------

with Vehic001, Vehic002, X29,
     Brick, Icosahedron,
     SkotKnot, Lissajous,
     Planet,
     Dreadnought,
     --  VRML_scene,
     --  gmax_scene,
     --  Doom3_Level,
     Extruded_surface,
     Sierpinski
     ;

-- with Ada.Text_IO;

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Command_Line;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

procedure GLOBE_3D_Demo is

  --pragma Linker_options("-mwindows"); -- Suppress console window

  package G3D  renames GLOBE_3D;
  package G3DM renames G3D.Math;

  fairly_far: constant:= 10_000.0;

  package Stars is new GLOBE_3D.Stars_sky(
    num_stars => 5_000,
    far_side  => fairly_far
  );

  GLUT_Problem: exception;

  main_size_x, main_size_y: GL.SizeI;

  foggy: constant Boolean:= False;

  frontal_light: G3D.Light_definition;

  procedure Prepare_demo_lighting(fact: GL.Float) is
    use GL, G3D;
    proto_light: Light_definition:=
      (position => (3.0, 4.0, 10.0, 1.0),
       ambient  => (0.1, 0.1, 0.1, fact),
       diffuse  => (0.9, 0.9, 0.9, fact),
       specular => (0.05, 0.05, 0.01, fact));
  begin
    Enable( LIGHTING );
    G3D.Define( 1, proto_light);
    frontal_light:= proto_light;
    proto_light.diffuse:= (0.5, 0.9, 0.5, fact);
    G3D.Define( 2, proto_light);
    proto_light.diffuse:= (0.2, 0.0, 0.9, fact);
    proto_light.specular:= (1.0, 1.0, 1.0, fact);
    G3D.Define( 3, proto_light);
    proto_light.position:= (-3.0, 4.0, 10.0, 1.0);
    G3D.Define( 4, proto_light);
    proto_light.position:= (3.0, -4.0, 10.0, 1.0);
    proto_light.ambient := (0.6, 0.6, 0.6, 0.1);
    G3D.Define( 5, proto_light);
    proto_light.ambient := (0.6, 0.0, 0.0, 0.1);
    G3D.Define( 6, proto_light);
    proto_light.ambient := (0.0, 0.6, 0.0, 0.1);
    G3D.Define( 7, proto_light);
    proto_light.ambient := (0.0, 0.0, 0.6, 0.1);
    G3D.Define( 8, proto_light);
    G3D.Switch_lights(True);
    G3D.Switch_light(4,False);
    G3D.Switch_light(5,False);
    G3D.Switch_light(6,False);
    G3D.Switch_light(7,False);
    G3D.Switch_light(8,False);
  end Prepare_demo_lighting;

  procedure Clear_modes is
    use GL;
  begin
    Disable( BLEND );
    Disable( LIGHTING );
    Disable( AUTO_NORMAL );
    Disable( NORMALIZE );
    Disable( DEPTH_TEST );
  end Clear_modes;

  deg2rad   : constant:= pi / 180.0;

  ego: G3D.Camera;

  procedure Reset_for_3D( width, height: Integer ) is
    use GL, G3D, G3D.REF;
    aspect, half_fov_max_rads, fovy: Real;
  begin
    Viewport(0, 0, sizei(width), sizei(height));
    MatrixMode( PROJECTION );
    LoadIdentity;
    aspect:= GL.Double(width) / GL.Double(height);
    fovy:= ego.FOVy;
    half_fov_max_rads:= 0.5 * fovy * deg2rad;
    if aspect > 1.0 then -- x side angle broader than y side angle
      half_fov_max_rads:= ArcTan(aspect * Tan(half_fov_max_rads));
    end if;
    ego.clipper.max_dot_product:= Sin(half_fov_max_rads);
    ego.clipper.main_clipping:= (0,0, width-1, height-1);
    ego.compose_rotations:= False; -- otherwise you get sea-sick!
    GLU.Perspective(
      fovy   => fovy,
      -- field of view angle (deg) in the y direction
      aspect => aspect,
      -- x/y aspect ratio
      zNear  => 1.0,
      -- distance from the viewer to the near clipping plane
      zFar   => fairly_far
      -- distance from the viewer to the far clipping plane
    );
    -- The matrix generated by GLU.Perspective is
    -- multipled by the current matrix
    MatrixMode( MODELVIEW );
    ShadeModel( SMOOTH ); -- GL's default is SMOOTH, vs FLAT
    ClearColor(0.0, 0.0, 0.0, 0.0);
    -- ^ Specifies clear values for color buffer(s)
    ClearAccum(0.0, 0.0, 0.0, 0.0);
    -- ^ Specifies clear values for the accumulation buffer
  end Reset_for_3D;

  forget_mouse: Natural:= 0;
  capturing_video: Boolean:= False;

  procedure Window_Resize( width, height: Integer ) is
  begin
    if capturing_video then
      GL.IO.Stop_capture;
    end if;
    main_size_x:= GL.sizei(width);
    main_size_y:= GL.sizei(height);
    forget_mouse:= 5;
    Reset_for_3D( Integer(main_size_x), Integer(main_size_y ));
  end Window_Resize;

  full_screen: Boolean:= False;

  procedure Say_bye_to_GLUT is
  begin
    if capturing_video then
      GL.IO.Stop_capture;
    end if;
    GLUT.LeaveMainLoop;
  end Say_bye_to_GLUT;

  procedure Menu( value: Integer ) is
    --  GameModeString("800x600:16@60") ;
    --  if GameModeGet(GLUT_GAME_MODE_WIDTH) /= -1 then -- if the width is different to -1
    --     EnterGameMode;                               -- enter full screen mode
    --  else                                            -- print out that this mode is not available
    --     printf("The current mode is not supported in this system\n") ;

    -- res: Integer;
    --  Full_Screen_Mode   : constant String:= "640x480:16@60";
    --  Full_Screen_Mode_2 : constant String:= "400x300:16@60";
  begin
    case value is
      when 1 => -- GLUT.GameModeString (Full_Screen_Mode);
                GLUT.FullScreen;
                -- res := GLUT.EnterGameMode;
                GLUT.SetCursor(GLUT.CURSOR_NONE);
                forget_mouse:= 10;
                full_screen:= True;
      when 2 => Say_bye_to_GLUT;
      when others => null;
    end case;
  end Menu;

  alpha: GL.Double:= 1.0;

  bri1, bri2,
  ico, icos,
  -- vrml,
  -- gmax,
  knot, liss,
  globe,
  x29_plane,
  vhc_001, vhc_002,
  dreadnought_ship,
  extrude_test_1, borg_star,
  sierp,
  cube, cube_tri, cube_bico : G3D.p_Object_3D;

  bestiaire, level_stuff: G3D.p_Object_3D_array:= null;
  level_idx, bri_idx, beast: Integer;
  level_BSP: G3D.BSP.p_BSP_node:= null;
  level_map: G3D.Map_of_Visuals:= G3D.empty_map; -- dictionary

  procedure Load_Doom(name: String) is
    area_max: Natural:= 0;
    ls: G3D.Object_3D_array(1..1_000);
    empty_level: exception;
  begin
    for i in ls'Range loop
      begin
        G3D.IO.Load(
          name & "_$_area" & Trim(Integer'Image(i-1),Left),
          ls(i)
        );
      exception
        when G3D.Missing_object =>
          exit;
      end;
      area_max:= i;
      G3D.Add(level_map, G3D.p_Visual(ls(i))); -- add to dictionary
    end loop;
    begin
      G3D.IO.Load(name, level_map, level_BSP); -- load BSP tree
    exception
      when G3D.Missing_object =>
        null; -- Some custom levels like the Reims Cathedral have no BSP
    end;
    level_stuff:= new G3D.Object_3D_array'(ls(1..area_max));
    if area_max = 0 then
      raise empty_level;
    end if;
  end Load_Doom;

  procedure Create_objects(load: Boolean; doom3_custom: String) is
    t: constant:= 1.0;
    f2: Natural;
    use GL, G3D, G3D.Textures;

    function Basic_face(
      P       : G3D.Index_array;
      tex_name: String;
      colour  : GL.RGB_Color;
      repeat  : Positive)
    return Face_type
    is
      f: Face_type; -- takes defaults values
    begin
      f.P       := P;
      f.skin    := coloured_texture;
      f.texture := Texture_ID(tex_name);
      f.colour  := colour;
      f.alpha   := alpha;
      f.repeat_U:= repeat;
      f.repeat_V:= repeat;
      return f;
    end Basic_face;

    portal1, portal2: Brick.Cubic_Face_index;

  begin
    -- Basic cube
    cube:= new G3D.Object_3D( Max_points=> 8, Max_faces=> 6 );
    cube.centre:= (0.0,0.0,0.0);
    cube.point:=
      ( (-t,-t,-t), (-t, t,-t), ( t, t,-t), ( t,-t,-t),
        (-t,-t, t), (-t, t, t), ( t, t, t), ( t,-t, t));
    cube.face:=
      ( Basic_face((3,2,6,7),"face1",(1.0,0.0,0.0),1),
        Basic_face((4,3,7,8),"face2",(0.0,1.0,0.0),2),
        Basic_face((8,7,6,5),"face3",(0.0,0.0,1.0),3),
        Basic_face((1,4,8,5),"face4",(1.0,1.0,0.0),4),
        Basic_face((2,1,5,6),"face5",(0.0,1.0,1.0),5),
        Basic_face((3,4,1,2),"face6",(1.0,0.0,1.0),6));
    Set_name(cube.all,"Trust the Cube !");
    --
    -- Basic cube, but with half-faces as triangles
    -- must look identical as the first one
    cube_tri:= new G3D.Object_3D( Max_points=> 8, Max_faces=> 12 );
    cube_tri.centre:= cube.centre;
    cube_tri.point:= cube.point;
    for f in cube.face'Range loop
      f2:= (f-cube.face'First)*2+cube_tri.face'First;
      cube_tri.face(f2)       := cube.face(f);
      cube_tri.face(f2).P(1)  := 0;
      cube_tri.face(f2+1)     := cube.face(f);
      cube_tri.face(f2+1).P(3):= 0;
      -- Now we have killed opposite edges, one on each triangle :-)
    end loop;
    Set_name(cube_tri.all,"Triangular Cube !");

    -- Also a cube with half-faces, but playing with colour/texture
    cube_bico:= new G3D.Object_3D'Class'(cube_tri.all);
    Set_name(cube_bico.all,"Technicolor");
    for f in cube_bico.face'Range loop
      if f mod 2 = 0 then
        cube_bico.face(f).skin:= colour_only;
      end if;
    end loop;

    X29.Create(
      object => x29_plane,
      scale  => 1.0,
      centre => (0.0,0.0,-17.0)
    );

    -- Space vehicles
    Vehic001.Create(
      object => vhc_001,
      scale  => 0.5,
      centre => (0.0,0.0,-25.0)
    );

    Vehic002.Create(
      object => vhc_002,
      scale  => 100.0,
      centre => (80.0,0.0,-70.0),
      metal_door    => Texture_id("portmet1"),
      metal_surface => Texture_id("fdmetal1"),
      bumped_blue   => Texture_id("bleubosl")
    );

    Pre_calculate(vhc_002.all);

    Icosahedron.Create(
      object => ico,
      scale  => 0.6,
      centre => (0.0,0.0,0.0),
      alpha  => 0.8,
      polyball => False
    );

    Icosahedron.Create(
      object => icos,
      scale  => 0.6,
      centre => (0.0,0.0,0.0),
      alpha  => 0.5,
      polyball => True
    );

    --  VRML_scene.Create(
    --    object => vrml,
    --    scale  => 0.05,
    --    centre => (0.0,0.0,-50.0)
    --  );

    -- Pre_calculate(vrml.all);

    Dreadnought.Create(
      object => dreadnought_ship,
      scale  => 0.065,
      centre => (0.0,-250.0,-500.0),
      alum_001 => Texture_id("alum_001"),
      alum_002 => Texture_id("alum_002"),
      grumnoir => Texture_id("grumnoir"),
      tole_001 => Texture_id("tole_001")
    );
    Pre_calculate(dreadnought_ship.all);

    Extruded_surface.Create(
      object     => extrude_test_1,
      scale      => 20.0,
      centre     => (-8.0,-8.0,-20.0),
      grid       => 40,
      surface    => Extruded_surface.square,
      max_u3     => 0.2,
      iterations => 100,
      hor_tex    => Texture_id("spacity1"),
      ver_tex    => Texture_id("spacity1"),
      tiling_hu  => 1,
      tiling_hv  => 1,
      tiling_vu  => 2,
      tiling_vv  => 2
    );
    Set_name(extrude_test_1.all,"Space City");

    Extruded_surface.Create(
      object     => borg_star,
      scale      => 500.0,
      centre     => (0.0,0.0,-1000.0),
      grid       => 65,
      surface    => Extruded_surface.sphere,
      max_u3     => 0.03,
      iterations => 2000,
      hor_tex    => Texture_id("alum_001"),
      ver_tex    => Texture_id("spacity1"),
      tiling_hu  => 30, -- ~ 2 * v-tiling
      tiling_hv  => 15,
      tiling_vu  => 31, -- should be ~ 2*pi* v-tiling
      tiling_vv  => 5
    );
    Set_name(borg_star.all,"Borg Star");

    Sierpinski.Create_Cube(
      object  => sierp,
      scale   => 200.0,
      centre  => (0.0,0.0,-300.0),
      texture => (Texture_id("face1"),
                  Texture_id("face2"),
                  Texture_id("face3"),
                  Texture_id("face4"),
                  Texture_id("face5"),
                  Texture_id("face6")),
      tiled   => False,
      fractal_level => 2
    );

    Planet.Create(
      object   => globe,
      scale    => 10.0,
      centre   => (0.0,0.0,-50.0),
      mercator => Texture_id("earth_map"),
      parts    => 45
    );
    Set_name(globe.all,"The Earth !");
    Pre_calculate(globe.all);

    SkotKnot.Create(
      object => knot,
      scale  => 1.0,
      centre => (0.0,0.0,-40.0)
    );
    Pre_calculate(knot.all);

    Lissajous.Create(
      object => liss,
      scale  => 1.0,
      centre => (0.0,0.0,-25.0)
    );
    Pre_calculate(liss.all);

    for i in icos.face'Range loop
      icos.face(i).skin:= material_only;
      case (i-1) / 20 is
        -- Non-transparent things
        when  1 => icos.face(i).material:= GL.Materials.Brass;
        when  2 => icos.face(i).material:= GL.Materials.Bronze;
        when  3 => icos.face(i).material:= GL.Materials.Copper;
        when  4 => icos.face(i).material:= GL.Materials.Polished_Copper;
        when  5 => icos.face(i).material:= GL.Materials.Gold;
        when  6 => icos.face(i).material:= GL.Materials.Polished_Bronze;
        -- Transparent things (Nabokov!)
        when  7 => icos.face(i).material:= GL.Materials.Pewter;
        when  8 => icos.face(i).material:= GL.Materials.Pearl;
        when  9 => icos.face(i).material:= GL.Materials.Obsidian;
        when 10 => icos.face(i).material:= GL.Materials.Jade;
        when 11 => icos.face(i).material:= GL.Materials.Emerald;
        when  0 => icos.face(i).material:= GL.Materials.Ruby;
        when others => null;
      end case;
    end loop;

    --
    -- Load a Doom 3 level from .g3d/.bsp files
    --
    if doom3_custom = "" then
      Load_Doom("Delta4g1");
    else
      G3D.Set_level_data_name(doom3_custom & ".zip");
      G3D.Textures.Reset_textures;
      G3D.Textures.Register_textures_from_resources;
      Load_Doom(doom3_custom);
    end if;

    if load and doom3_custom = "" then
      -- We test here the loading and mutual linking
      -- of some objects dumped by the -dump option.
      --
      -- Load the space station scene:
      --
      G3D.IO.Load("Space station brick ONE",bri1);
      G3D.IO.Load("Space station brick TWO",bri2);
      -- Relink both bricks:
      declare
        bricks_map: constant Map_of_Visuals:= Map_of((p_Visual(bri1),p_Visual(bri2)));
      begin
        G3D.Rebuild_links(bri1.all,bricks_map,False,False);
        G3D.Rebuild_links(bri2.all,bricks_map,False,False);
      end;
      Set_name(bri1.all,"Space station brick ONE (loaded)");
      Set_name(bri2.all,"Space station brick TWO (loaded)");
    else
      -- Create objects, don't load them (default).
      --
      -- Create the space station scene:
      --
      Brick.Create(
        object  => bri1,
        scale   => 100.0,
        centre  => (0.0,0.0,0.0),
        kind    => Brick.cube,
        opening => (5 => True, others=> False),
        portal  => portal1,
        texture => (Texture_id("face1"),
                    Texture_id("face2"),
                    Texture_id("face3"),
                    Texture_id("face4"),
                    Texture_id("face5"),
                    Texture_id("face6"))
      );
      Set_name(bri1.all,"Space station brick ONE");

      Brick.Create(
        object  => bri2,
        scale   => 100.0,
        centre  => (0.0,0.0,-100.0),
        kind    => Brick.cube,
        opening => (5|6 => True, others=> False),
        portal  => portal2,
        texture => (others => Texture_id("alum_002"))
      );
      Set_name(bri2.all,"Space station brick TWO");

      -- Connecting portals:
      bri1.face(portal1(5)).connecting:= bri2;
      bri2.face(portal2(6)).connecting:= bri1;
    end if;

    -- Relink Doom 3 level (either loaded or created):
    if level_stuff /= null then
      for i in level_stuff'Range loop
        -- NB:
        -- - portals may have been already linked (if created, not loaded);
        -- - textures need to be linked
        G3D.Rebuild_links(level_stuff(i).all, level_map,False,False);
        G3D.Pre_calculate(level_stuff(i).all);
      end loop;
    end if;

    -- Whole 3D zoo:
    bestiaire:= new Object_3D_array'(
      level_stuff(level_stuff'First),
      globe,
      sierp,
      extrude_test_1, borg_star,
      -- vrml,
      dreadnought_ship,
      cube, cube_tri, cube_bico,
      ico, icos,
      x29_plane,
      vhc_001, vhc_002,
      knot, liss,
      bri1
    );

    level_idx:= bestiaire'First;
    bri_idx:= bestiaire'Last;
    beast:= bestiaire'First;

    --  -- Not necessary, just for testing new objects
    --  for b in bestiaire'Range loop
    --    Check_object(bestiaire(b).all);
    --  end loop;

  end Create_objects;

  procedure Dump_objects is
  begin
    for i in bestiaire'Range loop
      if i = level_idx then
        for j in level_stuff'Range loop
          G3D.IO.Save_file(level_stuff(j).all);
        end loop;
      else
        G3D.IO.Save_file(bestiaire(i).all);
      end if;
    end loop;
    G3D.IO.Save_file(bri2.all);
    G3D.IO.Save_file("Delta4g1", level_BSP);
  end Dump_objects;

  detect_collisions: Boolean:= True;

  procedure Display_scene(
    o: in out G3D.Object_3D'Class;
    gc: Game_control.Command_set;
    sec: G3D.Real;
    technical_infos: Boolean
  )
  is
    procedure Msg(line: GL.Int; s: String) is
    begin
      GLUT_2D.Text_output(
        0,line,main_size_x, main_size_y, s, GLUT_2D.Helvetica_10
      );
    end Msg;
    use GL, G3D, G3D.REF, G3DM;
    light_info: String(1..8);
  begin
    Clear( DEPTH_BUFFER_BIT );
    Disable( LIGHTING );
    Enable( DEPTH_TEST );
    MatrixMode( MODELVIEW );
    Set_GL_Matrix(ego.world_rotation);
    Stars.Display(ego.world_rotation);
    Enable( LIGHTING );
    Enable( CULL_FACE );
    CullFace( BACK );

    GL.Translate ( - ego.clipper.eye_position );

    ------------------------
    -- Display the object --
    ------------------------
    PushMatrix;
    G3D.Display( o, ego.clipper );
    PopMatrix;

    if technical_infos then
      PushMatrix;

      Disable( LIGHTING );
      Disable( TEXTURE_2D );

      Color( red   => 0.7,
             green => 0.7,
             blue  => 0.6);

      GLUT_2D.Text_output( (0.0,0.0,0.0),"O", GLUT_2D.Times_Roman_24 );
      GLUT_2D.Text_output( (1.0,0.0,0.0),"x", GLUT_2D.Times_Roman_24 );
      GLUT_2D.Text_output( (0.0,1.0,0.0),"y", GLUT_2D.Times_Roman_24 );
      GLUT_2D.Text_output( (0.0,0.0,1.0),"z", GLUT_2D.Times_Roman_24 );

      Msg(10, "Name (Space key for next object or scene): " & Get_name(o) &
        " # of points" & Integer'Image(o.max_points) &
        " # of faces"  & Integer'Image(o.max_faces));
      Msg(20, "Run mode (Shift): " &
        Boolean'Image(gc( Game_control.run_mode )));
      Msg(30, "Slide mode (Alt): " &
        Boolean'Image(gc( Game_control.slide_mode )));
      Msg(40, "Ctrl mode: "  &
        Boolean'Image(gc( Game_control.ctrl_mode )));

      Msg(50, "Eye: " & Coords(ego.clipper.eye_position) & " reset: 0" );
      Msg(60, "View direction: " & Coords(ego.clipper.view_direction));

      for i in light_info'Range loop
        if Is_light_switched(i) then
          light_info(i):= Character'Val(Character'Pos('0')+i);
        else
          light_info(i):= 'x';
        end if;
      end loop;
      Msg(70, "Lights: [" & light_info & ']');
      Msg(80,
        "Objects seen:" & Natural'Image(info_b_ntl2) &
        "; max portal depth:" & Natural'Image(info_b_ntl3));
      if beast = level_idx then
        Msg(90, "BSP depth: " & Natural'Image(info_b_ntl1) &
          ". Area found: " & Boolean'Image(info_b_bool1) &
          ". BSP path: " & To_String(info_b_str1));
      end if;
      Msg(100, "Collision detection (F10): " & Boolean'Image(detect_collisions));

      if sec > 0.0 then
        Msg(140, "FPS: " & Integer'Image(Integer(1.0/sec)));
      end if;

      PopMatrix;
    end if; -- technical_infos
  end Display_scene;

  -- Timer management
  last_time: Integer;
  sample: array(1..123) of Natural:= (others =>0);
  average: G3D.Real; -- avg milliseconds
  new_scene: Boolean:= True;

  gc: Game_control.Command_set:= Game_control.no_command;

  technical_infos_enabled: Boolean:= True;

  procedure Graphic_display is
    use GL;
  begin
    Display_scene(
      bestiaire(beast).all, gc,
      average*0.001,
      technical_infos_enabled
    );
  end Graphic_display;

  package SAA is new GLOBE_3D.Software_Anti_Aliasing(Graphic_display);

  type Smoothing_method is ( none, software, hardware );
  -- hardware doesn't work (some code must be missing) and produces
  -- a dotted display on Vista.

  smoothing: constant Smoothing_method:= none;

  procedure Fill_screen is
    use GL;
  begin
    case smoothing is
      when software =>
        SAA.Set_Quality(SAA.Q3);
        for SAA_Phase in 1..SAA.Anti_Alias_phases loop
          SAA.Display_with_Anti_Aliasing(SAA_Phase);
        end loop;
      when hardware =>
        Enable( MULTISAMPLE_ARB ); -- (if not done yet)
        Clear( COLOR_BUFFER_BIT );
        Graphic_display;
        Flush;
      when none =>
        Clear( COLOR_BUFFER_BIT );
        Graphic_display;
        Flush;
    end case;
    GLUT.SwapBuffers;
  end Fill_screen;

  mem_rot: G3D.Matrix_33:= G3D.Id_33;

  procedure Reset_eye is
  begin
    ego.clipper.eye_position:= ( 0.0, 0.0, 4.0 );
    ego.rotation:= ( 0.0, 0.0, 0.0 );
  end Reset_eye;

  screenshot_count: Natural:= 0;

  video_count: Natural:= 0;

  video_rate: constant:= 20;
  video_declared_rate: constant:= 30;
  seconds_video: Long_Float; -- seconds since last captured image
  trigger_video: constant Long_Float:= 1.0 / Long_Float(video_rate);

  object_rotation_speed: G3D.Vector_3D:= ( 0.0, 0.0, 0.0 );

  procedure Main_operations is

    use GL, G3D, G3DM, G3D.REF, G3D.BSP, Game_control;

    procedure My_Limiting(step: in out GLOBE_3D.Vector_3D) is
      use G3D.Collision_detection;
      radius: constant:= 4.0;
      reacted: Real; -- unused further
    begin
      if detect_collisions then
        Reaction(
          bestiaire(beast).all,
          (ego.clipper.eye_position, radius),
          slide,
          step,
          reacted
        );
      end if;
    end My_Limiting;

    procedure My_Limited_Translation is
    new Actors.Limited_Translation(My_Limiting);

    function Can_be_rotated return Boolean is
    begin
      return beast /= bri_idx and beast /= level_idx;
    end Can_be_rotated;

    elaps, time_now: Integer;
    gx,gy: GL.Double;   -- mouse movement since last call
    seconds: GL.Double; -- seconds since last image
    alpha_correct: Boolean;
    attenu_t, attenu_r: Real;
  begin
    -- Number of milliseconds since GLUT.Init
    time_now := GLUT.Get( GLUT.ELAPSED_TIME );

    if new_scene then
      new_scene:= False;
      elaps:= 0;
    else
      elaps:= time_now - last_time;
    end if;
    last_time := time_now;
    average:= 0.0;
    for i in reverse sample'First+1..sample'Last loop
      sample(i):= sample(i-1);
      average:= average + Real(sample(i));
    end loop;
    sample(sample'First):= elaps;
    average:= average + Real(elaps);
    average:= average / Real(sample'Length);

    seconds:= Real(elaps) * 0.001;
    attenu_t:= Real'Min(0.975, Real'Max( 0.40, 1.0 - seconds*3.0) );
    attenu_r:= attenu_t ** 0.75;

    gc:= no_command;

    Game_control.Append_commands(
      size_x     => Integer(main_size_x),
      size_y     => Integer(main_size_y),
      warp_mouse => full_screen,
      c          => gc,
      gx         => gx,
      gy         => gy
    );

    if forget_mouse > 0 then -- mouse coords disturbed by resize
      gx:= 0.0;
      gy:= 0.0;
      forget_mouse:= forget_mouse - 1;
    end if;

    if gc( interrupt_game ) then
      Say_bye_to_GLUT;
    end if;

    technical_infos_enabled:=
      not (gc( photo ) or capturing_video);

    if gc( jump ) then
      if Can_be_rotated then
        mem_rot:= bestiaire(beast).rotation;
      end if;
      beast:= beast+1; -- Next object, please !
      if beast > bestiaire'Last then
        beast:= bestiaire'First;
      end if;
      if Can_be_rotated then
        bestiaire(beast).rotation:= mem_rot;
      end if;
      Reset_eye;
    end if;

    alpha_correct:= False;
    if gc( special_plus )  then alpha:= alpha + seconds; alpha_correct:= True; end if;
    if gc( special_minus ) then alpha:= alpha - seconds; alpha_correct:= True; end if;
    if alpha_correct then
      if alpha < 0.0 then alpha:= 0.0;
      elsif alpha > 1.0 then alpha:= 1.0; end if;
      for f in bestiaire(beast).face'Range loop
        bestiaire(beast).face(f).alpha:= alpha;
      end loop;
    end if;

    if gc(toggle_10) then
      detect_collisions:= not detect_collisions;
    end if;

    -------------------------------------
    -- Rotating they eye or the object --
    -------------------------------------
    if gc( ctrl_mode ) then
      if Can_be_rotated then
        Actors.Abstract_rotation(
          gc => gc,
          gx => gx,
          gy => gy,
          unitary_change => seconds,
          deceleration   => attenu_r,
          matrix         => bestiaire(beast).rotation,
          time_step      => seconds,
          rotation_speed => object_rotation_speed
        );
      end if;
    else
      Actors.Rotation(
        actor => ego,
        gc => gc,
        gx => gx,
        gy => gy,
        unitary_change => seconds,
        deceleration   => attenu_r,
        time_step      => seconds
      );
    end if;

    --------------------
    -- Moving the eye --
    --------------------
    My_Limited_Translation(
      actor => ego,
      gc => gc,
      gx => gx,
      gy => gy,
      unitary_change => seconds,
      deceleration   => attenu_t,
      time_step      => seconds
    );

    if beast = bri_idx then
      -- The cheapest Binary Space Partition ever !...
      if ego.clipper.eye_position(2) < -50.0 then
        bestiaire(bri_idx):= bri2;
      else
        bestiaire(bri_idx):= bri1;
      end if;
    elsif beast = level_idx and level_BSP /= null then
      declare
        area: p_Object_3D;
      begin
        G3D.BSP.Locate(ego.clipper.eye_position, level_BSP, area);
        if area = null then
          null; -- not found, we keep the previous one
        else
          bestiaire(level_idx):= area;
        end if;
      end;
    end if;

    Ego.clipper.view_direction:= Transpose(Ego.world_rotation) * (0.0,0.0,-1.0);

    frontal_light.position:= (
      GL.Float(ego.clipper.eye_position(0)),
      GL.Float(ego.clipper.eye_position(1)),
      GL.Float(ego.clipper.eye_position(2)),1.0);
    G3D.Define( 1, frontal_light);

    if gc( n0 ) then
      Reset_eye;
    end if;

    -------------------------
    -- Control of lighting --
    -------------------------

    for c in n1..n8 loop
      if gc( c ) then
        Reverse_light_switch(1 + Command'Pos(c) - Command'Pos(n1));
      end if;
    end loop;

    ------------------------
    -- Display everything --
    ------------------------

    Fill_screen;

    --------------------------------
    -- Screenshot / Video capture --
    --------------------------------

    if gc( photo ) then
      screenshot_count:= screenshot_count + 1;
      declare
        n: constant String:= Integer'Image(1_0000 + screenshot_count);
      begin
        GL.IO.Screenshot("shot" & n(n'Last-3..n'Last) & ".bmp");
      end;
    end if;

    if gc( video ) then -- start / stop capture
      if capturing_video then
        GL.IO.Stop_capture;
      else
        video_count:= video_count + 1;
        declare
          n: constant String:= Integer'Image(1_0000 + video_count);
        begin
          GL.IO.Start_capture(
            "capture" & n(n'Last-3..n'Last) & ".avi",
            video_declared_rate
          );
        end;
        seconds_video:= 0.0;
      end if;
      capturing_video:= not capturing_video;
    end if;

    if capturing_video then
      if seconds_video > trigger_video then
        seconds_video := seconds_video - trigger_video;
        GL.IO.Capture_frame;
      end if;
      seconds_video:= seconds_video + Long_Float(seconds);
    end if;

  end Main_Operations;

  -- Procedures passed to GLUT here: Window_Resize, Menu, Main_Operations
  -- GLUT.Devices handles: Keyboard, Motion, Mouse

  procedure Start_GLUTs is
    use GL,GLUT;
    GLUT_options: GLUT.Unsigned:= GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH;
  begin
    Init;
    if smoothing = hardware then
      GLUT_options:= GLUT_options or GLUT.MULTISAMPLE;
    end if;
    InitDisplayMode( GLUT_options );
    main_size_x:= 500;
    main_size_y:= 360;
    --  -- Mini Web video:
    --  main_size_x:= 200;
    --  main_size_y:= 128;
    InitWindowSize(Integer(main_size_x), Integer(main_size_y));
    InitWindowPosition(120, 120);
    if CreateWindow(
      "GLOBE_3D / Demo_1 / Extra Debug = " &
      Boolean'Image(G3D.Options.Is_debug_mode)
    ) = 0 then
      raise GLUT_Problem;
    end if;
    ReshapeFunc(      Window_Resize'Address        );
    DisplayFunc(      Main_operations'Address      );
    IdleFunc(         Main_operations'Address      );
    GLUT.Devices.Initialize;

    if CreateMenu( Menu'Address ) = 0 then
      raise GLUT_Problem;
    end if;
    AttachMenu( MIDDLE_BUTTON );
    AddMenuEntry(" * Full Screen", 1);
    AddMenuEntry("--> Exit (Esc)", 2);
  end Start_GLUTs;

  procedure Start_GLs is
    use GL;
    fog_colour: GL.Light_Float_vector:= (0.2,0.2,0.2,0.1);
  begin
    Clear_modes;
    Prepare_demo_lighting(0.9);
    if foggy then
      Enable( FOG );
      Fogfv(FOG_COLOR, fog_colour(0)'Unchecked_Access);
      Fogf(FOG_DENSITY, 0.02);
    end if;
    Reset_for_3D( Integer(main_size_x), Integer(main_size_y ));
    if smoothing = hardware then
      Enable( MULTISAMPLE_ARB );
      Enable( SAMPLE_COVERAGE_ARB ); -- Hope it helps switching on the AA...
    end if;
  end Start_GLs;

  -- Get eventual command line arguments.

  type Switch_Type is (
    load, -- load some scenes from .g3d files stored in the GLOBE_3D
          --        resource files, instead of rebuilding them (default)
          --   "-load=mylevel" sets "mylevel.zip" as level resource;
          --   from that resource, loads mylevel_$_area#.g3d with #=1,2,3...
          --   and loads mylevel.bsp.
    dump  -- dump all objects of the demo to .g3d files
  );

  switch: array(Switch_Type) of Boolean:= (others => False);
  custom: Unbounded_String;

  procedure Get_arguments is
    use Ada.Command_Line;
  begin
    for s in Switch_Type loop
      for a in 1..Argument_Count loop
        declare
          arg_long: constant String:= Argument(a);
          swi     : constant String:= Switch_Type'Image(s);
          arg     : constant String:= To_Upper(arg_long(arg_long'First..swi'Last+1));
        begin
          if arg = '-' & swi or arg = '/' & swi then
            switch(s):= True;
            if s = load then
              custom:= To_Unbounded_String(arg_long(swi'Last+3..arg_long'Last));
            end if;
          end if;
        end;
      end loop;
    end loop;
  end Get_arguments;

begin
  Get_arguments;
  G3D.Set_global_data_name("G3Demo_Global_Resources.zip");
  G3D.Set_level_data_name("G3Demo_Level_Resources.zip");
  --
  G3D.Textures.Register_textures_from_resources;

  Create_objects(switch(load), To_String(custom));
  if switch(dump) then
    Dump_objects; -- even those that were loaded (entropy check)
  end if;

  Start_GLUTs;    -- Initialize the GLUT things
  Start_GLs;      -- Initialize the (Open)GL things
  Reset_eye;

  G3D.Textures.Check_all_textures; -- Preload the textures

  GLUT.MainLoop;  -- Let's rock !

end GLOBE_3D_Demo;
