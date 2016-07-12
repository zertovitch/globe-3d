------------------------------------------------------------------------------
--  File:            Mini.adb
--  Description:     A minimal application / stub for GLOBE_3D
--  Copyright (c) Gautier de Montmollin 2008
------------------------------------------------------------------------------

with GL, GL.Math, GLU, GLUT.Devices, GLUT_2D;

with GLOBE_3D,
     GLOBE_3D.Math,
     GLOBE_3D.Textures;

procedure Mini is

  package G3D  renames GLOBE_3D;

  GLUT_Problem: exception;

  main_size_x, main_size_y: GL.Sizei;

  frontal_light: G3D.Light_definition;

  procedure Prepare_demo_lighting(fact: GL.Float) is
    use GL, G3D;
    proto_light: Light_definition:=
      (position => (10.0, 4.0, 10.0, 1.0),
       ambient  => (0.1, 0.1, 0.1, fact),
       diffuse  => (1.0, 0.8, 0.8, fact),
       specular => (0.8, 0.8, 1.0, fact));
  begin
    Enable( LIGHTING );
    G3D.Define( 1, proto_light);
    frontal_light:= proto_light;
    proto_light.diffuse:= (0.5, 0.9, 0.5, fact);
    G3D.Define( 2, proto_light);
    G3D.Switch_light(1,True);
    G3D.Switch_light(2,True);
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

  ego: G3D.Camera;
  deg2rad: constant:= 3.1415926535897932 / 180.0;

  procedure Set_Background_Color is
    use GL;
    fact: constant:= 0.4;
  begin
    ClearColor( fact * 0.2275, fact * 0.0745, fact * 0.4431, 0.0 );  --  Dark violet
  end Set_Background_Color;

  procedure Reset_for_3D( width, height: Integer ) is
    use GL, G3D, G3D.REF;
    aspect, half_fov_max_rads, fovy: Real;
  begin
    Viewport(0, 0, Sizei(width), Sizei(height));
    MatrixMode( PROJECTION );
    LoadIdentity;
    aspect:= GL.Double(width) / GL.Double(height);
    fovy:= ego.FoVy;
    half_fov_max_rads:= 0.5 * fovy * deg2rad;
    if aspect > 1.0 then -- x side angle broader than y side angle
      half_fov_max_rads:= Arctan(aspect * Tan(half_fov_max_rads));
    end if;
    ego.clipper.max_dot_product:= Sin(half_fov_max_rads);
    ego.clipper.main_clipping:= (0,0, width-1, height-1);
    GLU.Perspective(
      fovy   => fovy,
      -- field of view angle (deg) in the y direction
      aspect => aspect,
      -- x/y aspect ratio
      zNear  => 1.0,
      -- distance from the viewer to the near clipping plane
      zFar   => fairly_Far
      -- distance from the viewer to the far clipping plane
    );
    -- The matrix generated by GLU.Perspective is
    -- multipled by the current matrix
    MatrixMode( MODELVIEW );
    ShadeModel( SMOOTH ); -- GL's default is SMOOTH, vs FLAT
    Set_Background_Color;
    -- ^ Specifies clear values for color buffer(s)
    ClearAccum(0.0, 0.0, 0.0, 0.0);
    -- ^ Specifies clear values for the accumulation buffer
  end Reset_for_3D;

  procedure Window_Resize( width, height: Integer ) is
  begin
    main_size_x:= GL.Sizei(width);
    main_size_y:= GL.Sizei(height);
    Reset_for_3D( Integer(main_size_x), Integer(main_size_y ));
  end Window_Resize;

  procedure Menu( value: Integer ) is
  begin
    case value is
      when 1 => -- GLUT.GameModeString (Full_Screen_Mode);
                GLUT.FullScreen;
                -- res := GLUT.EnterGameMode;
                GLUT.SetCursor(GLUT.CURSOR_NONE);
      when 2 => GLUT.LeaveMainLoop;
      when others => null;
    end case;
  end Menu;

  cube : G3D.Object_3D( Max_points=> 8, Max_faces=> 6 );

  procedure Create_objects is
    t: constant:= 1.0;
    use GL, G3D, G3D.Textures;

    function Basic_cube_face(
      P       : G3D.Index_array;
      tex_name: String;
      colour  : GL.RGB_Color;
      repeat  : Positive)
    return Face_type
    is
      f: Face_type; -- takes defaults values
    begin
      f.P            := P;
      f.skin         := coloured_texture;
      f.texture      := Texture_ID(tex_name);
      f.specular_map := Texture_ID("face_specular");
      f.colour       := colour;
      f.alpha        := 1.0;
      f.repeat_U     := repeat;
      f.repeat_V     := repeat;
      return f;
    end Basic_cube_face;

  begin
    cube.centre:= (0.0,0.0,0.0);
    cube.point:=
      ( (-t,-t,-t), (-t, t,-t), ( t, t,-t), ( t,-t,-t),
        (-t,-t, t), (-t, t, t), ( t, t, t), ( t,-t, t));
    cube.face:=
      ( Basic_cube_face((3,2,6,7),"face1",(1.0,0.0,0.0),1),
        Basic_cube_face((4,3,7,8),"face2",(0.0,1.0,0.0),2),
        Basic_cube_face((8,7,6,5),"face3",(0.0,0.0,1.0),3),
        Basic_cube_face((1,4,8,5),"face4",(1.0,1.0,0.0),4),
        Basic_cube_face((2,1,5,6),"face5",(0.0,1.0,1.0),5),
        Basic_cube_face((3,4,1,2),"face6",(1.0,0.0,1.0),6));
    Set_name(cube, "Trust the Cube !");
  end Create_objects;

  procedure Title is
    use GL;
    logo: G3D.Image_ID;
    f: constant:= 2;
  begin
    PushMatrix;
    Disable( LIGHTING );
    Color(1.0,1.0,1.0);
    Enable( TEXTURE_2D );
    Enable(BLEND);
    BlendFunc(sfactor => SRC_ALPHA, dfactor => ONE_MINUS_SRC_ALPHA);
    logo:= G3D.Textures.Texture_ID("g3d_logo");
    G3D.Textures.Check_2D_texture(logo);
    GLUT_2D.Put_Image(
      G3D.Image_ID'Pos(logo)+1,
      0, Int'Max(0,Int(main_size_y)-128/f), 512/f, 128/f, main_size_x, main_size_y
    );
    PopMatrix;
  end Title;

  procedure Display_scene(
    o: in out G3D.Object_3D'Class
  )
  is
    use GL, G3D, G3D.REF, GLOBE_3D.Math, GL.Math;
  begin
    Clear( DEPTH_BUFFER_BIT );
    Disable( LIGHTING );
    Enable( DEPTH_TEST );
    --  Depth comparison function set to LEQUAL is needed for multitexturing:
    --  LESS (the default) prevents showing another texture onto the first one.
    DepthFunc( LEQUAL );
    MatrixMode( MODELVIEW );
    Set_GL_Matrix( ego.world_rotation );
    Enable( LIGHTING );
    Enable( CULL_FACE );
    CullFace( BACK );
    GL.Translate( - ego.clipper.eye_position );
    ------------------------
    -- Display the object --
    ------------------------
    PushMatrix;
    G3D.Display( o, ego.clipper );
    PopMatrix;
    Title;
  end Display_scene;

  -- Timer management
  last_time: Integer;
  new_scene: Boolean:= True;

  procedure Fill_screen is
    use GL;
  begin
    Clear( COLOR_BUFFER_BIT ); -- Clear the off-screen buffer
    Display_scene(cube);       -- Display the scene on the buffer
    GLUT.SwapBuffers;          -- Make the newly drawn buffer visible
  end Fill_screen;

  procedure Reset_eye is
  begin
    ego.clipper.eye_position:= ( 0.0, 0.0, 4.0 );
    ego.world_rotation:= G3D.Id_33;
  end Reset_eye;

  Xrot,Yrot,Zrot : Integer:= 0; -- object rotation
  totrot: G3D.Real:= 0.0;

  procedure Main_Operations is
    use GL, G3D, G3D.REF, GLOBE_3D.Math;
    rot_speed: constant:= 400.0;
    elaps, time_now: Integer;
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

    -------------------------
    -- Rotating the object --
    -------------------------

    totrot:= totrot + Real(elaps) * 0.001 * rot_speed;
    while totrot > 36000.0 loop totrot:= totrot - 36000.0; end loop;
    Xrot  := Integer(totrot *  9.0) mod 36000;
    Yrot  := Integer(totrot * 21.0) mod 36000;
    Zrot  := Integer(totrot *  7.0) mod 36000;
    cube.rotation:=
       XYZ_rotation( Real(Xrot)*deg2rad*0.01,
                     Real(Yrot)*deg2rad*0.01,
                     Real(Zrot)*deg2rad*0.01 );

    ego.clipper.view_direction:= Transpose(ego.world_rotation) * (0.0,0.0,-1.0);

    frontal_light.position:= (
      GL.Float(ego.clipper.eye_position(0)),
      GL.Float(ego.clipper.eye_position(1)),
      GL.Float(ego.clipper.eye_position(2)),1.0);
    G3D.Define( 1, frontal_light);

    ------------------------
    -- Display everything --
    ------------------------

    Fill_screen;

    if GLUT.Devices.Strike_once( Character'Val(27) ) then
      GLUT.LeaveMainLoop;
    end if;

  end Main_Operations;

  -- Procedures passed to GLUT here: Window_Resize, Menu, Main_Operations
  -- GLUT.Devices handles: Keyboard, Motion, Mouse

  procedure Start_GLUTs is
    use GL, GLUT;
  begin
    Init;
    InitDisplayMode( GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH );
    main_size_x:= 520;
    main_size_y:= 420;
    InitWindowSize(Integer(main_size_x), Integer(main_size_y));
    InitWindowPosition(120, 120);
    if CreateWindow( "GLOBE_3D / Mini demo" ) = 0 then
      raise GLUT_Problem;
    end if;
    ReshapeFunc(      Window_Resize'Address        );
    DisplayFunc(      Main_Operations'Address      );
    IdleFunc(         Main_Operations'Address      );
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
  begin
    Clear_modes;
    Prepare_demo_lighting(0.9);
    Reset_for_3D( Integer(main_size_x), Integer(main_size_y ));
  end Start_GLs;

begin
  G3D.Set_global_data_name("g3demo_global_resources.zip");
  --
  G3D.Textures.Register_textures_from_resources;

  Create_objects;

  Start_GLUTs;    -- Initialize the GLUT things
  Start_GLs;      -- Initialize the (Open)GL things
  Reset_eye;

  G3D.Textures.Check_all_textures; -- Preload the textures

  --  So far, there is an issue with ObjectAda Win32, GLUT.MainLoop callback,
  --  freeglut, under Windows 7 x64. Display @ Main_Operations is fine.
  --
  GLUT.MainLoop;  -- Let's rock !

end Mini;
