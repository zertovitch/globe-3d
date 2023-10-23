------------------------------------------------------------------------------
--  File:            GLOBE_3D_Demo.adb
--  Description:     A small demo for GLOBE_3D
--  Copyright (c) Gautier de Montmollin 2002 .. 2023
------------------------------------------------------------------------------

with GL,
     GL.IO,
     GL.Materials,
     GL.Math;

with GLOBE_3D,
     GLOBE_3D.IO,
     GLOBE_3D.BSP,
     GLOBE_3D.Options,
     GLOBE_3D.Math,
     GLOBE_3D.Textures,
     GLOBE_3D.Software_Anti_Aliasing,
     GLOBE_3D.Stars_Sky,
     GLOBE_3D.Collision_Detection,
     GLOBE_3D.Aux;

with GLU, GLUT.Devices, GLUT_2D;

with Actors, Game_Control;

---------------
-- 3D models --
---------------

with Vehic001, Vehic002,
     X29,
     Brick, Icosahedron,
     SkotKnot, Lissajous,
     Knot_10_102, Knot_link,
     Planet,
     A319,
     Dreadnought,
     --  VRML_scene,
     --  gmax_scene,
     --  Doom3_Level,
     Extruded_Surface,
     Sierpinski
     ;

with Ada.Numerics;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

procedure GLOBE_3D_Demo is

  --  pragma Linker_options("-mwindows");  --  Suppress console window on Windows

  package G3D  renames GLOBE_3D;
  package G3DM renames G3D.Math;

  fairly_far : constant := 10_000.0;

  package Stars is new GLOBE_3D.Stars_sky
    (num_stars => 5_000,
     far_side  => fairly_far);

  GLUT_Problem : exception;

  main_size_x, main_size_y : GL.Sizei;

  foggy : constant Boolean := True;

  frontal_light : G3D.Light_definition;

  procedure Prepare_demo_lighting (fact : GL.Float) is
    use G3D;
    use type GL.Float;
    proto_light : Light_definition :=
      (position => (3.0, 4.0, 10.0, 1.0),
       ambient  => (0.2, 0.2, 0.2, fact),
       diffuse  => (0.9, 0.9, 0.9, fact),  -- +/- a bulb
       specular => (0.8, 0.8, 0.8, fact)); -- +/- a flashlight
  begin
    GL.Enable (GL.Lighting);
    G3D.Define (1, proto_light);
    frontal_light := proto_light;
    proto_light.diffuse := (0.5, 0.9, 0.5, fact);
    G3D.Define (2, proto_light);
    proto_light.position := (10.0, 0.0, 0.0,  1.0);
    proto_light.diffuse  :=  (0.2, 0.0, 0.9, fact);
    proto_light.specular :=  (1.0, 1.0, 1.0, fact);
    G3D.Define (3, proto_light);
    proto_light.position := (-3.0, 4.0, 10.0, 1.0);
    G3D.Define (4, proto_light);
    proto_light.position := (3.0, -4.0, 10.0, 1.0);
    proto_light.ambient  := (0.6, 0.6, 0.6, 0.1);
    G3D.Define (5, proto_light);
    proto_light.ambient := (0.6, 0.0, 0.0, 0.1);
    G3D.Define (6, proto_light);
    proto_light.ambient := (0.0, 0.6, 0.0, 0.1);
    G3D.Define (7, proto_light);
    proto_light.ambient := (0.0, 0.0, 0.6, 0.1);
    G3D.Define (8, proto_light);
    G3D.Switch_Lights (True);
    G3D.Switch_Light (4, False);
    G3D.Switch_Light (5, False);
    G3D.Switch_Light (6, False);
    G3D.Switch_Light (7, False);
    G3D.Switch_Light (8, False);
  end Prepare_demo_lighting;

  procedure Clear_modes is
  begin
    GL.Disable (GL.Blend);
    GL.Disable (GL.Lighting);
    GL.Disable (GL.Auto_Normal);
    GL.Disable (GL.Normalize);
    GL.Disable (GL.Depth_Test);
  end Clear_modes;

  deg2rad : constant := Ada.Numerics.Pi / 180.0;
  ego : G3D.Camera;

  procedure Set_Background_Color is
    fact : constant := 0.4;
    use type GL.Clampf;
  begin
    GL.ClearColor (fact * 0.2275, fact * 0.0745, fact * 0.4431, 0.0);  --  Dark violet
  end Set_Background_Color;

  procedure Reset_for_3D (width, height : Integer) is
    use G3D, G3D.REF;
    use type GL.Double;
    aspect, half_fov_max_rads, fovy : Real;
  begin
    GL.Viewport (0, 0, GL.Sizei (width), GL.Sizei (height));
    GL.MatrixMode (GL.PROJECTION);
    GL.LoadIdentity;
    aspect := GL.Double (width) / GL.Double (height);
    fovy := ego.FoVy;
    half_fov_max_rads := 0.5 * fovy * deg2rad;
    if aspect > 1.0 then  --  x side angle broader than y side angle
      half_fov_max_rads := Arctan (aspect * Tan (half_fov_max_rads));
    end if;
    ego.clipper.max_dot_product := Sin (half_fov_max_rads);
    ego.clipper.main_clipping   := (0, 0, width - 1, height - 1);
    GLU.Perspective
      (fovy   => fovy,
       --  field of view angle (deg) in the y direction
       aspect => aspect,
       --  x/y aspect ratio
       zNear  => 1.0,
       --  distance from the viewer to the near clipping plane
       zFar   => fairly_far);
       --  distance from the viewer to the far clipping plane

    --  The matrix generated by GLU.Perspective is
    --  multipled by the current matrix
    GL.MatrixMode (GL.MODELVIEW);
    GL.ShadeModel (GL.SMOOTH);  --  GL's default is SMOOTH, vs FLAT
    Set_Background_Color;
    --  Specify clear values for the accumulation buffer
    GL.ClearAccum (0.0, 0.0, 0.0, 0.0);
  end Reset_for_3D;

  forget_mouse : Natural := 0;
  capturing_video : Boolean := False;

  procedure Window_Resize (width, height : Integer) is
  begin
    if capturing_video then
      GL.IO.Stop_capture;
    end if;
    main_size_x := GL.Sizei (width);
    main_size_y := GL.Sizei (height);
    forget_mouse := 5;
    Reset_for_3D (Integer (main_size_x), Integer (main_size_y));
  end Window_Resize;

  full_screen : Boolean := False;

  procedure Say_bye_to_GLUT is
  begin
    if capturing_video then
      GL.IO.Stop_capture;
    end if;
    GLUT.LeaveMainLoop;
  end Say_bye_to_GLUT;

  procedure Menu (value : Integer) is
    --  GameModeString("800x600:16@60") ;
    --  if GameModeGet(GLUT_GAME_MODE_WIDTH) /= -1 then -- if the width is different to -1
    --     EnterGameMode;                               -- enter full screen mode
    --  else                                            -- print out that this mode is not available
    --     printf("The current mode is not supported in this system\n") ;

    --  res: Integer;
    --  Full_Screen_Mode   : constant String:= "640x480:16@60";
    --  Full_Screen_Mode_2 : constant String:= "400x300:16@60";
  begin
    case value is
      when 1 => -- GLUT.GameModeString (Full_Screen_Mode);
                GLUT.FullScreen;
                --  res := GLUT.EnterGameMode;
                GLUT.SetCursor (GLUT.CURSOR_NONE);
                forget_mouse := 10;
                full_screen  := True;
      when 2 => Say_bye_to_GLUT;
      when others => null;
    end case;
  end Menu;

  alpha : GL.Double := 1.0;

  bri1, bri2,
  ico, icos,
  --  vrml,
  --  gmax,
  knot, liss,
  knot_10_102_obj, knot_link_obj,
  globe,
  a319_plane,
  x29_plane,
  vhc_001, vhc_002,
  dreadnought_ship,
  extrude_test_1, borg_star,
  sierp,
  cube, cube_glossy, cube_tri, cube_tri_quad, cube_bico : G3D.p_Object_3D;

  bestiaire, level_stuff : G3D.p_Object_3D_Array := null;
  level_idx, bri_idx, beast_idx : Integer;
  level_BSP : G3D.BSP.p_BSP_Node := null;

  procedure Create_Objects (load : Boolean; doom3_custom : String) is

    level_map : G3D.Map_of_Visuals := G3D.empty_map; -- dictionary

    procedure Load_Doom (name : String) is
      area_max : Natural := 0;
      ls : G3D.Object_3D_Array (1 .. 1_000);
      so : G3D.p_Object_3D;
      empty_level : exception;
      use G3D.Ident_Vectors;
      cv : Cursor;
      id : G3D.Ident;
    begin
      Area_loop : for i in ls'Range loop
        begin
          G3D.IO.Load
            (name & "_$_area" & Trim (Integer'Image (i - 1), Left), ls (i));
        exception
          when G3D.Missing_object =>
            exit Area_loop;  --  Previous area was the last one, exit.
        end;
        area_max := i;
        G3D.Add (level_map, G3D.p_Visual (ls (i)));  --  add area to dictionary
        --
        --  Load sub-objects for area #i.
        --
        --  for id of ls(i).sub_obj_ids loop  --  Ada 2012 shortcut notation
        cv := ls (i).sub_obj_ids.First;
        while Has_Element (cv) loop
          id := Element (cv);
          G3D.IO.Load (id, so);
          ls (i).sub_objects :=
            new G3D.Object_3D_List'
              (objc => so,
               next => ls (i).sub_objects);
          G3D.Add (level_map, G3D.p_Visual (so));  --  add sub-object to dictionary
          Next (cv);
        end loop;
      end loop Area_loop;
      begin
        G3D.IO.Load (name, level_map, level_BSP);  --  load BSP tree
      exception
        when G3D.Missing_object =>
          null; -- Some custom levels like the Reims Cathedral have no BSP
      end;
      if area_max = 0 then
        --  Perhaps just one object to display
        --  In this case we have only the name, no area counter
        --  E.g. a319.g3d inside of a319.zip
        begin
          G3D.IO.Load (name, ls (1));
          area_max := 1;
          G3D.Add (level_map, G3D.p_Visual (ls (1)));  --  add to dictionary
        exception
          when G3D.Missing_object =>
            raise empty_level with "Object name " & name & " not found";
        end;
      end if;
      level_stuff := new G3D.Object_3D_Array'(ls (1 .. area_max));
    end Load_Doom;

    t : constant := 20.0;
    f2 : Natural;
    use GL.Materials, G3D, G3D.Textures;

    function Basic_Cube_Face
      (P        : G3D.Index_Array;
       tex_name : String;
       colour   : GL.RGB_Color;
       repeat   : Positive;
       material : Material_type := neutral_material)
    return Face_Type
    is
      f : Face_Type;  --  takes defaults values
    begin
      f.P       := P;
      f.texture := Texture_ID (tex_name);
      f.repeat_U := repeat;
      f.repeat_V := repeat;
      if material = neutral_material then
        f.colour  := colour;
        f.skin    := coloured_texture;
      else
        f.material := material;
        f.skin     := material_texture;
      end if;
      f.alpha   := alpha;
      return f;
    end Basic_Cube_Face;

    portal1, portal2 : Brick.Cubic_Face_Index;

    Shiny : constant Material_type :=
      (ambient =>        (0.24725, 0.2245, 0.0645, 1.0),
       diffuse =>        (0.34615, 0.3143, 0.0903, 1.0),
       specular =>       (1.0, 1.0, 1.0, 1.0),
       emission =>       (0.0, 0.0, 0.0, 0.0),
       shininess =>      64.0);

    use type GL.Double;

  begin
    --  Basic cube
    cube := new G3D.Object_3D (Max_points => 8, Max_faces => 6);
    cube.centre := (0.0, 0.0, -4.0 - 3.0 * t);
    cube.point :=
      ((-t, -t, -t), (-t, +t, -t), (+t, +t, -t), (+t, -t, -t),
       (-t, -t, +t), (-t, +t, +t), (+t, +t, +t), (+t, -t, +t));
    cube.face :=
       (Basic_Cube_Face ((3, 2, 6, 7), "face1", (1.0, 0.0, 0.0), 1, Polished_Gold),
        Basic_Cube_Face ((4, 3, 7, 8), "face2", (0.0, 1.0, 0.0), 2, Shiny),
        Basic_Cube_Face ((8, 7, 6, 5), "face3", (0.0, 0.0, 1.0), 3),
        Basic_Cube_Face ((1, 4, 8, 5), "face4", (1.0, 1.0, 0.0), 4),
        Basic_Cube_Face ((2, 1, 5, 6), "face5", (0.0, 1.0, 1.0), 5),
        Basic_Cube_Face ((3, 4, 1, 2), "face6", (1.0, 0.0, 1.0), 6));
    Set_Name (cube.all, "Trust the Cube !");
    --  Basic cube, but with a specular map. A glossy 'S' should appear...
    cube_glossy := new G3D.Object_3D'(cube.all);  --  cloning
    for f in cube.face'Range loop
      cube_glossy.face (f).specular_map := Texture_ID ("face_specular");
    end loop;
    Set_Name (cube_glossy.all, "Shiny cube (check the 'S' !)");
    --
    --  Basic cube, but with half-faces as triangles
    --  must look identical as the first one
    cube_tri := new G3D.Object_3D (Max_points => 8, Max_faces => 12);
    cube_tri.centre := cube.centre;
    cube_tri.point  := cube.point;
    for f in cube.face'Range loop
      f2 := (f - cube.face'First) * 2 + cube_tri.face'First;
      cube_tri.face (f2)        := cube.face (f);
      cube_tri.face (f2).P (1)  := 0;
      cube_tri.face (f2 + 1)      := cube.face (f);
      cube_tri.face (f2 + 1).P (3) := 0;
      --  Now we have killed opposite edges, one on each triangle :-)
    end loop;
    Set_Name (cube_tri.all, "Triangular Cube !");

    cube_tri_quad := new G3D.Object_3D'(GLOBE_3D.Aux.Merge_Triangles (cube_tri.all));
    Set_Name (cube_tri_quad.all, "Cube, triangles merged to squares");

    --  Also a cube with half-faces, but playing with colour/texture
    cube_bico := new G3D.Object_3D'(cube_tri.all);  --  cloning
    Set_Name (cube_bico.all, "Technicolor");
    for f in cube_bico.face'Range loop
      if f mod 2 = 0 then
        if cube_bico.face (f).skin = coloured_texture then
          cube_bico.face (f).skin := colour_only;
        else
          cube_bico.face (f).skin := material_only;
        end if;
      end if;
    end loop;

    --  Plane: Airbus A319
    --
    A319.Create
      (object => a319_plane,
       scale  => 20.0,
       centre => (0.0, 0.0, -796.0));
    --  G3D.IO.Load ("A319", a319_plane);

    --  Plane: X29 prototype
    X29.Create
      (object => x29_plane,
       scale  => 10.0,
       centre => (0.0, 0.0, -170.0));

    --  Space vehicle 1
    Vehic001.Create
      (object => vhc_001,
       scale  => 4.0,
       centre => (0.0, 0.0, -180.0));
    --  Space vehicle 2
    Vehic002.Create
      (object => vhc_002,
       scale  => 100.0,
       centre => (80.0, 0.0, -70.0),
       metal_door    => Texture_ID ("portmet1"),
       metal_surface => Texture_ID ("fdmetal1"),
       bumped_blue   => Texture_ID ("bleubosl"));

    Pre_Calculate (vhc_002.all);

    Icosahedron.Create
      (object => ico,
       scale  => 12.0,
       centre => (0.0, 0.0, -60.0),
       alpha  => 0.8,
       polyball => False);

    Icosahedron.Create
      (object => icos,
       scale  => 12.0,
       centre => ico.centre,
       alpha  => 0.5,
       polyball => True);

    for i in icos.face'Range loop
      icos.face (i).skin := material_only;
      case (i - 1) / 20 is
        --  Non-transparent things
        when  1 => icos.face (i).material := GL.Materials.Brass;
        when  2 => icos.face (i).material := GL.Materials.Bronze;
        when  3 => icos.face (i).material := GL.Materials.Copper;
        when  4 => icos.face (i).material := GL.Materials.Polished_Copper;
        when  5 => icos.face (i).material := GL.Materials.Gold;
        when  6 => icos.face (i).material := GL.Materials.Polished_Bronze;
        --  Transparent things (Nabokov!)
        when  7 => icos.face (i).material := GL.Materials.Pewter;
        when  8 => icos.face (i).material := GL.Materials.Pearl;
        when  9 => icos.face (i).material := GL.Materials.Obsidian;
        when 10 => icos.face (i).material := GL.Materials.Jade;
        when 11 => icos.face (i).material := GL.Materials.Emerald;
        when  0 => icos.face (i).material := GL.Materials.Ruby;
        when others => null;
      end case;
    end loop;

    --  Dreadnought space ship modeled with GMax
    Dreadnought.Create
      (object => dreadnought_ship,
       scale  => 0.065,
       centre => (0.0, -250.0, -700.0),
       alum_001 => Texture_ID ("alum_001"),
       alum_002 => Texture_ID ("alum_002"),
       grumnoir => Texture_ID ("grumnoir"),
       tole_001 => Texture_ID ("tole_001"));
    Set_Name (dreadnought_ship.all, "Dreadnought");
    --  G3D.IO.Load("Dreadnought", dreadnought_ship);
    Pre_Calculate (dreadnought_ship.all);

    Extruded_Surface.Create
      (object     => extrude_test_1,
       scale      => 400.0,
       centre     => (-160.0, -160.0, -300.0),
       grid       => 57,
       surface    => Extruded_Surface.square,
       max_u3     => 0.15,
       iterations => 100,
       hor_tex    => Texture_ID ("spacity1"),
       ver_tex    => Texture_ID ("spacity1"),
       tiling_hu  => 1,
       tiling_hv  => 1,
       tiling_vu  => 2,
       tiling_vv  => 2);
    Set_Name (extrude_test_1.all, "Space City");

    Extruded_Surface.Create
      (object     => borg_star,
       scale      => 500.0,
       centre     => (0.0, 0.0, -1000.0),
       grid       => 65,
       surface    => Extruded_Surface.sphere,
       max_u3     => 0.03,
       iterations => 2000,
       hor_tex    => Texture_ID ("alum_001"),
       ver_tex    => Texture_ID ("spacity1"),
       tiling_hu  => 30,  --  ~ 2 * v-tiling
       tiling_hv  => 15,
       tiling_vu  => 31,  --  should be ~ 2*pi* v-tiling
       tiling_vv  => 5);
    Set_Name (borg_star.all, "Borg Star");

    Sierpinski.Create_Cube
      (object  => sierp,
       scale   => 200.0,
       centre  => (0.0, 0.0, -300.0),
       texture => (Texture_ID ("face1"),
                   Texture_ID ("face2"),
                   Texture_ID ("face3"),
                   Texture_ID ("face4"),
                   Texture_ID ("face5"),
                   Texture_ID ("face6")),
       tiled   => False,
       fractal_level => 2);

    Planet.Create
      (object   => globe,
       scale    => 200.0,
       centre   => (0.0, 0.0, -800.0),
       mercator => Texture_ID ("earth_map"),
       parts    => 47);
    Set_Name (globe.all, "The Earth !");
    Pre_Calculate (globe.all);

    SkotKnot.Create
      (object => knot,
       scale  => 1.0,
       centre => (0.0, 0.0, -40.0));
    Pre_Calculate (knot.all);

    Lissajous.Create
      (object => liss,
       scale  => 1.0,
       centre => (0.0, 0.0, -25.0));
    Pre_Calculate (liss.all);

    Knot_10_102.Create
      (object => knot_10_102_obj,
       scale  => 1.0,
       centre => (0.0, 0.0, -50.0));
    Pre_Calculate (knot_10_102_obj.all);

    Knot_link.Create
      (object => knot_link_obj,
       scale  => 1.0,
       centre => (0.0, 0.0, -50.0));
    Pre_Calculate (knot_link_obj.all);

    --
    --  Load a Doom 3 level from .g3d/.bsp files
    --
    if doom3_custom = "" then
      Load_Doom ("Delta4g1");
    else
      G3D.Set_Level_Data_Name (doom3_custom & ".zip");
      G3D.Textures.Reset_Textures;
      G3D.Textures.Register_Textures_From_Resources;
      Load_Doom (Ada.Directories.Simple_Name (doom3_custom));
    end if;

    if load and doom3_custom = "" then
      --  We test here the loading and mutual linking
      --  of some objects dumped by the -dump option.
      --
      --  Load the space station scene:
      --
      G3D.IO.Load ("Space station brick ONE", bri1);
      G3D.IO.Load ("Space station brick TWO", bri2);
      --  Relink both bricks:
      declare
        bricks_map : constant Map_of_Visuals :=
          Map_of ((p_Visual (bri1), p_Visual (bri2)));
      begin
        G3D.Rebuild_Links (bri1.all, bricks_map, False, False, True);
        G3D.Rebuild_Links (bri2.all, bricks_map, False, False, True);
      end;
      Set_Name (bri1.all, "Space station brick ONE (loaded)");
      Set_Name (bri2.all, "Space station brick TWO (loaded)");
    else
      --  Create objects, don't load them (default).
      --
      --  Create the space station scene:
      --
      Brick.Create
        (object  => bri1,
         scale   => 100.0,
         centre  => (0.0, 0.0, 0.0),
         kind    => Brick.cube,
         opening => (5 => True, others => False),
         portal  => portal1,
         texture => (Texture_ID ("face1"),
                     Texture_ID ("face2"),
                     Texture_ID ("face3"),
                     Texture_ID ("face4"),
                     Texture_ID ("face5"),
                     Texture_ID ("face6")));
      Set_Name (bri1.all, "Space station brick ONE");

      Brick.Create
        (object  => bri2,
         scale   => 100.0,
         centre  => (0.0, 0.0, -100.0),
         kind    => Brick.cube,
         opening => (5 | 6 => True, others => False),
         portal  => portal2,
         texture => (others => Texture_ID ("alum_002")));
      Set_Name (bri2.all, "Space station brick TWO");

      --  Connecting portals:
      bri1.face (portal1 (5)).connecting := bri2;
      bri2.face (portal2 (6)).connecting := bri1;
    end if;

    --  Relink Doom 3 level (either loaded or created):
    if level_stuff /= null then
      for i in level_stuff'Range loop
        --  NB:
        --    - portals may have been already linked (if created, not loaded);
        --    - textures need to be linked
        G3D.Rebuild_Links (level_stuff (i).all, level_map, False, False, True);
        G3D.Pre_Calculate (level_stuff (i).all);
      end loop;
    end if;

    --  Whole 3D zoo:
    bestiaire := new Object_3D_Array'
      (level_stuff (level_stuff'First),  --  Starting area in the DOOM 3 level is the first area
       cube, cube_glossy, cube_tri, cube_tri_quad, cube_bico,
       globe,
       sierp,
       extrude_test_1,
       borg_star,
       dreadnought_ship,
       a319_plane,
       x29_plane,
       vhc_001, vhc_002,
       ico, icos,
       knot, liss,
       knot_10_102_obj, knot_link_obj,
       bri1);

    --  Indices in the 3D zoo area where object accesses may change depending where the camera is:
    level_idx := bestiaire'First;  --  This is the index of the DOOM 3 level
    bri_idx   := bestiaire'Last;   --  This is the index of the pair of cubes

    --  We start with the first object:
    beast_idx := bestiaire'First;

    --  -- Not necessary, just for testing new objects
    --  for b in bestiaire'Range loop
    --    Check_object(bestiaire(b).all);
    --  end loop;

  end Create_Objects;

  procedure Dump_Objects is
  begin
    for i in bestiaire'Range loop
      if i = level_idx then
        for j in level_stuff'Range loop
          G3D.IO.Save_File (level_stuff (j).all);
        end loop;
      else
        G3D.IO.Save_File (bestiaire (i).all);
      end if;
    end loop;
    G3D.IO.Save_File (bri2.all);
    G3D.IO.Save_File ("Delta4g1", level_BSP);
  end Dump_Objects;

  detect_collisions : Boolean := True;

  procedure Display_Scene
    (o : in out G3D.Object_3D'Class;
     gc : Game_Control.Command_Set;
     sec : G3D.Real;
     technical_infos : Boolean)
  is
    use G3D, G3D.Aux, G3DM, GL.Math;
    use type GL.Double;
    --
    procedure Show_Technical_Infos is
      procedure Msg (line : GL.Int; s : String) is
      begin
        GLUT_2D.Text_output
          (0, line, main_size_x, main_size_y, s, GLUT_2D.Helvetica_10);
      end Msg;
      light_info : String (1 .. 8);
    begin
      GL.PushMatrix;

      GL.Disable (GL.Lighting);
      GL.Disable (GL.Texture_2D);

      GL.Color (red   => 0.7,
                green => 0.7,
                blue  => 0.6);

      GLUT_2D.Text_output ((0.0, 0.0, 0.0), "O", GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_output ((1.0, 0.0, 0.0), "x", GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_output ((0.0, 1.0, 0.0), "y", GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_output ((0.0, 0.0, 1.0), "z", GLUT_2D.Times_Roman_24);

      Msg (10, "Press Space for next object/scene.    Object name: " &
        Get_Name (o) &
        ". Points:" & Integer'Image (o.Max_points) &
        ". Faces:"  & Integer'Image (o.Max_faces) &
        ". GL Lists: " & List_Cases'Image (o.List_Status));
      Msg (20, "Run mode (Shift): " &
        Boolean'Image (gc (Game_Control.run_mode)));
      Msg (30, "Slide mode (Alt): " &
        Boolean'Image (gc (Game_Control.slide_mode)));
      Msg (40, "Ctrl mode: "  &
        Boolean'Image (gc (Game_Control.ctrl_mode)));

      Msg (50, "Eye: " & Coords (ego.clipper.eye_position) & " reset: 0");
      Msg (60, "View direction: " & Coords (ego.clipper.view_direction));

      for i in light_info'Range loop
        if Is_light_switched (i) then
          light_info (i) := Character'Val (Character'Pos ('0') + i);
        else
          light_info (i) := 'x';
        end if;
      end loop;
      Msg (70, "Lights: [" & light_info & ']');
      if Options.portal_tracking then
        Msg (80,
          "Connected objects seen:" & Natural'Image (info_b_ntl2) &
          "; max portal depth:" & Natural'Image (info_b_ntl3));
      end if;
      if Options.BSP_tracking and then beast_idx = level_idx then
        Msg (90, "BSP depth: " & Natural'Image (info_b_ntl1) &
          ". Area found: " & Boolean'Image (info_b_bool1) &
          ". BSP path: " & To_String (info_b_str1));
      end if;
      Msg (100, "Collision detection (F10): " & Boolean'Image (detect_collisions));

      if sec > 0.0 then
        Msg (140, "FPS: " & Integer'Image (Integer (1.0 / sec)));
      end if;

      GL.PopMatrix;
    end Show_Technical_Infos;
    --
  begin
    GL.Disable (GL.Lighting);
    --  Depth comparison function is set to LEQUAL is needed for multitexturing:
    --  LESS (the default) prevents showing another texture onto the first one.
    GL.Clear (GL.DEPTH_BUFFER_BIT);
    GL.Enable (GL.Depth_Test);
    GL.DepthFunc (GL.LEQUAL);
    --  ALPHA_TEST: prevent very transparent pixels to be displayed at all and to influence
    --  the depth buffer. E.g. for cross-shaped (non-convex) grass, the faces displayed behind
    --  are hidden by texture pixels that are transparent, but in front and displayed first.
    GL.Enable    (GL.Alpha_Test);
    GL.AlphaFunc (GL.GREATER, 0.05);
    --
    GL.MatrixMode (GL.MODELVIEW);
    Set_GL_Matrix (ego.world_rotation);
    Stars.Display (ego.world_rotation);
    GL.Enable (GL.Lighting);
    GL.Enable (GL.Cull_Face);
    GL.CullFace (GL.Back);

    GL.Translate (-ego.clipper.eye_position);

    ------------------------
    -- Display the object --
    ------------------------
    GL.PushMatrix;
    G3D.Display (o, ego.clipper);
    GL.PopMatrix;

    if technical_infos then
      Show_Technical_Infos;
    end if;
  end Display_Scene;

  --  Timer management
  last_time : Integer;
  sample : array (1 .. 123) of Natural := (others => 0);
  average : G3D.Real;  --  avg milliseconds
  new_scene : Boolean := True;

  gc : Game_Control.Command_Set := Game_Control.no_command;

  technical_infos_enabled : Boolean := True;

  procedure Graphic_Display is
    use type GL.Double;
  begin
    Display_Scene
      (bestiaire (beast_idx).all, gc,
       average * 0.001,
       technical_infos_enabled);
  end Graphic_Display;

  package SAA is new GLOBE_3D.Software_Anti_Aliasing (Graphic_Display);

  type Smoothing_method is (none, software, hardware);
  --  hardware doesn't work (some code must be missing) and produces
  --  a dotted display on Vista.

  smoothing : constant Smoothing_method := none;

  procedure Fill_Screen is
  begin
    Set_Background_Color;
    case smoothing is
      when software =>
        SAA.Set_Quality (SAA.Q3);
        for SAA_Phase in 1 .. SAA.Anti_Alias_Phases loop
          SAA.Display_with_Anti_Aliasing (SAA_Phase);
        end loop;
      when hardware =>
        GL.Enable (GL.MULTISAMPLE_ARB);  --  (if not done yet)
        GL.Clear (GL.COLOR_BUFFER_BIT);
        Graphic_Display;
        GL.Flush;
      when none =>
        GL.Clear (GL.COLOR_BUFFER_BIT);
        Graphic_Display;
        GL.Flush;
    end case;
    GLUT.SwapBuffers;
  end Fill_Screen;

  mem_rot : G3D.Matrix_33 := G3D.Id_33;

  procedure Reset_Eye is
  begin
    ego.clipper.eye_position := (0.0, 0.0, 4.0);
    ego.rotation := (0.0, 0.0, 0.0);
    ego.world_rotation := G3D.Id_33;
  end Reset_Eye;

  screenshot_count : Natural := 0;

  video_count : Natural := 0;

  video_rate : constant := 24;                  --  was 20 (for an old machine...)
  video_declared_rate : constant := video_rate;  --  was 30 (for an old machine...)
  seconds_video : Long_Float;  --  Seconds since last captured image
  trigger_video : constant Long_Float := 1.0 / Long_Float (video_rate);

  object_rotation_speed : G3D.Vector_3D := (0.0, 0.0, 0.0);

  procedure My_Limiting (step : in out GLOBE_3D.Vector_3D) is
    use G3D.Collision_Detection;
    radius : constant := 4.0;
    reacted : G3D.Real;  --  unused further
  begin
    if detect_collisions then
      Reaction
        (bestiaire (beast_idx).all,
         (ego.clipper.eye_position, radius),
         slide,
         step,
         reacted);
    end if;
  end My_Limiting;

  procedure My_Limited_Translation is
  new Actors.Limited_Translation (My_Limiting);

  procedure Main_Operations is

    use G3D, G3DM, G3D.REF, G3D.BSP, Game_Control;

    function Can_be_rotated return Boolean is
    begin
      --  Block object rotation if we have several objects
      return not (
        beast_idx = bri_idx or
        (beast_idx = level_idx and level_stuff'Length > 1)
      );
    end Can_be_rotated;

    elaps, time_now : Integer;
    gx, gy : GL.Double;   --  Mouse movement since last call
    seconds : GL.Double;  --  Seconds since last image
    alpha_correct : Boolean;
    attenu_t, attenu_r : Real;
    cycle_scene : Boolean;
    use type GL.Double;
  begin
    --  Number of milliseconds since GLUT.Init
    time_now := GLUT.Get (GLUT.ELAPSED_TIME);

    if new_scene then
      new_scene := False;
      elaps := 0;
    else
      elaps := time_now - last_time;
    end if;
    last_time := time_now;
    average := 0.0;
    for i in reverse sample'First + 1 .. sample'Last loop
      sample (i) := sample (i - 1);
      average := average + Real (sample (i));
    end loop;
    sample (sample'First) := elaps;
    average := average + Real (elaps);
    average := average / Real (sample'Length);

    seconds := Real (elaps) * 0.001;
    attenu_t := Real'Min (0.975, Real'Max (0.40, 1.0 - seconds * 3.0));
    attenu_r := attenu_t ** 0.75;

    gc := no_command;

    Game_Control.Append_Commands
      (size_x     => Integer (main_size_x),
       size_y     => Integer (main_size_y),
       warp_mouse => full_screen,
       c          => gc,
       gx         => gx,
       gy         => gy);

    if forget_mouse > 0 then -- mouse coords disturbed by resize
      gx := 0.0;
      gy := 0.0;
      forget_mouse := forget_mouse - 1;
    end if;

    if gc (interrupt_game) then
      Say_bye_to_GLUT;
    end if;

    technical_infos_enabled :=
      not (gc (photo) or capturing_video);

    cycle_scene := gc (jump);
    if cycle_scene then
      if Can_be_rotated then
        mem_rot := bestiaire (beast_idx).rotation;
      end if;
      beast_idx := beast_idx + 1;  --  Next object, please !
      if beast_idx > bestiaire'Last then
        beast_idx := bestiaire'First;
      end if;
      if Can_be_rotated then
        bestiaire (beast_idx).rotation := mem_rot;
      end if;
      Reset_Eye;
    end if;

    alpha_correct := False;
    if gc (special_plus)  then alpha := alpha + seconds; alpha_correct := True; end if;
    if gc (special_minus) then alpha := alpha - seconds; alpha_correct := True; end if;
    if alpha_correct then
      if alpha < 0.0 then alpha := 0.0;
      elsif alpha > 1.0 then alpha := 1.0; end if;
      for f in bestiaire (beast_idx).face'Range loop
        bestiaire (beast_idx).face (f).alpha := alpha;
      end loop;
    end if;

    if gc (toggle_10) then
      detect_collisions := not detect_collisions;
    end if;

    ego.compose_rotations := Can_be_rotated;
    --  ^ otherwise you get sea-sick when walking!...

    -------------------------------------
    -- Rotating they eye or the object --
    -------------------------------------
    if gc (ctrl_mode) then
      if Can_be_rotated then
        Actors.Abstract_Rotation
          (
           gc => gc,
           gx => gx,
           gy => gy,
           unitary_change => seconds,
           deceleration   => attenu_r,
           matrix         => bestiaire (beast_idx).rotation,
           time_step      => seconds,
           rotation_speed => object_rotation_speed);
      end if;
    else
      Actors.Rotation
        (actor => ego,
         gc => gc,
         gx => gx,
         gy => gy,
         unitary_change => seconds,
         deceleration   => attenu_r,
         time_step      => seconds);
    end if;

    --------------------
    -- Moving the eye --
    --------------------
    if cycle_scene then
      --  When scene just has been changed, a call to collision detection after
      --  or during a movement may produce:
      --    raised PROGRAM_ERROR : EXCEPTION_ACCESS_VIOLATION
      --  and this, in "Fast" or "Small" modes, not in "Debug" mode. Strange, isn't it ?
      null;
    else
      My_Limited_Translation
        (actor => ego,
         gc => gc,
         gx => gx,
         gy => gy,
         unitary_change => seconds,
         deceleration   => attenu_t,
         time_step      => seconds);
    end if;

    if beast_idx = bri_idx then
      --  The cheapest Binary Space Partition ever !...
      if ego.clipper.eye_position (2) < -50.0 then
        bestiaire (bri_idx) := bri2;
      else
        bestiaire (bri_idx) := bri1;
      end if;
    elsif beast_idx = level_idx and level_BSP /= null then
      declare
        area : p_Object_3D;
      begin
        G3D.BSP.Locate (ego.clipper.eye_position, level_BSP, area);
        if area = null then
          null;  --  not found, we keep the previous one
        else
          bestiaire (level_idx) := area;
        end if;
      end;
    end if;

    ego.clipper.view_direction :=
      Transpose (ego.world_rotation) * (0.0, 0.0, -1.0);

    frontal_light.position :=
      (GL.Float (ego.clipper.eye_position (0)),
       GL.Float (ego.clipper.eye_position (1)),
       GL.Float (ego.clipper.eye_position (2)),
       1.0);
    G3D.Define (1, frontal_light);

    if gc (n0) then
      Reset_Eye;
    end if;

    -------------------------
    -- Control of lighting --
    -------------------------

    for c in n1 .. n8 loop
      if gc (c) then
        Reverse_Light_Switch (1 + Command'Pos (c) - Command'Pos (n1));
      end if;
    end loop;

    ------------------------
    -- Display everything --
    ------------------------

    Fill_Screen;

    --------------------------------
    -- Screenshot / Video capture --
    --------------------------------

    if gc (photo) then
      screenshot_count := screenshot_count + 1;
      declare
        n : constant String := Integer'Image (1_0000 + screenshot_count);
      begin
        GL.IO.Screenshot ("shot" & n (n'Last - 3 .. n'Last) & ".bmp");
      end;
    end if;

    if gc (video) then  --  start / stop capture
      if capturing_video then
        GL.IO.Stop_capture;
      else
        video_count := video_count + 1;
        declare
          n : constant String := Integer'Image (1_0000 + video_count);
        begin
          GL.IO.Start_capture
            ("capture" & n (n'Last - 3 .. n'Last) & ".avi",
             video_declared_rate);
        end;
        seconds_video := 0.0;
      end if;
      capturing_video := not capturing_video;
    end if;

    if capturing_video then
      if seconds_video > trigger_video then
        seconds_video := seconds_video - trigger_video;
        GL.IO.Capture_frame;
      end if;
      seconds_video := seconds_video + Long_Float (seconds);
    end if;

  end Main_Operations;

  --  Procedures passed to GLUT here: Window_Resize, Menu, Main_Operations
  --  GLUT.Devices handles: Keyboard, Motion, Mouse

  procedure Start_GLUTs is
    use GLUT;
    GLUT_options : GLUT.Unsigned := GLUT.DOUBLE or GLUT.RGB or GLUT.DEPTH;
  begin
    Init;
    case smoothing is
      when hardware =>
        GLUT_options := GLUT_options or GLUT.MULTISAMPLE;
      when others =>
        null;
    end case;
    InitDisplayMode (GLUT_options);
    main_size_x := 1280; -- 854;  --  YouTube recommended sizes, for 16/9
    main_size_y := 720;  -- 480;  --  YouTube recommended sizes, for 16/9
    InitWindowSize (Integer (main_size_x), Integer (main_size_y));
    InitWindowPosition (120, 120);
    if CreateWindow
      ("GLOBE_3D / Demo_1 / Any Debug = " &
       G3D.Options.Is_Debug_Mode'Image &
       " / Press Space key for next scene")
      = 0
    then
      raise GLUT_Problem;
    end if;
    ReshapeFunc (Window_Resize'Address);
    DisplayFunc (Main_Operations'Address);
    IdleFunc    (Main_Operations'Address);
    GLUT.Devices.Initialize;

    if CreateMenu (Menu'Address) = 0 then
      raise GLUT_Problem;
    end if;
    AttachMenu (MIDDLE_BUTTON);
    AddMenuEntry (" * Full Screen", 1);
    AddMenuEntry ("--> Exit (Esc)", 2);
  end Start_GLUTs;

  procedure Start_GLs is
    fog_colour : GL.Light_Float_Vector :=
      (0.05, 0.15, 0.15, 1.0);  --  looks like a toxic smoke...
    use type GL.Float;
  begin
    Clear_modes;
    Prepare_demo_lighting (0.9);
    if foggy then
      GL.Enable (GL.Fog);
      GL.Fog (GL.FOG_MODE, GL.LINEAR);
      GL.Fog (GL.FOG_COLOR, fog_colour (0)'Unchecked_Access);
      GL.Hint (GL.FOG_HINT, GL.FASTEST);
      GL.Fog (GL.FOG_START, 1.0);
      GL.Fog (GL.FOG_END, 0.5 * fairly_far);
    end if;
    Reset_for_3D (Integer (main_size_x), Integer (main_size_y));
    case smoothing is
      when hardware =>
        GL.Enable (GL.MULTISAMPLE_ARB);
        GL.Enable (GL.SAMPLE_COVERAGE_ARB);  --  Hope it helps switching on the AA...
      when others =>
        null;
    end case;
  end Start_GLs;

  --  Get eventual command line arguments.

  type Switch_Type is
    (load,   --  load some scenes from .g3d files stored in the GLOBE_3D
             --         resource files, instead of rebuilding them (default).
             --   Additionally,
             --    "-load=mylevel" sets "mylevel.zip" as level resource;
             --    from that resource, the demo loads the mylevel_$_area#.g3d
             --    objects with #=1,2,3..., and loads mylevel.bsp.
     dump);  --  dump all objects of the demo to .g3d files

  switch : array (Switch_Type) of Boolean := (others => False);
  custom : Unbounded_String;

  procedure Get_Arguments is
    use Ada.Command_Line;
  begin
    for s in Switch_Type loop
      for a in 1 .. Argument_Count loop
        declare
          arg_long : constant String := Argument (a);
          swi      : constant String := Switch_Type'Image (s);
          arg      : constant String := To_Upper (arg_long (arg_long'First .. swi'Last + 1));
        begin
          if arg = '-' & swi or arg = '/' & swi then
            switch (s) := True;
            if s = load then
              custom := To_Unbounded_String (arg_long (swi'Last + 3 .. arg_long'Last));
            end if;
          end if;
        end;
      end loop;
    end loop;
  end Get_Arguments;

begin
  Get_Arguments;
  G3D.Set_Global_Data_Name ("g3demo_global_resources.zip");
  G3D.Set_Level_Data_Name ("g3demo_level_resources.zip");
  --
  G3D.Textures.Register_Textures_From_Resources;

  Create_Objects (switch (load), To_String (custom));
  if switch (dump) then
    Dump_Objects;  --  even those that were loaded (entropy check)
  end if;

  Start_GLUTs;    --  Initialize the GLUT things
  Start_GLs;      --  Initialize the (Open)GL things
  Reset_Eye;

  G3D.Textures.Check_All_Textures;  --  Preload the textures

  --  So far, there is an issue with ObjectAda Win32, GLUT.MainLoop callback,
  --  freeglut, under Windows 7 x64. Display @ Main_Operations is fine.
  --
  GLUT.MainLoop;  --  Let's rock !

end GLOBE_3D_Demo;
