with
     GLUT.Windows,

     GLOBE_3D.Sprite,
     GLOBE_3D.Textures,
     GLOBE_3D.Visuals_rendering,

     GL.Geometry.VBO,
--       gl.Primitive,
     GL.Buffer.Vertex,
     GL.Buffer.Indices,
     GL.Skins,
     GL.Textures,
     GL.Extended;

use
    GLUT.Windows,

    GLOBE_3D.Sprite,
    GLOBE_3D.Textures,

    GL.Geometry.VBO,
--      GL.Primitive,
    GL.Buffer.Vertex,
    GL.Buffer.Indices,
    GL.Skins,
    GL.Textures,
    GL.Extended;

procedure launch_sprite_Demo
is
   use GL, GL.Geometry, GLOBE_3D;
   package g3d renames GLOBE_3D;

   the_Viewer : GLUT.Windows.Window;
   the_Sprite : g3d.Sprite.p_Sprite;

   procedure VBO_Callback
   is
   begin
      GL.Extended.BindBuffer (GL.Extended.ARRAY_BUFFER, 0);
   end VBO_Callback;

begin
   GL.Skins.Disable_VBO_callback := VBO_Callback'Unrestricted_Access;

   g3d.Set_global_data_name ("../g3demo_global_resources.zip");
   g3d.Set_level_data_name  ("../g3demo_level_resources.zip");

   g3d.Textures.Register_textures_from_resources;

   GLUT.Windows.Initialize;

   the_Viewer.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);
   Define (the_Viewer);

   g3d.Textures.Check_all_textures; -- Preload the textures

   the_Sprite := new g3d.Sprite.Sprite (max_Geometries => 1);

   -- Using VBO geometry.
   --
   declare
      the_Geometry : constant GL.Geometry.VBO.p_vbo_Geometry    := new GL.Geometry.VBO.vbo_Geometry;
      the_Skin     : constant p_Skin_transparent_unlit_textured := new Skin_transparent_unlit_textured;

      the_Vertices : aliased GL.Geometry.Vertex_array    := (1 => (-100.0, -100.0, 0.0),
                                                             2 => ( 100.0, -100.0, 0.0),
                                                             3 => ( 100.0,  100.0, 0.0),
                                                             4 => (-100.0,  100.0, 0.0));
      the_Indices  : aliased GL.Geometry.vertex_Id_array := (1 => 1,
                                                             2 => 2,
                                                             3 => 3,
                                                             4 => 4);
   begin
      decrement (the_Indices);     -- Convert Indices to 0-based.

      the_Geometry.Vertices := to_Buffer (the_Vertices'Access, Usage => STATIC_DRAW);
      the_Geometry.Indices  := to_Buffer (the_Indices 'Access, Usage => STATIC_DRAW);

      the_Geometry.vertex_Count  := GL.Sizei (the_Vertices'Length);
      the_Geometry.indices_Count := GL.Sizei (the_Indices'Length);
      the_Geometry.primitive_Id  := GL.QUADS;

      set_Name (the_Skin.Texture,  To => GL.Textures.texture_Name (Texture_ID ("face1")));

      the_Sprite.add (Geometry => the_Geometry.all'Access,
                      Skin     => the_Skin);

      declare
         the_Veneer : constant p_Veneer_transparent_unlit_textured
           := p_Veneer_transparent_unlit_textured (the_Sprite.skinned_Geometries (the_Sprite.skinned_geometry_Count).Veneer);
      begin
         the_Veneer.texture_Coordinates :=  (1 => (0.0, 0.0),
                                             2 => (1.0, 0.0),
                                             3 => (1.0, 1.0),
                                             4 => (0.0, 1.0));
      end;

   end;

   Add (the_Viewer, the_Sprite.all'Access);

   -- Using primitive_Geometry.  -- tbd: update this
   --

   -- the_Sprite := new g3d.sprite.Sprite (max_Geometrys => 1);

--     declare
--        the_Quads    : p_Quads                           := gl.geometry.new_Quads (1, lit => False);
--        the_Geometry : gl.geometry.p_primitive_Geometry  := new gl.geometry.primitive_Geometry' (primitive => the_Quads.all'access);
--        the_Skin     : p_Skin_transparent_unlit_textured := new Skin_transparent_unlit_textured;
--
--        the_Vertices : gl.geometry.vertex_Array := (1 => (-10.0, -10.0, 0.0),
--                                                    2 => ( 10.0, -10.0, 0.0),
--                                                    3 => ( 10.0,  10.0, 0.0),
--                                                    4 => (-10.0,  10.0, 0.0));
--        the_Indices : gl.geometry.vertex_Id_Array := (1 => 1,
--                                                      2 => 2,
--                                                      3 => 3,
--                                                      4 => 4);
--     begin
--        the_Quads.vertex_pool.Vertices := the_Vertices;
--
--        the_Quads.set_vertex_Id (quad => 1,  vertex => 1,  to => 1);
--        the_Quads.set_vertex_Id (quad => 1,  vertex => 2,  to => 2);
--        the_Quads.set_vertex_Id (quad => 1,  vertex => 3,  to => 3);
--        the_Quads.set_vertex_Id (quad => 1,  vertex => 4,  to => 4);
--
--        declare
--           void : boolean;
--        begin
--           bind_2d_texture (glut.windows.texture_id'Pos (face1), void);
--        end;
--        set_Name (the_Skin.Texture,  to => glut.windows.texture_id'pos (glut.windows.face1) + 1);
--
--
--        the_Sprite.add (geometry => the_Geometry.all'access,
--                        skin     => the_Skin);
--
--        p_Veneer_transparent_unlit_textured (the_Sprite.skinned_Geometrys (1).Veneer).texture_Coordinates (1) := (0.0, 0.0);
--        p_Veneer_transparent_unlit_textured (the_Sprite.skinned_Geometrys (1).Veneer).texture_Coordinates (2) := (1.0, 0.0);
--        p_Veneer_transparent_unlit_textured (the_Sprite.skinned_Geometrys (1).Veneer).texture_Coordinates (3) := (1.0, 1.0);
--        p_Veneer_transparent_unlit_textured (the_Sprite.skinned_Geometrys (1).Veneer).texture_Coordinates (4) := (0.0, 1.0);
--     end;

--   add (the_Viewer, the_Sprite.all'access);

   while not the_Viewer.Is_Closed
   loop
      GLUT.MainLoopEvent;
      Freshen (the_Viewer, Time_Step => 0.02);
   end loop;

   Destroy (the_Viewer);
end launch_sprite_Demo;
