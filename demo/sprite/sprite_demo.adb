
with GLUT.Windows;      use GLUT.Windows;
with GLOBE_3D.Sprite;   use GLOBE_3D.Sprite;
with globe_3d.Textures; use globe_3d.Textures;

with gl.geometry.vbo;          use gl.Geometry.vbo;
with gl.geometry.primitives;   use gl.Geometry.primitives;

with gl.Buffer.vertex;         use gl.Buffer.vertex;
with gl.Buffer.indices;        use gl.Buffer.indices;

with gl.skins;          use gl.Skins;
with gl.Textures;       use gl.Textures;

with ada.Text_IO;       use ada.Text_IO;



procedure sprite_Demo
is
   use GL, gl.Geometry, globe_3d;
   package g3d renames GLOBE_3D;

   the_Viewer   : GLUT.windows.Window;
   the_Sprite   : g3d.sprite.p_Sprite;
begin
   g3d.Set_global_data_name ("../G3Demo_Global_Resources.zip");
   g3d.Set_level_data_name  ("../G3Demo_Level_Resources.zip");

   glut.Windows.initialize;

   define (the_Viewer);


   the_Sprite := new g3d.sprite.Sprite (max_Geometrys => 1);



   -- using vbo geometry
   --
   declare
      the_Geometry : gl.geometry.vbo.p_vbo_Geometry        := new gl.geometry.vbo.vbo_Geometry;
      the_Skin     : p_Skin_transparent_unlit_textured := new Skin_transparent_unlit_textured;

      the_Vertices : aliased gl.geometry.vertex_Array := (1 => (-100.0, -100.0, 0.0),
                                                          2 => ( 100.0, -100.0, 0.0),
                                                          3 => ( 100.0,  100.0, 0.0),
                                                          4 => (-100.0,  100.0, 0.0));
      the_Indices : aliased gl.geometry.vertex_Id_Array := (1 => 1,
                                                            2 => 2,
                                                            3 => 3,
                                                            4 => 4);
   begin
      decrement (the_Indices);   -- convert Indices to 0-based.

      the_Geometry.Vertices := to_Buffer (the_Vertices'access, usage => gl.STATIC_DRAW);
      the_Geometry.Indices  := to_Buffer (the_Indices'access,  usage => gl.STATIC_DRAW);

      the_Geometry.vertex_Count  := gl.SizeI (the_Vertices'Length);
      the_Geometry.indices_Count := gl.SizeI (the_Indices'Length);
      the_Geometry.primitive_Id  := gl.Quads;


      declare
        new_id: Image_ID;
      begin
        Add_texture_name("face1", new_id);
        set_Name (the_Skin.Texture,  to => gl.textures.texture_Name(new_id));
      end;


      the_Sprite.add (geometry => the_Geometry.all'access,
                      skin     => the_Skin);
   end;

   add (the_Viewer, the_Sprite.all'access);



   -- using primitive_Geometry  -- tbd: update this ...
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


   while not the_Viewer.is_Closed loop
      GLUT.mainLoopEvent;

      freshen (the_Viewer, time_step => 0.02);
   end loop;

   destroy (the_Viewer);
   put_Line ("Done.");
end sprite_Demo;
