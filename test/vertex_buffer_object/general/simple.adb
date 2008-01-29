
with GLOBE_3D;                        use GLOBE_3D;

with gl.geometry;                     use gl.Geometry;
with gl.Buffer.vertex;         use gl.Buffer.vertex;
with gl.Errors;

with glut.Windows; use glut.Windows;

with ada.text_io;                     use ada.text_io;



procedure Simple
is
   package g3d renames GLOBE_3D;

   Viewer     : aliased GLUT.windows.Window;

begin
   glut.Windows.initialize;
   define (Viewer);            -- define a Viewer to simply initialise glut & gl. (tbd: get rid of this need)


   declare
      the_Vertices : aliased gl.geometry.vertex_Array := ((1.1, 1.1, 1.1), (2.2, 2.2, 2.2), (3.3, 3.3, 3.3));
      the_Buffer   : aliased gl.buffer.vertex.Object  := to_Buffer (the_Vertices'access,  usage => gl.STATIC_DRAW);

      procedure log
      is
         mapped_Vertices : gl.geometry.vertex_Array renames get (the_Buffer'access);
      begin
         put_Line ("the_Vertices: ");
         put_Line (Image (mapped_Vertices));
      end;

   begin
      log;

      -- test 'set_Vertices'
      --

      -- test normal setting of vertices
      --
      set (the_Buffer, to => ((5.1, 5.1, 5.1), (6.2, 6.2, 6.2), (7.3, 7.3, 7.3)));
      log;


      -- a test for bounds exceeded.
      --
      begin
         set (the_Buffer, position => 2,  -- nb: starting at '2' will exceed the buffers bounds, so error is expected.
                          to       => ((5.1, 5.1, 5.1), (6.2, 6.2, 6.2), (7.3, 7.3, 7.3)));
         put_Line ("fail: expected openGL_Error *not* detected");
      exception
         when gl.errors.openGL_Error =>
            put_Line ("pass: expected openGL_Error detected and handled");
      end;
      log;


      -- test setting only the 1st two vertices
      --
      set (the_Buffer, to => ((1.0, 1.0, 1.0), (2.0, 2.0, 2.0)));
      log;


      -- test setting only the last two vertices
      --
      set (the_Buffer, position => 2,
                       to       => ((8.0, 8.0, 8.0), (9.0, 9.0, 9.0)));
      log;


      -- test 'get_Vertices'
      --

      declare
         mapped_Vertices : gl.buffer.vertex.write_only_Map'Class renames Map (the_Buffer'access);
         new_Vertex      : aliased gl.geometry.Vertex                 := (0.0, 1.0, 0.0);
      begin
         mapped_Vertices.set (1,  (0.0, 0.0, 0.0));
         mapped_Vertices.set (3,  new_Vertex'unchecked_access);
         mapped_Vertices.release;
      end;
      log;



      -- test read-only Map
      --

      declare
         mapped_Vertices : gl.buffer.vertex.read_only_Map'Class := Map (the_Buffer'access);
         the_Vertex      : gl.geometry.Vertex              renames mapped_Vertices.get (2);
      begin
         put_Line (Image (the_Vertex));
         mapped_Vertices.release;
      end;


   end;


end Simple;
