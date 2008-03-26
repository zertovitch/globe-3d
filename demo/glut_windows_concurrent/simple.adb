
with GLOBE_3D;     use GLOBE_3D;
with glow.Window; use glow.Window;
with gl;

with Box;

with ada.text_io; use ada.text_io;
with ada.Exceptions;



procedure Simple
is
   use GL;
   package g3d renames GLOBE_3D;

   Viewer_1   : glow.Window.item;
   Viewer_3   : glow.Window.item;

   the_Object : g3d.p_Object_3D;



--     task V2 is
--        entry start;
--     end;
--
--
--     task body V2
--     is
--        Viewer_2   : GLUT.windows.Window;
--     begin
--        accept Start;
--
--        g3d.Set_level_data_name  ("../G3Demo_Global_Resources.zip");
--        g3d.Set_global_data_name ("../G3Demo_Level_Resources.zip");
--
--        define (Viewer_2);
--        add (Viewer_2,  the_Object.all'access);
--
--        loop
--           --GLUT.mainLoopEvent;
--
--           exit when Viewer_2.is_Closed;
--
--           freshen (Viewer_2,  time_step => 0.02);
--
--        end loop;
--
--        destroy (Viewer_2);
--
--     exception
--        when E: others =>
--           put_Line ("V2 unhandled exception ...");
--           put_Line (ada.exceptions.Exception_Information (E));
--           put_Line ("V2 has terminated !");
--     end V2;




begin
   g3d.Set_level_data_name  ("../G3Demo_Global_Resources.zip");
   g3d.Set_global_data_name ("../G3Demo_Level_Resources.zip");

   glow.Window.initialize;

   define (Viewer_1);
   define (Viewer_3);

   box.create (the_Object);

   the_Object.Centre := (0.0, 2.0, -5.0);

   add (Viewer_1,  the_Object.all'access);
   add (Viewer_3,  the_Object.all'access);

   --V2.start;


   loop
      glow.mainLoopEvent;

      exit when     Viewer_1.is_Closed;

      freshen (Viewer_1,  time_step => 0.02);
      freshen (Viewer_3,  time_step => 0.02);

   end loop;

   destroy (Viewer_1);
   destroy (Viewer_3);

   put_Line ("Done");

--  exception
--     when E: others =>
--        put_Line ("V1 unhandled exception ...");
--        put_Line (ada.exceptions.Exception_Information (E));
--        put_Line ("V1 has terminated !");
end Simple;



