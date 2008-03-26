------------------------------------------------------------------------------
--  File:            GLUT-Windows.ads
--  Description:     A Windowed viewer for GLOBE_3D, based on GLUT
--  Copyright (c) Gautier de Montmollin/Rod Kay 2006..2007
------------------------------------------------------------------------------



-- tbd: - add new 'traits' for glutGet state data.
--      - generalise lighting, textures, game controls
--      - find way to fix visibilty when window is iconised (may be platform dependant).


with gl.Geometry;
with gl.skinned_Geometry;

with GLOBE_3D.Textures;
with Game_Control;
with glow.Devices;

with ada.strings.unbounded;



package glow.Windows is


   procedure initialize;   -- called before any other operation



   type Window is new GLOBE_3D.Window with private;
   type Window_view is access all Window'Class;



   procedure define  (Self : in out Window);
   procedure destroy (Self : in out Window);


   procedure Name_is (Self : in out Window;   Now : in String);
   function  Name    (Self : in     Window) return String;


   procedure enable (Self : in out Window);

   procedure freshen (Self      : in out Window;
                      time_Step : in     globe_3d.Real;
                      Extras    : in     globe_3d.Visual_array := globe_3d.null_Visuals);

   function is_Closed (Self : in Window) return Boolean;


   -- objects
   --

   procedure add (Self : in out Window;   the_Object : in globe_3d.p_Visual);
   procedure rid (Self : in out Window;   the_Object : in globe_3d.p_Visual);

   function  object_Count (Self : in Window) return Natural;

   no_such_Object : exception;   -- raised when trying to 'rid' an object which has not been added to the Window.






   -- smoothing
   --

   type Smoothing_method is ( none, software, hardware );

   function  Smoothing    (Self : in     Window)                             return Smoothing_method;
   procedure Smoothing_is (Self : in out Window;   Now : in Smoothing_method);




   -- textures (tbd: generalise textures)
   --

   type Texture_id is (face1, face2, face3, face4, face5, face6,
                       portmet1, fdmetal1, bleubosl,
                       alum_001, alum_002,
                       tole_001, grumnoir,
                       carometl, spacity1,
                       quad_qcq,
                       earth_map,
                       GBWINGS, GBTOP, GBDEC, GLOWING,
                       irin_Terrain);


   procedure Texture_association is
     new globe_3d.Textures.Associate_textures(Texture_id);





   -- Status display
   --

   function  show_Status (Self : in     Window) return Boolean;
   procedure show_Status (Self : in out Window;
                          Show : in     Boolean := True);


   procedure Display_status (Self : in out Window;
                             sec  :        globe_3d.Real);


   function Frames_per_second (Self : in Window) return Float;




   -- Devices
   --

   function Keyboard (Self : access Window'Class) return devices.p_Keyboard;
   function Mouse    (Self : access Window'Class) return devices.p_Mouse;





private


   type natural_Array is array (Positive range 1 .. 123) of Natural;



   type Window is new GLOBE_3D.Window with
      record
         Name         : ada.strings.unbounded.unbounded_String := ada.strings.unbounded.to_unbounded_String ("globe3d glut window");
         glut_Window  : Integer;

         Objects      : globe_3d.Visual_array (1 .. 5_000);
         object_Count : Natural := 0;

         Smoothing    : Smoothing_method := Hardware;
         is_Visible   : Boolean          := True;
         is_Closed    : Boolean          := False;
         show_Status  : Boolean          := True;

         main_size_x,
         main_size_y  : GL.SizeI;

         foggy           : Boolean                  := False;
         frontal_light   : globe_3d.Light_definition;
         forget_mouse    : Natural                  := 0;
         full_screen     : Boolean                  := False;
         alpha           : GL.Double                := 1.0;

         -- Timer management

         last_time : Integer;
         sample    : natural_Array := (others => 0);
         average   : globe_3d.Real := 30.0;                                -- avg milliseconds
         new_scene : Boolean      := True;

         game_command : Game_control.Command_set := Game_control.no_command;

         -- Devices

         Keyboard : aliased devices.Keyboard;
         Mouse    : aliased devices.Mouse;


         -- Video management

         is_capturing_Video : Boolean := False;

      end record;



  --pragma Linker_options("-mwindows"); -- Suppress console window
end glow.Windows;
