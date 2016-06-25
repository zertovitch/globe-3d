------------------------------------------------------------------------------
--  File:            GLUT-Windows.ads
--  Description:     A Windowed viewer for GLOBE_3D, based on GLUT
--  Copyright (c) Gautier de Montmollin/Rod Kay 2006..2008
------------------------------------------------------------------------------

-- tbd: - add new 'traits' for glutGet state data.
--      - generalise lighting, textures, game controls
--      - find way to fix visibilty when window is iconised (may be platform dependant).

--with gl.Geometry;
--with gl.skinned_Geometry;

with Game_control;
with GLUT.Devices;

with Ada.Strings.Unbounded;

with GLOBE_3D;

package GLUT.Windows is

   procedure initialize;   -- called before any other operation

   type Window is new GLOBE_3D.Window with private;
   type Window_view is access all Window'Class;

   procedure define  (Self : in out Window);
   procedure destroy (Self : in out Window);

   procedure Name_is (Self : in out Window;   Now : in String);
   function  Name    (Self : in     Window) return String;

   overriding
   procedure enable (Self : in out Window);

   type Renderer_Access is
      access procedure (the_Visuals : in GLOBE_3D.Visual_array; the_Camera : in GLOBE_3D.Camera'Class);

   procedure Set_renderer(Self: in out Window; Renderer: Renderer_Access);

   overriding
   procedure freshen (Self      : in out Window;
                      time_Step : in     GLOBE_3D.Real;
                      Extras    : in     GLOBE_3D.Visual_array := GLOBE_3D.null_Visuals);

   function is_Closed (Self : in Window) return Boolean;

   -- objects
   --

   procedure add (Self : in out Window;   the_Object : in GLOBE_3D.p_Visual);
   procedure rid (Self : in out Window;   the_Object : in GLOBE_3D.p_Visual);

   function  object_Count (Self : in Window) return Natural;

   no_such_Object : exception;   -- raised when trying to 'rid' an object which has not been added to the Window.

   -- smoothing
   --

   type Smoothing_method is ( none, software, hardware );

   function  Smoothing    (Self : in     Window)                             return Smoothing_method;
   procedure Smoothing_is (Self : in out Window;   Now : in Smoothing_method);

   -- Status display
   --

   function  show_Status (Self : in     Window) return Boolean;
   procedure show_Status (Self : in out Window;
                          Show : in     Boolean := True);

   procedure Display_status (Self : in out Window;
                             sec  :        GLOBE_3D.Real);

   function Frames_per_second (Self : in Window) return Float;

   -- Devices
   --

   function Keyboard (Self : access Window'Class) return Devices.p_Keyboard;
   function Mouse    (Self : access Window'Class) return Devices.p_Mouse;

private

   type natural_Array is array (Positive range 1 .. 123) of Natural;

   type Window is new GLOBE_3D.Window with
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("globe3d glut window");
         glut_Window  : Integer;

         Objects      : GLOBE_3D.Visual_array (1 .. 5_000);
         object_Count : Natural := 0;

         Smoothing    : Smoothing_method := hardware;
         is_Visible   : Boolean          := True;
         is_Closed    : Boolean          := False;
         show_Status  : Boolean          := True;

         main_size_x,
         main_size_y  : GL.Sizei;

         foggy           : Boolean                  := False;
         frontal_light   : GLOBE_3D.Light_definition;
         forget_mouse    : Natural                  := 0;
         full_screen     : Boolean                  := False;
         alpha           : GL.Double                := 1.0;

         -- Timer management

         last_time : Integer;
         sample    : natural_Array := (others => 0);
         average   : GLOBE_3D.Real := 30.0;                                -- avg milliseconds
         new_scene : Boolean      := True;

         game_command : Game_control.Command_set := Game_control.no_command;

         -- Devices

         Keyboard : aliased Devices.Keyboard;
         Mouse    : aliased Devices.Mouse;

         -- Video management

         is_capturing_Video : Boolean := False;

         rend: Renderer_Access;
      end record;

  --pragma Linker_options("-mwindows"); -- Suppress console window
end GLUT.Windows;
