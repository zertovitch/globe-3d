------------------------------------------------------------------------------
--  File:            GLUT-Windows.ads
--  Description:     A Windowed viewer for GLOBE_3D, based on GLUT
--
--                   TBD: give a more appropriate name (GLUT has
--                        nothing to do with GLOBE_3D)...
--
--  Copyright (c) Gautier de Montmollin / Rod Kay 2006 .. 2021
------------------------------------------------------------------------------

--  tbd: - add new 'traits' for glutGet state data.
--       - generalise lighting, textures, game controls
--       - find way to fix visibilty when window is iconised (may be platform dependant).

--  with gl.Geometry;
--  with gl.skinned_Geometry;

with Game_Control;
with GLUT.Devices;
with GLOBE_3D.Skinned_Visuals;

with Ada.Strings.Unbounded,
     Ada.Containers.Vectors;

package GLUT.Windows is

   procedure Initialize;   -- called before any other operation

   type Window_Pre_Type is tagged record
     Camera : aliased GLOBE_3D.Camera;  --  public
   end record;

   type Window is new Window_Pre_Type with private;
   type Window_View is access all Window'Class;
   type p_Window is access all Window'Class;

   procedure Define  (Self : in out Window);
   procedure Destroy (Self : in out Window);

   procedure Name_is (Self : in out Window; Now : in String);
   function  Name    (Self : in     Window) return String;

   procedure Enable (Self : in out Window);

   type Renderer_Access is
      access procedure
        (the_Visuals : in GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array; the_Camera : in GLOBE_3D.Camera'Class);

   procedure Set_Renderer (Self : in out Window; Renderer : Renderer_Access);

   procedure Freshen (Self      : in out Window;
                      Time_Step : in     GLOBE_3D.Real;
                      Extras    : in     GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array := GLOBE_3D.Skinned_Visuals.null_Visuals);

   function Is_Closed (Self : in Window) return Boolean;

   --  objects
   --

   procedure Add (Self : in out Window;   the_Object : in GLOBE_3D.Skinned_Visuals.p_Skinned_Visual);
   procedure Rid (Self : in out Window;   the_Object : in GLOBE_3D.Skinned_Visuals.p_Skinned_Visual);

   function  Object_Count (Self : in Window) return Natural;

   no_such_Object : exception;
   --  ^ Raised when trying to 'rid' an object which has not been added to the Window.

   --  Smoothing
   --

   type Smoothing_method is (none, software, hardware);

   function  Smoothing    (Self : in     Window) return Smoothing_method;
   procedure Smoothing_is (Self : in out Window; Now : in Smoothing_method);

   --  Status display
   --

   procedure Add_Status_Line
     (Self : in out Window;
      Text : in     String;
      X, Y : in     Integer);

   function  Show_Status (Self : in     Window) return Boolean;
   procedure Show_Status (Self : in out Window;
                          Show : in     Boolean := True);

   procedure Display_Status (Self : in out Window;
                             sec  :        GLOBE_3D.Real);

   function Frames_Per_Second (Self : in Window) return Float;

   --  Devices
   --

   function Keyboard (Self : access Window'Class) return Devices.p_Keyboard;
   function Mouse    (Self : access Window'Class) return Devices.p_Mouse;

private
   use Ada.Strings.Unbounded;

   type Natural_Array is array (Positive range 1 .. 123) of Natural;

   type Status_Line is
      record
         Text : Unbounded_String;
         X, Y : GL.Int;
      end record;

   package Status_Line_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Status_Line);

   type Window is new Window_Pre_Type with record
         Name         : Ada.Strings.Unbounded.Unbounded_String :=
                          Ada.Strings.Unbounded.To_Unbounded_String ("globe3d glut window");
         glut_Window  : Integer;

         Objects      : GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array (1 .. 5_000);
         object_Count : Natural := 0;

         Smoothing    : Smoothing_method := hardware;
         is_Visible   : Boolean          := True;
         is_Closed    : Boolean          := False;
         show_Status  : Boolean          := True;
         extra_Status : Status_Line_Vectors.Vector;

         main_size_x,
         main_size_y  : GL.Sizei;

         foggy           : Boolean                  := False;
         frontal_light   : GLOBE_3D.Light_definition;
         forget_mouse    : Natural                  := 0;
         full_screen     : Boolean                  := False;
         alpha           : GL.Double                := 1.0;

         --  Timer management

         last_time : Integer;
         sample    : Natural_Array := (others => 0);
         average   : GLOBE_3D.Real := 30.0;                                -- avg milliseconds
         new_scene : Boolean       := True;

         game_command : Game_Control.Command_Set := Game_Control.no_command;

         --  Devices

         Keyboard : aliased Devices.Keyboard;
         Mouse    : aliased Devices.Mouse;

         --  Video management

         is_capturing_Video : Boolean := False;

         rend : Renderer_Access;
      end record;

end GLUT.Windows;
