------------------------------------------------------------------------------
--  File:            GLUT-Windows.ads
--  Description:     A Windowed viewer for GLOBE_3D, based on GLUT
--  Copyright (c) Gautier de Montmollin/Rod Kay 2006..2007
------------------------------------------------------------------------------



-- tbd: - add new 'traits' for glutGet state data.
--      - generalise lighting, textures, game controls
--      - find way to fix visibilty when window is iconised (may be platform dependant).


with ogl.Visual;
with ogl.Geometry;
with ogl.skinned_Geometry;
with ogl.Window;

with GLOBE_3D.Textures;
with Game_Control;

with glow.Devices;
with glow.Culler;
with glow.Camera;

with ada.strings.unbounded;



package glow.Window is


   procedure initialize;   -- called before any other operation



   type Item is new ogl.Window.item with private;             -- new GLOBE_3D.Window with private;
   type View is access all Item'Class;



   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);


   procedure Name_is (Self : in out Item;   Now : in String);
   function  Name    (Self : in     Item) return String;


   procedure enable (Self : in out Item);

   procedure freshen (Self      : in out Item;
                      time_Step : in     globe_3d.Real;
                      Extras    : in     globe_3d.Visual_array := globe_3d.null_Visuals);

   function is_Closed (Self : in Item) return Boolean;


   -- objects
   --

   procedure add (Self : in out Item;   the_Object : in globe_3d.p_Visual);
   procedure rid (Self : in out Item;   the_Object : in globe_3d.p_Visual);

   function  object_Count (Self : in Item) return Natural;

   no_such_Object : exception;   -- raised when trying to 'rid' an object which has not been added to the Window.






   -- smoothing
   --

   type Smoothing_method is ( none, software, hardware );

   function  Smoothing    (Self : in     Item)                             return Smoothing_method;
   procedure Smoothing_is (Self : in out Item;   Now : in Smoothing_method);




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

   function  show_Status (Self : in     Item) return Boolean;
   procedure show_Status (Self : in out Item;
                          Show : in     Boolean := True);


   procedure Display_status (Self : in out Item;
                             sec  :        globe_3d.Real);


   function Frames_per_second (Self : in Item) return Float;




   -- Devices
   --

   function Keyboard (Self : access Item'Class) return devices.p_Keyboard;
   function Mouse    (Self : access Item'Class) return devices.p_Mouse;





private


   type natural_Array is array (Positive range 1 .. 123) of Natural;



   protected
   type safe_Visuals is

      procedure add (the_Visual : in ogl.Visual.p_Visual);
      procedure rid (the_Visual : in ogl.Visual.p_Visual);

      function Elements return globe_3d.Visual_array;

   private

      the_Visuals : globe_3d.Visual_array (1 .. 5_000);
      Count       : Natural;
   end safe_Visuals;



   type Item is new ogl.Window.item with
      record
         Name         : ada.strings.unbounded.unbounded_String := ada.strings.unbounded.to_unbounded_String ("globe3d glut window");
         glut_Window  : Integer;

         Camera       : glow.Camera.item;

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


         culler : glow.Culler.item (Item'access);
      end record;



  --pragma Linker_options("-mwindows"); -- Suppress console window
end glow.Window;
