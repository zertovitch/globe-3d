------------------------------------------------------------------------------
--  File:            GLUT-Windows.adb
--  Description:     A Windowed viewer for GLOBE_3D, based on GLUT
--  Copyright (c) Gautier de Montmollin / Rod Kay 2006 .. 2021
------------------------------------------------------------------------------

with GL, GL.IO, GLU;

with GLOBE_3D.Math,
     GLOBE_3D.Software_Anti_Aliasing,
     GLOBE_3D.Aux;

with GLUT_2D;

with Actors;

with Ada.Numerics;
with Ada.Unchecked_Conversion;

with Ada.Calendar;

package body GLUT.Windows is

   package G3D  renames GLOBE_3D;
   package G3DM renames G3D.Math;

   deg2rad      : constant :=  Ada.Numerics.Pi / 180.0;
   GLUT_Problem : exception;

   --  Current_Window : - for accessing the current GLUT window
   --                   - used by GLUT callbacks to determine the Window
   --                       to which a callback event relates.
   --
   function Current_Window return Window_View
   is
      function to_Window is
        new Ada.Unchecked_Conversion (System.Address, GLUT.Windows.p_Window);
   begin
      return GLUT.Windows.Window_View (to_Window (GetWindowData));
   end Current_Window;

   procedure Name_is (Self : in out Window; Now : in String)
   is
   begin
      Self.Name := To_Unbounded_String (Now);
   end Name_is;

   function  Name (Self : in Window) return String
   is
   begin
      return To_String (Self.Name);
   end Name;

   function Is_Closed (Self : in Window) return Boolean
   is
   begin
      return Self.is_Closed;
   end Is_Closed;

   procedure Prepare_Default_Lighting (Self : in out Window;
                                       fact : in     GL.Float)
   is
      use G3D;
      use type GL.Float;

      proto_light : Light_definition := (position => (0.0, 500.0,  0.0,  1.0),
                                         ambient  => (0.3,   0.3,  0.3,  fact),
                                         diffuse  => (0.9,   0.9,  0.9,  fact),
                                         specular => (0.05,  0.05, 0.01, fact));
   begin
      GL.Enable (GL.Lighting);

      G3D.Define (1, proto_light);
      Self.frontal_light   := proto_light;

      proto_light.diffuse  := (0.5, 0.9, 0.5, fact);
      G3D.Define (2, proto_light);

      proto_light.diffuse  := (0.2, 0.0, 0.9, fact);
      proto_light.specular := (1.0, 1.0, 1.0, fact);
      G3D.Define (3, proto_light);

      proto_light.position := (-3.0, 4.0, 10.0, 1.0);
      G3D.Define (4, proto_light);

      proto_light.position := (3.0, -4.0, 10.0, 1.0);
      proto_light.ambient  := (0.6, 0.6, 0.6, 0.1);
      G3D.Define (5, proto_light);

      proto_light.ambient  := (0.6, 0.0, 0.0, 0.1);
      G3D.Define (6, proto_light);

      proto_light.ambient  := (0.0, 0.6, 0.0, 0.1);
      G3D.Define (7, proto_light);

      proto_light.ambient  := (0.0, 0.0, 0.6, 0.1);
      G3D.Define (8, proto_light);

      G3D.Switch_Lights (True);

      G3D.Switch_Light (2, False);
      G3D.Switch_Light (3, False);
      G3D.Switch_Light (4, False);
      G3D.Switch_Light (5, False);
      G3D.Switch_Light (6, False);
      G3D.Switch_Light (7, False);
      G3D.Switch_Light (8, False);

   end Prepare_Default_Lighting;

   procedure Clear_Modes is
   begin
     GL.Disable (GL.Blend);
     GL.Disable (GL.Lighting);
     GL.Disable (GL.Auto_Normal);
     GL.Disable (GL.Normalize);
     GL.Disable (GL.Depth_Test);
   end Clear_Modes;

   procedure Reset_for_3D (Self : in out Window'Class)
   is
   pragma Unreferenced (Self);
      use GL;
   begin
      --  (tbd: still needed ?) ...
      --  The matrix generated by GLU.Perspective is multipled by the current matrix
      MatrixMode (MODELVIEW);
      ShadeModel (SMOOTH);     -- GL's default is SMOOTH, vs FLAT

      ClearColor (0.0, 0.0, 0.0, 0.0);    -- Specifies clear values for color buffer(s)
      ClearAccum (0.0, 0.0, 0.0, 0.0);    -- Specifies clear values for the accumulation buffer
   end Reset_for_3D;

   procedure Enable_Viewport_and_Perspective (Self : in out Window'Class)
   --  tbd: move projection matrix to 'window resize'.
   is
      use GL;
   begin
      Viewport (0, 0, Self.main_size_x, Self.main_size_y);

      MatrixMode (PROJECTION);
      LoadIdentity;

      GLU.Perspective
        (fovy   => Self.Camera.FoVy,                 -- field of view angle (deg) in the y direction
         aspect => Self.Camera.Aspect,               -- x/y aspect ratio
         zNear  => Self.Camera.near_plane_Distance,  -- dist. from viewer to the near clipping plane
         zFar   => Self.Camera.far_plane_Distance);  -- dist. from viewer to the far clipping plane

      --  Get the current PROJECTION matrix from OpenGL:
      Get
        (GL.PROJECTION_MATRIX,
         Self.Camera.projection_matrix (1, 1)'Unchecked_Access);

      Self.Camera.projection_matrix := G3D.Math.Transpose (Self.Camera.projection_matrix);

      --  The matrix generated by GLU.Perspective is multipled by the current matrix:
      MatrixMode (MODELVIEW);
   end Enable_Viewport_and_Perspective;

   procedure Set_Size (Self : in out Window'Class;  width, height : Integer)
   is
      use GLOBE_3D, GL;    use G3D.REF;

      half_fov_max_rads        : Real;
      Tan_of_half_fov_max_rads : Real;

   begin
      Self.main_size_x := GL.Sizei (width);
      Self.main_size_y := GL.Sizei (height);

      Self.Camera.clipper.main_clipping.X1 := 0;
      Self.Camera.clipper.main_clipping.Y1 := 0;
      Self.Camera.clipper.main_clipping.X2 := width - 1;
      Self.Camera.clipper.main_clipping.Y2 := height - 1;

      Self.Camera.Aspect := GL.Double (Self.main_size_x) / GL.Double (Self.main_size_y);
      half_fov_max_rads        := 0.5 * Self.Camera.FoVy * deg2rad;

      Tan_of_half_fov_max_rads := Tan (half_fov_max_rads);

      Self.Camera.near_plane_Height := Self.Camera.near_plane_Distance * Tan_of_half_fov_max_rads;
      Self.Camera.near_plane_Width  := Self.Camera.near_plane_Height   * Self.Camera.Aspect;

      Self.Camera.far_plane_Height  := Self.Camera.far_plane_Distance * Tan_of_half_fov_max_rads;
      Self.Camera.far_plane_Width   := Self.Camera.far_plane_Height   * Self.Camera.Aspect;

      if Self.Camera.Aspect > 1.0 then  --  x side angle broader than y side angle
         half_fov_max_rads := Arctan (Self.Camera.Aspect * Tan_of_half_fov_max_rads);
      end if;

      Self.Camera.clipper.max_dot_product := Sin (half_fov_max_rads);

   end Set_Size;

   --  Procedures passed to GLUT:
   --   Window_Resize, Keyboard, Motion, Menu, Mouse, Display

   procedure Window_Resize (width, height : Integer)
   is
      the_Window : constant GLUT.Windows.Window_View := Current_Window;
   begin
      the_Window.forget_mouse := 5;
      Set_Size     (the_Window.all,  width, height);
      Reset_for_3D (the_Window.all);
   end Window_Resize;

   procedure Menu (value : Integer) is
   begin
      case value is
         when 1 =>
            GLUT.FullScreen;
            GLUT.SetCursor (GLUT.CURSOR_NONE);
            Current_Window.forget_mouse := 10;
            Current_Window.full_screen  := True;
         when 2 => null;
         when others => null;
      end case;
   end Menu;
   pragma Unreferenced (Menu);

   procedure Display_Status (Self : in out Window;
                             sec  : GLOBE_3D.Real)
   is
      use G3D, G3D.Aux;
      use type GL.Double;
      light_info : String (1 .. 8);
   begin
      GL.PushMatrix;

      GL.Disable (GL.Lighting);
      GL.Disable (GL.Texture_2D);

      GL.Color (red_value   => 0.7,
                green_value => 0.7,
                blue_value  => 0.6);

      GLUT_2D.Text_Output ((1.0, 0.0, 0.0),  "(x)",  GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_Output ((0.0, 1.0, 0.0),  "(y)",  GLUT_2D.Times_Roman_24);
      GLUT_2D.Text_Output ((0.0, 0.0, 1.0),  "(z)",  GLUT_2D.Times_Roman_24);

      GLUT_2D.Text_Output (0,  50,  Self.main_size_x,  Self.main_size_y,
                           "Eye: " & Coords (Self.Camera.clipper.eye_position),
                           GLUT_2D.Helvetica_10);

      GLUT_2D.Text_Output (0,  60,  Self.main_size_x,  Self.main_size_y,
                           "View direction: " & Coords (Self.Camera.clipper.view_direction),
                           GLUT_2D.Helvetica_10);

      for i in light_info'Range loop

         if Is_light_switched (i) then
            light_info (i) := Character'Val (Character'Pos ('0') + i);
         else
            light_info (i) := 'x';
         end if;
      end loop;

      GLUT_2D.Text_Output
        (0, 70,
         Self.main_size_x, Self.main_size_y,
         "Lights: (" & light_info & ')', GLUT_2D.Helvetica_10);

      if sec > 0.0 then
         GLUT_2D.Text_Output
           (0, 130, Self.main_size_x, Self.main_size_y,
            "FPS: " & Integer'Image (Integer (1.0 / sec)), GLUT_2D.Helvetica_10);
      end if;

      if Self.is_capturing_Video then
         GLUT_2D.Text_Output
           (0, 150,
            Self.main_size_x, Self.main_size_y,
            "*recording*", GLUT_2D.Helvetica_10);
      end if;

      declare
         use Status_Line_Vectors;
         C : Status_Line_Vectors.Cursor := Self.extra_Status.First;
         L : Status_Line;
      begin
         while Has_Element (C)
         loop
            L := Element (C);
            GLUT_2D.Text_Output (L.X, L.Y,
                                 Self.main_size_x, Self.main_size_y,
                                 To_String (L.Text),
                                 GLUT_2D.Helvetica_10);
            Next (C);
         end loop;

         Self.extra_Status.Clear;
      end;

      GL.PopMatrix;

   end Display_Status;

   function Frames_Per_Second (Self : in Window) return Float
   is
      use type GL.Double;
   begin
       return Float (1.0 / (Self.average * 0.001));
   end Frames_Per_Second;

   procedure Graphic_display (Self   : in out Window'Class;
                              Extras : in     GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array := GLOBE_3D.Skinned_Visuals.null_Visuals)
   is
      use GL, G3D.Skinned_Visuals;
   begin
      if Self.rend = null then
         raise Program_Error with
            "You need to define a renderer with Self.Set_renderer(My_Renderer'Access)";
      else
         Self.rend (Self.Objects (1 .. Self.object_Count)  &  Extras, Self.Camera);
      end if;

      if Self.show_Status then
         Display_Status (Self,  Self.average * 0.001);
      end if;

   end Graphic_display;

   procedure Fill_screen (Self   : in out Window'Class;
                          Extras : in     GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array := GLOBE_3D.Skinned_Visuals.null_Visuals)
   is
      use GL;

      procedure Display
      is
      begin
         Graphic_display (Self, Extras);
      end Display;

      package SAA is new GLOBE_3D.Software_Anti_Aliasing (Display);
   begin

      case Self.Smoothing is

      when software =>
        SAA.Set_Quality (SAA.Q3);
        for SAA_Phase in 1 .. SAA.Anti_Alias_Phases loop
          SAA.Display_with_Anti_Aliasing (SAA_Phase);
        end loop;

      when hardware =>
        Enable (MULTISAMPLE_ARB); -- (if not done yet)

        --  ClearColor (0.0, 0.0, 0.0, 1.0);    -- Specifies clear values for color buffer(s)
        --  ClearColor (0.15, 0.4, 0.15, 1.0);    -- Specifies clear values for color buffer(s)
            --  tbd: make clear color user-settable
        ClearColor (0.0, 0.0, 0.0, 1.0);    -- Specifies clear values for color buffer(s)
            --  tbd: make clear color user-settable
        ClearAccum (0.0,  0.0, 0.0,  0.0);    -- Specifies clear values for the accumulation buffer

        Graphic_display (Self, Extras);
        Flush;

      when none =>
        Graphic_display (Self, Extras);
        Flush;
    end case;

    GLUT.SwapBuffers;
  end Fill_screen;

   procedure Reset_eye  (Self : in out Window'Class) is
   begin
      Self.Camera.clipper.eye_position := (0.0,  5.0,  4.0);
      Self.Camera.world_rotation       := GLOBE_3D.Id_33;
   end Reset_eye;

   function Image (Date : Ada.Calendar.Time) return String;
   --  Proxy for Ada 2005 Ada.Calendar.Formatting.Image

   procedure Main_Operations (Self      : access Window;
                              time_Step :        G3D.Real;
                              Extras    : in     GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array := GLOBE_3D.Skinned_Visuals.null_Visuals)
   is
      use GL, G3D, G3DM, G3D.REF, Game_Control;

      elaps, time_now    : Integer;
      gx,    gy          : GL.Double;   -- mouse movement since last call
      seconds            : GL.Double;   -- seconds since last image
      alpha_correct      : Boolean;
      attenu_t, attenu_r : Real;

   begin
      if        not Self.is_Visible
        or else Self.is_Closed
      then
         return;
      end if;

      Enable_Viewport_and_Perspective (Self.all);
      --  nb: must be done prior to setting frustum planes (when using gl.frustums.current_Planes)

      --  Control of lighting
      --
--        self.frontal_light.position := (GL.Float (self.Camera.Clipper.eye_Position (0)),
--                                              GL.Float (self.Camera.Clipper.eye_Position (1)),
--                                              GL.Float (self.Camera.Clipper.eye_Position (2)),
--                                              1.0);
--        G3D.Define (1, self.frontal_light);

      for c in n1 .. n8 loop
         if Self.game_command (c) then
            Reverse_Light_Switch (1 + Command'Pos (c) - Command'Pos (n1));
         end if;
      end loop;

      --  Display screen
      --
      Fill_screen (Self.all, Extras);

      --  Timer management
      --
      time_now := GLUT.Get (GLUT.ELAPSED_TIME);   -- Number of milliseconds since GLUT.Init

      if Self.new_scene then
         Self.new_scene := False;
         elaps          := 0;
      else
         elaps          := time_now - Self.last_time;
      end if;

      Self.last_time := time_now;
      Self.average   := 0.0;

      for i in reverse Self.sample'First + 1 .. Self.sample'Last loop
         Self.sample (i) := Self.sample (i - 1);
         Self.average    := Self.average + Real (Self.sample (i));
      end loop;

      Self.sample (Self.sample'First) := elaps;

      Self.average := Self.average + Real (elaps);
      Self.average := Self.average / Real (Self.sample'Length);

      seconds  := Real (elaps) * 0.001;
      attenu_t := Real'Min (0.96, Real'Max (0.04,  1.0 - seconds * 4.0));
      attenu_r := attenu_t ** 0.5;

      --  Game control management
      --
      Self.game_command := no_command;

      Game_Control.Append_Commands (size_x     => Integer (Self.main_size_x),
                                    size_y     => Integer (Self.main_size_y),
                                    warp_mouse => Self.full_screen,
                                    c          => Self.game_command,
                                    gx         => gx,
                                    gy         => gy,
                                    keyboard   => Self.Keyboard'Access,
                                    mouse      => Self.Mouse'Access);

      if Self.forget_mouse > 0 then  --  mouse coords disturbed by resize
         gx := 0.0;
         gy := 0.0;
         Self.forget_mouse := Self.forget_mouse - 1;
      end if;

      if Self.game_command (interrupt_game) then
         null; -- GLUT_exit;                     --  tbd: how to handle this best ?
      end if;

      alpha_correct := False;

      if Self.game_command (special_plus)  then
        Self.alpha := Self.alpha + seconds;
        alpha_correct := True;
      end if;

      if Self.game_command (special_minus) then
        Self.alpha := Self.alpha - seconds;
        alpha_correct := True;
      end if;

      if alpha_correct then
         if    Self.alpha < 0.0 then Self.alpha := 0.0;
         elsif Self.alpha > 1.0 then Self.alpha := 1.0; end if;

         for Each in 1 .. Self.object_Count loop
            Self.Objects (Each).Set_Alpha (Self.alpha);
         end loop;
      end if;

      --  Camera/Eye - nb: camera movement is done after rendering, so
      --                   camera is in a state ready for the next frame.
      --            -     (important for Impostors)

      --  Rotating the eye

      Actors.Rotation (Self.Camera,
                        gc => Self.game_command,
                        gx => gx,
                        gy => gy,
                        unitary_change => seconds,
                        deceleration   => attenu_r,
                        time_step      => time_Step);

      --  Moving the eye

      Actors.Translation (Self.Camera,
                          gc => Self.game_command,
                          gx => gx,
                          gy => gy,
                          unitary_change     => seconds,
                          deceleration       => attenu_t,
                          time_step          => time_Step);

      if Self.game_command (n0) then
         Reset_eye (Self.all);
      end if;

      Self.Camera.clipper.view_direction :=
        Transpose (Self.Camera.world_rotation) * (0.0, 0.0, -1.0);

      --  update camera frustum
      --
      MatrixMode (MODELVIEW);
      Set_GL_Matrix (Self.Camera.world_rotation);
      Translate
        (-Self.Camera.clipper.eye_position (0),
         -Self.Camera.clipper.eye_position (1),
         -Self.Camera.clipper.eye_position (2));

      --  video management
      --
      if Self.game_command (video) then
         if Self.is_capturing_Video then
            GL.IO.Stop_Capture;
            Self.is_capturing_Video := False;
         else
            GL.IO.Start_Capture
              (AVI_name   => To_String (Self.Name) & "." & Image (Ada.Calendar.Clock) & ".avi",
               frame_rate => 8); -- Integer (self.Frames_per_second));
            Self.is_capturing_Video := True;
         end if;
      end if;

      if Self.is_capturing_Video then
         GL.IO.Capture_Frame;
      end if;

      --  photo management
      --
      if Self.game_command (photo) then
         GL.IO.Screenshot
           (name => To_String (Self.Name) & "." & Image (Ada.Calendar.Clock) & ".bmp");
      end if;

   end Main_Operations;

   procedure Close_Window
   is
   begin
      Current_Window.is_Closed := True;
   end Close_Window;

   procedure Update_Visibility (State : Integer)
   is
   begin
      --  ada.text_io.put_line ("in update_Visibility callback state: " & integer'image( State));
      --
      --  tbd: this callback is not being called when a window is iconicised !!

      Current_Window.is_Visible := not (State = GLUT.HIDDEN
                                        or else State = GLUT.FULLY_COVERED);
   end Update_Visibility;

   procedure Null_Display_Func
   is
   begin
      null;
   end Null_Display_Func;

   procedure Start_GLUTs (Self : in out Window)
   is
      function to_Address is new Ada.Unchecked_Conversion (p_Window, System.Address);

      GLUT_options : GLUT.Unsigned := GLUT.DOUBLE  or  GLUT.RGBA or GLUT.ALPHA  or  GLUT.DEPTH;
   begin
      if Self.Smoothing = hardware then
         GLUT_options := GLUT_options or GLUT.MULTISAMPLE;
      end if;

      InitDisplayMode (GLUT_options);

      Set_Size (Self,  500, 400);

      InitWindowSize     (Integer (Self.main_size_x),  Integer (Self.main_size_y));
      InitWindowPosition (120, 120);

      Self.glut_Window := CreateWindow ("GLOBE_3D/GLUT Window");

      if Self.glut_Window = 0 then
         raise GLUT_Problem;
      end if;

      GLUT.CloseFunc        (Close_Window'Access);
      GLUT.ReshapeFunc      (Window_Resize'Access);
      GLUT.DisplayFunc      (Null_Display_Func'Access);
      GLUT.WindowStatusFunc (Update_Visibility'Access);
      GLUT.SetWindowData    (to_Address (Window'Class (Self)'Unchecked_Access));

      GLUT.Devices.Initialize;

--        if CreateMenu (Menu'access) = 0 then         -- tdb: deferred
--           raise GLUT_Problem;
--        end if;

--      AttachMenu( MIDDLE_BUTTON );

--      AddMenuEntry(" * Full Screen", 1);
--      AddMenuEntry("--> Exit (Esc)", 2);

   end Start_GLUTs;

   procedure Start_GLs (Self : in out Window)
   is
      fog_colour : GL.Light_Float_Vector := (0.2, 0.2, 0.2, 0.1);
   begin

      Clear_Modes;
      Prepare_Default_Lighting (Self, 0.9);

      if Self.foggy then
         GL.Enable (GL.Fog);
         GL.Fogfv  (GL.FOG_COLOR,   fog_colour (0)'Unchecked_Access);
         GL.Fogf   (GL.FOG_DENSITY, 0.02);
      end if;

      Reset_for_3D (Self);

      if Self.Smoothing = hardware then
         GL.Enable (GL.MULTISAMPLE_ARB);
         GL.Enable (GL.SAMPLE_COVERAGE_ARB);  --  Hope it helps switching on the AA...
      end if;

   end Start_GLs;

   procedure Initialize
   is
   begin
      GLUT.Init;
      GLUT.SetOption (GLUT.GLUT_RENDERING_CONTEXT, GLUT.GLUT_USE_CURRENT_CONTEXT);
      GLUT.SetOption (GLUT.ACTION_ON_WINDOW_CLOSE, ACTION_CONTINUE_EXECUTION);
   end Initialize;

   procedure Define (Self : in out Window)
   is
   begin
      Start_GLUTs (Self);    -- Initialize the GLUT things
      Start_GLs   (Self);    -- Initialize the (Open)GL things
      Reset_eye   (Self);

      Freshen     (Self, 0.02);    -- do an initial freshen, to initialise Camera, etc.
   end Define;

   procedure Destroy (Self : in out Window)
   is
   begin
      DestroyWindow (Self.glut_Window);
   end Destroy;

   procedure Enable (Self : in out Window)
   is
   begin
      GLUT.SetWindow  (Self.glut_Window);
--      opengl.glx.glXMakeCurrent;

   end Enable;

   procedure Set_Renderer (Self : in out Window; Renderer : Renderer_Access) is
   begin
     Self.rend := Renderer;
   end Set_Renderer;

   procedure Freshen (Self      : in out Window;
                      Time_Step : in     G3D.Real;
                      Extras    : in     GLOBE_3D.Skinned_Visuals.Skinned_Visual_Array := GLOBE_3D.Skinned_Visuals.null_Visuals)
   is
   begin
      Enable (Self);  -- for multi-window operation.
      Main_Operations (Self'Access, Time_Step, Extras);
   end Freshen;

   --  traits
   --

   function Smoothing (Self : in     Window) return Smoothing_method
   is
   begin
      return Self.Smoothing;
   end Smoothing;

   procedure Smoothing_is (Self : in out Window;
                           Now  : in Smoothing_method)
   is
   begin
      Self.Smoothing := Now;
   end Smoothing_is;

   procedure Add (Self : in out Window;   the_Object : in GLOBE_3D.Skinned_Visuals.p_Skinned_Visual)
   is
   begin
      Self.object_Count                := Self.object_Count + 1;
      Self.Objects (Self.object_Count) := the_Object.all'Access;
   end Add;

   procedure Rid (Self : in out Window;   the_Object : in GLOBE_3D.Skinned_Visuals.p_Skinned_Visual)
   is
      use G3D.Skinned_Visuals;
   begin
      for Each in 1 .. Self.object_Count loop

         if Self.Objects (Each) = the_Object then

            if Each /= Self.object_Count then
               Self.Objects (Each .. Self.object_Count - 1) :=
                 Self.Objects (Each + 1 .. Self.object_Count);
            end if;

            Self.object_Count := Self.object_Count - 1;
            return;
         end if;

      end loop;

      raise no_such_Object;
   end Rid;

   function Object_Count (Self : in Window) return Natural
   is
   begin
      return Self.object_Count;
   end Object_Count;

   --  status display
   --

   procedure Add_Status_Line (Self : in out Window;   Text : in String;
                                                      X, Y : in Integer)
   is
   begin
      Self.extra_Status.Append (
         New_Item => Status_Line'(Text => To_Unbounded_String (Text),
                                  X    => GL.Int (X),
                                  Y    => GL.Int (Y)));
   end Add_Status_Line;

   function  Show_Status (Self : in     Window) return Boolean
   is
   begin
      return Self.show_Status;
   end Show_Status;

   procedure Show_Status (Self : in out Window;
                          Show : in     Boolean := True)
   is
   begin
      Self.show_Status := Show;
   end Show_Status;

   --  Devices
   --

   function Keyboard (Self : access Window'Class) return Devices.p_Keyboard
   is
   begin
      return Self.Keyboard'Unchecked_Access;
   end Keyboard;

   function Mouse (Self : access Window'Class) return Devices.p_Mouse
   is
   begin
      return Self.Mouse'Access;
   end Mouse;

  --  Proxy for Ada 2005 Ada.Calendar.Formatting.Image
  function Image (Date : Ada.Calendar.Time) return String
  is
    use Ada.Calendar;
    subtype Sec_Int is Long_Integer;  --  must contain 86_400
    m, s : Sec_Int;
  begin
    s := Sec_Int (Seconds (Date));
    m := s / 60;

    declare
      --  + 100: trick for obtaining 0x
      sY : constant String := Integer'Image (Year (Date));
      sM : constant String := Integer'Image (Month (Date) + 100);
      sD : constant String := Integer'Image (Day (Date)  + 100);
      shr : constant String := Sec_Int'Image (m  /  60 + 100);
      smn : constant String := Sec_Int'Image (m mod 60 + 100);
      ssc : constant String := Sec_Int'Image (s mod 60 + 100);

    begin
      return
        sY (sY'Last - 3 .. sY'Last) & '-' &  -- not Year 10'000 compliant.
        sM (sM'Last - 1 .. sM'Last) & '-' &
        sD (sD'Last - 1 .. sD'Last) &
        " " &
        shr (shr'Last - 1 .. shr'Last) & '.' &
        smn (smn'Last - 1 .. smn'Last) & '.' &
        ssc (ssc'Last - 1 .. ssc'Last);
    end;
  end Image;

end GLUT.Windows;
