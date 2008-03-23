
with GL;

with glut.Internal;   use glut.Internal;
with glut.Platform;

with ada.Strings.unbounded;      use ada.Strings.unbounded;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Finalization;
with ada.Calendar;
with Ada.Environment_Variables;



package body GLUT is


   use interfaces.C;






   -- Sets the default display mode for all new windows
   --
   procedure InitDisplayMode (Mode : Unsigned)
   is
   begin
      fgState.DisplayMode := Mode;        -- We will make use of this value when creating a new OpenGL context.
   end;




   -- Sets the default initial window size for new windows
   --
   procedure InitWindowSize (Width : Integer; Height : Integer)
   is
   begin
      fgState.Size.X := width;
      fgState.Size.Y := height;

      if width > 0 and  height > 0 then
         fgState.Size.Used := True;
      else
         fgState.Size.Used := False;
      end if;
   end;




   -- Sets the default initial window position for new windows
   --
   procedure InitWindowPosition (X : Integer; Y : Integer)
   is
   begin
      fgState.Position.X := x;
      fgState.Position.Y := y;

      if x >= 0  and  y >= 0 then
         fgState.Position.Used := True;
      else
         fgState.Position.Used := False;
      end if;
   end;







   -- finalization - free Argv strings
   --
   -- RK 23-Oct-2006, to remove the memory leak in question.
   --

   type Argvz is array (0 .. 500) of aliased interfaces.c.strings.Chars_Ptr;

   type Arg_Type is new ada.finalization.Controlled with record
     v       : Argvz;
     v_Count : Natural;
   end record;




   procedure Finalize (Self : in out Arg_Type)
   is
      use interfaces.c.strings;
   begin
      free (Self.v (0));

      for I in 1 .. Self.v_Count loop
         free (Self.v (I));
      end loop;
   end Finalize;




   Arg : Arg_Type;



   procedure Glutinit (Argcp : access Integer;
      Argv : access Interfaces.C.Strings.Chars_Ptr);
   -- pragma Import (C, Glutinit, "glutInit", "glutInit"); -- APEX
   pragma Import (StdCall, GlutInit, "glutInit"); -- GNAT/OA


   -- Pure Ada method, from IBM / Rational Apex support:

   -- "This procedure may be a useful replacement when porting an
   --  Ada program written for Gnat, which imports argc and argv like this:
   --  argc : aliased integer;
   --  pragma Import (C, argc, "gnat_argc");
   --
   --  argv : chars_ptr_ptr;
   --  pragma Import (C, argv, "gnat_argv");
   -- "

   -- http://www-1.ibm.com/support/docview.wss?uid=swg21125019









   -- A call to this function should initialize all the display stuff.
   --
   procedure fghInitialize (display_Name : in String)
   is
   begin
      platform.fghInitialize (display_Name);
      fgState.Initialised := True;
   end;










   --   * Perform initialization. This usually happens on the program startup
   --   * and restarting after glutMainLoop termination...
   --
   procedure Init
   is
      use Ada.Command_Line;
      use Interfaces.C.Strings;

      Argc : aliased Integer := Argument_Count + 1;

      display_Name : unbounded_String;
   begin
      Arg.v_Count := Argument_Count;

      Arg.v (0) := New_String (Command_Name);
      for I in 1 .. Arg.v_Count loop
          Arg.v (I) := New_String (Argument (I));
      end loop;

      --Glutinit (Argc'Access, Arg.v (0)'Access);

      if fgState.Initialised then
         raise constraint_Error with "illegal glutInit reinitialization attempt";
      end if;

      fgState.ProgramName := to_unbounded_String (ada.command_line.command_Name);
      fgState.Time        := ada.calendar.Clock;                                        --  Get start time


      -- tbd: command line and environment variable parsing ... do later on
      --
      --
      --      char* displayName = NULL;
      --      char* geometry = NULL;
      --      int i, j, argc = *pargc;
      --
      --
      --      /* check if GLUT_FPS env var is set */
      --  #ifndef _WIN32_WCE
      --      {
      --          const char *fps = getenv( "GLUT_FPS" );
      --          if( fps )
      --          {
      --              int interval;
      --              sscanf( fps, "%d", &interval );
      --
      --              if( interval <= 0 )
      --                  fgState.FPSInterval = 5000;  /* 5000 millisecond default */
      --              else
      --                  fgState.FPSInterval = interval;
      --          }
      --      }

      display_Name := to_unbounded_String (ada.environment_variables.Value ("DISPLAY"));

      --      for( i = 1; i < argc; i++ )
      --      {
      --          if( strcmp( argv[ i ], "-display" ) == 0 )
      --          {
      --              if( ++i >= argc )
      --                  fgError( "-display parameter must be followed by display name" );
      --
      --              displayName = argv[ i ];
      --
      --              argv[ i - 1 ] = NULL;
      --              argv[ i     ] = NULL;
      --              ( *pargc ) -= 2;
      --          }
      --          else if( strcmp( argv[ i ], "-geometry" ) == 0 )
      --          {
      --              if( ++i >= argc )
      --                  fgError( "-geometry parameter must be followed by window "
      --                           "geometry settings" );
      --
      --              geometry = argv[ i ];
      --
      --              argv[ i - 1 ] = NULL;
      --              argv[ i     ] = NULL;
      --              ( *pargc ) -= 2;
      --          }
      --          else if( strcmp( argv[ i ], "-direct" ) == 0)
      --          {
      --              if( fgState.DirectContext == GLUT_FORCE_INDIRECT_CONTEXT )
      --                  fgError( "parameters ambiguity, -direct and -indirect "
      --                      "cannot be both specified" );
      --
      --              fgState.DirectContext = GLUT_FORCE_DIRECT_CONTEXT;
      --              argv[ i ] = NULL;
      --              ( *pargc )--;
      --          }
      --          else if( strcmp( argv[ i ], "-indirect" ) == 0 )
      --          {
      --              if( fgState.DirectContext == GLUT_FORCE_DIRECT_CONTEXT )
      --                  fgError( "parameters ambiguity, -direct and -indirect "
      --                      "cannot be both specified" );
      --
      --              fgState.DirectContext = GLUT_FORCE_INDIRECT_CONTEXT;
      --              argv[ i ] = NULL;
      --              (*pargc)--;
      --          }
      --          else if( strcmp( argv[ i ], "-iconic" ) == 0 )
      --          {
      --              fgState.ForceIconic = GL_TRUE;
      --              argv[ i ] = NULL;
      --              ( *pargc )--;
      --          }
      --          else if( strcmp( argv[ i ], "-gldebug" ) == 0 )
      --          {
      --              fgState.GLDebugSwitch = GL_TRUE;
      --              argv[ i ] = NULL;
      --              ( *pargc )--;
      --          }
      --          else if( strcmp( argv[ i ], "-sync" ) == 0 )
      --          {
      --              fgState.XSyncSwitch = GL_TRUE;
      --              argv[ i ] = NULL;
      --              ( *pargc )--;
      --          }
      --      }
      --
      --      /* Compact {argv}. */
      --      for( i = j = 1; i < *pargc; i++, j++ )
      --      {
      --          /* Guaranteed to end because there are "*pargc" arguments left */
      --          while ( argv[ j ] == NULL )
      --              j++;
      --          if ( i != j )
      --              argv[ i ] = argv[ j ];
      --      }
      --
      --  #endif /* _WIN32_WCE */


      --  Have the display created now. If there wasn't a "-display"
      --  in the program arguments, we will use the DISPLAY environment
      --  variable for opening the X display (see code above):
      --
      fghInitialize (to_String (display_Name));


      -- tbd: geometry parsing deferred til later ...

   end Init;


--  void FGAPIENTRY glutInit( int* pargc, char** argv )
--  {
--      /*
--       * Geometry parsing deffered until here because we may need the screen
--       * size.
--       */
--
--      if (geometry )
--      {
--          unsigned int parsedWidth, parsedHeight;
--          int mask = XParseGeometry( geometry,
--                                     &fgState.Position.X, &fgState.Position.Y,
--                                     &parsedWidth, &parsedHeight );
--          /* TODO: Check for overflow? */
--          fgState.Size.X = parsedWidth;
--          fgState.Size.Y = parsedHeight;
--
--          if( (mask & (WidthValue|HeightValue)) == (WidthValue|HeightValue) )
--              fgState.Size.Use = GL_TRUE;
--
--          if( mask & XNegative )
--              fgState.Position.X += fgDisplay.ScreenWidth - fgState.Size.X;
--
--          if( mask & YNegative )
--              fgState.Position.Y += fgDisplay.ScreenHeight - fgState.Size.Y;
--
--          if( (mask & (XValue|YValue)) == (XValue|YValue) )
--              fgState.Position.Use = GL_TRUE;
--      }
--  }



   procedure verify_Initialised
   is
   begin
      if not fgState.Initialised then
         raise constraint_Error with "GLUT has not been initialised.";
      end if;
   end;








   procedure fghDisplayAll (Windows : in window_List)
   is
      use Window_Lists;
      Cursor : Window_Lists.Cursor := First (Windows);
      Window : SFG_Window_view;
   begin

      while has_Element (Cursor) loop
         Window := Element (Cursor);

         if    window.State.Redisplay
           and window.State.Visible
         then
            window.State.Redisplay := False;
            platform.fghDisplayAll_RedrawWindow (window);
         end if;

         fghDisplayAll (Window.Children);     -- recurse through descendants

         next (Cursor);
      end loop;

   end;









   function fgWindowByID (Windows   : in window_List;
                          Window_Id : in Integer) return SFG_Window_view
   is
      use Window_Lists;
      Cursor : Window_Lists.Cursor := First (Windows);
      Window : SFG_Window_view;
   begin

      while has_Element (Cursor) loop
         Window := Element (Cursor);

         if Window.Id = Window_Id then
            return Window;
         else
            Window := fgWindowByID (Window.Children, Window_Id);   -- recurse through children.

            if Window /= null then
               return Window;
            end if;
         end if;

         next (Cursor);
      end loop;

      return null;
   end;





   -- This function selects the current window
   --
   procedure SetWindow (Win : Integer)
   is
   begin
      verify_Initialised;

      if         fgStructure.CurrentWindow   /= null
        and then fgStructure.CurrentWindow.ID = Win
      then
         return;
      end if;


      declare
         Window : SFG_Window_view := fgWindowByID (fgStructure.Windows, Win);
      begin
         if Window = null then
            raise constraint_Error with "glutSetWindow(): window not found ~ ID:" & Integer'Image (Win);
         end if;

         fgSetWindow (window);
      end;
   end;





   -- Swaps the buffers for the current window (if any)
   --
   procedure SwapBuffers
   is
   begin
      verify_Initialised;

      --   "glXSwapBuffers" already performs an implicit call to "glFlush". What about "SwapBuffers"?
      --
      gl.Flush;

      if not fgStructure.CurrentWindow.Window.DoubleBuffered then
         return;
      end if;


      platform.SwapBuffers;

      -- tbd: 'GLUT_FPS env var' support deferred...
      --
      --      /* GLUT_FPS env var support */
      --      if( fgState.FPSInterval )
      --      {
      --          GLint t = glutGet( GLUT_ELAPSED_TIME );
      --          fgState.SwapCount++;
      --          if( fgState.SwapTime == 0 )
      --              fgState.SwapTime = t;
      --          else if( t - fgState.SwapTime > fgState.FPSInterval )
      --          {
      --              float time = 0.001f * ( t - fgState.SwapTime );
      --              float fps = ( float )fgState.SwapCount / time;
      --              fprintf( stderr,
      --                       "freeglut: %d frames in %.2f seconds = %.2f FPS\n",
      --                       fgState.SwapCount, time, fps );
      --              fgState.SwapTime = t;
      --              fgState.SwapCount = 0;
      --          }
      --      }

   end;






   --   Handle a window configuration change. When no reshape
   --   callback is hooked, the viewport size is updated to
   --   match the new window size.
   --
   procedure fghReshapeWindow (window : in SFG_Window_view;   width, height : in Integer)
   is
      current_window : SFG_Window_view := fgStructure.CurrentWindow;
   begin
      platform.fghReshapeWindow (window,  width, height);

      -- Should update {window->State.OldWidth, window->State.OldHeight}
      -- to keep in lockstep with POSIX_X11 code.
      --
      if window.callbacks.CB_Reshape /= null then
         window.callbacks.CB_Reshape (width, height);
      else
         fgSetWindow (window );
         gl.Viewport (0, 0,   gl.Sizei (width), gl.Sizei (height));
      end if;

      --      Force a window redraw.  In Windows at least this is only a partial
      --      solution:  if the window is increasing in size in either dimension,
      --      the already-drawn part does not get drawn again and things look funny.
      --      But without this we get this bad behaviour whenever we resize the
      --      window.
      --
      window.State.Redisplay := True;

      if window.IsMenu then
         fgSetWindow (current_window);
      end if;
   end;







   -- Calls a window's redraw method. This is used when
   -- a redraw is forced by the incoming window messages.
   --
   procedure fghRedrawWindow (Window : access internal.SFG_Window)
   is
      current_window : SFG_Window_view := fgStructure.CurrentWindow;
   begin
      if Window.Callbacks.CB_Display = null then
         return;
      end if;

      window.State.Redisplay := False;

      if not window.State.Visible then
         return;
      end if;

      fgSetWindow (window);

      if window.State.NeedToResize then
         fghReshapeWindow (window.all'access,
                           window.State.Width,
                           window.State.Height);

         window.State.NeedToResize := False;
      end if;

      window.callbacks.CB_Display.all;

      fgSetWindow (current_window);
   end;










   -- Executes a single iteration in the freeglut processing loop.
   --
   procedure MainLoopEvent
   is
   begin
      platform.MainLoopEvent;

      --        if fgState.Timers.First then         -- we shouldn't need timers.
      --           fghCheckTimers;
      --        end if;

      --       fghCheckJoystickPolls;    -- tbd: deferred

      fghDisplayAll (fgStructure.Windows);

      fgCloseWindows;
   end;




   function GetWindowData return system.Address
   is
   begin
      verify_Initialised;
      return fgStructure.CurrentWindow.UserData;
   end;









   --   * Function to add a window to the linked list of windows to destroy.
   --   * Subwindows are automatically added because they hang from the window
   --   * structure.
   --   */

   procedure fgAddToWindowDestroyList (window : in SFG_Window_view)
   is
      null_Callbacks : Callback_Set;
   begin
      fgStructure.WindowsToDestroy.append (window);

      if fgStructure.CurrentWindow = window then          -- Check if the window is the current one
         fgStructure.CurrentWindow := null;
      end if;


      null_Callbacks.CB_Destroy := window.Callbacks.CB_Destroy;  -- Clear all window callbacks except Destroy, which will
      window.Callbacks          := null_Callbacks;               -- be invoked later.  Right now, we are potentially carrying
                                                                 -- out a freeglut operation at the behest of a client callback,
                                                                 -- so we are reluctant to re-enter the client with the Destroy
                                                                 -- callback, right now.  The others are all wiped out, however,
                                                                 -- to ensure that they are no longer called after this point.
   end;








   -- Destroys a window and all of its subwindows
   --

   procedure DestroyWindow (Win : Integer)
   is
      Window : SFG_Window_view;
   begin
      verify_Initialised;

      Window := fgWindowByID (fgStructure.Windows, win);

      fgAddToWindowDestroyList (window);
   end;




   -- Returns various device information.
   --
   function DeviceGet (Type_Id : GL.enum) return Integer
   is
   begin
      verify_Initialised;

      --XXX WARNING: we are mostly lying in this function.

      case Type_Id is
         when GLUT.HAS_KEYBOARD =>
            -- Win32 is assumed a keyboard, and this cannot be queried, except for WindowsCE.
            --
            -- X11 has a core keyboard by definition, although it can be present as a virtual/dummy
            -- keyboard. For now, there is no reliable way to tell if a real keyboard is present.
            --
            return platform.DeviceGet_has_Keyboard;



         when GLUT.HAS_MOUSE =>   -- X11 has a mouse by definition
            return platform.DeviceGet_has_Mouse;


         when GLUT.NUM_MOUSE_BUTTONS =>   -- X11 has a mouse by definition
            return platform.DeviceGet_num_mouse_Buttons;


         when GLUT.HAS_JOYSTICK =>
            return 0;    --          return fgJoystickDetect ();   -- tbd: deferred


         when GLUT.OWNS_JOYSTICK =>
            return boolean'Pos (fgState.JoysticksInitialised);


         when GLUT.JOYSTICK_POLL_RATE =>
            if fgStructure.CurrentWindow /= null then
               return fgStructure.CurrentWindow.State.JoystickPollRate;
            else
               return 0;
            end if;


            -- tbd: deferred
            --
            --      /* XXX The following two are only for Joystick 0 but this is an improvement */
            --      case GLUT_JOYSTICK_BUTTONS:
            --          return glutJoystickGetNumButtons ( 0 );
            --
            --      case GLUT_JOYSTICK_AXES:
            --          return glutJoystickGetNumAxes ( 0 );
            --
            --      case GLUT_HAS_DIAL_AND_BUTTON_BOX:
            --          return fgInputDeviceDetect ();


         when GLUT.NUM_DIALS =>
            if fgState.InputDevsInitialised then
               return 8;
            else
               return 0;
            end if;


         when GLUT.NUM_BUTTON_BOX_BUTTONS =>
            return 0;


         when GLUT.HAS_SPACEBALL
            | GLUT.HAS_TABLET =>
            return 0;


         when  GLUT.NUM_SPACEBALL_BUTTONS
            | GLUT.NUM_TABLET_BUTTONS =>
            return 0;


         when GLUT.DEVICE_IGNORE_KEY_REPEAT =>
            if fgStructure.CurrentWindow /= null then
               return boolean'Pos (fgStructure.CurrentWindow.State.IgnoreKeyRepeat);
            else
               return 0;
            end if;


         when GLUT.DEVICE_KEY_REPEAT =>
            return fgState.KeyRepeat;


         when others =>
            raise program_Error with "glutDeviceGet(): missing enum handle:" & gl.enum'Image (Type_Id);
      end case;

   end;





   -- This should return the current state of ALT, SHIFT and CTRL keys.
   --
   function GetModifiers return Integer
   is
   begin
      verify_Initialised;

      if fgState.Modifiers = INVALID_MODIFIERS then
         raise constraint_Error with "glutGetModifiers() called outside an input callback";
      end if;

      return Integer (fgState.Modifiers);
   end;





   -- General settings assignment method
   --
   procedure SetOption (option_flag : Integer;   value : Integer)
   is
   begin
      verify_Initialised;

      --   XXX In chronological code add order.  (WHY in that order?)
      --
      case option_flag is
         when GLUT.INIT_WINDOW_X =>
            fgState.Position.X := value;

         when GLUT.INIT_WINDOW_Y =>
            fgState.Position.Y := value;

         when GLUT.INIT_WINDOW_WIDTH =>
            fgState.Size.X := value;

         when GLUT.INIT_WINDOW_HEIGHT =>
            fgState.Size.Y := value;

         when GLUT.INIT_DISPLAY_MODE =>
            fgState.DisplayMode := glut.unSigned (value);

         when GLUT.ACTION_ON_WINDOW_CLOSE =>
            fgState.ActionOnWindowClose := value;

         when GLUT.RENDERING_CONTEXT =>
            if value = GLUT.USE_CURRENT_CONTEXT then
               fgState.UseCurrentContext := True;
            else
               fgState.UseCurrentContext := False;
            end if;

         when GLUT.DIRECT_RENDERING =>
            fgState.DirectContext := value;

         when GLUT.WINDOW_CURSOR =>
            if fgStructure.CurrentWindow /= null  then
               fgStructure.CurrentWindow.State.Cursor := value;
            end if;

         when GLUT.AUX =>
            fgState.AuxiliaryBufferNumber := value;

         when GLUT.MULTISAMPLE =>
            fgState.SampleNumber := value;

         when others =>
            raise program_Error with "glutSetOption: missing enumeration ";
      end case;

   end;













   --  Opens a window. Requires a SFG_Window object created and attached
   --  to the freeglut structure. OpenGL context is created here.
   --
   procedure fgOpenWindow (window      : access SFG_Window;
                           title       : in     String;
                           positionUse : in     Boolean;
                           x, y        : in     Integer;
                           sizeUse     : in     Boolean;
                           w, h        : in     Integer;
                           gameMode    : in     Boolean;
                           isSubWindow : in     Boolean)
   is
   begin
      platform.fgOpenWindow (window, title,
                             positionUse,  x, y,
                             sizeUse,      w, h,
                             gameMode,
                             isSubWindow);

      fgSetWindow (window);

      window.Window.DoubleBuffered := (fgState.DisplayMode and GLUT.DOUBLE) /= 0;

      if not window.Window.DoubleBuffered then
         gl.DrawBuffer (GL.FRONT);
         gl.ReadBuffer (GL.FRONT);
      end if;

   end;














   --   * This private function creates, opens and adds to the hierarchy
   --   * a freeglut window complete with OpenGL context and stuff...
   --   *
   --   * If parent is set to NULL, the window created will be a topmost one.
   --
   function fgCreateWindow (parent      : in SFG_Window_view;
                            title       : in String;
                            positionUse : in Boolean;
                            x, y        : in Integer;
                            sizeUse     : in Boolean;
                            w, h        : in Integer;
                            gameMode    : in Boolean;
                            isMenu      : in Boolean) return SFG_Window_view
   is
      use window_Lists;
      the_Window : SFG_Window_view := new SFG_Window;         -- Have the window object created
   begin
      --  Initialize the object properties
      --
      fgStructure.WindowID := fgStructure.WindowID + 1;
      the_window.ID        := fgStructure.WindowID;


      if parent /= null then
         append (parent.Children, the_Window);              --  fgListAppend ( &parent->Children, &window->Node);
         the_window.Parent := Parent;
      else
         append (fgStructure.Windows, the_window);          -- fgListAppend (&fgStructure.Windows, &window->Node);
      end if;


      -- Set the default mouse cursor and reset the modifiers value
      --
      the_window.State.Cursor := GLUT.CURSOR_INHERIT;

      the_window.IsMenu := isMenu;

      the_window.State.IgnoreKeyRepeat := False;
      the_window.State.KeyRepeating    := False;

      -- Open the window now. The fgOpenWindow() function is system
      -- dependant, and resides in freeglut_window.c. Uses fgState.
      --
      fgOpenWindow (the_window, title,
                    positionUse, x, y,
                    sizeUse, w, h,
                    gameMode,
                    parent /= null);

      return the_Window;
   end;








   -- Creates a new top-level freeglut window
   --
   function CreateWindow (Title : String) return Integer is
      Result : Integer;
   begin
      --       XXX GLUT does not exit; it simply calls "glutInit" quietly if the
      --       XXX application has not already done so.  The "freeglut" community
      --       XXX decided not to go this route (freeglut-developer e-mail from
      --       XXX Steve Baker, 12/16/04, 4:22 PM CST, "Re: [Freeglut-developer]
      --       XXX Desired 'freeglut' behaviour when there is no current window"
      --
      verify_Initialised;

      Result := fgCreateWindow (null, title,
                                fgState.Position.Used, fgState.Position.X, fgState.Position.Y,
                                fgState.Size.Used,     fgState.Size.X,     fgState.Size.Y,
                                False, False).ID;

      return Result;
   end CreateWindow;





   procedure CloseFunc (Callback : Glut_Proc_2)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Destroy := SFG_Proc (Callback);
   end;


   procedure ReshapeFunc (Callback : Glut_Proc_3)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Reshape := Callback;
   end;



   procedure WindowStatusFunc (Callback : Glut_Proc_23)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_WindowStatus := Callback;
   end;


   procedure KeyboardFunc (Callback : Glut_Proc_4)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Keyboard := Callback;
   end;



   procedure KeyboardUpFunc (Callback : Glut_KeyUpFunc)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_KeyboardUp := Callback;
   end;



   procedure SpecialFunc (Callback : Glut_Proc_13)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Special := Callback;
   end;


   procedure SpecialUpFunc (Func : Glut_SpecialUp)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_SpecialUp := Func;
   end;



   procedure MouseFunc (Callback : Glut_Proc_5)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Mouse := Callback;
   end;



   procedure MotionFunc (Callback : Glut_Proc_6)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Motion := Callback;
   end;



   procedure PassiveMotionFunc (Callback : Glut_Proc_7)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Passive := Callback;
   end;






   -- This private function creates a menu and adds it to the menus list
   --
   function fgCreateMenu (menuCallback : in Glut_Proc_1) return access SFG_Menu
   is
   begin
      return null;   -- tbd:
   end;


--  SFG_Menu* fgCreateMenu( FGCBMenu menuCallback )
--  {
--      int x = 100, y = 100, w = 100, h = 100;
--      SFG_Window *current_window = fgStructure.CurrentWindow;
--
--      /* Have the menu object created */
--      SFG_Menu* menu = (SFG_Menu *)calloc( sizeof(SFG_Menu), 1 );
--
--      menu->ParentWindow = NULL;
--
--      /* Create a window for the menu to reside in. */
--
--      fgCreateWindow( NULL, "freeglut menu", GL_TRUE, x, y, GL_TRUE, w, h,
--                      GL_FALSE, GL_TRUE );
--      menu->Window = fgStructure.CurrentWindow;
--      glutDisplayFunc( fgDisplayMenu );
--
--      glutHideWindow( );  /* Hide the window for now */
--      fgSetWindow( current_window );
--
--      /* Initialize the object properties: */
--      menu->ID       = ++fgStructure.MenuID;
--      menu->Callback = menuCallback;
--      menu->ActiveEntry = NULL;
--
--      fgListInit( &menu->Entries );
--      fgListAppend( &fgStructure.Menus, &menu->Node );
--
--      /* Newly created menus implicitly become current ones */
--      fgStructure.CurrentMenu = menu;
--
--      return menu;
--  }






   -- Creates a new menu object, adding it to the freeglut structure
   --
   function CreateMenu (Callback : Glut_Proc_1) return Integer
   is
   begin
      verify_Initialised;
      return fgCreateMenu (callback).ID;
   end;




   procedure SetWindowData (Data : system.Address)
   is
   begin
      verify_Initialised;
      fgStructure.CurrentWindow.UserData := data;
   end;



   -- Control the auto-repeat of keystrokes to the current window
   --
   procedure IgnoreKeyRepeat (Ignore : Integer)
   is
   begin
      verify_Initialised;
      fgStructure.CurrentWindow.State.IgnoreKeyRepeat := ignore /= 0;
   end;






   procedure InitDisplayString (Name : String) is
      C_Name : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Name);
   begin
      InitDisplayString (C_Name);
      Interfaces.C.Strings.Free (C_Name);
   end InitDisplayString;

   procedure SetWindowTitle (Title : String) is
      C_Title : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Title);
   begin
      SetWindowTitle (C_Title);
      Interfaces.C.Strings.Free (C_Title);
   end SetWindowTitle;

   procedure SetIconTitle (Title : String) is
      C_Title : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Title);
   begin
      SetIconTitle (C_Title);
      Interfaces.C.Strings.Free (C_Title);
   end SetIconTitle;

   procedure AddMenuEntry (Label : String; Value : Integer) is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      AddMenuEntry (C_Label, Value);
      Interfaces.C.Strings.Free (C_Label);
   end AddMenuEntry;

   procedure AddSubMenu (Label : String; Submenu : Integer) is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      AddSubMenu (C_Label, Submenu);
      Interfaces.C.Strings.Free (C_Label);
   end AddSubMenu;

   procedure ChangeToMenuEntry
     (Item  : Integer;
      Label : String;
      Value : Integer)
   is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      ChangeToMenuEntry (Item, C_Label, Value);
      Interfaces.C.Strings.Free (C_Label);
   end ChangeToMenuEntry;

   procedure ChangeToSubMenu
     (Item    : Integer;
      Label   : String;
      Submenu : Integer)
   is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      ChangeToSubMenu (Item, C_Label, Submenu);
      Interfaces.C.Strings.Free (C_Label);
   end ChangeToSubMenu;

   function ExtensionSupported (Name : String) return Integer is
      Result : Integer;
      C_Name : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Name);
   begin
      Result := ExtensionSupported (C_Name);
      Interfaces.C.Strings.Free (C_Name);
      return Result;
   end ExtensionSupported;

   -----------------------------------------------------
   -- GdM 2005: callbacks with the 'Address attribute --
   -----------------------------------------------------

  -- This method is functionally identical as GNAT's Unrestricted_Access
  -- but has no type safety (cf GNAT Docs)

   function CreateMenu (P1 : System.Address) return Integer is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_1);
   begin
     return CreateMenu( Cvt(P1) );
   end CreateMenu;

   procedure DisplayFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_2);
   begin
     DisplayFunc( Cvt(P1) );
   end DisplayFunc;

   procedure ReshapeFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_3);
   begin
     ReshapeFunc( Cvt(P1) );
   end ReshapeFunc;

--     procedure KeyboardFunc (P1 : System.Address) is
--       function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_4);
--     begin
--       KeyboardFunc( Cvt(P1) );
--     end KeyboardFunc;

   procedure KeyboardUpFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_KeyUpFunc);
   begin
     KeyboardUpFunc( Cvt(P1) );
   end KeyboardUpFunc;

   procedure MouseFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_5);
   begin
     MouseFunc( Cvt(P1) );
   end MouseFunc;

   procedure MotionFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_6);
   begin
     MotionFunc( Cvt(P1) );
   end MotionFunc;

   procedure PassiveMotionFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_7);
   begin
     PassiveMotionFunc( Cvt(P1) );
   end PassiveMotionFunc;

   procedure IdleFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_10);
   begin
     IdleFunc( Cvt(P1) );
   end IdleFunc;

   procedure SpecialFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_13);
   begin
     SpecialFunc( Cvt(P1) );
   end SpecialFunc;

   procedure SpecialUpFunc (Func : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_SpecialUp);
   begin
     SpecialUpFunc( Cvt(Func) );
   end SpecialUpFunc;

end GLUT;
