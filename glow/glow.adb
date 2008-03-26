
with GL;

with glow.Internal;   use glow.Internal;
with glow.Platform;

with ada.Strings.unbounded;      use ada.Strings.unbounded;
with ada.Strings.fixed;
with ada.Strings.Maps;           use ada.Strings.Maps;
with ada.characters.latin_1;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Finalization;
with ada.Calendar;
with Ada.Environment_Variables;

with interfaces.C.Strings;   use interfaces.C.Strings;



package body Glow is


   package C renames interfaces.C;
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
         fgSetWindow (window);
         window.callbacks.CB_Reshape (width, height);
      else
         fgSetWindow (window);
         gl.Viewport (0, 0,   gl.Sizei (width),  gl.Sizei (height));
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

      fgSetWindow (window);
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





   --This function returns the ID number of the current window, 0 if none exists
   --
   function GetWindow return Integer
   is
      Win : SFG_Window_view := fgStructure.CurrentWindow;
   begin
      -- Since GLUT did not throw an error if this function was called without a prior call to
      -- "glutInit", this function shouldn't do so here.  Instead let us return a zero.
      -- See Feature Request "[ 1307049 ] glutInit check".
      --
      if not fgState.Initialised then
         return 0;
      end if;


      while win /= null  and then  win.IsMenu loop
        win := win.Parent;
      end loop;


      if win /= null then
         return win.ID;
      else
         return 0;
      end if;
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





   -- Marks the current window to have the redisplay performed when possible...
   --
   procedure PostRedisplay
   is
   begin
      verify_Initialised;

      fgStructure.CurrentWindow.State.Redisplay := True;
   end;














   -- General settings query method
   --

   function Get (Type_Id : GL.enum) return Integer
   is
      nsamples : Integer := 0;
   begin

      case Type_Id is
         when glow.INIT_STATE =>
            return boolean'Pos (fgState.Initialised);

         when glow.ELAPSED_TIME =>
            return Integer (fgElapsedTime);


         when glow.SCREEN_WIDTH =>
            return fgDisplay.ScreenWidth;

         when glow.SCREEN_HEIGHT =>
            return fgDisplay.ScreenHeight;

         when glow.SCREEN_WIDTH_MM =>
            return fgDisplay.ScreenWidthMM;

         when glow.SCREEN_HEIGHT_MM =>
            return fgDisplay.ScreenHeightMM;

         when glow.INIT_WINDOW_X =>
            if fgState.Position.Used then
               return fgState.Position.X;
            else
               return -1;
            end if;

         when glow.INIT_WINDOW_Y =>
            if fgState.Position.Used then
               return fgState.Position.Y;
            else
               return -1;
            end if;

         when glow.INIT_WINDOW_WIDTH =>
            if fgState.Size.Used then
               return fgState.Size.X;
            else
               return  -1;
            end if;

         when glow.INIT_WINDOW_HEIGHT =>
            if fgState.Size.Used then
               return fgState.Size.Y;
            else
               return -1;
            end if;

         when glow.INIT_DISPLAY_MODE =>
            return Integer (fgState.DisplayMode);


        when glow.WINDOW_PARENT =>
            if fgStructure.CurrentWindow = null then
               return 0;
            end if;

            if fgStructure.CurrentWindow.Parent = null then
               return 0;
            end if;

            return fgStructure.CurrentWindow.Parent.ID;


         when glow.WINDOW_NUM_CHILDREN =>
            if fgStructure.CurrentWindow = null then
               return 0;
            end if;

            return Integer (fgStructure.CurrentWindow.Children.length);


         when glow.WINDOW_CURSOR =>
            if fgStructure.CurrentWindow = null then
               return 0;
            end if;

            return fgStructure.CurrentWindow.State.Cursor;


         when glow.MENU_NUM_ITEMS =>
            if fgStructure.CurrentMenu = NULL then
               return 0;
            end if;

            return 0; -- tbd: deferred ...    fgStructure.CurrentMenu.Entries.Length;


         when glow.ACTION_ON_WINDOW_CLOSE =>
            return fgState.ActionOnWindowClose;


--           when GLUT.VERSION  =>   -- tbd: deferred
--              return VERSION_MAJOR * 10000 + VERSION_MINOR * 100 + VERSION_PATCH;


         when glow.RENDERING_CONTEXT =>
            if fgState.UseCurrentContext then
               return glow.USE_CURRENT_CONTEXT;
            else
               return glow.CREATE_NEW_CONTEXT;
            end if;


         when glow.DIRECT_RENDERING =>
            return fgState.DirectContext;


--           when GLUT.FULL_SCREEN =>     -- tbd: deferred
--              return fghCheckFullScreen;


         when others =>
            return platform.Get (Type_Id);
      end case;


      return -1;
   end;













   -- Returns various device information.
   --
   function DeviceGet (Type_Id : GL.enum) return Integer
   is
   begin
      verify_Initialised;

      --XXX WARNING: we are mostly lying in this function.

      case Type_Id is
         when glow.HAS_KEYBOARD =>
            -- Win32 is assumed a keyboard, and this cannot be queried, except for WindowsCE.
            --
            -- X11 has a core keyboard by definition, although it can be present as a virtual/dummy
            -- keyboard. For now, there is no reliable way to tell if a real keyboard is present.
            --
            return platform.DeviceGet_has_Keyboard;



         when glow.HAS_MOUSE =>   -- X11 has a mouse by definition
            return platform.DeviceGet_has_Mouse;


         when glow.NUM_MOUSE_BUTTONS =>   -- X11 has a mouse by definition
            return platform.DeviceGet_num_mouse_Buttons;


         when glow.HAS_JOYSTICK =>
            return 0;    --          return fgJoystickDetect ();   -- tbd: deferred


         when glow.OWNS_JOYSTICK =>
            return boolean'Pos (fgState.JoysticksInitialised);


         when glow.JOYSTICK_POLL_RATE =>
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


         when glow.NUM_DIALS =>
            if fgState.InputDevsInitialised then
               return 8;
            else
               return 0;
            end if;


         when glow.NUM_BUTTON_BOX_BUTTONS =>
            return 0;


         when glow.HAS_SPACEBALL
            | glow.HAS_TABLET =>
            return 0;


         when  glow.NUM_SPACEBALL_BUTTONS
            | glow.NUM_TABLET_BUTTONS =>
            return 0;


         when glow.DEVICE_IGNORE_KEY_REPEAT =>
            if fgStructure.CurrentWindow /= null then
               return boolean'Pos (fgStructure.CurrentWindow.State.IgnoreKeyRepeat);
            else
               return 0;
            end if;


         when glow.DEVICE_KEY_REPEAT =>
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
         when glow.INIT_WINDOW_X =>
            fgState.Position.X := value;

         when glow.INIT_WINDOW_Y =>
            fgState.Position.Y := value;

         when glow.INIT_WINDOW_WIDTH =>
            fgState.Size.X := value;

         when glow.INIT_WINDOW_HEIGHT =>
            fgState.Size.Y := value;

         when glow.INIT_DISPLAY_MODE =>
            fgState.DisplayMode := glow.unSigned (value);

         when glow.ACTION_ON_WINDOW_CLOSE =>
            fgState.ActionOnWindowClose := value;

         when glow.RENDERING_CONTEXT =>
            if value = glow.USE_CURRENT_CONTEXT then
               fgState.UseCurrentContext := True;
            else
               fgState.UseCurrentContext := False;
            end if;

         when glow.DIRECT_RENDERING =>
            fgState.DirectContext := value;

         when glow.WINDOW_CURSOR =>
            if fgStructure.CurrentWindow /= null  then
               fgStructure.CurrentWindow.State.Cursor := value;
            end if;

         when glow.AUX =>
            fgState.AuxiliaryBufferNumber := value;

         when glow.MULTISAMPLE =>
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

      window.Window.DoubleBuffered := (fgState.DisplayMode and glow.DOUBLE) /= 0;

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
      the_window.State.Cursor := glow.CURSOR_INHERIT;

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



   procedure EntryFunc (Callback : Glut_Proc_8)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Entry := Callback;
   end;




   --   * Sets the Visibility callback for the current window.
   --   */
   procedure fghVisibility (Status : in Integer)
   is
      glut_status : Integer := glow.VISIBLE;
   begin
      if   glow.HIDDEN        = status
        or glow.FULLY_COVERED = status
      then
         glut_status := glow.NOT_VISIBLE;
      end if;

      fgSetWindow (fgStructure.CurrentWindow);
      fgStructure.CurrentWindow.callbacks.CB_Visibility (glut_Status);
--      INVOKE_WCB( *( fgStructure.CurrentWindow ), Visibility, ( glut_status ) );
   end;

--  static void fghVisibility( int status )
--  {
--      if( ( GLUT_HIDDEN == status )  || ( GLUT_FULLY_COVERED == status ) )
--          glut_status = GLUT_NOT_VISIBLE;
--      INVOKE_WCB( *( fgStructure.CurrentWindow ), Visibility, ( glut_status ) );
--  }




   procedure VisibilityFunc (Callback : Glut_Proc_9)
   is
   begin
      fgStructure.CurrentWindow.Callbacks.CB_Visibility := Callback;

      if callback /= null then
         glow.WindowStatusFunc (fghVisibility'access);
      else
         glow.WindowStatusFunc (null);
      end if;

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






   Tokens : array (Positive range <>) of unbounded_String
     := (to_unbounded_String ("alpha"),
         to_unbounded_String ("acca"),
         to_unbounded_String ("acc"),
         to_unbounded_String ("blue"),
         to_unbounded_String ("buffer"),
         to_unbounded_String ("conformant"),
         to_unbounded_String ("depth"),
         to_unbounded_String ("double"),
         to_unbounded_String ("green"),
         to_unbounded_String ("index"),
         to_unbounded_String ("num"),
         to_unbounded_String ("red"),
         to_unbounded_String ("rgba"),
         to_unbounded_String ("rgb"),
         to_unbounded_String ("luminance"),
         to_unbounded_String ("stencil"),
         to_unbounded_String ("single"),
         to_unbounded_String ("stereo"),
         to_unbounded_String ("samples"),
         to_unbounded_String ("slow"),
         to_unbounded_String ("win32pdf"),
         to_unbounded_String ("win32pfd"),
         to_unbounded_String ("xvisual"),
         to_unbounded_String ("xstaticgray"),
         to_unbounded_String ("xgrayscale"),
         to_unbounded_String ("xstaticcolor"),
         to_unbounded_String ("xpseudocolor"),
         to_unbounded_String ("xtruecolor"),
         to_unbounded_String ("xdirectcolor"),
         to_unbounded_String ("xstaticgrey"),
         to_unbounded_String ("xgreyscale"),
         to_unbounded_String ("xstaticcolour"),
         to_unbounded_String ("xpseudocolour"),
         to_unbounded_String ("xtruecolour"),
         to_unbounded_String ("xdirectcolour"),
         to_unbounded_String ("borderless"),
         to_unbounded_String ("aux"));




   procedure InitDisplayString (Name : String)
   is
      use ada.Strings.fixed,  ada.Strings.unbounded;
      glut_state_flag : c.Unsigned := 0;
      the_Token       : unbounded_String;
      Start           : Natural := Name'First;
      Last            : Natural;
   begin
      --       * Unpack a lot of options from a character string.  The options are
      --       * delimited by blanks or tabs.
      --
      Last := ada.strings.fixed.Index (Name, String'(" " & ada.characters.latin_1.HT),  from => Start);

      the_Token := to_unbounded_String (Name (Start .. Last - 2));

      loop
         declare
            use ada.Strings, ada.Strings.unbounded;
            First, Last : Natural;
         begin
            find_Token (the_Token, to_Set ("=<>~!"), outside,  First, Last);

            declare
               token_Index : Natural := 0;
               clean_Token : String  := Slice (the_Token, First, Last);
            begin
               for Each in Tokens'range loop                 -- find this tokens index
                  if clean_Token = Tokens (Each) then
                     token_Index := Each;
                     exit;
                  end if;
               end loop;


               case token_Index is
                  when 1 =>               -- "alpha":  Alpha color buffer precision in bits
                     glut_state_flag := glut_state_flag or glow.ALPHA;    --  /* Somebody fix this for me!

                  when 2 => --  /* "acca":  Red, green, blue, and alpha accumulation buffer  precision in bits */
                     null;

                  when 3 => --  /* "acc":  Red, green, and blue accumulation buffer precision in bits with zero bits alpha */
                     glut_state_flag := glut_state_flag or glow.ACCUM;     -- /* Somebody fix this for me!

                  when 4 => --  /* "blue":  Blue color buffer precision in bits */
                     null;

                  when 5 => --  /* "buffer":  Number of bits in the color index color buffer */
                     null;

                  when 6 => --  /* "conformant":  Boolean indicating if the frame buffer configuration is conformant or not */
                     null;

                  when 7 => -- /* "depth":  Number of bits of precsion in the depth buffer */
                     glut_state_flag := glut_state_flag or glow.DEPTH ;       -- /* Somebody fix this for me! */

                  when 8 => --  /* "double":  Boolean indicating if the color buffer is double buffered */
                     glut_state_flag := glut_state_flag or glow.DOUBLE ;

                  when 9 => --  /* "green":  Green color buffer precision in bits */
                     null;

                  when 10 => --  /* "index":  Boolean if the color model is color index or not */
                     glut_state_flag := glut_state_flag or glow.INDEX ;

                  when 11 => --  /* "num":  A special capability  name indicating where the value represents the Nth frame buffer
                     null;        -- configuration matching the description string */

                  when 12 => -- /* "red":  Red color buffer precision in bits */
                     null;

                  when 13 => --  /* "rgba":  Number of bits of red, green, blue, and alpha in  the RGBA color buffer */
                     glut_state_flag := glut_state_flag or glow.RGBA ;    -- /* Somebody fix this for me! */

                  when 14 => --/* "rgb":  Number of bits of red, green, and blue in the  RGBA color buffer with zero bits alpha */
                     glut_state_flag := glut_state_flag or glow.RGB;   --  /* Somebody fix this for me! */

                  when 15 => --  /* "luminance":  Number of bits of red in the RGBA and zero bits of green, blue (alpha not specified)
                     -- of color buffer precision */
                     glut_state_flag := glut_state_flag or glow.LUMINANCE;    -- /* Somebody fix this for me! */

                  when 16 => --  /* "stencil":  Number of bits in the stencil buffer */
                     glut_state_flag := glut_state_flag or glow.STENCIL;      -- /* Somebody fix this for me! */

                  when 17 => --  /* "single":  Boolean indicate the color buffer is single  buffered */
                     glut_state_flag := glut_state_flag or glow.SINGLE ;

                  when 18 => --  /* "stereo":  Boolean indicating the color buffer supports  OpenGL-style stereo */
                     glut_state_flag := glut_state_flag or glow.STEREO ;

                  when 19 => -- /* "samples":  Indicates the number of multisamples to use based on GLX's SGIS_multisample
                     -- extension (for antialiasing) */
                     glut_state_flag := glut_state_flag or glow.MULTISAMPLE;    -- /*Somebody fix this for me!*/

                  when 20 => --  /* "slow":  Boolean indicating if the frame buffer configuration is slow or not */
                     null;

                  when 21     --  /* "win32pdf": (incorrect spelling but was there before */
                     | 22 => --  /* "win32pfd":  matches the Win32 Pixel Format Descriptor by number */
                     null;

                  when 23 => --  /* "xvisual":  matches the X visual ID by number */
                     null;

                  when 24   -- /* "xstaticgray": */
                     | 30 => --  /* "xstaticgrey":  boolean indicating if the frame buffer configuration's X visual is of type StaticGray */
                     null ;

                  when 25  -- /* "xgrayscale": */
                     | 31 =>    --/* "xgreyscale":  boolean indicating if the frame buffer configuration's X visual is of type GrayScale */
                     null ;

                  when 26   --/* "xstaticcolor": */
                     | 32 => --  /* "xstaticcolour":  boolean indicating if the frame buffer configuration's X visual is of type StaticColor */
                     null ;

                  when 27   --/* "xpseudocolor": */
                     | 33 => --  /* "xpseudocolour":  boolean indicating if the frame buffer configuration's X visual is of type PseudoColor */
                     null ;

                  when 28    --  /* "xtruecolor": */
                     | 34 => --  /* "xtruecolour":  boolean indicating if the frame buffer configuration's X visual is of type TrueColor */
                     null ;

                  when 29 --  /* "xdirectcolor": */
                     | 35 => --  /* "xdirectcolour":  boolean indicating if the frame buffer configuration's X visual is of type DirectColor */
                     null ;

                  when 36 => --  /* "borderless":  windows should not have borders */
                     null ;

                  when 37 => --  /* "aux":  some number of aux buffers */
                     glut_state_flag := glut_state_flag or glow.AUX;

                  when others =>
                     raise program_Error with "Display string token not recognized: " & clean_Token;

               end case;

            end;
         end;

         Start     := Last + 1;
         exit when Start > Name'Last;

         Last      := ada.strings.fixed.Index (Name, String'(" " & ada.characters.latin_1.HT),  from => Start);
         the_Token := to_unbounded_String     (Name (Start .. Last));
      end loop;


      fgState.DisplayMode := glut_state_flag;   -- We will make use of this value when creating a new OpenGL context.
   end InitDisplayString;









   -- Set the current window's title
   --
   procedure SetWindowTitle (Title : String)
   is
   begin
      verify_Initialised;

      if fgStructure.CurrentWindow.Parent = null then
         platform.SetWindowTitle (Title);
      end if;

   end SetWindowTitle;






   -- Set the current window's iconified title
   --
   procedure SetIconTitle (Title : String)
   is
   begin
      verify_Initialised;

      if fgStructure.CurrentWindow.Parent = null then
         platform.SetIconTitle (Title);
      end if;
   end SetIconTitle;





   --  Recalculates current menu's box size
   --
   procedure fghCalculateMenuBoxSize
   is
      menuEntry : internal.SFG_MenuEntry_view;
      Width     : Integer := 0;
      Height    : Integer := 0;
   begin
      if fgStructure.CurrentMenu = null then               --  Make sure there is a current menu set
         return;
      end if;

         raise Program_Error; -- tbd
   end;

--  void fghCalculateMenuBoxSize( void )
--  {
--      /* The menu's box size depends on the menu entries: */
--      for( menuEntry = ( SFG_MenuEntry * )fgStructure.CurrentMenu->Entries.First;
--           menuEntry;
--           menuEntry = ( SFG_MenuEntry * )menuEntry->Node.Next )
--      {
--          /* Update the menu entry's width value */
--          menuEntry->Width = glutBitmapLength(
--              FREEGLUT_MENU_FONT,
--              (unsigned char *)menuEntry->Text
--          );
--
--          /*
--           * If the entry is a submenu, then it needs to be wider to
--           * accomodate the arrow. JCJ 31 July 2003
--           */
--          if (menuEntry->SubMenu )
--              menuEntry->Width += glutBitmapLength(
--                  FREEGLUT_MENU_FONT,
--                  (unsigned char *)"_"
--              );
--
--          /* Check if it's the biggest we've found */
--          if( menuEntry->Width > width )
--              width = menuEntry->Width;
--
--          height += FREEGLUT_MENU_HEIGHT;
--      }
--
--      /* Store the menu's box size now: */
--      fgStructure.CurrentMenu->Height = height + 2 * FREEGLUT_MENU_BORDER;
--      fgStructure.CurrentMenu->Width  = width  + 4 * FREEGLUT_MENU_BORDER;
--  }




   -- Adds a menu entry to the bottom of the current menu
   --
   procedure AddMenuEntry (Label : String;   Value : Integer)
   is
     menuEntry : internal.SFG_MenuEntry_view := new SFG_MenuEntry;
   begin
      verify_Initialised;

      if fgStructure.CurrentMenu = null then
         return;
      end if;

      menuEntry.Text := to_unbounded_String (Label);
      menuEntry.ID  := value;

      fgStructure.CurrentMenu.Entries.append (menuEntry);      -- Have the new menu entry attached to the current menu
      fghCalculateMenuBoxSize;
   end AddMenuEntry;





   procedure AddSubMenu (Label : String; Submenu : Integer)
   is
   begin
      raise program_Error;
   end AddSubMenu;



   procedure ChangeToMenuEntry
     (Item  : Integer;
      Label : String;
      Value : Integer)
   is
   begin
      raise program_Error;
   end ChangeToMenuEntry;



   procedure ChangeToSubMenu
     (Item    : Integer;
      Label   : String;
      Submenu : Integer)
   is
   begin
      raise program_Error;
   end ChangeToSubMenu;









   -- _glfwStringInExtensionString() - Check if a string can be found in an
   -- OpenGL extension string

   function String_In_Extension_String (Test_Extension : in String;
                                        Extensions     : in String) return Boolean
   is
      use ada.Strings.fixed;
      Where          : Natural;
      Start          : Positive := 1;
      Terminator     : Natural;
   begin
      -- It takes a bit of care to be fool-proof about parsing the
      -- OpenGL extensions string. Don't be fooled by sub-strings, etc.

      loop
         Where := ada.strings.fixed.Index (Extensions, Test_Extension, Start);

         if Where = 0 then
            return False;
         end if;

         Terminator := Where + Test_Extension'Length;

         if        Where = Start
           or else Extensions (Where - 1) = ' '
         then
            exit when         Extensions (Terminator) = ' '
                      or else Terminator = Extensions'Last;          -- tbd: check this !!
         end if;

         Start := Terminator;
      end loop;


      return True;
   end;





   -- This functions checks if an OpenGL extension is supported or not
   --
--     function ExtensionSupported (Name : String) return Integer
--     is
--        Result : Integer;
--        C_Name : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String (Name);
--     begin
--        Result := ExtensionSupported (C_Name);
--        Interfaces.C.Strings.Free (C_Name);
--        return Result;
--     end ExtensionSupported;


   function ExtensionSupported (Name : String) return Boolean
   is
      use ada.Strings.fixed;
   begin
      verify_Initialised;               -- Make sure there is a current window, and thus a current context available

      if ada.strings.fixed.Index (Name, " ") /= 0 then
         return False;
      else
         return String_In_Extension_String (Name, gl.GetString (GL.EXTENSIONS));
      end if;

   end ExtensionSupported;


--  int FGAPIENTRY glutExtensionSupported( const char* extension )
--  {
--    start = extensions = (const char *) glGetString(GL_EXTENSIONS);
--
--    /* XXX consider printing a warning to stderr that there's no current
--     * rendering context.
--     */
--    freeglut_return_val_if_fail( extensions != NULL, 0 );
--
--    while (1) {
--       const char *p = strstr(extensions, extension);
--       if (!p)
--          return 0;  /* not found */
--       /* check that the match isn't a super string */
--       if ((p == start || p[-1] == ' ') && (p[len] == ' ' || p[len] == 0))
--          return 1;
--       /* skip the false match and continue */
--       extensions = p + len;
--    }
--
--    return 0 ;
--  }





   -- Moves the mouse pointer to given window coordinates
   --
   procedure WarpPointer (X : Integer; Y : Integer)
   is
   begin
      verify_Initialised;

      platform.WarpPointer (X, Y);
   end;








   -- Set the cursor image to be used for the current window
   --
   procedure fgSetCursor (window : SFG_Window_view; cursorID : Integer)
   is
   begin
      platform.fgSetCursor (window, cursorID);
   end;


--  void fgSetCursor ( SFG_Window *window, int cursorID )
--  {
--  #if TARGET_HOST_POSIX_X11
--      {
--          Cursor cursor;
--          /*
--           * XXX FULL_CROSSHAIR demotes to plain CROSSHAIR. Old GLUT allows
--           * for this, but if there is a system that easily supports a full-
--           * window (or full-screen) crosshair, we might consider it.
--           */
--          int cursorIDToUse =
--              ( cursorID == GLUT_CURSOR_FULL_CROSSHAIR ) ? GLUT_CURSOR_CROSSHAIR : cursorID;
--
--          if( ( cursorIDToUse >= 0 ) &&
--              ( cursorIDToUse < sizeof( cursorCache ) / sizeof( cursorCache[0] ) ) ) {
--              cursorCacheEntry *entry = &cursorCache[ cursorIDToUse ];
--              if( entry->cachedCursor == None ) {
--                  entry->cachedCursor =
--                      XCreateFontCursor( fgDisplay.Display, entry->cursorShape );
--              }
--              cursor = entry->cachedCursor;
--          } else {
--              switch( cursorIDToUse )
--              {
--              case GLUT_CURSOR_NONE:
--                  cursor = getEmptyCursor( );
--                  break;
--
--              case GLUT_CURSOR_INHERIT:
--                  cursor = None;
--                  break;
--
--              default:
--                  fgError( "Unknown cursor type: %d", cursorIDToUse );
--                  return;
--              }
--          }
--
--          if ( cursorIDToUse == GLUT_CURSOR_INHERIT ) {
--              XUndefineCursor( fgDisplay.Display, window->Window.Handle );
--          } else if ( cursor != None ) {
--              XDefineCursor( fgDisplay.Display, window->Window.Handle, cursor );
--          } else if ( cursorIDToUse != GLUT_CURSOR_NONE ) {
--              fgError( "Failed to create cursor" );
--          }
--      }
--
--  #elif TARGET_HOST_MS_WINDOWS
--
--      /*
--       * Joe Krahn is re-writing the following code.
--       */
--      /* Set the cursor AND change it for this window class. */
--  #if _MSC_VER <= 1200
--  #       define MAP_CURSOR(a,b)                                   \
--          case a:                                                  \
--              SetCursor( LoadCursor( NULL, b ) );                  \
--              SetClassLong( window->Window.Handle,                 \
--                            GCL_HCURSOR,                           \
--                            ( LONG )LoadCursor( NULL, b ) );       \
--          break;
--      /* Nuke the cursor AND change it for this window class. */
--  #       define ZAP_CURSOR(a,b)                                   \
--          case a:                                                  \
--              SetCursor( NULL );                                   \
--              SetClassLong( window->Window.Handle,                 \
--                            GCL_HCURSOR, ( LONG )NULL );           \
--          break;
--  #else
--  #       define MAP_CURSOR(a,b)                                   \
--          case a:                                                  \
--              SetCursor( LoadCursor( NULL, b ) );                  \
--              SetClassLongPtr( window->Window.Handle,              \
--                            GCLP_HCURSOR,                          \
--                            ( LONG )( LONG_PTR )LoadCursor( NULL, b ) );       \
--          break;
--      /* Nuke the cursor AND change it for this window class. */
--  #       define ZAP_CURSOR(a,b)                                   \
--          case a:                                                  \
--              SetCursor( NULL );                                   \
--              SetClassLongPtr( window->Window.Handle,              \
--                            GCLP_HCURSOR, ( LONG )( LONG_PTR )NULL );          \
--          break;
--  #endif
--
--      switch( cursorID )
--      {
--          MAP_CURSOR( GLUT_CURSOR_RIGHT_ARROW,         IDC_ARROW     );
--          MAP_CURSOR( GLUT_CURSOR_LEFT_ARROW,          IDC_ARROW     );
--          MAP_CURSOR( GLUT_CURSOR_INFO,                IDC_HELP      );
--          MAP_CURSOR( GLUT_CURSOR_DESTROY,             IDC_CROSS     );
--          MAP_CURSOR( GLUT_CURSOR_HELP,                IDC_HELP      );
--          MAP_CURSOR( GLUT_CURSOR_CYCLE,               IDC_SIZEALL   );
--          MAP_CURSOR( GLUT_CURSOR_SPRAY,               IDC_CROSS     );
--          MAP_CURSOR( GLUT_CURSOR_WAIT,                IDC_WAIT      );
--          MAP_CURSOR( GLUT_CURSOR_TEXT,                IDC_IBEAM     );
--          MAP_CURSOR( GLUT_CURSOR_CROSSHAIR,           IDC_CROSS     );
--          MAP_CURSOR( GLUT_CURSOR_UP_DOWN,             IDC_SIZENS    );
--          MAP_CURSOR( GLUT_CURSOR_LEFT_RIGHT,          IDC_SIZEWE    );
--          MAP_CURSOR( GLUT_CURSOR_TOP_SIDE,            IDC_ARROW     ); /* XXX ToDo */
--          MAP_CURSOR( GLUT_CURSOR_BOTTOM_SIDE,         IDC_ARROW     ); /* XXX ToDo */
--          MAP_CURSOR( GLUT_CURSOR_LEFT_SIDE,           IDC_ARROW     ); /* XXX ToDo */
--          MAP_CURSOR( GLUT_CURSOR_RIGHT_SIDE,          IDC_ARROW     ); /* XXX ToDo */
--          MAP_CURSOR( GLUT_CURSOR_TOP_LEFT_CORNER,     IDC_SIZENWSE  );
--          MAP_CURSOR( GLUT_CURSOR_TOP_RIGHT_CORNER,    IDC_SIZENESW  );
--          MAP_CURSOR( GLUT_CURSOR_BOTTOM_RIGHT_CORNER, IDC_SIZENWSE  );
--          MAP_CURSOR( GLUT_CURSOR_BOTTOM_LEFT_CORNER,  IDC_SIZENESW  );
--          MAP_CURSOR( GLUT_CURSOR_INHERIT,             IDC_ARROW     ); /* XXX ToDo */
--          ZAP_CURSOR( GLUT_CURSOR_NONE,                NULL          );
--          MAP_CURSOR( GLUT_CURSOR_FULL_CROSSHAIR,      IDC_CROSS     ); /* XXX ToDo */
--
--      default:
--          fgError( "Unknown cursor type: %d", cursorID );
--          break;
--      }
--  #endif
--
--      window->State.Cursor = cursorID;
--  }




   -- Set the cursor image to be used for the current window
   --
   procedure SetCursor (Cursor : in Integer)
   is
   begin
      verify_Initialised;

      fgSetCursor (fgStructure.CurrentWindow, Cursor);
   end;









   -- Resize the current window so that it fits the whole screen
   --
   procedure FullScreen
   is
   begin
      verify_Initialised;

      if glow.Get (glow.FULL_SCREEN) /= 0 then
         platform.FullScreenToggle;     -- Leave full screen state before resizing.
      end if;

   end;


--  void FGAPIENTRY glutFullScreen( void )
--  {
--      if (glutGet(GLUT_FULL_SCREEN))
--      {
--        /*  Leave full screen state before resizing. */
--        glutFullScreenToggle();
--      }
--
--      {
--  #if TARGET_HOST_POSIX_X11
--
--          Status status;  /* Returned by XGetWindowAttributes(), not checked. */
--          XWindowAttributes attributes;
--
--          status = XGetWindowAttributes(fgDisplay.Display,
--                                        fgStructure.CurrentWindow->Window.Handle,
--                                        &attributes);
--          /*
--           * The "x" and "y" members of "attributes" are the window's coordinates
--           * relative to its parent, i.e. to the decoration window.
--           */
--          XMoveResizeWindow(fgDisplay.Display,
--                            fgStructure.CurrentWindow->Window.Handle,
--                            -attributes.x,
--                            -attributes.y,
--                            fgDisplay.ScreenWidth,
--                            fgDisplay.ScreenHeight);
--
--  #elif TARGET_HOST_MS_WINDOWS && !defined(_WIN32_WCE) /* FIXME: what about WinCE */
--          RECT rect;
--
--          /* For fullscreen mode, force the top-left corner to 0,0
--           * and adjust the window rectangle so that the client area
--           * covers the whole screen.
--           */
--
--          rect.left   = 0;
--          rect.top    = 0;
--          rect.right  = fgDisplay.ScreenWidth;
--          rect.bottom = fgDisplay.ScreenHeight;
--
--          AdjustWindowRect ( &rect, WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS |
--                                    WS_CLIPCHILDREN, FALSE );
--
--          /*
--           * SWP_NOACTIVATE     Do not activate the window
--           * SWP_NOOWNERZORDER  Do not change position in z-order
--           * SWP_NOSENDCHANGING Supress WM_WINDOWPOSCHANGING message
--           * SWP_NOZORDER       Retains the current Z order (ignore 2nd param)
--           */
--
--          SetWindowPos( fgStructure.CurrentWindow->Window.Handle,
--                        HWND_TOP,
--                        rect.left,
--                        rect.top,
--                        rect.right  - rect.left,
--                        rect.bottom - rect.top,
--                        SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
--                        SWP_NOZORDER
--                      );
--  #endif
--      }
--  }
















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






   -- Sets the Display callback for the current window
   --
   procedure DisplayFunc (P1 : Glut_Proc_2) is
   begin
      if P1 = null then
         raise program_Error with "Fatal error in program.  NULL display callback not permitted in GLUT 3.0+ or freeglut 2.0.1+";
      end if;

      fgStructure.CurrentWindow.Callbacks.CB_Display := SFG_Proc (P1);
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

--     procedure IdleFunc (P1 : System.Address) is
--       function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_10);
--     begin
--       IdleFunc( Cvt(P1) );
--     end IdleFunc;

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

end Glow;
