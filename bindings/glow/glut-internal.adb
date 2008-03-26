
with glut.Platform;



with ada.containers.doubly_linked_Lists;
with ada.Strings.unbounded;                use ada.Strings.unbounded;
with ada.Calendar;

with ada.unchecked_Deallocation;



package body glut.Internal is




   procedure fghClearCallBacks (Window : in out SFG_Window)
   is
      null_Callbacks : callback_Set;
   begin
      Window.Callbacks := null_Callbacks;
   end;





   function fgWindowByHandle (Windows       : in window_List;
                              Window_Handle : in platform.SFG_WindowHandleType) return SFG_Window_view
   is
      use Window_Lists;
      use type platform.SFG_WindowHandleType;

      Cursor : Window_Lists.Cursor := First (Windows);
      Window : SFG_Window_view;
   begin

      while has_Element (Cursor) loop
         Window := Element (Cursor);

         if window.Window.Handle = Window_Handle then
            return Window;
         else
            Window := fgWindowByHandle (Window.Children, Window_Handle);   -- recurse through children.

            if Window /= null then
               return Window;
            end if;
         end if;

         next (Cursor);
      end loop;

      return null;
   end;





   --   Sets the OpenGL context and the fgStructure "Current Window" pointer to
   --   the window structure passed in.
   --
   procedure fgSetWindow (window : access SFG_Window)
   is
   begin
      platform.fgSetWindow (window);
      fgStructure.CurrentWindow := window.all'access;
   end;





   -- Function to close down all the windows in the "WindowsToDestroy" list
   --
   procedure fgCloseWindows
   is
   begin
--      while( fgStructure.WindowsToDestroy.First )
--      {
--          SFG_WindowList *window_ptr = fgStructure.WindowsToDestroy.First;
--          fgDestroyWindow( window_ptr->window );
--          fgListRemove( &fgStructure.WindowsToDestroy, &window_ptr->node );
--          free( window_ptr );
--      }
      null; -- tbd: deferred

   end;

--  void fgCloseWindows( )
--  {
--      while( fgStructure.WindowsToDestroy.First )
--      {
--          SFG_WindowList *window_ptr = fgStructure.WindowsToDestroy.First;
--          fgDestroyWindow( window_ptr->window );
--          fgListRemove( &fgStructure.WindowsToDestroy, &window_ptr->node );
--          free( window_ptr );
--      }
--  }







   --    This function destroys a window and all of its subwindows. Actually,
   --    another function, defined in freeglut_window.c is called, but this is
   --    a whole different story...
   --

   procedure fgDestroyWindow (Window : in SFG_Window_view)
   is
      procedure free is new ada.unchecked_Deallocation (SFG_Window, SFG_Window_view);
      use window_Lists;
   begin
      while has_Element (window.Children.First) loop              -- destroy the windows children.
         declare
            Child : SFG_Window_view := Element (window.Children.First);
         begin
            fgDestroyWindow (Child);
         end;
         window.Children.delete_First;
      end loop;


      declare
         active_Window : SFG_Window_view := fgStructure.CurrentWindow;
      begin
         fgSetWindow (Window);
         Window.Callbacks.CB_Destroy.all;
         fgSetWindow (active_Window);
      end;


      if window.ActiveMenu /= null then
         null;  -- fgDeactivateMenu (window);    -- tbd: deferred
      end if;

      fghClearCallBacks      (window.all);
      platform.fgCloseWindow (window);

      declare
         freed_Window : SFG_Window_view := Window;
      begin
         free (freed_Window);
      end;

      if fgStructure.CurrentWindow = window then
         fgStructure.CurrentWindow := null;
      end if;

   end;




   --    This function is automatically called on glutMainLoop() return.
   --    It should deallocate and destroy all remnants of previous
   --    glutInit()-enforced structure initialization...
   --
   procedure fgDestroyStructure
   is
      use window_Lists;
   begin
      fgCloseWindows;       -- Clean up the WindowsToDestroy list.


      -- Make sure all windows and menus have been deallocated
      --

      --      while( fgStructure.Menus.First ) -- tbd: deferred
      --          fgDestroyMenu( ( SFG_Menu * )fgStructure.Menus.First );


      while has_Element (fgStructure.Windows.First) loop
         fgDestroyWindow (Element (fgStructure.Windows.First));
         fgStructure.Windows.delete_First;
      end loop;

   end;




   procedure fgInputDeviceClose
   is
   begin
      if fgState.InputDevsInitialised then
--           serial_close ( dialbox_port );
--           dialbox_port := NULL;
         fgState.InputDevsInitialised := False;
      end if;
   end;






--   Perform the freeglut deinitialization...
--

   procedure fgDeinitialize
   is
   begin
      if not fgState.Initialised then
         raise constraint_Error with "fgDeinitialize: no valid initialization has been performed";
      end if;

      -- If there was a menu created, destroy the rendering context
      --
      if fgStructure.MenuContext /= null then
         platform.DeInit_pre;
         free (fgStructure.MenuContext);
      end if;


      fgDestroyStructure;

      platform.DeInit_mid;

      fgState := (JoysticksInitialised => False,
                  InputDevsInitialised => False,

                  Initialised => False,

                  Position => ( -1,  -1, False),
                  Size     => (300, 300, True),

                  DisplayMode => GLUT.RGBA or GLUT.SINGLE or GLUT.DEPTH,

                  DirectContext  => TRY_DIRECT_CONTEXT,
                  ForceIconic         => False,
                  UseCurrentContext   => False,
                  GLDebugSwitch       => False,
                  XSyncSwitch         => False,
                  ActionOnWindowClose => ACTION_EXIT,
                  ExecState           => EXEC_STATE_INIT,

                  KeyRepeat       => GLUT.KEY_REPEAT_ON,
                  Modifiers       => INVALID_MODIFIERS,

                  GameModeSize    => (X => 640,  Y => 480,  used => False),
                  GameModeDepth   =>  16,
                  GameModeRefresh =>  72,

--                    MenuStateCallback  => null,
--                    MenuStatusCallback => null,

                  SwapCount   => 0,
                  SwapTime    => 0,
                  FPSInterval => 0,

                  ProgramName => null_unbounded_String,

                  Time                  => ada.calendar.Clock,
                  auxiliarybuffernumber => 0,
                  samplenumber          => 0);


      platform.DeInit_post;
   end;




   -- Elapsed Time
   --
   function fgElapsedTime return Duration
   is
      use ada.Calendar;
   begin
      return (ada.calendar.Clock - fgState.Time) * 1000.0;
   end;




end glut.Internal;

