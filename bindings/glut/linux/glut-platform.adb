
with glut.Internal;   use glut.Internal;
with opengl.glx;      use opengl.glx;

with x_lib.Property;
with X_Lib.Predefined_Atoms;   use X_Lib.Predefined_Atoms;

with ada.containers.doubly_linked_Lists;
with ada.Strings.unbounded;
with ada.Calendar;

with interfaces.c.Pointers;



package body glut.Platform is


   package C renames interfaces.C;
   use interfaces.C;

   use X_Lib;




   -- Return the atom associated with "name".
   --
   function fghGetAtom (Named : in String) return x_lib.Atom
   is
      use x_lib.Property;
   begin
      return X_Intern_Atom (fgDisplay.platform.Display,  Named,  False);
   end;





   --   * Check if "property" is set on "window".  The property's values are returned
   --   * through "data".  If the property is set and is of type "type", return the
   --   * number of elements in "data".  Return zero otherwise.  In both cases, use
   --   * "Xfree()" to free "data".
   --
   function fghGetWindowProperty (Window    : x_lib.Window_Id;
                                  Property  : x_lib.Atom;
                                  Kind      : x_lib.Atom) return x_lib.property.Get_Window_Property_Return_Type
   is
      use x_lib.Property;

      the_Property : x_lib.property.Get_Window_Property_Return_Type
        := X_Get_Window_Property (Display     => fgDisplay.platform.Display,
                                  W           => Window,
                                  Property    => Property,
                                  Long_Offset => 0,
                                  Long_Length => Natural'Last,
                                  Delete      => False,
                                  Req_Type    => Kind);
   begin

      return the_Property;
   end;

--  static int fghGetWindowProperty(Window window,
--  				Atom property,
--  				Atom type,
--  				unsigned char ** data)
--  {
--    int status;  /*  Returned by "XGetWindowProperty". */
--
--    Atom          type_returned;
--    int           temp_format;             /*  Not used. */
--    unsigned long number_of_elements;
--    unsigned long temp_bytes_after;        /*  Not used. */
--
--
--    status = XGetWindowProperty(fgDisplay.Display,
--  			      window,
--  			      property,
--  			      0,
--  			      LONG_MAX,
--  			      False,
--  			      type,
--  			      &type_returned,
--  			      &temp_format,
--  			      &number_of_elements,
--  			      &temp_bytes_after,
--  			      data);
--
--    FREEGLUT_INTERNAL_ERROR_EXIT(status == Success,
--  			       "XGetWindowProperty failled",
--  			       "fghGetWindowProperty");
--
--    if (type_returned != type)
--      {
--        number_of_elements = 0;
--      }
--
--    return number_of_elements;
--  }





   -- Check if the window manager is NET WM compliant.
   --
   function fghNetWMSupported return Boolean
   is
      wm_Check     : x_Lib.Atom                              := fghGetAtom ("_NET_SUPPORTING_WM_CHECK");
      window_ptr_1 : array (1 .. 1) of access x_lib.Window_Id;

      --     * Check that the window manager has set this property on the root window.
      --     * The property must be the ID of a child window.
      --
      window_1_Property : x_lib.property.Get_Window_Property_Return_Type := fghGetWindowProperty(fgDisplay.platform.Root_Window,
                                                                                                 wm_check,
                                                                                                 XA_WINDOW);
      number_of_windows : Integer := window_1_Property.Length;   -- tbd: check this
   begin
      if number_of_Windows = 1 then
         declare
            use x_lib;
            window_2_Property : x_lib.property.Get_Window_Property_Return_Type               -- Check that the window has the
              := fghGetWindowProperty (x_lib.drawable_Id (window_1_Property.value_32 (1)),   -- same property set to the same value.
                                       wm_check,
                                       XA_WINDOW);

         begin
            if         window_2_Property.Length = 1
              and then window_1_Property.value_32 (1) = window_2_Property.value_32 (1)
            then
               return True;            -- NET WM compliant
            end if;
         end;
      end if;

      return False;
   end;





   -- Check if "hint" is present in "property" for "window".
   --
   function fgHintPresent (Window   : x_lib.Window_Id;
                           property : x_lib.Atom;
                           hint     : x_lib.Atom) return Boolean
   is
      use x_lib;
      window_Property : x_lib.property.Get_Window_Property_Return_Type := fghGetWindowProperty (Window,
                                                                                                property,
                                                                                                XA_ATOM);
      number_of_atoms : Integer renames window_Property.Length;
   begin
      for Each in 1 .. number_of_atoms loop
         if Atom (window_Property.value_32 (Each)) = hint then
            return True;
         end if;
      end loop;

      return False;
   end;







   -- A call to this function should initialize all the display stuff.
   --
   procedure fghInitialize (display_Name : in String)
   is
      use X_Lib;
      use type x_lib.display_Pointer;
   begin
      fgDisplay.platform.Display := X_Open_Display (display_Name);

      if fgDisplay.platform.Display = null_display_Pointer then
         raise constraint_Error with "failed to open display " & display_Name;
      end if;

      if not glXQueryExtension (fgDisplay.platform.Display, null, null) then
         raise constraint_Error with "OpenGL GLX extension not supported by display " & display_Name;
      end if;


      fgDisplay.platform.Screen      := X_Default_Screen (fgDisplay.platform.Display);
      fgDisplay.platform.root_Window := X_Root_Window    (fgDisplay.platform.Display,  fgDisplay.platform.Screen);

      fgDisplay.ScreenWidth  := X_Display_Width  (fgDisplay.platform.Display,  fgDisplay.platform.Screen);
      fgDisplay.ScreenHeight := X_Display_Height (fgDisplay.platform.Display,  fgDisplay.platform.Screen);

      fgDisplay.ScreenWidthMM  := X_Display_Width_MM  (fgDisplay.platform.Display,  fgDisplay.platform.Screen);
      fgDisplay.ScreenHeightMM := X_Display_Height_MM (fgDisplay.platform.Display,  fgDisplay.platform.Screen);

      fgDisplay.platform.Connection := X_Connection_Number (fgDisplay.platform.Display);

      fgDisplay.platform.Delete_Window := fghGetAtom ("WM_DELETE_WINDOW");       -- Create the window deletion atom

      -- Create the state and full screen atoms
      --
      fgDisplay.platform.State            := null_Atom;
      fgDisplay.platform.State_fullScreen := null_Atom;

      if fghNetWMSupported then
         declare
            supported : constant x_lib.Atom := fghGetAtom ("_NET_SUPPORTED");
            state     : constant x_lib.Atom := fghGetAtom ("_NET_WM_STATE");
         begin
            if fgHintPresent (fgDisplay.platform.root_Window, supported, state) then    -- Check if the state hint is supported
               declare
                  full_screen : constant x_lib.Atom := fghGetAtom ("_NET_WM_STATE_FULLSCREEN");
               begin
                  fgDisplay.platform.State := state;

                  if fgHintPresent (fgDisplay.platform.root_Window, supported, full_screen) then   -- Check if the window manager supports full screen.
                     fgDisplay.platform.State_fullScreen := full_screen;                           -- tbd: Check "_NET_WM_ALLOWED_ACTIONS" on our window instead?
                  end if;
               end;
            end if;
         end;
      end if;

   end;






   function fgChooseFBConfig return access GLXFBConfig
   is
      wantIndexedMode : Boolean := False;
      attributes      : array (1 .. 32) of aliased Integer;
      where           : Integer := 0;

      procedure Attrib (A : in Integer)
      is
      begin
         where              := where + 1;
         attributes (Where) := A;
      end;

      procedure Attrib_Val (A : in Integer;   V : in Integer)
      is
      begin
         Attrib (A);
         Attrib (V);
      end;

   begin
      -- First we have to process the display mode settings
      --

      if (fgState.DisplayMode and GLUT.INDEX) /= 0 then
         ATTRIB_VAL (GLX_BUFFER_SIZE, 8);          --  Buffer size is selected later.
         ATTRIB_VAL (GLX_RENDER_TYPE, GLX_COLOR_INDEX_BIT );

         wantIndexedMode := True;
      else
         ATTRIB_VAL (GLX_RED_SIZE,   1);
         ATTRIB_VAL (GLX_GREEN_SIZE, 1);
         ATTRIB_VAL (GLX_BLUE_SIZE,  1);

         if (fgState.DisplayMode and GLUT.ALPHA) /= 0 then
            ATTRIB_VAL (GLX_ALPHA_SIZE, 1);
         end if;

      end if;


      if (fgState.DisplayMode and GLUT.DOUBLE) /= 0 then
         ATTRIB_VAL (GLX_DOUBLEBUFFER, 1);
      end if;

      if (fgState.DisplayMode and GLUT.STEREO) /= 0 then
         ATTRIB_VAL (GLX_STEREO, 1);   -- 1 => True
      end if;

      if (fgState.DisplayMode and GLUT.DEPTH) /= 0 then
         ATTRIB_VAL (GLX_DEPTH_SIZE, 1);
      end if;

      if (fgState.DisplayMode and GLUT.STENCIL) /= 0 then
         ATTRIB_VAL (GLX_STENCIL_SIZE, 1);
      end if;


      if (fgState.DisplayMode and GLUT.ACCUM) /= 0 then
         ATTRIB_VAL (GLX_ACCUM_RED_SIZE,   1);
         ATTRIB_VAL (GLX_ACCUM_GREEN_SIZE, 1);
         ATTRIB_VAL (GLX_ACCUM_BLUE_SIZE,  1);

         if (fgState.DisplayMode and GLUT.ALPHA) /= 0 then
            ATTRIB_VAL (GLX_ACCUM_ALPHA_SIZE, 1);
         end if;
      end if;


      if   (fgState.DisplayMode and GLUT.AUX)  /= 0
        or (fgState.DisplayMode and GLUT.AUX1) /= 0
        or (fgState.DisplayMode and GLUT.AUX2) /= 0
        or (fgState.DisplayMode and GLUT.AUX3) /= 0
        or (fgState.DisplayMode and GLUT.AUX4) /= 0
      then
         ATTRIB_VAL (GLX_AUX_BUFFERS, fgState.AuxiliaryBufferNumber);
      end if;


      if (fgState.DisplayMode and GLUT.MULTISAMPLE) /= 0 then
         ATTRIB_VAL (GLX_SAMPLE_BUFFERS, 1);
         ATTRIB_VAL (GLX_SAMPLES,        fgState.SampleNumber);
      end if;

      ATTRIB (0);    --  Push a null at the end of the list   tbd: check this !!   ATTRIB( None );


      declare
         use GLXFBConfig_Pointers;

         fbconfigArraySize     : aliased Integer;           -- Number of FBConfigs in the array
         fbconfigArray_pointer : GLXFBConfig_Pointers.Pointer
           := GLXFBConfig_Pointers.Pointer (glXChooseFBConfig (fgDisplay.platform.Display,
                                                               Integer (fgDisplay.platform.Screen),
                                                               attributes (1)'unchecked_access,
                                                               fbconfigArraySize'unchecked_access));


         fbconfig           : GLXFBConfig_Pointers.Pointer;       -- The FBConfig we want
         Result             : Integer;                   -- Returned by glXGetFBConfigAttrib, not checked.


      begin
         if fbconfigArray_pointer /= null then
            declare
               fbconfigArray : GLXFBConfig_Array                                           -- Array of FBConfigs
                               renames GLXFBConfig_Pointers.Value (fbconfigArray_pointer,
                                                                   ptrDiff_t (fbconfigArraySize));

            begin

               if wantIndexedMode then
                  --                   In index mode, we want the largest buffer size, i.e. visual
                  --                   depth.  Here, FBConfigs are sorted by increasing buffer size
                  --                   first, so FBConfigs with the largest size come last.
                  declare
                     bufferSizeMin,
                     bufferSizeMax : aliased Integer;
                  begin
                     result := glXGetFBConfigAttrib (fgDisplay.platform.Display,                    --  Get bufferSizeMin.
                                                     fbconfigArray (1),
                                                     GLX_BUFFER_SIZE,
                                                     bufferSizeMin'unchecked_access);

                     result := glXGetFBConfigAttrib (fgDisplay.platform.Display,                    --  Get bufferSizeMax.
                                                     fbconfigArray (fbconfigArraySize),
                                                     GLX_BUFFER_SIZE,
                                                     bufferSizeMax'unchecked_access);

                     if (bufferSizeMax > bufferSizeMin) then
                        -- Free and reallocate fbconfigArray, keeping only FBConfigs with the largest buffer size.
                        --
                        x_lib.XFree (fbconfigArray_pointer.all'address);    -- tbd: check this !!!!!!

                        -- Add buffer size token at the end of the list.
                        --
                        where := where - 1;

                        ATTRIB_VAL (GLX_BUFFER_SIZE, bufferSizeMax);
                        ATTRIB     (0); -- tbd: check this    --  ATTRIB (None);

                        fbconfigArray_Pointer := GLXFBConfig_Pointers.Pointer (glXChooseFBConfig (fgDisplay.platform.Display,
                                                                                                  Integer (fgDisplay.platform.Screen),
                                                                                                  attributes (1)'unchecked_access,
                                                                                                  fbconfigArraySize'unchecked_access));

                     end if;
                  end;
               end if;


               if fbconfigArray_Pointer /= null then
                  --
                  --                We now have an array of FBConfigs, the first one being the "best"
                  --                one.  So we should return only this FBConfig:
                  --
                  --                int fbconfigXID;
                  --
                  --                 - pick the XID of the FBConfig we want
                  --                result = glXGetFBConfigAttrib( fgDisplay.Display,
                  --                                               fbconfigArray[0],
                  --                                               GLX_FBCONFIG_ID,
                  --                                               &fbconfigXID );
                  --
                  --                - free the array
                  --                XFree(fbconfigArray);
                  --
                  --                - reset "attributes" with the XID
                  --                where = 0;
                  --                ATTRIB_VAL( GLX_FBCONFIG_ID, fbconfigXID );
                  --                ATTRIB( None );
                  --
                  --                - get our FBConfig only
                  --                fbconfig = glXChooseFBConfig( fgDisplay.Display,
                  --                                              fgDisplay.Screen,
                  --                                              attributes,
                  --                                              &fbconfigArraySize );
                  --
                  --                However, for some configurations (for instance multisampling with
                  --                Mesa 6.5.2 and ATI drivers), this does not work:
                  --                glXChooseFBConfig returns NULL, whereas fbconfigXID is a valid
                  --                XID.  Further investigation is needed.
                  --
                  --                So, for now, we return the whole array of FBConfigs.  This should
                  --                not produce any side effects elsewhere.
                  --
                  fbconfig := fbconfigArray_Pointer;
               else
                  fbconfig := null;
               end if;
            end;

         end if;


         return fbconfig;
      end;
   end;




   procedure fgSetWindow (window : access SFG_Window)
   is
      unused : Integer;
   begin
      if Window /= null then
         unused := glXMakeContextCurrent (fgDisplay.platform.Display,
                                          glxDrawable (window.Window.Handle),
                                          glxDrawable (window.Window.Handle),
                                          window.Window.Context);
      end if;
   end;





   procedure fgOpenWindow (window      : access SFG_Window;
                           title       : in     String;
                           positionUse : in     Boolean;
                           x, y        : in     Integer;
                           sizeUse     : in     Boolean;
                           w, h        : in     Integer;
                           gameMode    : in     Boolean;
                           isSubWindow : in     Boolean)
   is
      use x_lib, x_lib.Property;

      position_X          : c.Short := c.Short (x);
      position_Y          : c.Short := c.Short (y);

      Width               : c.unsigned_Short := c.unsigned_Short (w);
      Height              : c.unsigned_Short := c.unsigned_Short (h);

      visualInfo          : access X_Visual_Info;
      winAttr             : aliased Set_Window_Attributes_Type;
      sizeHints           : aliased Size_Hints_type;
      wmHints             : aliased WM_Hints_type;
      mask                : Set_Window_Attributes_Mask;
      renderType          : Integer;                     -- GLX_RGBA_TYPE or GLX_COLOR_INDEX_TYPE
      current_DisplayMode : glut.unSigned := fgState.DisplayMode;

   begin
      if window.IsMenu and (fgStructure.MenuContext = null) then
         fgState.DisplayMode := GLUT.DOUBLE or GLUT.RGB ;           -- Save the display mode, if we are creating a menu window
      end if;

      window.Window.platform.FBConfig := fgChooseFBConfig;

      if window.IsMenu and (fgStructure.MenuContext = null) then
         fgState.DisplayMode := current_DisplayMode;
      end if;


      if window.Window.platform.FBConfig = null then
         --          The "fgChooseFBConfig" returned a null meaning that the visual
         --          context is not available.
         --          Try a couple of variations to see if they will work.
         --
         if (fgState.DisplayMode and GLUT.DOUBLE) = 0 then
            fgState.DisplayMode    := fgState.DisplayMode or GLUT.DOUBLE ;
            window.Window.platform.FBConfig := fgChooseFBConfig;
            fgState.DisplayMode    := fgState.DisplayMode and not GLUT.DOUBLE;
         end if;

         if (fgState.DisplayMode and GLUT.MULTISAMPLE) /= 0 then
            fgState.DisplayMode             := fgState.DisplayMode and not GLUT.MULTISAMPLE ;
            window.Window.platform.FBConfig := fgChooseFBConfig;
            fgState.DisplayMode             := fgState.DisplayMode or GLUT.MULTISAMPLE;
         end if;
      end if;


      if window.Window.platform.FBConfig = null then
         raise constraint_Error with "FBConfig with necessary capabilities not found.";
      end if;


      visualInfo := glXGetVisualFromFBConfig (fgDisplay.platform.Display,               -- Get the X visual.
                                              window.Window.platform.FBConfig.all);

      --       HINT: the masks should be updated when adding/removing callbacks.
      --             This might speed up message processing. Is that true?
      --
      --       A: Not appreciably, but it WILL make it easier to debug.
      --          Try tracing old GLUT and try tracing freeglut.  Old GLUT
      --          turns off events that it doesn't need and is a whole lot
      --          more pleasant to trace.  (Think mouse-motion!  Tons of
      --          ``bonus'' GUI events stream in.)
      --
      winAttr.ev_mask           :=    (Structure_Notify    => True,
                                       Substructure_Notify => True,
                                       Exposure            => True,
                                       Button_Press        => True,
                                       Button_Release      => True,
                                       Key_Press           => True,
                                       Key_Release         => True,
                                       Visibility_Change   => True,
                                       Enter_Window        => True,
                                       Leave_Window        => True,
                                       Pointer_Motion      => True,
                                       Button_Motion       => True,
                                       others              => False);
      winAttr.background_pixmap := Null_Drawable_ID;
      winAttr.background_pixel  := 0;
      winAttr.border_pixel      := 0;


      winAttr.colormap := X_Create_Colormap (fgDisplay.platform.Display,
                                             fgDisplay.platform.root_Window,
                                             visualInfo.visual,
                                             Alloc_None);
      mask := (Back_Pixmap  => True,
               Border_Pixel => True,
               Colormap     => True,
               Ev_Mask      => True,
               others       => False);

      if window.IsMenu  or  gameMode then
         winAttr.override_redirect := True;
         mask.Override_Redirect    := True;
      end if;

      if not positionUse then
         position_X := -1;     -- default window position
         position_Y := -1;
      end if;

      if not sizeUse then
         width  := 300;   -- default window size
         height := 300;
      end if;


      if window.Parent = null then
         window.Window.Handle := X_Create_Window (fgDisplay.platform.Display,
                                                  fgDisplay.platform.root_Window,
                                                  position_X, position_Y,
                                                  width, height, 0,
                                                  visualInfo.depth, Input_Output,
                                                  visualInfo.visual, mask,
                                                  winAttr);
      else
         window.Window.Handle := X_Create_Window (fgDisplay.platform.Display,
                                                  window.Parent.Window.Handle,
                                                  position_X, position_Y,
                                                  width, height, 0,
                                                  visualInfo.depth, Input_Output,
                                                  visualInfo.visual, mask,
                                                  winAttr);
      end if;


      -- The GLX context creation, possibly trying the direct context rendering
      -- or else use the current context if the user has so specified
      --

      if window.IsMenu and (fgStructure.MenuContext = null) then       --     Set renderType.
         renderType := GLX_RGBA_TYPE;              -- Display mode has been set to GLUT_RGB.

      elsif (fgState.DisplayMode and GLUT.INDEX) /= 0 then
         renderType := GLX_COLOR_INDEX_TYPE;
      else
         renderType := GLX_RGBA_TYPE;
      end if;


      if window.IsMenu then

         -- If there isn't already an OpenGL rendering context for menu
         -- windows, make one
         --
         if fgStructure.MenuContext = null then
            fgStructure.MenuContext          := new SFG_MenuContext;
            fgStructure.MenuContext.MContext := glXCreateNewContext (fgDisplay.platform.Display,
                                                                     window.Window.platform.FBConfig.all,
                                                                     renderType,
                                                                     NULL,
                                                                     boolean'Pos (fgState.DirectContext /= GLUT.FORCE_INDIRECT_CONTEXT));
         end if;

         -- /* window->Window.Context = fgStructure.MenuContext->MContext; */
         window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                       window.Window.platform.FBConfig.all,
                                                       renderType,
                                                       NULL,
                                                       boolean'Pos (fgState.DirectContext /= GLUT.FORCE_INDIRECT_CONTEXT));

      elsif fgState.UseCurrentContext then

         window.Window.Context := glXGetCurrentContext;

         if  window.Window.Context = null then
            window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                          window.Window.platform.FBConfig.all,
                                                          renderType,
                                                          NULL,
                                                          boolean'Pos (fgState.DirectContext /= GLUT.FORCE_INDIRECT_CONTEXT));
         else
            window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                          window.Window.platform.FBConfig.all,
                                                          renderType,
                                                          NULL,
                                                          boolean'Pos (fgState.DirectContext /= GLUT.FORCE_INDIRECT_CONTEXT));
         end if;

      end if;



      if not (OS_Platform = free_BSD  or  OS_Platform = net_BSD) then

         if glXIsDirect (fgDisplay.platform.Display,  window.Window.Context) = 0 then

            if fgState.DirectContext = GLUT.FORCE_DIRECT_CONTEXT then
               raise constraint_Error with "Unable to force direct context rendering for window " & title;

            elsif  fgState.DirectContext = GLUT.TRY_DIRECT_CONTEXT then
               raise constraint_Error with   "Unable to create direct context rendering for window" & title
                                           & " ... this may hurt performance.";
            end if;

         end if;

      end if;


      -- Assume the new window is visible by default. Is this a safe assumption?
      --
      window.State.Visible := True;
      sizeHints.flags      := (others => False);

      if positionUse then
         sizeHints.flags.US_Position := True;
      end if;

      if sizeUse then
         sizeHints.flags.US_Size := True;
      end if;


      --        Fill in the size hints values now (the x, y, width and height
      --        settings are obsolete, are there any more WMs that support them?)
      --        Unless the X servers actually stop supporting these, we should
      --        continue to fill them in.  It is *not* our place to tell the user
      --        that they should replace a window manager that they like, and which
      --        works, just because *we* think that it's not "modern" enough.
      --
      sizeHints.x      := Integer (position_x);
      sizeHints.y      := Integer (position_y);
      sizeHints.width  := Integer (width);
      sizeHints.height := Integer (height);

      wmHints.flags := (State => True, others => False);

      if fgState.ForceIconic then
         wmHints.initial_state := property.Iconic;
      else
         wmHints.initial_state := property.Normal;
      end if;



      declare
         textProperty : aliased Text_Property_Type := To_Text_Property (title);      -- Prepare the window and iconified window names
      begin
         X_Set_WM_Properties (fgDisplay.platform.Display,
                              window.Window.Handle,
                              textProperty,
                              textProperty,
                              system.null_Address,
                              0,
                              sizeHints'access,
                              wmHints'access,
                              null);
      end;


      X_Set_WM_Protocols (fgDisplay.platform.Display,
                          window.Window.Handle,
                          (1 => fgDisplay.platform.Delete_Window));


      declare
         unused : Integer;
      begin
         unused := glXMakeContextCurrent (fgDisplay.platform.Display,
                                          glxDrawable (window.Window.Handle),
                                          glxDrawable (window.Window.Handle),
                                          window.Window.Context);
      end;

      X_Map_Window (fgDisplay.platform.Display, window.Window.Handle);

      XFree (visualInfo.all'address);
   end;



   --   Closes a window, destroying the frame and OpenGL context
   --
   procedure fgCloseWindow (window : access SFG_Window)
   is
      use window_Lists;

      used : Boolean := False;
      Cursor : window_Lists.Cursor := fgStructure.Windows.First;

   begin
      --      /* glXDestroyContext( fgDisplay.Display, window->Window.Context ); */

      --       Step through the list of windows.  If the rendering context
      --       is not being used by another window, then we delete it.
      --

      while has_Element (Cursor) loop

         if          Element (Cursor).Window.Context = window.Window.Context
            and then Element (Cursor) /= window
         then
            used := True;
         end if;

         next (Cursor);
      end loop;

      if not used then
         glXDestroyContext( fgDisplay.platform.Display, window.Window.Context );
      end if;

      XFree            (window.Window.platform.FBConfig.all'address);    -- tbd: check/fix this
      X_Destroy_Window (fgDisplay.platform.Display, window.Window.Handle);

      --    /* XFlush( fgDisplay.Display ); */ /* XXX Shouldn't need this */
   end;


--  void fgCloseWindow( SFG_Window* window )
--  {
--  #elif TARGET_HOST_MS_WINDOWS
--
--      /* Make sure we don't close a window with current context active */
--      if( fgStructure.CurrentWindow == window )
--          wglMakeCurrent( NULL, NULL );
--
--      /*
--       * Step through the list of windows.  If the rendering context
--       * is not being used by another window, then we delete it.
--       */
--      {
--          int used = FALSE ;
--          SFG_Window *iter ;
--
--          for( iter = (SFG_Window *)fgStructure.Windows.First;
--               iter;
--               iter = (SFG_Window *)iter->Node.Next )
--          {
--              if( ( iter->Window.Context == window->Window.Context ) &&
--                  ( iter != window ) )
--                  used = TRUE;
--          }
--
--          if( ! used )
--              wglDeleteContext( window->Window.Context );
--      }
--
--      DestroyWindow( window->Window.Handle );
--  #endif
--  }



   procedure fghDisplayAll_RedrawWindow (window : access SFG_Window)
   is
   begin
       fghRedrawWindow (window);

      --  #elif TARGET_HOST_MS_WINDOWS
      --          RedrawWindow(
      --              window->Window.Handle, NULL, NULL,
      --              RDW_NOERASE | RDW_INTERNALPAINT | RDW_INVALIDATE | RDW_UPDATENOW
      --          );
      --  #endif
   end;





   procedure fghReshapeWindow (window : access SFG_Window;   width, height : in Integer)
   is
   begin
      X_Resize_Window (fgDisplay.platform.Display,
                       window.Window.Handle,
                       c.unsigned_Short (width),
                       c.unsigned_Short (height));
      X_Flush (fgDisplay.platform.Display);            -- XXX Shouldn't need this
   end;


--  #elif TARGET_HOST_MS_WINDOWS && !defined(_WIN32_WCE)
--      {
--          RECT winRect;
--          int x, y, w, h;
--
--          /*
--           * For windowed mode, get the current position of the
--           * window and resize taking the size of the frame
--           * decorations into account.
--           */
--
--          /* "GetWindowRect" returns the pixel coordinates of the outside of the window */
--          GetWindowRect( window->Window.Handle, &winRect );
--          x = winRect.left;
--          y = winRect.top;
--          w = width;
--          h = height;
--
--          if ( window->Parent == NULL )
--          {
--              if ( ! window->IsMenu && (window != fgStructure.GameModeWindow) )
--              {
--                  w += GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
--                  h += GetSystemMetrics( SM_CYSIZEFRAME ) * 2 +
--                       GetSystemMetrics( SM_CYCAPTION );
--              }
--          }
--          else
--          {
--              RECT parentRect;
--              GetWindowRect( window->Parent->Window.Handle, &parentRect );
--              x -= parentRect.left + GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
--              y -= parentRect.top  + GetSystemMetrics( SM_CYSIZEFRAME ) * 2 +
--                                     GetSystemMetrics( SM_CYCAPTION );
--          }
--
--          /*
--           * SWP_NOACTIVATE      Do not activate the window
--           * SWP_NOOWNERZORDER   Do not change position in z-order
--           * SWP_NOSENDCHANGING  Supress WM_WINDOWPOSCHANGING message
--           * SWP_NOZORDER        Retains the current Z order (ignore 2nd param)
--           */
--
--          SetWindowPos( window->Window.Handle,
--                        HWND_TOP,
--                        x, y, w, h,
--                        SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
--                        SWP_NOZORDER
--          );
--      }
--  #endif





   procedure SwapBuffers
   is
   begin
      glXSwapBuffers (fgDisplay.platform.Display,
                      glxDrawable (fgStructure.CurrentWindow.Window.Handle));
   end;

--  #elif TARGET_HOST_MS_WINDOWS
--      SwapBuffers( fgStructure.CurrentWindow->Window.Device );
--  #endif





   procedure DeInit_pre      -- nb: this is a no-op for any platform but X11
   is
   begin
      --  Note that the MVisualInfo is not owned by the MenuContext!
      --
      glXDestroyContext( fgDisplay.platform.Display, fgStructure.MenuContext.MContext);
   end;




   procedure DeInit_mid      -- nb: this is needed by all platforms except _WIN32_WCE
   is
   begin
      --        if fgState.JoysticksInitialised then    -- tbd: deferred.
      --           fgJoystickClose;
      --        end if;

      if fgState.InputDevsInitialised then
         fgInputDeviceClose;
      end if;
   end;





   procedure DeInit_post
   is
   begin
      X_Set_Close_Down_Mode (fgDisplay.platform.Display, Destroy_All);   -- Make sure all X-client data we have created
                                                                         -- will be destroyed on display closing

      X_Close_Display (fgDisplay.platform.Display);   -- Close the display connection, destroying all windows we have created so far
   end;


--  #elif TARGET_HOST_MS_WINDOWS
--
--      /* Reset the timer granularity */
--      timeEndPeriod ( 1 );
--
--  #endif








--      /* This code was repeated constantly, so here it goes into a definition: */
--  #define GETWINDOW(a)                             \
--      window = fgWindowByHandle( event.a.window ); \
--      if( window == NULL )                         \
--          break;
--
--  #define GETMOUSE(a)                              \
--      window->State.MouseX = event.a.x;            \
--      window->State.MouseY = event.a.y;


   procedure MainLoopEvent
   is
      Window : SFG_Window_view;
      Event  : X_Event;


      procedure resize_Window (Width, Height : in Integer)
      is
         current_window : SFG_Window_view := fgStructure.CurrentWindow;
      begin
         if window = null then return; end if;    -- debug

         if    width  /= window.State.OldWidth
            or height /= window.State.OldHeight
         then

            window.State.OldWidth  := width;
            window.State.OldHeight := height;

            if window.callbacks.CB_Reshape /= null then
               window.callbacks.CB_Reshape (width, height);
            else
               fgSetWindow (window);
               gl.Viewport (0, 0, gl.Sizei (width), gl.Sizei (height));
            end if;

            glut.PostRedisplay;

            if window.IsMenu then
               fgSetWindow (current_window);
            end if;
         end if;

      end;

   begin
      while X_Pending (fgDisplay.platform.Display) /= 0 loop
         X_Next_Event (fgDisplay.platform.Display, event);

         case event.ev_type is
           when Client_Message =>       -- Destroy the window when the WM_DELETE_WINDOW message arrives
               declare
                  closed_Event : x_lib.X_Event (Client_Message) := event;
               begin
                  if x_lib.Atom (closed_Event.X_Client.data (1)) = fgDisplay.platform.Delete_Window then
                     window := fgWindowByHandle (fgStructure.Windows,
                                                 closed_Event.X_Client.window);

                     fgDestroyWindow (window);

                     if fgState.ActionOnWindowClose = GLUT.ACTION_EXIT then
                        fgDeinitialize;
                        raise constraint_Error; --tbd: what to do here ? ... -- exit( 0 );

                     elsif fgState.ActionOnWindowClose = GLUT.ACTION_GLUTMAINLOOP_RETURNS then
                        fgState.ExecState := EXEC_STATE_STOP;
                     end if;

                     return;
                  end if;
               end;


               --               * CreateNotify causes a configure-event so that sub-windows are
               --               * handled compatibly with GLUT.  Otherwise, your sub-windows
               --               * (in freeglut only) will not get an initial reshape event,
               --               * which can break things.
               --               *
               --               * GLUT presumably does this because it generally tries to treat
               --               * sub-windows the same as windows.
               --               */
            when Create_Notify =>
               declare
                  create_Event : x_lib.X_Event (Create_Notify) := event;
               begin
                  resize_Window (Integer (create_Event.x_create_window.width),
                                 Integer (create_Event.x_create_window.height));
               end;


            when Configure_Notify =>
               declare
                  configure_Event : x_lib.X_Event (Configure_Notify) := event;
               begin
                  resize_Window (Integer (configure_Event.X_Configure.width),
                                 Integer (configure_Event.X_Configure.height));
               end;



            when others =>
               null;
         end case;

      end loop;
   end;

--  {
--      while( XPending( fgDisplay.Display ) )
--      {
--          switch( event.type )
--          {
--
--          case DestroyNotify:
--              /*
--               * This is sent to confirm the XDestroyWindow call.
--               *
--               * XXX WHY is this commented out?  Should we re-enable it?
--               */
--              /* fgAddToWindowDestroyList ( window ); */
--              break;
--
--          case Expose:
--              /*
--               * We are too dumb to process partial exposes...
--               *
--               * XXX Well, we could do it.  However, it seems to only
--               * XXX be potentially useful for single-buffered (since
--               * XXX double-buffered does not respect viewport when we
--               * XXX do a buffer-swap).
--               *
--               */
--              if( event.xexpose.count == 0 )
--              {
--                  GETWINDOW( xexpose );
--                  window->State.Redisplay = GL_TRUE;
--              }
--              break;
--
--          case MapNotify:
--              break;
--
--          case UnmapNotify:
--              /* We get this when iconifying a window. */
--              GETWINDOW( xunmap );
--              INVOKE_WCB( *window, WindowStatus, ( GLUT_HIDDEN ) );
--              window->State.Visible = GL_FALSE;
--              break;
--
--          case MappingNotify:
--              /*
--               * Have the client's keyboard knowledge updated (xlib.ps,
--               * page 206, says that's a good thing to do)
--               */
--              XRefreshKeyboardMapping( (XMappingEvent *) &event );
--              break;
--
--          case VisibilityNotify:
--          {
--              /*
--               * Sending this event, the X server can notify us that the window
--               * has just acquired one of the three possible visibility states:
--               * VisibilityUnobscured, VisibilityPartiallyObscured or
--               * VisibilityFullyObscured. Note that we DO NOT receive a
--               * VisibilityNotify event when iconifying a window, we only get an
--               * UnmapNotify then.
--               */
--              GETWINDOW( xvisibility );
--              switch( event.xvisibility.state )
--              {
--              case VisibilityUnobscured:
--                  INVOKE_WCB( *window, WindowStatus, ( GLUT_FULLY_RETAINED ) );
--                  window->State.Visible = GL_TRUE;
--                  break;
--
--              case VisibilityPartiallyObscured:
--                  INVOKE_WCB( *window, WindowStatus,
--                              ( GLUT_PARTIALLY_RETAINED ) );
--                  window->State.Visible = GL_TRUE;
--                  break;
--
--              case VisibilityFullyObscured:
--                  INVOKE_WCB( *window, WindowStatus, ( GLUT_FULLY_COVERED ) );
--                  window->State.Visible = GL_FALSE;
--                  break;
--
--              default:
--                  fgWarning( "Unknown X visibility state: %d",
--                             event.xvisibility.state );
--                  break;
--              }
--          }
--          break;
--
--          case EnterNotify:
--          case LeaveNotify:
--              GETWINDOW( xcrossing );
--              GETMOUSE( xcrossing );
--              if( ( event.type == LeaveNotify ) && window->IsMenu &&
--                  window->ActiveMenu && window->ActiveMenu->IsActive )
--                  fgUpdateMenuHighlight( window->ActiveMenu );
--
--              INVOKE_WCB( *window, Entry, ( ( EnterNotify == event.type ) ?
--                                            GLUT_ENTERED :
--                                            GLUT_LEFT ) );
--              break;
--
--          case MotionNotify:
--          {
--              GETWINDOW( xmotion );
--              GETMOUSE( xmotion );
--
--              if( window->ActiveMenu )
--              {
--                  if( window == window->ActiveMenu->ParentWindow )
--                  {
--                      window->ActiveMenu->Window->State.MouseX =
--                          event.xmotion.x_root - window->ActiveMenu->X;
--                      window->ActiveMenu->Window->State.MouseY =
--                          event.xmotion.y_root - window->ActiveMenu->Y;
--                  }
--
--                  fgUpdateMenuHighlight( window->ActiveMenu );
--
--                  break;
--              }
--
--              /*
--               * XXX For more than 5 buttons, just check {event.xmotion.state},
--               * XXX rather than a host of bit-masks?  Or maybe we need to
--               * XXX track ButtonPress/ButtonRelease events in our own
--               * XXX bit-mask?
--               */
--  	    fgState.Modifiers = fghGetXModifiers( event.xmotion.state );
--              if ( event.xmotion.state & ( Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask ) ) {
--                  INVOKE_WCB( *window, Motion, ( event.xmotion.x,
--                                                 event.xmotion.y ) );
--              } else {
--                  INVOKE_WCB( *window, Passive, ( event.xmotion.x,
--                                                  event.xmotion.y ) );
--  	    }
--  	    fgState.Modifiers = INVALID_MODIFIERS;
--          }
--          break;
--
--          case ButtonRelease:
--          case ButtonPress:
--          {
--              GLboolean pressed = GL_TRUE;
--              int button;
--
--              if( event.type == ButtonRelease )
--                  pressed = GL_FALSE ;
--
--              /*
--               * A mouse button has been pressed or released. Traditionally,
--               * break if the window was found within the freeglut structures.
--               */
--              GETWINDOW( xbutton );
--              GETMOUSE( xbutton );
--
--              /*
--               * An X button (at least in XFree86) is numbered from 1.
--               * A GLUT button is numbered from 0.
--               * Old GLUT passed through buttons other than just the first
--               * three, though it only gave symbolic names and official
--               * support to the first three.
--               */
--              button = event.xbutton.button - 1;
--
--              /*
--               * Do not execute the application's mouse callback if a menu
--               * is hooked to this button.  In that case an appropriate
--               * private call should be generated.
--               */
--              if( fgCheckActiveMenu( window, button, pressed,
--                                     event.xbutton.x_root, event.xbutton.y_root ) )
--                  break;
--
--              /*
--               * Check if there is a mouse or mouse wheel callback hooked to the
--               * window
--               */
--              if( ! FETCH_WCB( *window, Mouse ) &&
--                  ! FETCH_WCB( *window, MouseWheel ) )
--                  break;
--
--              fgState.Modifiers = fghGetXModifiers( event.xbutton.state );
--
--              /* Finally execute the mouse or mouse wheel callback */
--              if( ( button < glutDeviceGet ( GLUT_NUM_MOUSE_BUTTONS ) ) || ( ! FETCH_WCB( *window, MouseWheel ) ) )
--                  INVOKE_WCB( *window, Mouse, ( button,
--                                                pressed ? GLUT_DOWN : GLUT_UP,
--                                                event.xbutton.x,
--                                                event.xbutton.y )
--                  );
--              else
--              {
--                  /*
--                   * Map 4 and 5 to wheel zero; EVEN to +1, ODD to -1
--                   *  "  6 and 7 "    "   one; ...
--                   *
--                   * XXX This *should* be behind some variables/macros,
--                   * XXX since the order and numbering isn't certain
--                   * XXX See XFree86 configuration docs (even back in the
--                   * XXX 3.x days, and especially with 4.x).
--                   *
--                   * XXX Note that {button} has already been decremeted
--                   * XXX in mapping from X button numbering to GLUT.
--                   */
--                  int wheel_number = (button - glutDeviceGet ( GLUT_NUM_MOUSE_BUTTONS )) / 2;
--                  int direction = -1;
--                  if( button % 2 )
--                      direction = 1;
--
--                  if( pressed )
--                      INVOKE_WCB( *window, MouseWheel, ( wheel_number,
--                                                         direction,
--                                                         event.xbutton.x,
--                                                         event.xbutton.y )
--                      );
--              }
--              fgState.Modifiers = INVALID_MODIFIERS;
--          }
--          break;
--
--          case KeyRelease:
--          case KeyPress:
--          {
--              FGCBKeyboard keyboard_cb;
--              FGCBSpecial special_cb;
--
--              GETWINDOW( xkey );
--              GETMOUSE( xkey );
--
--              /* Detect auto repeated keys, if configured globally or per-window */
--
--              if ( fgState.KeyRepeat==GLUT_KEY_REPEAT_OFF || window->State.IgnoreKeyRepeat==GL_TRUE )
--              {
--                  if (event.type==KeyRelease)
--                  {
--                      /*
--                       * Look at X11 keystate to detect repeat mode.
--                       * While X11 says the key is actually held down, we'll ignore KeyRelease/KeyPress pairs.
--                       */
--
--                      char keys[32];
--                      XQueryKeymap( fgDisplay.Display, keys ); /* Look at X11 keystate to detect repeat mode */
--
--                      if ( event.xkey.keycode<256 )            /* XQueryKeymap is limited to 256 keycodes    */
--                      {
--                          if ( keys[event.xkey.keycode>>3] & (1<<(event.xkey.keycode%8)) )
--                              window->State.KeyRepeating = GL_TRUE;
--                          else
--                              window->State.KeyRepeating = GL_FALSE;
--                      }
--                  }
--              }
--              else
--                  window->State.KeyRepeating = GL_FALSE;
--
--              /* Cease processing this event if it is auto repeated */
--
--              if (window->State.KeyRepeating)
--              {
--                  if (event.type == KeyPress) window->State.KeyRepeating = GL_FALSE;
--                  break;
--              }
--
--              if( event.type == KeyPress )
--              {
--                  keyboard_cb = (FGCBKeyboard)( FETCH_WCB( *window, Keyboard ));
--                  special_cb  = (FGCBSpecial) ( FETCH_WCB( *window, Special  ));
--              }
--              else
--              {
--                  keyboard_cb = (FGCBKeyboard)( FETCH_WCB( *window, KeyboardUp ));
--                  special_cb  = (FGCBSpecial) ( FETCH_WCB( *window, SpecialUp  ));
--              }
--
--              /* Is there a keyboard/special callback hooked for this window? */
--              if( keyboard_cb || special_cb )
--              {
--                  XComposeStatus composeStatus;
--                  char asciiCode[ 32 ];
--                  KeySym keySym;
--                  int len;
--
--                  /* Check for the ASCII/KeySym codes associated with the event: */
--                  len = XLookupString( &event.xkey, asciiCode, sizeof(asciiCode),
--                                       &keySym, &composeStatus
--                  );
--
--                  /* GLUT API tells us to have two separate callbacks... */
--                  if( len > 0 )
--                  {
--                      /* ...one for the ASCII translateable keypresses... */
--                      if( keyboard_cb )
--                      {
--                          fgSetWindow( window );
--                          fgState.Modifiers = fghGetXModifiers( event.xkey.state );
--                          keyboard_cb( asciiCode[ 0 ],
--                                       event.xkey.x, event.xkey.y
--                          );
--                          fgState.Modifiers = INVALID_MODIFIERS;
--                      }
--                  }
--                  else
--                  {
--                      int special = -1;
--
--                      /*
--                       * ...and one for all the others, which need to be
--                       * translated to GLUT_KEY_Xs...
--                       */
--                      switch( keySym )
--                      {
--                      case XK_F1:     special = GLUT_KEY_F1;     break;
--                      case XK_F2:     special = GLUT_KEY_F2;     break;
--                      case XK_F3:     special = GLUT_KEY_F3;     break;
--                      case XK_F4:     special = GLUT_KEY_F4;     break;
--                      case XK_F5:     special = GLUT_KEY_F5;     break;
--                      case XK_F6:     special = GLUT_KEY_F6;     break;
--                      case XK_F7:     special = GLUT_KEY_F7;     break;
--                      case XK_F8:     special = GLUT_KEY_F8;     break;
--                      case XK_F9:     special = GLUT_KEY_F9;     break;
--                      case XK_F10:    special = GLUT_KEY_F10;    break;
--                      case XK_F11:    special = GLUT_KEY_F11;    break;
--                      case XK_F12:    special = GLUT_KEY_F12;    break;
--
--                      case XK_Left:   special = GLUT_KEY_LEFT;   break;
--                      case XK_Right:  special = GLUT_KEY_RIGHT;  break;
--                      case XK_Up:     special = GLUT_KEY_UP;     break;
--                      case XK_Down:   special = GLUT_KEY_DOWN;   break;
--
--                      case XK_KP_Prior:
--                      case XK_Prior:  special = GLUT_KEY_PAGE_UP; break;
--                      case XK_KP_Next:
--                      case XK_Next:   special = GLUT_KEY_PAGE_DOWN; break;
--                      case XK_KP_Home:
--                      case XK_Home:   special = GLUT_KEY_HOME;   break;
--                      case XK_KP_End:
--                      case XK_End:    special = GLUT_KEY_END;    break;
--                      case XK_KP_Insert:
--                      case XK_Insert: special = GLUT_KEY_INSERT; break;
--                      }
--
--                      /*
--                       * Execute the callback (if one has been specified),
--                       * given that the special code seems to be valid...
--                       */
--                      if( special_cb && (special != -1) )
--                      {
--                          fgSetWindow( window );
--                          fgState.Modifiers = fghGetXModifiers( event.xkey.state );
--                          special_cb( special, event.xkey.x, event.xkey.y );
--                          fgState.Modifiers = INVALID_MODIFIERS;
--                      }
--                  }
--              }
--          }
--          break;
--
--          case ReparentNotify:
--              break; /* XXX Should disable this event */
--
--          /* Not handled */
--          case GravityNotify:
--              break;
--
--          default:
--              fgWarning ("Unknown X event type: %d\n", event.type);
--              break;
--          }
--      }





--  #elif TARGET_HOST_MS_WINDOWS
--
--      MSG stMsg;
--
--      FREEGLUT_EXIT_IF_NOT_INITIALISED ( "glutMainLoopEvent" );
--
--      while( PeekMessage( &stMsg, NULL, 0, 0, PM_NOREMOVE ) )
--      {
--          if( GetMessage( &stMsg, NULL, 0, 0 ) == 0 )
--          {
--              if( fgState.ActionOnWindowClose == GLUT_ACTION_EXIT )
--              {
--                  fgDeinitialize( );
--                  exit( 0 );
--              }
--              else if( fgState.ActionOnWindowClose == GLUT_ACTION_GLUTMAINLOOP_RETURNS )
--                  fgState.ExecState = GLUT_EXEC_STATE_STOP;
--
--              return;
--          }
--
--          TranslateMessage( &stMsg );
--          DispatchMessage( &stMsg );
--      }
--  #endif




end glut.Platform;


