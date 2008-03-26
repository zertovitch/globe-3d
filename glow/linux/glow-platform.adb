
with glow.Internal;   use glow.Internal;
with opengl.glx;      use opengl.glx;

with x_lib.Property;
with x_lib.Predefined_Atoms;   use x_lib.Predefined_Atoms;
with x_lib.Util;               use x_lib.Util;
with x_lib.key_Syms;
with x_lib.Cursor;

with ada.containers.doubly_linked_Lists;
with ada.Strings.unbounded;
with ada.Calendar;
with ada.characters.latin_1;

with interfaces.c.Pointers;



package body glow.Platform is


   package C renames interfaces.C;
   use interfaces.C, interfaces.C.Strings;

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

      if (fgState.DisplayMode and glow.INDEX) /= 0 then
         ATTRIB_VAL (GLX_BUFFER_SIZE, 8);          --  Buffer size is selected later.
         ATTRIB_VAL (GLX_RENDER_TYPE, GLX_COLOR_INDEX_BIT );

         wantIndexedMode := True;
      else
         ATTRIB_VAL (GLX_RED_SIZE,   1);
         ATTRIB_VAL (GLX_GREEN_SIZE, 1);
         ATTRIB_VAL (GLX_BLUE_SIZE,  1);

         if (fgState.DisplayMode and glow.ALPHA) /= 0 then
            ATTRIB_VAL (GLX_ALPHA_SIZE, 1);
         end if;

      end if;


      if (fgState.DisplayMode and glow.DOUBLE) /= 0 then
         ATTRIB_VAL (GLX_DOUBLEBUFFER, 1);
      end if;

      if (fgState.DisplayMode and glow.STEREO) /= 0 then
         ATTRIB_VAL (GLX_STEREO, 1);   -- 1 => True
      end if;

      if (fgState.DisplayMode and glow.DEPTH) /= 0 then
         ATTRIB_VAL (GLX_DEPTH_SIZE, 1);
      end if;

      if (fgState.DisplayMode and glow.STENCIL) /= 0 then
         ATTRIB_VAL (GLX_STENCIL_SIZE, 1);
      end if;


      if (fgState.DisplayMode and glow.ACCUM) /= 0 then
         ATTRIB_VAL (GLX_ACCUM_RED_SIZE,   1);
         ATTRIB_VAL (GLX_ACCUM_GREEN_SIZE, 1);
         ATTRIB_VAL (GLX_ACCUM_BLUE_SIZE,  1);

         if (fgState.DisplayMode and glow.ALPHA) /= 0 then
            ATTRIB_VAL (GLX_ACCUM_ALPHA_SIZE, 1);
         end if;
      end if;


      if   (fgState.DisplayMode and glow.AUX)  /= 0
        or (fgState.DisplayMode and glow.AUX1) /= 0
        or (fgState.DisplayMode and glow.AUX2) /= 0
        or (fgState.DisplayMode and glow.AUX3) /= 0
        or (fgState.DisplayMode and glow.AUX4) /= 0
      then
         ATTRIB_VAL (GLX_AUX_BUFFERS, fgState.AuxiliaryBufferNumber);
      end if;


      if (fgState.DisplayMode and glow.MULTISAMPLE) /= 0 then
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
      Status : Integer;
   begin
      if Window /= null then
         Status := glXMakeContextCurrent (fgDisplay.platform.Display,
                                          glxDrawable (window.Window.Handle),
                                          glxDrawable (window.Window.Handle),
                                          window.Window.Context);

         if Status /= 1 then
            raise Constraint_Error with "unable to set GL context with 'glXMakeContextCurrent'";
         end if;
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
      current_DisplayMode : glow.unSigned := fgState.DisplayMode;

   begin
      if window.IsMenu and (fgStructure.MenuContext = null) then
         fgState.DisplayMode := glow.DOUBLE or glow.RGB ;           -- Save the display mode, if we are creating a menu window
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
         if (fgState.DisplayMode and glow.DOUBLE) = 0 then
            fgState.DisplayMode    := fgState.DisplayMode or glow.DOUBLE ;
            window.Window.platform.FBConfig := fgChooseFBConfig;
            fgState.DisplayMode    := fgState.DisplayMode and not glow.DOUBLE;
         end if;

         if (fgState.DisplayMode and glow.MULTISAMPLE) /= 0 then
            fgState.DisplayMode             := fgState.DisplayMode and not glow.MULTISAMPLE ;
            window.Window.platform.FBConfig := fgChooseFBConfig;
            fgState.DisplayMode             := fgState.DisplayMode or glow.MULTISAMPLE;
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

      elsif (fgState.DisplayMode and glow.INDEX) /= 0 then
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
                                                                     boolean'Pos (fgState.DirectContext /= glow.FORCE_INDIRECT_CONTEXT));
         end if;

         -- /* window->Window.Context = fgStructure.MenuContext->MContext; */
         window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                       window.Window.platform.FBConfig.all,
                                                       renderType,
                                                       NULL,
                                                       boolean'Pos (fgState.DirectContext /= glow.FORCE_INDIRECT_CONTEXT));

      elsif fgState.UseCurrentContext then

         window.Window.Context := glXGetCurrentContext;

         if  window.Window.Context = null then
            window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                          window.Window.platform.FBConfig.all,
                                                          renderType,
                                                          NULL,
                                                          boolean'Pos (fgState.DirectContext /= glow.FORCE_INDIRECT_CONTEXT));
         end if;
      else
         window.Window.Context := glXCreateNewContext (fgDisplay.platform.Display,
                                                       window.Window.platform.FBConfig.all,
                                                       renderType,
                                                       NULL,
                                                       boolean'Pos (fgState.DirectContext /= glow.FORCE_INDIRECT_CONTEXT));

      end if;



      if not (OS_Platform = free_BSD  or  OS_Platform = net_BSD) then

         if glXIsDirect (fgDisplay.platform.Display,  window.Window.Context) = 0 then

            if fgState.DirectContext = glow.FORCE_DIRECT_CONTEXT then
               raise constraint_Error with "Unable to force direct context rendering for window " & title;

            elsif  fgState.DirectContext = glow.TRY_DIRECT_CONTEXT then
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






   -- Returns GLUT modifier mask for the state field of an X11 event.
   --
   function fghGetXModifiers (state : in Modifier_And_Button_Mask) return c.Unsigned
   is
      ret : c.Unsigned := 0;
   begin
      if state.Shift or state.Lock then
         ret := ret or glow.ACTIVE_SHIFT;
      end if;

      if state.Control then
         ret := ret or glow.ACTIVE_CTRL;
      end if;

      if state.Mod1 then
         ret := ret or glow.ACTIVE_ALT;
      end if;

      return ret;
   end;







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
      Window :         SFG_Window_view;
      Event  : aliased X_Event;


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
               internal.fgSetWindow (window);
               gl.Viewport (0,  0,  gl.Sizei (width),  gl.Sizei (height));
            end if;

            glow.PostRedisplay;

            if window.IsMenu then
               internal.fgSetWindow (current_window);
            end if;
         end if;

      end resize_Window;


   begin
      while X_Pending (fgDisplay.platform.Display) /= 0 loop
         X_Next_Event (fgDisplay.platform.Display, event);

         case event.ev_type is

           when Client_Message =>       -- Destroy the window when the WM_DELETE_WINDOW message arrives
               declare
                  closed_Event : x_lib.X_Event (Client_Message) := event;
               begin
                  if x_lib.Atom (closed_Event.X_Client.data (1)) = fgDisplay.platform.Delete_Window then
                     window := fgWindowByHandle (fgStructure.Windows,  closed_Event.X_Client.window);

                     fgDestroyWindow (window);

                     if fgState.ActionOnWindowClose = glow.ACTION_EXIT then
                        fgDeinitialize;
                        raise constraint_Error; --tbd: what to do here ? ... -- exit( 0 );

                     elsif fgState.ActionOnWindowClose = glow.ACTION_GLUTMAINLOOP_RETURNS then
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
            when Create_Notify
               | Configure_Notify =>
               declare
                  Width,
                  Height : Integer;
               begin

                  if event.ev_type = Create_Notify then
                     declare
                        create_Event : x_lib.X_Event (Create_Notify) := event;
                     begin
                        window := fgWindowByHandle (fgStructure.Windows, create_Event.x_create_window.window);
                        Width  := Integer (event.x_create_window.Width);
                        Height := Integer (event.x_create_window.Height);
                     end;
                  else
                     declare
                        configure_Event : x_lib.X_Event (Configure_Notify) := event;
                     begin
                        window := fgWindowByHandle (fgStructure.Windows, configure_Event.X_Configure.window);
                        Width  := Integer (event.X_Configure.Width);
                        Height := Integer (event.X_Configure.Height);
                     end;
                  end if;


                  if   width  /= window.State.OldWidth
                    or height /= window.State.OldHeight
                  then
                     declare
                        current_window : SFG_Window_view := fgStructure.CurrentWindow;
                     begin
                        window.State.OldWidth  := width;
                        window.State.OldHeight := height;

                        if window.callbacks.CB_Reshape /= null then
                           internal.fgSetWindow (window);
                           window.callbacks.CB_Reshape (width, height);
                        else
                           fgSetWindow (window);
                           gl.Viewport (0, 0, gl.Sizei (width), gl.Sizei (height));
                        end if;

                        glow.PostRedisplay;

                        if window.IsMenu then
                           internal.fgSetWindow (current_window);
                        end if;
                     end;
                  end if;
--                          resize_Window (Integer (create_Event.x_create_window.width),
--                                         Integer (create_Event.x_create_window.height));
               end;


            when Destroy_Notify =>         -- This is sent to confirm the XDestroyWindow call.
               -- fgAddToWindowDestroyList ( window ); */     -- * XXX WHY is this commented out?  Should we re-enable it?
               null;


            when Expose =>
               --  We are too dumb to process partial exposes...
               --
               --  Well, we could do it.  However, it seems to only be potentially useful for single-buffered (since
               --  double-buffered does not respect viewport when we do a buffer-swap).
               --
               declare
                  expose_Event : x_lib.X_Event (Expose) := event;
               begin
                  if expose_Event.X_Expose.count = 0 then
                     window                 := fgWindowByHandle (fgStructure.Windows,
                                                                 expose_Event.X_Expose.window);  --GETWINDOW( xexpose );
                     window.State.Redisplay := True;
                  end if;
               end;


            when Map_Notify =>
               null;


            when Unmap_Notify =>           -- We get this when iconifying a window.
               declare
                  unmap_Event : x_lib.X_Event (Unmap_Notify) := event;
               begin
                  window := fgWindowByHandle (fgStructure.Windows,  unmap_Event.X_Unmap.window);    --  GETWINDOW( xunmap );

                  internal.fgSetWindow (window);

                  if window.Callbacks.CB_WindowStatus /= null then
                     window.Callbacks.CB_WindowStatus (glow.HIDDEN);
                  end if;

                  window.State.Visible := False;
               end;


            when Mapping_Notify =>      -- Have the client's keyboard knowledge updated (xlib.ps, page 206, says that's
               declare                  -- a good thing to do).
                  unused : c.Int;
               begin
                  unused := X_Refresh_Keyboard_Mapping (event'access);
               end;


            when Visibility_Notify =>
               -- Sending this event, the X server can notify us that the window
               -- has just acquired one of the three possible visibility states:
               -- VisibilityUnobscured, VisibilityPartiallyObscured or
               -- VisibilityFullyObscured. Note that we DO NOT receive a
               -- VisibilityNotify event when iconifying a window, we only get an UnmapNotify then.
               --
               declare
                  visibility_Event : x_lib.X_Event (Visibility_Notify) := event;
               begin
                  window := fgWindowByHandle (fgStructure.Windows,
                                              visibility_Event.X_Visibility.window);   -- GETWINDOW( xvisibility );

                  internal.fgSetWindow (window);

                  case visibility_Event.X_Visibility.state is
                     when Visibility_Unobscured =>
                        window.Callbacks.CB_WindowStatus (glow.FULLY_RETAINED);
                        window.State.Visible := True;

                     when Visibility_Partially_Obscured =>
                        window.Callbacks.CB_WindowStatus (glow.PARTIALLY_RETAINED);
                        window.State.Visible := True;

                     when Visibility_Fully_Obscured =>
                        window.Callbacks.CB_WindowStatus (glow.FULLY_COVERED);
                        window.State.Visible := False;
                  end case;
               end;


            when Enter_Notify =>
               declare
                  enter_Event : x_lib.X_Event (Enter_Notify) := event;
               begin
                  window := fgWindowByHandle (fgStructure.Windows,
                                              enter_Event.X_Crossing.window);   -- GETWINDOW( xcrossing );
                  window.State.MouseX := Integer (enter_Event.X_Crossing.x);
                  window.State.MouseY := Integer (enter_Event.X_Crossing.y);     -- GETMOUSE( xcrossing );

                  internal.fgSetWindow (window);

                  if window.callbacks.CB_Entry /= null then
                     window.callbacks.CB_Entry (glow.ENTERED);
                  end if;
               end;


            when Leave_Notify =>
               declare
                  leave_Event : x_lib.X_Event (Leave_Notify) := event;
               begin
                  window := fgWindowByHandle (fgStructure.Windows,
                                              leave_Event.X_Crossing.window);   -- GETWINDOW( xcrossing );
                  window.State.MouseX := Integer (leave_Event.X_Crossing.x);
                  window.State.MouseY := Integer (leave_Event.X_Crossing.y);              -- GETMOUSE( xcrossing );

                  if         window.IsMenu
                    and then window.ActiveMenu /= null
                    and then window.ActiveMenu.IsActive
                  then
                     null; -- tbd: deferred ... fgUpdateMenuHighlight (window.ActiveMenu);
                  end if;

                  internal.fgSetWindow (window);

                  if window.callbacks.CB_Entry /= null then
                     window.callbacks.CB_Entry (glow.LEFT);
                  end if;
               end;


            when Motion_Notify =>
               declare
                  motion_Event : x_lib.X_Event (Motion_Notify) := event;
               begin
                  window := fgWindowByHandle (fgStructure.Windows,
                                              motion_Event.X_Motion.window);   --  GETWINDOW( xmotion );
                  window.State.MouseX := Integer (motion_Event.X_Motion.x);
                  window.State.MouseY := Integer (motion_Event.X_Motion.y);              -- GETMOUSE( xmotion );

                  if window.ActiveMenu /= null then

                     if window = window.ActiveMenu.ParentWindow then
                        window.ActiveMenu.Window.State.MouseX := Integer (motion_Event.X_Motion.x_root) - window.ActiveMenu.X;
                        window.ActiveMenu.Window.State.MouseY := Integer (motion_Event.X_Motion.y_root) - window.ActiveMenu.Y;
                     end if;

                     -- tbd: deferred ... fgUpdateMenuHighlight (window.ActiveMenu);

                  else
                     -- For more than 5 buttons, just check {event.xmotion.state}, rather than a host of bit-masks ?
                     -- Or maybe we need to track ButtonPress/ButtonRelease events in our own bit-mask ?
                     --
                     fgState.Modifiers := fghGetXModifiers (motion_Event.X_Motion.state);

                     internal.fgSetWindow (window);

                     if   motion_Event.X_Motion.state.Button1
                       or motion_Event.X_Motion.state.Button2
                       or motion_Event.X_Motion.state.Button3
                       or motion_Event.X_Motion.state.Button4
                       or motion_Event.X_Motion.state.Button5
                     then
                        window.callbacks.CB_Motion (Integer (motion_Event.X_Motion.x),
                                                    Integer (motion_Event.X_Motion.y));
                     else
                        window.callbacks.CB_Passive (Integer (motion_Event.X_Motion.x),
                                                     Integer (motion_Event.X_Motion.y));
                     end if;

                     fgState.Modifiers := INVALID_MODIFIERS;
                  end if;
               end;


            when   Button_Press
                 | Button_Release =>
               declare
                  pressed     : Boolean;

                  button      : Integer;
                  State       : Modifier_And_Button_Mask;
                  X, Y        : Integer;
                  X_root,
                  Y_root      : Integer;
                  Window_Id   : x_lib.Drawable_ID;
                  Up_or_Down  : Integer;
               begin
                  -- An X button (at least in XFree86) is numbered from 1.
                  -- A GLUT button is numbered from 0.
                  -- Old GLUT passed through buttons other than just the first three, though it only gave
                  -- symbolic names and official support to the first three.
                  --
                  if event.ev_type = Button_Press then
                     declare
                        press_Event : x_lib.X_Event (Button_Press) := event;
                     begin
                        button     := Button_type'Pos (press_Event.X_Button.button) - 1;
                        X          := Integer (press_Event.X_Button.x);
                        Y          := Integer (press_Event.X_Button.y);
                        X_root     := Integer (press_Event.X_Button.x_root);
                        Y_root     := Integer (press_Event.X_Button.y_root);
                        State      := press_Event.X_Button.state;
                        Window_Id  := press_Event.X_Button.window;
                        Up_or_Down := glow.DOWN;
                        pressed    := True;
                     end;
                  else
                     declare
                        release_Event : x_lib.X_Event (Button_Release) := event;
                     begin
                        button     := Button_type'Pos (release_Event.X_Button.button) - 1;
                        X          := Integer (release_Event.X_Button.x);
                        Y          := Integer (release_Event.X_Button.y);
                        X_root     := Integer (release_Event.X_Button.x_root);
                        Y_root     := Integer (release_Event.X_Button.y_root);
                        State      := release_Event.X_Button.state;
                        Window_Id  := release_Event.X_Button.window;
                        Up_or_Down := glow.UP;
                        pressed    := False;
                     end;
                  end if;


                  -- A mouse button has been pressed or released. Traditionally,
                  -- break if the window was found within the freeglut structures.
                  --
                  window              := fgWindowByHandle (fgStructure.Windows, Window_Id);     --  GETWINDOW( xbutton );
                  window.State.MouseX := Integer (X);
                  window.State.MouseY := Integer (Y);              -- GETMOUSE( xbutton );


                  -- Do not execute the application's mouse callback if a menu is hooked to this button.  In that
                  -- case an appropriate private call should be generated.
                  --
                  if true --tbd: deferred ...    not fgCheckActiveMenu (window,  button,  pressed,
                          --                  X_root,
                          --                  Y_root)
                  then
                     -- Check if there is a mouse or mouse wheel callback hooked to the window.
                     --
                     if   window.callbacks.CB_Mouse      /= null
                       or window.callbacks.CB_MouseWheel /= null
                     then
                        fgState.Modifiers := fghGetXModifiers (State);

                        -- Finally execute the mouse or mouse wheel callback
                        --
                        if   button < glow.DeviceGet (glow.NUM_MOUSE_BUTTONS)
                          or window.callbacks.CB_MouseWheel = null
                        then
                           internal.fgSetWindow (window);
                           window.callbacks.CB_Mouse (button,  Up_or_Down,  Integer (X), Integer (Y));
                        else
                           -- Map 4 and 5 to wheel zero; EVEN to +1, ODD to -1
                           --  "  6 and 7 "    "   one; ...
                           --
                           -- This *should* be behind some variables/macros, since the order and numbering isn't certain
                           -- See XFree86 configuration docs (even back in the 3.x days, and especially with 4.x).
                           --
                           -- Note that {button} has already been decremeted in mapping from X button numbering to GLUT.
                           --
                           declare
                              wheel_number : Integer := (button - glow.DeviceGet (glow.NUM_MOUSE_BUTTONS)) / 2;
                              direction    : Integer := -1;
                           begin
                              if button mod 2 /= 0 then
                                 direction := 1;
                              end if;

                              if pressed then
                                 internal.fgSetWindow (window);
                                 window.callbacks.CB_MouseWheel (wheel_number,  direction,  Integer (X), Integer (Y));
                              end if;
                           end;
                        end if;

                        fgState.Modifiers := INVALID_MODIFIERS;
                     end if;
                  end if;
              end;


            when   Key_Press
                 | Key_Release =>
               declare
                  keyboard_cb : Glut_Proc_4;
                  special_cb  : Glut_Proc_13;
--                    pressed     : Boolean;
--
                    keycode    : c.Size_t;
                    State       : Modifier_And_Button_Mask;
                   X, Y        : Integer;
--                    X_root,
--                    Y_root      : Integer;
                   Window_Id   : x_lib.Drawable_ID;
--                    Up_or_Down  : Integer;
               begin
                  -- An X button (at least in XFree86) is numbered from 1.
                  -- A GLUT button is numbered from 0.
                  -- Old GLUT passed through buttons other than just the first three, though it only gave
                  -- symbolic names and official support to the first three.
                  --
                  if event.ev_type = key_Press then
                     declare
                        press_Event : x_lib.X_Event (key_Press) := event;
                     begin
--                          button     := Button_type'Pos (press_Event.X_Button.button) - 1;
                        X          := Integer (press_Event.X_Key.x);
                        Y          := Integer (press_Event.X_Key.y);
--                          X_root     := Integer (press_Event.X_Button.x_root);
--                          Y_root     := Integer (press_Event.X_Button.y_root);
                          State      := press_Event.X_Key.state;
                         Window_Id  := press_Event.X_Key.window;
--                          Up_or_Down := GLUT.DOWN;
--                          pressed    := True;
                        keycode :=  c.Size_t (press_Event.X_Key.Key_Code);
                     end;
                  else
                     declare
                        release_Event : x_lib.X_Event (key_Release) := event;
                     begin
                        null;
--                          button     := Button_type'Pos (release_Event.X_Button.button) - 1;
                        X          := Integer (release_Event.X_Key.x);
                        Y          := Integer (release_Event.X_Key.y);
--                          X_root     := Integer (release_Event.X_Button.x_root);
--                          Y_root     := Integer (release_Event.X_Button.y_root);
                          State      := release_Event.X_Key.state;
                         Window_Id  := release_Event.X_Key.window;
--                          Up_or_Down := GLUT.UP;
--                          pressed    := False;
                        keycode := c.Size_t (release_Event.X_Key.Key_Code);
                     end;
                  end if;

                  window              := fgWindowByHandle (fgStructure.Windows, Window_Id);     --  GETWINDOW( xkey );
                  window.State.MouseX := Integer (X);
                  window.State.MouseY := Integer (Y);              -- GETMOUSE( xbutton );


                  -- Detect auto repeated keys, if configured globally or per-window
                  --
                  if   fgState.KeyRepeat            = glow.KEY_REPEAT_OFF
                    or window.State.IgnoreKeyRepeat = True
                  then
                     if event.ev_type = Key_Release then                        -- Look at X11 keystate to detect repeat mode.
                        declare                                                 -- While X11 says the key is actually held down,
                           use c.Strings;                                       -- we'll ignore KeyRelease/KeyPress pairs.
                           keys   : aliased c.char_array := (1 .. 32 => <>);
                           unused : c.Int;
                        begin
                           unused := X_Query_Keymap (fgDisplay.platform.Display, to_chars_ptr (keys'unchecked_access));         -- Look at X11 keystate to detect repeat mode

                           if  keycode < 256 then                -- XQueryKeymap is limited to 256 keycodes
                              if (    c.Char'Pos (keys (keycode / 2**3))
                                  and c.unsigned_Char (2 ** Integer (keycode mod 8)))  /=  0
                              then
                                 window.State.KeyRepeating := True;
                              else
                                 window.State.KeyRepeating := False;
                              end if;
                           end if;
                        end;
                     end if;

                  else
                     window.State.KeyRepeating := False;
                  end if;


                  -- Cease processing this event if it is auto repeated

                  if window.State.KeyRepeating then

                     if event.ev_type = Key_Press then
                        window.State.KeyRepeating := False;
                     end if;

                  else

                     if event.ev_type = Key_Press then
                        keyboard_cb := window.callbacks.CB_Keyboard;
                        special_cb  := window.callbacks.CB_Special;
                     else
                        keyboard_cb := glut_proc_4 (window.callbacks.CB_KeyboardUp);   -- tbd: tidy up callback duplicates
                        special_cb  := glut_proc_13 (window.callbacks.CB_SpecialUp);
                     end if;


                     -- Is there a keyboard/special callback hooked for this window?

                     if keyboard_cb /= null  or  special_cb /= null then
                        declare
                           --composeStatus : X_Compose_Status;
                           asciiCode     : aliased c.char_array := (1 .. 32 => <>);
                           keySym        : aliased Key_Sym_Id;
                           len           : Integer;
                        begin
                           -- Check for the ASCII/KeySym codes associated with the event:
                           --
                           len := X_Lookup_String (event'access, to_Chars_ptr (asciiCode'unchecked_access), asciiCode'Length,
                                                   keySym'access); --, composeStatus);
--                             len := X_Lookup_String (&event.xkey, asciiCode, sizeof(asciiCode),
--                                                     &keySym, &composeStatus);

                           if len > 0 then                    -- GLUT API tells us to have two separate callbacks ...

                              if keyboard_cb /= null then    -- ... one for the ASCII translateable keypresses ...
                                 internal.fgSetWindow (window);
                                 fgState.Modifiers := fghGetXModifiers (event.X_Key.state);
                                 keyboard_cb (c.Char'Pos (asciiCode (1)),  x, y);
                                 fgState.Modifiers := INVALID_MODIFIERS;
                              end if;

                           else                  -- ... and one for all the others, which need to be translated to GLUT_KEY_Xs...
                              declare
                                 use x_lib.key_Syms;
                                 special : Integer := -1;
                              begin
                                 case keySym is
                                    when XK_F1 =>     special := glow.KEY_F1;
                                    when XK_F2 =>     special := glow.KEY_F2;
                                    when XK_F3 =>     special := glow.KEY_F3;
                                    when XK_F4 =>     special := glow.KEY_F4;
                                    when XK_F5 =>     special := glow.KEY_F5;
                                    when XK_F6 =>     special := glow.KEY_F6;
                                    when XK_F7 =>     special := glow.KEY_F7;
                                    when XK_F8 =>     special := glow.KEY_F8;
                                    when XK_F9 =>     special := glow.KEY_F9;
                                    when XK_F10 =>    special := glow.KEY_F10;
                                    when XK_F11 =>    special := glow.KEY_F11;
                                    when XK_F12 =>    special := glow.KEY_F12;

                                    when XK_Left =>   special := glow.KEY_LEFT;
                                    when XK_Right =>  special := glow.KEY_RIGHT;
                                    when XK_Up =>     special := glow.KEY_UP;
                                    when XK_Down =>   special := glow.KEY_DOWN;

                                    when XK_KP_Prior
                                       | XK_Prior =>  special := glow.KEY_PAGE_UP;
                                    when XK_KP_Next
                                       | XK_Next =>   special := glow.KEY_PAGE_DOWN;
                                    when XK_KP_Home
                                       | XK_Home =>   special := glow.KEY_HOME;
                                    when XK_KP_End
                                       | XK_End =>    special := glow.KEY_END;
                                    when XK_KP_Insert
                                       | XK_Insert => special := glow.KEY_INSERT;

                                    when others =>
                                       null;   -- tbd: warning message ?
                                 end case;

                                 -- Execute the callback (if one has been specified), given that the special code seems to be valid...
                                 --
                                 if special_cb /= null  and   special /= -1 then
                                    internal.fgSetWindow (window);
                                    fgState.Modifiers := fghGetXModifiers (event.X_Key.state);
                                    special_cb (special, x, y);
                                    fgState.Modifiers := INVALID_MODIFIERS;
                                 end if;
                              end;
                           end if;

                        end;
                     end if;

                  end if;
               end;


            when Reparent_Notify =>
               null;                            -- XXX Should disable this event


            when Gravity_Notify =>                   -- Not handled
               null;


            when others =>
               raise program_Error with "Unknown X event type:" & event_type'Image (event.ev_type);
         end case;

      end loop;
   end;



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





   function DeviceGet_has_Keyboard return Integer
   is
   begin
      return 1;
   end;

--  #if defined(_WIN32_CE)
--          return ( GetKeyboardStatus() & KBDI_KEYBOARD_PRESENT ) ? 1 : 0;
--  #   if FREEGLUT_LIB_PRAGMAS
--  #       pragma comment (lib,"Kbdui.lib")
--  #   endif




   function DeviceGet_has_Mouse return Integer
   is
   begin
      return 1;   -- X11 has a mouse by definition
   end;

--          /*
--           * MS Windows can be booted without a mouse.
--           */
--          return GetSystemMetrics( SM_MOUSEPRESENT );





   function DeviceGet_num_mouse_Buttons return Integer
   is
      use c.Strings;
      Map      : aliased c.Char_array := (1 => <>);
      nbuttons : c.Int := X_Get_Pointer_Mapping (fgDisplay.platform.Display, to_Chars_Ptr (Map'unchecked_access), 0);
   begin
      -- We should be able to pass NULL when the last argument is zero,
      -- but at least one X server has a bug where this causes a segfault.
      --
      -- In XFree86/Xorg servers, a mouse wheel is seen as two buttons
      -- rather than an Axis; "freeglut_main.c" expects this when
      -- checking for a wheel event.
      --
      return Integer (nbuttons);
   end;


--  #elif TARGET_HOST_MS_WINDOWS
--
--      case GLUT_NUM_MOUSE_BUTTONS:
--  #  if defined(_WIN32_WCE)
--          return 1;
--  #  else
--          return GetSystemMetrics( SM_CMOUSEBUTTONS );
--  #  endif
--
--  #endif





   -- Queries the GL context about some attributes
   --
   function fghGetConfig (attribute : in Integer) return Integer
   is
      returnValue : aliased Integer := 0;
      result      : Integer;                   -- Not checked
   begin
      if fgStructure.CurrentWindow /= null then
         result := glXGetFBConfigAttrib (fgDisplay.platform.Display,
                                         fgStructure.CurrentWindow.Window.platform.FBConfig.all,
                                         attribute,
                                         returnValue'unchecked_access);
      end if;

      return returnValue;
   end;





   procedure WarpPointer (X : Integer; Y : Integer)
   is
      unused : c.Int;
   begin
      unused := X_Warp_Pointer (fgDisplay.platform.Display,
                                Drawable_Id (null_XID),
                                fgStructure.CurrentWindow.Window.Handle,
                                0, 0, 0, 0,
                                c.Int (x),  c.Int (y));

      X_Flush (fgDisplay.platform.Display);        -- Make the warp visible immediately.
   end;


--  #elif TARGET_HOST_MS_WINDOWS
--
--      {
--          POINT coords;
--          coords.x = x;
--          coords.y = y;
--
--          /* ClientToScreen() translates {coords} for us. */
--          ClientToScreen( fgStructure.CurrentWindow->Window.Handle, &coords );
--          SetCursorPos( coords.x, coords.y );
--      }
--
--  #endif






   procedure SetWindowTitle (Title : String)
   is
      text    : x_lib.property.Text_Property_type (format => Bits_8, length => Title'length);
      Value_8 : Bits_8_Array_Type (1 .. Title'Length);
   begin
      for Each in text.value_8'range loop
         text.value_8 (Each) := Character'Pos (Title (Each));
      end loop;

      text.encoding := XA_STRING;

      x_lib.property.X_Set_WM_Name (fgDisplay.platform.Display,
                                    fgStructure.CurrentWindow.Window.Handle,
                                    text);

      X_Flush (fgDisplay.platform.Display);     -- /* XXX Shouldn't need this */
   end;


--  #elif TARGET_HOST_MS_WINDOWS
--  #    ifdef _WIN32_WCE
--          {
--              wchar_t* wstr = fghWstrFromStr(title);
--              SetWindowText( fgStructure.CurrentWindow->Window.Handle, wstr );
--              free(wstr);
--          }
--  #    else
--          SetWindowText( fgStructure.CurrentWindow->Window.Handle, title );
--  #    endif
--
--  #endif




   procedure SetIconTitle (Title : String)
   is
      text    : x_lib.property.Text_Property_type (format => Bits_8, length => Title'length);
      Value_8 : Bits_8_Array_Type (1 .. Title'Length);
   begin
      for Each in text.value_8'range loop
         text.value_8 (Each) := Character'Pos (Title (Each));
      end loop;

      text.encoding := XA_STRING;

      x_lib.property.X_Set_WM_Icon_Name (fgDisplay.platform.Display,
                                         fgStructure.CurrentWindow.Window.Handle,
                                         text);

      X_Flush (fgDisplay.platform.Display);     -- /* XXX Shouldn't need this */

   end SetIconTitle;


--  #elif TARGET_HOST_MS_WINDOWS
--  #    ifdef _WIN32_WCE
--          {
--              wchar_t* wstr = fghWstrFromStr(title);
--              SetWindowText( fgStructure.CurrentWindow->Window.Handle, wstr );
--              free(wstr);
--          }
--  #    else
--          SetWindowText( fgStructure.CurrentWindow->Window.Handle, title );
--  #    endif
--
--  #endif




   -- A factory method for an empty cursor
   --
   function getEmptyCursor return Cursor_Id
   is
      cursorNone : Cursor_Id := Null_Cursor_ID;
   begin
      if cursorNone = Null_Cursor_ID then
         declare
            cursorNoneBits   : Bitmap_Type (1..4, 1..4) := (others => (others => 0));
            dontCare         : X_Color := (Pix   => 0,
                                           Red   => 0,
                                           Green => 0,
                                           Blue  => 0,
                                           Flags => (others => False),
                                           Pad   => 0);
            cursorNonePixmap : Pixmap_Id;
         begin
            cursorNonePixmap := X_Create_Bitmap_From_Data (fgDisplay.platform.Display,
                                                           fgDisplay.platform.root_Window,
                                                           cursorNoneBits);
            if cursorNonePixmap /= Null_pixmap_ID then
               cursorNone := x_lib.cursor.X_Create_Pixmap_Cursor (fgDisplay.platform.Display,
                                                     cursorNonePixmap, cursorNonePixmap,
                                                     dontCare, dontCare, 0, 0 );
               X_Free_Pixmap (fgDisplay.platform.Display, cursorNonePixmap);
            end if;
         end;
      end if;

      return cursorNone;
   end;






   type cursorCacheEntry is
      record
         cursorShape : x_lib.cursor.Font_Cursor_Shape;         -- an XC_foo value
         cachedCursor : Cursor_Id;      -- one if the corresponding cursor has not been created yet
      end record;

   type cursorCacheEntry_array is array (Cursor_Id range <>) of aliased cursorCacheEntry;

   --   * Note: The arrangement of the table below depends on the fact that
   --   * the "normal" GLUT_CURSOR_* values start a 0 and are consecutive.
   --   */

   -- tbd: not task safe !!
   cursorCache : cursorCacheEntry_array := ( 1 => (x_lib.Cursor.XC_arrow,               Null_Cursor_ID),   -- GLUT_CURSOR_RIGHT_ARROW
                                             2 => (x_lib.Cursor.XC_top_left_arrow,      Null_Cursor_ID),   -- GLUT_CURSOR_LEFT_ARROW
                                             3 => (x_lib.Cursor.XC_hand1,               Null_Cursor_ID),   -- GLUT_CURSOR_INFO
                                             4 => (x_lib.Cursor.XC_pirate,              Null_Cursor_ID),   -- GLUT_CURSOR_DESTROY
                                             5 => (x_lib.Cursor.XC_question_arrow,      Null_Cursor_ID),   -- GLUT_CURSOR_HELP
                                             6 => (x_lib.Cursor.XC_exchange,            Null_Cursor_ID),   -- GLUT_CURSOR_CYCLE
                                             7 => (x_lib.Cursor.XC_spraycan,            Null_Cursor_ID),   -- GLUT_CURSOR_SPRAY
                                             8 => (x_lib.Cursor.XC_watch,               Null_Cursor_ID),   -- GLUT_CURSOR_WAIT
                                             9 => (x_lib.Cursor.XC_xterm,               Null_Cursor_ID),   -- GLUT_CURSOR_TEXT
                                            10 => (x_lib.Cursor.XC_crosshair,           Null_Cursor_ID),   -- GLUT_CURSOR_CROSSHAIR
                                            11 => (x_lib.Cursor.XC_sb_v_double_arrow  , Null_Cursor_ID),   -- GLUT_CURSOR_UP_DOWN
                                            12 => (x_lib.Cursor.XC_sb_h_double_arrow,   Null_Cursor_ID),   -- GLUT_CURSOR_LEFT_RIGHT
                                            13 => (x_lib.Cursor.XC_top_side,            Null_Cursor_ID),   -- GLUT_CURSOR_TOP_SIDE
                                            14 => (x_lib.Cursor.XC_bottom_side,         Null_Cursor_ID),   -- GLUT_CURSOR_BOTTOM_SIDE
                                            15 => (x_lib.Cursor.XC_left_side,           Null_Cursor_ID),   -- GLUT_CURSOR_LEFT_SIDE
                                            16 => (x_lib.Cursor.XC_right_side,          Null_Cursor_ID),   -- GLUT_CURSOR_RIGHT_SIDE
                                            17 => (x_lib.Cursor.XC_top_left_corner,     Null_Cursor_ID),   -- GLUT_CURSOR_TOP_LEFT_CORNER
                                            18 => (x_lib.Cursor.XC_top_right_corner,    Null_Cursor_ID),   -- GLUT_CURSOR_TOP_RIGHT_CORNER
                                            19 => (x_lib.Cursor.XC_bottom_right_corner, Null_Cursor_ID),   -- GLUT_CURSOR_BOTTOM_RIGHT_CORNER
                                            20 => (x_lib.Cursor.XC_bottom_left_corner,  Null_Cursor_ID));   -- GLUT_CURSOR_BOTTOM_LEFT_CORNER




   procedure fgSetCursor (window : access internal.SFG_Window;   cursorID : Integer)
   is
       cursor        : Cursor_Id;
       cursorIDToUse : Cursor_Id;
   begin
      --           * XXX FULL_CROSSHAIR demotes to plain CROSSHAIR. Old GLUT allows
      --           * for this, but if there is a system that easily supports a full-
      --           * window (or full-screen) crosshair, we might consider it.
      --
      if cursorID = glow.CURSOR_FULL_CROSSHAIR then
         cursorIDToUse := glow.CURSOR_CROSSHAIR;
      else
         cursorIDToUse := Cursor_Id (cursorID);
      end if;


      if    cursorIDToUse >= 0
        and cursorIDToUse <= cursorCache'Last
      then
         declare
            the_Entry : access cursorCacheEntry := cursorCache (cursorIDToUse)'access;
         begin
            if the_entry.cachedCursor = Null_Cursor_ID then
               the_entry.cachedCursor := x_lib.cursor.X_Create_Font_Cursor (fgDisplay.platform.Display, the_entry.cursorShape);
            end if;
            cursor := the_entry.cachedCursor;
         end;

      else
         case cursorIDToUse is
            when glow.CURSOR_NONE =>
               cursor := getEmptyCursor;

            when glow.CURSOR_INHERIT =>
               cursor := Null_Cursor_ID;

            when others =>
               raise constraint_Error with "Unknown cursor type: " & cursor_Id'image (cursorIDToUse);
         end case;
      end if;

   end;

--      {
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


--          if ( cursorIDToUse == GLUT_CURSOR_INHERIT ) {
--              XUndefineCursor( fgDisplay.Display, window->Window.Handle );
--          } else if ( cursor != None ) {
--              XDefineCursor( fgDisplay.Display, window->Window.Handle, cursor );
--          } else if ( cursorIDToUse != GLUT_CURSOR_NONE ) {
--              fgError( "Failed to create cursor" );
--          }
--      }




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







   -- Toggle the window's full screen state.
   --
   procedure FullScreenToggle
   is
   begin

      if fgDisplay.platform.State_fullScreen /= null_Atom then
         declare
            xevent     : X_Event (Client_Message);
         begin
            xevent.X_Client.serial := 0;       -- tbd: use an aggregate
            xevent.X_Client.send_event := True;
            xevent.X_Client.display := fgDisplay.platform.Display;
            xevent.X_Client.window := fgStructure.CurrentWindow.Window.Handle;
            xevent.X_Client.message_type := fgDisplay.platform.State;
            xevent.X_Client.format := long_Format;
            xevent.X_Client.data (1) := 2;  -- _NET_WM_STATE_TOGGLE
            xevent.X_Client.data (2) := c.Long (fgDisplay.Platform.State_fullScreen);
            xevent.X_Client.data (3) := 0;
            xevent.X_Client.data (4) := 0;
            xevent.X_Client.data (5) := 0;

            X_Send_Event (fgDisplay.platform.Display,
                          fgDisplay.platform.root_Window,
                          False,
                          (Substructure_Redirect => True,  -- /*** Don't really understand how event masks work... ***/
                           Substructure_Notify   => True,
                           others                => False),   --event_mask,
                          xevent);
         end;
      else
         glow.FullScreen; --    * If the window manager is not Net WM compliant, fall back to legacy behaviour.
      end if;

   end;


--  void FGAPIENTRY glutFullScreenToggle( void )
--  {
--      {
--  #if TARGET_HOST_POSIX_X11
--
--        if (fgDisplay.StateFullScreen != None)
--        {
--          XEvent xevent;
--          long event_mask;
--          int status;
--
--          xevent.type = ClientMessage;
--          xevent.xclient.type = ClientMessage;
--          xevent.xclient.serial = 0;
--          xevent.xclient.send_event = True;
--          xevent.xclient.display = fgDisplay.Display;
--          xevent.xclient.window = fgStructure.CurrentWindow->Window.Handle;
--          xevent.xclient.message_type = fgDisplay.State;
--          xevent.xclient.format = 32;
--          xevent.xclient.data.l[0] = 2;  /* _NET_WM_STATE_TOGGLE */
--          xevent.xclient.data.l[1] = fgDisplay.StateFullScreen;
--          xevent.xclient.data.l[2] = 0;
--          xevent.xclient.data.l[3] = 0;
--          xevent.xclient.data.l[4] = 0;
--
--          /*** Don't really understand how event masks work... ***/
--          event_mask = SubstructureRedirectMask | SubstructureNotifyMask;
--
--          status = XSendEvent(fgDisplay.Display,
--            fgDisplay.RootWindow,
--            False,
--            event_mask,
--            &xevent);
--          FREEGLUT_INTERNAL_ERROR_EXIT(status != 0,
--            "XSendEvent failed",
--            "glutFullScreenToggle");
--        }
--        else
--  #endif
--        {
--          /*
--           * If the window manager is not Net WM compliant, fall back to legacy
--           * behaviour.
--           */
--          glutFullScreen();
--        }
--      }
--  }










   function Get (Type_Id : GL.enum) return Integer
   is
      nsamples : aliased gl.Int := 0;
   begin
      -- The window/context specific queries are handled mostly by fghGetConfig().
      --
      case Type_Id is

         when glow.WINDOW_NUM_SAMPLES =>
            if GLX_VERSION_1_3 /= 0 then
               gl.GetIntegerv (GL.SAMPLES, nsamples'unchecked_access);
            end if;

            return Integer (nsamples);


         when glow.WINDOW_RGBA =>
            return fghGetConfig (GLX_RGBA);

         when glow.WINDOW_DOUBLEBUFFER =>
            return fghGetConfig (GLX_DOUBLEBUFFER);

         when glow.WINDOW_BUFFER_SIZE =>
            return fghGetConfig (GLX_BUFFER_SIZE);

         when glow.WINDOW_STENCIL_SIZE =>
            return fghGetConfig (GLX_STENCIL_SIZE);

         when glow.WINDOW_DEPTH_SIZE =>
            return fghGetConfig (GLX_DEPTH_SIZE);

         when glow.WINDOW_RED_SIZE =>
            return fghGetConfig (GLX_RED_SIZE);

         when glow.WINDOW_GREEN_SIZE =>
            return fghGetConfig (GLX_GREEN_SIZE);

         when glow.WINDOW_BLUE_SIZE =>
            return fghGetConfig (GLX_BLUE_SIZE);

         when glow.WINDOW_ALPHA_SIZE =>
            return fghGetConfig (GLX_ALPHA_SIZE);

         when glow.WINDOW_ACCUM_RED_SIZE =>
            return fghGetConfig (GLX_ACCUM_RED_SIZE);

         when glow.WINDOW_ACCUM_GREEN_SIZE =>
            return fghGetConfig (GLX_ACCUM_GREEN_SIZE);

         when glow.WINDOW_ACCUM_BLUE_SIZE =>
            return fghGetConfig (GLX_ACCUM_BLUE_SIZE);

         when glow.WINDOW_ACCUM_ALPHA_SIZE =>
            return fghGetConfig (GLX_ACCUM_ALPHA_SIZE);

         when glow.WINDOW_STEREO =>
            return fghGetConfig (GLX_STEREO);


         when glow.WINDOW_COLORMAP_SIZE =>        -- Colormap size is handled in a bit different way than all the rest
            if   fghGetConfig (GLX_RGBA)  /= 0
              or fgStructure.CurrentWindow = null
            then                                       -- We've got a RGBA visual, so there is no colormap at all.
               return 0;                               -- The other possibility is that we have no current window set.
            else
               declare
                  fbconfig   : access GLXFBConfig   := fgStructure.CurrentWindow.Window.platform.FBConfig;
                  visualInfo : access X_Visual_Info := glXGetVisualFromFBConfig (fgDisplay.platform.Display, fbconfig.all);
                  result     : constant Integer     := Integer (visualInfo.visual.map_entries);
               begin
                  XFree (visualInfo.all'address);
                  return result;
               end;
            end if;


         when glow.WINDOW_X             -- Those calls are somewhat similiar, as they use XGetWindowAttributes function
            | glow.WINDOW_Y
            | glow.WINDOW_BORDER_WIDTH
            | glow.WINDOW_HEADER_HEIGHT =>
            declare
               use type gl.Enum;
               x, y   : aliased c.Int;
               w      : aliased x_lib.Window_Id;
               unused :         Boolean;
            begin
               if fgStructure.CurrentWindow = null then
                  return 0;
               end if;

               unused := X_Translate_Coordinates (fgDisplay.platform.Display,
                                                  fgStructure.CurrentWindow.Window.Handle,
                                                  fgDisplay.platform.root_Window,
                                                  0, 0, x'access, y'access, w'access);

               if    Type_Id = glow.WINDOW_X then   return Integer (x);
               elsif Type_Id = glow.WINDOW_Y then   return Integer (y);
               end if;

               if w = 0 then
                  return 0;
               end if;

               unused := X_Translate_Coordinates (fgDisplay.platform.Display,
                                                  fgStructure.CurrentWindow.Window.Handle,
                                                  w, 0, 0, x'access, y'access, w'access);

               if    Type_Id = glow.WINDOW_BORDER_WIDTH  then   return Integer (x);
               elsif Type_Id = glow.WINDOW_HEADER_HEIGHT then   return Integer (y);
               else                                             raise  program_Error;
               end if;
            end;


         when glow.WINDOW_WIDTH
            | glow.WINDOW_HEIGHT =>
            declare
               use type gl.Enum;
               winAttributes : aliased X_Window_Attributes;
               unused        : x_lib.Status;
            begin
               if fgStructure.CurrentWindow = null then
                  return 0;
               end if;

               unused := X_Get_Window_Attributes (fgDisplay.platform.Display,
                                                  fgStructure.CurrentWindow.Window.Handle,
                                                  winAttributes'access);

               if Type_Id  =  glow.WINDOW_WIDTH then
                  return Integer (winAttributes.width);
               else
                  return Integer (winAttributes.height);
               end if;
            end;



         when glow.DISPLAY_MODE_POSSIBLE =>       -- I don't know yet if there will be a fgChooseVisual() function for Win32
            declare
               fbconfig   : access GLXFBConfig;
               isPossible :        Integer;
            begin
               fbconfig := fgChooseFBConfig;       -- We should not have to call fgChooseFBConfig again here.

               if fbconfig = null then
                  isPossible := 0;
               else
                  isPossible := 1;
                  XFree (fbconfig.all'address);
               end if;

               return isPossible;
            end;


         when glow.WINDOW_FORMAT_ID =>                    -- This is system-dependant
            if fgStructure.CurrentWindow = null then
               return 0;
            end if;

            return fghGetConfig (GLX_VISUAL_ID);


         when others =>
            raise program_Error with "glutGet(): missing enum handle " & gl.enum'Image (Type_Id);

      end case;


--      return 0;
   end;




--  #elif TARGET_HOST_MS_WINDOWS
--      int returnValue ;
--      GLboolean boolValue ;
--
--      case GLUT_WINDOW_NUM_SAMPLES:
--        glGetIntegerv(WGL_SAMPLES_ARB, &nsamples);
--        return nsamples;
--
--      /* Handle the OpenGL inquiries */
--      case GLUT_WINDOW_RGBA:
--        glGetBooleanv ( GL_RGBA_MODE, &boolValue );
--        returnValue = boolValue ? 1 : 0;
--        return returnValue;
--      case GLUT_WINDOW_DOUBLEBUFFER:
--        glGetBooleanv ( GL_DOUBLEBUFFER, &boolValue );
--        returnValue = boolValue ? 1 : 0;
--        return returnValue;
--      case GLUT_WINDOW_STEREO:
--        glGetBooleanv ( GL_STEREO, &boolValue );
--        returnValue = boolValue ? 1 : 0;
--        return returnValue;
--
--      case GLUT_WINDOW_RED_SIZE:
--        glGetIntegerv ( GL_RED_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_GREEN_SIZE:
--        glGetIntegerv ( GL_GREEN_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_BLUE_SIZE:
--        glGetIntegerv ( GL_BLUE_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_ALPHA_SIZE:
--        glGetIntegerv ( GL_ALPHA_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_ACCUM_RED_SIZE:
--        glGetIntegerv ( GL_ACCUM_RED_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_ACCUM_GREEN_SIZE:
--        glGetIntegerv ( GL_ACCUM_GREEN_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_ACCUM_BLUE_SIZE:
--        glGetIntegerv ( GL_ACCUM_BLUE_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_ACCUM_ALPHA_SIZE:
--        glGetIntegerv ( GL_ACCUM_ALPHA_BITS, &returnValue );
--        return returnValue;
--      case GLUT_WINDOW_DEPTH_SIZE:
--        glGetIntegerv ( GL_DEPTH_BITS, &returnValue );
--        return returnValue;
--
--      case GLUT_WINDOW_BUFFER_SIZE:
--        returnValue = 1 ;                                      /* ????? */
--        return returnValue;
--      case GLUT_WINDOW_STENCIL_SIZE:
--        returnValue = 0 ;                                      /* ????? */
--        return returnValue;
--
--      case GLUT_WINDOW_X:
--      case GLUT_WINDOW_Y:
--      case GLUT_WINDOW_WIDTH:
--      case GLUT_WINDOW_HEIGHT:
--      {
--          /*
--           *  There is considerable confusion about the "right thing to
--           *  do" concerning window  size and position.  GLUT itself is
--           *  not consistent between Windows and UNIX/X11; since
--           *  platform independence is a virtue for "freeglut", we
--           *  decided to break with GLUT's behaviour.
--           *
--           *  Under UNIX/X11, it is apparently not possible to get the
--           *  window border sizes in order to subtract them off the
--           *  window's initial position until some time after the window
--           *  has been created.  Therefore we decided on the following
--           *  behaviour, both under Windows and under UNIX/X11:
--           *  - When you create a window with position (x,y) and size
--           *    (w,h), the upper left hand corner of the outside of the
--           *    window is at (x,y) and the size of the drawable area  is
--           *    (w,h).
--           *  - When you query the size and position of the window--as
--           *    is happening here for Windows--"freeglut" will return
--           *    the size of the drawable area--the (w,h) that you
--           *    specified when you created the window--and the coordinates
--           *    of the upper left hand corner of the drawable
--           *    area--which is NOT the (x,y) you specified.
--           */
--
--          RECT winRect;
--
--          freeglut_return_val_if_fail( fgStructure.CurrentWindow != NULL, 0 );
--
--          /*
--           * We need to call GetWindowRect() first...
--           *  (this returns the pixel coordinates of the outside of the window)
--           */
--          GetWindowRect( fgStructure.CurrentWindow->Window.Handle, &winRect );
--
--          /* ...then we've got to correct the results we've just received... */
--
--  #if !defined(_WIN32_WCE)
--          if ( ( fgStructure.GameModeWindow != fgStructure.CurrentWindow ) && ( fgStructure.CurrentWindow->Parent == NULL ) &&
--               ( ! fgStructure.CurrentWindow->IsMenu ) )
--          {
--            winRect.left   += GetSystemMetrics( SM_CXSIZEFRAME );
--            winRect.right  -= GetSystemMetrics( SM_CXSIZEFRAME );
--            winRect.top    += GetSystemMetrics( SM_CYSIZEFRAME ) + GetSystemMetrics( SM_CYCAPTION );
--            winRect.bottom -= GetSystemMetrics( SM_CYSIZEFRAME );
--          }
--  #endif /* !defined(_WIN32_WCE) */
--
--          switch( eWhat )
--          {
--          case GLUT_WINDOW_X:      return winRect.left                ;
--          case GLUT_WINDOW_Y:      return winRect.top                 ;
--          case GLUT_WINDOW_WIDTH:  return winRect.right - winRect.left;
--          case GLUT_WINDOW_HEIGHT: return winRect.bottom - winRect.top;
--          }
--      }
--      break;
--
--      case GLUT_WINDOW_BORDER_WIDTH :
--  #if defined(_WIN32_WCE)
--          return 0;
--  #else
--          return GetSystemMetrics( SM_CXSIZEFRAME );
--  #endif /* !defined(_WIN32_WCE) */
--
--      case GLUT_WINDOW_HEADER_HEIGHT :
--  #if defined(_WIN32_WCE)
--          return 0;
--  #else
--          return GetSystemMetrics( SM_CYCAPTION );
--  #endif /* defined(_WIN32_WCE) */
--
--      case GLUT_DISPLAY_MODE_POSSIBLE:
--  #if defined(_WIN32_WCE)
--          return 0;
--  #else
--          return fgSetupPixelFormat( fgStructure.CurrentWindow, GL_TRUE,
--                                      PFD_MAIN_PLANE );
--  #endif /* defined(_WIN32_WCE) */
--
--
--      case GLUT_WINDOW_FORMAT_ID:
--  #if !defined(_WIN32_WCE)
--          if( fgStructure.CurrentWindow != NULL )
--              return GetPixelFormat( fgStructure.CurrentWindow->Window.Device );
--  #endif /* defined(_WIN32_WCE) */
--          return 0;
--
--  #endif (WINDOWS)
--












end glow.Platform;


