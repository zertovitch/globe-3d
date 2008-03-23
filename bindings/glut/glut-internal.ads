
with glut.Platform;

with ada.containers.doubly_linked_Lists;
with ada.Strings.unbounded;
with ada.Calendar;

with ada.unchecked_Deallocation;



private package glut.Internal is



   INVALID_MODIFIERS : constant := Integer'Last;   -- 16#ffffffff#;




   -- A helper structure holding two ints and a boolean
   --
   type SFG_XYUse is
      record
         X, Y : Integer;
         Used : Boolean;
      end record;







   --    An enumeration containing the state of the GLUT execution:
   --    initializing, running, or stopping
   --
   type fgExecutionState is (EXEC_STATE_INIT,
                             EXEC_STATE_RUNNING,
                             EXEC_STATE_STOP);





   type SFG_State is
      record
         Position    : SFG_XYUse;              -- The default windows' position
         Size        : SFG_XYUse;              -- The default windows' size
         DisplayMode : glut.Unsigned;          -- Display mode for new windows

         Initialised : Boolean := False;                -- freeglut has been initialised
         ForceIconic : Boolean := False;                -- New top windows are iconified


         DirectContext : Integer;              -- Direct rendering state

         UseCurrentContext : Boolean;          -- New windows share with current




         ExecState : fgExecutionState;        -- Used for GLUT termination

         ProgramName : ada.strings.unbounded.unbounded_String;         -- Name of the invoking program


         JoysticksInitialised : Boolean;        -- Only initialize if application calls for them
         InputDevsInitialised : Boolean;        -- Only initialize if application calls for them

         GLDebugSwitch : Boolean;        -- OpenGL state debugging switch
         XSyncSwitch   : Boolean;        -- X11 sync protocol switch

         KeyRepeat : Integer;                  -- Global key repeat mode.
         Modifiers : interfaces.c.Unsigned;    -- Current ALT/SHIFT/CTRL state

         Time : ada.calendar.Time;                 -- Time that glutInit was called
         --      SFG_List         Timers;               /* The freeglut timer hooks       */
         --      SFG_List         FreeTimers;           /* The unused timer hooks         */


         ActionOnWindowClose : Integer;        -- Action when user closes window

         AuxiliaryBufferNumber : Integer;      -- Number of auxiliary buffers
         SampleNumber          : Integer;      -- Number of samples per pixel


         GameModeSize : SFG_XYUse;     -- Game mode screen's dimensions
         GameModeDepth : Integer;      -- The pixel depth for game mode
         GameModeRefresh : Integer;    -- The refresh rate for game mode


               FPSInterval : gl.uInt;          -- Interval between FPS printfs   tbd: porbably useless
               SwapCount   : gl.uInt;          -- Count of glutSwapBuffer calls
               SwapTime    : gl.uInt;          -- Time of last SwapBuffers

      end record;



--  /* This structure holds different freeglut settings */
--  typedef struct tagSFG_State SFG_State;
--  struct tagSFG_State
--  {
--
--
--
--
--
--      FGCBIdle         IdleCallback;         /* The global idle callback       */
--
--      int              ActiveMenus;          /* Num. of currently active menus */
--      FGCBMenuState    MenuStateCallback;    /* Menu callbacks are global      */
--      FGCBMenuStatus   MenuStatusCallback;
--
--
--
--
--  };





   -- Callbacks
   --
   --   * The window callbacks the user can supply us with. Should be kept portable.
   --   *
   --   * This enumeration provides the freeglut CallBack numbers.
   --   * The symbolic constants are indices into a window's array of
   --   * function callbacks.  The names are formed by splicing a common
   --   * prefix onto the callback's base name.  (This was originally
   --   * done so that an early stage of development could live side-by-
   --   * side with the old callback code.  The old callback code used
   --   * the bare callback's name as a structure member, so I used a
   --   * prefix for the array index name.)
   --   *
   --   * XXX For consistancy, perhaps the prefix should match the
   --   * XXX FETCH* and INVOKE* macro suffices.  I.e., WCB_, rather than
   --   * XXX CB_.
   --
--     type callback_Kind is (CB_Display,
--                            CB_Reshape,
--                            CB_Keyboard,
--                            CB_KeyboardUp,
--                            CB_Special,
--                            CB_SpecialUp,
--                            CB_Mouse,
--                            CB_MouseWheel,
--                            CB_Motion,
--                            CB_Passive,
--                            CB_Entry,
--                            CB_Visibility,
--                            CB_WindowStatus,
--                            CB_Joystick,
--                            CB_Destroy,
--
--                            -- Presently ignored
--                            --
--                            CB_Select,
--                            CB_OverlayDisplay,
--                            CB_SpaceMotion,
--                            CB_SpaceRotation,
--                            CB_SpaceButton,
--                            CB_Dials,
--                            CB_ButtonBox,
--                            CB_TabletMotion,
--                            CB_TabletButton);


--     type FGCBKeyboard is access procedure (arg1 : Character;   arg2, arg3 : Integer);
--     type FGCBSpecial  is access procedure (arg1, arg2, arg3 : Integer);




   --  A generic function pointer.  We should really use the GLUTproc type
   --  defined in freeglut_ext.h, but if we include that header in this file
   --  a bunch of other stuff (font-related) blows up!
   --
   type SFG_Proc is access procedure;

   type callback_Set is
      record
         CB_Display      : SFG_Proc;
         CB_Reshape      : Glut_Proc_3;
         CB_Keyboard     : Glut_Proc_4;
         CB_KeyboardUp   : Glut_KeyUpFunc;
         CB_Special      : Glut_Proc_13;
         CB_SpecialUp    : Glut_SpecialUp;
         CB_Mouse        : Glut_Proc_5;
         CB_MouseWheel   : Glut_Proc_5;
         CB_Motion       : Glut_Proc_6;
         CB_Passive      : Glut_Proc_7;
         CB_Entry        : Glut_Proc_8;
         CB_Visibility   : SFG_Proc;
         CB_WindowStatus : Glut_Proc_23;
         CB_Joystick     : SFG_Proc;
         CB_Destroy      : SFG_Proc;

         -- Presently ignored
         --
         CB_Select         : SFG_Proc;
         CB_OverlayDisplay : SFG_Proc;
         CB_SpaceMotion    : SFG_Proc;
         CB_SpaceRotation  : SFG_Proc;
         CB_SpaceButton    : SFG_Proc;
         CB_Dials          : SFG_Proc;
         CB_ButtonBox      : SFG_Proc;
         CB_TabletMotion   : SFG_Proc;
         CB_TabletButton   : SFG_Proc;
      end record;




   -- Context
   --
   -- A window and its OpenGL context. The contents of this structure
   -- are highly dependant on the target operating system we aim at...
   --
   type SFG_Context is
      record
         Handle   : platform.SFG_WindowHandleType;       -- The window's handle
         Context  : platform.SFG_WindowContextType;     --  The window's OpenGL/WGL context

         Platform : glut.platform.SFG_Context;     -- Platform specific state.

         DoubleBuffered : Boolean;              -- Treat the window as double-buffered
      end record;






   -- Windows
   --

   -- Window's state description. This structure should be kept portable.
   --
   type SFG_WindowState is
      record
         OldWidth  : Integer := -1;          -- Window width and height from before a resize
         OldHeight : Integer := -1;          --

         Width     : Integer;              --  Window's width in pixels
         Height    : Integer;              -- The same about the height

         Redisplay    : Boolean := False;          -- Do we have to redisplay?
         Visible      : Boolean := False;          -- Is the window visible now
         NeedToResize : Boolean := False;          -- Do we need to resize the window ?

         Cursor : Integer;             -- The currently selected cursor

         IgnoreKeyRepeat : Boolean;    -- Whether to ignore key repeat.
         KeyRepeating    : Boolean;    -- Currently in repeat mode

         MouseX,
         MouseY          : Integer;    -- The most recent mouse position

         JoystickPollRate : Integer;        -- The joystick polling rate
         JoystickLastPoll : long_Integer;   -- When the last poll happened
      end record;


--  typedef struct tagSFG_WindowState SFG_WindowState;
--  struct tagSFG_WindowState
--  {
--
--
--
--
--  };

   type SFG_Menu;

   type SFG_Window;
   type SFG_Window_view is access all SFG_Window;


   package window_Lists is new ada.containers.doubly_linked_Lists (SFG_Window_view);
   subtype window_List is window_Lists.List;




   -- A window, making part of freeglut windows hierarchy.
   -- Should be kept portable.
   --
   -- NOTE: that ActiveMenu is set to menu itself if the window is a menu.
   --
   type SFG_Window is
      record
         State : SFG_WindowState;                  -- The window state

         ID    : Integer;                          -- Window's ID number

         Window : SFG_Context;                     -- Window and OpenGL context

         Parent   : SFG_Window_view;               -- The parent to this window
         Children : window_List;                   -- The subwindows d.l. list

         IsMenu     : Boolean;                     -- Set to True if we are a menu
         ActiveMenu : access SFG_Menu;             -- The window's active menu

         Callbacks : callback_Set;                 -- Array of window callbacks;

         UserData : system.Address;                -- For use by user
      end record;


--  /*
--   */
--  struct tagSFG_Window
--  {
--      SFG_Node            Node;
--
--
--      SFG_Menu*       Menu[ FREEGLUT_MAX_MENUS ]; /* Menus appended to window  */
--
--
--  };




   -- Menus
   --

   type SFG_Menu is
      record
         ID : Integer;              -- The global menu ID

         IsActive : Boolean;        -- Is the menu selected?

         X, Y     : Integer;        -- Menu box raster position


         ParentWindow : SFG_Window_view;     -- Window in which the menu is invoked
         Window       : SFG_Window_view;     -- Window for menu
      end record;

--  struct tagSFG_Menu
--  {
--      SFG_Node            Node;
--      void               *UserData;     /* User data passed back at callback   */
--      SFG_List            Entries;      /* The menu entries list               */
--      FGCBMenu            Callback;     /* The menu callback                   */
--      FGCBDestroy         Destroy;      /* Destruction callback                */
--      int                 Width;        /* Menu box width in pixels            */
--      int                 Height;       /* Menu box height in pixels           */
--
--      SFG_MenuEntry      *ActiveEntry;  /* Currently active entry in the menu  */
--  };
--
--  /* This is a menu entry */
--  struct tagSFG_MenuEntry
--  {
--      SFG_Node            Node;
--      int                 ID;                     /* The menu entry ID (local) */
--      int                 Ordinal;                /* The menu's ordinal number */
--      char*               Text;                   /* The text to be displayed  */
--      SFG_Menu*           SubMenu;                /* Optional sub-menu tree    */
--      GLboolean           IsActive;               /* Is the entry highlighted? */
--      int                 Width;                  /* Label's width in pixels   */
--  };
--




   -- This structure holds the OpenGL rendering context for all the menu windows
   --
   type SFG_MenuContext is
      record
         MContext : platform.SFG_WindowContextType;      -- The menu window's WGL context
      end record;


   type SFG_MenuContext_view is access all SFG_MenuContext;

   procedure free is new ada.unchecked_Deallocation (SFG_MenuContext, SFG_MenuContext_view);




   -- This holds information about all the windows, menus etc.
   --
   type SFG_Structure is
      record
         Windows          : window_List;             -- The global windows list
         WindowsToDestroy : window_List;

         CurrentWindow : SFG_Window_view;   -- The currently set window
         WindowID      : Integer := 0;      -- The new current window ID

         MenuContext   : SFG_MenuContext_view;    -- OpenGL rendering context for menus

      end record;



--  typedef struct tagSFG_Structure SFG_Structure;
--  struct tagSFG_Structure
--  {
--      SFG_List        Menus;           /* The global menus list              */
--
--      SFG_Menu*       CurrentMenu;     /* Same, but menu...                 */
--
--
--      SFG_Window*      GameModeWindow; /* The game mode window               */
--
--      int              MenuID;         /* The new current menu ID            */  := 0;
--  };




   -- The structure used by display initialization in freeglut_init.c
   --

   type SFG_Display is limited
      record
         ScreenWidth    : Integer;        -- The screen's width in pixels
         ScreenHeight   : Integer;        -- The screen's height in pixels
         ScreenWidthMM  : Integer;        -- The screen's width in milimeters
         ScreenHeightMM : Integer;        -- The screen's height in milimeters

         Platform       : glut.platform.SFG_Display (SFG_Display'access);   -- platform specific state.
      end record;








   -- GLOBAL VARIABLES
   --
   fgDisplay   : SFG_Display;               -- Freeglut display related stuff (initialized once per session)
   fgStructure : SFG_Structure;             -- Freeglut internal structure
   fgState     : SFG_State;                 -- The current freeglut settings





   function fgWindowByHandle (Windows       : in window_List;
                              Window_Handle : in platform.SFG_WindowHandleType) return SFG_Window_view;


   procedure fgSetWindow (window : access SFG_Window);


   procedure fgDestroyWindow (Window : in SFG_Window_view);

   procedure fgCloseWindows;

   procedure fgDeinitialize;


   procedure fgInputDeviceClose;    -- probably useless !

end glut.Internal;





--  /*
--   * freeglut_internal.h
--   *
--   * The freeglut library private include file.
--   *
--   * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
--   * Written by Pawel W. Olszta, <olszta@sourceforge.net>
--   * Creation date: Thu Dec 2 1999
--   *
--   * Permission is hereby granted, free of charge, to any person obtaining a
--   * copy of this software and associated documentation files (the "Software"),
--   * to deal in the Software without restriction, including without limitation
--   * the rights to use, copy, modify, merge, publish, distribute, sublicense,
--   * and/or sell copies of the Software, and to permit persons to whom the
--   * Software is furnished to do so, subject to the following conditions:
--   *
--   * The above copyright notice and this permission notice shall be included
--   * in all copies or substantial portions of the Software.
--   *
--   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
--   * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
--   * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
--   * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
--   * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--   */
--
--  #ifndef  FREEGLUT_INTERNAL_H
--  #define  FREEGLUT_INTERNAL_H
--
--  #if HAVE_CONFIG_H
--  #    include "config.h"
--  #endif
--
--  /* XXX Update these for each release! */
--  #define  VERSION_MAJOR 2
--  #define  VERSION_MINOR 4
--  #define  VERSION_PATCH 0
--
--  /* Freeglut is intended to function under all Unix/X11 and Win32 platforms. */
--  /* XXX: Don't all MS-Windows compilers (except Cygwin) have _WIN32 defined?
--   * XXX: If so, remove the first set of defined()'s below.
--   */
--  #if defined(_MSC_VER) || defined(__WATCOMC__) || defined(__MINGW32__) \
--      || defined(_WIN32) || defined(_WIN32_WCE) \
--      || ( defined(__CYGWIN__) && defined(X_DISPLAY_MISSING) )
--  #   define  TARGET_HOST_MS_WINDOWS 1
--
--  #elif defined(__posix__) || defined(__unix__) || defined(__linux__)
--  #   define  TARGET_HOST_POSIX_X11  1
--
--  /* FIXME: no Macintosh support?
--  #if ...
--  #   define  TARGET_HOST_MAC_OSX    1
--  #else
--  #   error "Unrecognized target host!"
--  */
--  #endif
--
--  #ifndef TARGET_HOST_MS_WINDOWS
--  #   define  TARGET_HOST_MS_WINDOWS 0
--  #endif
--
--  #ifndef  TARGET_HOST_POSIX_X11
--  #   define  TARGET_HOST_POSIX_X11  0
--  #endif
--
--  #ifndef  TARGET_HOST_MAC_OSX
--  #   define  TARGET_HOST_MAC_OSX    0
--  #endif
--
--  /* -- FIXED CONFIGURATION LIMITS ------------------------------------------- */
--
--  #define  FREEGLUT_MAX_MENUS         3
--
--  /* -- PLATFORM-SPECIFIC INCLUDES ------------------------------------------- */
--
--  /* All Win32 headers depend on the huge Windows.h recursive include.
--   * Note: Let's use proper case for MS-Win headers. Even though it's
--   * not required due to case insensitivity, it's a good habit to keep
--   * because the cross-platform includes are case sensitive.
--   */
--  #if TARGET_HOST_MS_WINDOWS && !defined(_WIN32_WCE)
--  #    include <Windows.h>
--  #    include <WindowsX.h>
--  #    include <MMSystem.h>
--  /* CYGWIN does not have tchar.h, but has TEXT(x), defined in winnt.h. */
--  #    ifndef __CYGWIN__
--  #      include <tchar.h>
--  #    else
--  #      define _TEXT(x) TEXT(x)
--  #      define _T(x)    TEXT(x)
--  #    endif
--
--  #elif TARGET_HOST_POSIX_X11
--  #    include <GL/glx.h>
--  #    include <X11/Xlib.h>
--  #    include <X11/Xatom.h>
--  #    include <X11/keysym.h>
--  #    include <X11/extensions/XInput.h>
--  #    ifdef HAVE_X11_EXTENSIONS_XF86VMODE_H
--  #        include <X11/extensions/xf86vmode.h>
--  #    endif
--
--  #endif
--
--  /* These files should be available on every platform. */
--  #include <GL/gl.h>
--  #include <GL/glu.h>
--  #include <stdio.h>
--  #include <string.h>
--  #include <math.h>
--  #include <stdlib.h>
--
--  /* These are included based on autoconf directives. */
--  #if HAVE_SYS_TYPES_H
--  #    include <sys/types.h>
--  #endif
--  #if HAVE_UNISTD_H
--  #    include <unistd.h>
--  #endif
--  #if TIME_WITH_SYS_TIME
--  #    include <sys/time.h>
--  #    include <time.h>
--  #elif HAVE_SYS_TIME_H
--  #    include <sys/time.h>
--  #else
--  #    include <time.h>
--  #endif
--
--  /* -- AUTOCONF HACKS --------------------------------------------------------*/
--
--  /* XXX: Update autoconf to avoid these.
--   * XXX: Are non-POSIX platforms intended not to use autoconf?
--   * If so, perhaps there should be a config_guess.h for them. Alternatively,
--   * config guesses could be placed above, just after the config.h exclusion.
--   */
--  #if defined(__FreeBSD__) || defined(__NetBSD__)
--  #    define HAVE_USB_JS 1
--  #    if defined(__NetBSD__) || ( defined(__FreeBSD__) && __FreeBSD_version >= 500000)
--  #        define HAVE_USBHID_H 1
--  #    endif
--  #endif
--
--  #if TARGET_HOST_MS_WINDOWS
--  #    define  HAVE_VPRINTF 1
--  #endif
--
--  #if !defined(HAVE_VPRINTF) && !defined(HAVE_DOPRNT)
--  /* XXX warning directive here? */
--  #    define  HAVE_VPRINTF 1
--  #endif
--
--  /* MinGW may lack a prototype for ChangeDisplaySettingsEx() (depending on the version?) */
--  #if TARGET_HOST_MS_WINDOWS && !defined(ChangeDisplaySettingsEx)
--  LONG WINAPI ChangeDisplaySettingsExA(LPCSTR,LPDEVMODEA,HWND,DWORD,LPVOID);
--  LONG WINAPI ChangeDisplaySettingsExW(LPCWSTR,LPDEVMODEW,HWND,DWORD,LPVOID);
--  #    ifdef UNICODE
--  #        define ChangeDisplaySettingsEx ChangeDisplaySettingsExW
--  #    else
--  #        define ChangeDisplaySettingsEx ChangeDisplaySettingsExA
--  #    endif
--  #endif
--
--  #if defined(_MSC_VER) || defined(__WATCOMC__)
--  /* strdup() is non-standard, for all but POSIX-2001 */
--  #define strdup   _strdup
--  #endif
--
--  /* M_PI is non-standard (defined by BSD, not ISO-C) */
--  #ifndef M_PI
--  #    define  M_PI  3.14159265358979323846
--  #endif
--
--  #ifndef TRUE
--  #    define  TRUE  1
--  #endif
--
--  #ifndef FALSE
--  #    define  FALSE  0
--  #endif
--
--  /* General defines */
--
--
--  /* -- GLOBAL TYPE DEFINITIONS ---------------------------------------------- */
--
--  /* Freeglut callbacks type definitions */
--  typedef void (* FGCBDisplay       )( void );
--  typedef void (* FGCBReshape       )( int, int );
--  typedef void (* FGCBVisibility    )( int );
--  typedef void (* FGCBKeyboard      )( unsigned char, int, int );
--  typedef void (* FGCBSpecial       )( int, int, int );
--  typedef void (* FGCBMouse         )( int, int, int, int );
--  typedef void (* FGCBMouseWheel    )( int, int, int, int );
--  typedef void (* FGCBMotion        )( int, int );
--  typedef void (* FGCBPassive       )( int, int );
--  typedef void (* FGCBEntry         )( int );
--  typedef void (* FGCBWindowStatus  )( int );
--  typedef void (* FGCBSelect        )( int, int, int );
--  typedef void (* FGCBJoystick      )( unsigned int, int, int, int );
--  typedef void (* FGCBKeyboardUp    )( unsigned char, int, int );
--  typedef void (* FGCBSpecialUp     )( int, int, int );
--  typedef void (* FGCBOverlayDisplay)( void );
--  typedef void (* FGCBSpaceMotion   )( int, int, int );
--  typedef void (* FGCBSpaceRotation )( int, int, int );
--  typedef void (* FGCBSpaceButton   )( int, int );
--  typedef void (* FGCBDials         )( int, int );
--  typedef void (* FGCBButtonBox     )( int, int );
--  typedef void (* FGCBTabletMotion  )( int, int );
--  typedef void (* FGCBTabletButton  )( int, int, int, int );
--  typedef void (* FGCBDestroy       )( void );
--
--  /* The global callbacks type definitions */
--  typedef void (* FGCBIdle          )( void );
--  typedef void (* FGCBTimer         )( int );
--  typedef void (* FGCBMenuState     )( int );
--  typedef void (* FGCBMenuStatus    )( int, int, int );
--
--  /* The callback used when creating/using menus */
--  typedef void (* FGCBMenu          )( int );
--
--
--  /* A list structure */
--  typedef struct tagSFG_List SFG_List;
--  struct tagSFG_List
--  {
--      void *First;
--      void *Last;
--  };
--
--  /* A list node structure */
--  typedef struct tagSFG_Node SFG_Node;
--  struct tagSFG_Node
--  {
--      void *Next;
--      void *Prev;
--  };
--
--
--
--
--
--
--  /* The user can create any number of timer hooks */
--  typedef struct tagSFG_Timer SFG_Timer;
--  struct tagSFG_Timer
--  {
--      SFG_Node        Node;
--      int             ID;                 /* The timer ID integer              */
--      FGCBTimer       Callback;           /* The timer callback                */
--      long            TriggerTime;        /* The timer trigger time            */
--  };
--
--
--
--
--
--
--  /*
--   * SET_WCB() is used as:
--   *
--   *     SET_WCB( window, cbname, func );
--   *
--   * ...where {window} is the freeglut window to set the callback,
--   *          {cbname} is the window-specific callback to set,
--   *          {func} is a function-pointer.
--   *
--   * Originally, {FETCH_WCB( ... ) = func} was rather sloppily used,
--   * but this can cause warnings because the FETCH_WCB() macro type-
--   * casts its result, and a type-cast value shouldn't be an lvalue.
--   *
--   * The {if( FETCH_WCB( ... ) != func )} test is to do type-checking
--   * and for no other reason.  Since it's hidden in the macro, the
--   * ugliness is felt to be rather benign.
--   */
--  #define SET_WCB(window,cbname,func)                            \
--  do                                                             \
--  {                                                              \
--      if( FETCH_WCB( window, cbname ) != (SFG_Proc)(func) )      \
--          (((window).CallBacks[CB_ ## cbname]) = (SFG_Proc)(func)); \
--  } while( 0 )
--
--  /*
--   * FETCH_WCB() is used as:
--   *
--   *     FETCH_WCB( window, cbname );
--   *
--   * ...where {window} is the freeglut window to fetch the callback from,
--   *          {cbname} is the window-specific callback to fetch.
--   *
--   * The result is correctly type-cast to the callback function pointer
--   * type.
--   */
--  #define FETCH_WCB(window,cbname) \
--      ((window).CallBacks[CB_ ## cbname])
--
--  /*
--   * INVOKE_WCB() is used as:
--   *
--   *     INVOKE_WCB( window, cbname, ( arg_list ) );
--   *
--   * ...where {window} is the freeglut window,
--   *          {cbname} is the window-specific callback to be invoked,
--   *          {(arg_list)} is the parameter list.
--   *
--   * The callback is invoked as:
--   *
--   *    callback( arg_list );
--   *
--   * ...so the parentheses are REQUIRED in the {arg_list}.
--   *
--   * NOTE that it does a sanity-check and also sets the
--   * current window.
--   *
--   */
--  #if TARGET_HOST_MS_WINDOWS && !defined(_WIN32_WCE) /* FIXME: also WinCE? */
--  #define INVOKE_WCB(window,cbname,arg_list)    \
--  do                                            \
--  {                                             \
--      if( FETCH_WCB( window, cbname ) )         \
--      {                                         \
--          FGCB ## cbname func = (FGCB ## cbname)(FETCH_WCB( window, cbname )); \
--          fgSetWindow( &window );               \
--          func arg_list;                        \
--      }                                         \
--  } while( 0 )
--  #else
--  #define INVOKE_WCB(window,cbname,arg_list)    \
--  do                                            \
--  {                                             \
--      if( FETCH_WCB( window, cbname ) )         \
--      {                                         \
--          fgSetWindow( &window );               \
--          ((FGCB ## cbname)FETCH_WCB( window, cbname )) arg_list; \
--      }                                         \
--  } while( 0 )
--  #endif
--
--
--
--
--
--
--  /* A linked list structure of windows */
--  typedef struct tagSFG_WindowList SFG_WindowList ;
--  struct tagSFG_WindowList
--  {
--      SFG_Node node;
--      SFG_Window *window ;
--  };
--
--
--  /*
--   * This structure is used for the enumeration purposes.
--   * You can easily extend its functionalities by declaring
--   * a structure containing enumerator's contents and custom
--   * data, then casting its pointer to (SFG_Enumerator *).
--   */
--  typedef struct tagSFG_Enumerator SFG_Enumerator;
--  struct tagSFG_Enumerator
--  {
--      GLboolean   found;                          /* Used to terminate search  */
--      void*       data;                           /* Custom data pointer       */
--  };
--  typedef void (* FGCBenumerator  )( SFG_Window *, SFG_Enumerator * );
--
--  /* The bitmap font structure */
--  typedef struct tagSFG_Font SFG_Font;
--  struct tagSFG_Font
--  {
--      char*           Name;         /* The source font name             */
--      int             Quantity;     /* Number of chars in font          */
--      int             Height;       /* Height of the characters         */
--      const GLubyte** Characters;   /* The characters mapping           */
--
--      float           xorig, yorig; /* Relative origin of the character */
--  };
--
--  /* The stroke font structures */
--
--  typedef struct tagSFG_StrokeVertex SFG_StrokeVertex;
--  struct tagSFG_StrokeVertex
--  {
--      GLfloat         X, Y;
--  };
--
--  typedef struct tagSFG_StrokeStrip SFG_StrokeStrip;
--  struct tagSFG_StrokeStrip
--  {
--      int             Number;
--      const SFG_StrokeVertex* Vertices;
--  };
--
--  typedef struct tagSFG_StrokeChar SFG_StrokeChar;
--  struct tagSFG_StrokeChar
--  {
--      GLfloat         Right;
--      int             Number;
--      const SFG_StrokeStrip* Strips;
--  };
--
--  typedef struct tagSFG_StrokeFont SFG_StrokeFont;
--  struct tagSFG_StrokeFont
--  {
--      char*           Name;                       /* The source font name      */
--      int             Quantity;                   /* Number of chars in font   */
--      GLfloat         Height;                     /* Height of the characters  */
--      const SFG_StrokeChar** Characters;          /* The characters mapping    */
--  };
--
--
--
--  /* -- PRIVATE FUNCTION DECLARATIONS ---------------------------------------- */
--
--  /*
--   * A call to this function makes us sure that the Display and Structure
--   * subsystems have been properly initialized and are ready to be used
--   */
--  #define  FREEGLUT_EXIT_IF_NOT_INITIALISED( string )               \
--    if ( ! fgState.Initialised )                                    \
--    {                                                               \
--      fgError ( " ERROR:  Function <%s> called"                     \
--                " without first calling 'glutInit'.", (string) ) ;  \
--    }
--
--  #define  FREEGLUT_INTERNAL_ERROR_EXIT_IF_NOT_INITIALISED( string )  \
--    if ( ! fgState.Initialised )                                      \
--    {                                                                 \
--      fgError ( " ERROR:  Internal <%s> function called"              \
--                " without first calling 'glutInit'.", (string) ) ;    \
--    }
--
--  #define  FREEGLUT_INTERNAL_ERROR_EXIT( cond, string, function )  \
--    if ( ! ( cond ) )                                              \
--    {                                                              \
--      fgError ( " ERROR:  Internal error <%s> in function %s",     \
--                (string), (function) ) ;                           \
--    }
--
--  /*
--   * Following definitions are somewhat similiar to GLib's,
--   * but do not generate any log messages:
--   */
--  #define  freeglut_return_if_fail( expr ) \
--      if( !(expr) )                        \
--          return;
--  #define  freeglut_return_val_if_fail( expr, val ) \
--      if( !(expr) )                                 \
--          return val ;
--
--  /*
--   * A call to those macros assures us that there is a current
--   * window set, respectively:
--   */
--  #define  FREEGLUT_EXIT_IF_NO_WINDOW( string )                   \
--    if ( ! fgStructure.CurrentWindow )                            \
--    {                                                             \
--      fgError ( " ERROR:  Function <%s> called"                   \
--                " with no current window defined.", (string) ) ;  \
--    }
--
--  /*
--   * The deinitialize function gets called on glutMainLoop() end. It should clean up
--   * everything inside of the freeglut
--   */
--  void fgDeinitialize( void );
--
--  /*
--   * Those two functions are used to create/destroy the freeglut internal
--   * structures. This actually happens when calling glutInit() and when
--   * quitting the glutMainLoop() (which actually happens, when all windows
--   * have been closed).
--   */
--  void fgCreateStructure( void );
--  void fgDestroyStructure( void );
--
--  /* A helper function to check if a display mode is possible to use */
--  #if TARGET_HOST_POSIX_X11
--  GLXFBConfig* fgChooseFBConfig( void );
--  #endif
--
--  /* The window procedure for Win32 events handling */
--  #if TARGET_HOST_MS_WINDOWS
--  LRESULT CALLBACK fgWindowProc( HWND hWnd, UINT uMsg,
--                                 WPARAM wParam, LPARAM lParam );
--  GLboolean fgSetupPixelFormat( SFG_Window* window, GLboolean checkOnly,
--                                unsigned char layer_type );
--  #endif
--
--  /*
--   * Window creation, opening, closing and destruction.
--   * Also CallBack clearing/initialization.
--   * Defined in freeglut_structure.c, freeglut_window.c.
--   */
--  SFG_Window* fgCreateWindow( SFG_Window* parent, const char* title,
--                              GLboolean positionUse, int x, int y,
--                              GLboolean sizeUse, int w, int h,
--                              GLboolean gameMode, GLboolean isMenu );
--  void        fgSetWindow ( SFG_Window *window );
--  void        fgOpenWindow( SFG_Window* window, const char* title,
--                            GLboolean positionUse, int x, int y,
--                            GLboolean sizeUse, int w, int h,
--                            GLboolean gameMode, GLboolean isSubWindow );
--  void        fgCloseWindow( SFG_Window* window );
--  void        fgAddToWindowDestroyList ( SFG_Window* window );
--  void        fgCloseWindows ();
--  void        fgDestroyWindow( SFG_Window* window );
--
--  /* Menu creation and destruction. Defined in freeglut_structure.c */
--  SFG_Menu*   fgCreateMenu( FGCBMenu menuCallback );
--  void        fgDestroyMenu( SFG_Menu* menu );
--
--  /* Joystick device management functions, defined in freeglut_joystick.c */
--  int         fgJoystickDetect( void );
--  void        fgInitialiseJoysticks( void );
--  void        fgJoystickClose( void );
--  void        fgJoystickPollWindow( SFG_Window* window );
--
--  /* InputDevice Initialisation and Closure */
--  int         fgInputDeviceDetect( void );
--  void        fgInitialiseInputDevices( void );
--  void        fgInputDeviceClose( void );
--
--  /* Setting the cursor for a given window */
--  void fgSetCursor ( SFG_Window *window, int cursorID );
--
--  /*
--   * Helper function to enumerate through all registered windows
--   * and one to enumerate all of a window's subwindows...
--   *
--   * The GFunc callback for those functions will be defined as:
--   *
--   *      void enumCallback( gpointer window, gpointer enumerator );
--   *
--   * where window is the enumerated (sub)window pointer (SFG_Window *),
--   * and userData is the a custom user-supplied pointer. Functions
--   * are defined and exported from freeglut_structure.c file.
--   */
--  void fgEnumWindows( FGCBenumerator enumCallback, SFG_Enumerator* enumerator );
--  void fgEnumSubWindows( SFG_Window* window, FGCBenumerator enumCallback,
--                         SFG_Enumerator* enumerator );
--
--  /*
--   * fgWindowByHandle returns a (SFG_Window *) value pointing to the
--   * first window in the queue matching the specified window handle.
--   * The function is defined in freeglut_structure.c file.
--   */
--  SFG_Window* fgWindowByHandle( SFG_WindowHandleType hWindow );
--
--  /*
--   * This function is similiar to the previous one, except it is
--   * looking for a specified (sub)window identifier. The function
--   * is defined in freeglut_structure.c file.
--   */
--  SFG_Window* fgWindowByID( int windowID );
--
--  /*
--   * Looks up a menu given its ID. This is easier than fgWindowByXXX
--   * as all menus are placed in a single doubly linked list...
--   */
--  SFG_Menu* fgMenuByID( int menuID );
--
--  /*
--   * The menu activation and deactivation the code. This is the meat
--   * of the menu user interface handling code...
--   */
--  void fgUpdateMenuHighlight ( SFG_Menu *menu );
--  GLboolean fgCheckActiveMenu ( SFG_Window *window, int button, GLboolean pressed,
--                                int mouse_x, int mouse_y );
--  void fgDeactivateMenu( SFG_Window *window );
--
--  /*
--   * This function gets called just before the buffers swap, so that
--   * freeglut can display the pull-down menus via OpenGL. The function
--   * is defined in freeglut_menu.c file.
--   */
--  void fgDisplayMenu( void );
--
--  /* Elapsed time as per glutGet(GLUT_ELAPSED_TIME). */
--  long fgElapsedTime( void );
--
--  /* System time in milliseconds */
--  long unsigned fgSystemTime(void);
--
--  /* List functions */
--  void fgListInit(SFG_List *list);
--  void fgListAppend(SFG_List *list, SFG_Node *node);
--  void fgListRemove(SFG_List *list, SFG_Node *node);
--  int fgListLength(SFG_List *list);
--  void fgListInsert(SFG_List *list, SFG_Node *next, SFG_Node *node);
--
--  /* Error Message functions */
--  void fgError( const char *fmt, ... );
--  void fgWarning( const char *fmt, ... );
--
--  /*
--   * Check if "hint" is present in "property" for "window".  See freeglut_init.c
--   */
--  #if TARGET_HOST_POSIX_X11
--  int fgHintPresent(Window window, Atom property, Atom hint);
--  #endif
--
--  #endif /* FREEGLUT_INTERNAL_H */
--
--  /*** END OF FILE ***/
