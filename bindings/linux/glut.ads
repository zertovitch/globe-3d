----------------------------------------------------------------------
--  *** CAUTION ***    This glut.ads file is generated by
--                     preprocessing glut.prs.
--                     You should modify glut.prs and preprocess
--                     it with: tools/glut_prep.cmd
--
--  Bindings to FreeGLUT (2.4.0 or later).
--  Maintained by Gautier de Montmollin and Rod Kay
--
--  Overall changes made to older Ada bindings to GLUT 3.7,
--  the ancestor of FreeGLUT:
--   - mixed Jerry van Dijk's and Pascal Obry's bindings
--   - uses GL without the GL prefixes and "4f"-style suffixes
--   - useless and harmful "GLUT_" and "glut" prefixes removed
--   - one can set callbacks with the 'Address attribute, since
--     the 'Unrestricted_Access is GNAT-specific

with Interfaces.C.Strings;
with System;

with GL;

package GLUT is

   subtype Unsigned is Interfaces.C.unsigned;

   function "and" (x, y : Unsigned) return Unsigned
     renames Interfaces.C."and";

   function "or" (x, y : Unsigned) return Unsigned
     renames Interfaces.C."or";

   --  Display mode bit masks.

   RGB                 : constant := 0;
   RGBA                : constant := 0;
   INDEX               : constant := 1;
   SINGLE              : constant := 0;
   DOUBLE              : constant := 2;
   ACCUM               : constant := 4;
   ALPHA               : constant := 8;
   DEPTH               : constant := 16;
   STENCIL             : constant := 32;
   MULTISAMPLE         : constant := 128;
   STEREO              : constant := 256;
   LUMINANCE           : constant := 512;

   --  Mouse buttons.

   LEFT_BUTTON         : constant := 0;
   MIDDLE_BUTTON       : constant := 1;
   RIGHT_BUTTON        : constant := 2;

   --  Mouse button callback state.

   DOWN                : constant := 0;
   UP                  : constant := 1;

   --  function keys

   KEY_F1              : constant := 1;
   KEY_F2              : constant := 2;
   KEY_F3              : constant := 3;
   KEY_F4              : constant := 4;
   KEY_F5              : constant := 5;
   KEY_F6              : constant := 6;
   KEY_F7              : constant := 7;
   KEY_F8              : constant := 8;
   KEY_F9              : constant := 9;
   KEY_F10             : constant := 10;
   KEY_F11             : constant := 11;
   KEY_F12             : constant := 12;

   --  directional keys

   KEY_LEFT            : constant := 100;
   KEY_UP              : constant := 101;
   KEY_RIGHT           : constant := 102;
   KEY_DOWN            : constant := 103;
   KEY_PAGE_UP         : constant := 104;
   KEY_PAGE_DOWN       : constant := 105;
   KEY_HOME            : constant := 106;
   KEY_END             : constant := 107;
   KEY_INSERT          : constant := 108;

   --  Entry/exit callback state.

   LEFT                : constant := 0;
   ENTERED             : constant := 1;

   --  Menu usage callback state.

   MENU_NOT_IN_USE     : constant := 0;
   MENU_IN_USE         : constant := 1;

   --  Visibility callback state.

   NOT_VISIBLE         : constant := 0;
   VISIBLE             : constant := 1;

   --  Window status callback state.

   HIDDEN              : constant := 0;
   FULLY_RETAINED      : constant := 1;
   PARTIALLY_RETAINED  : constant := 2;
   FULLY_COVERED       : constant := 3;

   --  Color index component selection values.

   RED                 : constant := 0;
   GREEN               : constant := 1;
   BLUE                : constant := 2;

   --  glutGameModeGet

   GAME_MODE_ACTIVE          : constant := 0;
   GAME_MODE_POSSIBLE        : constant := 1;
   GAME_MODE_WIDTH           : constant := 2;
   GAME_MODE_HEIGHT          : constant := 3;
   GAME_MODE_PIXEL_DEPTH     : constant := 4;
   GAME_MODE_REFRESH_RATE    : constant := 5;
   GAME_MODE_DISPLAY_CHANGED : constant := 6;

   --  glutSetKeyRepeat modes

   KEY_REPEAT_OFF            : constant := 0;
   KEY_REPEAT_ON             : constant := 1;
   KEY_REPEAT_DEFAULT        : constant := 2;

   --  Joystick button masks

   JOYSTICK_BUTTON_A          : constant := 1;
   JOYSTICK_BUTTON_B          : constant := 2;
   JOYSTICK_BUTTON_C          : constant := 4;
   JOYSTICK_BUTTON_D          : constant := 8;

   --  Stroke font constants (use these in GLUT program).

   STROKE_ROMAN_STROKE : constant System.Address;
   pragma Import (C, STROKE_ROMAN_STROKE, "glutStrokeRoman");
   STROKE_ROMAN      : constant System.Address := STROKE_ROMAN_STROKE'Address;
   STROKE_MONO_ROMAN_STROKE : constant System.Address;
   pragma Import (C, STROKE_MONO_ROMAN_STROKE, "glutStrokeMonoRoman");
   STROKE_MONO_ROMAN : constant System.Address := STROKE_MONO_ROMAN_STROKE'Address;

   --  Bitmap font constants (use these in GLUT program).

   BITMAP_9_BY_15_FONT   : constant System.Address;
   pragma Import (C, BITMAP_9_BY_15_FONT, "glutBitmap9By15");
   BITMAP_9_BY_15        : constant System.Address := BITMAP_9_BY_15_FONT'Address;
   BITMAP_8_BY_13_FONT   : constant System.Address;
   pragma Import (C, BITMAP_8_BY_13_FONT, "glutBitmap8By13");
   BITMAP_8_BY_13        : constant System.Address := BITMAP_8_BY_13_FONT'Address;
   BITMAP_TIMES_ROMAN_10_FONT : constant System.Address;
   pragma Import (C, BITMAP_TIMES_ROMAN_10_FONT, "glutBitmapTimesRoman10");
   BITMAP_TIMES_ROMAN_10 : constant System.Address := BITMAP_TIMES_ROMAN_10_FONT'Address;
   BITMAP_TIMES_ROMAN_24_FONT : constant System.Address;
   pragma Import (C, BITMAP_TIMES_ROMAN_24_FONT, "glutBitmapTimesRoman24");
   BITMAP_TIMES_ROMAN_24 : constant System.Address := BITMAP_TIMES_ROMAN_24_FONT'Address;
   BITMAP_HELVETICA_10_FONT   : constant System.Address;
   pragma Import (C, BITMAP_HELVETICA_10_FONT, "glutBitmapHelvetica10");
   BITMAP_HELVETICA_10   : constant System.Address := BITMAP_HELVETICA_10_FONT'Address;
   BITMAP_HELVETICA_12_FONT   : constant System.Address;
   pragma Import (C, BITMAP_HELVETICA_12_FONT, "glutBitmapHelvetica12");
   BITMAP_HELVETICA_12   : constant System.Address := BITMAP_HELVETICA_12_FONT'Address;
   BITMAP_HELVETICA_18_FONT   : constant System.Address;
   pragma Import (C, BITMAP_HELVETICA_18_FONT, "glutBitmapHelvetica18");
   BITMAP_HELVETICA_18   : constant System.Address := BITMAP_HELVETICA_18_FONT'Address;

   --  glutGet parameters.

   WINDOW_X                  : constant := 100;
   WINDOW_Y                  : constant := 101;
   WINDOW_WIDTH              : constant := 102;
   WINDOW_HEIGHT             : constant := 103;
   WINDOW_BUFFER_SIZE        : constant := 104;
   WINDOW_STENCIL_SIZE       : constant := 105;
   WINDOW_DEPTH_SIZE         : constant := 106;
   WINDOW_RED_SIZE           : constant := 107;
   WINDOW_GREEN_SIZE         : constant := 108;
   WINDOW_BLUE_SIZE          : constant := 109;
   WINDOW_ALPHA_SIZE         : constant := 110;
   WINDOW_ACCUM_RED_SIZE     : constant := 111;
   WINDOW_ACCUM_GREEN_SIZE   : constant := 112;
   WINDOW_ACCUM_BLUE_SIZE    : constant := 113;
   WINDOW_ACCUM_ALPHA_SIZE   : constant := 114;
   WINDOW_DOUBLEBUFFER       : constant := 115;
   WINDOW_RGBA               : constant := 116;
   WINDOW_PARENT             : constant := 117;
   WINDOW_NUM_CHILDREN       : constant := 118;
   WINDOW_COLORMAP_SIZE      : constant := 119;
   WINDOW_NUM_SAMPLES        : constant := 120;
   WINDOW_STEREO             : constant := 121;
   WINDOW_CURSOR             : constant := 122;
   SCREEN_WIDTH              : constant := 200;
   SCREEN_HEIGHT             : constant := 201;
   SCREEN_WIDTH_MM           : constant := 202;
   SCREEN_HEIGHT_MM          : constant := 203;
   MENU_NUM_ITEMS            : constant := 300;
   DISPLAY_MODE_POSSIBLE     : constant := 400;
   INIT_WINDOW_X             : constant := 500;
   INIT_WINDOW_Y             : constant := 501;
   INIT_WINDOW_WIDTH         : constant := 502;
   INIT_WINDOW_HEIGHT        : constant := 503;
   INIT_DISPLAY_MODE         : constant := 504;
   ELAPSED_TIME              : constant := 700;

   --  glutDeviceGet parameters.

   HAS_KEYBOARD              : constant := 600;
   HAS_MOUSE                 : constant := 601;
   HAS_SPACEBALL             : constant := 602;
   HAS_DIAL_AND_BUTTON_BOX   : constant := 603;
   HAS_TABLET                : constant := 604;
   NUM_MOUSE_BUTTONS         : constant := 605;
   NUM_SPACEBALL_BUTTONS     : constant := 606;
   NUM_BUTTON_BOX_BUTTONS    : constant := 607;
   NUM_DIALS                 : constant := 608;
   NUM_TABLET_BUTTONS        : constant := 609;

   --  glutLayerGet parameters.

   OVERLAY_POSSIBLE          : constant := 800;
   LAYER_IN_USE              : constant := 801;
   HAS_OVERLAY               : constant := 802;
   TRANSPARENT_INDEX         : constant := 803;
   NORMAL_DAMAGED            : constant := 804;
   OVERLAY_DAMAGED           : constant := 805;

   --  glutVideoResizeGet parameters.

   VIDEO_RESIZE_POSSIBLE     : constant := 900;
   VIDEO_RESIZE_IN_USE       : constant := 901;
   VIDEO_RESIZE_X_DELTA      : constant := 902;
   VIDEO_RESIZE_Y_DELTA      : constant := 903;
   VIDEO_RESIZE_WIDTH_DELTA  : constant := 904;
   VIDEO_RESIZE_HEIGHT_DELTA : constant := 905;
   VIDEO_RESIZE_X            : constant := 906;
   VIDEO_RESIZE_Y            : constant := 907;
   VIDEO_RESIZE_WIDTH        : constant := 908;
   VIDEO_RESIZE_HEIGHT       : constant := 909;

   --  UseLayer parameters.

   NORMAL  : constant := 0;
   OVERLAY : constant := 1;

   --  GetModifiers return mask.

   ACTIVE_SHIFT               : constant := 1;
   ACTIVE_CTRL                : constant := 2;
   ACTIVE_ALT                 : constant := 4;

   --  SetCursor parameters.
   --  Basic arrows.

   CURSOR_RIGHT_ARROW         : constant := 0;
   CURSOR_LEFT_ARROW          : constant := 1;

   --  Symbolic cursor shapes.

   CURSOR_INFO                : constant := 2;
   CURSOR_DESTROY             : constant := 3;
   CURSOR_HELP                : constant := 4;
   CURSOR_CYCLE               : constant := 5;
   CURSOR_SPRAY               : constant := 6;
   CURSOR_WAIT                : constant := 7;
   CURSOR_TEXT                : constant := 8;
   CURSOR_CROSSHAIR           : constant := 9;

   --  Directional cursors.

   CURSOR_UP_DOWN             : constant := 10;
   CURSOR_LEFT_RIGHT          : constant := 11;

   --  Sizing cursors.

   CURSOR_TOP_SIDE            : constant := 12;
   CURSOR_BOTTOM_SIDE         : constant := 13;
   CURSOR_LEFT_SIDE           : constant := 14;
   CURSOR_RIGHT_SIDE          : constant := 15;
   CURSOR_TOP_LEFT_CORNER     : constant := 16;
   CURSOR_TOP_RIGHT_CORNER    : constant := 17;
   CURSOR_BOTTOM_RIGHT_CORNER : constant := 18;
   CURSOR_BOTTOM_LEFT_CORNER  : constant := 19;

   --  Inherit from parent window.

   CURSOR_INHERIT             : constant := 100;

   --  Blank cursor.

   CURSOR_NONE                : constant := 101;

   --  Fullscreen crosshair (if available).

   CURSOR_FULL_CROSSHAIR      : constant := 102;

   --  GLUT initialization sub-API.

   procedure Init;

   --  GLUT API Extension macro definitions
   --  behaviour when the user clicks on an "x" to close a window
   --
   ACTION_EXIT                 : constant := 0;
   ACTION_GLUTMAINLOOP_RETURNS : constant := 1;
   ACTION_CONTINUE_EXECUTION   : constant := 2;

   ACTION_ON_WINDOW_CLOSE      : constant := 16#01F9#;    -- for 'Get' and 'SetOption' parameter.

   GLUT_RENDERING_CONTEXT   : constant := 16#01FD#;

   GLUT_CREATE_NEW_CONTEXT  : constant := 0;
   GLUT_USE_CURRENT_CONTEXT : constant := 1;

   procedure SetOption (option_flag : Integer;   value : Integer);
   pragma Import (StdCall, SetOption, "glutSetOption");

   procedure InitDisplayMode (Mode : Unsigned);
   pragma Import (StdCall, InitDisplayMode, "glutInitDisplayMode");

   procedure InitDisplayString (String : Interfaces.C.Strings.chars_ptr);
   pragma Import (StdCall, InitDisplayString, "glutInitDisplayString");

   procedure InitDisplayString (Name : String);

   procedure InitWindowPosition (X : Integer; Y : Integer);
   pragma Import (StdCall, InitWindowPosition, "glutInitWindowPosition");

   procedure InitWindowSize (Width : Integer; Height : Integer);
   pragma Import (StdCall, InitWindowSize, "glutInitWindowSize");

   procedure MainLoop;
   pragma Import (StdCall, MainLoop, "glutMainLoop");

   procedure LeaveMainLoop; -- FreeGLUT
   pragma Import (StdCall, LeaveMainLoop, "glutLeaveMainLoop");

   procedure MainLoopEvent; -- FreeGLUT
   pragma Import (StdCall, MainLoopEvent, "glutMainLoopEvent");

   --  GLUT window sub-API.

   function CreateWindow
     (Title : Interfaces.C.Strings.chars_ptr)
      return Integer;
   pragma Import (StdCall, CreateWindow, "glutCreateWindow");

   function CreateWindow (Title : String) return Integer;

   function CreateSubWindow
     (Win    : Integer;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
      return Integer;
   pragma Import (StdCall, CreateSubWindow, "glutCreateSubWindow");

   procedure DestroyWindow (Win : Integer);
   pragma Import (StdCall, DestroyWindow, "glutDestroyWindow");

   procedure PostRedisplay;
   pragma Import (StdCall, PostRedisplay, "glutPostRedisplay");

   procedure PostWindowRedisplay (Win : Integer);
   pragma Import (StdCall, PostWindowRedisplay, "glutPostWindowRedisplay");

   procedure SwapBuffers;
   pragma Import (StdCall, SwapBuffers, "glutSwapBuffers");

   function GetWindow return Integer;
   pragma Import (StdCall, GetWindow, "glutGetWindow");

   function GetWindowData return System.Address; -- FreeGLUT
   pragma Import (StdCall, GetWindowData, "glutGetWindowData");

   procedure SetWindow (Win : Integer);
   pragma Import (StdCall, SetWindow, "glutSetWindow");

   procedure SetWindowData (Data : System.Address); -- FreeGLUT
   pragma Import (StdCall, SetWindowData, "glutSetWindowData");

   procedure SetWindowTitle (Title : Interfaces.C.Strings.chars_ptr);
   pragma Import (StdCall, SetWindowTitle, "glutSetWindowTitle");

   procedure SetWindowTitle (Title : String);

   procedure SetIconTitle (Title : Interfaces.C.Strings.chars_ptr);
   pragma Import (StdCall, SetIconTitle, "glutSetIconTitle");

   procedure SetIconTitle (Title : String);

   procedure PositionWindow (X : Integer; Y : Integer);
   pragma Import (StdCall, PositionWindow, "glutPositionWindow");

   procedure ReshapeWindow (Width : Integer; Height : Integer);
   pragma Import (StdCall, ReshapeWindow, "glutReshapeWindow");

   procedure PopWindow;
   pragma Import (StdCall, PopWindow, "glutPopWindow");

   procedure PushWindow;
   pragma Import (StdCall, PushWindow, "glutPushWindow");

   procedure IconifyWindow;
   pragma Import (StdCall, IconifyWindow, "glutIconifyWindow");

   procedure ShowWindow;
   pragma Import (StdCall, ShowWindow, "glutShowWindow");

   procedure HideWindow;
   pragma Import (StdCall, HideWindow, "glutHideWindow");

   procedure FullScreen;
   pragma Import (StdCall, FullScreen, "glutFullScreen");

   procedure SetCursor (Cursor : Integer);
   pragma Import (StdCall, SetCursor, "glutSetCursor");

   procedure WarpPointer (X : Integer; Y : Integer);
   pragma Import (StdCall, WarpPointer, "glutWarpPointer");

   type Glut_SpecialUp is access procedure
     (Key : Integer;
      X   : Integer;
      Y   : Integer);

   procedure SpecialUpFunc (Func : Glut_SpecialUp);
   pragma Import (StdCall, SpecialUpFunc, "glutSpecialUpFunc");
   procedure SpecialUpFunc (Func : System.Address);

   type Glut_Joystick is access procedure
     (ButtonMask : Unsigned;
      X          : Integer;
      Y          : Integer;
      Z          : Integer);

   procedure JoystickFunc (Funct : Glut_Joystick; PollInterval : Integer);
   pragma Import (StdCall, JoystickFunc, "glutJoystickFunc");

   --  GLUT overlay sub-API.

   procedure EstablishOverlay;
   pragma Import (StdCall, EstablishOverlay, "glutEstablishOverlay");

   procedure RemoveOverlay;
   pragma Import (StdCall, RemoveOverlay, "glutRemoveOverlay");

   procedure UseLayer (Layer : GL.enum);
   pragma Import (StdCall, UseLayer, "glutUseLayer");

   procedure PostOverlayRedisplay;
   pragma Import (StdCall, PostOverlayRedisplay,
                  "glutPostOverlayRedisplay");

   procedure PostWindowOverlayRedisplay (Win : Integer);
   pragma Import (StdCall, PostWindowOverlayRedisplay,
                  "glutPostWindowOverlayRedisplay");

   procedure ShowOverlay;
   pragma Import (StdCall, ShowOverlay, "glutShowOverlay");

   procedure HideOverlay;
   pragma Import (StdCall, HideOverlay, "glutHideOverlay");

   --  GLUT menu sub-API.

   type Glut_Proc_1 is access procedure (P1 : Integer);

   function CreateMenu (P1 : Glut_Proc_1) return Integer;
   pragma Import (StdCall, CreateMenu, "glutCreateMenu");
   function CreateMenu (P1 : System.Address) return Integer;

   procedure DestroyMenu (Menu : Integer);
   pragma Import (StdCall, DestroyMenu, "glutDestroyMenu");

   function GetMenu return Integer;
   pragma Import (StdCall, GetMenu, "glutGetMenu");

   procedure SetMenu (Menu : Integer);
   pragma Import (StdCall, SetMenu, "glutSetMenu");

   procedure AddMenuEntry
     (Label : Interfaces.C.Strings.chars_ptr;
      Value : Integer);
   pragma Import (StdCall, AddMenuEntry, "glutAddMenuEntry");

   procedure AddMenuEntry (Label : String; Value : Integer);

   procedure AddSubMenu
     (Label   : Interfaces.C.Strings.chars_ptr;
      Submenu : Integer);
   pragma Import (StdCall, AddSubMenu, "glutAddSubMenu");

   procedure AddSubMenu (Label : String; Submenu : Integer);

   procedure ChangeToMenuEntry
     (Item  : Integer;
      Label : Interfaces.C.Strings.chars_ptr;
      Value : Integer);
   pragma Import (StdCall, ChangeToMenuEntry, "glutChangeToMenuEntry");

   procedure ChangeToMenuEntry
     (Item  : Integer;
      Label : String;
      Value : Integer);

   procedure ChangeToSubMenu
     (Item    : Integer;
      Label   : Interfaces.C.Strings.chars_ptr;
      Submenu : Integer);
   pragma Import (StdCall, ChangeToSubMenu, "glutChangeToSubMenu");

   procedure ChangeToSubMenu
     (Item    : Integer;
      Label   : String;
      Submenu : Integer);

   procedure RemoveMenuItem (Item : Integer);
   pragma Import (StdCall, RemoveMenuItem, "glutRemoveMenuItem");

   procedure AttachMenu (Button : Integer);
   pragma Import (StdCall, AttachMenu, "glutAttachMenu");

   procedure DetachMenu (Button : Integer);
   pragma Import (StdCall, DetachMenu, "glutDetachMenu");

   --  GLUT callback sub-API.

   type Glut_Proc_2 is access procedure;

   procedure CloseFunc (Callback : Glut_Proc_2);
   pragma Import (StdCall, CloseFunc, "glutCloseFunc");

   procedure DisplayFunc (P1 : Glut_Proc_2);
   pragma Import (StdCall, DisplayFunc, "glutDisplayFunc");
   procedure DisplayFunc (P1 : System.Address);

   type Glut_Proc_3 is access procedure (Width : Integer; Height : Integer);

   procedure ReshapeFunc (P1 : Glut_Proc_3);
   pragma Import (StdCall, ReshapeFunc, "glutReshapeFunc");
   procedure ReshapeFunc (P1 : System.Address);

   subtype Key_type is Interfaces.C.unsigned_char;

   type Glut_Proc_4 is access
     procedure (Key : Key_type; X : Integer; Y : Integer);

   procedure KeyboardFunc (P1 : Glut_Proc_4);
   pragma Import (StdCall, KeyboardFunc, "glutKeyboardFunc");
   procedure KeyboardFunc (P1 : System.Address);

   type Glut_KeyUpFunc is access procedure
     (Key : Key_type;
      X   : Integer;
      Y   : in Integer);

   procedure KeyboardUpFunc (P1 : Glut_KeyUpFunc);
   pragma Import (StdCall, KeyboardUpFunc, "glutKeyboardUpFunc");
   procedure KeyboardUpFunc (P1 : System.Address);

   type Glut_Proc_5 is access procedure
     (Button : Integer; State : Integer; X : Integer; Y : Integer);

   procedure MouseFunc (P1 : Glut_Proc_5);
   pragma Import (StdCall, MouseFunc, "glutMouseFunc");
   procedure MouseFunc (P1 : System.Address);

   type Glut_Proc_6 is access procedure (X : Integer; Y : Integer);

   procedure MotionFunc (P1 : Glut_Proc_6);
   pragma Import (StdCall, MotionFunc, "glutMotionFunc");
   procedure MotionFunc (P1 : System.Address);

   type Glut_Proc_7 is access procedure (X : Integer; Y : Integer);

   procedure PassiveMotionFunc (P1 : Glut_Proc_7);
   pragma Import (StdCall, PassiveMotionFunc, "glutPassiveMotionFunc");
   procedure PassiveMotionFunc (P1 : System.Address);

   type Glut_Proc_8 is access procedure (State : Integer);

   procedure EntryFunc (P1 : Glut_Proc_8);
   pragma Import (StdCall, EntryFunc, "glutEntryFunc");

   type Glut_Proc_9 is access procedure (State : Integer);

   procedure VisibilityFunc (P1 : Glut_Proc_9);
   pragma Import (StdCall, VisibilityFunc, "glutVisibilityFunc");

   type Glut_Proc_10 is access procedure;

   procedure IdleFunc (P1 : Glut_Proc_10);
   pragma Import (StdCall, IdleFunc, "glutIdleFunc");
   procedure IdleFunc (P1 : System.Address);

   type Glut_Proc_11 is access procedure (Value : Integer);

   procedure TimerFunc
     (Millis : Unsigned;
      P2     : Glut_Proc_11;
      Value  : Integer);
   pragma Import (StdCall, TimerFunc, "glutTimerFunc");

   type Glut_Proc_12 is access procedure (State : Integer);

   procedure MenuStateFunc (P1 : Glut_Proc_12);
   pragma Import (StdCall, MenuStateFunc, "glutMenuStateFunc");

   type Glut_Proc_13 is access procedure
     (Key : Integer; X : Integer; Y : Integer);

   procedure SpecialFunc (P1 : Glut_Proc_13);
   pragma Import (StdCall, SpecialFunc, "glutSpecialFunc");
   procedure SpecialFunc (P1 : System.Address);

   type Glut_Proc_14 is access
     procedure (X : Integer; Y : Integer; Z : Integer);

   procedure SpaceballMotionFunc (P1 : Glut_Proc_14);
   pragma Import (StdCall, SpaceballMotionFunc, "glutSpaceballMotionFunc");

   type Glut_Proc_15 is access
     procedure (X : Integer; Y : Integer; Z : Integer);

   procedure SpaceballRotateFunc (P1 : Glut_Proc_15);
   pragma Import (StdCall, SpaceballRotateFunc, "glutSpaceballRotateFunc");

   type Glut_Proc_16 is access procedure (Button : Integer; State : Integer);

   procedure SpaceballButtonFunc (P1 : Glut_Proc_16);
   pragma Import (StdCall, SpaceballButtonFunc, "glutSpaceballButtonFunc");

   type Glut_Proc_17 is access procedure (Button : Integer; State : Integer);

   procedure ButtonBoxFunc (P1 : Glut_Proc_17);
   pragma Import (StdCall, ButtonBoxFunc, "glutButtonBoxFunc");

   type Glut_Proc_18 is access procedure (Dial : Integer; Value : Integer);

   procedure DialsFunc (P1 : Glut_Proc_18);
   pragma Import (StdCall, DialsFunc, "glutDialsFunc");

   type Glut_Proc_19 is access procedure (X : Integer; Y : Integer);

   procedure TabletMotionFunc (P1 : Glut_Proc_19);
   pragma Import (StdCall, TabletMotionFunc, "glutTabletMotionFunc");

   type Glut_Proc_20 is access procedure
     (Button : Integer; State : Integer; X : Integer; Y : Integer);

   procedure TabletButtonFunc (P1 : Glut_Proc_20);
   pragma Import (StdCall, TabletButtonFunc, "glutTabletButtonFunc");

   type Glut_Proc_21 is access procedure
     (Status : Integer; X : Integer; Y : Integer);

   procedure MenuStatusFunc (P1 : Glut_Proc_21);
   pragma Import (StdCall, MenuStatusFunc, "glutMenuStatusFunc");

   type Glut_Proc_22 is access procedure;

   procedure OverlayDisplayFunc (P1 : Glut_Proc_22);
   pragma Import (StdCall, OverlayDisplayFunc, "glutOverlayDisplayFunc");

   type Glut_Proc_23 is access procedure (State : Integer);

   procedure WindowStatusFunc (P1 : Glut_Proc_23);
   pragma Import (StdCall, WindowStatusFunc, "glutWindowStatusFunc");

   --  GLUT color index sub-API.

   procedure SetColor
     (P1          : Integer;
      Red_Value   : GL.Float;
      Green_Value : GL.Float;
      Blue_Value  : GL.Float);
   pragma Import (StdCall, SetColor, "glutSetColor");

   function GetColor
     (Ndx       : Integer;
      Component : Integer)
      return GL.Float;
   pragma Import (StdCall, GetColor, "glutGetColor");

   procedure CopyColormap (Win : Integer);
   pragma Import (StdCall, CopyColormap, "glutCopyColormap");

   --  GLUT state retrieval sub-API.

   function Get (Type_Id : GL.enum) return Integer;
   pragma Import (StdCall, Get, "glutGet");

   function DeviceGet (Type_Id : GL.enum) return Integer;
   pragma Import (StdCall, DeviceGet, "glutDeviceGet");

   --  GLUT extension support sub-API

   function ExtensionSupported
     (Name : Interfaces.C.Strings.chars_ptr)
      return Integer;
   pragma Import (StdCall, ExtensionSupported, "glutExtensionSupported");

   function ExtensionSupported (Name : String) return Integer;

   function GetModifiers return Integer;
   pragma Import (StdCall, GetModifiers, "glutGetModifiers");

   function LayerGet (Type_Id : GL.enum) return Integer;
   pragma Import (StdCall, LayerGet, "glutLayerGet");

   --  GLUT font sub-API

   procedure BitmapCharacter
     (Font      : System.Address;
      Character : Integer);
   pragma Import (StdCall, BitmapCharacter, "glutBitmapCharacter");

   function BitmapWidth
     (Font      : System.Address;
      Character : Integer)
      return Integer;
   pragma Import (StdCall, BitmapWidth, "glutBitmapWidth");

   procedure StrokeCharacter
     (Font      : System.Address;
      Character : Integer);
   pragma Import (StdCall, StrokeCharacter, "glutStrokeCharacter");

   function StrokeWidth
     (Font      : System.Address;
      Character : Integer)
      return Integer;
   pragma Import (StdCall, StrokeWidth, "glutStrokeWidth");

   function StrokeLength
     (Font   : System.Address;
      String : Interfaces.C.Strings.chars_ptr)
      return Integer;
   pragma Import (StdCall, StrokeLength, "glutStrokeLength");

   function BitmapLength
     (Font   : System.Address;
      String : Interfaces.C.Strings.chars_ptr)
      return Integer;
   pragma Import (StdCall, BitmapLength, "glutBitmapLength");

   --  GLUT pre-built models sub-API

   procedure WireSphere
     (Radius : GL.Double;
      Slices : GL.Int;
      Stacks : GL.Int);
   pragma Import (StdCall, WireSphere, "glutWireSphere");

   procedure SolidSphere
     (Radius : GL.Double;
      Slices : GL.Int;
      Stacks : GL.Int);
   pragma Import (StdCall, SolidSphere, "glutSolidSphere");

   procedure WireCone
     (Base   : GL.Double;
      Height : GL.Double;
      Slices : GL.Int;
      Stacks : GL.Int);
   pragma Import (StdCall, WireCone, "glutWireCone");

   procedure SolidCone
     (Base   : GL.Double;
      Height : GL.Double;
      Slices : GL.Int;
      Stacks : GL.Int);
   pragma Import (StdCall, SolidCone, "glutSolidCone");

   procedure WireCube (Size : GL.Double);
   pragma Import (StdCall, WireCube, "glutWireCube");

   procedure SolidCube (Size : GL.Double);
   pragma Import (StdCall, SolidCube, "glutSolidCube");

   procedure WireTorus
     (InnerRadius : GL.Double;
      OuterRadius : GL.Double;
      Sides       : GL.Int;
      Rings       : GL.Int);
   pragma Import (StdCall, WireTorus, "glutWireTorus");

   procedure SolidTorus
     (InnerRadius : GL.Double;
      OuterRadius : GL.Double;
      Sides       : GL.Int;
      Rings       : GL.Int);
   pragma Import (StdCall, SolidTorus, "glutSolidTorus");

   procedure WireDodecahedron;
   pragma Import (StdCall, WireDodecahedron, "glutWireDodecahedron");

   procedure SolidDodecahedron;
   pragma Import (StdCall, SolidDodecahedron, "glutSolidDodecahedron");

   procedure WireTeapot (Size : GL.Double);
   pragma Import (StdCall, WireTeapot, "glutWireTeapot");

   procedure SolidTeapot (Size : GL.Double);
   pragma Import (StdCall, SolidTeapot, "glutSolidTeapot");

   procedure WireOctahedron;
   pragma Import (StdCall, WireOctahedron, "glutWireOctahedron");

   procedure SolidOctahedron;
   pragma Import (StdCall, SolidOctahedron, "glutSolidOctahedron");

   procedure WireTetrahedron;
   pragma Import (StdCall, WireTetrahedron, "glutWireTetrahedron");

   procedure SolidTetrahedron;
   pragma Import (StdCall, SolidTetrahedron, "glutSolidTetrahedron");

   procedure WireIcosahedron;
   pragma Import (StdCall, WireIcosahedron, "glutWireIcosahedron");

   procedure SolidIcosahedron;
   pragma Import (StdCall, SolidIcosahedron, "glutSolidIcosahedron");

   function VideoResizeGet (Param : GL.enum) return Integer;
   pragma Import (StdCall, VideoResizeGet, "glutVideoResizeGet");

   procedure SetupVideoResizing;
   pragma Import (StdCall, SetupVideoResizing, "glutSetupVideoResizing");

   procedure StopVideoResizing;
   pragma Import (StdCall, StopVideoResizing, "glutStopVideoResizing");

   procedure VideoResize
     (X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);
   pragma Import (StdCall, VideoResize, "glutVideoResize");

   procedure VideoPan
     (X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);
   pragma Import (StdCall, VideoPan, "glutVideoPan");

   --  GLUT debugging sub-API

   procedure ReportErrors;
   pragma Import (StdCall, ReportErrors, "glutReportErrors");

   --  GLUT device control sub-API

   procedure IgnoreKeyRepeat (Ignore : Integer);
   pragma Import (StdCall, IgnoreKeyRepeat, "glutIgnoreKeyRepeat");

   procedure SetKeyRepeat (RepeatMode : Integer);
   pragma Import (StdCall, SetKeyRepeat, "glutSetKeyRepeat");

   procedure ForceJoystickFunc;
   pragma Import (StdCall, ForceJoystickFunc, "glutForceJoystickFunc");

   --  GLUT game mode sub-API

   procedure GameModeString (String : Interfaces.C.Strings.chars_ptr);
   pragma Import (StdCall, GameModeString, "glutGameModeString");

   function EnterGameMode return Integer;
   pragma Import (StdCall, EnterGameMode, "glutEnterGameMode");

   procedure LeaveGameMode;
   pragma Import (StdCall, LeaveGameMode, "glutLeaveGameMode");

   function GameModeGet (Mode : GL.enum) return Integer;
   pragma Import (StdCall, GameModeGet, "glutGameModeGet");

end GLUT;
