--
--  Copyright  (c) 2002-2003, David Holm
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met :
--
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice this list of conditions and the following disclaimer in the
--     documentation
--     and/or other materials provided with the distribution.
--   * The names of its contributors may not be used to endorse or promote
--     products derived from this software without specific prior written
--     permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES;
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--


with Interfaces.C;
with System;
with OpenGL;

package OpenGL.GLUT is

   GLUT_API_VERSION     : constant  := 3;
   GLUT_XLIB_IMPLEMENTATION : constant  := 13;
   GLUT_RGB                 : constant  := 8#0000#;
   GLUT_RGBA                : constant  := 8#0000#;
   GLUT_INDEX               : constant  := 1;
   GLUT_SINGLE              : constant  := 8#0000#;
   GLUT_DOUBLE              : constant  := 2;
   GLUT_ACCUM               : constant  := 4;
   GLUT_ALPHA               : constant  := 8;
   GLUT_DEPTH               : constant  := 16#0010#;
   GLUT_STENCIL             : constant  := 16#0020#;
   GLUT_MULTISAMPLE         : constant  := 16#0080#;
   GLUT_STEREO              : constant  := 16#0100#;
   GLUT_LUMINANCE           : constant  := 16#0200#;
   GLUT_LEFT_BUTTON         : constant  := 8#0000#;
   GLUT_MIDDLE_BUTTON       : constant  := 1;
   GLUT_RIGHT_BUTTON        : constant  := 2;
   GLUT_DOWN                : constant  := 8#0000#;
   GLUT_UP                  : constant  := 1;
   GLUT_KEY_F1              : constant  := 1;
   GLUT_KEY_F2              : constant  := 2;
   GLUT_KEY_F3              : constant  := 3;
   GLUT_KEY_F4              : constant  := 4;
   GLUT_KEY_F5              : constant  := 5;
   GLUT_KEY_F6              : constant  := 6;
   GLUT_KEY_F7              : constant  := 7;
   GLUT_KEY_F8              : constant  := 8;
   GLUT_KEY_F9              : constant  := 9;
   GLUT_KEY_F10             : constant  := 10;
   GLUT_KEY_F11             : constant  := 11;
   GLUT_KEY_F12             : constant  := 12;
   GLUT_KEY_LEFT            : constant  := 100;
   GLUT_KEY_UP              : constant  := 101;
   GLUT_KEY_RIGHT           : constant  := 102;
   GLUT_KEY_DOWN            : constant  := 103;
   GLUT_KEY_PAGE_UP         : constant  := 104;
   GLUT_KEY_PAGE_DOWN       : constant  := 105;
   GLUT_KEY_HOME            : constant  := 106;
   GLUT_KEY_END             : constant  := 107;
   GLUT_KEY_INSERT          : constant  := 108;
   GLUT_LEFT                : constant  := 8#0000#;
   GLUT_ENTERED             : constant  := 1;
   GLUT_MENU_NOT_IN_USE     : constant  := 8#0000#;
   GLUT_MENU_IN_USE         : constant  := 1;
   GLUT_NOT_VISIBLE         : constant  := 8#0000#;
   GLUT_VISIBLE             : constant  := 1;
   GLUT_HIDDEN              : constant  := 8#0000#;
   GLUT_FULLY_RETAINED      : constant  := 1;
   GLUT_PARTIALLY_RETAINED  : constant  := 2;
   GLUT_FULLY_COVERED       : constant  := 3;
   GLUT_RED                 : constant  := 8#0000#;
   GLUT_GREEN               : constant  := 1;
   GLUT_BLUE                : constant  := 2;
   GLUT_NORMAL              : constant  := 8#0000#;
   GLUT_OVERLAY             : constant  := 1;
   GLUT_WINDOW_X            : constant  := 100;
   GLUT_WINDOW_Y            : constant  := 101;
   GLUT_WINDOW_WIDTH        : constant  := 102;
   GLUT_WINDOW_HEIGHT       : constant  := 103;
   GLUT_WINDOW_BUFFER_SIZE  : constant  := 104;
   GLUT_WINDOW_STENCIL_SIZE : constant  := 105;
   GLUT_WINDOW_DEPTH_SIZE   : constant  := 106;
   GLUT_WINDOW_RED_SIZE     : constant  := 107;
   GLUT_WINDOW_GREEN_SIZE   : constant  := 108;
   GLUT_WINDOW_BLUE_SIZE    : constant  := 109;
   GLUT_WINDOW_ALPHA_SIZE   : constant  := 110;
   GLUT_WINDOW_ACCUM_RED_SIZE : constant  := 111;
   GLUT_WINDOW_ACCUM_GREEN_SIZE : constant  := 112;
   GLUT_WINDOW_ACCUM_BLUE_SIZE  : constant  := 113;
   GLUT_WINDOW_ACCUM_ALPHA_SIZE : constant  := 114;
   GLUT_WINDOW_DOUBLEBUFFER     : constant  := 115;
   GLUT_WINDOW_RGBA             : constant  := 116;
   GLUT_WINDOW_PARENT           : constant  := 117;
   GLUT_WINDOW_NUM_CHILDREN     : constant  := 118;
   GLUT_WINDOW_COLORMAP_SIZE    : constant  := 119;
   GLUT_WINDOW_NUM_SAMPLES      : constant  := 120;
   GLUT_WINDOW_STEREO           : constant  := 121;
   GLUT_WINDOW_CURSOR           : constant  := 122;
   GLUT_SCREEN_WIDTH            : constant  := 200;
   GLUT_SCREEN_HEIGHT           : constant  := 201;
   GLUT_SCREEN_WIDTH_MM         : constant  := 202;
   GLUT_SCREEN_HEIGHT_MM        : constant  := 203;
   GLUT_MENU_NUM_ITEMS          : constant  := 300;
   GLUT_DISPLAY_MODE_POSSIBLE   : constant  := 400;
   GLUT_INIT_WINDOW_X           : constant  := 500;
   GLUT_INIT_WINDOW_Y           : constant  := 501;
   GLUT_INIT_WINDOW_WIDTH       : constant  := 502;
   GLUT_INIT_WINDOW_HEIGHT      : constant  := 503;
   GLUT_INIT_DISPLAY_MODE       : constant  := 504;
   GLUT_ELAPSED_TIME            : constant  := 700;
   GLUT_WINDOW_FORMAT_ID        : constant  := 123;
   GLUT_HAS_KEYBOARD            : constant  := 600;
   GLUT_HAS_MOUSE               : constant  := 601;
   GLUT_HAS_SPACEBALL           : constant  := 602;
   GLUT_HAS_DIAL_AND_BUTTON_BOX : constant  := 603;
   GLUT_HAS_TABLET              : constant  := 604;
   GLUT_NUM_MOUSE_BUTTONS       : constant  := 605;
   GLUT_NUM_SPACEBALL_BUTTONS   : constant  := 606;
   GLUT_NUM_BUTTON_BOX_BUTTONS  : constant  := 607;
   GLUT_NUM_DIALS               : constant  := 608;
   GLUT_NUM_TABLET_BUTTONS      : constant  := 609;
   GLUT_DEVICE_IGNORE_KEY_REPEAT : constant  := 610;
   GLUT_DEVICE_KEY_REPEAT        : constant  := 611;
   GLUT_HAS_JOYSTICK             : constant  := 612;
   GLUT_OWNS_JOYSTICK            : constant  := 613;
   GLUT_JOYSTICK_BUTTONS         : constant  := 614;
   GLUT_JOYSTICK_AXES            : constant  := 615;
   GLUT_JOYSTICK_POLL_RATE       : constant  := 616;
   GLUT_OVERLAY_POSSIBLE         : constant  := 800;
   GLUT_LAYER_IN_USE             : constant  := 801;
   GLUT_HAS_OVERLAY              : constant  := 802;
   GLUT_TRANSPARENT_INDEX        : constant  := 803;
   GLUT_NORMAL_DAMAGED           : constant  := 804;
   GLUT_OVERLAY_DAMAGED          : constant  := 805;
   GLUT_VIDEO_RESIZE_POSSIBLE    : constant  := 900;
   GLUT_VIDEO_RESIZE_IN_USE      : constant  := 901;
   GLUT_VIDEO_RESIZE_X_DELTA     : constant  := 902;
   GLUT_VIDEO_RESIZE_Y_DELTA     : constant  := 903;
   GLUT_VIDEO_RESIZE_WIDTH_DELTA : constant  := 904;
   GLUT_VIDEO_RESIZE_HEIGHT_DELTA : constant  := 905;
   GLUT_VIDEO_RESIZE_X            : constant  := 906;
   GLUT_VIDEO_RESIZE_Y            : constant  := 907;
   GLUT_VIDEO_RESIZE_WIDTH        : constant  := 908;
   GLUT_VIDEO_RESIZE_HEIGHT       : constant  := 909;
   GLUT_ACTIVE_SHIFT              : constant  := 1;
   GLUT_ACTIVE_CTRL               : constant  := 2;
   GLUT_ACTIVE_ALT                : constant  := 4;
   GLUT_CURSOR_RIGHT_ARROW        : constant  := 8#0000#;
   GLUT_CURSOR_LEFT_ARROW         : constant  := 1;
   GLUT_CURSOR_INFO               : constant  := 2;
   GLUT_CURSOR_DESTROY            : constant  := 3;
   GLUT_CURSOR_HELP               : constant  := 4;
   GLUT_CURSOR_CYCLE              : constant  := 5;
   GLUT_CURSOR_SPRAY              : constant  := 6;
   GLUT_CURSOR_WAIT               : constant  := 7;
   GLUT_CURSOR_TEXT               : constant  := 8;
   GLUT_CURSOR_CROSSHAIR          : constant  := 9;
   GLUT_CURSOR_UP_DOWN            : constant  := 10;
   GLUT_CURSOR_LEFT_RIGHT         : constant  := 11;
   GLUT_CURSOR_TOP_SIDE           : constant  := 12;
   GLUT_CURSOR_BOTTOM_SIDE        : constant  := 13;
   GLUT_CURSOR_LEFT_SIDE          : constant  := 14;
   GLUT_CURSOR_RIGHT_SIDE         : constant  := 15;
   GLUT_CURSOR_TOP_LEFT_CORNER    : constant  := 16#0010#;
   GLUT_CURSOR_TOP_RIGHT_CORNER   : constant  := 17;
   GLUT_CURSOR_BOTTOM_RIGHT_CORNER : constant  := 18;
   GLUT_CURSOR_BOTTOM_LEFT_CORNER  : constant  := 19;
   GLUT_CURSOR_INHERIT             : constant  := 100;
   GLUT_CURSOR_NONE                : constant  := 101;
   GLUT_CURSOR_FULL_CROSSHAIR      : constant  := 102;
   GLUT_KEY_REPEAT_OFF             : constant  := 8#0000#;
   GLUT_KEY_REPEAT_ON              : constant  := 1;
   GLUT_KEY_REPEAT_DEFAULT         : constant  := 2;
   GLUT_JOYSTICK_BUTTON_A          : constant  := 1;
   GLUT_JOYSTICK_BUTTON_B          : constant  := 2;
   GLUT_JOYSTICK_BUTTON_C          : constant  := 4;
   GLUT_JOYSTICK_BUTTON_D          : constant  := 8;
   GLUT_GAME_MODE_ACTIVE           : constant  := 8#0000#;
   GLUT_GAME_MODE_POSSIBLE         : constant  := 1;
   GLUT_GAME_MODE_WIDTH            : constant  := 2;
   GLUT_GAME_MODE_HEIGHT           : constant  := 3;
   GLUT_GAME_MODE_PIXEL_DEPTH      : constant  := 4;
   GLUT_GAME_MODE_REFRESH_RATE     : constant  := 5;
   GLUT_GAME_MODE_DISPLAY_CHANGED  : constant  := 6;

   glutStrokeRoman        : System.Address;
   glutStrokeMonoRoman    : System.Address;
   glutBitmap9By15        : System.Address;
   glutBitmap8By13        : System.Address;
   glutBitmapTimesRoman10 : System.Address;
   glutBitmapTimesRoman24 : System.Address;
   glutBitmapHelvetica10  : System.Address;
   glutBitmapHelvetica12  : System.Address;
   glutBitmapHelvetica18  : System.Address;

   procedure glutInit (argcp : access Integer;
                      argv  : GLubytePtr);

   procedure glutInitDisplayMode (mode : Interfaces.C.unsigned);

   procedure glutInitDisplayString (str : OpenGL.GLubytePtr);

   procedure glutInitWindowPosition (x : Integer;
                                    y : Integer);

   procedure glutInitWindowSize (width  : Integer;
                                height : Integer);

   procedure glutMainLoop;

   function glutCreateWindow (title : OpenGL.GLubytePtr) return Integer;

   function glutCreateSubWindow (win    : Integer;
                                x      : Integer;
                                y      : Integer;
                                width  : Integer;
                                height : Integer)
                                        return Integer;

   procedure glutDestroyWindow (win : Integer);

   procedure glutPostRedisplay;

   procedure glutPostWindowRedisplay (win : Integer);

   procedure glutSwapBuffers;

   function glutGetWindow return Integer;

   procedure glutSetWindow (win : Integer);

   procedure glutSetWindowTitle (title : OpenGL.GLubytePtr);

   procedure glutSetIconTitle (title : OpenGL.GLubytePtr);

   procedure glutPositionWindow (x : Integer;
                                y : Integer);

   procedure glutReshapeWindow (width  : Integer;
                               height : Integer);

   procedure glutPopWindow;

   procedure glutPushWindow;

   procedure glutIconifyWindow;

   procedure glutShowWindow;

   procedure glutHideWindow;

   procedure glutFullScreen;

   procedure glutSetCursor (cursor : Integer);

   procedure glutWarpPointer (x : Integer;
                             y : Integer);

   procedure glutEstablishOverlay;

   procedure glutRemoveOverlay;

   procedure glutUseLayer (layer : OpenGL.GLenum);

   procedure glutPostOverlayRedisplay;

   procedure glutPostWindowOverlayRedisplay (win : Integer);

   procedure glutShowOverlay;

   procedure glutHideOverlay;

   type ProcCreateMenu is access procedure (Selected_Item : Integer);
   function glutCreateMenu (MenuCallback : ProcCreateMenu) return Integer;

   procedure glutDestroyMenu (menu : Integer);

   function glutGetMenu return Integer;

   procedure glutSetMenu (menu : Integer);

   procedure glutAddMenuEntry (label : OpenGL.GLubytePtr;
                              value : Integer);

   procedure glutAddSubMenu (label   : OpenGL.GLubytePtr;
                            submenu : Integer);

   procedure glutChangeToMenuEntry (item  : Integer;
                                   label : OpenGL.GLubytePtr;
                                   value : Integer);

   procedure glutChangeToSubMenu (item    : Integer;
                                 label   : OpenGL.GLubytePtr;
                                 submenu : Integer);

   procedure glutRemoveMenuItem (item : Integer);

   procedure glutAttachMenu (button : Integer);

   procedure glutDetachMenu (button : Integer);

   type ProcDisplayFunc is access procedure;
   procedure glutDisplayFunc (func : ProcDisplayFunc);

   type ProcReshapeFunc is access procedure (width, height : Integer);
   procedure glutReshapeFunc (func : ProcReshapeFunc);

   type ProcKeyboardFunc is access procedure (Key : GLubyte;
                                             x, y : Integer);
   procedure glutKeyboardFunc (func : ProcKeyboardFunc);

   type ProcMouseFunc is access procedure (button, state, x, y : Integer);
   procedure glutMouseFunc (func : ProcMouseFunc);

   type ProcMotionFunc is access procedure (x, y : Integer);
   procedure glutMotionFunc (func : ProcMotionFunc);

   type ProcPassiveMotionFunc is access procedure (x, y : Integer);
   procedure glutPassiveMotionFunc (func : ProcPassiveMotionFunc);

   type ProcEntryFunc is access procedure (state : Integer);
   procedure glutEntryFunc (func : ProcEntryFunc);

   type ProcVisibilityFunc is access procedure (state : Integer);
   procedure glutVisibilityFunc (func : ProcVisibilityFunc);

   type ProcIdleFunc is access procedure;
   procedure glutIdleFunc (func : ProcIdleFunc);

   type ProcTimerFunc is access procedure (Value : Integer);
   procedure glutTimerFunc (millis : Interfaces.C.unsigned;
                           func   : ProcTimerFunc;
                           value  : Integer);

   type ProcMenuStateFunc is access procedure (Status : Integer);
   procedure glutMenuStateFunc (func : ProcMenuStateFunc);

   type ProcSpecialFunc is access procedure (key, x, y : Integer);
   procedure glutSpecialFunc (func : ProcSpecialFunc);

   type ProcSpaceballMotionFunc is access procedure (x, y, z : Integer);
   procedure glutSpaceballMotionFunc (func : ProcSpaceballMotionFunc);

   type ProcSpaceballRotateFunc is access procedure (x, y, z : Integer);
   procedure glutSpaceballRotateFunc (func : ProcSpaceballRotateFunc);

   type ProcSpaceballButtonFunc is access procedure (Button, State : Integer);
   procedure glutSpaceballButtonFunc (func : ProcSpaceballButtonFunc);

   type ProcButtonBoxFunc is access procedure (Button, State : Integer);
   procedure glutButtonBoxFunc (func : ProcButtonBoxFunc);

   type ProcDialsFunc is access procedure (Dial, Value : Integer);
   procedure glutDialsFunc (func : ProcDialsFunc);

   type ProcTabletMotionFunc is access procedure (x, y : Integer);
   procedure glutTabletMotionFunc (func : ProcTabletMotionFunc);

   type ProcTabletButtonFunc is access procedure
      (Button, State, x, y : Integer);
   procedure glutTabletButtonFunc (func : ProcTabletButtonFunc);

   type ProcMenuStatusFunc is access procedure (Status, x, y : Integer);
   procedure glutMenuStatusFunc (func : ProcMenuStatusFunc);

   type ProcOverlayDisplayFunc is access procedure;
   procedure glutOverlayDisplayFunc (func : ProcOverlayDisplayFunc);

   type ProcWindowStatusFunc is access procedure (State : Integer);
   procedure glutWindowStatusFunc (func : ProcWindowStatusFunc);

   type ProcKeyboardUpFunc is access procedure (Key : GLubyte;
                                               x, y : Integer);
   procedure glutKeyboardUpFunc (func : ProcKeyboardUpFunc);

   type ProcSpecialUpFunc is access procedure (Key, x, y : Integer);
   procedure glutSpecialUpFunc (func : ProcSpecialUpFunc);

   type ProcJoystickFunc is access procedure (Button_Mask : GLuint;
                                             x, y, z : Integer);
   procedure glutJoystickFunc (func         : ProcJoystickFunc;
                              pollInterval : Integer);

   procedure glutSetColor (p1    : Integer;
                          red   : OpenGL.GLfloat;
                          green : OpenGL.GLfloat;
                          blue  : OpenGL.GLfloat);

   function glutGetColor (ndx       : Integer;
                         component : Integer)
                                    return OpenGL.GLfloat;

   procedure glutCopyColormap (win : Integer);

   function glutGet (c_type : OpenGL.GLenum) return Integer;

   function glutDeviceGet (c_type : OpenGL.GLenum) return Integer;

   function glutExtensionSupported (name : OpenGL.GLubytePtr) return Integer;

   function glutGetModifiers return Integer;

   function glutLayerGet (c_type : OpenGL.GLenum) return Integer;

   procedure glutBitmapCharacter (font      : System.Address;
                                 character : Integer);

   function glutBitmapWidth (font      : System.Address;
                            character : Integer)
                                       return Integer;

   procedure glutStrokeCharacter (font      : System.Address;
                                 character : Integer);

   function glutStrokeWidth (font      : System.Address;
                            character : Integer)
                                       return Integer;

   function glutBitmapLength (font   : System.Address;
                             str : OpenGL.GLubytePtr)
                                     return Integer;

   function glutStrokeLength (font   : System.Address;
                             str : OpenGL.GLubytePtr)
                                     return Integer;

   procedure glutWireSphere (radius : OpenGL.GLdouble;
                            slices : OpenGL.GLint;
                            stacks : OpenGL.GLint);

   procedure glutSolidSphere (radius : OpenGL.GLdouble;
                             slices : OpenGL.GLint;
                             stacks : OpenGL.GLint);

   procedure glutWireCone (base   : OpenGL.GLdouble;
                          height : OpenGL.GLdouble;
                          slices : OpenGL.GLint;
                          stacks : OpenGL.GLint);

   procedure glutSolidCone (base   : OpenGL.GLdouble;
                           height : OpenGL.GLdouble;
                           slices : OpenGL.GLint;
                           stacks : OpenGL.GLint);

   procedure glutWireCube (size : OpenGL.GLdouble);

   procedure glutSolidCube (size : OpenGL.GLdouble);

   procedure glutWireTorus (innerRadius : OpenGL.GLdouble;
                           outerRadius : OpenGL.GLdouble;
                           sides       : OpenGL.GLint;
                           rings       : OpenGL.GLint);

   procedure glutSolidTorus (innerRadius : OpenGL.GLdouble;
                            outerRadius : OpenGL.GLdouble;
                            sides       : OpenGL.GLint;
                            rings       : OpenGL.GLint);

   procedure glutWireDodecahedron;

   procedure glutSolidDodecahedron;

   procedure glutWireTeapot (size : OpenGL.GLdouble);

   procedure glutSolidTeapot (size : OpenGL.GLdouble);

   procedure glutWireOctahedron;

   procedure glutSolidOctahedron;

   procedure glutWireTetrahedron;

   procedure glutSolidTetrahedron;

   procedure glutWireIcosahedron;

   procedure glutSolidIcosahedron;

   function glutVideoResizeGet (param : OpenGL.GLenum) return Integer;

   procedure glutSetupVideoResizing;

   procedure glutStopVideoResizing;

   procedure glutVideoResize (x      : Integer;
                             y      : Integer;
                             width  : Integer;
                             height : Integer);

   procedure glutVideoPan (x      : Integer;
                          y      : Integer;
                          width  : Integer;
                          height : Integer);

   procedure glutReportErrors;

   procedure glutIgnoreKeyRepeat (ignore : Integer);

   procedure glutSetKeyRepeat (repeatMode : Integer);

   procedure glutForceJoystickFunc;

   procedure glutGameModeString (str : String);

   function glutEnterGameMode return Integer;

   procedure glutLeaveGameMode;

   function glutGameModeGet (mode : OpenGL.GLenum) return Integer;

private

   pragma Import (C, glutInit, "glutInit");

   pragma Import (C, glutInitDisplayMode, "glutInitDisplayMode");

   pragma Import (C, glutInitDisplayString, "glutInitDisplayString");

   pragma Import (C, glutInitWindowPosition, "glutInitWindowPosition");

   pragma Import (C, glutInitWindowSize, "glutInitWindowSize");

   pragma Import (C, glutMainLoop, "glutMainLoop");

   pragma Import (C, glutCreateWindow, "glutCreateWindow");

   pragma Import (C, glutCreateSubWindow, "glutCreateSubWindow");

   pragma Import (C, glutDestroyWindow, "glutDestroyWindow");

   pragma Import (C, glutPostRedisplay, "glutPostRedisplay");

   pragma Import (C, glutPostWindowRedisplay, "glutPostWindowRedisplay");

   pragma Import (C, glutSwapBuffers, "glutSwapBuffers");

   pragma Import (C, glutGetWindow, "glutGetWindow");

   pragma Import (C, glutSetWindow, "glutSetWindow");

   pragma Import (C, glutSetWindowTitle, "glutSetWindowTitle");

   pragma Import (C, glutSetIconTitle, "glutSetIconTitle");

   pragma Import (C, glutPositionWindow, "glutPositionWindow");

   pragma Import (C, glutReshapeWindow, "glutReshapeWindow");

   pragma Import (C, glutPopWindow, "glutPopWindow");

   pragma Import (C, glutPushWindow, "glutPushWindow");

   pragma Import (C, glutIconifyWindow, "glutIconifyWindow");

   pragma Import (C, glutShowWindow, "glutShowWindow");

   pragma Import (C, glutHideWindow, "glutHideWindow");

   pragma Import (C, glutFullScreen, "glutFullScreen");

   pragma Import (C, glutSetCursor, "glutSetCursor");

   pragma Import (C, glutWarpPointer, "glutWarpPointer");

   pragma Import (C, glutEstablishOverlay, "glutEstablishOverlay");

   pragma Import (C, glutRemoveOverlay, "glutRemoveOverlay");

   pragma Import (C, glutUseLayer, "glutUseLayer");

   pragma Import (C, glutPostOverlayRedisplay, "glutPostOverlayRedisplay");

   pragma Import
      (C, glutPostWindowOverlayRedisplay, "glutPostWindowOverlayRedisplay");

   pragma Import (C, glutShowOverlay, "glutShowOverlay");

   pragma Import (C, glutHideOverlay, "glutHideOverlay");

   pragma Import (C, glutCreateMenu, "glutCreateMenu");

   pragma Import (C, glutDestroyMenu, "glutDestroyMenu");

   pragma Import (C, glutGetMenu, "glutGetMenu");

   pragma Import (C, glutSetMenu, "glutSetMenu");

   pragma Import (C, glutAddMenuEntry, "glutAddMenuEntry");

   pragma Import (C, glutAddSubMenu, "glutAddSubMenu");

   pragma Import (C, glutChangeToMenuEntry, "glutChangeToMenuEntry");

   pragma Import (C, glutChangeToSubMenu, "glutChangeToSubMenu");

   pragma Import (C, glutRemoveMenuItem, "glutRemoveMenuItem");

   pragma Import (C, glutAttachMenu, "glutAttachMenu");

   pragma Import (C, glutDetachMenu, "glutDetachMenu");

   pragma Import (C, glutDisplayFunc, "glutDisplayFunc");

   pragma Import (C, glutReshapeFunc, "glutReshapeFunc");

   pragma Import (C, glutKeyboardFunc, "glutKeyboardFunc");

   pragma Import (C, glutMouseFunc, "glutMouseFunc");

   pragma Import (C, glutMotionFunc, "glutMotionFunc");

   pragma Import (C, glutPassiveMotionFunc, "glutPassiveMotionFunc");

   pragma Import (C, glutEntryFunc, "glutEntryFunc");

   pragma Import (C, glutVisibilityFunc, "glutVisibilityFunc");

   pragma Import (C, glutIdleFunc, "glutIdleFunc");

   pragma Import (C, glutTimerFunc, "glutTimerFunc");

   pragma Import (C, glutMenuStateFunc, "glutMenuStateFunc");

   pragma Import (C, glutSpecialFunc, "glutSpecialFunc");

   pragma Import (C, glutSpaceballMotionFunc, "glutSpaceballMotionFunc");

   pragma Import (C, glutSpaceballRotateFunc, "glutSpaceballRotateFunc");

   pragma Import (C, glutSpaceballButtonFunc, "glutSpaceballButtonFunc");

   pragma Import (C, glutButtonBoxFunc, "glutButtonBoxFunc");

   pragma Import (C, glutDialsFunc, "glutDialsFunc");

   pragma Import (C, glutTabletMotionFunc, "glutTabletMotionFunc");

   pragma Import (C, glutTabletButtonFunc, "glutTabletButtonFunc");

   pragma Import (C, glutMenuStatusFunc, "glutMenuStatusFunc");

   pragma Import (C, glutOverlayDisplayFunc, "glutOverlayDisplayFunc");

   pragma Import (C, glutWindowStatusFunc, "glutWindowStatusFunc");

   pragma Import (C, glutKeyboardUpFunc, "glutKeyboardUpFunc");

   pragma Import (C, glutSpecialUpFunc, "glutSpecialUpFunc");

   pragma Import (C, glutJoystickFunc, "glutJoystickFunc");

   pragma Import (C, glutSetColor, "glutSetColor");

   pragma Import (C, glutGetColor, "glutGetColor");

   pragma Import (C, glutCopyColormap, "glutCopyColormap");

   pragma Import (C, glutGet, "glutGet");

   pragma Import (C, glutDeviceGet, "glutDeviceGet");

   pragma Import (C, glutExtensionSupported, "glutExtensionSupported");

   pragma Import (C, glutGetModifiers, "glutGetModifiers");

   pragma Import (C, glutLayerGet, "glutLayerGet");

   pragma Import (C, glutBitmapCharacter, "glutBitmapCharacter");

   pragma Import (C, glutBitmapWidth, "glutBitmapWidth");

   pragma Import (C, glutStrokeCharacter, "glutStrokeCharacter");

   pragma Import (C, glutStrokeWidth, "glutStrokeWidth");

   pragma Import (C, glutBitmapLength, "glutBitmapLength");

   pragma Import (C, glutStrokeLength, "glutStrokeLength");

   pragma Import (C, glutWireSphere, "glutWireSphere");

   pragma Import (C, glutSolidSphere, "glutSolidSphere");

   pragma Import (C, glutWireCone, "glutWireCone");

   pragma Import (C, glutSolidCone, "glutSolidCone");

   pragma Import (C, glutWireCube, "glutWireCube");

   pragma Import (C, glutSolidCube, "glutSolidCube");

   pragma Import (C, glutWireTorus, "glutWireTorus");

   pragma Import (C, glutSolidTorus, "glutSolidTorus");

   pragma Import (C, glutWireDodecahedron, "glutWireDodecahedron");

   pragma Import (C, glutSolidDodecahedron, "glutSolidDodecahedron");

   pragma Import (C, glutWireTeapot, "glutWireTeapot");

   pragma Import (C, glutSolidTeapot, "glutSolidTeapot");

   pragma Import (C, glutWireOctahedron, "glutWireOctahedron");

   pragma Import (C, glutSolidOctahedron, "glutSolidOctahedron");

   pragma Import (C, glutWireTetrahedron, "glutWireTetrahedron");

   pragma Import (C, glutSolidTetrahedron, "glutSolidTetrahedron");

   pragma Import (C, glutWireIcosahedron, "glutWireIcosahedron");

   pragma Import (C, glutSolidIcosahedron, "glutSolidIcosahedron");

   pragma Import (C, glutVideoResizeGet, "glutVideoResizeGet");

   pragma Import (C, glutSetupVideoResizing, "glutSetupVideoResizing");

   pragma Import (C, glutStopVideoResizing, "glutStopVideoResizing");

   pragma Import (C, glutVideoResize, "glutVideoResize");

   pragma Import (C, glutVideoPan, "glutVideoPan");

   pragma Import (C, glutReportErrors, "glutReportErrors");

   pragma Import (C, glutIgnoreKeyRepeat, "glutIgnoreKeyRepeat");

   pragma Import (C, glutSetKeyRepeat, "glutSetKeyRepeat");

   pragma Import (C, glutForceJoystickFunc, "glutForceJoystickFunc");

   pragma Import (C, glutGameModeString, "glutGameModeString");

   pragma Import (C, glutEnterGameMode, "glutEnterGameMode");

   pragma Import (C, glutLeaveGameMode, "glutLeaveGameMode");

   pragma Import (C, glutGameModeGet, "glutGameModeGet");

end OpenGL.GLUT;
