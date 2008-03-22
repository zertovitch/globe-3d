--
--  Copyright  (c) 2002-2003, David Holm
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met:
--
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice,
--     this list of conditions and the following disclaimer in the
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
with Interfaces.C.Strings;
with System;

package OpenGL.GLFW is

   GLFW_VERSION_MAJOR               : constant := 2;
   GLFW_VERSION_MINOR               : constant := 4;
   GLFW_VERSION_REVISION            : constant := 8#0000#;
   GLFW_RELEASE                     : constant := 8#0000#;
   GLFW_PRESS                       : constant := 1;
   GLFW_KEY_UNKNOWN                 : constant := -1;
   GLFW_KEY_SPACE                   : constant := 16#0020#;
   GLFW_KEY_SPECIAL                 : constant := 16#0100#;
   GLFW_KEY_ESC                     : constant := 16#0101#;
   GLFW_KEY_F1                      : constant := 16#0102#;
   GLFW_KEY_F2                      : constant := 16#0103#;
   GLFW_KEY_F3                      : constant := 16#0104#;
   GLFW_KEY_F4                      : constant := 16#0105#;
   GLFW_KEY_F5                      : constant := 16#0106#;
   GLFW_KEY_F6                      : constant := 16#0107#;
   GLFW_KEY_F7                      : constant := 16#0108#;
   GLFW_KEY_F8                      : constant := 16#0109#;
   GLFW_KEY_F9                      : constant := 16#010A#;
   GLFW_KEY_F10                     : constant := 16#010B#;
   GLFW_KEY_F11                     : constant := 16#010C#;
   GLFW_KEY_F12                     : constant := 16#010D#;
   GLFW_KEY_F13                     : constant := 16#010E#;
   GLFW_KEY_F14                     : constant := 16#010F#;
   GLFW_KEY_F15                     : constant := 16#0110#;
   GLFW_KEY_F16                     : constant := 16#0111#;
   GLFW_KEY_F17                     : constant := 16#0112#;
   GLFW_KEY_F18                     : constant := 16#0113#;
   GLFW_KEY_F19                     : constant := 16#0114#;
   GLFW_KEY_F20                     : constant := 16#0115#;
   GLFW_KEY_F21                     : constant := 16#0116#;
   GLFW_KEY_F22                     : constant := 16#0117#;
   GLFW_KEY_F23                     : constant := 16#0118#;
   GLFW_KEY_F24                     : constant := 16#0119#;
   GLFW_KEY_F25                     : constant := 16#011A#;
   GLFW_KEY_UP                      : constant := 16#011B#;
   GLFW_KEY_DOWN                    : constant := 16#011C#;
   GLFW_KEY_LEFT                    : constant := 16#011D#;
   GLFW_KEY_RIGHT                   : constant := 16#011E#;
   GLFW_KEY_LSHIFT                  : constant := 16#011F#;
   GLFW_KEY_RSHIFT                  : constant := 16#0120#;
   GLFW_KEY_LCTRL                   : constant := 16#0121#;
   GLFW_KEY_RCTRL                   : constant := 16#0122#;
   GLFW_KEY_LALT                    : constant := 16#0123#;
   GLFW_KEY_RALT                    : constant := 16#0124#;
   GLFW_KEY_TAB                     : constant := 16#0125#;
   GLFW_KEY_ENTER                   : constant := 16#0126#;
   GLFW_KEY_BACKSPACE               : constant := 16#0127#;
   GLFW_KEY_INSERT                  : constant := 16#0128#;
   GLFW_KEY_DEL                     : constant := 16#0129#;
   GLFW_KEY_PAGEUP                  : constant := 16#012A#;
   GLFW_KEY_PAGEDOWN                : constant := 16#012B#;
   GLFW_KEY_HOME                    : constant := 16#012C#;
   GLFW_KEY_END                     : constant := 16#012D#;
   GLFW_KEY_KP_0                    : constant := 16#012E#;
   GLFW_KEY_KP_1                    : constant := 16#012F#;
   GLFW_KEY_KP_2                    : constant := 16#0130#;
   GLFW_KEY_KP_3                    : constant := 16#0131#;
   GLFW_KEY_KP_4                    : constant := 16#0132#;
   GLFW_KEY_KP_5                    : constant := 16#0133#;
   GLFW_KEY_KP_6                    : constant := 16#0134#;
   GLFW_KEY_KP_7                    : constant := 16#0135#;
   GLFW_KEY_KP_8                    : constant := 16#0136#;
   GLFW_KEY_KP_9                    : constant := 16#0137#;
   GLFW_KEY_KP_DIVIDE               : constant := 16#0138#;
   GLFW_KEY_KP_MULTIPLY             : constant := 16#0139#;
   GLFW_KEY_KP_SUBTRACT             : constant := 16#013A#;
   GLFW_KEY_KP_ADD                  : constant := 16#013B#;
   GLFW_KEY_KP_DECIMAL              : constant := 16#013C#;
   GLFW_KEY_KP_EQUAL                : constant := 16#013D#;
   GLFW_KEY_KP_ENTER                : constant := 16#013E#;
   GLFW_KEY_LAST                    : constant := 16#013E#;
   GLFW_MOUSE_BUTTON_LEFT           : constant := 8#0000#;
   GLFW_MOUSE_BUTTON_RIGHT          : constant := 1;
   GLFW_MOUSE_BUTTON_MIDDLE         : constant := 2;
   GLFW_MOUSE_BUTTON_LAST           : constant := 2;
   GLFW_JOYSTICK_1                  : constant := 8#0000#;
   GLFW_JOYSTICK_2                  : constant := 1;
   GLFW_JOYSTICK_3                  : constant := 2;
   GLFW_JOYSTICK_4                  : constant := 3;
   GLFW_JOYSTICK_5                  : constant := 4;
   GLFW_JOYSTICK_6                  : constant := 5;
   GLFW_JOYSTICK_7                  : constant := 6;
   GLFW_JOYSTICK_8                  : constant := 7;
   GLFW_JOYSTICK_9                  : constant := 8;
   GLFW_JOYSTICK_10                 : constant := 9;
   GLFW_JOYSTICK_11                 : constant := 16#000A#;
   GLFW_JOYSTICK_12                 : constant := 16#000B#;
   GLFW_JOYSTICK_13                 : constant := 16#000C#;
   GLFW_JOYSTICK_14                 : constant := 16#000D#;
   GLFW_JOYSTICK_15                 : constant := 16#000E#;
   GLFW_JOYSTICK_16                 : constant := 16#000F#;
   GLFW_JOYSTICK_LAST               : constant := 16#000F#;
   GLFW_WINDOW                      : constant := 16#0001_0001#;
   GLFW_FULLSCREEN                  : constant := 16#0001_0002#;
   GLFW_OPENED                      : constant := 16#0002_0001#;
   GLFW_ACTIVE                      : constant := 16#0002_0002#;
   GLFW_ICONIFIED                   : constant := 16#0002_0003#;
   GLFW_ACCELERATED                 : constant := 16#0002_0004#;
   GLFW_RED_BITS                    : constant := 16#0002_0005#;
   GLFW_GREEN_BITS                  : constant := 16#0002_0006#;
   GLFW_BLUE_BITS                   : constant := 16#0002_0007#;
   GLFW_ALPHA_BITS                  : constant := 16#0002_0008#;
   GLFW_DEPTH_BITS                  : constant := 16#0002_0009#;
   GLFW_STENCIL_BITS                : constant := 16#0002_000A#;
   GLFW_REFRESH_RATE                : constant := 16#0002_000B#;
   GLFW_ACCUM_RED_BITS              : constant := 16#0002_000C#;
   GLFW_ACCUM_GREEN_BITS            : constant := 16#0002_000D#;
   GLFW_ACCUM_BLUE_BITS             : constant := 16#0002_000E#;
   GLFW_ACCUM_ALPHA_BITS            : constant := 16#0002_000F#;
   GLFW_AUX_BUFFERS                 : constant := 16#0002_0010#;
   GLFW_STEREO                      : constant := 16#0002_0011#;
   GLFW_MOUSE_CURSOR                : constant := 16#0003_0001#;
   GLFW_STICKY_KEYS                 : constant := 16#0003_0002#;
   GLFW_STICKY_MOUSE_BUTTONS        : constant := 16#0003_0003#;
   GLFW_SYSTEM_KEYS                 : constant := 16#0003_0004#;
   GLFW_KEY_REPEAT                  : constant := 16#0003_0005#;
   GLFW_AUTO_POLL_EVENTS            : constant := 16#0003_0006#;
   GLFW_WAIT                        : constant := 16#0004_0001#;
   GLFW_NOWAIT                      : constant := 16#0004_0002#;
   GLFW_PRESENT                     : constant := 16#0005_0001#;
   GLFW_AXES                        : constant := 16#0005_0002#;
   GLFW_BUTTONS                     : constant := 16#0005_0003#;
   GLFW_NO_RESCALE_BIT              : constant := 16#0001#;
   GLFW_ORIGIN_UL_BIT               : constant := 16#0002#;
   GLFW_BUILD_MIPMAPS_BIT           : constant := 16#0004#;
   GLFW_INFINITY                    : constant := 1.00000000000000000000e+05;

   subtype GLFWMUTEX is Interfaces.C.Extensions.void_ptr;
   subtype GLFWCOND is Interfaces.C.Extensions.void_ptr;
   subtype GLFWTHREAD is Interfaces.C.int;

   type char_ptr is access all Interfaces.C.char;

   type GLFW_Vid_Mode is
      record
         Width     : Interfaces.C.int;
         Height    : Interfaces.C.int;
         RedBits   : Interfaces.C.int;
         BlueBits  : Interfaces.C.int;
         GreenBits : Interfaces.C.int;
      end record;
   pragma Convention (C, GLFW_Vid_Mode);

   type GLFW_Image is
      record
          Width         : Interfaces.C.int;
          Height        : Interfaces.C.int;
          Format        : Interfaces.C.int;
          BytesPerPixel : Interfaces.C.int;
          Data          : char_ptr;
      end record;
   pragma Convention (C, GLFW_Image);

   type GLFW_Vid_Mode_Type is access all GLFW_Vid_Mode;
   type GLFW_Image_Type is access all GLFW_Image;


   type GLFWWINDOWSIZEFUN is access procedure (p1 : Interfaces.C.int;
                                               p2 : Interfaces.C.int);
   type GLFWMOUSEBUTTONFUN is access procedure (p1 : Interfaces.C.int;
                                                p2 : Interfaces.C.int);
   type GLFWMOUSEPOSFUN is access procedure (p1 : Interfaces.C.int;
                                             p2 : Interfaces.C.int);
   type GLFWMOUSEWHEELFUN is access procedure (p1 : Interfaces.C.int);
   type GLFWKEYFUN is access procedure (p1 : Interfaces.C.int;
                                        p2 : Interfaces.C.int);
   type GLFWCHARFUN is access procedure (p1 : Interfaces.C.int;
                                         p2 : Interfaces.C.int);
   type GLFWTHREADFUN is access procedure (p1 : System.Address);

   function glfwInit return Interfaces.C.int;

   procedure glfwTerminate;

   procedure glfwGetVersion (Major : access Interfaces.C.int;
                             Minor : access Interfaces.C.int;
                             Rev   : access Interfaces.C.int);

   function glfwOpenWindow (Width       : Interfaces.C.int;
                            Height      : Interfaces.C.int;
                            Redbits     : Interfaces.C.int;
                            Greenbits   : Interfaces.C.int;
                            Bluebits    : Interfaces.C.int;
                            Alphabits   : Interfaces.C.int;
                            Depthbits   : Interfaces.C.int;
                            Stencilbits : Interfaces.C.int;
                            Mode        : Interfaces.C.int)
                                        return Interfaces.C.int;

   procedure glfwOpenWindowHint (Target : Interfaces.C.int;
                                 Hint   : Interfaces.C.int);

   procedure glfwCloseWindow;

   procedure glfwSetWindowTitle (Title : Interfaces.C.Strings.chars_ptr);

   procedure glfwGetWindowSize (Width  : access Interfaces.C.int;
                                Height : access Interfaces.C.int);

   procedure glfwSetWindowSize (Width  : Interfaces.C.int;
                                Height : Interfaces.C.int);

   procedure glfwSetWindowPos (X : Interfaces.C.int;
                               Y : Interfaces.C.int);

   procedure glfwIconifyWindow;

   procedure glfwRestoreWindow;

   procedure glfwSwapBuffers;

   procedure glfwSwapInterval (Interval : Interfaces.C.int);

   function glfwGetWindowParam (Param : Interfaces.C.int)
      return Interfaces.C.int;

   procedure glfwSetWindowSizeCallback (Cbfun : GLFWWINDOWSIZEFUN);

   function glfwGetVideoModes (List     : GLFW_Vid_Mode_Type;
                               Maxcount : Interfaces.C.int)
                                        return Interfaces.C.int;

   procedure glfwGetDesktopMode (Mode : GLFW_Vid_Mode_Type);

   procedure glfwPollEvents;

   function glfwGetKey (Key : Interfaces.C.int)
      return Interfaces.C.int;

   function glfwGetMouseButton (Button : Interfaces.C.int)
      return Interfaces.C.int;

   procedure glfwGetMousePos (Xpos : access Interfaces.C.int;
                              Ypos : access Interfaces.C.int);

   procedure glfwSetMousePos (Xpos : Interfaces.C.int;
                              Ypos : Interfaces.C.int);

   function glfwGetMouseWheel return Interfaces.C.int;

   procedure glfwSetMouseWheel (Pos : Interfaces.C.int);

   procedure glfwSetKeyCallback (Cbfun : GLFWKEYFUN);

   procedure glfwSetCharCallback (Cbfun : GLFWCHARFUN);

   procedure glfwSetMouseButtonCallback (Cbfun : GLFWMOUSEBUTTONFUN);

   procedure glfwSetMousePosCallback (Cbfun : GLFWMOUSEPOSFUN);

   procedure glfwSetMouseWheelCallback (Cbfun : GLFWMOUSEWHEELFUN);

   function glfwGetJoystickParam (Joy   : Interfaces.C.int;
                                  Param : Interfaces.C.int)
                                        return Interfaces.C.int;

   function glfwGetJoystickPos (Joy     : Interfaces.C.int;
                                Pos     : access Interfaces.C.C_float;
                                Numaxes : Interfaces.C.int)
                                        return Interfaces.C.int;

   function glfwGetJoystickButtons (Joy        : Interfaces.C.int;
                                    Buttons    : char_ptr;
                                    Numbuttons : Interfaces.C.int)
                                               return Interfaces.C.int;

   function glfwGetTime return Interfaces.C.double;

   procedure glfwSetTime (Time : Interfaces.C.double);

   procedure glfwSleep (Time : Interfaces.C.double);

   function glfwExtensionSupported (Extension : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;

   function glfwGetProcAddress (Procname : Interfaces.C.Strings.chars_ptr)
      return System.Address;

   procedure glfwGetGLVersion (Major : access Interfaces.C.int;
                               Minor : access Interfaces.C.int;
                               Rev   : access Interfaces.C.int);

   function glfwCreateThread (Fun : GLFWTHREADFUN;
                              Arg : System.Address)
                                  return GLFWTHREAD;

   procedure glfwDestroyThread (ID : GLFWTHREAD);

   function glfwWaitThread (ID       : GLFWTHREAD;
                            Waitmode : Interfaces.C.int)
                                     return Interfaces.C.int;

   function glfwGetThreadID return GLFWTHREAD;

   function glfwCreateMutex return GLFWMUTEX;

   procedure glfwDestroyMutex (Mutex : GLFWMUTEX);

   procedure glfwLockMutex (Mutex : GLFWMUTEX);

   procedure glfwUnlockMutex (Mutex : GLFWMUTEX);

   function glfwCreateCond return GLFWCOND;

   procedure glfwDestroyCond (Cond : GLFWCOND);

   procedure glfwWaitCond (Cond    : GLFWCOND;
                           Mutex   : GLFWMUTEX;
                           Timeout : Interfaces.C.double);

   procedure glfwSignalCond (Cond : GLFWCOND);

   procedure glfwBroadcastCond (Cond : GLFWCOND);

   function glfwGetNumberOfProcessors return Interfaces.C.int;

   procedure glfwEnable (Token : Interfaces.C.int);

   procedure glfwDisable (Token : Interfaces.C.int);

   function glfwReadImage (Name  : Interfaces.C.Strings.chars_ptr;
                           Img   : GLFW_Image_Type;
                           Flags : Interfaces.C.int)
                                 return Interfaces.C.int;

   procedure glfwFreeImage (Img : GLFW_Image_Type);

   function glfwLoadTexture2D (Name  : Interfaces.C.Strings.chars_ptr;
                               Flags : Interfaces.C.int)
                                     return Interfaces.C.int;

private

   pragma Import (C, glfwInit, "glfwInit");

   pragma Import (C, glfwTerminate, "glfwTerminate");

   pragma Import (C, glfwGetVersion, "glfwGetVersion");

   pragma Import (C, glfwOpenWindow, "glfwOpenWindow");

   pragma Import (C, glfwOpenWindowHint, "glfwOpenWindowHint");

   pragma Import (C, glfwCloseWindow, "glfwCloseWindow");

   pragma Import (C, glfwSetWindowTitle, "glfwSetWindowTitle");

   pragma Import (C, glfwGetWindowSize, "glfwGetWindowSize");

   pragma Import (C, glfwSetWindowSize, "glfwSetWindowSize");

   pragma Import (C, glfwSetWindowPos, "glfwSetWindowPos");

   pragma Import (C, glfwIconifyWindow, "glfwIconifyWindow");

   pragma Import (C, glfwRestoreWindow, "glfwRestoreWindow");

   pragma Import (C, glfwSwapBuffers, "glfwSwapBuffers");

   pragma Import (C, glfwSwapInterval, "glfwSwapInterval");

   pragma Import (C, glfwGetWindowParam, "glfwGetWindowParam");

   pragma Import (C, glfwSetWindowSizeCallback, "glfwSetWindowSizeCallback");

   pragma Import (C, glfwGetVideoModes, "glfwGetVideoModes");

   pragma Import (C, glfwGetDesktopMode, "glfwGetDesktopMode");

   pragma Import (C, glfwPollEvents, "glfwPollEvents");

   pragma Import (C, glfwGetKey, "glfwGetKey");

   pragma Import (C, glfwGetMouseButton, "glfwGetMouseButton");

   pragma Import (C, glfwGetMousePos, "glfwGetMousePos");

   pragma Import (C, glfwSetMousePos, "glfwSetMousePos");

   pragma Import (C, glfwGetMouseWheel, "glfwGetMouseWheel");

   pragma Import (C, glfwSetMouseWheel, "glfwSetMouseWheel");

   pragma Import (C, glfwSetKeyCallback, "glfwSetKeyCallback");

   pragma Import (C, glfwSetCharCallback, "glfwSetCharCallback");

   pragma Import
      (C, glfwSetMouseButtonCallback, "glfwSetMouseButtonCallback");

   pragma Import (C, glfwSetMousePosCallback, "glfwSetMousePosCallback");

   pragma Import (C, glfwSetMouseWheelCallback, "glfwSetMouseWheelCallback");

   pragma Import (C, glfwGetJoystickParam, "glfwGetJoystickParam");

   pragma Import (C, glfwGetJoystickPos, "glfwGetJoystickPos");

   pragma Import (C, glfwGetJoystickButtons, "glfwGetJoystickButtons");

   pragma Import (C, glfwGetTime, "glfwGetTime");

   pragma Import (C, glfwSetTime, "glfwSetTime");

   pragma Import (C, glfwSleep, "glfwSleep");

   pragma Import (C, glfwExtensionSupported, "glfwExtensionSupported");

   pragma Import (C, glfwGetProcAddress, "glfwGetProcAddress");

   pragma Import (C, glfwGetGLVersion, "glfwGetGLVersion");

   pragma Import (C, glfwCreateThread, "glfwCreateThread");

   pragma Import (C, glfwDestroyThread, "glfwDestroyThread");

   pragma Import (C, glfwWaitThread, "glfwWaitThread");

   pragma Import (C, glfwGetThreadID, "glfwGetThreadID");

   pragma Import (C, glfwCreateMutex, "glfwCreateMutex");

   pragma Import (C, glfwDestroyMutex, "glfwDestroyMutex");

   pragma Import (C, glfwLockMutex, "glfwLockMutex");

   pragma Import (C, glfwUnlockMutex, "glfwUnlockMutex");

   pragma Import (C, glfwCreateCond, "glfwCreateCond");

   pragma Import (C, glfwDestroyCond, "glfwDestroyCond");

   pragma Import (C, glfwWaitCond, "glfwWaitCond");

   pragma Import (C, glfwSignalCond, "glfwSignalCond");

   pragma Import (C, glfwBroadcastCond, "glfwBroadcastCond");

   pragma Import (C, glfwGetNumberOfProcessors, "glfwGetNumberOfProcessors");

   pragma Import (C, glfwEnable, "glfwEnable");

   pragma Import (C, glfwDisable, "glfwDisable");

   pragma Import (C, glfwReadImage, "glfwReadImage");

   pragma Import (C, glfwFreeImage, "glfwFreeImage");

   pragma Import (C, glfwLoadTexture2D, "glfwLoadTexture2D");

end OpenGL.GLFW;

