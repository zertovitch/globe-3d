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
with Interfaces.C.Extensions;
with interfaces.C.Pointers;

with System;
with OpenGL;

with X_Lib;



package OpenGL.GLX is

   GLX_VERSION_1_1      : constant := 1;
   GLX_VERSION_1_2      : constant := 1;
   GLX_VERSION_1_3      : constant := 1;
   GLX_VERSION_1_4      : constant := 1;
   GLX_EXTENSION_NAME   : constant String := "GLX";
   GLX_USE_GL           : constant := 1;
   GLX_BUFFER_SIZE      : constant := 2;
   GLX_LEVEL            : constant := 3;
   GLX_RGBA             : constant := 4;
   GLX_DOUBLEBUFFER     : constant := 5;
   GLX_STEREO           : constant := 6;
   GLX_AUX_BUFFERS      : constant := 7;
   GLX_RED_SIZE         : constant := 8;
   GLX_GREEN_SIZE       : constant := 9;
   GLX_BLUE_SIZE        : constant := 10;
   GLX_ALPHA_SIZE       : constant := 11;
   GLX_DEPTH_SIZE       : constant := 12;
   GLX_STENCIL_SIZE     : constant := 13;
   GLX_ACCUM_RED_SIZE   : constant := 14;
   GLX_ACCUM_GREEN_SIZE : constant := 15;
   GLX_ACCUM_BLUE_SIZE  : constant := 16#0010#;
   GLX_ACCUM_ALPHA_SIZE : constant := 17;
   GLX_BAD_SCREEN       : constant := 1;
   GLX_BAD_ATTRIBUTE    : constant := 2;
   GLX_NO_EXTENSION     : constant := 3;
   GLX_BAD_VISUAL       : constant := 4;
   GLX_BAD_CONTEXT      : constant := 5;
   GLX_BAD_VALUE        : constant := 6;
   GLX_BAD_ENUM         : constant := 7;
   GLX_VENDOR           : constant := 1;
   GLX_VERSION          : constant := 2;
   GLX_EXTENSIONS       : constant := 3;
   GLX_CONFIG_CAVEAT    : constant := 16#0020#;
   GLX_DONT_CARE        : constant := 16#FFFF_FFFF#;
   GLX_SLOW_CONFIG      : constant := 16#0000_8001#;
   GLX_NON_CONFORMANT_CONFIG : constant := 16#0000_800D#;
   GLX_X_VISUAL_TYPE         : constant := 16#0022#;
   GLX_TRANSPARENT_TYPE      : constant := 16#0023#;
   GLX_TRANSPARENT_INDEX_VALUE : constant := 16#0024#;
   GLX_TRANSPARENT_RED_VALUE   : constant := 16#0025#;
   GLX_TRANSPARENT_GREEN_VALUE : constant := 16#0026#;
   GLX_TRANSPARENT_BLUE_VALUE  : constant := 16#0027#;
   GLX_TRANSPARENT_ALPHA_VALUE : constant := 16#0028#;
   GLX_MAX_PBUFFER_WIDTH       : constant := 16#0000_8016#;
   GLX_MAX_PBUFFER_HEIGHT      : constant := 16#0000_8017#;
   GLX_MAX_PBUFFER_PIXELS      : constant := 16#0000_8018#;
   GLX_PRESERVED_CONTENTS      : constant := 16#0000_801B#;
   GLX_LARGEST_PBUFFER         : constant := 16#0000_801C#;
   GLX_WIDTH                   : constant := 16#0000_801D#;
   GLX_HEIGHT                  : constant := 16#0000_801E#;
   GLX_EVENT_MASK              : constant := 16#0000_801F#;
   GLX_DRAWABLE_TYPE           : constant := 16#0000_8010#;
   GLX_FBCONFIG_ID             : constant := 16#0000_8013#;
   GLX_VISUAL_ID               : constant := 16#0000_800B#;
   GLX_WINDOW_BIT              : constant := 16#0001#;
   GLX_PIXMAP_BIT              : constant := 16#0002#;
   GLX_PBUFFER_BIT             : constant := 16#0004#;
   GLX_AUX_BUFFERS_BIT         : constant := 16#0010#;
   GLX_FRONT_LEFT_BUFFER_BIT   : constant := 16#0001#;
   GLX_FRONT_RIGHT_BUFFER_BIT  : constant := 16#0002#;
   GLX_BACK_LEFT_BUFFER_BIT    : constant := 16#0004#;
   GLX_BACK_RIGHT_BUFFER_BIT   : constant := 16#0008#;
   GLX_DEPTH_BUFFER_BIT        : constant := 16#0020#;
   GLX_STENCIL_BUFFER_BIT      : constant := 16#0040#;
   GLX_ACCUM_BUFFER_BIT        : constant := 16#0080#;
   GLX_RENDER_TYPE             : constant := 16#0000_8011#;
   GLX_X_RENDERABLE            : constant := 16#0000_8012#;
   GLX_NONE                    : constant := 16#8000#;
   GLX_TRUE_COLOR              : constant := 16#0000_8002#;
   GLX_DIRECT_COLOR            : constant := 16#0000_8003#;
   GLX_PSEUDO_COLOR            : constant := 16#0000_8004#;
   GLX_STATIC_COLOR            : constant := 16#0000_8005#;
   GLX_GRAY_SCALE              : constant := 16#0000_8006#;
   GLX_STATIC_GRAY             : constant := 16#0000_8007#;
   GLX_TRANSPARENT_RGB         : constant := 16#0000_8008#;
   GLX_TRANSPARENT_INDEX       : constant := 16#0000_8009#;
   GLX_RGBA_TYPE               : constant := 16#0000_8014#;
   GLX_COLOR_INDEX_TYPE        : constant := 16#0000_8015#;
   GLX_COLOR_INDEX_BIT         : constant := 16#0002#;
   GLX_RGBA_BIT                : constant := 16#0001#;
   GLX_SCREEN                  : constant := 16#0000_800C#;
   GLX_PBUFFER_CLOBBER_MASK    : constant := 16#0800_0000#;
   GLX_DAMAGED                 : constant := 16#0000_8020#;
   GLX_SAVED                   : constant := 16#0000_8021#;
   GLX_WINDOW                  : constant := 16#0000_8022#;
   GLX_PBUFFER                 : constant := 16#0000_8023#;
   GLX_PBUFFER_HEIGHT          : constant := 16#0000_8040#;
   GLX_PBUFFER_WIDTH           : constant := 16#0000_8041#;
   GLX_SAMPLE_BUFFERS          : constant := 16#0001_86A0#;
   GLX_SAMPLES                 : constant := 16#0001_86A1#;
   GLX_MESA_AGP_OFFSET         : constant := 1;

   type GLXPIXMAP is new X_Lib.XID;
   type GLXDRAWABLE is new X_Lib.XID;
   type GLXFBCONFIGID is new X_Lib.XID;
   type GLXCONTEXTID is new X_Lib.XID;
   type GLXWINDOW is new X_Lib.XID;
   type GLXPBUFFER is new X_Lib.XID;

   type struct_GLXcontextRec;
   type struct_GLXFBConfigRec;

   type GLXCONTEXT is access all struct_GLXcontextRec;
   type GLXFBCONFIG is access all struct_GLXFBConfigRec;
   type A_GLXFBCONFIG_T is access all GLXFBCONFIG;


   type GLXFBConfig_Array is array (Positive range <>) of aliased GLXFBConfig;

   package GLXFBConfig_Pointers is new interfaces.C.Pointers (Index              => Positive,
                                                              Element            => GLXFBConfig,
                                                              Element_Array      => GLXFBConfig_Array,
                                                              Default_Terminator => null);



   type X_Visual_Info_Pointer is access all X_Lib.X_Visual_Info;

   type struct_GLXcontextRec is
      record
          null;
      end record;

   type struct_GLXFBConfigRec is
      record
          null;
      end record;

   function glXChooseVisual (dpy       : X_Lib.Display_Pointer;
                            screen     : X_Lib.Screen_Number;
                            attribList : OpenGL.GLintPtr)
                                        return X_Visual_Info_Pointer;

   function glXCreateContext (dpy      : X_Lib.Display_Pointer;
                             vis       : X_Visual_Info_Pointer;
                             shareList : GLXCONTEXT;
                             direct    : Integer)
                                        return GLXCONTEXT;

   procedure glXDestroyContext (dpy : X_Lib.Display_Pointer;
                               ctx  : GLXCONTEXT);

   function glXMakeCurrent (dpy     : X_Lib.Display_Pointer;
                           drawable : GLXDRAWABLE;
                           ctx      : GLXCONTEXT)
                                     return Integer;

   procedure glXCopyContext (dpy : X_Lib.Display_Pointer;
                            src  : GLXCONTEXT;
                            dst  : GLXCONTEXT;
                            mask : Interfaces.C.unsigned);

   procedure glXSwapBuffers (dpy     : X_Lib.Display_Pointer;
                            drawable : GLXDRAWABLE);

   function glXCreateGLXPixmap (dpy   : X_Lib.Display_Pointer;
                               visual : X_Visual_Info_Pointer;
                               pixmap : X_Lib.Pixmap_ID)
                                       return GLXPIXMAP;

   procedure glXDestroyGLXPixmap (dpy   : X_Lib.Display_Pointer;
                                 pixmap : GLXPIXMAP);

   function glXQueryExtension (dpy   : X_Lib.Display_Pointer;
                              errorb : OpenGL.GLintPtr;
                              event  : OpenGL.GLintPtr)
                                      return Boolean;

   function glXQueryVersion (dpy : X_Lib.Display_Pointer;
                            maj  : OpenGL.GLintPtr;
                            min  : OpenGL.GLintPtr)
                                 return Boolean;

   function glXIsDirect (dpy : X_Lib.Display_Pointer;
                        ctx  : GLXCONTEXT)
                             return Integer;

   function glXGetConfig (dpy   : X_Lib.Display_Pointer;
                         visual : X_Visual_Info_Pointer;
                         attrib : Integer;
                         value  : OpenGL.GLintPtr)
                                 return Integer;

   function glXGetCurrentContext return GLXCONTEXT;

   function glXGetCurrentDrawable return GLXDRAWABLE;

   procedure glXWaitGL;

   procedure glXWaitX;

   procedure glXUseXFont (font : X_Lib.Font_ID;
                         first : Integer;
                         count : Integer;
                         list  : Integer);

   function glXQueryExtensionsString (dpy   : X_Lib.Display_Pointer;
                                     screen : Integer)
                                             return OpenGL.GLubytePtr;

   function glXQueryServerString (dpy   : X_Lib.Display_Pointer;
                                 screen : Integer;
                                 name   : Integer)
                                         return OpenGL.GLubytePtr;

   function glXGetClientString (dpy : X_Lib.Display_Pointer;
                               name : Integer)
                                     return OpenGL.GLubytePtr;

   function glXGetCurrentDisplay return X_Lib.Display_Pointer;

   function glXChooseFBConfig (dpy       : X_Lib.Display_Pointer;
                              screen     : Integer;
                              attribList : OpenGL.GLintPtr;
                              nitems     : OpenGL.GLintPtr)
                                          return A_GLXFBCONFIG_T;

   function glXGetFBConfigAttrib (dpy      : X_Lib.Display_Pointer;
                                 config    : GLXFBCONFIG;
                                 attribute : Integer;
                                 value     : OpenGL.GLintPtr)
                                            return Integer;

   function glXGetFBConfigs (dpy      : X_Lib.Display_Pointer;
                            screen    : Integer;
                            nelements : OpenGL.GLintPtr)
                                       return A_GLXFBCONFIG_T;

   function glXGetVisualFromFBConfig (dpy   : X_Lib.Display_Pointer;
                                     config : GLXFBCONFIG)
                                           return X_Visual_Info_Pointer;

   function glXCreateWindow (dpy       : X_Lib.Display_Pointer;
                            config     : GLXFBCONFIG;
                            win        : X_Lib.Window_ID;
                            attribList : OpenGL.GLintPtr)
                                        return GLXWINDOW;

   procedure glXDestroyWindow (dpy   : X_Lib.Display_Pointer;
                              window : GLXWINDOW);

   function glXCreatePixmap (dpy       : X_Lib.Display_Pointer;
                            config     : GLXFBCONFIG;
                            pixmap     : X_Lib.Pixmap_ID;
                            attribList : OpenGL.GLintPtr)
                                        return GLXPIXMAP;

   procedure glXDestroyPixmap (dpy   : X_Lib.Display_Pointer;
                              pixmap : GLXPIXMAP);

   function glXCreatePbuffer (dpy       : X_Lib.Display_Pointer;
                             config     : GLXFBCONFIG;
                             attribList : OpenGL.GLintPtr)
                                         return GLXPBUFFER;

   procedure glXDestroyPbuffer (dpy : X_Lib.Display_Pointer;
                               pbuf : GLXPBUFFER);

   procedure glXQueryDrawable (dpy      : X_Lib.Display_Pointer;
                              draw      : GLXDRAWABLE;
                              attribute : Integer;
                              value     : OpenGL.GLuintPtr);

   function glXCreateNewContext (dpy       : X_Lib.Display_Pointer;
                                config     : GLXFBCONFIG;
                                renderType : Integer;
                                shareList  : GLXCONTEXT;
                                direct     : Integer)
                                            return GLXCONTEXT;

   function glXMakeContextCurrent (dpy : X_Lib.Display_Pointer;
                                  draw : GLXDRAWABLE;
                                  read : GLXDRAWABLE;
                                  ctx  : GLXCONTEXT)
                                        return Integer;

   function glXGetCurrentReadDrawable return GLXDRAWABLE;

   function glXQueryContext (dpy      : X_Lib.Display_Pointer;
                            ctx       : GLXCONTEXT;
                            attribute : Integer;
                            value     : OpenGL.GLintPtr)
                                       return Integer;

   procedure glXSelectEvent (dpy     : X_Lib.Display_Pointer;
                            drawable : GLXDRAWABLE;
                            mask     : Interfaces.C.unsigned);

   procedure glXGetSelectedEvent (dpy     : X_Lib.Display_Pointer;
                                 drawable : GLXDRAWABLE;
                                 mask     : OpenGL.GLuintPtr);

   function glXBindTexImageARB (dpy    : X_Lib.Display_Pointer;
                               pbuffer : GLXPBUFFER;
                               buffer  : Integer) return Boolean;

   function glXReleaseTexImageARB (dpy    : X_Lib.Display_Pointer;
                                  pbuffer : GLXPBUFFER;
                                  buffer  : Integer) return Boolean;

   function glXDrawableAttribARB (dpy : X_Lib.Display_Pointer;
                                 draw : GLXDRAWABLE;
                                 attribList : OpenGL.GLintPtr) return Boolean;

--  This one is tricky ;
    function glXGetProcAddress (procname: OpenGL.GLubytePtr)
       return system.Address;
--       return Interfaces.C.function_pointer;

   function glXAllocateMemoryNV (size     : OpenGL.GLsizei;
                                readfreq  : OpenGL.GLfloat;
                                writefreq : OpenGL.GLfloat;
                                priority  : OpenGL.GLfloat)
                                           return System.Address;

   procedure glXFreeMemoryNV
      (pointer : access Interfaces.C.Extensions.void_ptr);

   function glXGetAGPOffsetMESA
      (pointer : access Interfaces.C.Extensions.void_ptr) return OpenGL.GLuint;

private

   pragma Import (C, glXChooseVisual, "glXChooseVisual");

   pragma Import (C, glXCreateContext, "glXCreateContext");

   pragma Import (C, glXDestroyContext, "glXDestroyContext");

   pragma Import (C, glXMakeCurrent, "glXMakeCurrent");

   pragma Import (C, glXCopyContext, "glXCopyContext");

   pragma Import (C, glXSwapBuffers, "glXSwapBuffers");

   pragma Import (C, glXCreateGLXPixmap, "glXCreateGLXPixmap");

   pragma Import (C, glXDestroyGLXPixmap, "glXDestroyGLXPixmap");

   pragma Import (C, glXQueryExtension, "glXQueryExtension");

   pragma Import (C, glXQueryVersion, "glXQueryVersion");

   pragma Import (C, glXIsDirect, "glXIsDirect");

   pragma Import (C, glXGetConfig, "glXGetConfig");

   pragma Import (C, glXGetCurrentContext, "glXGetCurrentContext");

   pragma Import (C, glXGetCurrentDrawable, "glXGetCurrentDrawable");

   pragma Import (C, glXWaitGL, "glXWaitGL");

   pragma Import (C, glXWaitX, "glXWaitX");

   pragma Import (C, glXUseXFont, "glXUseXFont");

   pragma Import (C, glXQueryExtensionsString, "glXQueryExtensionsString");

   pragma Import (C, glXQueryServerString, "glXQueryServerString");

   pragma Import (C, glXGetClientString, "glXGetClientString");

   pragma Import (C, glXGetCurrentDisplay, "glXGetCurrentDisplay");

   pragma Import (C, glXChooseFBConfig, "glXChooseFBConfig");

   pragma Import (C, glXGetFBConfigAttrib, "glXGetFBConfigAttrib");

   pragma Import (C, glXGetFBConfigs, "glXGetFBConfigs");

   pragma Import (C, glXGetVisualFromFBConfig, "glXGetVisualFromFBConfig");

   pragma Import (C, glXCreateWindow, "glXCreateWindow");

   pragma Import (C, glXDestroyWindow, "glXDestroyWindow");

   pragma Import (C, glXCreatePixmap, "glXCreatePixmap");

   pragma Import (C, glXDestroyPixmap, "glXDestroyPixmap");

   pragma Import (C, glXCreatePbuffer, "glXCreatePbuffer");

   pragma Import (C, glXDestroyPbuffer, "glXDestroyPbuffer");

   pragma Import (C, glXQueryDrawable, "glXQueryDrawable");

   pragma Import (C, glXCreateNewContext, "glXCreateNewContext");

   pragma Import (C, glXMakeContextCurrent, "glXMakeContextCurrent");

   pragma Import (C, glXGetCurrentReadDrawable, "glXGetCurrentReadDrawable");

   pragma Import (C, glXQueryContext, "glXQueryContext");

   pragma Import (C, glXSelectEvent, "glXSelectEvent");

   pragma Import (C, glXGetSelectedEvent, "glXGetSelectedEvent");

   pragma Import (C, glXBindTexImageARB, "glXBindTexImageARB");

   pragma Import (C, glXReleaseTexImageARB, "glXReleaseTexImageARB");

   pragma Import (C, glXDrawableAttribARB, "glXDrawableAttribARB");

   pragma Import (C, glXGetProcAddress, "glXGetProcAddress");

   pragma Import (C, glXAllocateMemoryNV, "glXAllocateMemoryNV");

   pragma Import (C, glXFreeMemoryNV, "glXFreeMemoryNV");

   pragma Import (C, glXGetAGPOffsetMESA, "glXGetAGPOffsetMESA");

end OpenGL.GLX;
