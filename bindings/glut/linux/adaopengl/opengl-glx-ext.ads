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


with OpenGL.GLX;
with X_Lib;

package OpenGL.GLX.EXT is

   GLX_GLXEXT_VERSION                            : constant := 5;
   GLX_SAMPLE_BUFFERS_ARB                        : constant := 100000;
   GLX_SAMPLES_ARB                               : constant := 100001;
   GLX_SAMPLE_BUFFERS_SGIS                       : constant := 100000;
   GLX_SAMPLES_SGIS                              : constant := 100001;
   GLX_X_VISUAL_TYPE_EXT                         : constant := 16#0022#;
   GLX_TRANSPARENT_TYPE_EXT                      : constant := 16#0023#;
   GLX_TRANSPARENT_INDEX_VALUE_EXT               : constant := 16#0024#;
   GLX_TRANSPARENT_RED_VALUE_EXT                 : constant := 16#0025#;
   GLX_TRANSPARENT_GREEN_VALUE_EXT               : constant := 16#0026#;
   GLX_TRANSPARENT_BLUE_VALUE_EXT                : constant := 16#0027#;
   GLX_TRANSPARENT_ALPHA_VALUE_EXT               : constant := 16#0028#;
   GLX_NONE_EXT                                  : constant := 16#8000#;
   GLX_TRUE_COLOR_EXT                            : constant := 16#0000_8002#;
   GLX_DIRECT_COLOR_EXT                          : constant := 16#0000_8003#;
   GLX_PSEUDO_COLOR_EXT                          : constant := 16#0000_8004#;
   GLX_STATIC_COLOR_EXT                          : constant := 16#0000_8005#;
   GLX_GRAY_SCALE_EXT                            : constant := 16#0000_8006#;
   GLX_STATIC_GRAY_EXT                           : constant := 16#0000_8007#;
   GLX_TRANSPARENT_RGB_EXT                       : constant := 16#0000_8008#;
   GLX_TRANSPARENT_INDEX_EXT                     : constant := 16#0000_8009#;
   GLX_VISUAL_CAVEAT_EXT                         : constant := 16#0020#;
   GLX_SLOW_VISUAL_EXT                           : constant := 16#0000_8001#;
   GLX_NON_CONFORMANT_VISUAL_EXT                 : constant := 16#0000_800D#;
   GLX_SHARE_CONTEXT_EXT                         : constant := 16#0000_800A#;
   GLX_VISUAL_ID_EXT                             : constant := 16#0000_800B#;
   GLX_SCREEN_EXT                                : constant := 16#0000_800C#;
   GLX_WINDOW_BIT_SGIX                           : constant := 16#0001#;
   GLX_PIXMAP_BIT_SGIX                           : constant := 16#0002#;
   GLX_RGBA_BIT_SGIX                             : constant := 16#0001#;
   GLX_COLOR_INDEX_BIT_SGIX                      : constant := 16#0002#;
   GLX_DRAWABLE_TYPE_SGIX                        : constant := 16#0000_8010#;
   GLX_RENDER_TYPE_SGIX                          : constant := 16#0000_8011#;
   GLX_X_RENDERABLE_SGIX                         : constant := 16#0000_8012#;
   GLX_FBCONFIG_ID_SGIX                          : constant := 16#0000_8013#;
   GLX_RGBA_TYPE_SGIX                            : constant := 16#0000_8014#;
   GLX_COLOR_INDEX_TYPE_SGIX                     : constant := 16#0000_8015#;
   GLX_PBUFFER_BIT_SGIX                          : constant := 16#0004#;
   GLX_BUFFER_CLOBBER_MASK_SGIX                  : constant := 16#0800_0000#;
   GLX_FRONT_LEFT_BUFFER_BIT_SGIX                : constant := 16#0001#;
   GLX_FRONT_RIGHT_BUFFER_BIT_SGIX               : constant := 16#0002#;
   GLX_BACK_LEFT_BUFFER_BIT_SGIX                 : constant := 16#0004#;
   GLX_BACK_RIGHT_BUFFER_BIT_SGIX                : constant := 16#0008#;
   GLX_AUX_BUFFERS_BIT_SGIX                      : constant := 16#0010#;
   GLX_DEPTH_BUFFER_BIT_SGIX                     : constant := 16#0020#;
   GLX_STENCIL_BUFFER_BIT_SGIX                   : constant := 16#0040#;
   GLX_ACCUM_BUFFER_BIT_SGIX                     : constant := 16#0080#;
   GLX_SAMPLE_BUFFERS_BIT_SGIX                   : constant := 16#0100#;
   GLX_MAX_PBUFFER_WIDTH_SGIX                    : constant := 16#0000_8016#;
   GLX_MAX_PBUFFER_HEIGHT_SGIX                   : constant := 16#0000_8017#;
   GLX_MAX_PBUFFER_PIXELS_SGIX                   : constant := 16#0000_8018#;
   GLX_OPTIMAL_PBUFFER_WIDTH_SGIX                : constant := 16#0000_8019#;
   GLX_OPTIMAL_PBUFFER_HEIGHT_SGIX               : constant := 16#0000_801A#;
   GLX_PRESERVED_CONTENTS_SGIX                   : constant := 16#0000_801B#;
   GLX_LARGEST_PBUFFER_SGIX                      : constant := 16#0000_801C#;
   GLX_WIDTH_SGIX                                : constant := 16#0000_801D#;
   GLX_HEIGHT_SGIX                               : constant := 16#0000_801E#;
   GLX_EVENT_MASK_SGIX                           : constant := 16#0000_801F#;
   GLX_DAMAGED_SGIX                              : constant := 16#0000_8020#;
   GLX_SAVED_SGIX                                : constant := 16#0000_8021#;
   GLX_WINDOW_SGIX                               : constant := 16#0000_8022#;
   GLX_PBUFFER_SGIX                              : constant := 16#0000_8023#;
   GLX_SYNC_FRAME_SGIX                           : constant := 16#0000#;
   GLX_SYNC_SWAP_SGIX                            : constant := 16#0001#;
   GLX_DIGITAL_MEDIA_PBUFFER_SGIX                : constant := 16#0000_8024#;
   GLX_BLENDED_RGBA_SGIS                         : constant := 16#0000_8025#;
   GLX_MULTISAMPLE_SUB_RECT_WIDTH_SGIS           : constant := 16#0000_8026#;
   GLX_MULTISAMPLE_SUB_RECT_HEIGHT_SGIS          : constant := 16#0000_8027#;
   GLX_SAMPLE_BUFFERS_3DFX                       : constant := 16#0000_8050#;
   GLX_SAMPLES_3DFX                              : constant := 16#0000_8051#;
   GLX_3DFX_WINDOW_MODE_MESA                     : constant := 16#0001#;
   GLX_3DFX_FULLSCREEN_MODE_MESA                 : constant := 16#0002#;
   GLX_VISUAL_SELECT_GROUP_SGIX                  : constant := 16#0000_8028#;
   GLX_SWAP_METHOD_OML                           : constant := 16#0000_8060#;
   GLX_SWAP_EXCHANGE_OML                         : constant := 16#0000_8061#;
   GLX_SWAP_COPY_OML                             : constant := 16#0000_8062#;
   GLX_SWAP_UNDEFINED_OML                        : constant := 16#0000_8063#;

   type GLXVIDEOSOURCESGIX is new X_Lib.XID;
   type GLXFBCONFIGIDSGIX is new X_Lib.XID;
   type GLXPBUFFERSGIX is new X_Lib.XID;


   type GLXFBCONFIGSGIX is access all OpenGL.GLX.struct_GLXFBConfigRec;

   type GLXEXTFUNCPTR is access procedure;

end OpenGL.GLX.EXT;

