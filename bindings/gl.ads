--#-#-#-#-----------------
--  Change log:
--
--  GdM: 2011: using System.Address_To_Access_Conversions instead of Ada.Unchecked_Conversion
--  GdM: 2008: GL 1.5 items moved to GL.Extended ('cause of Windows :-( )
--  RK : 2007: added CLAMP_TO_EDGE and CLAMP_TO_BORDER for texturing.
--  RK : 2007: added positive_uInt
--  RK : 2007: renamed 'intPtr' to 'intPointer' and added correct
--               openGL intPtr type (which is a 'ptrdiff_t')
--  RK : 2007: added support for vertex buffer objects
--  GdM: 2007: added BGR, BGRA to PixelFormatEnm and TexFormatEnm (OpenGL 1.2)
--  RK : 2007: conversions to gl.Pointer
--  GdM: 2006: added MULTISAMPLE_ARB, GetString returning a String
--  GdM: End 2005: improved 2002's point 3)
--  GdM: 27-Jan-2004: Added Material_Float_vector and Material(...) for it
--  GdM:  4-Jan-2003 :
--    for overloading names, preference is given to GL.Double
--    (Gl.Float keeps 'f') and GL.Int (GL.Short keeps 's'), in order to avoid
--    confusing compilers i.r.o. arithmetics with universal types.
--  GdM: 11-Apr-2002 :
--    1) "gl" and "GL_" useless prefixes removed,
--        except when conflicting with Ada keywords
--    2) improving the independance from the "pointer" model
--    3) possibility of avoiding useless "4f"-style
--        suffixes through overloading
--#-#-#-#-----------------
--
--  Origin: OpenGL 1.1 Ada binding, package GL
--  W. M. Richards, NiEstu, Phoenix AZ, December 1997
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--  You should have received a copy of the GNU Library General Public
--  License along with this library; if not, write to the Free
--  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System.Address_To_Access_Conversions;

package GL is

  package C renames Interfaces.C;

  ------------------------------------------------------------------------------

  MESA_MAJOR_VERSION          : constant := 2;
  MESA_MINOR_VERSION          : constant := 5;
  VERSION_1_1                 : constant := 1;
  EXT_BLEND_COLOR             : constant := 1;
  EXT_BLEND_LOGIC_OP          : constant := 1;
  EXT_BLEND_MINMAX            : constant := 1;
  EXT_BLEND_SUBTRACT          : constant := 1;
  EXT_POLYGON_OFFSET          : constant := 1;
  EXT_VERTEX_ARRAY            : constant := 1;
  EXT_TEXTURE_OBJECT          : constant := 1;
  EXT_TEXTURE3D               : constant := 1;
  EXT_PALETTED_TEXTURE        : constant := 1;
  EXT_SHARED_TEXTURE_PALETTE  : constant := 1;
  EXT_POINT_PARAMETERS        : constant := 1;
  MESA_WINDOW_POS             : constant := 1;
  MESA_RESIZE_BUFFERS         : constant := 1;

  CURRENT_BIT                 : constant := 16#00000001#;
  POINT_BIT                   : constant := 16#00000002#;
  LINE_BIT                    : constant := 16#00000004#;
  POLYGON_BIT                 : constant := 16#00000008#;
  POLYGON_STIPPLE_BIT         : constant := 16#00000010#;
  PIXEL_MODE_BIT              : constant := 16#00000020#;
  LIGHTING_BIT                : constant := 16#00000040#;
  FOG_BIT                     : constant := 16#00000080#;
  DEPTH_BUFFER_BIT            : constant := 16#00000100#;
  ACCUM_BUFFER_BIT            : constant := 16#00000200#;
  STENCIL_BUFFER_BIT          : constant := 16#00000400#;
  VIEWPORT_BIT                : constant := 16#00000800#;
  TRANSFORM_BIT               : constant := 16#00001000#;
  ENABLE_BIT                  : constant := 16#00002000#;
  COLOR_BUFFER_BIT            : constant := 16#00004000#;
  HINT_BIT                    : constant := 16#00008000#;
  EVAL_BIT                    : constant := 16#00010000#;
  LIST_BIT                    : constant := 16#00020000#;
  TEXTURE_BIT                 : constant := 16#00040000#;
  SCISSOR_BIT                 : constant := 16#00080000#;
  ALL_ATTRIB_BITS             : constant := 16#000FFFFF#;
  CLIENT_PIXEL_STORE_BIT      : constant := 16#00000001#;
  CLIENT_VERTEX_ARRAY_BIT     : constant := 16#00000002#;
  CLIENT_ALL_ATTRIB_BITS      : constant := 16#0000FFFF#;

  ------------------------------------------------------------------------------

  --  Base types
  type Bitfield      is new C.unsigned;        -- 4-byte unsigned
  type GL_Boolean    is new C.unsigned_char;   -- 1-byte unsigned in [0,1]
  type Byte          is new C.char;            -- 1-byte signed
  type Short         is new C.short;           -- 2-byte signed
  type Int           is new C.int;             -- 4-byte signed
  type Ubyte         is new C.unsigned_char;   -- 1-byte unsigned
  type Ushort        is new C.unsigned_short;  -- 2-byte unsigned
  type Uint          is new C.unsigned;        -- 4-byte unsigned
  type Sizei         is new C.int;             -- 4-byte signed
  type Float         is new C.C_float;         -- single precision float
  type Clampf        is new C.C_float;         -- single precision float in [0,1]
  type Double        is new C.double;          -- double precision float
  type Clampd        is new C.double;          -- double precision float in [0,1]

  type positive_uInt is new GL.Uint range 1 .. GL.Uint'Last;

  package A2A_double is new System.Address_To_Access_Conversions (Double);

  --  Pointer types
  type GL_BooleanPtr is access all GL_Boolean;
  type bytePtr    is access all Byte;
  type shortPtr   is access all Short;
  type intPointer is access all Int;
  type ubytePtr   is access all Ubyte;
  type ushortPtr  is access all Ushort;
  type uintPtr    is access all Uint;
  type floatPtr   is access all GL.Float;
  type clampfPtr  is access all Clampf;
  subtype doublePtr is A2A_double.Object_Pointer;

  subtype sizeiPtr is Interfaces.C.ptrdiff_t;   -- used for pointer arithmetic
  subtype intPtr   is Interfaces.C.ptrdiff_t;

  type pointer   is access all Ubyte;  -- our substitute for "void *"

  --  Vectors
  type Light_Float_Vector    is array (0 .. 3) of aliased GL.Float;
  type Material_Float_Vector is array (0 .. 3) of aliased GL.Float;
  type Double_Vector_3D      is array (0 .. 2) of aliased GL.Double;

  type RGB_Color  is record red, green, blue        : GL.Double; end record;
  type RGBA_Color is record red, green, blue, alpha : GL.Double; end record;

   --  Conversions to gl.Pointer

   type color_access is access all GL.RGB_Color;
   function to_Pointer is new Ada.Unchecked_Conversion (color_access, GL.pointer);

   subtype double_access is doublePtr;
   function to_Pointer is new Ada.Unchecked_Conversion (double_access, GL.pointer);

   type natural_access is access all Natural;
   function to_Pointer is new Ada.Unchecked_Conversion (natural_access, GL.pointer);

   function to_Pointer is new Ada.Unchecked_Conversion (uintPtr, GL.pointer);
   function to_Pointer is new Ada.Unchecked_Conversion (ushortPtr,  GL.pointer);

  ------------------------------------------------------------------------------

  --  GL.enum is used only for sizing of the real enumeration types
  type enum is new C.unsigned;

  --  The boolean constants
  GL_FALSE                       : constant GL_Boolean := GL_Boolean'Val (0);
  GL_TRUE                        : constant GL_Boolean := GL_Boolean'Val (1);

  --  Get pointer values
  type GetPointerEnm is
  (
     FEEDBACK_BUFFER_POINTER,
     SELECTION_BUFFER_POINTER,
     VERTEX_ARRAY_POINTER,
     NORMAL_ARRAY_POINTER,
     COLOR_ARRAY_POINTER,
     INDEX_ARRAY_POINTER,
     TEXTURE_COORD_ARRAY_POINTER,
     EDGE_FLAG_ARRAY_POINTER
  );

  for GetPointerEnm use
  (
     FEEDBACK_BUFFER_POINTER                 => 16#0DF0#,
     SELECTION_BUFFER_POINTER                => 16#0DF3#,
     VERTEX_ARRAY_POINTER                    => 16#808E#,
     NORMAL_ARRAY_POINTER                    => 16#808F#,
     COLOR_ARRAY_POINTER                     => 16#8090#,
     INDEX_ARRAY_POINTER                     => 16#8091#,
     TEXTURE_COORD_ARRAY_POINTER             => 16#8092#,
     EDGE_FLAG_ARRAY_POINTER                 => 16#8093#
  );
  for GetPointerEnm'Size use GL.enum'Size;

  procedure GetPointerv (pname  : GetPointerEnm;
                         params : GL.pointer);

  --  Alpha, stencil, and depth tests
  type FuncEnm is
  (
     NEVER,
     LESS,
     EQUAL,
     LEQUAL,
     GREATER,
     NOTEQUAL,
     GEQUAL,
     ALWAYS
  );
  for FuncEnm use
  (
     NEVER                                   => 16#0200#,
     LESS                                    => 16#0201#,
     EQUAL                                   => 16#0202#,
     LEQUAL                                  => 16#0203#,
     GREATER                                 => 16#0204#,
     NOTEQUAL                                => 16#0205#,
     GEQUAL                                  => 16#0206#,
     ALWAYS                                  => 16#0207#
  );
  for FuncEnm'Size use GL.enum'Size;

  procedure AlphaFunc (func : FuncEnm;
                       ref  : GL.Clampf);

  procedure DepthFunc (func : FuncEnm);

  procedure StencilFunc (func : FuncEnm;
                         ref  : GL.Int;
                         mask : GL.Uint);

  --  Stencil operations
  type StencilOpEnm is
  (
     ZERO,
     INVERT,
     KEEP,
     REPLACE,
     INCR,
     DECR
  );
  for StencilOpEnm use
  (
     ZERO                                    => 16#0000#,
     INVERT                                  => 16#150A#,
     KEEP                                    => 16#1E00#,
     REPLACE                                 => 16#1E01#,
     INCR                                    => 16#1E02#,
     DECR                                    => 16#1E03#
  );
  for StencilOpEnm'Size use GL.enum'Size;

  procedure StencilOp (fail  : StencilOpEnm;
                       zfail : StencilOpEnm;
                       zpass : StencilOpEnm);

  --  Blending functions
  type BlendSrcEnm is
  (
     ZERO,
     ONE,
     SRC_ALPHA,
     ONE_MINUS_SRC_ALPHA,
     DST_ALPHA,
     ONE_MINUS_DST_ALPHA,
     DST_COLOR,
     ONE_MINUS_DST_COLOR,
     SRC_ALPHA_SATURATE,
     CONSTANT_COLOR,
     ONE_MINUS_CONSTANT_COLOR,
     CONSTANT_ALPHA,
     ONE_MINUS_CONSTANT_ALPHA
  );
  for BlendSrcEnm use
  (
     ZERO                                    => 16#0000#,
     ONE                                     => 16#0001#,
     SRC_ALPHA                               => 16#0302#,
     ONE_MINUS_SRC_ALPHA                     => 16#0303#,
     DST_ALPHA                               => 16#0304#,
     ONE_MINUS_DST_ALPHA                     => 16#0305#,
     DST_COLOR                               => 16#0306#,
     ONE_MINUS_DST_COLOR                     => 16#0307#,
     SRC_ALPHA_SATURATE                      => 16#0308#,
     CONSTANT_COLOR                          => 16#8001#,  -- are these four Mesa-specific?
     ONE_MINUS_CONSTANT_COLOR                => 16#8002#,
     CONSTANT_ALPHA                          => 16#8003#,
     ONE_MINUS_CONSTANT_ALPHA                => 16#8004#
  );
  for BlendSrcEnm'Size use GL.enum'Size;

  type BlendDstEnm is
  (
     ZERO,
     ONE,
     SRC_COLOR,
     ONE_MINUS_SRC_COLOR,
     SRC_ALPHA,
     ONE_MINUS_SRC_ALPHA,
     DST_ALPHA,
     ONE_MINUS_DST_ALPHA
  );
  for BlendDstEnm use
  (
     ZERO                                    => 16#0000#,
     ONE                                     => 16#0001#,
     SRC_COLOR                               => 16#0300#,
     ONE_MINUS_SRC_COLOR                     => 16#0301#,
     SRC_ALPHA                               => 16#0302#,
     ONE_MINUS_SRC_ALPHA                     => 16#0303#,
     DST_ALPHA                               => 16#0304#,
     ONE_MINUS_DST_ALPHA                     => 16#0305#
  );
  for BlendDstEnm'Size use GL.enum'Size;

  type BlendEquationEnm is
  (
     LOGIC_OP,
     FUNC_ADD_EXT,
     MIN_EXT,
     MAX_EXT,
     FUNC_SUBTRACT_EXT,
     FUNC_REVERSE_SUBTRACT_EXT
  );
  for BlendEquationEnm use
  (
     LOGIC_OP                                => 16#0BF1#,
     FUNC_ADD_EXT                            => 16#8006#,
     MIN_EXT                                 => 16#8007#,
     MAX_EXT                                 => 16#8008#,
     FUNC_SUBTRACT_EXT                       => 16#800A#,
     FUNC_REVERSE_SUBTRACT_EXT               => 16#800B#
  );
  for BlendEquationEnm'Size use GL.enum'Size;

  procedure BlendFunc (sfactor : BlendSrcEnm;
                       dfactor : BlendDstEnm);

  procedure BlendEquationEXT (mode : BlendEquationEnm);

  procedure BlendColorEXT (red   : GL.Clampf;
                           green : GL.Clampf;
                           blue  : GL.Clampf;
                           alpha : GL.Clampf);

  --  Locic operation function
  type LogicOpEnm is
  (
     CLEAR,
     GL_AND,
     AND_REVERSE,
     COPY,
     AND_INVERTED,
     NOOP,
     GL_XOR,
     GL_OR,
     NOR,
     EQUIV,
     INVERT,
     OR_REVERSE,
     COPY_INVERTED,
     OR_INVERTED,
     NAND,
     SET
  );
  for LogicOpEnm use
  (
     CLEAR                                   => 16#1500#,
     GL_AND                                  => 16#1501#,
     AND_REVERSE                             => 16#1502#,
     COPY                                    => 16#1503#,
     AND_INVERTED                            => 16#1504#,
     NOOP                                    => 16#1505#,
     GL_XOR                                  => 16#1506#,
     GL_OR                                   => 16#1507#,
     NOR                                     => 16#1508#,
     EQUIV                                   => 16#1509#,
     INVERT                                  => 16#150A#,
     OR_REVERSE                              => 16#150B#,
     COPY_INVERTED                           => 16#150C#,
     OR_INVERTED                             => 16#150D#,
     NAND                                    => 16#150E#,
     SET                                     => 16#150F#
  );
  for LogicOpEnm'Size use GL.enum'Size;

  procedure LogicOp (opcode : LogicOpEnm);

  --  Face culling.
  --  In OpenGLAda: see type Face_Selector in the package GL.Culling.
  type FaceEnm is
  (
     Front,
     Back,
     Front_And_Back
  );

  for FaceEnm use
  (
     Front                                   => 16#0404#,
     Back                                    => 16#0405#,
     Front_And_Back                          => 16#0408#
  );
  for FaceEnm'Size use GL.enum'Size;

  procedure CullFace (mode : FaceEnm);

  --  Polygon orientation
  type OrientationEnm is
  (
     CW,
     CCW
  );
  for OrientationEnm use
  (
     CW                                      => 16#0900#,
     CCW                                     => 16#0901#
  );
  for OrientationEnm'Size use GL.enum'Size;

  procedure FrontFace (mode : OrientationEnm);

  --  Polygon mode
  type PolygonModeEnm is
  (
     POINT,
     LINE,
     FILL
  );
  for PolygonModeEnm use
  (
     POINT                                   => 16#1B00#,
     LINE                                    => 16#1B01#,
     FILL                                    => 16#1B02#
  );
  for PolygonModeEnm'Size use GL.enum'Size;

  procedure PolygonMode (face : FaceEnm;
                         mode : PolygonModeEnm);

  --  Clipping plane operations
  type ClipPlaneEnm is
  (
     CLIP_PLANE0,
     CLIP_PLANE1,
     CLIP_PLANE2,
     CLIP_PLANE3,
     CLIP_PLANE4,
     CLIP_PLANE5
  );
  for ClipPlaneEnm use
  (
     CLIP_PLANE0                             => 16#3000#,
     CLIP_PLANE1                             => 16#3001#,
     CLIP_PLANE2                             => 16#3002#,
     CLIP_PLANE3                             => 16#3003#,
     CLIP_PLANE4                             => 16#3004#,
     CLIP_PLANE5                             => 16#3005#
  );
  for ClipPlaneEnm'Size use GL.enum'Size;

  procedure ClipPlane (plane    : ClipPlaneEnm;
                       equation : GL.doublePtr);

  procedure GetClipPlane (plane    : ClipPlaneEnm;
                          equation : GL.doublePtr);

  --  Buffer selection
  type DrawBufferEnm is
  (
     NONE,
     FRONT_LEFT,
     FRONT_RIGHT,
     BACK_LEFT,
     BACK_RIGHT,
     FRONT,
     BACK,
     LEFT,
     RIGHT,
     FRONT_AND_BACK,
     AUX0,
     AUX1,
     AUX2,
     AUX3
  );
  for DrawBufferEnm use
  (
     NONE                                    => 16#0000#,
     FRONT_LEFT                              => 16#0400#,
     FRONT_RIGHT                             => 16#0401#,
     BACK_LEFT                               => 16#0402#,
     BACK_RIGHT                              => 16#0403#,
     FRONT                                   => 16#0404#,
     BACK                                    => 16#0405#,
     LEFT                                    => 16#0406#,
     RIGHT                                   => 16#0407#,
     FRONT_AND_BACK                          => 16#0408#,
     AUX0                                    => 16#0409#,
     AUX1                                    => 16#040A#,
     AUX2                                    => 16#040B#,
     AUX3                                    => 16#040C#
  );
  for DrawBufferEnm'Size use GL.enum'Size;

  procedure DrawBuffer (mode : DrawBufferEnm);

  type ReadBufferEnm is
  (
     FRONT_LEFT,
     FRONT_RIGHT,
     BACK_LEFT,
     BACK_RIGHT,
     FRONT,
     BACK,
     LEFT,
     RIGHT,
     AUX0,
     AUX1,
     AUX2,
     AUX3
  );
  for ReadBufferEnm use
  (
     FRONT_LEFT                              => 16#0400#,
     FRONT_RIGHT                             => 16#0401#,
     BACK_LEFT                               => 16#0402#,
     BACK_RIGHT                              => 16#0403#,
     FRONT                                   => 16#0404#,
     BACK                                    => 16#0405#,
     LEFT                                    => 16#0406#,
     RIGHT                                   => 16#0407#,
     AUX0                                    => 16#0409#,
     AUX1                                    => 16#040A#,
     AUX2                                    => 16#040B#,
     AUX3                                    => 16#040C#
  );
  for ReadBufferEnm'Size use GL.enum'Size;

  procedure ReadBuffer (mode : ReadBufferEnm);

  --  Server-side capabilities.
  --  In OpenGLAda: see type Toggle in the package GL.Toggles.
  type ServerCapabilityEnm is
  (
     POINT_SMOOTH,
     LINE_SMOOTH,
     LINE_STIPPLE,
     POLYGON_SMOOTH,
     POLYGON_STIPPLE,
     Cull_Face,
     Lighting,
     Color_Material,
     Fog,
     Depth_Test,
     STENCIL_TEST,
     Normalize,
     Alpha_Test,
     DITHER,
     Blend,
     INDEX_LOGIC_OP,
     COLOR_LOGIC_OP,
     Scissor_Test,
     TEXTURE_GEN_S,
     TEXTURE_GEN_T,
     TEXTURE_GEN_R,
     TEXTURE_GEN_Q,
     Auto_Normal,
     MAP1_COLOR_4,
     MAP1_INDEX,
     MAP1_NORMAL,
     MAP1_TEXTURE_COORD_1,
     MAP1_TEXTURE_COORD_2,
     MAP1_TEXTURE_COORD_3,
     MAP1_TEXTURE_COORD_4,
     MAP1_VERTEX_3,
     MAP1_VERTEX_4,
     MAP2_COLOR_4,
     MAP2_INDEX,
     MAP2_NORMAL,
     MAP2_TEXTURE_COORD_1,
     MAP2_TEXTURE_COORD_2,
     MAP2_TEXTURE_COORD_3,
     MAP2_TEXTURE_COORD_4,
     MAP2_VERTEX_3,
     MAP2_VERTEX_4,
     Texture_1D,
     Texture_2D,
     POLYGON_OFFSET_POINT,
     POLYGON_OFFSET_LINE,
     CLIP_PLANE0,
     CLIP_PLANE1,
     CLIP_PLANE2,
     CLIP_PLANE3,
     CLIP_PLANE4,
     CLIP_PLANE5,
     LIGHT0,
     LIGHT1,
     LIGHT2,
     LIGHT3,
     LIGHT4,
     LIGHT5,
     LIGHT6,
     LIGHT7,
     POLYGON_OFFSET_FILL,
     TEXTURE_3D_EXT,
     --  ARB_multisampling:
     MULTISAMPLE_ARB,
     SAMPLE_ALPHA_TO_COVERAGE_ARB,
     SAMPLE_ALPHA_TO_ONE_ARB,
     SAMPLE_COVERAGE_ARB,
     SAMPLE_BUFFERS_ARB,
     SAMPLES_ARB,
     SAMPLE_COVERAGE_VALUE_ARB,
     SAMPLE_COVERAGE_INVERT_ARB,
     MULTISAMPLE_BIT_ARB
);
  for ServerCapabilityEnm use
  (
     POINT_SMOOTH                            => 16#0B10#,
     LINE_SMOOTH                             => 16#0B20#,
     LINE_STIPPLE                            => 16#0B24#,
     POLYGON_SMOOTH                          => 16#0B41#,
     POLYGON_STIPPLE                         => 16#0B42#,
     Cull_Face                               => 16#0B44#,
     Lighting                                => 16#0B50#,
     Color_Material                          => 16#0B57#,
     Fog                                     => 16#0B60#,
     Depth_Test                              => 16#0B71#,
     STENCIL_TEST                            => 16#0B90#,
     Normalize                               => 16#0BA1#,
     Alpha_Test                              => 16#0BC0#,
     DITHER                                  => 16#0BD0#,
     Blend                                   => 16#0BE2#,
     INDEX_LOGIC_OP                          => 16#0BF1#,
     COLOR_LOGIC_OP                          => 16#0BF2#,
     Scissor_Test                            => 16#0C11#,
     TEXTURE_GEN_S                           => 16#0C60#,
     TEXTURE_GEN_T                           => 16#0C61#,
     TEXTURE_GEN_R                           => 16#0C62#,
     TEXTURE_GEN_Q                           => 16#0C63#,
     Auto_Normal                             => 16#0D80#,
     MAP1_COLOR_4                            => 16#0D90#,
     MAP1_INDEX                              => 16#0D91#,
     MAP1_NORMAL                             => 16#0D92#,
     MAP1_TEXTURE_COORD_1                    => 16#0D93#,
     MAP1_TEXTURE_COORD_2                    => 16#0D94#,
     MAP1_TEXTURE_COORD_3                    => 16#0D95#,
     MAP1_TEXTURE_COORD_4                    => 16#0D96#,
     MAP1_VERTEX_3                           => 16#0D97#,
     MAP1_VERTEX_4                           => 16#0D98#,
     MAP2_COLOR_4                            => 16#0DB0#,
     MAP2_INDEX                              => 16#0DB1#,
     MAP2_NORMAL                             => 16#0DB2#,
     MAP2_TEXTURE_COORD_1                    => 16#0DB3#,
     MAP2_TEXTURE_COORD_2                    => 16#0DB4#,
     MAP2_TEXTURE_COORD_3                    => 16#0DB5#,
     MAP2_TEXTURE_COORD_4                    => 16#0DB6#,
     MAP2_VERTEX_3                           => 16#0DB7#,
     MAP2_VERTEX_4                           => 16#0DB8#,
     Texture_1D                              => 16#0DE0#,
     Texture_2D                              => 16#0DE1#,
     POLYGON_OFFSET_POINT                    => 16#2A01#,
     POLYGON_OFFSET_LINE                     => 16#2A02#,
     CLIP_PLANE0                             => 16#3000#,
     CLIP_PLANE1                             => 16#3001#,
     CLIP_PLANE2                             => 16#3002#,
     CLIP_PLANE3                             => 16#3003#,
     CLIP_PLANE4                             => 16#3004#,
     CLIP_PLANE5                             => 16#3005#,
     LIGHT0                                  => 16#4000#,
     LIGHT1                                  => 16#4001#,
     LIGHT2                                  => 16#4002#,
     LIGHT3                                  => 16#4003#,
     LIGHT4                                  => 16#4004#,
     LIGHT5                                  => 16#4005#,
     LIGHT6                                  => 16#4006#,
     LIGHT7                                  => 16#4007#,
     POLYGON_OFFSET_FILL                     => 16#8037#,
     TEXTURE_3D_EXT                          => 16#806F#,

     MULTISAMPLE_ARB                         => 16#809D#,
     SAMPLE_ALPHA_TO_COVERAGE_ARB            => 16#809E#,
     SAMPLE_ALPHA_TO_ONE_ARB                 => 16#809F#,
     SAMPLE_COVERAGE_ARB                     => 16#80A0#,
     SAMPLE_BUFFERS_ARB                      => 16#80A8#,
     SAMPLES_ARB                             => 16#80A9#,
     SAMPLE_COVERAGE_VALUE_ARB               => 16#80AA#,
     SAMPLE_COVERAGE_INVERT_ARB              => 16#80AB#,
     MULTISAMPLE_BIT_ARB                     => 16#20000000#
  );
  for ServerCapabilityEnm'Size use GL.enum'Size;

  procedure Enable (cap : ServerCapabilityEnm);

  procedure Disable (cap : ServerCapabilityEnm);

  function IsEnabled (cap : ServerCapabilityEnm)
  return GL_Boolean;

  --  Client state
  type ClientCapabilityEnm is
  (
     VERTEX_ARRAY,
     NORMAL_ARRAY,
     COLOR_ARRAY,
     INDEX_ARRAY,
     TEXTURE_COORD_ARRAY,
     EDGE_FLAG_ARRAY
  );
  for ClientCapabilityEnm use
  (
     VERTEX_ARRAY                            => 16#8074#,
     NORMAL_ARRAY                            => 16#8075#,
     COLOR_ARRAY                             => 16#8076#,
     INDEX_ARRAY                             => 16#8077#,
     TEXTURE_COORD_ARRAY                     => 16#8078#,
     EDGE_FLAG_ARRAY                         => 16#8079#
  );
  for ClientCapabilityEnm'Size use GL.enum'Size;

  procedure EnableClientState (cap : ClientCapabilityEnm);

  procedure DisableClientState (cap : ClientCapabilityEnm);

  --  Parameter fetches
  type ParameterNameEnm is
  (
     CURRENT_COLOR,
     CURRENT_INDEX,
     CURRENT_NORMAL,
     CURRENT_TEXTURE_COORDS,
     CURRENT_RASTER_COLOR,
     CURRENT_RASTER_INDEX,
     CURRENT_RASTER_TEXTURE_COORDS,
     CURRENT_RASTER_POSITION,
     CURRENT_RASTER_POSITION_VALID,
     CURRENT_RASTER_DISTANCE,
     POINT_SMOOTH,
     POINT_SIZE,
     POINT_SIZE_RANGE,
     POINT_SIZE_GRANULARITY,
     LINE_SMOOTH,
     LINE_WIDTH,
     LINE_WIDTH_RANGE,
     LINE_WIDTH_GRANULARITY,
     LINE_STIPPLE,
     LINE_STIPPLE_PATTERN,
     LINE_STIPPLE_REPEAT,
     LIST_MODE,
     MAX_LIST_NESTING,
     LIST_BASE,
     LIST_INDEX,
     POLYGON_MODE,
     POLYGON_SMOOTH,
     POLYGON_STIPPLE,
     EDGE_FLAG,
     CULL_FACE,
     CULL_FACE_MODE,
     FRONT_FACE,
     LIGHTING,
     LIGHT_MODEL_LOCAL_VIEWER,
     LIGHT_MODEL_TWO_SIDE,
     LIGHT_MODEL_AMBIENT,
     SHADE_MODEL,
     COLOR_MATERIAL_FACE,
     COLOR_MATERIAL_PARAMETER,
     COLOR_MATERIAL,
     FOG,
     FOG_INDEX,
     FOG_DENSITY,
     FOG_START,
     FOG_END,
     FOG_MODE,
     FOG_COLOR,
     DEPTH_RANGE,
     DEPTH_TEST,
     DEPTH_WRITEMASK,
     DEPTH_CLEAR_VALUE,
     DEPTH_FUNC,
     ACCUM_CLEAR_VALUE,
     STENCIL_TEST,
     STENCIL_CLEAR_VALUE,
     STENCIL_FUNC,
     STENCIL_VALUE_MASK,
     STENCIL_FAIL,
     STENCIL_PASS_DEPTH_FAIL,
     STENCIL_PASS_DEPTH_PASS,
     STENCIL_REF,
     STENCIL_WRITEMASK,
     MATRIX_MODE,
     NORMALIZE,
     VIEWPORT,
     MODELVIEW_STACK_DEPTH,
     PROJECTION_STACK_DEPTH,
     TEXTURE_STACK_DEPTH,
     MODELVIEW_MATRIX,
     PROJECTION_MATRIX,
     TEXTURE_MATRIX,
     ATTRIB_STACK_DEPTH,
     CLIENT_ATTRIB_STACK_DEPTH,
     ALPHA_TEST,
     ALPHA_TEST_FUNC,
     ALPHA_TEST_REF,
     DITHER,
     BLEND_DST,
     BLEND_SRC,
     BLEND,
     LOGIC_OP_MODE,
     INDEX_LOGIC_OP,
     COLOR_LOGIC_OP,
     AUX_BUFFERS,
     DRAW_BUFFER,
     READ_BUFFER,
     SCISSOR_BOX,
     SCISSOR_TEST,
     INDEX_CLEAR_VALUE,
     INDEX_WRITEMASK,
     COLOR_CLEAR_VALUE,
     COLOR_WRITEMASK,
     INDEX_MODE,
     RGBA_MODE,
     DOUBLEBUFFER,
     STEREO,
     RENDER_MODE,
     PERSPECTIVE_CORRECTION_HINT,
     POINT_SMOOTH_HINT,
     LINE_SMOOTH_HINT,
     POLYGON_SMOOTH_HINT,
     FOG_HINT,
     TEXTURE_GEN_S,
     TEXTURE_GEN_T,
     TEXTURE_GEN_R,
     TEXTURE_GEN_Q,
     PIXEL_MAP_I_TO_I_SIZE,
     PIXEL_MAP_S_TO_S_SIZE,
     PIXEL_MAP_I_TO_R_SIZE,
     PIXEL_MAP_I_TO_G_SIZE,
     PIXEL_MAP_I_TO_B_SIZE,
     PIXEL_MAP_I_TO_A_SIZE,
     PIXEL_MAP_R_TO_R_SIZE,
     PIXEL_MAP_G_TO_G_SIZE,
     PIXEL_MAP_B_TO_B_SIZE,
     PIXEL_MAP_A_TO_A_SIZE,
     UNPACK_SWAP_BYTES,
     UNPACK_LSB_FIRST,
     UNPACK_ROW_LENGTH,
     UNPACK_SKIP_ROWS,
     UNPACK_SKIP_PIXELS,
     UNPACK_ALIGNMENT,
     PACK_SWAP_BYTES,
     PACK_LSB_FIRST,
     PACK_ROW_LENGTH,
     PACK_SKIP_ROWS,
     PACK_SKIP_PIXELS,
     PACK_ALIGNMENT,
     MAP_COLOR,
     MAP_STENCIL,
     INDEX_SHIFT,
     INDEX_OFFSET,
     RED_SCALE,
     RED_BIAS,
     ZOOM_X,
     ZOOM_Y,
     GREEN_SCALE,
     GREEN_BIAS,
     BLUE_SCALE,
     BLUE_BIAS,
     ALPHA_SCALE,
     ALPHA_BIAS,
     DEPTH_SCALE,
     DEPTH_BIAS,
     MAX_EVAL_ORDER,
     MAX_LIGHTS,
     MAX_CLIP_PLANES,
     MAX_TEXTURE_SIZE,
     MAX_PIXEL_MAP_TABLE,
     MAX_ATTRIB_STACK_DEPTH,
     MAX_MODELVIEW_STACK_DEPTH,
     MAX_NAME_STACK_DEPTH,
     MAX_PROJECTION_STACK_DEPTH,
     MAX_TEXTURE_STACK_DEPTH,
     MAX_VIEWPORT_DIMS,
     MAX_CLIENT_ATTRIB_STACK_DEPTH,
     SUBPIXEL_BITS,
     INDEX_BITS,
     RED_BITS,
     GREEN_BITS,
     BLUE_BITS,
     ALPHA_BITS,
     DEPTH_BITS,
     STENCIL_BITS,
     ACCUM_RED_BITS,
     ACCUM_GREEN_BITS,
     ACCUM_BLUE_BITS,
     ACCUM_ALPHA_BITS,
     NAME_STACK_DEPTH,
     AUTO_NORMAL,
     MAP1_COLOR_4,
     MAP1_INDEX,
     MAP1_NORMAL,
     MAP1_TEXTURE_COORD_1,
     MAP1_TEXTURE_COORD_2,
     MAP1_TEXTURE_COORD_3,
     MAP1_TEXTURE_COORD_4,
     MAP1_VERTEX_3,
     MAP1_VERTEX_4,
     MAP2_COLOR_4,
     MAP2_INDEX,
     MAP2_NORMAL,
     MAP2_TEXTURE_COORD_1,
     MAP2_TEXTURE_COORD_2,
     MAP2_TEXTURE_COORD_3,
     MAP2_TEXTURE_COORD_4,
     MAP2_VERTEX_3,
     MAP2_VERTEX_4,
     MAP1_GRID_DOMAIN,
     MAP1_GRID_SEGMENTS,
     MAP2_GRID_DOMAIN,
     MAP2_GRID_SEGMENTS,
     TEXTURE_1D,
     TEXTURE_2D,
     POLYGON_OFFSET_UNITS,
     POLYGON_OFFSET_POINT,
     POLYGON_OFFSET_LINE,
     POLYGON_OFFSET_FILL,
     POLYGON_OFFSET_FACTOR,
     TEXTURE_BINDING_1D,
     TEXTURE_BINDING_2D,
     VERTEX_ARRAY,
     NORMAL_ARRAY,
     COLOR_ARRAY,
     INDEX_ARRAY,
     TEXTURE_COORD_ARRAY,
     EDGE_FLAG_ARRAY,
     VERTEX_ARRAY_SIZE,
     VERTEX_ARRAY_TYPE,
     VERTEX_ARRAY_STRIDE,
     NORMAL_ARRAY_TYPE,
     NORMAL_ARRAY_STRIDE,
     COLOR_ARRAY_SIZE,
     COLOR_ARRAY_TYPE,
     COLOR_ARRAY_STRIDE,
     INDEX_ARRAY_TYPE,
     INDEX_ARRAY_STRIDE,
     TEXTURE_COORD_ARRAY_SIZE,
     TEXTURE_COORD_ARRAY_TYPE,
     TEXTURE_COORD_ARRAY_STRIDE,
     EDGE_FLAG_ARRAY_STRIDE,
     SAMPLES
  );
  for ParameterNameEnm use
  (
     CURRENT_COLOR                           => 16#0B00#,
     CURRENT_INDEX                           => 16#0B01#,
     CURRENT_NORMAL                          => 16#0B02#,
     CURRENT_TEXTURE_COORDS                  => 16#0B03#,
     CURRENT_RASTER_COLOR                    => 16#0B04#,
     CURRENT_RASTER_INDEX                    => 16#0B05#,
     CURRENT_RASTER_TEXTURE_COORDS           => 16#0B06#,
     CURRENT_RASTER_POSITION                 => 16#0B07#,
     CURRENT_RASTER_POSITION_VALID           => 16#0B08#,
     CURRENT_RASTER_DISTANCE                 => 16#0B09#,
     POINT_SMOOTH                            => 16#0B10#,
     POINT_SIZE                              => 16#0B11#,
     POINT_SIZE_RANGE                        => 16#0B12#,
     POINT_SIZE_GRANULARITY                  => 16#0B13#,
     LINE_SMOOTH                             => 16#0B20#,
     LINE_WIDTH                              => 16#0B21#,
     LINE_WIDTH_RANGE                        => 16#0B22#,
     LINE_WIDTH_GRANULARITY                  => 16#0B23#,
     LINE_STIPPLE                            => 16#0B24#,
     LINE_STIPPLE_PATTERN                    => 16#0B25#,
     LINE_STIPPLE_REPEAT                     => 16#0B26#,
     LIST_MODE                               => 16#0B30#,
     MAX_LIST_NESTING                        => 16#0B31#,
     LIST_BASE                               => 16#0B32#,
     LIST_INDEX                              => 16#0B33#,
     POLYGON_MODE                            => 16#0B40#,
     POLYGON_SMOOTH                          => 16#0B41#,
     POLYGON_STIPPLE                         => 16#0B42#,
     EDGE_FLAG                               => 16#0B43#,
     CULL_FACE                               => 16#0B44#,
     CULL_FACE_MODE                          => 16#0B45#,
     FRONT_FACE                              => 16#0B46#,
     LIGHTING                                => 16#0B50#,
     LIGHT_MODEL_LOCAL_VIEWER                => 16#0B51#,
     LIGHT_MODEL_TWO_SIDE                    => 16#0B52#,
     LIGHT_MODEL_AMBIENT                     => 16#0B53#,
     SHADE_MODEL                             => 16#0B54#,
     COLOR_MATERIAL_FACE                     => 16#0B55#,
     COLOR_MATERIAL_PARAMETER                => 16#0B56#,
     COLOR_MATERIAL                          => 16#0B57#,
     FOG                                     => 16#0B60#,
     FOG_INDEX                               => 16#0B61#,
     FOG_DENSITY                             => 16#0B62#,
     FOG_START                               => 16#0B63#,
     FOG_END                                 => 16#0B64#,
     FOG_MODE                                => 16#0B65#,
     FOG_COLOR                               => 16#0B66#,
     DEPTH_RANGE                             => 16#0B70#,
     DEPTH_TEST                              => 16#0B71#,
     DEPTH_WRITEMASK                         => 16#0B72#,
     DEPTH_CLEAR_VALUE                       => 16#0B73#,
     DEPTH_FUNC                              => 16#0B74#,
     ACCUM_CLEAR_VALUE                       => 16#0B80#,
     STENCIL_TEST                            => 16#0B90#,
     STENCIL_CLEAR_VALUE                     => 16#0B91#,
     STENCIL_FUNC                            => 16#0B92#,
     STENCIL_VALUE_MASK                      => 16#0B93#,
     STENCIL_FAIL                            => 16#0B94#,
     STENCIL_PASS_DEPTH_FAIL                 => 16#0B95#,
     STENCIL_PASS_DEPTH_PASS                 => 16#0B96#,
     STENCIL_REF                             => 16#0B97#,
     STENCIL_WRITEMASK                       => 16#0B98#,
     MATRIX_MODE                             => 16#0BA0#,
     NORMALIZE                               => 16#0BA1#,
     VIEWPORT                                => 16#0BA2#,
     MODELVIEW_STACK_DEPTH                   => 16#0BA3#,
     PROJECTION_STACK_DEPTH                  => 16#0BA4#,
     TEXTURE_STACK_DEPTH                     => 16#0BA5#,
     MODELVIEW_MATRIX                        => 16#0BA6#,
     PROJECTION_MATRIX                       => 16#0BA7#,
     TEXTURE_MATRIX                          => 16#0BA8#,
     ATTRIB_STACK_DEPTH                      => 16#0BB0#,
     CLIENT_ATTRIB_STACK_DEPTH               => 16#0BB1#,
     ALPHA_TEST                              => 16#0BC0#,
     ALPHA_TEST_FUNC                         => 16#0BC1#,
     ALPHA_TEST_REF                          => 16#0BC2#,
     DITHER                                  => 16#0BD0#,
     BLEND_DST                               => 16#0BE0#,
     BLEND_SRC                               => 16#0BE1#,
     BLEND                                   => 16#0BE2#,
     LOGIC_OP_MODE                           => 16#0BF0#,
     INDEX_LOGIC_OP                          => 16#0BF1#,
     COLOR_LOGIC_OP                          => 16#0BF2#,
     AUX_BUFFERS                             => 16#0C00#,
     DRAW_BUFFER                             => 16#0C01#,
     READ_BUFFER                             => 16#0C02#,
     SCISSOR_BOX                             => 16#0C10#,
     SCISSOR_TEST                            => 16#0C11#,
     INDEX_CLEAR_VALUE                       => 16#0C20#,
     INDEX_WRITEMASK                         => 16#0C21#,
     COLOR_CLEAR_VALUE                       => 16#0C22#,
     COLOR_WRITEMASK                         => 16#0C23#,
     INDEX_MODE                              => 16#0C30#,
     RGBA_MODE                               => 16#0C31#,
     DOUBLEBUFFER                            => 16#0C32#,
     STEREO                                  => 16#0C33#,
     RENDER_MODE                             => 16#0C40#,
     PERSPECTIVE_CORRECTION_HINT             => 16#0C50#,
     POINT_SMOOTH_HINT                       => 16#0C51#,
     LINE_SMOOTH_HINT                        => 16#0C52#,
     POLYGON_SMOOTH_HINT                     => 16#0C53#,
     FOG_HINT                                => 16#0C54#,
     TEXTURE_GEN_S                           => 16#0C60#,
     TEXTURE_GEN_T                           => 16#0C61#,
     TEXTURE_GEN_R                           => 16#0C62#,
     TEXTURE_GEN_Q                           => 16#0C63#,
     PIXEL_MAP_I_TO_I_SIZE                   => 16#0CB0#,
     PIXEL_MAP_S_TO_S_SIZE                   => 16#0CB1#,
     PIXEL_MAP_I_TO_R_SIZE                   => 16#0CB2#,
     PIXEL_MAP_I_TO_G_SIZE                   => 16#0CB3#,
     PIXEL_MAP_I_TO_B_SIZE                   => 16#0CB4#,
     PIXEL_MAP_I_TO_A_SIZE                   => 16#0CB5#,
     PIXEL_MAP_R_TO_R_SIZE                   => 16#0CB6#,
     PIXEL_MAP_G_TO_G_SIZE                   => 16#0CB7#,
     PIXEL_MAP_B_TO_B_SIZE                   => 16#0CB8#,
     PIXEL_MAP_A_TO_A_SIZE                   => 16#0CB9#,
     UNPACK_SWAP_BYTES                       => 16#0CF0#,
     UNPACK_LSB_FIRST                        => 16#0CF1#,
     UNPACK_ROW_LENGTH                       => 16#0CF2#,
     UNPACK_SKIP_ROWS                        => 16#0CF3#,
     UNPACK_SKIP_PIXELS                      => 16#0CF4#,
     UNPACK_ALIGNMENT                        => 16#0CF5#,
     PACK_SWAP_BYTES                         => 16#0D00#,
     PACK_LSB_FIRST                          => 16#0D01#,
     PACK_ROW_LENGTH                         => 16#0D02#,
     PACK_SKIP_ROWS                          => 16#0D03#,
     PACK_SKIP_PIXELS                        => 16#0D04#,
     PACK_ALIGNMENT                          => 16#0D05#,
     MAP_COLOR                               => 16#0D10#,
     MAP_STENCIL                             => 16#0D11#,
     INDEX_SHIFT                             => 16#0D12#,
     INDEX_OFFSET                            => 16#0D13#,
     RED_SCALE                               => 16#0D14#,
     RED_BIAS                                => 16#0D15#,
     ZOOM_X                                  => 16#0D16#,
     ZOOM_Y                                  => 16#0D17#,
     GREEN_SCALE                             => 16#0D18#,
     GREEN_BIAS                              => 16#0D19#,
     BLUE_SCALE                              => 16#0D1A#,
     BLUE_BIAS                               => 16#0D1B#,
     ALPHA_SCALE                             => 16#0D1C#,
     ALPHA_BIAS                              => 16#0D1D#,
     DEPTH_SCALE                             => 16#0D1E#,
     DEPTH_BIAS                              => 16#0D1F#,
     MAX_EVAL_ORDER                          => 16#0D30#,
     MAX_LIGHTS                              => 16#0D31#,
     MAX_CLIP_PLANES                         => 16#0D32#,
     MAX_TEXTURE_SIZE                        => 16#0D33#,
     MAX_PIXEL_MAP_TABLE                     => 16#0D34#,
     MAX_ATTRIB_STACK_DEPTH                  => 16#0D35#,
     MAX_MODELVIEW_STACK_DEPTH               => 16#0D36#,
     MAX_NAME_STACK_DEPTH                    => 16#0D37#,
     MAX_PROJECTION_STACK_DEPTH              => 16#0D38#,
     MAX_TEXTURE_STACK_DEPTH                 => 16#0D39#,
     MAX_VIEWPORT_DIMS                       => 16#0D3A#,
     MAX_CLIENT_ATTRIB_STACK_DEPTH           => 16#0D3B#,
     SUBPIXEL_BITS                           => 16#0D50#,
     INDEX_BITS                              => 16#0D51#,
     RED_BITS                                => 16#0D52#,
     GREEN_BITS                              => 16#0D53#,
     BLUE_BITS                               => 16#0D54#,
     ALPHA_BITS                              => 16#0D55#,
     DEPTH_BITS                              => 16#0D56#,
     STENCIL_BITS                            => 16#0D57#,
     ACCUM_RED_BITS                          => 16#0D58#,
     ACCUM_GREEN_BITS                        => 16#0D59#,
     ACCUM_BLUE_BITS                         => 16#0D5A#,
     ACCUM_ALPHA_BITS                        => 16#0D5B#,
     NAME_STACK_DEPTH                        => 16#0D70#,
     AUTO_NORMAL                             => 16#0D80#,
     MAP1_COLOR_4                            => 16#0D90#,
     MAP1_INDEX                              => 16#0D91#,
     MAP1_NORMAL                             => 16#0D92#,
     MAP1_TEXTURE_COORD_1                    => 16#0D93#,
     MAP1_TEXTURE_COORD_2                    => 16#0D94#,
     MAP1_TEXTURE_COORD_3                    => 16#0D95#,
     MAP1_TEXTURE_COORD_4                    => 16#0D96#,
     MAP1_VERTEX_3                           => 16#0D97#,
     MAP1_VERTEX_4                           => 16#0D98#,
     MAP2_COLOR_4                            => 16#0DB0#,
     MAP2_INDEX                              => 16#0DB1#,
     MAP2_NORMAL                             => 16#0DB2#,
     MAP2_TEXTURE_COORD_1                    => 16#0DB3#,
     MAP2_TEXTURE_COORD_2                    => 16#0DB4#,
     MAP2_TEXTURE_COORD_3                    => 16#0DB5#,
     MAP2_TEXTURE_COORD_4                    => 16#0DB6#,
     MAP2_VERTEX_3                           => 16#0DB7#,
     MAP2_VERTEX_4                           => 16#0DB8#,
     MAP1_GRID_DOMAIN                        => 16#0DD0#,
     MAP1_GRID_SEGMENTS                      => 16#0DD1#,
     MAP2_GRID_DOMAIN                        => 16#0DD2#,
     MAP2_GRID_SEGMENTS                      => 16#0DD3#,
     TEXTURE_1D                              => 16#0DE0#,
     TEXTURE_2D                              => 16#0DE1#,
     POLYGON_OFFSET_UNITS                    => 16#2A00#,
     POLYGON_OFFSET_POINT                    => 16#2A01#,
     POLYGON_OFFSET_LINE                     => 16#2A02#,
     POLYGON_OFFSET_FILL                     => 16#8037#,
     POLYGON_OFFSET_FACTOR                   => 16#8038#,
     TEXTURE_BINDING_1D                      => 16#8068#,
     TEXTURE_BINDING_2D                      => 16#8069#,
     VERTEX_ARRAY                            => 16#8074#,
     NORMAL_ARRAY                            => 16#8075#,
     COLOR_ARRAY                             => 16#8076#,
     INDEX_ARRAY                             => 16#8077#,
     TEXTURE_COORD_ARRAY                     => 16#8078#,
     EDGE_FLAG_ARRAY                         => 16#8079#,
     VERTEX_ARRAY_SIZE                       => 16#807A#,
     VERTEX_ARRAY_TYPE                       => 16#807B#,
     VERTEX_ARRAY_STRIDE                     => 16#807C#,
     NORMAL_ARRAY_TYPE                       => 16#807E#,
     NORMAL_ARRAY_STRIDE                     => 16#807F#,
     COLOR_ARRAY_SIZE                        => 16#8081#,
     COLOR_ARRAY_TYPE                        => 16#8082#,
     COLOR_ARRAY_STRIDE                      => 16#8083#,
     INDEX_ARRAY_TYPE                        => 16#8085#,
     INDEX_ARRAY_STRIDE                      => 16#8086#,
     TEXTURE_COORD_ARRAY_SIZE                => 16#8088#,
     TEXTURE_COORD_ARRAY_TYPE                => 16#8089#,
     TEXTURE_COORD_ARRAY_STRIDE              => 16#808A#,
     EDGE_FLAG_ARRAY_STRIDE                  => 16#808C#,
     SAMPLES                                 => 16#80A9#
  );
  for ParameterNameEnm'Size use GL.enum'Size;

  procedure GetBooleanv (pname  : ParameterNameEnm;
                         params : GL_BooleanPtr);

  procedure Get (pname  : ParameterNameEnm;
                 params : GL.doublePtr);

  procedure GetFloatv (pname  : ParameterNameEnm;
                       params : floatPtr);

  procedure GetIntegerv (pname  : ParameterNameEnm;
                         params : GL.intPointer);

  --  Render mode
  type RenderModeEnm is
  (
     RENDER,
     FEEDBACK,
     GL_SELECT
  );
  for RenderModeEnm use
  (
     RENDER                                  => 16#1C00#,
     FEEDBACK                                => 16#1C01#,
     GL_SELECT                               => 16#1C02#
  );
  for RenderModeEnm'Size use GL.enum'Size;

  function RenderMode (mode : RenderModeEnm)
  return GL.Int;

  --  Error information
  type ErrorEnm is
  (
     NO_ERROR,
     INVALID_ENUM,
     INVALID_VALUE,
     INVALID_OPERATION,
     STACK_OVERFLOW,
     STACK_UNDERFLOW,
     OUT_OF_MEMORY
  );
  for ErrorEnm use
  (
     NO_ERROR                                => 16#0000#,
     INVALID_ENUM                            => 16#0500#,
     INVALID_VALUE                           => 16#0501#,
     INVALID_OPERATION                       => 16#0502#,
     STACK_OVERFLOW                          => 16#0503#,
     STACK_UNDERFLOW                         => 16#0504#,
     OUT_OF_MEMORY                           => 16#0505#
  );
  for ErrorEnm'Size use GL.enum'Size;

  function GetError
  return ErrorEnm;

  --  Connection description
  type StringEnm is
  (
     VENDOR,
     RENDERER,
     VERSION,
     EXTENSIONS
  );
  for StringEnm use
  (
     VENDOR                                  => 16#1F00#,
     RENDERER                                => 16#1F01#,
     VERSION                                 => 16#1F02#,
     EXTENSIONS                              => 16#1F03#
  );
  for StringEnm'Size use GL.enum'Size;

  function GetString (name : StringEnm) return ubytePtr;
  function GetString (name : StringEnm) return String;

  --  Behavior hints
  type HintEnm is
  (
     PERSPECTIVE_CORRECTION_HINT,
     POINT_SMOOTH_HINT,
     LINE_SMOOTH_HINT,
     POLYGON_SMOOTH_HINT,
     FOG_HINT
  );
  for HintEnm use
  (
     PERSPECTIVE_CORRECTION_HINT             => 16#0C50#,
     POINT_SMOOTH_HINT                       => 16#0C51#,
     LINE_SMOOTH_HINT                        => 16#0C52#,
     POLYGON_SMOOTH_HINT                     => 16#0C53#,
     FOG_HINT                                => 16#0C54#
  );
  for HintEnm'Size use GL.enum'Size;

  type HintModeEnm is
  (
     DONT_CARE,
     FASTEST,
     NICEST
  );
  for HintModeEnm use
  (
     DONT_CARE                               => 16#1100#,
     FASTEST                                 => 16#1101#,
     NICEST                                  => 16#1102#
  );
  for HintModeEnm'Size use GL.enum'Size;

  procedure Hint (target : HintEnm;
                  mode   : HintModeEnm);

  --  Accumulation buffer
  type AccumEnm is
  (
     ACCUM,
     LOAD,
     GL_RETURN,
     MULT,
     ADD
  );
  for AccumEnm use
  (
     ACCUM                                   => 16#0100#,
     LOAD                                    => 16#0101#,
     GL_RETURN                               => 16#0102#,
     MULT                                    => 16#0103#,
     ADD                                     => 16#0104#
  );
  for AccumEnm'Size use GL.enum'Size;

  procedure Accum (op    : AccumEnm;
                   value : GL.Float);

  --  Matrix mode
  type MatrixModeEnm is
  (
     MODELVIEW,
     PROJECTION,
     TEXTURE
  );
  for MatrixModeEnm use
  (
     MODELVIEW                               => 16#1700#,
     PROJECTION                              => 16#1701#,
     TEXTURE                                 => 16#1702#
  );
  for MatrixModeEnm'Size use GL.enum'Size;

  procedure MatrixMode (mode : MatrixModeEnm);

  --  Display liststype ListModeEnm is
  type ListModeEnm is
  (
     COMPILE,
     COMPILE_AND_EXECUTE
  );
  for ListModeEnm use
  (
     COMPILE                                 => 16#1300#,
     COMPILE_AND_EXECUTE                     => 16#1301#
  );
  for ListModeEnm'Size use GL.enum'Size;

  type OffsetTypeEnm is
  (
     GL_BYTE,
     GL_UNSIGNED_BYTE,
     GL_SHORT,
     GL_UNSIGNED_SHORT,
     GL_INT,
     GL_UNSIGNED_INT,
     GL_FLOAT,
     GL_2_BYTES,
     GL_3_BYTES,
     GL_4_BYTES
  );
  for OffsetTypeEnm use
  (
     GL_BYTE                                 => 16#1400#,
     GL_UNSIGNED_BYTE                        => 16#1401#,
     GL_SHORT                                => 16#1402#,
     GL_UNSIGNED_SHORT                       => 16#1403#,
     GL_INT                                  => 16#1404#,
     GL_UNSIGNED_INT                         => 16#1405#,
     GL_FLOAT                                => 16#1406#,
     GL_2_BYTES                              => 16#1407#,
     GL_3_BYTES                              => 16#1408#,
     GL_4_BYTES                              => 16#1409#
  );
  for OffsetTypeEnm'Size use GL.enum'Size;

  function IsList (list : GL.Uint) return GL_Boolean;

  procedure DeleteLists (list    : GL.Uint;
                         c_range : GL.Sizei);

  function GenLists (c_range : GL.Sizei) return GL.Uint;

  procedure NewList (list : GL.Uint;
                     mode : ListModeEnm);

  procedure EndList;

  procedure CallList (list : GL.Uint);

  procedure CallLists (n      : GL.Sizei;
                       c_type : OffsetTypeEnm;
                       lists  : GL.pointer);

  procedure ListBase (base : GL.Uint);

  --  Object definition
  type ObjectTypeEnm is
  (
     POINTS,
     LINES,
     LINE_LOOP,
     LINE_STRIP,
     TRIANGLES,
     TRIANGLE_STRIP,
     TRIANGLE_FAN,
     QUADS,
     QUAD_STRIP,
     POLYGON
  );
  for ObjectTypeEnm use
  (
     POINTS                                  => 16#0000#,
     LINES                                   => 16#0001#,
     LINE_LOOP                               => 16#0002#,
     LINE_STRIP                              => 16#0003#,
     TRIANGLES                               => 16#0004#,
     TRIANGLE_STRIP                          => 16#0005#,
     TRIANGLE_FAN                            => 16#0006#,
     QUADS                                   => 16#0007#,
     QUAD_STRIP                              => 16#0008#,
     POLYGON                                 => 16#0009#
  );
  for ObjectTypeEnm'Size use GL.enum'Size;

  procedure GL_Begin (mode : ObjectTypeEnm);

  procedure GL_End;

  --  Vertex arrays and related
  type VertexTypeEnm is
  (
     GL_SHORT,
     GL_INT,
     GL_FLOAT,
     GL_DOUBLE
  );
  for VertexTypeEnm use
  (
     GL_SHORT                                   => 16#1402#,
     GL_INT                                     => 16#1404#,
     GL_FLOAT                                   => 16#1406#,
     GL_DOUBLE                                  => 16#140A#
  );
  for VertexTypeEnm'Size use GL.enum'Size;

  type NormalTypeEnm is
  (
     GL_BYTE,
     GL_SHORT,
     GL_INT,
     GL_FLOAT,
     GL_DOUBLE
  );
  for NormalTypeEnm use
  (
     GL_BYTE                                    => 16#1400#,
     GL_SHORT                                   => 16#1402#,
     GL_INT                                     => 16#1404#,
     GL_FLOAT                                   => 16#1406#,
     GL_DOUBLE                                  => 16#140A#
  );
  for NormalTypeEnm'Size use GL.enum'Size;

  type ColorTypeEnm is
  (
     GL_BYTE,
     GL_UNSIGNED_BYTE,
     GL_SHORT,
     GL_UNSIGNED_SHORT,
     GL_INT,
     GL_UNSIGNED_INT,
     GL_FLOAT,
     GL_DOUBLE
  );
  for ColorTypeEnm use
  (
     GL_BYTE                                    => 16#1400#,
     GL_UNSIGNED_BYTE                           => 16#1401#,
     GL_SHORT                                   => 16#1402#,
     GL_UNSIGNED_SHORT                          => 16#1403#,
     GL_INT                                     => 16#1404#,
     GL_UNSIGNED_INT                            => 16#1405#,
     GL_FLOAT                                   => 16#1406#,
     GL_DOUBLE                                  => 16#140A#
  );
  for ColorTypeEnm'Size use GL.enum'Size;

  type IndexTypeEnm is
  (
     GL_UNSIGNED_BYTE,
     GL_SHORT,
     GL_INT,
     GL_FLOAT,
     GL_DOUBLE
  );
  for IndexTypeEnm use
  (
     GL_UNSIGNED_BYTE                           => 16#1401#,
     GL_SHORT                                   => 16#1402#,
     GL_INT                                     => 16#1404#,
     GL_FLOAT                                   => 16#1406#,
     GL_DOUBLE                                  => 16#140A#
  );
  for IndexTypeEnm'Size use GL.enum'Size;

  type TexCoordTypeEnm is
  (
     GL_SHORT,
     GL_INT,
     GL_FLOAT,
     GL_DOUBLE
  );
  for TexCoordTypeEnm use
  (
     GL_SHORT                                   => 16#1402#,
     GL_INT                                     => 16#1404#,
     GL_FLOAT                                   => 16#1406#,
     GL_DOUBLE                                  => 16#140A#
  );
  for TexCoordTypeEnm'Size use GL.enum'Size;

  type ArrayIndexTypeEnm is
  (
     UNSIGNED_BYTE,
     UNSIGNED_SHORT,
     UNSIGNED_INT
  );
  for ArrayIndexTypeEnm use
  (
     UNSIGNED_BYTE                           => 16#1401#,
     UNSIGNED_SHORT                          => 16#1403#,
     UNSIGNED_INT                            => 16#1405#
  );
  for ArrayIndexTypeEnm'Size use GL.enum'Size;

  type InterleaveFormatEnm is
  (
     V2F,
     V3F,
     C4UB_V2F,
     C4UB_V3F,
     C3F_V3F,
     N3F_V3F,
     C4F_N3F_V3F,
     T2F_V3F,
     T4F_V4F,
     T2F_C4UB_V3F,
     T2F_C3F_V3F,
     T2F_N3F_V3F,
     T2F_C4F_N3F_V3F,
     T4F_C4F_N3F_V4F
  );
  for InterleaveFormatEnm use
  (
     V2F                                     => 16#2A20#,
     V3F                                     => 16#2A21#,
     C4UB_V2F                                => 16#2A22#,
     C4UB_V3F                                => 16#2A23#,
     C3F_V3F                                 => 16#2A24#,
     N3F_V3F                                 => 16#2A25#,
     C4F_N3F_V3F                             => 16#2A26#,
     T2F_V3F                                 => 16#2A27#,
     T4F_V4F                                 => 16#2A28#,
     T2F_C4UB_V3F                            => 16#2A29#,
     T2F_C3F_V3F                             => 16#2A2A#,
     T2F_N3F_V3F                             => 16#2A2B#,
     T2F_C4F_N3F_V3F                         => 16#2A2C#,
     T4F_C4F_N3F_V4F                         => 16#2A2D#
  );
  for InterleaveFormatEnm'Size use GL.enum'Size;

  procedure VertexPointer (size   : GL.Int;
                           c_type : VertexTypeEnm;
                           stride : GL.Sizei;
                           ptr    : GL.pointer);

  procedure NormalPointer (c_type : NormalTypeEnm;
                           stride : GL.Sizei;
                           ptr    : GL.pointer);

  procedure ColorPointer (size   : GL.Int;
                          c_type : ColorTypeEnm;
                          stride : GL.Sizei;
                          ptr    : GL.pointer);

  procedure IndexPointer (c_type : IndexTypeEnm;
                          stride : GL.Sizei;
                          ptr    : GL.pointer);

  procedure TexCoordPointer (size   : GL.Int;
                             c_type : TexCoordTypeEnm;
                             stride : GL.Sizei;
                             ptr    : GL.pointer);

  procedure EdgeFlagPointer (stride : GL.Sizei;
                             ptr    : GL_BooleanPtr);

  procedure ArrayElement (i : GL.Int);

  procedure DrawArrays (mode  : ObjectTypeEnm;
                        first : GL.Int;
                        count : GL.Sizei);

  procedure DrawElements (mode    : ObjectTypeEnm;
                          count   : GL.Sizei;
                          c_type  : ArrayIndexTypeEnm;
                          indices : GL.pointer);

  procedure InterleavedArrays (format  : InterleaveFormatEnm;
                               stride  : GL.Sizei;
                               ptr     : GL.pointer);

  --  Shading model
  type ShadeModeEnm is
  (
     FLAT,
     SMOOTH
  );
  for ShadeModeEnm use
  (
     FLAT                                    => 16#1D00#,
     SMOOTH                                  => 16#1D01#
  );
  for ShadeModeEnm'Size use GL.enum'Size;

  procedure ShadeModel (mode : ShadeModeEnm);

  --  Lighting
  type LightIDEnm is
  (
     LIGHT0,
     LIGHT1,
     LIGHT2,
     LIGHT3,
     LIGHT4,
     LIGHT5,
     LIGHT6,
     LIGHT7
  );
  for LightIDEnm use
  (
     LIGHT0                                  => 16#4000#,
     LIGHT1                                  => 16#4001#,
     LIGHT2                                  => 16#4002#,
     LIGHT3                                  => 16#4003#,
     LIGHT4                                  => 16#4004#,
     LIGHT5                                  => 16#4005#,
     LIGHT6                                  => 16#4006#,
     LIGHT7                                  => 16#4007#
  );
  for LightIDEnm'Size use GL.enum'Size;

  type LightParameterEnm is
  (
     SPOT_EXPONENT,
     SPOT_CUTOFF,
     CONSTANT_ATTENUATION,
     LINEAR_ATTENUATION,
     QUADRATIC_ATTENUATION
  );
  for LightParameterEnm use
  (
     SPOT_EXPONENT                           => 16#1205#,
     SPOT_CUTOFF                             => 16#1206#,
     CONSTANT_ATTENUATION                    => 16#1207#,
     LINEAR_ATTENUATION                      => 16#1208#,
     QUADRATIC_ATTENUATION                   => 16#1209#
  );
  for LightParameterEnm'Size use GL.enum'Size;

  type LightParameterVEnm is
  (
     AMBIENT,
     DIFFUSE,
     SPECULAR,
     POSITION,
     SPOT_DIRECTION,
     SPOT_EXPONENT,
     SPOT_CUTOFF,
     CONSTANT_ATTENUATION,
     LINEAR_ATTENUATION,
     QUADRATIC_ATTENUATION
  );
  for LightParameterVEnm use
  (
     AMBIENT                                 => 16#1200#,
     DIFFUSE                                 => 16#1201#,
     SPECULAR                                => 16#1202#,
     POSITION                                => 16#1203#,
     SPOT_DIRECTION                          => 16#1204#,
     SPOT_EXPONENT                           => 16#1205#,
     SPOT_CUTOFF                             => 16#1206#,
     CONSTANT_ATTENUATION                    => 16#1207#,
     LINEAR_ATTENUATION                      => 16#1208#,
     QUADRATIC_ATTENUATION                   => 16#1209#
  );
  for LightParameterVEnm'Size use GL.enum'Size;

  type LightModelEnm is
  (
     LIGHT_MODEL_LOCAL_VIEWER,
     LIGHT_MODEL_TWO_SIDE
  );
  for LightModelEnm use
  (
     LIGHT_MODEL_LOCAL_VIEWER                => 16#0B51#,
     LIGHT_MODEL_TWO_SIDE                    => 16#0B52#
  );
  for LightModelEnm'Size use GL.enum'Size;

  type LightModelVEnm is
  (
     LIGHT_MODEL_LOCAL_VIEWER,
     LIGHT_MODEL_TWO_SIDE,
     LIGHT_MODEL_AMBIENT
  );
  for LightModelVEnm use
  (
     LIGHT_MODEL_LOCAL_VIEWER                => 16#0B51#,
     LIGHT_MODEL_TWO_SIDE                    => 16#0B52#,
     LIGHT_MODEL_AMBIENT                     => 16#0B53#
  );
  for LightModelVEnm'Size use GL.enum'Size;

  procedure Light (light_id : LightIDEnm;
                   pname    : LightParameterEnm;
                   param    : GL.Float);

  procedure Lighti (light_id : LightIDEnm;
                    pname    : LightParameterEnm;
                    param    : GL.Int);

  procedure Light (light_id  : LightIDEnm;
                   pname     : LightParameterVEnm;
                   params    : Light_Float_Vector);

  procedure Lightiv (light_id  : LightIDEnm;
                     pname     : LightParameterVEnm;
                     params    : GL.intPointer);

  procedure GetLightfv (light_id  : LightIDEnm;
                        pname     : LightParameterVEnm;
                        params    : floatPtr);

  procedure GetLightiv (light_id  : LightIDEnm;
                        pname     : LightParameterVEnm;
                        params    : GL.intPointer);

  procedure LightModelf (pname : LightModelEnm;
                         param : GL.Float);

  procedure LightModeli (pname : LightModelEnm;
                         param : GL.Int);

  procedure LightModelfv (pname  : LightModelVEnm;
                          params : floatPtr);

  procedure LightModeliv (pname  : LightModelVEnm;
                          params : GL.intPointer);

  --  Materials
  type MaterialParameterEnm is
  (
     SHININESS
  );
  for MaterialParameterEnm use
  (
     SHININESS                               => 16#1601#
  );
  for MaterialParameterEnm'Size use GL.enum'Size;

  type MaterialParameterVEnm is
  (
     AMBIENT,
     DIFFUSE,
     SPECULAR,
     EMISSION,
     SHININESS,
     AMBIENT_AND_DIFFUSE,
     COLOR_INDEXES
  );
  for MaterialParameterVEnm use
  (
     AMBIENT                                 => 16#1200#,
     DIFFUSE                                 => 16#1201#,
     SPECULAR                                => 16#1202#,
     EMISSION                                => 16#1600#,
     SHININESS                               => 16#1601#,
     AMBIENT_AND_DIFFUSE                     => 16#1602#,
     COLOR_INDEXES                           => 16#1603#
  );
  for MaterialParameterVEnm'Size use GL.enum'Size;

  type GetMaterialParameterEnm is
  (
     AMBIENT,
     DIFFUSE,
     SPECULAR,
     EMISSION,
     SHININESS,
     COLOR_INDEXES
  );
  for GetMaterialParameterEnm use
  (
     AMBIENT                                 => 16#1200#,
     DIFFUSE                                 => 16#1201#,
     SPECULAR                                => 16#1202#,
     EMISSION                                => 16#1600#,
     SHININESS                               => 16#1601#,
     COLOR_INDEXES                           => 16#1603#
  );
  for GetMaterialParameterEnm'Size use GL.enum'Size;

  type ColorMaterialEnm is
  (
     AMBIENT,
     DIFFUSE,
     SPECULAR,
     EMISSION,
     AMBIENT_AND_DIFFUSE
  );
  for ColorMaterialEnm use
  (
     AMBIENT                                 => 16#1200#,
     DIFFUSE                                 => 16#1201#,
     SPECULAR                                => 16#1202#,
     EMISSION                                => 16#1600#,
     AMBIENT_AND_DIFFUSE                     => 16#1602#
  );
  for ColorMaterialEnm'Size use GL.enum'Size;

  procedure Material (face  : FaceEnm;
                      pname : MaterialParameterEnm;
                      param : GL.Float);

  procedure Materiali (face  : FaceEnm;
                       pname : MaterialParameterEnm;
                       param : GL.Int);

  procedure Material (face   : FaceEnm;
                      pname  : MaterialParameterVEnm;
                      params : Material_Float_Vector);

  procedure Materialiv (face   : FaceEnm;
                        pname  : MaterialParameterVEnm;
                        params : GL.intPointer);

  procedure GetMaterialfv (face   : FaceEnm;
                           pname  : GetMaterialParameterEnm;
                           params : floatPtr);

  procedure GetMaterialiv (face   : FaceEnm;
                           pname  : GetMaterialParameterEnm;
                           params : GL.intPointer);

  procedure ColorMaterial (face : FaceEnm;
                           mode : ColorMaterialEnm);

  --  Pixel stuff
  type PixelStorageEnm is
  (
     UNPACK_SWAP_BYTES,
     UNPACK_LSB_FIRST,
     UNPACK_ROW_LENGTH,
     UNPACK_SKIP_ROWS,
     UNPACK_SKIP_PIXELS,
     UNPACK_ALIGNMENT,
     PACK_SWAP_BYTES,
     PACK_LSB_FIRST,
     PACK_ROW_LENGTH,
     PACK_SKIP_ROWS,
     PACK_SKIP_PIXELS,
     PACK_ALIGNMENT
  );
  for PixelStorageEnm use
  (
     UNPACK_SWAP_BYTES                       => 16#0CF0#,
     UNPACK_LSB_FIRST                        => 16#0CF1#,
     UNPACK_ROW_LENGTH                       => 16#0CF2#,
     UNPACK_SKIP_ROWS                        => 16#0CF3#,
     UNPACK_SKIP_PIXELS                      => 16#0CF4#,
     UNPACK_ALIGNMENT                        => 16#0CF5#,
     PACK_SWAP_BYTES                         => 16#0D00#,
     PACK_LSB_FIRST                          => 16#0D01#,
     PACK_ROW_LENGTH                         => 16#0D02#,
     PACK_SKIP_ROWS                          => 16#0D03#,
     PACK_SKIP_PIXELS                        => 16#0D04#,
     PACK_ALIGNMENT                          => 16#0D05#
  );
  for PixelStorageEnm'Size use GL.enum'Size;

  type PixelTransferEnm is
  (
     MAP_COLOR,
     MAP_STENCIL,
     INDEX_SHIFT,
     INDEX_OFFSET,
     RED_SCALE,
     RED_BIAS,
     GREEN_SCALE,
     GREEN_BIAS,
     BLUE_SCALE,
     BLUE_BIAS,
     ALPHA_SCALE,
     ALPHA_BIAS,
     DEPTH_SCALE,
     DEPTH_BIAS
  );
  for PixelTransferEnm use
  (
     MAP_COLOR                               => 16#0D10#,
     MAP_STENCIL                             => 16#0D11#,
     INDEX_SHIFT                             => 16#0D12#,
     INDEX_OFFSET                            => 16#0D13#,
     RED_SCALE                               => 16#0D14#,
     RED_BIAS                                => 16#0D15#,
     GREEN_SCALE                             => 16#0D18#,
     GREEN_BIAS                              => 16#0D19#,
     BLUE_SCALE                              => 16#0D1A#,
     BLUE_BIAS                               => 16#0D1B#,
     ALPHA_SCALE                             => 16#0D1C#,
     ALPHA_BIAS                              => 16#0D1D#,
     DEPTH_SCALE                             => 16#0D1E#,
     DEPTH_BIAS                              => 16#0D1F#
  );
  for PixelTransferEnm'Size use GL.enum'Size;

  type PixelMapEnm is
  (
     PIXEL_MAP_I_TO_I,
     PIXEL_MAP_S_TO_S,
     PIXEL_MAP_I_TO_R,
     PIXEL_MAP_I_TO_G,
     PIXEL_MAP_I_TO_B,
     PIXEL_MAP_I_TO_A,
     PIXEL_MAP_R_TO_R,
     PIXEL_MAP_G_TO_G,
     PIXEL_MAP_B_TO_B,
     PIXEL_MAP_A_TO_A
  );
  for PixelMapEnm use
  (
     PIXEL_MAP_I_TO_I                        => 16#0C70#,
     PIXEL_MAP_S_TO_S                        => 16#0C71#,
     PIXEL_MAP_I_TO_R                        => 16#0C72#,
     PIXEL_MAP_I_TO_G                        => 16#0C73#,
     PIXEL_MAP_I_TO_B                        => 16#0C74#,
     PIXEL_MAP_I_TO_A                        => 16#0C75#,
     PIXEL_MAP_R_TO_R                        => 16#0C76#,
     PIXEL_MAP_G_TO_G                        => 16#0C77#,
     PIXEL_MAP_B_TO_B                        => 16#0C78#,
     PIXEL_MAP_A_TO_A                        => 16#0C79#
  );
  for PixelMapEnm'Size use GL.enum'Size;

  type PixelFormatEnm is
  (
     COLOR_INDEX,
     STENCIL_INDEX,
     DEPTH_COMPONENT,
     RED,
     GREEN,
     BLUE,
     ALPHA,
     RGB,
     RGBA,
     LUMINANCE,
     LUMINANCE_ALPHA,
     BGR,
     BGRA
  );
  for PixelFormatEnm use
  (
     COLOR_INDEX                             => 16#1900#,
     STENCIL_INDEX                           => 16#1901#,
     DEPTH_COMPONENT                         => 16#1902#,
     RED                                     => 16#1903#,
     GREEN                                   => 16#1904#,
     BLUE                                    => 16#1905#,
     ALPHA                                   => 16#1906#,
     RGB                                     => 16#1907#,
     RGBA                                    => 16#1908#,
     LUMINANCE                               => 16#1909#,
     LUMINANCE_ALPHA                         => 16#190A#,
     BGR                                     => 16#80E0#,
     BGRA                                    => 16#80E1#
  );
  for PixelFormatEnm'Size use GL.enum'Size;

  type PixelDataTypeEnm is
  (
     GL_BYTE,
     GL_UNSIGNED_BYTE,
     GL_SHORT,
     GL_UNSIGNED_SHORT,
     GL_INT,
     GL_UNSIGNED_INT,
     GL_FLOAT,
     GL_BITMAP
  );
  for PixelDataTypeEnm use
  (
     GL_BYTE                                    => 16#1400#,
     GL_UNSIGNED_BYTE                           => 16#1401#,
     GL_SHORT                                   => 16#1402#,
     GL_UNSIGNED_SHORT                          => 16#1403#,
     GL_INT                                     => 16#1404#,
     GL_UNSIGNED_INT                            => 16#1405#,
     GL_FLOAT                                   => 16#1406#,
     GL_BITMAP                                  => 16#1A00#
  );
  for PixelDataTypeEnm'Size use GL.enum'Size;

  type PixelCopyTypeEnm is
  (
     COLOR,
     DEPTH,
     STENCIL
  );
  for PixelCopyTypeEnm use
  (
     COLOR                                   => 16#1800#,
     DEPTH                                   => 16#1801#,
     STENCIL                                 => 16#1802#
  );
  for PixelCopyTypeEnm'Size use GL.enum'Size;

  procedure PixelZoom (xfactor : GL.Float;
                       yfactor : GL.Float);

  procedure PixelStoref (pname : PixelStorageEnm;
                         param : GL.Float);

  procedure PixelStore (pname : PixelStorageEnm;
                        param : GL.Int);

  procedure PixelTransferf (pname : PixelTransferEnm;
                            param : GL.Float);

  procedure PixelTransferi (pname : PixelTransferEnm;
                            param : GL.Int);

  procedure PixelMapfv (map     : PixelMapEnm;
                        mapsize : GL.Int;
                        values  : floatPtr);

  procedure PixelMapuiv (map     : PixelMapEnm;
                         mapsize : GL.Int;
                         values  : GL.uintPtr);

  procedure PixelMapusv (map     : PixelMapEnm;
                         mapsize : GL.Int;
                         values  : ushortPtr);

  procedure GetPixelMapfv (map    : PixelMapEnm;
                           values : floatPtr);

  procedure GetPixelMapuiv (map    : PixelMapEnm;
                            values : GL.uintPtr);

  procedure GetPixelMapusv (map    : PixelMapEnm;
                            values : ushortPtr);

  procedure ReadPixels (x      : GL.Int;
                        y      : GL.Int;
                        width  : GL.Sizei;
                        height : GL.Sizei;
                        format : PixelFormatEnm;
                        c_type : PixelDataTypeEnm;
                        pixels : GL.pointer);

  procedure DrawPixels (width  : GL.Sizei;
                        height : GL.Sizei;
                        format : PixelFormatEnm;
                        c_type : PixelDataTypeEnm;
                        pixels : GL.pointer);

  procedure CopyPixels (x      : GL.Int;
                        y      : GL.Int;
                        width  : GL.Sizei;
                        height : GL.Sizei;
                        c_type : PixelCopyTypeEnm);

  --  Texturing
  type TexCoordEnm is
  (
     S,
     T,
     R,
     Q
  );
  for TexCoordEnm use
  (
     S                                       => 16#2000#,
     T                                       => 16#2001#,
     R                                       => 16#2002#,
     Q                                       => 16#2003#
  );
  for TexCoordEnm'Size use GL.enum'Size;

  type TexParameterEnm is
  (
     TEXTURE_GEN_MODE
  );
  for TexParameterEnm use
  (
     TEXTURE_GEN_MODE                        => 16#2500#
  );
  for TexParameterEnm'Size use GL.enum'Size;

  type TexParameterVEnm is
  (
     TEXTURE_GEN_MODE,
     OBJECT_PLANE,
     EYE_PLANE
  );
  for TexParameterVEnm use
  (
     TEXTURE_GEN_MODE                        => 16#2500#,
     OBJECT_PLANE                            => 16#2501#,
     EYE_PLANE                               => 16#2502#
  );
  for TexParameterVEnm'Size use GL.enum'Size;

  type TexEnvEnm is
  (
     TEXTURE_ENV
  );
  for TexEnvEnm use
  (
     TEXTURE_ENV                             => 16#2300#
  );
  for TexEnvEnm'Size use GL.enum'Size;

  type TexEnvParameterEnm is
  (
     TEXTURE_ENV_MODE
  );
  for TexEnvParameterEnm use
  (
     TEXTURE_ENV_MODE                        => 16#2200#
  );
  for TexEnvParameterEnm'Size use GL.enum'Size;

  type TexEnvParameterVEnm is
  (
     TEXTURE_ENV_MODE,
     TEXTURE_ENV_COLOR
  );
  for TexEnvParameterVEnm use
  (
     TEXTURE_ENV_MODE                        => 16#2200#,
     TEXTURE_ENV_COLOR                       => 16#2201#
  );
  for TexEnvParameterVEnm'Size use GL.enum'Size;

  type TargetTexEnm is
  (
     Texture_1D,
     Texture_2D
  );
  for TargetTexEnm use
  (
     Texture_1D                              => 16#0DE0#,
     Texture_2D                              => 16#0DE1#
  );
  for TargetTexEnm'Size use GL.enum'Size;

  type TexParamEnm is
  (
     TEXTURE_MAG_FILTER,
     TEXTURE_MIN_FILTER,
     TEXTURE_WRAP_S,
     TEXTURE_WRAP_T,
     TEXTURE_PRIORITY
  );
  for TexParamEnm use
  (
     TEXTURE_MAG_FILTER                      => 16#2800#,
     TEXTURE_MIN_FILTER                      => 16#2801#,
     TEXTURE_WRAP_S                          => 16#2802#,
     TEXTURE_WRAP_T                          => 16#2803#,
     TEXTURE_PRIORITY                        => 16#8066#
  );
  for TexParamEnm'Size use GL.enum'Size;

  type TexParamVEnm is
  (
     TEXTURE_BORDER_COLOR,
     TEXTURE_MAG_FILTER,
     TEXTURE_MIN_FILTER,
     TEXTURE_WRAP_S,
     TEXTURE_WRAP_T,
     TEXTURE_PRIORITY
  );
  for TexParamVEnm use
  (
     TEXTURE_BORDER_COLOR                    => 16#1004#,
     TEXTURE_MAG_FILTER                      => 16#2800#,
     TEXTURE_MIN_FILTER                      => 16#2801#,
     TEXTURE_WRAP_S                          => 16#2802#,
     TEXTURE_WRAP_T                          => 16#2803#,
     TEXTURE_PRIORITY                        => 16#8066#
  );
  for TexParamVEnm'Size use GL.enum'Size;

  type GetTexParamEnm is
  (
     TEXTURE_BORDER_COLOR,
     TEXTURE_MAG_FILTER,
     TEXTURE_MIN_FILTER,
     TEXTURE_WRAP_S,
     TEXTURE_WRAP_T,
     TEXTURE_PRIORITY,
     TEXTURE_RESIDENT
  );
  for GetTexParamEnm use
  (
     TEXTURE_BORDER_COLOR                    => 16#1004#,
     TEXTURE_MAG_FILTER                      => 16#2800#,
     TEXTURE_MIN_FILTER                      => 16#2801#,
     TEXTURE_WRAP_S                          => 16#2802#,
     TEXTURE_WRAP_T                          => 16#2803#,
     TEXTURE_PRIORITY                        => 16#8066#,
     TEXTURE_RESIDENT                        => 16#8067#
  );
  for GetTexParamEnm'Size use GL.enum'Size;

  type TargetTexLevelEnm is
  (
     TEXTURE_1D,
     TEXTURE_2D,
     PROXY_TEXTURE_1D,
     PROXY_TEXTURE_2D
  );
  for TargetTexLevelEnm use
  (
     TEXTURE_1D                              => 16#0DE0#,
     TEXTURE_2D                              => 16#0DE1#,
     PROXY_TEXTURE_1D                        => 16#8063#,
     PROXY_TEXTURE_2D                        => 16#8064#
  );
  for TargetTexLevelEnm'Size use GL.enum'Size;

  type TexLevelParameterEnm is
  (
     TEXTURE_WIDTH,
     TEXTURE_HEIGHT,
     TEXTURE_COMPONENTS,
     TEXTURE_BORDER,
     TEXTURE_RED_SIZE,
     TEXTURE_GREEN_SIZE,
     TEXTURE_BLUE_SIZE,
     TEXTURE_ALPHA_SIZE,
     TEXTURE_LUMINANCE_SIZE,
     TEXTURE_INTENSITY_SIZE,
     TEXTURE_INTERNAL_FORMAT
  );
  for TexLevelParameterEnm use
  (
     TEXTURE_WIDTH                           => 16#1000#,
     TEXTURE_HEIGHT                          => 16#1001#,
     TEXTURE_COMPONENTS                      => 16#1003#,
     TEXTURE_BORDER                          => 16#1005#,
     TEXTURE_RED_SIZE                        => 16#805C#,
     TEXTURE_GREEN_SIZE                      => 16#805D#,
     TEXTURE_BLUE_SIZE                       => 16#805E#,
     TEXTURE_ALPHA_SIZE                      => 16#805F#,
     TEXTURE_LUMINANCE_SIZE                  => 16#8060#,
     TEXTURE_INTENSITY_SIZE                  => 16#8061#,
     TEXTURE_INTERNAL_FORMAT                 => 16#FFFF#
  );
  for TexLevelParameterEnm'Size use GL.enum'Size;

  type TargetTex1DEnm is
  (
     TEXTURE_1D,
     PROXY_TEXTURE_1D
  );
  for TargetTex1DEnm use
  (
     TEXTURE_1D                              => 16#0DE0#,
     PROXY_TEXTURE_1D                        => 16#8063#
  );
  for TargetTex1DEnm'Size use GL.enum'Size;

  type TexFormatEnm is
  (
     ALPHA,
     RGB,
     RGBA,
     LUMINANCE,
     LUMINANCE_ALPHA,
     R3_G3_B2,
     ALPHA4,
     ALPHA8,
     ALPHA12,
     ALPHA16,
     LUMINANCE4,
     LUMINANCE8,
     LUMINANCE12,
     LUMINANCE16,
     LUMINANCE4_ALPHA4,
     LUMINANCE6_ALPHA2,
     LUMINANCE8_ALPHA8,
     LUMINANCE12_ALPHA4,
     LUMINANCE12_ALPHA12,
     LUMINANCE16_ALPHA16,
     INTENSITY,
     INTENSITY4,
     INTENSITY8,
     INTENSITY12,
     INTENSITY16,
     RGB4,
     RGB5,
     RGB8,
     RGB10,
     RGB12,
     RGB16,
     RGBA2,
     RGBA4,
     RGB5_A1,
     RGBA8,
     RGB10_A2,
     RGBA12,
     RGBA16,
     BGR,
     BGRA
  );
  for TexFormatEnm use
  (
     ALPHA                                   => 16#1906#,
     RGB                                     => 16#1907#,
     RGBA                                    => 16#1908#,
     LUMINANCE                               => 16#1909#,
     LUMINANCE_ALPHA                         => 16#190A#,
     R3_G3_B2                                => 16#2A10#,
     ALPHA4                                  => 16#803B#,
     ALPHA8                                  => 16#803C#,
     ALPHA12                                 => 16#803D#,
     ALPHA16                                 => 16#803E#,
     LUMINANCE4                              => 16#803F#,
     LUMINANCE8                              => 16#8040#,
     LUMINANCE12                             => 16#8041#,
     LUMINANCE16                             => 16#8042#,
     LUMINANCE4_ALPHA4                       => 16#8043#,
     LUMINANCE6_ALPHA2                       => 16#8044#,
     LUMINANCE8_ALPHA8                       => 16#8045#,
     LUMINANCE12_ALPHA4                      => 16#8046#,
     LUMINANCE12_ALPHA12                     => 16#8047#,
     LUMINANCE16_ALPHA16                     => 16#8048#,
     INTENSITY                               => 16#8049#,
     INTENSITY4                              => 16#804A#,
     INTENSITY8                              => 16#804B#,
     INTENSITY12                             => 16#804C#,
     INTENSITY16                             => 16#804D#,
     RGB4                                    => 16#804F#,
     RGB5                                    => 16#8050#,
     RGB8                                    => 16#8051#,
     RGB10                                   => 16#8052#,
     RGB12                                   => 16#8053#,
     RGB16                                   => 16#8054#,
     RGBA2                                   => 16#8055#,
     RGBA4                                   => 16#8056#,
     RGB5_A1                                 => 16#8057#,
     RGBA8                                   => 16#8058#,
     RGB10_A2                                => 16#8059#,
     RGBA12                                  => 16#805A#,
     RGBA16                                  => 16#805B#,
     BGR                                     => 16#80E0#,
     BGRA                                    => 16#80E1#
  );
  for TexFormatEnm'Size use GL.enum'Size;

  type TexPixelFormatEnm is
  (
     COLOR_INDEX,
     RED,
     GREEN,
     BLUE,
     ALPHA,
     RGB,
     RGBA,
     LUMINANCE,
     LUMINANCE_ALPHA
  );
  for TexPixelFormatEnm use
  (
     COLOR_INDEX                             => 16#1900#,
     RED                                     => 16#1903#,
     GREEN                                   => 16#1904#,
     BLUE                                    => 16#1905#,
     ALPHA                                   => 16#1906#,
     RGB                                     => 16#1907#,
     RGBA                                    => 16#1908#,
     LUMINANCE                               => 16#1909#,
     LUMINANCE_ALPHA                         => 16#190A#
  );
  for TexPixelFormatEnm'Size use GL.enum'Size;

  type TargetTex2DEnm is
  (
     TEXTURE_2D,
     PROXY_TEXTURE_2D
  );
  for TargetTex2DEnm use
  (
     TEXTURE_2D                              => 16#0DE1#,
     PROXY_TEXTURE_2D                        => 16#8064#
  );
  for TargetTex2DEnm'Size use GL.enum'Size;

  type TexImageFormatEnm is
  (
     RED,
     GREEN,
     BLUE,
     ALPHA,
     RGB,
     RGBA,
     LUMINANCE,
     LUMINANCE_ALPHA
  );
  for TexImageFormatEnm use
  (
     RED                                     => 16#1903#,
     GREEN                                   => 16#1904#,
     BLUE                                    => 16#1905#,
     ALPHA                                   => 16#1906#,
     RGB                                     => 16#1907#,
     RGBA                                    => 16#1908#,
     LUMINANCE                               => 16#1909#,
     LUMINANCE_ALPHA                         => 16#190A#
  );
  for TexImageFormatEnm'Size use GL.enum'Size;

  type TargetTex1DOnlyEnm is
  (
     TEXTURE_1D
  );
  for TargetTex1DOnlyEnm use
  (
     TEXTURE_1D                              => 16#0DE0#
  );
  for TargetTex1DOnlyEnm'Size use GL.enum'Size;

  type TargetTex2DOnlyEnm is
  (
     TEXTURE_2D
  );
  for TargetTex2DOnlyEnm use
  (
     TEXTURE_2D                              => 16#0DE1#
  );
  for TargetTex2DOnlyEnm'Size use GL.enum'Size;

  type TargetTex3DEnm is
  (
     TEXTURE_3D_EXT,
     PROXY_TEXTURE_3D_EXT
  );
  for TargetTex3DEnm use
  (
     TEXTURE_3D_EXT                          => 16#806F#,
     PROXY_TEXTURE_3D_EXT                    => 16#8070#
  );
  for TargetTex3DEnm'Size use GL.enum'Size;

  type TargetTex3DOnlyEnm is
  (
     TEXTURE_3D_EXT
  );
  for TargetTex3DOnlyEnm use
  (
     TEXTURE_3D_EXT                          => 16#806F#
  );
  for TargetTex3DOnlyEnm'Size use GL.enum'Size;

  --  Texture map parameters
  OBJECT_LINEAR               : constant := 16#2401#;
  EYE_LINEAR                  : constant := 16#2400#;
  SPHERE_MAP                  : constant := 16#2402#;

  --  Texture filter parameter values
  NEAREST_MIPMAP_NEAREST      : constant := 16#2700#;
  NEAREST_MIPMAP_LINEAR       : constant := 16#2702#;
  LINEAR_MIPMAP_NEAREST       : constant := 16#2701#;
  LINEAR_MIPMAP_LINEAR        : constant := 16#2703#;
  DECAL                       : constant := 16#2101#;
  MODULATE                    : constant := 16#2100#;
  NEAREST                     : constant := 16#2600#;
  REPEAT                      : constant := 16#2901#;
  CLAMP                       : constant := 16#2900#;
  CLAMP_TO_EDGE               : constant := 16#812F#;
  CLAMP_TO_BORDER             : constant := 16#812D#;

  procedure TexGend (coord : TexCoordEnm;
                     pname : TexParameterEnm;
                     param : GL.Double);

  procedure TexGenf (coord : TexCoordEnm;
                     pname : TexParameterEnm;
                     param : GL.Float);

  procedure TexGeni (coord : TexCoordEnm;
                     pname : TexParameterEnm;
                     param : GL.Int);

  procedure TexGendv (coord  : TexCoordEnm;
                      pname  : TexParameterVEnm;
                      params : GL.doublePtr);

  procedure TexGenfv (coord  : TexCoordEnm;
                      pname  : TexParameterVEnm;
                      params : floatPtr);

  procedure TexGeniv (coord  : TexCoordEnm;
                      pname  : TexParameterVEnm;
                      params : GL.intPointer);

  procedure GetTexGendv (coord  : TexCoordEnm;
                         pname  : TexParameterVEnm;
                         params : GL.doublePtr);

  procedure GetTexGenfv (coord  : TexCoordEnm;
                         pname  : TexParameterVEnm;
                         params : floatPtr);

  procedure GetTexGeniv (coord  : TexCoordEnm;
                         pname  : TexParameterVEnm;
                         params : GL.intPointer);

  procedure TexEnvf (target : TexEnvEnm;
                     pname  : TexEnvParameterEnm;
                     param  : GL.Float);

  procedure TexEnv (target : TexEnvEnm;
                    pname  : TexEnvParameterEnm;
                    param  : GL.Int);

  procedure TexEnvfv (target : TexEnvEnm;
                      pname  : TexEnvParameterVEnm;
                      params : floatPtr);

  procedure TexEnviv (target : TexEnvEnm;
                      pname  : TexEnvParameterVEnm;
                      params : GL.intPointer);

  procedure GetTexEnvfv (target : TexEnvEnm;
                         pname  : TexEnvParameterVEnm;
                         params : floatPtr);

  procedure GetTexEnviv (target : TexEnvEnm;
                         pname  : TexEnvParameterVEnm;
                         params : GL.intPointer);

  procedure TexParameterf (target : TargetTexEnm;
                           pname  : TexParamEnm;
                           param  : GL.Float);

  procedure TexParameter (target : TargetTexEnm;
                          pname  : TexParamEnm;
                          param  : GL.Int);

  procedure TexParameterfv (target : TargetTexEnm;
                            pname  : TexParamVEnm;
                            params : floatPtr);

  procedure TexParameteriv (target : TargetTexEnm;
                            pname  : TexParamVEnm;
                            params : GL.intPointer);

  procedure GetTexParameterfv (target : TargetTexEnm;
                               pname  : GetTexParamEnm;
                               params : floatPtr);

  procedure GetTexParameteriv (target : TargetTexEnm;
                               pname  : GetTexParamEnm;
                               params : GL.intPointer);

  procedure GetTexLevelParameterfv (target : TargetTexLevelEnm;
                                    level  : GL.Int;
                                    pname  : TexLevelParameterEnm;
                                    params : floatPtr);

  procedure GetTexLevelParameteriv (target : TargetTexLevelEnm;
                                    level  : GL.Int;
                                    pname  : TexLevelParameterEnm;
                                    params : GL.intPointer);

  procedure TexImage1D (target         : TargetTex1DEnm;
                        level          : GL.Int;
                        internalFormat : TexFormatEnm;
                        width          : GL.Sizei;
                        border         : GL.Int;
                        format         : TexPixelFormatEnm;
                        c_type         : PixelDataTypeEnm;
                        pixels         : GL.pointer);

  procedure TexImage2D (target         : TargetTex2DEnm;
                        level          : GL.Int;
                        internalFormat : TexFormatEnm;
                        width          : GL.Sizei;
                        height         : GL.Sizei;
                        border         : GL.Int;
                        format         : TexPixelFormatEnm;
                        c_type         : PixelDataTypeEnm;
                        pixels         : GL.pointer);

  procedure GetTexImage (target : TargetTexEnm;
                         level  : GL.Int;
                         format : TexImageFormatEnm;
                         c_type : PixelDataTypeEnm;
                         pixels : GL.pointer);

  procedure GenTextures (n        : GL.Sizei;
                         textures : GL.uintPtr);

  procedure DeleteTextures (n        : GL.Sizei;
                            textures : GL.uintPtr);

  procedure BindTexture (target     : TargetTexEnm;
                         texture_id : GL.Uint);

  procedure PrioritizeTextures (n          : GL.Sizei;
                                textures   : GL.uintPtr;
                                priorities : GL.clampfPtr);

  function AreTexturesResident (n          : GL.Sizei;
                                textures   : GL.uintPtr;
                                residences : GL_BooleanPtr)
  return GL_Boolean;

  function IsTexture (texture_id : GL.Uint)
  return GL_Boolean;

  procedure TexSubImage1D (target  : TargetTex1DOnlyEnm;
                           level   : GL.Int;
                           xoffset : GL.Int;
                           width   : GL.Sizei;
                           format  : TexPixelFormatEnm;
                           c_type  : PixelDataTypeEnm;
                           pixels  : GL.pointer);

  procedure TexSubImage2D (target  : TargetTex2DOnlyEnm;
                           level   : GL.Int;
                           xoffset : GL.Int;
                           yoffset : GL.Int;
                           width   : GL.Sizei;
                           height  : GL.Sizei;
                           format  : TexPixelFormatEnm;
                           c_type  : PixelDataTypeEnm;
                           pixels  : GL.pointer);

  procedure CopyTexImage1D (target         : TargetTex1DOnlyEnm;
                            level          : GL.Int;
                            internalformat : TexFormatEnm;
                            x              : GL.Int;
                            y              : GL.Int;
                            width          : GL.Sizei;
                            border         : GL.Int);

  procedure CopyTexImage2D (target         : TargetTex2DOnlyEnm;
                            level          : GL.Int;
                            internalformat : TexFormatEnm;
                            x              : GL.Int;
                            y              : GL.Int;
                            width          : GL.Sizei;
                            height         : GL.Sizei;
                            border         : GL.Int);

  procedure CopyTexSubImage1D (target  : TargetTex1DOnlyEnm;
                               level   : GL.Int;
                               xoffset : GL.Int;
                               x       : GL.Int;
                               y       : GL.Int;
                               width   : GL.Sizei);

  procedure CopyTexSubImage2D (target  : TargetTex2DOnlyEnm;
                               level   : GL.Int;
                               xoffset : GL.Int;
                               yoffset : GL.Int;
                               x       : GL.Int;
                               y       : GL.Int;
                               width   : GL.Sizei;
                               height  : GL.Sizei);

  procedure TexImage3DEXT (target         : TargetTex3DEnm;
                           level          : GL.Int;
                           internalFormat : TexPixelFormatEnm;
                           width          : GL.Sizei;
                           height         : GL.Sizei;
                           depth_value    : GL.Sizei;
                           border         : GL.Int;
                           format         : TexPixelFormatEnm;
                           c_type         : PixelDataTypeEnm;
                           pixels         : GL.pointer);

  procedure TexSubImage3DEXT (target      : TargetTex3DOnlyEnm;
                              level       : GL.Int;
                              xoffset     : GL.Int;
                              yoffset     : GL.Int;
                              zoffset     : GL.Int;
                              width       : GL.Sizei;
                              height      : GL.Sizei;
                              depth_value : GL.Sizei;
                              format      : TexPixelFormatEnm;
                              c_type      : PixelDataTypeEnm;
                              pixels      : GL.pointer);

  procedure CopyTexSubImage3DEXT (target  : TargetTex3DOnlyEnm;
                                  level   : GL.Int;
                                  xoffset : GL.Int;
                                  yoffset : GL.Int;
                                  zoffset : GL.Int;
                                  x       : GL.Int;
                                  y       : GL.Int;
                                  width   : GL.Sizei;
                                  height  : GL.Sizei);

  --  Evaluators
  type Map1TargetEnm is
  (
     MAP1_COLOR_4,
     MAP1_INDEX,
     MAP1_NORMAL,
     MAP1_TEXTURE_COORD_1,
     MAP1_TEXTURE_COORD_2,
     MAP1_TEXTURE_COORD_3,
     MAP1_TEXTURE_COORD_4,
     MAP1_VERTEX_3,
     MAP1_VERTEX_4
  );
  for Map1TargetEnm use
  (
     MAP1_COLOR_4                            => 16#0D90#,
     MAP1_INDEX                              => 16#0D91#,
     MAP1_NORMAL                             => 16#0D92#,
     MAP1_TEXTURE_COORD_1                    => 16#0D93#,
     MAP1_TEXTURE_COORD_2                    => 16#0D94#,
     MAP1_TEXTURE_COORD_3                    => 16#0D95#,
     MAP1_TEXTURE_COORD_4                    => 16#0D96#,
     MAP1_VERTEX_3                           => 16#0D97#,
     MAP1_VERTEX_4                           => 16#0D98#
  );
  for Map1TargetEnm'Size use GL.enum'Size;

  type Map2TargetEnm is
  (
     MAP2_COLOR_4,
     MAP2_INDEX,
     MAP2_NORMAL,
     MAP2_TEXTURE_COORD_1,
     MAP2_TEXTURE_COORD_2,
     MAP2_TEXTURE_COORD_3,
     MAP2_TEXTURE_COORD_4,
     MAP2_VERTEX_3,
     MAP2_VERTEX_4
  );
  for Map2TargetEnm use
  (
     MAP2_COLOR_4                            => 16#0DB0#,
     MAP2_INDEX                              => 16#0DB1#,
     MAP2_NORMAL                             => 16#0DB2#,
     MAP2_TEXTURE_COORD_1                    => 16#0DB3#,
     MAP2_TEXTURE_COORD_2                    => 16#0DB4#,
     MAP2_TEXTURE_COORD_3                    => 16#0DB5#,
     MAP2_TEXTURE_COORD_4                    => 16#0DB6#,
     MAP2_VERTEX_3                           => 16#0DB7#,
     MAP2_VERTEX_4                           => 16#0DB8#
  );
  for Map2TargetEnm'Size use GL.enum'Size;

  type MapTargetEnm is
  (
     MAP1_COLOR_4,
     MAP1_INDEX,
     MAP1_NORMAL,
     MAP1_TEXTURE_COORD_1,
     MAP1_TEXTURE_COORD_2,
     MAP1_TEXTURE_COORD_3,
     MAP1_TEXTURE_COORD_4,
     MAP1_VERTEX_3,
     MAP1_VERTEX_4,
     MAP2_COLOR_4,
     MAP2_INDEX,
     MAP2_NORMAL,
     MAP2_TEXTURE_COORD_1,
     MAP2_TEXTURE_COORD_2,
     MAP2_TEXTURE_COORD_3,
     MAP2_TEXTURE_COORD_4,
     MAP2_VERTEX_3,
     MAP2_VERTEX_4
  );
  for MapTargetEnm use
  (
     MAP1_COLOR_4                            => 16#0D90#,
     MAP1_INDEX                              => 16#0D91#,
     MAP1_NORMAL                             => 16#0D92#,
     MAP1_TEXTURE_COORD_1                    => 16#0D93#,
     MAP1_TEXTURE_COORD_2                    => 16#0D94#,
     MAP1_TEXTURE_COORD_3                    => 16#0D95#,
     MAP1_TEXTURE_COORD_4                    => 16#0D96#,
     MAP1_VERTEX_3                           => 16#0D97#,
     MAP1_VERTEX_4                           => 16#0D98#,
     MAP2_COLOR_4                            => 16#0DB0#,
     MAP2_INDEX                              => 16#0DB1#,
     MAP2_NORMAL                             => 16#0DB2#,
     MAP2_TEXTURE_COORD_1                    => 16#0DB3#,
     MAP2_TEXTURE_COORD_2                    => 16#0DB4#,
     MAP2_TEXTURE_COORD_3                    => 16#0DB5#,
     MAP2_TEXTURE_COORD_4                    => 16#0DB6#,
     MAP2_VERTEX_3                           => 16#0DB7#,
     MAP2_VERTEX_4                           => 16#0DB8#
  );
  for MapTargetEnm'Size use GL.enum'Size;

  type MapQueryEnm is
  (
     COEFF,
     ORDER,
     DOMAIN
  );
  for MapQueryEnm use
  (
     COEFF                                   => 16#0A00#,
     ORDER                                   => 16#0A01#,
     DOMAIN                                  => 16#0A02#
  );
  for MapQueryEnm'Size use GL.enum'Size;

  type Mesh1ModeEnm is
  (
     POINT,
     LINE
  );
  for Mesh1ModeEnm use
  (
     POINT                                   => 16#1B00#,
     LINE                                    => 16#1B01#
  );
  for Mesh1ModeEnm'Size use GL.enum'Size;

  type Mesh2ModeEnm is
  (
     POINT,
     LINE,
     FILL
  );
  for Mesh2ModeEnm use
  (
     POINT                                   => 16#1B00#,
     LINE                                    => 16#1B01#,
     FILL                                    => 16#1B02#
  );
  for Mesh2ModeEnm'Size use GL.enum'Size;

  procedure Map1d (target      : Map1TargetEnm;
                   u1          : GL.Double;
                   u2          : GL.Double;
                   stride      : GL.Int;
                   order_value : GL.Int;
                   point_list  : GL.doublePtr);

  procedure Map1f (target      : Map1TargetEnm;
                   u1          : GL.Float;
                   u2          : GL.Float;
                   stride      : GL.Int;
                   order_value : GL.Int;
                   point_list  : floatPtr);

  procedure Map2d (target     : Map2TargetEnm;
                   u1         : GL.Double;
                   u2         : GL.Double;
                   ustride    : GL.Int;
                   uorder     : GL.Int;
                   v1         : GL.Double;
                   v2         : GL.Double;
                   vstride    : GL.Int;
                   vorder     : GL.Int;
                   point_list : GL.doublePtr);

  procedure Map2f (target     : Map2TargetEnm;
                   u1         : GL.Float;
                   u2         : GL.Float;
                   ustride    : GL.Int;
                   uorder     : GL.Int;
                   v1         : GL.Float;
                   v2         : GL.Float;
                   vstride    : GL.Int;
                   vorder     : GL.Int;
                   point_list : floatPtr);

  procedure GetMapdv (target : MapTargetEnm;
                      query  : MapQueryEnm;
                      v      : GL.doublePtr);

  procedure GetMapfv (target : MapTargetEnm;
                      query  : MapQueryEnm;
                      v      : floatPtr);

  procedure GetMapiv (target : MapTargetEnm;
                      query  : MapQueryEnm;
                      v      : GL.intPointer);

  procedure EvalPoint1 (i : GL.Int);

  procedure EvalPoint2 (i : GL.Int;
                        j : GL.Int);

  procedure EvalMesh1 (mode : Mesh1ModeEnm;
                       i1   : GL.Int;
                       i2   : GL.Int);

  procedure EvalMesh2 (mode : Mesh2ModeEnm;
                       i1   : GL.Int;
                       i2   : GL.Int;
                       j1   : GL.Int;
                       j2   : GL.Int);

  procedure EvalCoord1d  (u : GL.Double);

  procedure EvalCoord1f  (u : GL.Float);

  procedure EvalCoord1dv (u : GL.doublePtr);

  procedure EvalCoord1fv (u : floatPtr);

  procedure EvalCoord2d (u : GL.Double;
                         v : GL.Double);

  procedure EvalCoord2f (u : GL.Float;
                         v : GL.Float);

  procedure EvalCoord2dv (u : GL.doublePtr);

  procedure EvalCoord2fv (u : floatPtr);

  procedure MapGrid1d (un : GL.Int;
                       u1 : GL.Double;
                       u2 : GL.Double);

  procedure MapGrid1f (un : GL.Int;
                       u1 : GL.Float;
                       u2 : GL.Float);

  procedure MapGrid2d (un : GL.Int;
                       u1 : GL.Double;
                       u2 : GL.Double;
                       vn : GL.Int;
                       v1 : GL.Double;
                       v2 : GL.Double);

  procedure MapGrid2f (un : GL.Int;
                       u1 : GL.Float;
                       u2 : GL.Float;
                       vn : GL.Int;
                       v1 : GL.Float;
                       v2 : GL.Float);

  --  Fog
  type FogParameterEnm is
  (
     FOG_INDEX,
     FOG_DENSITY,
     FOG_START,
     FOG_END,
     FOG_MODE
  );
  for FogParameterEnm use
  (
     FOG_INDEX      => 16#0B61#,
     FOG_DENSITY    => 16#0B62#,
     FOG_START      => 16#0B63#,
     FOG_END        => 16#0B64#,
     FOG_MODE       => 16#0B65#
  );
  for FogParameterEnm'Size use GL.enum'Size;

  type FogParameterVEnm is
  (
     FOG_INDEX,
     FOG_DENSITY,
     FOG_START,
     FOG_END,
     FOG_MODE,
     FOG_COLOR
  );
  for FogParameterVEnm use
  (
     FOG_INDEX       => 16#0B61#,
     FOG_DENSITY     => 16#0B62#,
     FOG_START       => 16#0B63#,
     FOG_END         => 16#0B64#,
     FOG_MODE        => 16#0B65#,
     FOG_COLOR       => 16#0B66#
  );
  for FogParameterVEnm'Size use GL.enum'Size;

  --  Fog attenuation modes
  LINEAR : constant := 16#2601#;
  EXP1   : constant := 16#0800#;
  EXP2   : constant := 16#0801#;
  --  EXP1: original was EXP, confused with the Exp function (29-May-2006)

  procedure Fogf (pname : FogParameterEnm;
                  param : GL.Float);
  procedure Fog (pname : FogParameterEnm;
                 param : GL.Float) renames Fogf;

  procedure Fogi (pname : FogParameterEnm;
                  param : GL.Int);
  procedure Fog (pname : FogParameterEnm;
                 param : GL.Int) renames Fogi;

  procedure Fogfv (pname  : FogParameterVEnm;
                   params : floatPtr);
  procedure Fog (pname  : FogParameterVEnm;
                 params : floatPtr) renames Fogfv;

  procedure Fogiv (pname  : FogParameterVEnm;
                   params : GL.intPointer);
  procedure Fog (pname  : FogParameterVEnm;
                 params : GL.intPointer) renames Fogiv;

  --  Feedback
  type FeedbackModeEnm is
  (
     GL_2D,
     GL_3D,
     GL_3D_COLOR,
     GL_3D_COLOR_TEXTURE,
     GL_4D_COLOR_TEXTURE
  );
  for FeedbackModeEnm use
  (
     GL_2D                    => 16#0600#,
     GL_3D                    => 16#0601#,
     GL_3D_COLOR              => 16#0602#,
     GL_3D_COLOR_TEXTURE      => 16#0603#,
     GL_4D_COLOR_TEXTURE      => 16#0604#
  );
  for FeedbackModeEnm'Size use GL.enum'Size;

  --  Feedback tokens
  POINT_TOKEN           : constant := 16#0701#;
  LINE_TOKEN            : constant := 16#0702#;
  LINE_RESET_TOKEN      : constant := 16#0707#;
  POLYGON_TOKEN         : constant := 16#0703#;
  BITMAP_TOKEN          : constant := 16#0704#;
  DRAW_PIXEL_TOKEN      : constant := 16#0705#;
  COPY_PIXEL_TOKEN      : constant := 16#0706#;
  PASS_THROUGH_TOKEN    : constant := 16#0700#;
  FEEDBACK_BUFFER_SIZE  : constant := 16#0DF1#;
  FEEDBACK_BUFFER_TYPE  : constant := 16#0DF2#;

  procedure FeedbackBuffer (size   : GL.Sizei;
                            c_type : FeedbackModeEnm;
                            buffer : floatPtr);

  procedure PassThrough (token : GL.Float);

  --  Color tables (extension)
  type ColorTableTargetEnm is
  (
     TEXTURE_1D,
     TEXTURE_2D,
     PROXY_TEXTURE_1D,
     PROXY_TEXTURE_2D,
     TEXTURE_3D_EXT,
     PROXY_TEXTURE_3D_EXT,
     SHARED_TEXTURE_PALETTE_EXT

  );
  for ColorTableTargetEnm use
  (
     TEXTURE_1D                    => 16#0DE0#,
     TEXTURE_2D                    => 16#0DE1#,
     PROXY_TEXTURE_1D              => 16#8063#,
     PROXY_TEXTURE_2D              => 16#8064#,
     TEXTURE_3D_EXT                => 16#806F#,
     PROXY_TEXTURE_3D_EXT          => 16#8070#,
     SHARED_TEXTURE_PALETTE_EXT    => 16#81FB#
  );
  for ColorTableTargetEnm'Size use GL.enum'Size;

  type GetColorTableTargetEnm is
  (
     TEXTURE_1D,
     TEXTURE_2D,
     TEXTURE_3D_EXT,
     SHARED_TEXTURE_PALETTE_EXT

  );
  for GetColorTableTargetEnm use
  (
     TEXTURE_1D                    => 16#0DE0#,
     TEXTURE_2D                    => 16#0DE1#,
     TEXTURE_3D_EXT                => 16#806F#,
     SHARED_TEXTURE_PALETTE_EXT    => 16#81FB#
  );
  for GetColorTableTargetEnm'Size use GL.enum'Size;

  type ColorTableParameterEnm is
  (
     COLOR_TABLE_FORMAT_EXT,
     COLOR_TABLE_WIDTH_EXT,
     COLOR_TABLE_RED_SIZE_EXT,
     COLOR_TABLE_GREEN_SIZE_EXT,
     COLOR_TABLE_BLUE_SIZE_EXT,
     COLOR_TABLE_ALPHA_SIZE_EXT,
     COLOR_TABLE_LUMINANCE_SIZE_EXT,
     COLOR_TABLE_INTENSITY_SIZE_EXT
  );
  for ColorTableParameterEnm use
  (
     COLOR_TABLE_FORMAT_EXT           => 16#80D8#,
     COLOR_TABLE_WIDTH_EXT            => 16#80D9#,
     COLOR_TABLE_RED_SIZE_EXT         => 16#80DA#,
     COLOR_TABLE_GREEN_SIZE_EXT       => 16#80DB#,
     COLOR_TABLE_BLUE_SIZE_EXT        => 16#80DC#,
     COLOR_TABLE_ALPHA_SIZE_EXT       => 16#80DD#,
     COLOR_TABLE_LUMINANCE_SIZE_EXT   => 16#80DE#,
     COLOR_TABLE_INTENSITY_SIZE_EXT   => 16#80DF#
  );
  for ColorTableParameterEnm'Size use GL.enum'Size;

  procedure ColorTableEXT (target         : ColorTableTargetEnm;
                           internalformat : TexFormatEnm;
                           width          : GL.Sizei;
                           format         : TexPixelFormatEnm;
                           c_type         : PixelDataTypeEnm;
                           table          : GL.pointer);

  procedure ColorSubTableEXT (target : ColorTableTargetEnm;
                              start  : GL.Sizei;
                              count  : GL.Sizei;
                              format : TexPixelFormatEnm;
                              c_type : PixelDataTypeEnm;
                              data   : GL.pointer);

  procedure GetColorTableEXT (target : GetColorTableTargetEnm;
                              format : TexPixelFormatEnm;
                              c_type : PixelDataTypeEnm;
                              table  : GL.pointer);

  procedure GetColorTableParameterfvEXT (target : GetColorTableTargetEnm;
                                         pname  : ColorTableParameterEnm;
                                         params : floatPtr);

  procedure GetColorTableParameterivEXT (target : GetColorTableTargetEnm;
                                         pname  : ColorTableParameterEnm;
                                         params : GL.intPointer);

  --  Point parameters (extension)
  type PointParameterEnm is
  (
     POINT_SIZE_MIN_EXT,
     POINT_SIZE_MAX_EXT,
     POINT_FADE_THRESHOLD_SIZE_EXT
  );
  for PointParameterEnm use
  (
     POINT_SIZE_MIN_EXT             => 16#8126#,
     POINT_SIZE_MAX_EXT             => 16#8127#,
     POINT_FADE_THRESHOLD_SIZE_EXT  => 16#8128#
  );
  for PointParameterEnm'Size use GL.enum'Size;

  type PointParameterVEnm is
  (
     POINT_SIZE_MIN_EXT,
     POINT_SIZE_MAX_EXT,
     POINT_FADE_THRESHOLD_SIZE_EXT,
     DISTANCE_ATTENUATION_EXT
  );
  for PointParameterVEnm use
  (
     POINT_SIZE_MIN_EXT             => 16#8126#,
     POINT_SIZE_MAX_EXT             => 16#8127#,
     POINT_FADE_THRESHOLD_SIZE_EXT  => 16#8128#,
     DISTANCE_ATTENUATION_EXT       => 16#8129#
  );
  for PointParameterVEnm'Size use GL.enum'Size;

  procedure PointParameterfEXT (pname : PointParameterEnm;
                                param : GL.Float);

  procedure PointParameterfvEXT (pname  : PointParameterVEnm;
                                 params : floatPtr);

  --  Clears
  procedure ClearIndex (c : GL.Float);

  procedure ClearColor (red_value   : GL.Clampf;
                        green_value : GL.Clampf;
                        blue_value  : GL.Clampf;
                        alpha_value : GL.Clampf);

  procedure Clear (mask : Bitfield);

  procedure ClearDepth (depth_value : GL.Clampd);

  procedure ClearAccum (red_value   : GL.Float;
                        green_value : GL.Float;
                        blue_value  : GL.Float;
                        alpha_value : GL.Float);

  --  Masks
  procedure IndexMask (mask : GL.Uint);

  procedure ColorMask (red_value   : GL_Boolean;
                       green_value : GL_Boolean;
                       blue_value  : GL_Boolean;
                       alpha_value : GL_Boolean);

  --  Drawing parameters
  procedure PointSize (size : GL.Float);

  procedure LineWidth (width : GL.Float);

  procedure LineStipple (factor  : GL.Int;
                         pattern : GL.Ushort);

  procedure PolygonOffset (factor : GL.Float;
                           units  : GL.Float);

  procedure PolygonStipple (mask : ubytePtr);

  procedure GetPolygonStipple (mask : ubytePtr);

  procedure EdgeFlag (flag : GL_Boolean);

  procedure EdgeFlagv (flag : GL_BooleanPtr);

  procedure Scissor (x      : GL.Int;
                     y      : GL.Int;
                     width  : GL.Sizei;
                     height : GL.Sizei);

  --  Attribute stacks
  procedure PushAttrib (mask : Bitfield);

  procedure PopAttrib;

  procedure PushClientAttrib (mask : Bitfield);

  procedure PopClientAttrib;

  --  Pipeline control
  procedure Finish;

  procedure Flush;

  procedure DepthMask (flag : GL_Boolean);

  procedure DepthRange (near_val : GL.Clampd;
                        far_val  : GL.Clampd);

  --  Projections
  procedure Ortho (left_val   : GL.Double;
                   right_val  : GL.Double;
                   bottom_val : GL.Double;
                   top_val    : GL.Double;
                   near_val   : GL.Double;
                   far_val    : GL.Double);

  procedure Frustum (left_val   : GL.Double;
                     right_val  : GL.Double;
                     bottom_val : GL.Double;
                     top_val    : GL.Double;
                     near_val   : GL.Double;
                     far_val    : GL.Double);

  procedure Viewport (x      : GL.Int;
                      y      : GL.Int;
                      width  : GL.Sizei;
                      height : GL.Sizei);

  --  Matrix stacks
  procedure PushMatrix;

  procedure PopMatrix;

  procedure LoadIdentity;

  procedure LoadMatrixd (m : GL.doublePtr);

  procedure LoadMatrixf (m : floatPtr);

  procedure MultMatrixd (m : GL.doublePtr);

  procedure MultMatrixf (m : floatPtr);

  --  Transformations
  procedure Rotate (angle : GL.Double;
                    x     : GL.Double;
                    y     : GL.Double;
                    z     : GL.Double);

  procedure Rotate_f (angle : GL.Float;
                      x     : GL.Float;
                      y     : GL.Float;
                      z     : GL.Float);

  procedure Scale   (x, y, z : GL.Double);
  procedure Scale_f (x, y, z : GL.Float);

  procedure Translate   (x, y, z : GL.Double);
  procedure Translate_f (x, y, z : GL.Float);
  procedure Translate (v : Double_Vector_3D);
  pragma Inline (Translate);

  --  Specify vertices
  procedure Vertex   (x, y : GL.Double);
  procedure Vertex_f (x, y : GL.Float);

  procedure Vertex   (x, y : GL.Int);
  procedure Vertex_s (x, y : GL.Short);

  procedure Vertex   (x, y, z : GL.Double);
  procedure Vertex_f (x, y, z : GL.Float);

  procedure Vertex   (x, y, z : GL.Int);
  procedure Vertex_s (x, y, z : GL.Short);

  procedure Vertex   (x, y, z, w : GL.Double);
  procedure Vertex_f (x, y, z, w : GL.Float);

  procedure Vertex   (x, y, z, w : GL.Int);
  procedure Vertex_s (x, y, z, w : GL.Short);

  procedure Vertex (v : Double_Vector_3D);
  pragma Inline (Vertex);

  procedure Vertex2dv (v : GL.doublePtr);

  procedure Vertex2fv (v : floatPtr);

  procedure Vertex2iv (v : GL.intPointer);

  procedure Vertex2sv (v : GL.shortPtr);

  procedure Vertex3dv (v : GL.doublePtr);

  procedure Vertex3fv (v : floatPtr);

  procedure Vertex3iv (v : GL.intPointer);

  procedure Vertex3sv (v : GL.shortPtr);

  procedure Vertex4dv (v : GL.doublePtr);

  procedure Vertex4fv (v : floatPtr);

  procedure Vertex4iv (v : GL.intPointer);

  procedure Vertex4sv (v : GL.shortPtr);

  --  Specify normal vectors

  procedure Normal   (x, y, z : GL.Double);
  procedure Normal_f (x, y, z : GL.Float);
  procedure Normal   (x, y, z : GL.Int);
  procedure Normal_b (x, y, z : GL.Byte);
  procedure Normal_s (x, y, z : GL.Short);

  procedure Normal   (v : Double_Vector_3D);
  pragma Inline (Normal);

  procedure Normal3bv (v : GL.bytePtr);

  procedure Normal3dv (v : GL.doublePtr);

  procedure Normal3fv (v : floatPtr);

  procedure Normal3iv (v : GL.intPointer);

  procedure Normal3sv (v : GL.shortPtr);

  --  Indexed color
  procedure Indexd   (c : GL.Double);

  procedure Indexf   (c : GL.Float);

  procedure Indexi   (c : GL.Int);

  procedure Indexs   (c : GL.Short);

  procedure Indexub  (c : GL.Ubyte);

  procedure Indexdv  (c : GL.doublePtr);

  procedure Indexfv  (c : floatPtr);

  procedure Indexiv  (c : GL.intPointer);

  procedure Indexsv  (c : GL.shortPtr);

  procedure Indexubv (c : ubytePtr);

  --  Component color
  procedure Color3b (red_value   : GL.Byte;
                     green_value : GL.Byte;
                     blue_value  : GL.Byte);

  procedure Color   (red_value, green_value, blue_value : GL.Double);
  procedure Color_f (red_value, green_value, blue_value : GL.Float);
  procedure Color   (red_value, green_value, blue_value : GL.Int);
  procedure Color_s (red_value, green_value, blue_value : GL.Short);

  procedure Color3ub (red_value   : GL.Ubyte;
                      green_value : GL.Ubyte;
                      blue_value  : GL.Ubyte);

  procedure Color3ui (red_value   : GL.Uint;
                      green_value : GL.Uint;
                      blue_value  : GL.Uint);

  procedure Color3us (red_value   : GL.Ushort;
                      green_value : GL.Ushort;
                      blue_value  : GL.Ushort);

  procedure Color4b (red_value   : GL.Byte;
                     green_value : GL.Byte;
                     blue_value  : GL.Byte;
                     alpha_value : GL.Byte);

  procedure Color   (red_value, green_value, blue_value, alpha_value : GL.Double);
  procedure Color_f (red_value, green_value, blue_value, alpha_value : GL.Float);
  procedure Color   (red_value, green_value, blue_value, alpha_value : GL.Int);
  procedure Color_s (red_value, green_value, blue_value, alpha_value : GL.Short);

  procedure Color4ub (red_value   : GL.Ubyte;
                      green_value : GL.Ubyte;
                      blue_value  : GL.Ubyte;
                      alpha_value : GL.Ubyte);

  procedure Color4ui (red_value   : GL.Uint;
                      green_value : GL.Uint;
                      blue_value  : GL.Uint;
                      alpha_value : GL.Uint);

  procedure Color4us (red_value   : GL.Ushort;
                      green_value : GL.Ushort;
                      blue_value  : GL.Ushort;
                      alpha_value : GL.Ushort);

  procedure Color3bv (v : GL.bytePtr);

  procedure Color3dv (v : GL.doublePtr);
  procedure Color (v : RGB_Color);

  procedure Color3fv (v : GL.floatPtr);

  procedure Color3iv (v : GL.intPointer);

  procedure Color3sv (v : GL.shortPtr);

  procedure Color3ubv (v : GL.ubytePtr);

  procedure Color3uiv (v : GL.uintPtr);

  procedure Color3usv (v : GL.ushortPtr);

  procedure Color4bv (v : GL.bytePtr);

  procedure Color4dv (v : GL.doublePtr);
  procedure Color (v : RGBA_Color);

  procedure Color4fv (v : GL.floatPtr);

  procedure Color4iv (v : GL.intPointer);

  procedure Color4sv (v : GL.shortPtr);

  procedure Color4ubv (v : GL.ubytePtr);

  procedure Color4uiv (v : GL.uintPtr);

  procedure Color4usv (v : GL.ushortPtr);

  --  Texture coordinates
  procedure TexCoord1d (s : GL.Double);

  procedure TexCoord1f (s : GL.Float);

  procedure TexCoord1i (s : GL.Int);

  procedure TexCoord1s (s : GL.Short);

  procedure TexCoord (s : GL.Double;
                      t : GL.Double);

  procedure TexCoordf (s : GL.Float;
                       t : GL.Float);

  procedure TexCoord2i (s : GL.Int;
                        t : GL.Int);

  procedure TexCoord2s (s : GL.Short;
                        t : GL.Short);

  procedure TexCoord3d (s : GL.Double;
                        t : GL.Double;
                        r : GL.Double);

  procedure TexCoord3f (s : GL.Float;
                        t : GL.Float;
                        r : GL.Float);

  procedure TexCoord3i (s : GL.Int;
                        t : GL.Int;
                        r : GL.Int);

  procedure TexCoord3s (s : GL.Short;
                        t : GL.Short;
                        r : GL.Short);

  procedure TexCoord4d (s : GL.Double;
                        t : GL.Double;
                        r : GL.Double;
                        q : GL.Double);

  procedure TexCoord4f (s : GL.Float;
                        t : GL.Float;
                        r : GL.Float;
                        q : GL.Float);

  procedure TexCoord4i (s : GL.Int;
                        t : GL.Int;
                        r : GL.Int;
                        q : GL.Int);

  procedure TexCoord4s (s : GL.Short;
                        t : GL.Short;
                        r : GL.Short;
                        q : GL.Short);

  procedure TexCoord1dv (v : GL.doublePtr);

  procedure TexCoord1fv (v : floatPtr);

  procedure TexCoord1iv (v : GL.intPointer);

  procedure TexCoord1sv (v : GL.shortPtr);

  procedure TexCoord2dv (v : GL.doublePtr);

  procedure TexCoord2fv (v : floatPtr);

  procedure TexCoord2iv (v : GL.intPointer);

  procedure TexCoord2sv (v : GL.shortPtr);

  procedure TexCoord3dv (v : GL.doublePtr);

  procedure TexCoord3fv (v : floatPtr);

  procedure TexCoord3iv (v : GL.intPointer);

  procedure TexCoord3sv (v : GL.shortPtr);

  procedure TexCoord4dv (v : GL.doublePtr);

  procedure TexCoord4fv (v : floatPtr);

  procedure TexCoord4iv (v : GL.intPointer);

  procedure TexCoord4sv (v : GL.shortPtr);

  --  Pixel op raster position
  procedure RasterPos2d (x : GL.Double;
                         y : GL.Double);

  procedure RasterPos2f (x : GL.Float;
                         y : GL.Float);

  procedure RasterPos (x, y : GL.Int);

  procedure RasterPos2s (x : GL.Short;
                         y : GL.Short);

  procedure RasterPos3d (x : GL.Double;
                         y : GL.Double;
                         z : GL.Double);

  procedure RasterPos3f (x : GL.Float;
                         y : GL.Float;
                         z : GL.Float);

  procedure RasterPos3i (x : GL.Int;
                         y : GL.Int;
                         z : GL.Int);

  procedure RasterPos3s (x : GL.Short;
                         y : GL.Short;
                         z : GL.Short);

  procedure RasterPos4d (x : GL.Double;
                         y : GL.Double;
                         z : GL.Double;
                         w : GL.Double);

  procedure RasterPos4f (x : GL.Float;
                         y : GL.Float;
                         z : GL.Float;
                         w : GL.Float);

  procedure RasterPos4i (x : GL.Int;
                         y : GL.Int;
                         z : GL.Int;
                         w : GL.Int);

  procedure RasterPos4s (x : GL.Short;
                         y : GL.Short;
                         z : GL.Short;
                         w : GL.Short);

  procedure RasterPos2dv (v : GL.doublePtr);

  procedure RasterPos2fv (v : floatPtr);

  procedure RasterPos2iv (v : GL.intPointer);

  procedure RasterPos2sv (v : GL.shortPtr);

  procedure RasterPos3dv (v : GL.doublePtr);

  procedure RasterPos3fv (v : floatPtr);

  procedure RasterPos3iv (v : GL.intPointer);

  procedure RasterPos3sv (v : GL.shortPtr);

  procedure RasterPos4dv (v : GL.doublePtr);

  procedure RasterPos4fv (v : floatPtr);

  procedure RasterPos4iv (v : GL.intPointer);

  procedure RasterPos4sv (v : GL.shortPtr);

  --  Rectangles
  procedure Rectd (x1 : GL.Double;
                   y1 : GL.Double;
                   x2 : GL.Double;
                   y2 : GL.Double);

  procedure Rectf (x1 : GL.Float;
                   y1 : GL.Float;
                   x2 : GL.Float;
                   y2 : GL.Float);

  procedure Recti (x1 : GL.Int;
                   y1 : GL.Int;
                   x2 : GL.Int;
                   y2 : GL.Int);

  procedure Rects (x1 : GL.Short;
                   y1 : GL.Short;
                   x2 : GL.Short;
                   y2 : GL.Short);

  procedure Rectdv (v1 : GL.doublePtr;
                    v2 : GL.doublePtr);

  procedure Rectfv (v1 : floatPtr;
                    v2 : floatPtr);

  procedure Rectiv (v1 : GL.intPointer;
                    v2 : GL.intPointer);

  procedure Rectsv (v1 : GL.shortPtr;
                    v2 : GL.shortPtr);

  --  Bitmap
  procedure Bitmap (width  : GL.Sizei;
                    height : GL.Sizei;
                    xorig  : GL.Float;
                    yorig  : GL.Float;
                    xmove  : GL.Float;
                    ymove  : GL.Float;
                    bitmap : ubytePtr);

  --  Stenciling
  procedure StencilMask (mask : GL.Uint);

  procedure ClearStencil (s : GL.Int);

  --  Selections and name stack
  procedure SelectBuffer (size   : GL.Sizei;
                          buffer : GL.uintPtr);
  procedure InitNames;

  procedure LoadName (name : GL.Uint);

  procedure PushName (name : GL.Uint);

  procedure PopName;

  --  Mesa-specific routines
  procedure WindowPos2iMESA (x : GL.Int;
                             y : GL.Int);

  procedure WindowPos2sMESA (x : GL.Short;
                             y : GL.Short);

  procedure WindowPos2fMESA (x : GL.Float;
                             y : GL.Float);

  procedure WindowPos2dMESA (x : GL.Double;
                             y : GL.Double);

  procedure WindowPos2ivMESA (p : GL.intPointer);

  procedure WindowPos2svMESA (p : GL.shortPtr);

  procedure WindowPos2fvMESA (p : floatPtr);

  procedure WindowPos2dvMESA (p : GL.doublePtr);

  procedure WindowPos3iMESA (x : GL.Int;
                             y : GL.Int;
                             z : GL.Int);

  procedure WindowPos3sMESA (x : GL.Short;
                             y : GL.Short;
                             z : GL.Short);

  procedure WindowPos3fMESA (x : GL.Float;
                             y : GL.Float;
                             z : GL.Float);

  procedure WindowPos3dMESA (x : GL.Double;
                             y : GL.Double;
                             z : GL.Double);

  procedure WindowPos3ivMESA (p : GL.intPointer);

  procedure WindowPos3svMESA (p : GL.shortPtr);

  procedure WindowPos3fvMESA (p : floatPtr);

  procedure WindowPos3dvMESA (p : GL.doublePtr);

  procedure WindowPos4iMESA (x : GL.Int;
                             y : GL.Int;
                             z : GL.Int;
                             w : GL.Int);

  procedure WindowPos4sMESA (x : GL.Short;
                             y : GL.Short;
                             z : GL.Short;
                             w : GL.Short);

  procedure WindowPos4fMESA (x : GL.Float;
                             y : GL.Float;
                             z : GL.Float;
                             w : GL.Float);

  procedure WindowPos4dMESA (x : GL.Double;
                             y : GL.Double;
                             z : GL.Double;
                             w : GL.Double);

  procedure WindowPos4ivMESA (p : GL.intPointer);

  procedure WindowPos4svMESA (p : GL.shortPtr);

  procedure WindowPos4fvMESA (p : floatPtr);

  procedure WindowPos4dvMESA (p : GL.doublePtr);

  procedure ResizeBuffersMESA;

private

  --  Workaround for GNAT 3.15p (OA 7.2.2 OK), when applying pragma Import to all
  --  functions named GetString:
  --  -> convention for "GetString" does not permit returning unconstrained array type
  function glGetString (name : StringEnm) return ubytePtr;
  function GetString (name : StringEnm) return ubytePtr renames glGetString;

  --  GdM: renames for getting rid of pointers and "...4f"-style suffixes

--  The following wrappers are automatically generated by
--  the GL_Overloader tool. To generate other wrappers easily,
--  look at GL_Overloader.adb .

--  *** Code generated by `tools/gl_overloader.adb` ***

  --  Wrapper for Color3d   (Color, 3 arguments, type: Double)
  procedure Color3d (red_value, green_value, blue_value : GL.Double);
  procedure Color   (red_value, green_value, blue_value : GL.Double) renames Color3d;

  --  Wrapper for Color3f   (Color, 3 arguments, type: Float)
  procedure Color3f (red_value, green_value, blue_value : GL.Float);
  procedure Color_f (red_value, green_value, blue_value : GL.Float) renames Color3f;

  --  Wrapper for Color3i   (Color, 3 arguments, type: Int)
  procedure Color3i (red_value, green_value, blue_value : GL.Int);
  procedure Color   (red_value, green_value, blue_value : GL.Int) renames Color3i;

  --  Wrapper for Color3s   (Color, 3 arguments, type: Short)
  procedure Color3s (red_value, green_value, blue_value : GL.Short);
  procedure Color_s (red_value, green_value, blue_value : GL.Short) renames Color3s;

  --  Wrapper for Color4d   (Color, 4 arguments, type: Double)
  procedure Color4d (red_value, green_value, blue_value, alpha_value : GL.Double);
  procedure Color   (red_value, green_value, blue_value, alpha_value : GL.Double) renames Color4d;

  --  Wrapper for Color4f   (Color, 4 arguments, type: Float)
  procedure Color4f (red_value, green_value, blue_value, alpha_value : GL.Float);
  procedure Color_f (red_value, green_value, blue_value, alpha_value : GL.Float) renames Color4f;

  --  Wrapper for Color4i   (Color, 4 arguments, type: Int)
  procedure Color4i (red_value, green_value, blue_value, alpha_value : GL.Int);
  procedure Color   (red_value, green_value, blue_value, alpha_value : GL.Int) renames Color4i;

  --  Wrapper for Color4s   (Color, 4 arguments, type: Short)
  procedure Color4s (red_value, green_value, blue_value, alpha_value : GL.Short);
  procedure Color_s (red_value, green_value, blue_value, alpha_value : GL.Short) renames Color4s;

  --  Wrapper for Normal3d   (Normal, 3 arguments, type: Double)
  procedure Normal3d (x, y, z : GL.Double);
  procedure Normal   (x, y, z : GL.Double) renames Normal3d;

  --  Wrapper for Normal3f   (Normal, 3 arguments, type: Float)
  procedure Normal3f (x, y, z : GL.Float);
  procedure Normal_f (x, y, z : GL.Float) renames Normal3f;

  --  Wrapper for Normal3i   (Normal, 3 arguments, type: Int)
  procedure Normal3i (x, y, z : GL.Int);
  procedure Normal   (x, y, z : GL.Int) renames Normal3i;

  --  Wrapper for Normal3b   (Normal, 3 arguments, type: Byte)
  procedure Normal3b (x, y, z : GL.Byte);
  procedure Normal_b (x, y, z : GL.Byte) renames Normal3b;

  --  Wrapper for Normal3s   (Normal, 3 arguments, type: Short)
  procedure Normal3s (x, y, z : GL.Short);
  procedure Normal_s (x, y, z : GL.Short) renames Normal3s;

  --  Wrapper for Scale3d   (Scale, 3 arguments, type: Double)
  procedure Scale3d (x, y, z : GL.Double);
  procedure Scale   (x, y, z : GL.Double) renames Scale3d;

  --  Wrapper for Scale3f   (Scale, 3 arguments, type: Float)
  procedure Scale3f (x, y, z : GL.Float);
  procedure Scale_f (x, y, z : GL.Float) renames Scale3f;

  --  Wrapper for Translate3d   (Translate, 3 arguments, type: Double)
  procedure Translate3d (x, y, z : GL.Double);
  procedure Translate   (x, y, z : GL.Double) renames Translate3d;

  --  Wrapper for Translate3f   (Translate, 3 arguments, type: Float)
  procedure Translate3f (x, y, z : GL.Float);
  procedure Translate_f (x, y, z : GL.Float) renames Translate3f;

  --  Wrapper for Vertex2d   (Vertex, 2 arguments, type: Double)
  procedure Vertex2d (x, y : GL.Double);
  procedure Vertex   (x, y : GL.Double) renames Vertex2d;

  --  Wrapper for Vertex2f   (Vertex, 2 arguments, type: Float)
  procedure Vertex2f (x, y : GL.Float);
  procedure Vertex_f (x, y : GL.Float) renames Vertex2f;

  --  Wrapper for Vertex2i   (Vertex, 2 arguments, type: Int)
  procedure Vertex2i (x, y : GL.Int);
  procedure Vertex   (x, y : GL.Int) renames Vertex2i;

  --  Wrapper for Vertex2s   (Vertex, 2 arguments, type: Short)
  procedure Vertex2s (x, y : GL.Short);
  procedure Vertex_s (x, y : GL.Short) renames Vertex2s;

  --  Wrapper for Vertex3d   (Vertex, 3 arguments, type: Double)
  procedure Vertex3d (x, y, z : GL.Double);
  procedure Vertex   (x, y, z : GL.Double) renames Vertex3d;

  --  Wrapper for Vertex3f   (Vertex, 3 arguments, type: Float)
  procedure Vertex3f (x, y, z : GL.Float);
  procedure Vertex_f (x, y, z : GL.Float) renames Vertex3f;

  --  Wrapper for Vertex3i   (Vertex, 3 arguments, type: Int)
  procedure Vertex3i (x, y, z : GL.Int);
  procedure Vertex   (x, y, z : GL.Int) renames Vertex3i;

  --  Wrapper for Vertex3s   (Vertex, 3 arguments, type: Short)
  procedure Vertex3s (x, y, z : GL.Short);
  procedure Vertex_s (x, y, z : GL.Short) renames Vertex3s;

  --  Wrapper for Vertex4d   (Vertex, 4 arguments, type: Double)
  procedure Vertex4d (x, y, z, w : GL.Double);
  procedure Vertex   (x, y, z, w : GL.Double) renames Vertex4d;

  --  Wrapper for Vertex4f   (Vertex, 4 arguments, type: Float)
  procedure Vertex4f (x, y, z, w : GL.Float);
  procedure Vertex_f (x, y, z, w : GL.Float) renames Vertex4f;

  --  Wrapper for Vertex4i   (Vertex, 4 arguments, type: Int)
  procedure Vertex4i (x, y, z, w : GL.Int);
  procedure Vertex   (x, y, z, w : GL.Int) renames Vertex4i;

  --  Wrapper for Vertex4s   (Vertex, 4 arguments, type: Short)
  procedure Vertex4s (x, y, z, w : GL.Short);
  procedure Vertex_s (x, y, z, w : GL.Short) renames Vertex4s;

--  *** End of Code generated by `tools/gl_overloader.adb` ***

  --  Wrappers for Get
  procedure GetDoublev (pname  : ParameterNameEnm;
                        params : GL.doublePtr);

  procedure Get (pname  : ParameterNameEnm;
                 params : GL.doublePtr) renames GetDoublev;

  --  Wrappers for Light

  procedure Lightf (light_id : LightIDEnm;
                    pname   : LightParameterEnm;
                    param   : GL.Float);
  procedure Light (light_id : LightIDEnm;
                   pname    : LightParameterEnm;
                   param    : GL.Float) renames Lightf;

  procedure Lightfv (light_id : LightIDEnm;
                     pname    : LightParameterVEnm;
                     params   : floatPtr);

  --  Wrappers for Material

  procedure Materialf (face  : FaceEnm;
                       pname : MaterialParameterEnm;
                       param : GL.Float);
  procedure Material (face  : FaceEnm;
                      pname : MaterialParameterEnm;
                      param : GL.Float) renames Materialf;

  procedure Materialfv (face   : FaceEnm;
                        pname  : MaterialParameterVEnm;
                        params : floatPtr);

  procedure PixelStorei (pname : PixelStorageEnm;
                         param : GL.Int);
  procedure PixelStore (pname : PixelStorageEnm;
                        param : GL.Int) renames PixelStorei;

  procedure RasterPos2i (x : GL.Int;
                         y : GL.Int);
  procedure RasterPos (x, y : GL.Int) renames RasterPos2i;

  procedure Rotated (angle : GL.Double;
                     x     : GL.Double;
                     y     : GL.Double;
                     z     : GL.Double);
  procedure Rotate (angle : GL.Double;
                    x     : GL.Double;
                    y     : GL.Double;
                    z     : GL.Double) renames Rotated;

  procedure Rotatef (angle : GL.Float;
                     x     : GL.Float;
                     y     : GL.Float;
                     z     : GL.Float);
  procedure Rotate_f (angle : GL.Float;
                      x     : GL.Float;
                      y     : GL.Float;
                      z     : GL.Float) renames Rotatef;

  procedure TexCoord2d (s : GL.Double;
                        t : GL.Double);
  procedure TexCoord (s : GL.Double;
                      t : GL.Double) renames TexCoord2d;

  procedure TexCoord2f (s : GL.Float;
                        t : GL.Float);
  procedure TexCoordf (s : GL.Float;
                       t : GL.Float) renames TexCoord2f;

  procedure TexEnvi (target : TexEnvEnm;
                     pname  : TexEnvParameterEnm;
                     param  : GL.Int);
  procedure TexEnv (target : TexEnvEnm;
                    pname  : TexEnvParameterEnm;
                    param  : GL.Int) renames TexEnvi;

  procedure TexParameteri (target : TargetTexEnm;
                           pname  : TexParamEnm;
                           param  : GL.Int);
  procedure TexParameter (target : TargetTexEnm;
                          pname  : TexParamEnm;
                          param  : GL.Int) renames TexParameteri;

  --  Some renames due to possible ambiguity with enumerated
  --  values (Accum, Clear, Viewport) that can be interpreted
  --  as a parameterless function, confusing then pragma Import
  --  on the Janus compiler and not GNAT and ObjectAda.
  --  GM/TM 9-Sep-2006
  procedure glAccum (op    : AccumEnm;
                     value : GL.Float);
  procedure Accum   (op    : AccumEnm;
                     value : GL.Float) renames glAccum;
  --
  procedure glClear (mask : Bitfield);
  procedure Clear   (mask : Bitfield) renames glClear;
  --
  procedure glViewport (x      : GL.Int;
                        y      : GL.Int;
                        width  : GL.Sizei;
                        height : GL.Sizei);
  procedure Viewport (x      : GL.Int;
                      y      : GL.Int;
                      width  : GL.Sizei;
                      height : GL.Sizei) renames glViewport;

  -----------------
  -- Interfacing --
  -----------------

  pragma Import (Stdcall, ClearIndex, "glClearIndex");
  pragma Import (Stdcall, ClearColor, "glClearColor");
  pragma Import (Stdcall, glClear, "glClear");
  pragma Import (Stdcall, IndexMask, "glIndexMask");
  pragma Import (Stdcall, ColorMask, "glColorMask");
  pragma Import (Stdcall, AlphaFunc, "glAlphaFunc");
  pragma Import (Stdcall, BlendFunc, "glBlendFunc");
  pragma Import (Stdcall, LogicOp, "glLogicOp");
  pragma Import (Stdcall, CullFace, "glCullFace");
  pragma Import (Stdcall, FrontFace, "glFrontFace");
  pragma Import (Stdcall, PointSize, "glPointSize");
  pragma Import (Stdcall, LineWidth, "glLineWidth");
  pragma Import (Stdcall, LineStipple, "glLineStipple");
  pragma Import (Stdcall, PolygonMode, "glPolygonMode");
  pragma Import (Stdcall, PolygonOffset, "glPolygonOffset");
  pragma Import (Stdcall, PolygonStipple, "glPolygonStipple");
  pragma Import (Stdcall, GetPolygonStipple, "glGetPolygonStipple");
  pragma Import (Stdcall, EdgeFlag, "glEdgeFlag");
  pragma Import (Stdcall, EdgeFlagv, "glEdgeFlagv");
  pragma Import (Stdcall, Scissor, "glScissor");
  pragma Import (Stdcall, ClipPlane, "glClipPlane");
  pragma Import (Stdcall, GetClipPlane, "glGetClipPlane");
  pragma Import (Stdcall, DrawBuffer, "glDrawBuffer");
  pragma Import (Stdcall, ReadBuffer, "glReadBuffer");
  pragma Import (Stdcall, Enable, "glEnable");
  pragma Import (Stdcall, Disable, "glDisable");
  pragma Import (Stdcall, IsEnabled, "glIsEnabled");
  pragma Import (Stdcall, EnableClientState, "glEnableClientState");
  pragma Import (Stdcall, DisableClientState, "glDisableClientState");
  pragma Import (Stdcall, GetBooleanv, "glGetBooleanv");
  pragma Import (Stdcall, GetDoublev, "glGetDoublev");
  pragma Import (Stdcall, GetFloatv, "glGetFloatv");
  pragma Import (Stdcall, GetIntegerv, "glGetIntegerv");
  pragma Import (Stdcall, PushAttrib, "glPushAttrib");
  pragma Import (Stdcall, PopAttrib, "glPopAttrib");
  pragma Import (Stdcall, PushClientAttrib, "glPushClientAttrib");
  pragma Import (Stdcall, PopClientAttrib, "glPopClientAttrib");
  pragma Import (Stdcall, RenderMode, "glRenderMode");
  pragma Import (Stdcall, GetError, "glGetError");
  pragma Import (Stdcall, glGetString, "glGetString");
  pragma Import (Stdcall, Finish, "glFinish");
  pragma Import (Stdcall, Flush, "glFlush");
  pragma Import (Stdcall, Hint, "glHint");
  pragma Import (Stdcall, ClearDepth, "glClearDepth");
  pragma Import (Stdcall, DepthFunc, "glDepthFunc");
  pragma Import (Stdcall, DepthMask, "glDepthMask");
  pragma Import (Stdcall, DepthRange, "glDepthRange");
  pragma Import (Stdcall, ClearAccum, "glClearAccum");
  pragma Import (Stdcall, glAccum, "glAccum");
  pragma Import (Stdcall, MatrixMode, "glMatrixMode");
  pragma Import (Stdcall, Ortho, "glOrtho");
  pragma Import (Stdcall, Frustum, "glFrustum");
  pragma Import (Stdcall, glViewport, "glViewport");
  pragma Import (Stdcall, PushMatrix, "glPushMatrix");
  pragma Import (Stdcall, PopMatrix, "glPopMatrix");
  pragma Import (Stdcall, LoadIdentity, "glLoadIdentity");
  pragma Import (Stdcall, LoadMatrixd, "glLoadMatrixd");
  pragma Import (Stdcall, LoadMatrixf, "glLoadMatrixf");
  pragma Import (Stdcall, MultMatrixd, "glMultMatrixd");
  pragma Import (Stdcall, MultMatrixf, "glMultMatrixf");
  pragma Import (Stdcall, Rotated, "glRotated");
  pragma Import (Stdcall, Rotatef, "glRotatef");
  pragma Import (Stdcall, Scale3d, "glScaled");
  pragma Import (Stdcall, Scale3f, "glScalef");
  pragma Import (Stdcall, Translate3d, "glTranslated");
  pragma Import (Stdcall, Translate3f, "glTranslatef");
  pragma Import (Stdcall, IsList, "glIsList");
  pragma Import (Stdcall, DeleteLists, "glDeleteLists");
  pragma Import (Stdcall, GenLists, "glGenLists");
  pragma Import (Stdcall, NewList, "glNewList");
  pragma Import (Stdcall, EndList, "glEndList");
  pragma Import (Stdcall, CallList, "glCallList");
  pragma Import (Stdcall, CallLists, "glCallLists");
  pragma Import (Stdcall, ListBase, "glListBase");
  pragma Import (Stdcall, GL_Begin, "glBegin");
  pragma Import (Stdcall, GL_End, "glEnd");
  pragma Import (Stdcall, Vertex2d, "glVertex2d");
  pragma Import (Stdcall, Vertex2f, "glVertex2f");
  pragma Import (Stdcall, Vertex2i, "glVertex2i");
  pragma Import (Stdcall, Vertex2s, "glVertex2s");
  pragma Import (Stdcall, Vertex3d, "glVertex3d");
  pragma Import (Stdcall, Vertex3f, "glVertex3f");
  pragma Import (Stdcall, Vertex3i, "glVertex3i");
  pragma Import (Stdcall, Vertex3s, "glVertex3s");
  pragma Import (Stdcall, Vertex4d, "glVertex4d");
  pragma Import (Stdcall, Vertex4f, "glVertex4f");
  pragma Import (Stdcall, Vertex4i, "glVertex4i");
  pragma Import (Stdcall, Vertex4s, "glVertex4s");
  pragma Import (Stdcall, Vertex2dv, "glVertex2dv");
  pragma Import (Stdcall, Vertex2fv, "glVertex2fv");
  pragma Import (Stdcall, Vertex2iv, "glVertex2iv");
  pragma Import (Stdcall, Vertex2sv, "glVertex2sv");
  pragma Import (Stdcall, Vertex3dv, "glVertex3dv");
  pragma Import (Stdcall, Vertex3fv, "glVertex3fv");
  pragma Import (Stdcall, Vertex3iv, "glVertex3iv");
  pragma Import (Stdcall, Vertex3sv, "glVertex3sv");
  pragma Import (Stdcall, Vertex4dv, "glVertex4dv");
  pragma Import (Stdcall, Vertex4fv, "glVertex4fv");
  pragma Import (Stdcall, Vertex4iv, "glVertex4iv");
  pragma Import (Stdcall, Vertex4sv, "glVertex4sv");
  pragma Import (Stdcall, Normal3b, "glNormal3b");
  pragma Import (Stdcall, Normal3d, "glNormal3d");
  pragma Import (Stdcall, Normal3f, "glNormal3f");
  pragma Import (Stdcall, Normal3i, "glNormal3i");
  pragma Import (Stdcall, Normal3s, "glNormal3s");
  pragma Import (Stdcall, Normal3bv, "glNormal3bv");
  pragma Import (Stdcall, Normal3dv, "glNormal3dv");
  pragma Import (Stdcall, Normal3fv, "glNormal3fv");
  pragma Import (Stdcall, Normal3iv, "glNormal3iv");
  pragma Import (Stdcall, Normal3sv, "glNormal3sv");
  pragma Import (Stdcall, Indexd, "glIndexd");
  pragma Import (Stdcall, Indexf, "glIndexf");
  pragma Import (Stdcall, Indexi, "glIndexi");
  pragma Import (Stdcall, Indexs, "glIndexs");
  pragma Import (Stdcall, Indexub, "glIndexub");
  pragma Import (Stdcall, Indexdv, "glIndexdv");
  pragma Import (Stdcall, Indexfv, "glIndexfv");
  pragma Import (Stdcall, Indexiv, "glIndexiv");
  pragma Import (Stdcall, Indexsv, "glIndexsv");
  pragma Import (Stdcall, Indexubv, "glIndexubv");
  pragma Import (Stdcall, Color3b, "glColor3b");
  pragma Import (Stdcall, Color3d, "glColor3d");
  pragma Import (Stdcall, Color3f, "glColor3f");
  pragma Import (Stdcall, Color3i, "glColor3i");
  pragma Import (Stdcall, Color3s, "glColor3s");
  pragma Import (Stdcall, Color3ub, "glColor3ub");
  pragma Import (Stdcall, Color3ui, "glColor3ui");
  pragma Import (Stdcall, Color3us, "glColor3us");
  pragma Import (Stdcall, Color4b, "glColor4b");
  pragma Import (Stdcall, Color4d, "glColor4d");
  pragma Import (Stdcall, Color4f, "glColor4f");
  pragma Import (Stdcall, Color4i, "glColor4i");
  pragma Import (Stdcall, Color4s, "glColor4s");
  pragma Import (Stdcall, Color4ub, "glColor4ub");
  pragma Import (Stdcall, Color4ui, "glColor4ui");
  pragma Import (Stdcall, Color4us, "glColor4us");
  pragma Import (Stdcall, Color3bv, "glColor3bv");
  pragma Import (Stdcall, Color3dv, "glColor3dv");
  pragma Import (Stdcall, Color3fv, "glColor3fv");
  pragma Import (Stdcall, Color3iv, "glColor3iv");
  pragma Import (Stdcall, Color3sv, "glColor3sv");
  pragma Import (Stdcall, Color3ubv, "glColor3ubv");
  pragma Import (Stdcall, Color3uiv, "glColor3uiv");
  pragma Import (Stdcall, Color3usv, "glColor3usv");
  pragma Import (Stdcall, Color4bv, "glColor4bv");
  pragma Import (Stdcall, Color4dv, "glColor4dv");
  pragma Import (Stdcall, Color4fv, "glColor4fv");
  pragma Import (Stdcall, Color4iv, "glColor4iv");
  pragma Import (Stdcall, Color4sv, "glColor4sv");
  pragma Import (Stdcall, Color4ubv, "glColor4ubv");
  pragma Import (Stdcall, Color4uiv, "glColor4uiv");
  pragma Import (Stdcall, Color4usv, "glColor4usv");
  pragma Import (Stdcall, TexCoord1d, "glTexCoord1d");
  pragma Import (Stdcall, TexCoord1f, "glTexCoord1f");
  pragma Import (Stdcall, TexCoord1i, "glTexCoord1i");
  pragma Import (Stdcall, TexCoord1s, "glTexCoord1s");
  pragma Import (Stdcall, TexCoord2d, "glTexCoord2d");
  pragma Import (Stdcall, TexCoord2f, "glTexCoord2f");
  pragma Import (Stdcall, TexCoord2i, "glTexCoord2i");
  pragma Import (Stdcall, TexCoord2s, "glTexCoord2s");
  pragma Import (Stdcall, TexCoord3d, "glTexCoord3d");
  pragma Import (Stdcall, TexCoord3f, "glTexCoord3f");
  pragma Import (Stdcall, TexCoord3i, "glTexCoord3i");
  pragma Import (Stdcall, TexCoord3s, "glTexCoord3s");
  pragma Import (Stdcall, TexCoord4d, "glTexCoord4d");
  pragma Import (Stdcall, TexCoord4f, "glTexCoord4f");
  pragma Import (Stdcall, TexCoord4i, "glTexCoord4i");
  pragma Import (Stdcall, TexCoord4s, "glTexCoord4s");
  pragma Import (Stdcall, TexCoord1dv, "glTexCoord1dv");
  pragma Import (Stdcall, TexCoord1fv, "glTexCoord1fv");
  pragma Import (Stdcall, TexCoord1iv, "glTexCoord1iv");
  pragma Import (Stdcall, TexCoord1sv, "glTexCoord1sv");
  pragma Import (Stdcall, TexCoord2dv, "glTexCoord2dv");
  pragma Import (Stdcall, TexCoord2fv, "glTexCoord2fv");
  pragma Import (Stdcall, TexCoord2iv, "glTexCoord2iv");
  pragma Import (Stdcall, TexCoord2sv, "glTexCoord2sv");
  pragma Import (Stdcall, TexCoord3dv, "glTexCoord3dv");
  pragma Import (Stdcall, TexCoord3fv, "glTexCoord3fv");
  pragma Import (Stdcall, TexCoord3iv, "glTexCoord3iv");
  pragma Import (Stdcall, TexCoord3sv, "glTexCoord3sv");
  pragma Import (Stdcall, TexCoord4dv, "glTexCoord4dv");
  pragma Import (Stdcall, TexCoord4fv, "glTexCoord4fv");
  pragma Import (Stdcall, TexCoord4iv, "glTexCoord4iv");
  pragma Import (Stdcall, TexCoord4sv, "glTexCoord4sv");
  pragma Import (Stdcall, RasterPos2d, "glRasterPos2d");
  pragma Import (Stdcall, RasterPos2f, "glRasterPos2f");
  pragma Import (Stdcall, RasterPos2i, "glRasterPos2i");
  pragma Import (Stdcall, RasterPos2s, "glRasterPos2s");
  pragma Import (Stdcall, RasterPos3d, "glRasterPos3d");
  pragma Import (Stdcall, RasterPos3f, "glRasterPos3f");
  pragma Import (Stdcall, RasterPos3i, "glRasterPos3i");
  pragma Import (Stdcall, RasterPos3s, "glRasterPos3s");
  pragma Import (Stdcall, RasterPos4d, "glRasterPos4d");
  pragma Import (Stdcall, RasterPos4f, "glRasterPos4f");
  pragma Import (Stdcall, RasterPos4i, "glRasterPos4i");
  pragma Import (Stdcall, RasterPos4s, "glRasterPos4s");
  pragma Import (Stdcall, RasterPos2dv, "glRasterPos2dv");
  pragma Import (Stdcall, RasterPos2fv, "glRasterPos2fv");
  pragma Import (Stdcall, RasterPos2iv, "glRasterPos2iv");
  pragma Import (Stdcall, RasterPos2sv, "glRasterPos2sv");
  pragma Import (Stdcall, RasterPos3dv, "glRasterPos3dv");
  pragma Import (Stdcall, RasterPos3fv, "glRasterPos3fv");
  pragma Import (Stdcall, RasterPos3iv, "glRasterPos3iv");
  pragma Import (Stdcall, RasterPos3sv, "glRasterPos3sv");
  pragma Import (Stdcall, RasterPos4dv, "glRasterPos4dv");
  pragma Import (Stdcall, RasterPos4fv, "glRasterPos4fv");
  pragma Import (Stdcall, RasterPos4iv, "glRasterPos4iv");
  pragma Import (Stdcall, RasterPos4sv, "glRasterPos4sv");
  pragma Import (Stdcall, Rectd, "glRectd");
  pragma Import (Stdcall, Rectf, "glRectf");
  pragma Import (Stdcall, Recti, "glRecti");
  pragma Import (Stdcall, Rects, "glRects");
  pragma Import (Stdcall, Rectdv, "glRectdv");
  pragma Import (Stdcall, Rectfv, "glRectfv");
  pragma Import (Stdcall, Rectiv, "glRectiv");
  pragma Import (Stdcall, Rectsv, "glRectsv");
  pragma Import (Stdcall, VertexPointer, "glVertexPointer");
  pragma Import (Stdcall, NormalPointer, "glNormalPointer");
  pragma Import (Stdcall, ColorPointer, "glColorPointer");
  pragma Import (Stdcall, IndexPointer, "glIndexPointer");
  pragma Import (Stdcall, TexCoordPointer, "glTexCoordPointer");
  pragma Import (Stdcall, EdgeFlagPointer, "glEdgeFlagPointer");
  pragma Import (Stdcall, GetPointerv, "glGetPointerv");
  pragma Import (Stdcall, ArrayElement, "glArrayElement");
  pragma Import (Stdcall, DrawArrays, "glDrawArrays");
  pragma Import (Stdcall, DrawElements, "glDrawElements");
  pragma Import (Stdcall, InterleavedArrays, "glInterleavedArrays");
  pragma Import (Stdcall, ShadeModel, "glShadeModel");
  pragma Import (Stdcall, Lightf, "glLightf");
  pragma Import (Stdcall, Lighti, "glLighti");
  pragma Import (Stdcall, Lightfv, "glLightfv");
  pragma Import (Stdcall, Lightiv, "glLightiv");
  pragma Import (Stdcall, GetLightfv, "glGetLightfv");
  pragma Import (Stdcall, GetLightiv, "glGetLightiv");
  pragma Import (Stdcall, LightModelf, "glLightModelf");
  pragma Import (Stdcall, LightModeli, "glLightModeli");
  pragma Import (Stdcall, LightModelfv, "glLightModelfv");
  pragma Import (Stdcall, LightModeliv, "glLightModeliv");
  pragma Import (Stdcall, Materialf, "glMaterialf");
  pragma Import (Stdcall, Materiali, "glMateriali");
  pragma Import (Stdcall, Materialfv, "glMaterialfv");
  pragma Import (Stdcall, Materialiv, "glMaterialiv");
  pragma Import (Stdcall, GetMaterialfv, "glGetMaterialfv");
  pragma Import (Stdcall, GetMaterialiv, "glGetMaterialiv");
  pragma Import (Stdcall, ColorMaterial, "glColorMaterial");
  pragma Import (Stdcall, PixelZoom, "glPixelZoom");
  pragma Import (Stdcall, PixelStoref, "glPixelStoref");
  pragma Import (Stdcall, PixelStorei, "glPixelStorei");
  pragma Import (Stdcall, PixelTransferf, "glPixelTransferf");
  pragma Import (Stdcall, PixelTransferi, "glPixelTransferi");
  pragma Import (Stdcall, PixelMapfv, "glPixelMapfv");
  pragma Import (Stdcall, PixelMapuiv, "glPixelMapuiv");
  pragma Import (Stdcall, PixelMapusv, "glPixelMapusv");
  pragma Import (Stdcall, GetPixelMapfv, "glGetPixelMapfv");
  pragma Import (Stdcall, GetPixelMapuiv, "glGetPixelMapuiv");
  pragma Import (Stdcall, GetPixelMapusv, "glGetPixelMapusv");
  pragma Import (Stdcall, Bitmap, "glBitmap");
  pragma Import (Stdcall, ReadPixels, "glReadPixels");
  pragma Import (Stdcall, DrawPixels, "glDrawPixels");
  pragma Import (Stdcall, CopyPixels, "glCopyPixels");
  pragma Import (Stdcall, StencilFunc, "glStencilFunc");
  pragma Import (Stdcall, StencilMask, "glStencilMask");
  pragma Import (Stdcall, StencilOp, "glStencilOp");
  pragma Import (Stdcall, ClearStencil, "glClearStencil");
  pragma Import (Stdcall, TexGend, "glTexGend");
  pragma Import (Stdcall, TexGenf, "glTexGenf");
  pragma Import (Stdcall, TexGeni, "glTexGeni");
  pragma Import (Stdcall, TexGendv, "glTexGendv");
  pragma Import (Stdcall, TexGenfv, "glTexGenfv");
  pragma Import (Stdcall, TexGeniv, "glTexGeniv");
  pragma Import (Stdcall, GetTexGendv, "glGetTexGendv");
  pragma Import (Stdcall, GetTexGenfv, "glGetTexGenfv");
  pragma Import (Stdcall, GetTexGeniv, "glGetTexGeniv");
  pragma Import (Stdcall, TexEnvf, "glTexEnvf");
  pragma Import (Stdcall, TexEnvi, "glTexEnvi");
  pragma Import (Stdcall, TexEnvfv, "glTexEnvfv");
  pragma Import (Stdcall, TexEnviv, "glTexEnviv");
  pragma Import (Stdcall, GetTexEnvfv, "glGetTexEnvfv");
  pragma Import (Stdcall, GetTexEnviv, "glGetTexEnviv");
  pragma Import (Stdcall, TexParameterf, "glTexParameterf");
  pragma Import (Stdcall, TexParameteri, "glTexParameteri");
  pragma Import (Stdcall, TexParameterfv, "glTexParameterfv");
  pragma Import (Stdcall, TexParameteriv, "glTexParameteriv");
  pragma Import (Stdcall, GetTexParameterfv, "glGetTexParameterfv");
  pragma Import (Stdcall, GetTexParameteriv, "glGetTexParameteriv");
  pragma Import (Stdcall, GetTexLevelParameterfv, "glGetTexLevelParameterfv");
  pragma Import (Stdcall, GetTexLevelParameteriv, "glGetTexLevelParameteriv");
  pragma Import (Stdcall, TexImage1D, "glTexImage1D");
  pragma Import (Stdcall, TexImage2D, "glTexImage2D");
  pragma Import (Stdcall, GetTexImage, "glGetTexImage");
  pragma Import (Stdcall, GenTextures, "glGenTextures");
  pragma Import (Stdcall, DeleteTextures, "glDeleteTextures");
  pragma Import (Stdcall, BindTexture, "glBindTexture");
  pragma Import (Stdcall, PrioritizeTextures, "glPrioritizeTextures");
  pragma Import (Stdcall, AreTexturesResident, "glAreTexturesResident");
  pragma Import (Stdcall, IsTexture, "glIsTexture");
  pragma Import (Stdcall, TexSubImage1D, "glTexSubImage1D");
  pragma Import (Stdcall, TexSubImage2D, "glTexSubImage2D");
  pragma Import (Stdcall, CopyTexImage1D, "glCopyTexImage1D");
  pragma Import (Stdcall, CopyTexImage2D, "glCopyTexImage2D");
  pragma Import (Stdcall, CopyTexSubImage1D, "glCopyTexSubImage1D");
  pragma Import (Stdcall, CopyTexSubImage2D, "glCopyTexSubImage2D");
  pragma Import (Stdcall, Map1d, "glMap1d");
  pragma Import (Stdcall, Map1f, "glMap1f");
  pragma Import (Stdcall, Map2d, "glMap2d");
  pragma Import (Stdcall, Map2f, "glMap2f");
  pragma Import (Stdcall, GetMapdv, "glGetMapdv");
  pragma Import (Stdcall, GetMapfv, "glGetMapfv");
  pragma Import (Stdcall, GetMapiv, "glGetMapiv");
  pragma Import (Stdcall, EvalCoord1d, "glEvalCoord1d");
  pragma Import (Stdcall, EvalCoord1f, "glEvalCoord1f");
  pragma Import (Stdcall, EvalCoord1dv, "glEvalCoord1dv");
  pragma Import (Stdcall, EvalCoord1fv, "glEvalCoord1fv");
  pragma Import (Stdcall, EvalCoord2d, "glEvalCoord2d");
  pragma Import (Stdcall, EvalCoord2f, "glEvalCoord2f");
  pragma Import (Stdcall, EvalCoord2dv, "glEvalCoord2dv");
  pragma Import (Stdcall, EvalCoord2fv, "glEvalCoord2fv");
  pragma Import (Stdcall, MapGrid1d, "glMapGrid1d");
  pragma Import (Stdcall, MapGrid1f, "glMapGrid1f");
  pragma Import (Stdcall, MapGrid2d, "glMapGrid2d");
  pragma Import (Stdcall, MapGrid2f, "glMapGrid2f");
  pragma Import (Stdcall, EvalPoint1, "glEvalPoint1");
  pragma Import (Stdcall, EvalPoint2, "glEvalPoint2");
  pragma Import (Stdcall, EvalMesh1, "glEvalMesh1");
  pragma Import (Stdcall, EvalMesh2, "glEvalMesh2");
  pragma Import (Stdcall, Fogf, "glFogf");
  pragma Import (Stdcall, Fogi, "glFogi");
  pragma Import (Stdcall, Fogfv, "glFogfv");
  pragma Import (Stdcall, Fogiv, "glFogiv");
  pragma Import (Stdcall, FeedbackBuffer, "glFeedbackBuffer");
  pragma Import (Stdcall, PassThrough, "glPassThrough");
  pragma Import (Stdcall, SelectBuffer, "glSelectBuffer");
  pragma Import (Stdcall, InitNames, "glInitNames");
  pragma Import (Stdcall, LoadName, "glLoadName");
  pragma Import (Stdcall, PushName, "glPushName");
  pragma Import (Stdcall, PopName, "glPopName");
  pragma Import (Stdcall, BlendEquationEXT, "glBlendEquationEXT");
  pragma Import (Stdcall, BlendColorEXT, "glBlendColorEXT");
  pragma Import (Stdcall, TexImage3DEXT, "glTexImage3DEXT");
  pragma Import (Stdcall, TexSubImage3DEXT, "glTexSubImage3DEXT");
  pragma Import (Stdcall, CopyTexSubImage3DEXT, "glCopyTexSubImage3DEXT");
  pragma Import (Stdcall, ColorTableEXT, "glColorTableEXT");
  pragma Import (Stdcall, ColorSubTableEXT, "glColorSubTableEXT");
  pragma Import (Stdcall, GetColorTableEXT, "glGetColorTableEXT");
  pragma Import (Stdcall, GetColorTableParameterfvEXT, "glGetColorTableParameterfvEXT");
  pragma Import (Stdcall, GetColorTableParameterivEXT, "glGetColorTableParameterivEXT");
  pragma Import (Stdcall, PointParameterfEXT, "glPointParameterfEXT");
  pragma Import (Stdcall, PointParameterfvEXT, "glPointParameterfvEXT");
  pragma Import (Stdcall, WindowPos2iMESA, "glWindowPos2iMESA");
  pragma Import (Stdcall, WindowPos2sMESA, "glWindowPos2sMESA");
  pragma Import (Stdcall, WindowPos2fMESA, "glWindowPos2fMESA");
  pragma Import (Stdcall, WindowPos2dMESA, "glWindowPos2dMESA");
  pragma Import (Stdcall, WindowPos2ivMESA, "glWindowPos2ivMESA");
  pragma Import (Stdcall, WindowPos2svMESA, "glWindowPos2svMESA");
  pragma Import (Stdcall, WindowPos2fvMESA, "glWindowPos2fvMESA");
  pragma Import (Stdcall, WindowPos2dvMESA, "glWindowPos2dvMESA");
  pragma Import (Stdcall, WindowPos3iMESA, "glWindowPos3iMESA");
  pragma Import (Stdcall, WindowPos3sMESA, "glWindowPos3sMESA");
  pragma Import (Stdcall, WindowPos3fMESA, "glWindowPos3fMESA");
  pragma Import (Stdcall, WindowPos3dMESA, "glWindowPos3dMESA");
  pragma Import (Stdcall, WindowPos3ivMESA, "glWindowPos3ivMESA");
  pragma Import (Stdcall, WindowPos3svMESA, "glWindowPos3svMESA");
  pragma Import (Stdcall, WindowPos3fvMESA, "glWindowPos3fvMESA");
  pragma Import (Stdcall, WindowPos3dvMESA, "glWindowPos3dvMESA");
  pragma Import (Stdcall, WindowPos4iMESA, "glWindowPos4iMESA");
  pragma Import (Stdcall, WindowPos4sMESA, "glWindowPos4sMESA");
  pragma Import (Stdcall, WindowPos4fMESA, "glWindowPos4fMESA");
  pragma Import (Stdcall, WindowPos4dMESA, "glWindowPos4dMESA");
  pragma Import (Stdcall, WindowPos4ivMESA, "glWindowPos4ivMESA");
  pragma Import (Stdcall, WindowPos4svMESA, "glWindowPos4svMESA");
  pragma Import (Stdcall, WindowPos4fvMESA, "glWindowPos4fvMESA");
  pragma Import (Stdcall, WindowPos4dvMESA, "glWindowPos4dvMESA");
  pragma Import (Stdcall, ResizeBuffersMESA, "glResizeBuffersMESA");

end GL;
