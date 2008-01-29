-- Change log:

-- GdM: 2005, 2006: Get, Project also in Ada style

-- GdM: 29-Jan-2004 : added GLU.Get, (glGetdoublev) for GLU's matrices

-- GdM: 11-Apr-2002 : * adapated to the "GL..." and "...4x" -less GL
--                    * "glu..." and other useless C prefixes removed
--                    * removing of "...4f" -style siffixes in progress

-- Changed by MB for Windows 95, 980529
-- C replaced by Stdcall
--
-- OpenGL 1.1 Ada binding, package GLU
--
-- W. M. Richards, NiEstu, Phoenix AZ, December 1997
--
-- Converted from Brian Paul's Mesa package glu.h header file, version 2,5.
-- As noted below in Brian's original comments, this code is distributed
-- under the terms of the GNU Library General Public License.
--
-- Version 0.1, 21 December 1997
--
--
-- Here are the original glu.h comments:
--
-- Mesa 3-D graphics library
-- Version:  2.4
-- Copyright (C) 1995-1997  Brian Paul
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Library General Public License for more details.
--
-- You should have received a copy of the GNU Library General Public
-- License along with this library; if not, write to the Free
-- Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

with GL;

package GLU is

  VERSION_1_1                     : constant := 1;


  -- The GLU boolean constants
  GL_FALSE                           : constant := GL.GL_FALSE;
  GL_TRUE                            : constant := GL.GL_TRUE;

  ------------------------------------------------------------------------------

  type viewPortRec is record
     X, Y:           aliased GL.int;
     Width, Height:  aliased GL.int;
  end record;

  type floatMatrix      is array ( 0..3, 0..3 ) of GL.float;
  type doubleMatrix     is array ( 0..3, 0..3 ) of GL.double;

  type viewPortRecPtr   is access all viewPortRec;
  type floatMatrixPtr   is access all floatMatrix;
  type doubleMatrixPtr  is access all doubleMatrix;

  type GLUquadricObj      is private;
  type GLUtriangulatorObj is private;
  type GLUnurbsObj        is private;

  type GLUquadricObjPtr      is access all GLUquadricObj;
  type GLUtriangulatorObjPtr is access all GLUtriangulatorObj;
  type GLUnurbsObjPtr        is access all GLUnurbsObj;

  ------------------------------------------------------------------------------


  -- Error string
  type ErrorEnm is
  (
     GL_NO_ERROR,
     GL_INVALID_ENUM,
     GL_INVALID_VALUE,
     GL_INVALID_OPERATION,
     GL_STACK_OVERFLOW,
     GL_STACK_UNDERFLOW,
     GL_OUT_OF_MEMORY,
     GLU_INVALID_ENUM,
     GLU_INVALID_VALUE,
     GLU_OUT_OF_MEMORY,
     GLU_INCOMPATIBLE_GL_VERSION
  );
  for ErrorEnm use
  (
     GL_NO_ERROR                                => 16#0000#,
     GL_INVALID_ENUM                            => 16#0500#,
     GL_INVALID_VALUE                           => 16#0501#,
     GL_INVALID_OPERATION                       => 16#0502#,
     GL_STACK_OVERFLOW                          => 16#0503#,
     GL_STACK_UNDERFLOW                         => 16#0504#,
     GL_OUT_OF_MEMORY                           => 16#0505#,
     GLU_INVALID_ENUM                           => 16#18A24#,
     GLU_INVALID_VALUE                          => 16#18A25#,
     GLU_OUT_OF_MEMORY                          => 16#18A26#,
     GLU_INCOMPATIBLE_GL_VERSION                => 16#18A27#  -- Mesa-specific?
  );
  for ErrorEnm'Size use GL.enum'Size;

  function ErrorString (errorCode: ErrorEnm)
  return GL.ubytePtr;

  function ErrorString (errorCode: gl.ErrorEnm)
  return GL.ubytePtr;


  -- Scale image
  function ScaleImage (format   : GL.PixelFormatEnm;
                          widthin  : GL.int;
                          heightin : GL.int;
                          typein   : GL.PixelDataTypeEnm;
                          datain   : GL.pointer;
                          widthout : GL.int;
                          heightout: GL.int;
                          typeout  : GL.PixelDataTypeEnm;
                          dataout  : GL.pointer)
  return GL.int;


  -- Build mipmaps
  function Build1DMipmaps (target    : GL.TargetTex1DOnlyEnm;
                              components: GL.int;
                              width     : GL.int;
                              format    : GL.TexPixelFormatEnm;
                              c_type    : GL.PixelDataTypeEnm;
                              data      : GL.pointer)
  return GL.int;

  function Build2DMipmaps (target    : GL.TargetTex2DOnlyEnm;
                              components: GL.int;
                              width     : GL.int;
                              height    : GL.int;
                              format    : GL.TexPixelFormatEnm;
                              c_type    : GL.PixelDataTypeEnm;
                              data      : GL.pointer)
  return GL.int;


  -- Quadric objects
  type DrawStyleEnm is
  (
     GLU_POINT,
     GLU_LINE,
     GLU_FILL,
     GLU_SILHOUETTE
  );
  for DrawStyleEnm use
  (
     GLU_POINT                                  => 16#186AA#,
     GLU_LINE                                   => 16#186AB#,
     GLU_FILL                                   => 16#186AC#,
     GLU_SILHOUETTE                             => 16#186AD#
  );
  for DrawStyleEnm'Size use GL.enum'Size;

  type OrientationEnm is
  (
     GLU_OUTSIDE,
     GLU_INSIDE
  );
  for OrientationEnm use
  (
     GLU_OUTSIDE                                => 16#186B4#,
     GLU_INSIDE                                 => 16#186B5#
  );
  for OrientationEnm'Size use GL.enum'Size;

  type NormalsEnm is
  (
     GLU_SMOOTH,
     GLU_FLAT,
     GLU_NONE
  );
  for NormalsEnm use
  (
     GLU_SMOOTH                                 => 16#186A0#,
     GLU_FLAT                                   => 16#186A1#,
     GLU_NONE                                   => 16#186A2#
  );
  for NormalsEnm'Size use GL.enum'Size;

  type CallbackEnm is
  (
     GLU_ERROR
  );
  for CallbackEnm use
  (
     GLU_ERROR                                  => 16#18707#
  );
  for CallbackEnm'Size use GL.enum'Size;

  type QuadricCallbackFunction is access procedure (Error:  ErrorEnm);

  function NewQuadric
  return GLUquadricObjPtr;

  procedure DeleteQuadric (state: GLUquadricObjPtr);

  procedure QuadricDrawStyle (quadObject: GLUquadricObjPtr;
                                 drawStyle : DrawStyleEnm);

  procedure QuadricOrientation (quadObject : GLUquadricObjPtr;
                                   orientation: OrientationEnm);

  procedure QuadricNormals (quadObject: GLUquadricObjPtr;
                               normals   : NormalsEnm);

  procedure QuadricTexture (quadObject   : GLUquadricObjPtr;
                               textureCoords: GL.GL_boolean);

  procedure QuadricCallback (qobj : GLUquadricObjPtr;
                                which: CallbackEnm;
                                fn   : QuadricCallbackFunction);

  procedure Cylinder (qobj      : GLUquadricObjPtr;
                         baseRadius: GL.double;
                         topRadius : GL.double;
                         height    : GL.double;
                         slices    : GL.int;
                         stacks    : GL.int);

  procedure Sphere (qobj  : GLUquadricObjPtr;
                       radius: GL.double;
                       slices: GL.int;
                       stacks: GL.int);

  procedure Disk (qobj       : GLUquadricObjPtr;
                     innerRadius: GL.double;
                     outerRadius: GL.double;
                     slices     : GL.int;
                     loops      : GL.int);

  procedure PartialDisk (qobj       : GLUquadricObjPtr;
                            innerRadius: GL.double;
                            outerRadius: GL.double;
                            slices     : GL.int;
                            loops      : GL.int;
                            startAngle : GL.double;
                            sweepAngle : GL.double);


  -- Non-uniform rational B-splines (NURBS)
  type NurbsPropertyEnm is
  (
     GLU_AUTO_LOAD_MATRIX,
     GLU_CULLING,
     GLU_PARAMETRIC_TOLERANCE,
     GLU_SAMPLING_TOLERANCE,
     GLU_DISPLAY_MODE,
     GLU_SAMPLING_METHOD,
     GLU_U_STEP,
     GLU_V_STEP
  );
  for NurbsPropertyEnm use
  (
     GLU_AUTO_LOAD_MATRIX                       => 16#18768#,
     GLU_CULLING                                => 16#18769#,
     GLU_PARAMETRIC_TOLERANCE                   => 16#1876A#,
     GLU_SAMPLING_TOLERANCE                     => 16#1876B#,
     GLU_DISPLAY_MODE                           => 16#1876C#,
     GLU_SAMPLING_METHOD                        => 16#1876D#,
     GLU_U_STEP                                 => 16#1876E#,
     GLU_V_STEP                                 => 16#1876F#
  );
  for NurbsPropertyEnm'Size use GL.enum'Size;

  type NurbsDisplayModeEnm is
  (
     GLU_FILL,
     GLU_OUTLINE_POLYGON,
     GLU_OUTLINE_PATCH
  );
  for NurbsDisplayModeEnm use
  (
     GLU_FILL                                   => 16#186AC#,
     GLU_OUTLINE_POLYGON                        => 16#18790#,
     GLU_OUTLINE_PATCH                          => 16#18791#
  );
  for NurbsDisplayModeEnm'Size use GL.enum'Size;

  -- NURBS property values
  GLU_PATH_LENGTH                     : constant := 16#18777#;
  GLU_PARAMETRIC_ERROR                : constant := 16#18778#;
  GLU_DOMAIN_DISTANCE                 : constant := 16#18779#;

  type NurbsErrorEnm is
  (
     GLU_NURBS_ERROR1,                                       -- spline order un-supported ,
     GLU_NURBS_ERROR2,                                       -- too few knots ,
     GLU_NURBS_ERROR3,                                       -- valid knot range is empty ,
     GLU_NURBS_ERROR4,                                       -- decreasing knot sequence ,
     GLU_NURBS_ERROR5,                                       -- knot multiplicity > spline order ,
     GLU_NURBS_ERROR6,                                       -- endcurve() must follow bgncurve() ,
     GLU_NURBS_ERROR7,                                       -- bgncurve() must precede endcurve() ,
     GLU_NURBS_ERROR8,                                       -- ctrlarray or knot vector is NULL ,
     GLU_NURBS_ERROR9,                                       -- can't draw pwlcurves ,
     GLU_NURBS_ERROR10,                                      -- missing gluNurbsCurve() ,
     GLU_NURBS_ERROR11,                                      -- missing gluNurbsSurface() ,
     GLU_NURBS_ERROR12,                                      -- endtrim() must precede endsurface() ,
     GLU_NURBS_ERROR13,                                      -- bgnsurface() must precede endsurface() ,
     GLU_NURBS_ERROR14,                                      -- curve of improper type passed as trim curve ,
     GLU_NURBS_ERROR15,                                      -- bgnsurface() must precede bgntrim() ,
     GLU_NURBS_ERROR16,                                      -- endtrim() must follow bgntrim() ,
     GLU_NURBS_ERROR17,                                      -- bgntrim() must precede endtrim(),
     GLU_NURBS_ERROR18,                                      -- invalid or missing trim curve,
     GLU_NURBS_ERROR19,                                      -- bgntrim() must precede pwlcurve() ,
     GLU_NURBS_ERROR20,                                      -- pwlcurve referenced twice,
     GLU_NURBS_ERROR21,                                      -- pwlcurve and nurbscurve mixed ,
     GLU_NURBS_ERROR22,                                      -- improper usage of trim data type ,
     GLU_NURBS_ERROR23,                                      -- nurbscurve referenced twice ,
     GLU_NURBS_ERROR24,                                      -- nurbscurve and pwlcurve mixed ,
     GLU_NURBS_ERROR25,                                      -- nurbssurface referenced twice ,
     GLU_NURBS_ERROR26,                                      -- invalid property ,
     GLU_NURBS_ERROR27,                                      -- endsurface() must follow bgnsurface() ,
     GLU_NURBS_ERROR28,                                      -- intersecting or misoriented trim curves ,
     GLU_NURBS_ERROR29,                                      -- intersecting trim curves ,
     GLU_NURBS_ERROR30,                                      -- UNUSED ,
     GLU_NURBS_ERROR31,                                      -- unconnected trim curves ,
     GLU_NURBS_ERROR32,                                      -- unknown knot error ,
     GLU_NURBS_ERROR33,                                      -- negative vertex count encountered ,
     GLU_NURBS_ERROR34,                                      -- negative byte-stride ,
     GLU_NURBS_ERROR35,                                      -- unknown type descriptor ,
     GLU_NURBS_ERROR36,                                      -- null control point reference ,
     GLU_NURBS_ERROR37                                       -- duplicate point on pwlcurve
  );
  for NurbsErrorEnm use
  (
     GLU_NURBS_ERROR1                           => 16#1879B#,
     GLU_NURBS_ERROR2                           => 16#1879C#,
     GLU_NURBS_ERROR3                           => 16#1879D#,
     GLU_NURBS_ERROR4                           => 16#1879E#,
     GLU_NURBS_ERROR5                           => 16#1879F#,
     GLU_NURBS_ERROR6                           => 16#187A0#,
     GLU_NURBS_ERROR7                           => 16#187A1#,
     GLU_NURBS_ERROR8                           => 16#187A2#,
     GLU_NURBS_ERROR9                           => 16#187A3#,
     GLU_NURBS_ERROR10                          => 16#187A4#,
     GLU_NURBS_ERROR11                          => 16#187A5#,
     GLU_NURBS_ERROR12                          => 16#187A6#,
     GLU_NURBS_ERROR13                          => 16#187A7#,
     GLU_NURBS_ERROR14                          => 16#187A8#,
     GLU_NURBS_ERROR15                          => 16#187A9#,
     GLU_NURBS_ERROR16                          => 16#187AA#,
     GLU_NURBS_ERROR17                          => 16#187AB#,
     GLU_NURBS_ERROR18                          => 16#187AC#,
     GLU_NURBS_ERROR19                          => 16#187AD#,
     GLU_NURBS_ERROR20                          => 16#187AE#,
     GLU_NURBS_ERROR21                          => 16#187AF#,
     GLU_NURBS_ERROR22                          => 16#187B0#,
     GLU_NURBS_ERROR23                          => 16#187B1#,
     GLU_NURBS_ERROR24                          => 16#187B2#,
     GLU_NURBS_ERROR25                          => 16#187B3#,
     GLU_NURBS_ERROR26                          => 16#187B4#,
     GLU_NURBS_ERROR27                          => 16#187B5#,
     GLU_NURBS_ERROR28                          => 16#187B6#,
     GLU_NURBS_ERROR29                          => 16#187B7#,
     GLU_NURBS_ERROR30                          => 16#187B8#,
     GLU_NURBS_ERROR31                          => 16#187B9#,
     GLU_NURBS_ERROR32                          => 16#187BA#,
     GLU_NURBS_ERROR33                          => 16#187BB#,
     GLU_NURBS_ERROR34                          => 16#187BC#,
     GLU_NURBS_ERROR35                          => 16#187BD#,
     GLU_NURBS_ERROR36                          => 16#187BE#,
     GLU_NURBS_ERROR37                          => 16#187BF#
  );
  for NurbsErrorEnm'Size use GL.enum'Size;

  type PwlCurveTypeEnm is
  (
     GLU_MAP1_TRIM_2,
     GLU_MAP1_TRIM_3
  );
  for PwlCurveTypeEnm use
  (
     GLU_MAP1_TRIM_2                            => 16#18772#,
     GLU_MAP1_TRIM_3                            => 16#18773#
  );
  for PwlCurveTypeEnm'Size use GL.enum'Size;

  type NurbsCallbackFunction is access procedure (Error:  NurbsErrorEnm);

  function NewNurbsRenderer
  return GLUnurbsObjPtr;

  procedure DeleteNurbsRenderer (nobj: GLUnurbsObjPtr);

  procedure LoadSamplingMatrices (nobj       : GLUnurbsObjPtr;
                                     modelMatrix: floatMatrixPtr;
                                     projMatrix : floatMatrixPtr;
                                     viewport   : viewPortRecPtr);

  procedure NurbsProperty (nobj    : GLUnurbsObjPtr;
                              property: NurbsPropertyEnm;
                              value   : GL.float);

  procedure GetNurbsProperty (nobj    : GLUnurbsObjPtr;
                                 property: NurbsPropertyEnm;
                                 value   : GL.floatPtr);

  procedure BeginCurve (nobj: GLUnurbsObjPtr);

  procedure EndCurve (nobj: GLUnurbsObjPtr);

  procedure NurbsCurve (nobj    : GLUnurbsObjPtr;
                           nknots  : GL.int;
                           knot    : GL.floatPtr;
                           stride  : GL.int;
                           ctlarray: GL.floatPtr;
                           order   : GL.int;
                           c_type  : GL.Map1TargetEnm);

  procedure BeginSurface (nobj: GLUnurbsObjPtr);

  procedure EndSurface (nobj: GLUnurbsObjPtr);

  procedure NurbsSurface (nobj       : GLUnurbsObjPtr;
                             sknot_count: GL.int;
                             sknot      : GL.floatPtr;
                             tknot_count: GL.int;
                             tknot      : GL.floatPtr;
                             s_stride   : GL.int;
                             t_stride   : GL.int;
                             ctlarray   : GL.floatPtr;
                             sorder     : GL.int;
                             torder     : GL.int;
                             c_type     : GL.Map2TargetEnm);

  procedure BeginTrim (nobj: GLUnurbsObjPtr);

  procedure EndTrim (nobj: GLUnurbsObjPtr);

  procedure PwlCurve (nobj   : GLUnurbsObjPtr;
                         count  : GL.int;
                         c_array: GL.floatPtr;
                         stride : GL.int;
                         c_type : PwlCurveTypeEnm);

  procedure NurbsCallback (nobj : GLUnurbsObjPtr;
                              which: CallbackEnm;
                              fn   : NurbsCallbackFunction);


  -- Polygon tesselation
  type TessCallbackEnm is
  (
     GLU_BEGIN,
     GLU_VERTEX,
     GLU_END,
     GLU_ERROR,
     GLU_EDGE_FLAG
  );
  for TessCallbackEnm use
  (
     GLU_BEGIN                                  => 16#18704#,  -- Note: some implementations use "GLU_TESS_..."
     GLU_VERTEX                                 => 16#18705#,
     GLU_END                                    => 16#18706#,
     GLU_ERROR                                  => 16#18707#,
     GLU_EDGE_FLAG                              => 16#18708#
  );
  for TessCallbackEnm'Size use GL.enum'Size;

  type TessBeginEnm is
  (
     GL_LINE_LOOP,
     GL_TRIANGLES,
     GL_TRIANGLE_STRIP,
     GL_TRIANGLE_FAN
  );
  for TessBeginEnm use
  (
     GL_LINE_LOOP                               => 16#0002#,
     GL_TRIANGLES                               => 16#0004#,
     GL_TRIANGLE_STRIP                          => 16#0005#,
     GL_TRIANGLE_FAN                            => 16#0006#
  );
  for TessBeginEnm'Size use GL.enum'Size;
  type TessBeginCallbackFunction is access procedure (ObjType:  TessBeginEnm);

  type TessVertexCallbackFunction is access procedure (VertexData:  GL.pointer);

  type TessEndCallbackFunction is access procedure;

  type TessErrorEnm is
  (
     GLU_TESS_ERROR1,                                        -- missing gluEndPolygon ,
     GLU_TESS_ERROR2,                                        -- missing gluBeginPolygon ,
     GLU_TESS_ERROR3,                                        -- misoriented contour ,
     GLU_TESS_ERROR4,                                        -- vertex/edge intersection ,
     GLU_TESS_ERROR5,                                        -- misoriented or self-intersecting loops ,
     GLU_TESS_ERROR6,                                        -- coincident vertices ,
     GLU_TESS_ERROR7,                                        -- all vertices collinear ,
     GLU_TESS_ERROR8,                                        -- intersecting edges ,
     GLU_TESS_ERROR9                                         -- not coplanar contours
  );
  for TessErrorEnm use
  (
     GLU_TESS_ERROR1                            => 16#18737#,
     GLU_TESS_ERROR2                            => 16#18738#,
     GLU_TESS_ERROR3                            => 16#18739#,
     GLU_TESS_ERROR4                            => 16#1873A#,
     GLU_TESS_ERROR5                            => 16#1873B#,
     GLU_TESS_ERROR6                            => 16#1873C#,
     GLU_TESS_ERROR7                            => 16#1873D#,
     GLU_TESS_ERROR8                            => 16#1873E#,
     GLU_TESS_ERROR9                            => 16#1873F#
  );
  for TessErrorEnm'Size use GL.enum'Size;
  type TessErrorCallbackFunction is access procedure (Error:  TessErrorEnm);

  type TessEdgeFlagCallbackFunction is access procedure (Flag:  GL.GL_boolean);

  type ContourTypeEnm is
  (
     GLU_CW,
     GLU_CCW,
     GLU_INTERIOR,
     GLU_EXTERIOR,
     GLU_UNKNOWN
  );
  for ContourTypeEnm use
  (
     GLU_CW                                     => 16#18718#,
     GLU_CCW                                    => 16#18719#,
     GLU_INTERIOR                               => 16#1871A#,
     GLU_EXTERIOR                               => 16#1871B#,
     GLU_UNKNOWN                                => 16#1871C#
  );
  for ContourTypeEnm'Size use GL.enum'Size;

  function NewTess
  return GLUtriangulatorObjPtr;

  procedure TessCallback (tobj : GLUtriangulatorObjPtr;
                             which: TessCallbackEnm;
                             fn   : TessBeginCallbackFunction);
  procedure TessCallback (tobj : GLUtriangulatorObjPtr;
                             which: TessCallbackEnm;
                             fn   : TessVertexCallbackFunction);
  procedure TessCallback (tobj : GLUtriangulatorObjPtr;
                             which: TessCallbackEnm;
                             fn   : TessEndCallbackFunction);
  procedure TessCallback (tobj : GLUtriangulatorObjPtr;
                             which: TessCallbackEnm;
                             fn   : TessErrorCallbackFunction);
  procedure TessCallback (tobj : GLUtriangulatorObjPtr;
                             which: TessCallbackEnm;
                             fn   : TessEdgeFlagCallbackFunction);

  procedure DeleteTess (tobj: GLUtriangulatorObjPtr);

  procedure BeginPolygon (tobj: GLUtriangulatorObjPtr);

  procedure EndPolygon (tobj: GLUtriangulatorObjPtr);

  procedure NextContour (tobj  : GLUtriangulatorObjPtr;
                            c_type: ContourTypeEnm);

  procedure TessVertex (tobj: GLUtriangulatorObjPtr;
                           v   : GL.doublePtr;
                           data: GL.pointer);


  -- GLU strings
  type StringEnm is
  (
     GLU_VERSION,
     GLU_EXTENSIONS
  );
  for StringEnm use
  (
     GLU_VERSION                                => 16#189C0#,
     GLU_EXTENSIONS                             => 16#189C1#
  );
  for StringEnm'Size use GL.enum'Size;

  function GetString (name: StringEnm)
  return GL.ubytePtr;


  -- Projections
  procedure LookAt (eyex   : GL.double;
                    eyey   : GL.double;
                    eyez   : GL.double;
                    centerx: GL.double;
                    centery: GL.double;
                    centerz: GL.double;
                    upx    : GL.double;
                    upy    : GL.double;
                    upz    : GL.double);

  procedure Ortho2D (left  : GL.double;
                     right : GL.double;
                     bottom: GL.double;
                     top   : GL.double);

  procedure Perspective (fovy  : GL.double;
                         aspect: GL.double;
                         zNear : GL.double;
                         zFar  : GL.double);

  procedure PickMatrix (x       : GL.double;
                        y       : GL.double;
                        width   : GL.double;
                        height  : GL.double;
                        viewport: viewPortRecPtr);

  function Project (objx       : GL.double;
                    objy       : GL.double;
                    objz       : GL.double;
                    modelMatrix: doubleMatrixPtr;
                    projMatrix : doubleMatrixPtr;
                    viewport   : viewPortRecPtr;
                    winx       : GL.doublePtr;
                    winy       : GL.doublePtr;
                    winz       : GL.doublePtr)
  return GL.int;
  pragma Import (Stdcall, Project, "gluProject");

  -- Project, Ada style

  procedure Project (objx       : GL.double;
                     objy       : GL.double;
                     objz       : GL.double;
                     modelMatrix: doubleMatrix;
                     projMatrix : doubleMatrix;
                     viewport   : viewPortRec;
                     winx       : out GL.double;
                     winy       : out GL.double;
                     winz       : out GL.double;
                     result     : out Boolean );

  function UnProject (winx       : GL.double;
                      winy       : GL.double;
                      winz       : GL.double;
                      modelMatrix: doubleMatrixPtr;
                      projMatrix : doubleMatrixPtr;
                      viewport   : viewPortRecPtr;
                      objx       : GL.doublePtr;
                      objy       : GL.doublePtr;
                      objz       : GL.doublePtr)
  return GL.int;

  -- GLU.Get's

  procedure Get (pname : GL.ParameterNameEnm;
                 params: doubleMatrixPtr);

  procedure Get (pname : GL.ParameterNameEnm;
                 params: out doubleMatrix);

  procedure Get (pname : GL.ParameterNameEnm;
                 params: viewPortRecPtr);

  procedure Get (params: out viewPortRec);

  ------------------------------------------------------------------------------

  private

  type GLUquadricObj      is record null; end record;
  type GLUtriangulatorObj is record null; end record;
  type GLUnurbsObj        is record null; end record;

  pragma Import (Stdcall, LookAt, "gluLookAt");
  pragma Import (Stdcall, Ortho2D, "gluOrtho2D");
  pragma Import (Stdcall, Perspective, "gluPerspective");
  pragma Import (Stdcall, PickMatrix, "gluPickMatrix");
  -- pragma Import (Stdcall, Project, "gluProject");
  pragma Import (Stdcall, UnProject, "gluUnProject");

  function ErrorString_1 (errorCode: ErrorEnm)  return GL.ubytePtr;
  function ErrorString   (errorCode: ErrorEnm)  return GL.ubytePtr renames ErrorString_1;
  pragma Import (Stdcall, ErrorString_1, "gluErrorString");

  function ErrorString_2 (errorCode: gl.ErrorEnm) return GL.ubytePtr;
  function ErrorString   (errorCode: gl.ErrorEnm) return GL.ubytePtr renames ErrorString_2;
  pragma Import (Stdcall, ErrorString_2, "gluErrorString");

  pragma Import (Stdcall, ScaleImage, "gluScaleImage");
  pragma Import (Stdcall, Build1DMipmaps, "gluBuild1DMipmaps");
  pragma Import (Stdcall, Build2DMipmaps, "gluBuild2DMipmaps");
  pragma Import (Stdcall, NewQuadric, "gluNewQuadric");
  pragma Import (Stdcall, DeleteQuadric, "gluDeleteQuadric");
  pragma Import (Stdcall, QuadricDrawStyle, "gluQuadricDrawStyle");
  pragma Import (Stdcall, QuadricOrientation, "gluQuadricOrientation");
  pragma Import (Stdcall, QuadricNormals, "gluQuadricNormals");
  pragma Import (Stdcall, QuadricTexture, "gluQuadricTexture");
  pragma Import (Stdcall, QuadricCallback, "gluQuadricCallback");
  pragma Import (Stdcall, Cylinder, "gluCylinder");
  pragma Import (Stdcall, Sphere, "gluSphere");
  pragma Import (Stdcall, Disk, "gluDisk");
  pragma Import (Stdcall, PartialDisk, "gluPartialDisk");
  pragma Import (Stdcall, NewNurbsRenderer, "gluNewNurbsRenderer");
  pragma Import (Stdcall, DeleteNurbsRenderer, "gluDeleteNurbsRenderer");
  pragma Import (Stdcall, LoadSamplingMatrices, "gluLoadSamplingMatrices");
  pragma Import (Stdcall, NurbsProperty, "gluNurbsProperty");
  pragma Import (Stdcall, GetNurbsProperty, "gluGetNurbsProperty");
  pragma Import (Stdcall, BeginCurve, "gluBeginCurve");
  pragma Import (Stdcall, EndCurve, "gluEndCurve");
  pragma Import (Stdcall, NurbsCurve, "gluNurbsCurve");
  pragma Import (Stdcall, BeginSurface, "gluBeginSurface");
  pragma Import (Stdcall, EndSurface, "gluEndSurface");
  pragma Import (Stdcall, NurbsSurface, "gluNurbsSurface");
  pragma Import (Stdcall, BeginTrim, "gluBeginTrim");
  pragma Import (Stdcall, EndTrim, "gluEndTrim");
  pragma Import (Stdcall, PwlCurve, "gluPwlCurve");
  pragma Import (Stdcall, NurbsCallback, "gluNurbsCallback");
  pragma Import (Stdcall, NewTess, "gluNewTess");
  pragma Import (Stdcall, TessCallback, "gluTessCallback");
  pragma Import (Stdcall, DeleteTess, "gluDeleteTess");
  pragma Import (Stdcall, BeginPolygon, "gluBeginPolygon");
  pragma Import (Stdcall, EndPolygon, "gluEndPolygon");
  pragma Import (Stdcall, NextContour, "gluNextContour");
  pragma Import (Stdcall, TessVertex, "gluTessVertex");
  pragma Import (Stdcall, GetString, "gluGetString");

  -- GL procedures for GLU types:

  -- Wrappers for Get (doubleMatrix)
  procedure GetDoublev (pname : GL.ParameterNameEnm;
                        params: doubleMatrixPtr);
  procedure Get (pname : GL.ParameterNameEnm;
                 params: doubleMatrixPtr) renames GetDoublev;
  pragma Import (Stdcall, GetDoublev, "glGetDoublev");

  -- Wrappers for Get (viewPortRec)

  procedure GetIntegerv (pname : GL.ParameterNameEnm;
                         params: viewPortRecPtr);
  procedure Get (pname : GL.ParameterNameEnm;
                 params: viewPortRecPtr) renames GetIntegerv;
  pragma Import (Stdcall, GetIntegerv, "glGetIntegerv");

end GLU;
