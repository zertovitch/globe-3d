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
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
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


with System;
with OpenGL;

package OpenGL.GLU is

   GLU_EXT_OBJECT_SPACE_TESS : constant  := 1;
   GLU_EXT_NURBS_TESSELLATOR : constant  := 1;
   GLU_FALSE                 : constant  := 8#0000#;
   GLU_TRUE                  : constant  := 1;
   GLU_VERSION_1_1           : constant  := 1;
   GLU_VERSION_1_2           : constant  := 1;
   GLU_VERSION_1_3           : constant  := 1;
   GLU_VERSION               : constant  := 100800;
   GLU_EXTENSIONS            : constant  := 100801;
   GLU_INVALID_ENUM          : constant  := 100900;
   GLU_INVALID_VALUE         : constant  := 100901;
   GLU_OUT_OF_MEMORY         : constant  := 100902;
   GLU_INVALID_OPERATION     : constant  := 100904;
   GLU_OUTLINE_POLYGON       : constant  := 100240;
   GLU_OUTLINE_PATCH         : constant  := 100241;
   GLU_NURBS_ERROR           : constant  := 100103;
   GLU_ERROR                 : constant  := 100103;
   GLU_NURBS_BEGIN           : constant  := 100164;
   GLU_NURBS_BEGIN_EXT       : constant  := 100164;
   GLU_NURBS_VERTEX          : constant  := 100165;
   GLU_NURBS_VERTEX_EXT      : constant  := 100165;
   GLU_NURBS_NORMAL          : constant  := 100166;
   GLU_NURBS_NORMAL_EXT      : constant  := 100166;
   GLU_NURBS_COLOR           : constant  := 100167;
   GLU_NURBS_COLOR_EXT       : constant  := 100167;
   GLU_NURBS_TEXTURE_COORD   : constant  := 100168;
   GLU_NURBS_TEX_COORD_EXT   : constant  := 100168;
   GLU_NURBS_END             : constant  := 100169;
   GLU_NURBS_END_EXT         : constant  := 100169;
   GLU_NURBS_BEGIN_DATA      : constant  := 100170;
   GLU_NURBS_BEGIN_DATA_EXT  : constant  := 100170;
   GLU_NURBS_VERTEX_DATA     : constant  := 100171;
   GLU_NURBS_VERTEX_DATA_EXT : constant  := 100171;
   GLU_NURBS_NORMAL_DATA     : constant  := 100172;
   GLU_NURBS_NORMAL_DATA_EXT : constant  := 100172;
   GLU_NURBS_COLOR_DATA      : constant  := 100173;
   GLU_NURBS_COLOR_DATA_EXT  : constant  := 100173;
   GLU_NURBS_TEXTURE_COORD_DATA : constant  := 100174;
   GLU_NURBS_TEX_COORD_DATA_EXT : constant  := 100174;
   GLU_NURBS_END_DATA           : constant  := 100175;
   GLU_NURBS_END_DATA_EXT       : constant  := 100175;
   GLU_NURBS_ERROR1             : constant  := 100251;
   GLU_NURBS_ERROR2             : constant  := 100252;
   GLU_NURBS_ERROR3             : constant  := 100253;
   GLU_NURBS_ERROR4             : constant  := 100254;
   GLU_NURBS_ERROR5             : constant  := 100255;
   GLU_NURBS_ERROR6             : constant  := 100256;
   GLU_NURBS_ERROR7             : constant  := 100257;
   GLU_NURBS_ERROR8             : constant  := 100258;
   GLU_NURBS_ERROR9             : constant  := 100259;
   GLU_NURBS_ERROR10            : constant  := 100260;
   GLU_NURBS_ERROR11            : constant  := 100261;
   GLU_NURBS_ERROR12            : constant  := 100262;
   GLU_NURBS_ERROR13            : constant  := 100263;
   GLU_NURBS_ERROR14            : constant  := 100264;
   GLU_NURBS_ERROR15            : constant  := 100265;
   GLU_NURBS_ERROR16            : constant  := 100266;
   GLU_NURBS_ERROR17            : constant  := 100267;
   GLU_NURBS_ERROR18            : constant  := 100268;
   GLU_NURBS_ERROR19            : constant  := 100269;
   GLU_NURBS_ERROR20            : constant  := 100270;
   GLU_NURBS_ERROR21            : constant  := 100271;
   GLU_NURBS_ERROR22            : constant  := 100272;
   GLU_NURBS_ERROR23            : constant  := 100273;
   GLU_NURBS_ERROR24            : constant  := 100274;
   GLU_NURBS_ERROR25            : constant  := 100275;
   GLU_NURBS_ERROR26            : constant  := 100276;
   GLU_NURBS_ERROR27            : constant  := 100277;
   GLU_NURBS_ERROR28            : constant  := 100278;
   GLU_NURBS_ERROR29            : constant  := 100279;
   GLU_NURBS_ERROR30            : constant  := 100280;
   GLU_NURBS_ERROR31            : constant  := 100281;
   GLU_NURBS_ERROR32            : constant  := 100282;
   GLU_NURBS_ERROR33            : constant  := 100283;
   GLU_NURBS_ERROR34            : constant  := 100284;
   GLU_NURBS_ERROR35            : constant  := 100285;
   GLU_NURBS_ERROR36            : constant  := 100286;
   GLU_NURBS_ERROR37            : constant  := 100287;
   GLU_AUTO_LOAD_MATRIX         : constant  := 100200;
   GLU_CULLING                  : constant  := 100201;
   GLU_SAMPLING_TOLERANCE       : constant  := 100203;
   GLU_DISPLAY_MODE             : constant  := 100204;
   GLU_PARAMETRIC_TOLERANCE     : constant  := 100202;
   GLU_SAMPLING_METHOD          : constant  := 100205;
   GLU_U_STEP                   : constant  := 100206;
   GLU_V_STEP                   : constant  := 100207;
   GLU_NURBS_MODE               : constant  := 100160;
   GLU_NURBS_MODE_EXT           : constant  := 100160;
   GLU_NURBS_TESSELLATOR        : constant  := 100161;
   GLU_NURBS_TESSELLATOR_EXT    : constant  := 100161;
   GLU_NURBS_RENDERER           : constant  := 100162;
   GLU_NURBS_RENDERER_EXT       : constant  := 100162;
   GLU_OBJECT_PARAMETRIC_ERROR  : constant  := 100208;
   GLU_OBJECT_PARAMETRIC_ERROR_EXT : constant  := 100208;
   GLU_OBJECT_PATH_LENGTH          : constant  := 100209;
   GLU_OBJECT_PATH_LENGTH_EXT      : constant  := 100209;
   GLU_PATH_LENGTH                 : constant  := 100215;
   GLU_PARAMETRIC_ERROR            : constant  := 100216;
   GLU_DOMAIN_DISTANCE             : constant  := 100217;
   GLU_MAP1_TRIM_2                 : constant  := 100210;
   GLU_MAP1_TRIM_3                 : constant  := 100211;
   GLU_POINT                       : constant  := 100010;
   GLU_LINE                        : constant  := 100011;
   GLU_FILL                        : constant  := 100012;
   GLU_SILHOUETTE                  : constant  := 100013;
   GLU_SMOOTH                      : constant  := 100000;
   GLU_FLAT                        : constant  := 100001;
   GLU_NONE                        : constant  := 100002;
   GLU_OUTSIDE                     : constant  := 100020;
   GLU_INSIDE                      : constant  := 100021;
   GLU_TESS_BEGIN                  : constant  := 100100;
   GLU_BEGIN                       : constant  := 100100;
   GLU_TESS_VERTEX                 : constant  := 100101;
   GLU_VERTEX                      : constant  := 100101;
   GLU_TESS_END                    : constant  := 100102;
   GLU_END                         : constant  := 100102;
   GLU_TESS_ERROR                  : constant  := 100103;
   GLU_TESS_EDGE_FLAG              : constant  := 100104;
   GLU_EDGE_FLAG                   : constant  := 100104;
   GLU_TESS_COMBINE                : constant  := 100105;
   GLU_TESS_BEGIN_DATA             : constant  := 100106;
   GLU_TESS_VERTEX_DATA            : constant  := 100107;
   GLU_TESS_END_DATA               : constant  := 100108;
   GLU_TESS_ERROR_DATA             : constant  := 100109;
   GLU_TESS_EDGE_FLAG_DATA         : constant  := 100110;
   GLU_TESS_COMBINE_DATA           : constant  := 100111;
   GLU_CW                          : constant  := 100120;
   GLU_CCW                         : constant  := 100121;
   GLU_INTERIOR                    : constant  := 100122;
   GLU_EXTERIOR                    : constant  := 100123;
   GLU_UNKNOWN                     : constant  := 100124;
   GLU_TESS_WINDING_RULE           : constant  := 100140;
   GLU_TESS_BOUNDARY_ONLY          : constant  := 100141;
   GLU_TESS_TOLERANCE              : constant  := 100142;
   GLU_TESS_ERROR1                 : constant  := 100151;
   GLU_TESS_ERROR2                 : constant  := 100152;
   GLU_TESS_ERROR3                 : constant  := 100153;
   GLU_TESS_ERROR4                 : constant  := 100154;
   GLU_TESS_ERROR5                 : constant  := 100155;
   GLU_TESS_ERROR6                 : constant  := 100156;
   GLU_TESS_ERROR7                 : constant  := 100157;
   GLU_TESS_ERROR8                 : constant  := 100158;
   GLU_TESS_MISSING_BEGIN_POLYGON  : constant  := 100151;
   GLU_TESS_MISSING_BEGIN_CONTOUR  : constant  := 100152;
   GLU_TESS_MISSING_END_POLYGON    : constant  := 100153;
   GLU_TESS_MISSING_END_CONTOUR    : constant  := 100154;
   GLU_TESS_COORD_TOO_LARGE        : constant  := 100155;
   GLU_TESS_NEED_COMBINE_CALLBACK  : constant  := 100156;
   GLU_TESS_WINDING_ODD            : constant  := 100130;
   GLU_TESS_WINDING_NONZERO        : constant  := 100131;
   GLU_TESS_WINDING_POSITIVE       : constant  := 100132;
   GLU_TESS_WINDING_NEGATIVE       : constant  := 100133;
   GLU_TESS_WINDING_ABS_GEQ_TWO    : constant  := 100134;
   GLU_TESS_MAX_COORD              : constant  := 1.00000000000000052596e+150;


   type STRUCT_GLUNURBS;
   type STRUCT_GLUQUADRIC;
   type STRUCT_GLUTESSELATOR;

   type A_GLUNURBS_T is access all STRUCT_GLUNURBS;
   type A_GLUTESSELATOR_T is access all STRUCT_GLUTESSELATOR;
   type A_GLUQUADRIC_T is access all STRUCT_GLUQUADRIC;

   type STRUCT_GLUNURBS is
       record
           null;
       end record;

   subtype GLUNURBS is STRUCT_GLUNURBS;

   type STRUCT_GLUQUADRIC is
       record
           null;
       end record;

   subtype GLUQUADRIC is STRUCT_GLUQUADRIC;

   type STRUCT_GLUTESSELATOR is
       record
           null;
       end record;

   subtype GLUTESSELATOR is STRUCT_GLUTESSELATOR;

   subtype GLUNURBSOBJ is GLUNURBS;

   subtype GLUQUADRICOBJ is GLUQUADRIC;

   subtype GLUTESSELATOROBJ is GLUTESSELATOR;

   subtype GLUTRIANGULATOROBJ is GLUTESSELATOR;

   type GLUFUNCPTR is access procedure;

   procedure gluBeginCurve (nurb : A_GLUNURBS_T);

   procedure gluBeginPolygon (tess : A_GLUTESSELATOR_T);

   procedure gluBeginSurface (nurb : A_GLUNURBS_T);

   procedure gluBeginTrim (nurb : A_GLUNURBS_T);

   function gluBuild1DMipmapLevels (target         : OpenGL.GLenum;
                                   internalFormat : OpenGL.GLint;
                                   width          : OpenGL.GLsizei;
                                   format         : OpenGL.GLenum;
                                   c_type         : OpenGL.GLenum;
                                   level          : OpenGL.GLint;
                                   base           : OpenGL.GLint;
                                   max            : OpenGL.GLint;
                                   data           : System.Address)
                                                   return OpenGL.GLint;

   function gluBuild1DMipmaps (target         : OpenGL.GLenum;
                              internalFormat : OpenGL.GLint;
                              width          : OpenGL.GLsizei;
                              format         : OpenGL.GLenum;
                              c_type         : OpenGL.GLenum;
                              data           : System.Address)
                                              return OpenGL.GLint;

   function gluBuild2DMipmapLevels (target         : OpenGL.GLenum;
                                   internalFormat : OpenGL.GLint;
                                   width          : OpenGL.GLsizei;
                                   height         : OpenGL.GLsizei;
                                   format         : OpenGL.GLenum;
                                   c_type         : OpenGL.GLenum;
                                   level          : OpenGL.GLint;
                                   base           : OpenGL.GLint;
                                   max            : OpenGL.GLint;
                                   data           : System.Address)
                                                   return OpenGL.GLint;

   function gluBuild2DMipmaps (target         : OpenGL.GLenum;
                              internalFormat : OpenGL.GLint;
                              width          : OpenGL.GLsizei;
                              height         : OpenGL.GLsizei;
                              format         : OpenGL.GLenum;
                              c_type         : OpenGL.GLenum;
                              data           : System.Address)
                                              return OpenGL.GLint;

   function gluBuild3DMipmapLevels (target         : OpenGL.GLenum;
                                   internalFormat : OpenGL.GLint;
                                   width          : OpenGL.GLsizei;
                                   height         : OpenGL.GLsizei;
                                   depth          : OpenGL.GLsizei;
                                   format         : OpenGL.GLenum;
                                   c_type         : OpenGL.GLenum;
                                   level          : OpenGL.GLint;
                                   base           : OpenGL.GLint;
                                   max            : OpenGL.GLint;
                                   data           : System.Address)
                                                   return OpenGL.GLint;

   function gluBuild3DMipmaps (target         : OpenGL.GLenum;
                              internalFormat : OpenGL.GLint;
                              width          : OpenGL.GLsizei;
                              height         : OpenGL.GLsizei;
                              depth          : OpenGL.GLsizei;
                              format         : OpenGL.GLenum;
                              c_type         : OpenGL.GLenum;
                              data           : System.Address)
                                              return OpenGL.GLint;

   function gluCheckExtension (extName   : OpenGL.GLubytePtr;
                              extString : OpenGL.GLubytePtr)
                                         return OpenGL.GLboolean;

   procedure gluCylinder (quad   : A_GLUQUADRIC_T;
                         base   : OpenGL.GLdouble;
                         top    : OpenGL.GLdouble;
                         height : OpenGL.GLdouble;
                         slices : OpenGL.GLint;
                         stacks : OpenGL.GLint);

   procedure gluDeleteNurbsRenderer (nurb : A_GLUNURBS_T);

   procedure gluDeleteQuadric (quad : A_GLUQUADRIC_T);

   procedure gluDeleteTess (tess : A_GLUTESSELATOR_T);

   procedure gluDisk (quad   : A_GLUQUADRIC_T;
                     inner  : OpenGL.GLdouble;
                     outer  : OpenGL.GLdouble;
                     slices : OpenGL.GLint;
                     loops  : OpenGL.GLint);

   procedure gluEndCurve (nurb : A_GLUNURBS_T);

   procedure gluEndPolygon (tess : A_GLUTESSELATOR_T);

   procedure gluEndSurface (nurb : A_GLUNURBS_T);

   procedure gluEndTrim (nurb : A_GLUNURBS_T);

   function gluErrorString (error : OpenGL.GLenum) return OpenGL.GLubytePtr;

   procedure gluGetNurbsProperty (nurb     : A_GLUNURBS_T;
                                 property : OpenGL.GLenum;
                                 data     : OpenGL.GLfloatPtr);

   function gluGetString (name : OpenGL.GLenum) return OpenGL.GLubytePtr;

   procedure gluGetTessProperty (tess  : A_GLUTESSELATOR_T;
                                which : OpenGL.GLenum;
                                data  : OpenGL.GLdoublePtr);

   procedure gluLoadSamplingMatrices (nurb        : A_GLUNURBS_T;
                                     model       : OpenGL.GLfloatPtr;
                                     perspective : OpenGL.GLfloatPtr;
                                     view        : OpenGL.GLintPtr);

   procedure gluLookAt (eyeX    : OpenGL.GLdouble;
                       eyeY    : OpenGL.GLdouble;
                       eyeZ    : OpenGL.GLdouble;
                       centerX : OpenGL.GLdouble;
                       centerY : OpenGL.GLdouble;
                       centerZ : OpenGL.GLdouble;
                       upX     : OpenGL.GLdouble;
                       upY     : OpenGL.GLdouble;
                       upZ     : OpenGL.GLdouble);

   function gluNewNurbsRenderer return A_GLUNURBS_T;

   function gluNewQuadric return A_GLUQUADRIC_T;

   function gluNewTess return A_GLUTESSELATOR_T;

   procedure gluNextContour (tess   : A_GLUTESSELATOR_T;
                            c_type : OpenGL.GLenum);

   procedure gluNurbsCallback (nurb         : A_GLUNURBS_T;
                              which        : OpenGL.GLenum;
                              CallBackFunc : GLUFUNCPTR);

   procedure gluNurbsCallbackData (nurb     : A_GLUNURBS_T;
                                  userData : OpenGL.GLvoidPtr);

   procedure gluNurbsCallbackDataEXT (nurb     : A_GLUNURBS_T;
                                     userData : OpenGL.GLvoidPtr);

   procedure gluNurbsCurve (nurb      : A_GLUNURBS_T;
                           knotCount : OpenGL.GLint;
                           knots     : OpenGL.GLfloatPtr;
                           stride    : OpenGL.GLint;
                           control   : OpenGL.GLfloatPtr;
                           order     : OpenGL.GLint;
                           c_type    : OpenGL.GLenum);

   procedure gluNurbsProperty (nurb     : A_GLUNURBS_T;
                              property : OpenGL.GLenum;
                              value    : OpenGL.GLfloat);

   procedure gluNurbsSurface (nurb       : A_GLUNURBS_T;
                             sKnotCount : OpenGL.GLint;
                             sKnots     : OpenGL.GLfloatPtr;
                             tKnotCount : OpenGL.GLint;
                             tKnots     : OpenGL.GLfloatPtr;
                             sStride    : OpenGL.GLint;
                             tStride    : OpenGL.GLint;
                             control    : OpenGL.GLfloatPtr;
                             sOrder     : OpenGL.GLint;
                             tOrder     : OpenGL.GLint;
                             c_type     : OpenGL.GLenum);

   procedure gluOrtho2D (left   : OpenGL.GLdouble;
                        right  : OpenGL.GLdouble;
                        bottom : OpenGL.GLdouble;
                        top    : OpenGL.GLdouble);

   procedure gluPartialDisk (quad   : A_GLUQUADRIC_T;
                            inner  : OpenGL.GLdouble;
                            outer  : OpenGL.GLdouble;
                            slices : OpenGL.GLint;
                            loops  : OpenGL.GLint;
                            start  : OpenGL.GLdouble;
                            sweep  : OpenGL.GLdouble);

   procedure gluPerspective (fovy   : OpenGL.GLdouble;
                            aspect : OpenGL.GLdouble;
                            zNear  : OpenGL.GLdouble;
                            zFar   : OpenGL.GLdouble);

   procedure gluPickMatrix (x        : OpenGL.GLdouble;
                           y        : OpenGL.GLdouble;
                           delX     : OpenGL.GLdouble;
                           delY     : OpenGL.GLdouble;
                           viewport : OpenGL.GLintPtr);

   function gluProject (objX  : OpenGL.GLdouble;
                       objY  : OpenGL.GLdouble;
                       objZ  : OpenGL.GLdouble;
                       model : OpenGL.GLdoublePtr;
                       proj  : OpenGL.GLdoublePtr;
                       view  : OpenGL.GLintPtr;
                       winX  : OpenGL.GLdoublePtr;
                       winY  : OpenGL.GLdoublePtr;
                       winZ  : OpenGL.GLdoublePtr)
                              return OpenGL.GLint;

   procedure gluPwlCurve (nurb   : A_GLUNURBS_T;
                         count  : OpenGL.GLint;
                         data   : OpenGL.GLfloatPtr;
                         stride : OpenGL.GLint;
                         c_type : OpenGL.GLenum);

   procedure gluQuadricCallback (quad         : A_GLUQUADRIC_T;
                                which        : OpenGL.GLenum;
                                CallBackFunc : GLUFUNCPTR);

   procedure gluQuadricDrawStyle (quad : A_GLUQUADRIC_T;
                                 draw : OpenGL.GLenum);

   procedure gluQuadricNormals (quad   : A_GLUQUADRIC_T;
                               normal : OpenGL.GLenum);

   procedure gluQuadricOrientation (quad        : A_GLUQUADRIC_T;
                                   orientation : OpenGL.GLenum);

   procedure gluQuadricTexture (quad    : A_GLUQUADRIC_T;
                               texture : OpenGL.GLboolean);

   function gluScaleImage (format  : OpenGL.GLenum;
                          wIn     : OpenGL.GLsizei;
                          hIn     : OpenGL.GLsizei;
                          typeIn  : OpenGL.GLenum;
                          dataIn  : OpenGL.GLvoidPtr;
                          wOut    : OpenGL.GLsizei;
                          hOut    : OpenGL.GLsizei;
                          typeOut : OpenGL.GLenum;
                          dataOut : OpenGL.GLvoidPtr)
                                   return OpenGL.GLint;

   procedure gluSphere (quad   : A_GLUQUADRIC_T;
                       radius : OpenGL.GLdouble;
                       slices : OpenGL.GLint;
                       stacks : OpenGL.GLint);

   procedure gluTessBeginContour (tess : A_GLUTESSELATOR_T);

   procedure gluTessBeginPolygon (tess : A_GLUTESSELATOR_T;
                                 data : OpenGL.GLvoidPtr);

   procedure gluTessCallback (tess         : A_GLUTESSELATOR_T;
                             which        : OpenGL.GLenum;
                             CallBackFunc : GLUFUNCPTR);

   procedure gluTessEndContour (tess : A_GLUTESSELATOR_T);

   procedure gluTessEndPolygon (tess : A_GLUTESSELATOR_T);

   procedure gluTessNormal (tess   : A_GLUTESSELATOR_T;
                           valueX : OpenGL.GLdouble;
                           valueY : OpenGL.GLdouble;
                           valueZ : OpenGL.GLdouble);

   procedure gluTessProperty (tess  : A_GLUTESSELATOR_T;
                             which : OpenGL.GLenum;
                             data  : OpenGL.GLdouble);

   procedure gluTessVertex (tess     : A_GLUTESSELATOR_T;
                           location : OpenGL.GLdoublePtr;
                           data     : OpenGL.GLvoidPtr);

   function gluUnProject (winX  : OpenGL.GLdouble;
                         winY  : OpenGL.GLdouble;
                         winZ  : OpenGL.GLdouble;
                         model : OpenGL.GLdoublePtr;
                         proj  : OpenGL.GLdoublePtr;
                         view  : OpenGL.GLintPtr;
                         objX  : OpenGL.GLdoublePtr;
                         objY  : OpenGL.GLdoublePtr;
                         objZ  : OpenGL.GLdoublePtr)
                                return OpenGL.GLint;

   function gluUnProject4 (winX    : OpenGL.GLdouble;
                          winY    : OpenGL.GLdouble;
                          winZ    : OpenGL.GLdouble;
                          clipW   : OpenGL.GLdouble;
                          model   : OpenGL.GLdoublePtr;
                          proj    : OpenGL.GLdoublePtr;
                          view    : OpenGL.GLintPtr;
                          nearVal : OpenGL.GLdouble;
                          farVal  : OpenGL.GLdouble;
                          objX    : OpenGL.GLdoublePtr;
                          objY    : OpenGL.GLdoublePtr;
                          objZ    : OpenGL.GLdoublePtr;
                          objW    : OpenGL.GLdoublePtr)
                                   return OpenGL.GLint;

private

   pragma Import (C, gluBeginCurve, "gluBeginCurve");

   pragma Import (C, gluBeginPolygon, "gluBeginPolygon");

   pragma Import (C, gluBeginSurface, "gluBeginSurface");

   pragma Import (C, gluBeginTrim, "gluBeginTrim");

   pragma Import (C, gluBuild1DMipmapLevels, "gluBuild1DMipmapLevels");

   pragma Import (C, gluBuild1DMipmaps, "gluBuild1DMipmaps");

   pragma Import (C, gluBuild2DMipmapLevels, "gluBuild2DMipmapLevels");

   pragma Import (C, gluBuild2DMipmaps, "gluBuild2DMipmaps");

   pragma Import (C, gluBuild3DMipmapLevels, "gluBuild3DMipmapLevels");

   pragma Import (C, gluBuild3DMipmaps, "gluBuild3DMipmaps");

   pragma Import (C, gluCheckExtension, "gluCheckExtension");

   pragma Import (C, gluCylinder, "gluCylinder");

   pragma Import (C, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");

   pragma Import (C, gluDeleteQuadric, "gluDeleteQuadric");

   pragma Import (C, gluDeleteTess, "gluDeleteTess");

   pragma Import (C, gluDisk, "gluDisk");

   pragma Import (C, gluEndCurve, "gluEndCurve");

   pragma Import (C, gluEndPolygon, "gluEndPolygon");

   pragma Import (C, gluEndSurface, "gluEndSurface");

   pragma Import (C, gluEndTrim, "gluEndTrim");

   pragma Import (C, gluErrorString, "gluErrorString");

   pragma Import (C, gluGetNurbsProperty, "gluGetNurbsProperty");

   pragma Import (C, gluGetString, "gluGetString");

   pragma Import (C, gluGetTessProperty, "gluGetTessProperty");

   pragma Import (C, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");

   pragma Import (C, gluLookAt, "gluLookAt");

   pragma Import (C, gluNewNurbsRenderer, "gluNewNurbsRenderer");

   pragma Import (C, gluNewQuadric, "gluNewQuadric");

   pragma Import (C, gluNewTess, "gluNewTess");

   pragma Import (C, gluNextContour, "gluNextContour");

   pragma Import (C, gluNurbsCallback, "gluNurbsCallback");

   pragma Import (C, gluNurbsCallbackData, "gluNurbsCallbackData");

   pragma Import (C, gluNurbsCallbackDataEXT, "gluNurbsCallbackDataEXT");

   pragma Import (C, gluNurbsCurve, "gluNurbsCurve");

   pragma Import (C, gluNurbsProperty, "gluNurbsProperty");

   pragma Import (C, gluNurbsSurface, "gluNurbsSurface");

   pragma Import (C, gluOrtho2D, "gluOrtho2D");

   pragma Import (C, gluPartialDisk, "gluPartialDisk");

   pragma Import (C, gluPerspective, "gluPerspective");

   pragma Import (C, gluPickMatrix, "gluPickMatrix");

   pragma Import (C, gluProject, "gluProject");

   pragma Import (C, gluPwlCurve, "gluPwlCurve");

   pragma Import (C, gluQuadricCallback, "gluQuadricCallback");

   pragma Import (C, gluQuadricDrawStyle, "gluQuadricDrawStyle");

   pragma Import (C, gluQuadricNormals, "gluQuadricNormals");

   pragma Import (C, gluQuadricOrientation, "gluQuadricOrientation");

   pragma Import (C, gluQuadricTexture, "gluQuadricTexture");

   pragma Import (C, gluScaleImage, "gluScaleImage");

   pragma Import (C, gluSphere, "gluSphere");

   pragma Import (C, gluTessBeginContour, "gluTessBeginContour");

   pragma Import (C, gluTessBeginPolygon, "gluTessBeginPolygon");

   pragma Import (C, gluTessCallback, "gluTessCallback");

   pragma Import (C, gluTessEndContour, "gluTessEndContour");

   pragma Import (C, gluTessEndPolygon, "gluTessEndPolygon");

   pragma Import (C, gluTessNormal, "gluTessNormal");

   pragma Import (C, gluTessProperty, "gluTessProperty");

   pragma Import (C, gluTessVertex, "gluTessVertex");

   pragma Import (C, gluUnProject, "gluUnProject");

   pragma Import (C, gluUnProject4, "gluUnProject4");

end OpenGL.GLU;
