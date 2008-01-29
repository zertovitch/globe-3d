-- *---------------------------------------------------------------------------*
-- *  vrml.l                                                                 
-- *                                                                           
-- * Project: SICS DIVE
-- * Copyright: SICS
-- * Implemented by: Emmanuel Frecon and Olof Hagsand
-- *
-- * This file contains lex declarations for a VRML Parser.
-- *
-- *---------------------------------------------------------------------------*

--  /*==========================================================================
--    Local variables
--    ==========================================================================*/

--  static char rcsid[] := "$Id: file.l,v 1.6 1995/05/26 12:13:16 olof Exp $";
--  static void print_rcsid() {if (1) fprintf(stdout,"%s", rcsid); else print_rcsid();}

--  static int linenum := 1;
--  static char current_file[128];

--  /* #undef   ECHO*/

--  #include <string.h>
--  #include "vrml.tab.h"
--  #include "vrml.h"

--  #undef yywrap
--  int yywrap()
--  {
--    return 1;
--  } 
--  %}

--  /*****************************************************************************/
--  /* lexical definitions */
--  /*****************************************************************************/

--  %e 1800
--  %p 14000
--  %a 22000
--  %n 2000
--  %s use_and_def

--  /* The definition of an instance is not exactly correct, it should also
--     exclude control characters, this could be done by [:cntrl:] with flex
--     and [\0-\37] with lex. */

INSTANCE    [^0-9{}\'\"\\ \t\n\f[:cntrl:]][^{}\'\"\\ \t\n\f[:cntrl:]]*
-- INSTANCE    [^0-9{}\'\"\\ \t\n\f]*
-- INSTANCE [{}\'\"\\]*
-- ID   [A-Za-z_][0-9A-Za-z_]*
-- VRMLWORD    [A-Za-z_][0-9A-Za-z_]*
LETTER [a-zA-Z_]
DIGIT [0-9]
IDCHAR [0-9a-zA-Z_]
NOTGUIL [^\"]
GUIL [\"]

%%

DEF                { -- BEGIN(use_and_def); 
                     return DEF; }
--<use_and_def>{INSTANCE}        { yylval.string := strdup(yytext);
--                  BEGIN(INITIAL);
--                  return VRMLWORD; }
--<use_and_def>[ \t]*        ;
--<use_and_def>\n            linenum ++;
USE                { -- BEGIN(use_and_def);
                      return USE_t; }

AsciiText            { return ASCIITEXT; }
Cone                { return CONE; }
Coordinate3            { return COORDINATE3; }
Cube                { return CUBE; }
Cylinder            { return CYLINDER; }
DirectionalLight        { return DIRECTIONALLIGHT; }
FontStyle            { return FONTSTYLE; }
Group                { return GROUP; }
IndexedFaceSet            { return INDEXEDFACESET; }
IndexedLineSet            { return INDEXEDLINESET; }
Info                { return INFO; }
LOD                { return LOD; }
LevelOfDetail            { return LEVELOFDETAIL; }
Material            { return MATERIAL; }
MaterialBinding            { return MATERIALBINDING; }
MatrixTransform            { return MATRIXTRANSFORM; }
Normal                { return NORMAL; }
NormalBinding            { return NORMALBINDING; }
OrthographicCamera        { return ORTHOGRAPHICCAMERA; }
PerspectiveCamera        { return PERSPECTIVECAMERA; }
PointLight            { return POINTLIGHT; }
PointSet            { return POINTSET; }
Rotation            { return ROTATION; }
Scale                { return SCALE; }
Separator            { return SEPARATOR; }
ShapeHints            { return SHAPEHINTS_t; }
Sphere                { return SPHERE; }
SpotLight            { return SPOTLIGHT; }
Switch                { return SWITCH; }
Texture2            { return TEXTURE2; }
Texture2Transform        { return TEXTURE2TRANSFORM; }
TextureCoordinate2        { return TEXTURECOORDINATE2; }
Transform            { return TRANSFORM; }
TransformSeparator        { return TRANSFORMSEPARATOR; }
Translation            { return TRANSLATION; }
WWWAnchor            { return WWWANCHOR; }
WWWInline            { return WWWINLINE; }

parts                { return PARTS; }
bottomRadius            { return BOTTOMRADIUS; }
height                { return HEIGHT; }
point                { return POINT; }
width                { return WIDTH; }
depth                { return DEPTH; }
string                { return STRING_t; }
spacing                { return SPACING; }
justification            { return JUSTIFICATION; }
radius                { return RADIUS; }
on                { return ON; }
intensity            { return INTENSITY; }
color                { return COLOR; }
direction            { return DIRECTION; }
size                { return SIZE; }
family                { return FAMILY; }
style                { return STYLE; }
coordIndex            { return COORDINDEX; }
materialIndex            { return MATERIALINDEX; }
normalIndex            { return NORMALINDEX; }
textureCoordIndex        { return TEXTURECOORDINDEX; }
range                { return RANGE_t; }
center                { return CENTER; }
screenArea            { return SCREENAREA; }
ambientColor            { return AMBIENTCOLOR; }
diffuseColor            { return DIFFUSECOLOR; }
specularColor            { return SPECULARCOLOR; }
emissiveColor            { return EMISSIVECOLOR; }
shininess            { return SHININESS; }
transparency            { return TRANSPARENCY; }
value                { return VALUE; }
matrix                { return MATRIX; }
vector                { return VECTOR; }
position            { return POSITION; }
orientation            { return ORIENTATION; }
focalDistance            { return FOCALDISTANCE; }
heightAngle            { return HEIGHTANGLE; }
location            { return LOCATION; }
startIndex            { return STARTINDEX; }
numPoints            { return NUMPOINTS; }
rotation            { return ROTATION_F; }
scaleFactor            { return SCALEFACTOR; }
renderCulling            { return RENDERCULLING; }
vertexOrdering            { return VERTEXORDERING; }
shapeType            { return SHAPETYPE; }
faceType            { return FACETYPE; }
creaseAngle            { return CREASEANGLE; }
dropOffRate            { return DROPOFFRATE; }
cutOffAngle            { return CUTOFFANGLE; }
whichChild            { return WHICHCHILD; }
filename            { return FILENAME; }
image                { return IMAGE; }
wrapS                { return WRAPS; }
wrapT                { return WRAPT; }
scaleOrientation        { return SCALEORIENTATION; }
translation            { return TRANSLATION_F; }
name                { return NAME; }
description            { return DESCRIPTION; }
map                { return MAP; }
bboxSize            { return BBOXSIZE; }
bboxCenter            { return BBOXCENTER; }

LEFT                { yylval.intval := VRML_justification'Pos(VRML_LEFT); 
                  return NUMBER; }
CENTER              { yylval.intval := VRML_justification'Pos(VRML_CENTER);
                  return NUMBER; }
RIGHT               { yylval.intval := VRML_justification'Pos(VRML_RIGHT);
                  return NUMBER; }

SIDES               { yylval.intval := Integer(VRML_SIDES);
                  return NUMBER; }
BOTTOM              { yylval.intval := Integer(VRML_BOTTOM);
                  return NUMBER; }
TOP                 { yylval.intval := Integer(VRML_TOP);
                  return NUMBER; }
ALL                 { yylval.intval := Integer(VRML_ALL);
                  return NUMBER; }

TRUE                { yylval.intval := 0; return NUMBER; }
FALSE               { yylval.intval := 1; return NUMBER; }

SERIF               { yylval.intval := VRML_font_family'Pos(VRML_SERIF); 
                  return NUMBER; }
SANS                { yylval.intval := VRML_font_family'Pos(VRML_SANS);
                  return NUMBER; }
TYPEWRITER          { yylval.intval := VRML_font_family'Pos(VRML_TYPEWRITER);
                  return NUMBER; }

NONE                { yylval.intval := Integer(VRML_NONE);
                  return NUMBER; }
BOLD                { yylval.intval := Integer(VRML_BOLD);
                  return NUMBER; }
ITALIC                { yylval.intval := Integer(VRML_ITALIC);
                  return NUMBER; }

DEFAULT                { yylval.intval := VRML_binding'Pos(VRML_DEFAULT);
                  return NUMBER; }
OVERALL                { yylval.intval := VRML_binding'Pos(VRML_OVERALL);
                  return NUMBER; }
PER_PART            { yylval.intval := VRML_binding'Pos(VRML_PER_PART);
                  return NUMBER; }
PER_PART_INDEXED        { yylval.intval := VRML_binding'Pos(VRML_PER_PART_INDEXED);
                  return NUMBER; }
PER_FACE            { yylval.intval := VRML_binding'Pos(VRML_PER_FACE);
                  return NUMBER; }
PER_FACE_INDEXED        { yylval.intval := VRML_binding'Pos(VRML_PER_FACE_INDEXED);
                  return NUMBER; }
PER_VERTEX            { yylval.intval := VRML_binding'Pos(VRML_PER_VERTEX);
                  return NUMBER; }
PER_VERTEX_INDEXED        { yylval.intval := VRML_binding'Pos(VRML_PER_VERTEX_INDEXED);
                  return NUMBER; }

ON                { yylval.intval := VRML_culling'Pos(VRML_ON);
                  return NUMBER; }
OFF                { yylval.intval := VRML_culling'Pos(VRML_OFF);
                  return NUMBER; }
AUTO                { yylval.intval := VRML_culling'Pos(VRML_AUTO);
                  return NUMBER; }

UNKNOWN_ORDERING        { yylval.intval := VRML_vertex_ordering'Pos(VRML_UNKNOWN_ORDERING);
                  return NUMBER; }
CLOCKWISE            { yylval.intval := VRML_vertex_ordering'Pos(VRML_CLOCKWISE);
                  return NUMBER; }
COUNTERCLOCKWISE        { yylval.intval := VRML_vertex_ordering'Pos(VRML_COUNTERCLOCKWISE);
                  return NUMBER; }

UNKNOWN_SHAPE_TYPE        { yylval.intval := VRML_shape_type'Pos(VRML_UNKNOWN_SHAPE_TYPE);
                  return NUMBER; }
SOLID                { yylval.intval := VRML_shape_type'Pos(VRML_SOLID);
                  return NUMBER; }

UNKNOWN_FACE_TYPE        { yylval.intval := VRML_face_type'Pos(VRML_UNKNOWN_FACE_TYPE);
                  return NUMBER; }
CONVEX                { yylval.intval := VRML_face_type'Pos(VRML_CONVEX);
                  return NUMBER; }

REPEAT                { yylval.intval := VRML_wrap'Pos(VRML_REPEAT);
                  return NUMBER; }
CLAMP                { yylval.intval := VRML_wrap'Pos(VRML_CLAMP);
                  return NUMBER; }

fields                {
                declare
                  c: Character;
                begin
                  loop
                    loop
                      Get(c);
                      exit when c=']' or End_of_File or c=ASCII.CR or c=ASCII.LF;
                    end loop;
                    if c=ASCII.CR or c=ASCII.LF then
                      linenum:= linenum + 1;
                    elsif c=']' then
                      return FIELDS;
                    else
                      YY_Terminate;
                      -- break;
                    end if;
                  end loop;
                  return FIELDS;
                end;
                }

0                { yylval.intval   := 0;
                   yylval.floatval := 0.0;
                   return NUMBER;
                 }
-?[1-9][0-9]*    { yylval.intval := Integer'Value(yytext);
                   yylval.floatval:= Real( yylval.intval );
                   return NUMBER;}
0[1-7][0-7]*     { yylval.intval := Integer'Value("8#"&yytext&'#');
                   yylval.floatval:= Real( yylval.intval );
                   return NUMBER;}

-?[0-9]*\.?[0-9]*([eE][+-]?[0-9]+)?    { yylval.floatval := Real'Value(yytext);
                      return FLOAT_t;}
0x[0-9A-Fa-f]+   { yylval.intval := Integer'Value("16#"&yytext&'#');
                   yylval.floatval:= Real( yylval.intval );
                   return NUMBER;}

[\#].*\n            { -- Skip rest of line.
                      VRML_Comment(YYText);
                      linenum:= linenum + 1;
                    }

\"({NOTGUIL})*\" {return VRMLSTRING;}

--\"                {

--                declare
--                  c,oldc: Integer;
--                begin
--                  char *buffer;
--                  int buflen=8;
--                  int len=0;

--                  c := EOF;
--                  buffer := (char *)malloc(buflen);
--                  if (!buffer)
--                    yyterminate();
--                  buffer[0] := '\0';
--                  for (;;) {
--                    oldc := c;
--                    c := input();
--                    if (c=='\"') {
--                      if (oldc=='\\') {
--                        buffer[len-1] := '\"';
--                      } else {
--                        yylval.string := buffer;
--                        return VRMLSTRING;
--                      }
--                    } else {
--                      if (len+2 >= buflen) {
--                        buflen *= 2;
--                        buffer := (char *)realloc(buffer, buflen);
--                        if (!buffer)
--                      yyterminate();
--                      }
--                      buffer[len] := c;
--                      buffer[++len] := '\0';
--                    }
--                  }
--                  return VRMLSTRING;
--                end;
--                }

[ \t]+               { null; -- \t } ;
\n                {linenum:= linenum+1;}

--{ID}                {
--                declare
--                  c: Integer;
--                begin
--                  register int c;
--                  int stack := 0;

--                  for ( ; ; ) {
--                    do {
--                      c := input();
--                    } while (c!='{' && c!='}' && c!=EOF &&
--                         c!='\n');
--                    if (c=='{') {
--                      stack ++;
--                    } else if (c=='}') {
--                      stack --;
--                    } else if (c=='\n') {
--                      linenum ++;
--                    } else {
--                      yyterminate();
--                      break;
--                    }
--                    if (!stack)
--                      break;
--                  }
--                  return UNKNOWNNODE;
--                end;
--                }

{LETTER}{IDCHAR}* {return VRMLWORD;}

[|]                { null; -- ??
                     -- C: return *yytext;
                   }
","                { return ','; }
"}"                { return '}'; }
"{"                { return '{'; }
"["                { return '['; }
"]"                { return ']'; }

.                { Put_Line(Standard_Error, 
                   -- current_file'img & ' ' & 
                   Integer'Image(linenum) & " Warning:");
                   Put_Line(Standard_Error, "Skipping illegal input" & yytext);}

%%

with VRML_Tokens; use VRML_Tokens;
with VRML_Help;   use VRML_Help;

--/*
--\"[^\"]*\"            {
--                       yytext[strlen(yytext) - 1] := '\0';
--                      yylval.string := strdup(yytext + 1);
--                  return VRMLSTRING;
--                }
--fields                { BEGIN(field); return FIELDS; }
--<field>"]"            { BEGIN(INITIAL); return ']'; }
--<field>[^\]\n]*            ;
--<field>\n            linenum ++;
--{ID}                { BEGIN(newnode); return UNKNOWNNODE; }
--<newnode>"}"            { BEGIN(INITIAL); return '}'; }
--<newnode>[^\}\n]*        ;
--<newnode>\n            linenum ++;
--*/

--int vrml_filel_linenum(char **file)
--{
--  *file := current_file;
--  return linenum;
--}

--void vrml_filel_init(FILE *f, char *file_name)
--{
--#ifdef FLEX_SCANNER
--  static int firstcall := 1;
--#endif

--  linenum := 1;
--  strcpy(current_file, file_name);

--#ifdef FLEX_SCANNER
--  if (!firstcall)
--    yyrestart(yyin=f);
--  else {
--    firstcall := 0;
--    yyin := f;
--  }
--#else /* FLEX_SCANNER */
--  yyin := f;
--#endif /* FLEX_SCANNER */
--}

##
