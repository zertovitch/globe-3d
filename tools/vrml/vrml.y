-- *------------------------------------------------------------------------
-- *  vrml.y
-- *           
-- * Project: SICS DIVE
-- * Copyright: SICS
-- * Implemented by: Emmanuel Frécon and Olof Hagsand
-- *
-- *  This file contains yacc declarations a VRML Parser.
-- *
-- *------------------------------------------------------------------------

--  %token <intval> NUMBER 
--  %token <floatval> FLOAT
--  %token <string> VRMLSTRING VRMLWORD

%token NUMBER 
%token FLOAT_t
%token VRMLSTRING VRMLWORD

%token ASCIITEXT CONE COORDINATE3 CUBE CYLINDER DIRECTIONALLIGHT FONTSTYLE
%token GROUP INDEXEDFACESET INDEXEDLINESET INFO LOD MATERIAL MATERIALBINDING
%token LEVELOFDETAIL MATRIXTRANSFORM
%token NORMAL NORMALBINDING ORTHOGRAPHICCAMERA PERSPECTIVECAMERA
%token POINTLIGHT POINTSET ROTATION SCALE SEPARATOR SHAPEHINTS_t SPHERE
%token SPOTLIGHT SWITCH TEXTURE2 TEXTURE2TRANSFORM TEXTURECOORDINATE2
%token TRANSFORM TRANSFORMSEPARATOR TRANSLATION WWWANCHOR WWWINLINE

%token PARTS BOTTOMRADIUS HEIGHT POINT WIDTH DEPTH STRING_t SPACING
%token JUSTIFICATION RADIUS ON INTENSITY COLOR DIRECTION SIZE FAMILY STYLE
%token COORDINDEX MATERIALINDEX NORMALINDEX TEXTURECOORDINDEX RANGE_t
%token SCREENAREA TRANSLATION_F
%token CENTER AMBIENTCOLOR DIFFUSECOLOR SPECULARCOLOR EMISSIVECOLOR SHININESS
%token TRANSPARENCY VALUE MATRIX VECTOR POSITION ORIENTATION FOCALDISTANCE
%token HEIGHTANGLE LOCATION STARTINDEX NUMPOINTS ROTATION_F SCALEFACTOR
%token RENDERCULLING VERTEXORDERING SHAPETYPE FACETYPE CREASEANGLE DROPOFFRATE
%token CUTOFFANGLE WHICHCHILD FILENAME IMAGE WRAPS WRAPT SCALEORIENTATION
%token NAME DESCRIPTION MAP BBOXSIZE BBOXCENTER

%token DEF USE_t

%token FIELDS UNKNOWNNODE

--  %type <floatval> SFFloat
--  %type <intval> SFBitMask bitmasklist SFBool SFEnum SFLong
--  %type <string> SFString  

%start vrml

{

  type const_type is (
    intval, 
    floatval, 
    doubleval, 
    stringval, 
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Float;
  end record;

} 

%%
--  /*****************************************************************************/
--  /* syntactical definitions
--  /*****************************************************************************/

vrml		: Nodes {VRML_Help.YY_ACCEPT;}
		| error {VRML_Help.YY_ABORT;}
		;

Nodes		: Node Nodes
		|
		;

Node		: NodeSpec NodeBody 
		| USE_t VRMLWORD
		;

NodeSpec	: DEF VRMLWORD
		|
		;

NodeBody	: AnAsciiText
		| ACone
		| ACoordinate3
		| ACube
		| ACylinder
		| Dlight
		| AFontStyle
		| AGroup
		| IdxFaceSet
		| IdxLineSet
		| AnInfo
		| ALod
		| ALevelOfDetail
		| AMaterial
		| AMaterialBinding
		| AMatrixTransform
		| ANormal
		| ANormalBinding
		| AnOrthographicCamera
		| APerspectiveCamera
		| APointLight
		| APointSet
		| ARotation
		| AScale
		| ASeparator
		| AShapeHints
		| ASphere
		| ASpotLight
		| ASwitch
		| ATexture2
		| Tex2Transform
		| ATexCoordinate2
		| ATransform
		| TransSeparator
		| ATranslation
		| AWWWAnchor
		| AWWWInline
		| AnUnknownNode
		;

--  /* Field specification */
FieldSpecs	: FIELDS
		|
		;

--  /* Unknown node */
AnUnknownNode	: UNKNOWNNODE
		;

--  /* Ascii Text */
asciitextfields	: asciitextfield asciitextfields
		|
		;

asciitextfield	: STRING_t	MFString
		| SPACING	SFFloat
		| JUSTIFICATION	SFEnum
		| WIDTH		MFFloat
		;
		
AnAsciiText	: ASCIITEXT '{' FieldSpecs asciitextfields '}'
		;

--  /* Cones */
conefields	: conefield conefields
		|
		;

conefield	: PARTS		SFBitMask
		| BOTTOMRADIUS	SFFloat
		| HEIGHT	SFFloat
		;

ACone		: CONE '{' FieldSpecs conefields '}'
		;


--  Coordinate 3
coordinatefield :  POINT MFVec3f_coo
		|
		;

ACoordinate3    :
     COORDINATE3
     '{'
     FieldSpecs
     { Ada_Put_Line(
         "coord_" & Trim (sepa_count'Image, Left) &
         " : constant Point_3D_Array :="
       );
       indent:= indent + 1;
     }
     coordinatefield
     { indent:= indent - 1;
       Ada_Put_Line(";"); }
     '}'
		;

--  /* Cubes */
cubefields	: cubefield cubefields
		|
		;

cubefield	: WIDTH	SFFloat
		| HEIGHT	SFFloat
		| DEPTH	SFFloat
		;

ACube		: CUBE '{' FieldSpecs cubefields '}'
		;


--  /* Cylinder */
cylinderfields	: cylinderfield cylinderfields
		|
		;

cylinderfield	: PARTS		SFBitMask
		| RADIUS	SFFloat
		| HEIGHT	SFFloat
		;

ACylinder	: CYLINDER '{' FieldSpecs cylinderfields '}'
		;

--  /* Directional Light */
dlightfields	: dlightfield dlightfields
		|
		;

dlightfield	: ON		SFBool
		| INTENSITY	SFFloat
		| COLOR		SFColor
		| DIRECTION	SFVec3f
		;

Dlight		: DIRECTIONALLIGHT '{' FieldSpecs dlightfields '}'
		;

--  /* Font Style */
fstylefields	: fstylefield fstylefields
		|
		;

fstylefield	: SIZE		SFFloat
		| FAMILY	SFEnum
		| STYLE		SFBitMask
		;

AFontStyle	: FONTSTYLE '{' FieldSpecs fstylefields '}'
		;

--  /* Groups */
AGroup		: GROUP '{' Nodes '}'
		;

--  /* Indexed Face Set */
ifacesetfields	: ifacesetfield ifacesetfields
		|
		;

ifacesetfield	:
              COORDINDEX
              {  indent := indent - 1;
                 Ada_New_Line;
                 indent := indent + 1;
                 Ada_Put_Line
                   ("idx_" & Trim (sepa_count'Image, Left) &
                    " : constant Idx_4_Array_Array := ");
                 indent := indent + 1;
                 Reset_Index_Grouping;
              }
              MFLong_1
              {  indent := indent - 1;
                 idx_last_sepa := idx_last_sepa + sepa_points (sepa_count);
                 Ada_Put_Line (";");
              }
		| MATERIALINDEX		MFLong
		| NORMALINDEX		MFLong
		| TEXTURECOORDINDEX	MFLong
		;

IdxFaceSet	: INDEXEDFACESET
              '{'
              FieldSpecs
              ifacesetfields
              '}'
		;

--  /* Indexed Line Set */
ilinesetfields	: ilinesetfield ilinesetfields
		|
		;

ilinesetfield	: COORDINDEX		MFLong
		| MATERIALINDEX		MFLong
		| NORMALINDEX		MFLong
		| TEXTURECOORDINDEX	MFLong
		;

IdxLineSet	: INDEXEDLINESET '{' FieldSpecs ilinesetfields '}'
		;

--  /* Info */
infofield	: STRING_t SFString {VRML_Info(YYText);}
		|
		;

AnInfo	: INFO '{' FieldSpecs infofield '}'
		;

--  /* Level of Detail 
--     LOD comes from the new VRML draft, LEVELOFDETAIL from the old one !! */
lodfields	: lodfield lodfields
		|
		;

lodfield	: RANGE_t		MFFloat
		| CENTER	SFVec3f
		;

ALod		: LOD '{' FieldSpecs lodfields Nodes '}'
		;

oldlodfield	: SCREENAREA	MFFloat
		|
		;

ALevelOfDetail	: LEVELOFDETAIL '{' FieldSpecs oldlodfield Nodes '}'
		;

--  /* Material */
materialfields	: materialfield materialfields
		|
		;

materialfield
      	: AMBIENTCOLOR		MFColor
              {current_matos.ambient  := last_color; }
		| DIFFUSECOLOR		MFColor
              {current_matos.diffuse  := last_color; }
		| SPECULARCOLOR		MFColor
              {current_matos.specular := last_color; }
		| EMISSIVECOLOR		MFColor
              {current_matos.emission := last_color; }
		| SHININESS		      MFFloat
              {current_matos.shininess:= yylval.floatval * 128.0; }
		| TRANSPARENCY		MFFloat
              {current_matos.ambient(3) := 1.0 - yylval.floatval;
               current_matos.diffuse(3) := current_matos.ambient(3);
               current_matos.specular(3):= current_matos.ambient(3);
               current_matos.emission(3):= current_matos.ambient(3);
              }
		;

AMaterial	: MATERIAL '{' FieldSpecs
              {
               Sepa_matos_defined (sepa_count) := True;
               current_matos := default_material;
               indent := indent - 1;
               Ada_New_Line;
               indent := indent + 2;
               Ada_Put_Line(
                 "matos_" & Trim (sepa_count'Image, Left) &
                 " : constant Material_Type :=" );
              }
              materialfields
             { Ada_Put_Line ("(ambient   => " & RGBA (current_matos.ambient)  & ',');
               Ada_Put_Line (" specular  => " & RGBA (current_matos.specular) & ',');
               Ada_Put_Line (" diffuse   => " & RGBA (current_matos.diffuse)  & ',');
               Ada_Put_Line (" emission  => " & RGBA (current_matos.emission) & ',');
               Ada_Put_Line (" shininess => " & Image (current_matos.shininess, force => True) & ");");
               indent := indent - 1;
               Ada_New_Line;
             }
              '}'
		;

--  /* Material Binding */
mbindingfields	: mbindingfield mbindingfields
		|
		;

mbindingfield	: VALUE			SFEnum
		;

AMaterialBinding	: MATERIALBINDING '{' FieldSpecs mbindingfields '}'
		;

--  /* Matrix Transform */
mxtransformfields	: mxtransformfield mxtransformfields
			|
			;

mxtransformfield	: MATRIX	SFMatrix
			;

AMatrixTransform	: MATRIXTRANSFORM '{' FieldSpecs mxtransformfields '}'
		;

--  /* Normal */
normalfields	: normalfield normalfields
		|
		;

normalfield	: VECTOR	MFVec3f
		;

ANormal		: NORMAL '{' FieldSpecs
             { indent:= indent - 1;
               Ada_New_Line;
               indent:= indent + 1;
               Ada_Put_Line(
                "normal_" & Trim(Natural'Image(sepa_count), Left) &
                 ": constant Vector_3D_array:= "
               );
               indent:= indent + 1;
             }
             normalfields
             { indent:= indent - 1;
               Ada_Put_Line(";"); }
             '}'
		;

--  /* Normal Binding */
nbindingfields	: nbindingfield nbindingfields
		|
		;

nbindingfield	: VALUE			SFEnum
		;

ANormalBinding	: NORMALBINDING '{' FieldSpecs nbindingfields '}'
		;

--  /* Orthographic camera */
ocamerafields	: ocamerafield ocamerafields
		|
		;

ocamerafield	: POSITION	SFVec3f
		| ORIENTATION	SFRotation
		| FOCALDISTANCE	SFFloat
		| HEIGHT	SFFloat
		;

AnOrthographicCamera	: ORTHOGRAPHICCAMERA '{' FieldSpecs ocamerafields '}'
			;

--  /* Perspective camera */
pcamerafields	: pcamerafield pcamerafields
		|
		;

pcamerafield	: POSITION	SFVec3f
		| ORIENTATION	SFRotation
		| FOCALDISTANCE	SFFloat
		| HEIGHTANGLE	SFFloat
		;

APerspectiveCamera	: PERSPECTIVECAMERA '{' FieldSpecs pcamerafields '}'
			;

--  /* Point Light */
plightfields	: plightfield plightfields
		|
		;

plightfield	: ON		SFBool
		| INTENSITY	SFFloat
		| COLOR		SFColor
		| LOCATION	SFVec3f
		;	

APointLight	: POINTLIGHT '{' FieldSpecs plightfields '}'
		;

--  /* Point Set */
psetfields	: psetfield psetfields
		|
		;

psetfield	: STARTINDEX	SFLong
		| NUMPOINTS	SFLong
		;

APointSet	: POINTSET '{' FieldSpecs psetfields '}'
		;

--  /* Rotation */
rotationfields	: rotationfield rotationfields
		|
		;

rotationfield	: ROTATION_F	SFRotation
		;

ARotation	: ROTATION '{' FieldSpecs rotationfields '}'
		;

--  /* Scale */
scalefields	: scalefield scalefields
		|
		;

scalefield	: SCALEFACTOR	SFVec3f
		;

AScale		: SCALE '{' FieldSpecs scalefields '}'
		;

--  /* Separator */
separatorfields	: separatorfield separatorfields
		|
		;

separatorfield	: RENDERCULLING	SFEnum
		;

ASeparator	: SEPARATOR 
              '{'
               { sepa_count:= sepa_count + 1;
                 Ada_comment("begin Separator #" & Natural'Image(sepa_count));
                 indent:= indent + 1;
               }
               FieldSpecs
               separatorfields
               Nodes
               { Ada_comment("last index now:" & Natural'Image(idx_last_sepa));
                 indent:= indent - 1;
                 Ada_comment("end Separator #" & Natural'Image(sepa_count));
               }
               '}'
		;

--  /* Shape Hints */
shapehintsfields	: shapehintsfield shapehintsfields
			|
			;

shapehintsfield
  	      : VERTEXORDERING	SFEnum
              { current_shape_hints.ordering:=
                VRML_vertex_ordering'Val(yylval.intval);
              }
		| SHAPETYPE		SFEnum
		| FACETYPE		SFEnum
		| CREASEANGLE		SFFloat
		;

AShapeHints	: SHAPEHINTS_t
              { current_shape_hints:= default_shape_hints; }
              '{'
              FieldSpecs
              shapehintsfields
              '}'
		;

--  /* Sphere */
spherefields	: spherefield spherefields
		|
		;

spherefield	: RADIUS	SFFloat
		;

ASphere		: SPHERE '{' FieldSpecs spherefields '}'
		;

--  /* Spotlight */
spotlightfields	: spotlightfield spotlightfields
		|
		;

spotlightfield	: ON		SFBool
		| INTENSITY	SFFloat
		| COLOR		SFColor
		| LOCATION	SFVec3f
		| DIRECTION	SFVec3f
		| DROPOFFRATE	SFFloat
		| CUTOFFANGLE	SFFloat
		;

ASpotLight	: SPOTLIGHT '{' FieldSpecs spotlightfields '}'
		;

--  /* Switch */
switchfields	: switchfield switchfields
		|
		;

switchfield	: WHICHCHILD	SFLong
		;

ASwitch		: SWITCH '{' FieldSpecs switchfields Nodes '}'
		;

--  /* Texture2 */
texture2fields	: texture2field texture2fields
		|
		;

texture2field	: FILENAME	SFString
		| IMAGE		SFImage
		| WRAPS		SFEnum
		| WRAPT		SFEnum
		;

ATexture2	: TEXTURE2 '{' FieldSpecs texture2fields '}'
		;

--  /* Texture transformation */
tex2transfields	: tex2transfield tex2transfields
		|
		;

tex2transfield	: TRANSLATION_F	SFVec2f
		| ROTATION_F	SFFloat
		| SCALEFACTOR	SFVec2f
		| CENTER	SFVec2f
		;

Tex2Transform	: TEXTURE2TRANSFORM '{' FieldSpecs tex2transfields '}'
		;

--  /* Texcoord2 */
texcoord2fields	: texcoord2field texcoord2fields
		|
		;

texcoord2field	: POINT		MFVec2f
		;

ATexCoordinate2	: TEXTURECOORDINATE2 '{' FieldSpecs texcoord2fields '}'
		;

--  /* Transformation */
transformfields	: transformfield transformfields
		|
		;

transformfield	: TRANSLATION_F		SFVec3f
		| ROTATION_F		SFRotation
		| SCALEFACTOR		SFVec3f
		| SCALEORIENTATION	SFRotation
		| CENTER		SFVec3f
		;

ATransform	: TRANSFORM '{' FieldSpecs transformfields '}'
		;

--  /* Transform Separator */
TransSeparator	: TRANSFORMSEPARATOR '{' FieldSpecs Nodes '}'
		;

--  /* Translation */
translationfields	: translationfield translationfields
			|
			;

translationfield	: TRANSLATION_F	SFVec3f
			;

ATranslation	: TRANSLATION '{' FieldSpecs translationfields '}'
		;

--  /* WWWAnchor */
WWWanchorfields	: WWWanchorfield WWWanchorfields
		|
		;

WWWanchorfield	: NAME		SFString
		| DESCRIPTION	SFString
		| MAP		SFEnum
		;

AWWWAnchor	: WWWANCHOR '{' FieldSpecs WWWanchorfields Nodes '}'
		;

--  /* WWWinline */
WWWinlinefields	: WWWinlinefield WWWinlinefields
		|
		;

WWWinlinefield	: NAME		SFString
		| BBOXSIZE	SFVec3f
		| BBOXCENTER	SFVec3f
		;

AWWWInline	: WWWINLINE '{' FieldSpecs WWWinlinefields '}'
		;



--  /* ========== Classes of Fields ========== */

--  /* Single Value Fields */
bitmasklist	: NUMBER 
		| bitmasklist '|' NUMBER {
            declare
              img: constant String:=
                Unsigned_32'Image(
                  Unsigned_32'Value($1.text(1..$1.length))
                    or
                  Unsigned_32'Value($3.text(1..$3.length))
                );
            begin
              $$.length := img'Length;
              $$.text(1..img'Length) := img;
              --  Single Value Fields / bitmasklist
            end;
            }
		;

SFBitMask	: NUMBER
		| '(' bitmasklist ')' {$$ := $2;}
		;

SFBool		: NUMBER
		;

SFColor	: SFFloat
              { last_color(0):= yylval.floatval; }
              SFFloat
              { last_color(1):= yylval.floatval; }
              SFFloat
              { last_color(2):= yylval.floatval;
                last_color(3):= 1.0;
                -- alpha is determined by the Transparency field;
                -- alpha=1 <-> Transparency=0 (default)
              }
		;

SFEnum		: NUMBER
		;

SFFloat	: FLOAT_t
		| NUMBER {$$ := ($1); -- Float
            }
		;

imglist		: NUMBER imglist { null; }
		|
		;

SFImage		: NUMBER NUMBER NUMBER imglist { null; }
		;

SFLong		: NUMBER
		;

SFMatrix	: SFFloat SFFloat SFFloat SFFloat
		  SFFloat SFFloat SFFloat SFFloat
		  SFFloat SFFloat SFFloat SFFloat
		  SFFloat SFFloat SFFloat SFFloat
              { null; 
                -- SFMatrix
              }
		;

SFRotation	: SFFloat SFFloat SFFloat SFFloat { null; }
		;

SFString	: VRMLSTRING
		;

SFVec2f		: SFFloat SFFloat { null; }
		;

SFVec3f	: SFFloat
              { last_pt(0):= yylval.floatval; }
              SFFloat
              { last_pt(1):= yylval.floatval; }
              SFFloat
              { last_pt(2):= yylval.floatval; }
		;

--  /* Multiple Value Field */
colorlist	: SFColor
		| SFColor ',' colorlist
		;

MFColor	: SFColor 
		| '[' colorlist ']'
		;

longlist	: SFLong { null; }
		| SFLong ',' longlist { null; }
		;

MFLong	: SFLong { null; }
		| '[' longlist ']' { null; }
		; 

longlist_1	:
          SFLong
             { Point_index (yylval.intval);
               --  Last index, should be end of last group
               Ada_Put ("  ");
               Ada_Comment (Integer'Image (sepa_polys (sepa_count)));
             }
		| SFLong ','
             { Point_index(yylval.intval);
               if flag_group then
                 --  We just finished a group, not the last one
                 if pretty then
                   Ada_Put (", ");
                   if sepa_polys (sepa_count) mod 3 = 0 then
                     Ada_Put (" ");
                     Ada_Comment (Integer'Image (sepa_polys (sepa_count)));
                   end if;
                 else
                   Ada_Put (",");
                   if sepa_polys (sepa_count) mod 8 = 0 then
                     Ada_New_Line;
                   end if;
                 end if;
               end if;
             }
              longlist_1 { null; }
		;

MFLong_1	: { Ada_Put("(1 => "); }
              SFLong
              { Point_index(yylval.intval);
                Ada_Put(")"); }
            | '['
              { Ada_Put("("); }
              longlist_1
              { Ada_Put(")"); }
              ']' { null; }
		; 

floatlist	: SFFloat { null; }
		| SFFloat ',' floatlist { null; }
		;

MFFloat		: SFFloat { null; }
		| '[' floatlist ']' { null; }
		; 

stringlist	: SFString { null; }
		| SFString ',' stringlist { null; }
		;

MFString	: SFString { null; }
		| '[' stringlist ']' { null; }
		; 

vec2flist	: SFVec2f
		| SFVec2f ',' vec2flist
		;

MFVec2f		: SFVec2f
		| '[' vec2flist ']'
		; 

vec3flist	: SFVec3f     
              { Ada_Put_Line (Coords (last_pt)); }
		| SFVec3f ','
              { if pretty then
                  Ada_Put_Line (Coords (last_pt) & ", ");
                else
                  Ada_Put_Line (Coords (last_pt) & ",");
                end if;
              }
              vec3flist
		-- | ',' -- bug in certain VRML files, tolerated
		;

MFVec3f	:  { Ada_Put("(1 => "); }
               SFVec3f
               { Ada_Put(")"); }
		| 
              '['
               { Ada_Put("( "); }
               vec3flist
               { Ada_Put(")"); }
              ']'
		; 

vec3flist_coo	:
          SFVec3f     
              { Ada_Put (Coords (last_pt) & "  ");
                sepa_points (sepa_count) := sepa_points (sepa_count) + 1;
                Ada_Comment (Integer'Image (sepa_points (sepa_count)));
              }
		| SFVec3f ','
              { sepa_points (sepa_count) := sepa_points (sepa_count) + 1;
                if pretty then
                  Ada_Put (Coords (last_pt) & ", ");
                  if sepa_points (sepa_count) mod 2 = 0 then
                    Ada_Put (" ");
                    Ada_Comment (Integer'Image (sepa_points (sepa_count)));
                  end if;
                else
                  Ada_Put (Coords (last_pt) & ",");
                  if sepa_points (sepa_count) mod 5 = 0 then
                    Ada_New_Line;
                  end if;
                end if;
              }
              vec3flist_coo
		-- | ',' -- bug in certain VRML files, tolerated
		;

MFVec3f_coo	:  { Ada_Put ("(1 => "); }
               SFVec3f
               { Ada_Put (")");
                 sepa_points(sepa_count) := sepa_points (sepa_count) + 1;
                }
		| 
              '['
               { Ada_Put("("); }
               vec3flist_coo
               { Ada_Put(")"); }
              ']'
		; 

%%

-- wrl2ada

with VRML_Tokens, VRML_Shift_Reduce, VRML_Goto, VRML_Help, VRML_IO;
use  VRML_Tokens, VRML_Shift_Reduce, VRML_Goto, VRML_Help, VRML_IO;

with VRML_DFA, YYroutines, YYerror;
use  VRML_DFA, YYroutines;

with Ada.Text_IO;                       use Ada.Text_IO;
with Text_IO; -- for compat.

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Interfaces;                        use Interfaces;

--yyerror(char *s) 
--{ 
--  extern  char    yytext[];
--  int linenum;
--  char *ff;

--  linenum = vrml_filel_linenum(&ff);
--  fprintf(stderr, "%s:%d: Error:\n%s at or before '%s'\n\n\n", 
--	  ff,linenum, (char *)s, *yytext=='\n'? "\\n": yytext); 
--  return;
--}

##