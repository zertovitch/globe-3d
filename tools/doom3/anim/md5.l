--------------------------------------------------------------------------
--  MD5.l
--
--  MD5 mesh / anim grammar file (AFLEX)
--
--  Copyright (c) Gautier de Montmollin 2017
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 28-Jul-2008 on the site
-- http://www.opensource.org/licenses/mit-license.php
--------------------------------------------------------------------------
--
-- Change log:
--
-- xx-yyy-2017 GdM: Created
--

INSTANCE    [^0-9{}\'\"\\ \t\n\f[:cntrl:]][^{}\'\"\\ \t\n\f[:cntrl:]]*
LETTER [a-zA-Z_]
DIGIT [0-9]
IDCHAR [0-9a-zA-Z_]
GUIL [\"]
NOTGUIL [^\"]
STAR [*]
NOTSTAR [^*]
SLASH [/]
LBRACE [{]
RBRACE [}]
NOTSLASHSTAR [^/*]
NOTGREATER [^>]

%%

-- /* ... */ -style comments
{SLASH}{STAR}(({NOTSTAR}*)|({STAR}+{NOTSLASHSTAR}))*({STAR}+{SLASH}) {
         MD5_Help.MD5_Comment(YYText(3..YYLength-2));
         }

-- // -style comment -> skip rest of line.
"//".*\n                 {
         MD5_Help.MD5_Comment(YYText);
                      linenum:= linenum + 1;
         }

"|"              { return BAR_t; }
,                { return COMMA_t; }
{LBRACE}         { return LBRACE_t; }
{RBRACE}         { return RBRACE_t; }
"("                { return '('; }
")"                { return ')'; }
"#include"       { return C_INCLUDE_t; }
NOT              { return NOT_t; }

-----------
-- Noise --
-----------

"#define".*\n    { null; } -- skip rest of line - was { return C_CRAP_t; }
"#undef".*\n     { null; }
"#if".*\n        { null; }
"#ifdef".*\n     { null; }
"#ifndef".*\n    { null; }
"#else".*\n      { null; }
"#elif".*\n      { null; }
"#endif".*\n     { null; }
"#pragma".*\n    { null; }

---------------
-- Resources --
---------------

MD5Version               { return MD5Version_t; }
commandline              { return commandline_t; }
                         
numJoints                { return numJoints_t; }
numMeshes                { return numMeshes_t; }
joints                   { return joints_t; }
mesh                     { return mesh_t; }
numverts                 { return numverts_t; }
vert                     { return vert_t; }
numtris                  { return numtris_t; }
tri                      { return tri_t; }
numweights               { return numweights_t; }
weight                   { return weight_t; }
                         
numFrames                { return numFrames_t; }
frameRate                { return frameRate_t; }
numAnimatedComponents    { return numAnimatedComponents_t; }

TRUE                { yylval.intval := 0; return NUMBER; }
FALSE               { yylval.intval := 1; return NUMBER; }

0                { yylval.intval   := 0;
                   yylval.floatval := 0.0;
                   return NUMBER;
                 }

-?[1-9][0-9]*    { begin
                     yylval.intval := Long_Long_Integer'Value(yytext);
                   exception
                     when Constraint_Error =>
                      -- declare
                      --   msg: constant String:=
                      --    "Error with integer ? [" & yytext & ']';
                      -- begin
                      --   Put_Line(Standard_Error, msg);
                      --   raise Constraint_Error with msg;
                      -- end;
                      yylval.intval := -1;
                      -- not nice at all, but a >2**31 number
                      -- seems to appear only at top (some checksum)
                   end;
                   yylval.floatval:= Long_Float( yylval.intval );
                   return NUMBER;}

0[1-7][0-7]*     { yylval.intval := Long_Long_Integer'Value("8#"&yytext&'#');
                   yylval.floatval:= Long_Float( yylval.intval );
                   return NUMBER;}

-?[0-9]*\.?[0-9]*([eE][+-]?[0-9]+)? {
                   yylval.floatval := Long_Float'Value(yytext);
                   return FLOAT_t;
                   }

0x[0-9A-Fa-f]+L?   { declare
                     hexa_c: String:= yytext;
                     hexa_last: Integer:= hexa_c'Last;
                   begin
                     if hexa_c(hexa_last)='L' then
                       hexa_last:= hexa_last - 1;
                     end if;
                     yylval.intval := Long_Long_Integer'Value("16#" & hexa_c(hexa_c'First+2 .. hexa_last) & '#');
                     yylval.floatval:= Long_Float( yylval.intval );
                     return NUMBER;
                   end;
                }

\"(\"\"|[^\n\"])*\"    {return RCString;} -- borrowed from ada95.l

-- Doesn't work with double quotes
-- \"({NOTGUIL})*\"    {return RCString;}

"<"{NOTGREATER}*">" {return INCString;}

[a-zA-Z_][a-zA-Z0-9_.]* { return IDENT_t; }


[ \t]+               { null; -- \t } ;
\n                {linenum:= linenum+1;}

[|]                { null; -- ??
                     -- C: return *yytext;
                   }
-- "("                { return '('; }
-- ")"                { return ')'; }
-- "}"                { return '}'; }
-- "{"                { return '{'; }

%%

with MD5_Tokens; use MD5_Tokens;
with MD5_Help;   use MD5_Help;

##
