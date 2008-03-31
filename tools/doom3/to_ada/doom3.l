-- Doom3.l

INSTANCE    [^0-9{}\'\"\\ \t\n\f[:cntrl:]][^{}\'\"\\ \t\n\f[:cntrl:]]*
LETTER [a-zA-Z_]
DIGIT [0-9]
IDCHAR [0-9a-zA-Z_]
GUIL [\"]
NOTGUIL [^\"]
STAR [*]
NOTSTAR [^*]
SLASH [/]
NOTSLASHSTAR [^/*]

%%

-- /* ... */ -style comments
{SLASH}{STAR}(({NOTSTAR}*)|({STAR}+{NOTSLASHSTAR}))*({STAR}+{SLASH}) {
         Doom3_Help.Doom3_Comment(YYText(3..YYLength-2));
         }

-- // -style comment -> skip rest of line.
"//".*\n                 {
         Doom3_Help.Doom3_Comment(YYText);
                      linenum:= linenum + 1;
         }

PROC             { return mapProcFile_t; }
mapProcFile003   { return mapProcFile003_t; }
model            { return model_t; }
shadowModel      { return shadowModel_t; }
interAreaPortals { return interAreaPortals_t; }
nodes            { return nodes_t; }


TRUE                { yylval.intval := 0; return NUMBER; }
FALSE               { yylval.intval := 1; return NUMBER; }

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

\"({NOTGUIL})*\" {return D3String;}

[ \t]+               { null; -- \t } ;
\n                {linenum:= linenum+1;}

[|]                { null; -- ??
                     -- C: return *yytext;
                   }
"("                { return '('; }
")"                { return ')'; }
"}"                { return '}'; }
"{"                { return '{'; }

%%

with DOOM3_Tokens; use DOOM3_Tokens;
with DOOM3_Help;   use DOOM3_Help;

##
