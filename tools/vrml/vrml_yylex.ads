with VRML_tokens; use VRML_tokens;
with yylex;

function VRML_yylex return token renames yylex;
