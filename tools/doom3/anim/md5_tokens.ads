package Md5_Tokens is



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
     intval  : Long_Long_Integer;
     floatval: Long_Float;
  end record;


    YYLVal, YYVal : YYSType;

    type Token is
        (End_Of_Input, Error, Number, Float_T,
         Comma_T, Bar_T, Lbrace_T,
         Rbrace_T, Not_T, C_Include_T,
         Md5version_T, Commandline_T, Numjoints_T,
         Nummeshes_T, Joints_T, Mesh_T,
         Shader_T, Numverts_T, Vert_T,
         Numtris_T, Tri_T, Numweights_T,
         Weight_T, Numframes_T, Framerate_T,
         Numanimatedcomponents_T, Hierarchy_T, Bounds_T,
         Baseframe_T, Frame_T, Ident_T,
         Stringtable_T, Rcstring, Incstring,
         Consume_Eol_T, '(', ')' );

    Syntax_Error : exception;

end Md5_Tokens;
