package Doom3_Tokens is



  type const_type is (
    intval, 
    floatval, 
    doubleval, 
    stringval, 
    any_type
  );

  type YYSType is record
     text    : String(1..255);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Long_Float;
  end record;


    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Number, Float_T,
         D3string, Mapprocfile003_T, Mapprocfile_T,
         Model_T, Shadowmodel_T, Interareaportals_T,
         Nodes_T, '{', '}',
         '(', ')' );

    Syntax_Error : exception;

end Doom3_Tokens;
