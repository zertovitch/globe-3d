with MD5_yylex;

package body yyroutines is

  Lookahead : Token;
  HaveLookahead : Boolean := False;
  SecondUnYYLex : exception;

  function YYLex return Token is
  begin
    if HaveLookahead then
      HaveLookahead := False;
      return Lookahead;
    else
      return MD5_yylex;
    end if;
  end YYLex;

  procedure UnYYLex(tok : Token) is
  begin
    if HaveLookahead then
      raise SecondUnYYLex;
    else
      HaveLookahead := True;
      Lookahead := tok;
    end if;
  end UnYYLex;

end yyroutines;
