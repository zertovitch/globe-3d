with Doom3_YYlex, Doom3_tokens;

package body yyroutines is
  use Doom3_tokens;

  Lookahead : Token;
  HaveLookahead : Boolean := False;
  SecondUnYYLex : exception;

  function YYLex return Token is
  begin
    if HaveLookahead then
      HaveLookahead := False;
      return Lookahead;
    else
      return Doom3_YYlex;
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
