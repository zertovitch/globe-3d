with MD5_Tokens;

package yyroutines is

  use MD5_Tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end yyroutines;
