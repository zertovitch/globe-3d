with VRML_tokens;

package yyroutines is

  use VRML_tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end yyroutines;
