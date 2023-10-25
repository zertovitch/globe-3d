with Vrml_Tokens;

package yyroutines is

  use Vrml_Tokens;

  function YYLex return Token;

  procedure UnYYLex (tok : Token);

end yyroutines;
