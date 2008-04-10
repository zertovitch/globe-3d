with Doom3_tokens;

package yyroutines is

  use Doom3_tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end yyroutines;
