-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, Sprite_demo;

procedure Sprite_TB is new TB_Wrap(Sprite_demo);
