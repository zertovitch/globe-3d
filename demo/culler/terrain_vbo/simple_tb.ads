-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, Simple;

procedure Simple_TB is new TB_Wrap(Simple);