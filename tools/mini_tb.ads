-- Trace-Back wrapper for post-mortem debugging the mini demo with GNAT

with TB_Wrap, Mini;

procedure Mini_tb is new TB_Wrap(Mini);