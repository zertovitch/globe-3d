-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, GLOBE_3D_Demo;

procedure g3dem_tb is new TB_Wrap(GLOBE_3D_Demo);