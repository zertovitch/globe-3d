-- Trace-Back wrapper for post-mortem debugging the demo with GNAT

with TB_Wrap, launch_Terrain_vbo;

procedure Terrain_vbo_TB is new TB_Wrap(launch_Terrain_vbo);
