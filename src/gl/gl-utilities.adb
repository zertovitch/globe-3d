package body GL.Utilities is

  procedure Clear_Modes is
  begin
    Disable (Blend);
    Disable (Lighting);
    Disable (Auto_Normal);
    Disable (Normalize);
    Disable (Depth_Test);
  end Clear_Modes;

end GL.Utilities;
