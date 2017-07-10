package MD5_Help is

  procedure YY_Accept;
  procedure YY_Abort;
  procedure YY_Terminate;
  procedure MD5_Comment(s: String);

  procedure Reset_globals;
  
  linenum : Integer;
  Syntax_Error : exception;

  --
  
  num_joints: Natural;
  num_meshes: Natural;
  --
  num_frames: Natural;
  frame_rate: Natural;
  
end;