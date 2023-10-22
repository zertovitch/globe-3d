package GLOBE_3D.Options is

  --  Visual checks:

  show_normals        : constant Boolean := False;
  show_portals        : constant Boolean := False;
  show_texture_labels : constant Boolean := False;
  filter_portal_depth : constant Boolean := False;

  --  Formal checks:

  full_check_objects  : constant Boolean := False;
  strict_geometry     : constant Boolean := False;

  --  Misc.:
  portal_tracking     : constant Boolean := True;
  BSP_tracking        : constant Boolean := True;

  function Is_Debug_Mode return Boolean;

  arrow_inflator : constant := 10.0;

end GLOBE_3D.Options;
