package body GLOBE_3D.Options is

  function Is_debug_mode return Boolean is
  begin
    return
      show_normals or
      show_portals or
      full_check_objects or
      filter_portal_depth or
      portal_tracking or
      BSP_tracking;
  end Is_debug_mode;

end GLOBE_3D.Options;
