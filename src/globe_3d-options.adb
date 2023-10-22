package body GLOBE_3D.Options is

  function Is_Debug_Mode return Boolean is
  begin
    return
      show_normals or
      show_portals or
      show_texture_labels or
      full_check_objects or
      filter_portal_depth or
      portal_tracking or
      BSP_tracking;
  end Is_Debug_Mode;

end GLOBE_3D.Options;
