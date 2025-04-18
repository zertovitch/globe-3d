package body GLOBE_3D.Skinned_Visuals is

  function Width  (o : in Skinned_Visual'class) return Real
  is
  begin
     return Bounds (o).Box.X_Extent.Max - Bounds (o).Box.X_Extent.Min;
  end Width;

  function Height  (o : in Skinned_Visual'class) return Real
  is
  begin
     return Bounds (o).Box.Y_Extent.Max - Bounds (o).Box.Y_Extent.Min;
  end Height;

  function Depth  (o : in Skinned_Visual'class) return Real
  is
  begin
     return Bounds (o).Box.Z_Extent.Max - Bounds (o).Box.Z_Extent.Min;
  end Depth;

end GLOBE_3D.Skinned_Visuals;
