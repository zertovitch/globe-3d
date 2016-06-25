package body GLOBE_3D.Sprite is

   overriding
   function skinned_Geometries (o : in Sprite) return GL.Skinned_Geometry.Skinned_Geometries
   is
   begin
      return o.skinned_Geometries (1 .. o.skinned_geometry_Count);
   end;

   procedure add (o : in out Sprite;   Geometry : access GL.Geometry.Geometry'Class;
                                       Skin     : access GL.Skins.Skin'Class)
   is
   begin
      o.skinned_geometry_Count                        := o.skinned_geometry_Count + 1;
      o.skinned_Geometries (o.skinned_geometry_Count) := (Geometry => Geometry.all'Access,
                                                         Skin     => Skin.all'Access,
                                                         Veneer   => Skin.new_Veneer (for_Geometry => Geometry.all));
   end;

   overriding
   procedure Pre_calculate (o: in out Sprite)
   is
      use GL, GL.Geometry;
   begin
      --vertex_cache_optimise (o);  -- tbd: doesn't seem to help !! ... :(
                                  -- at least with terrain ... (terrain dataset may already naturally be in optimal order ?)
                                  -- so need to test with other dataset
      o.Bounds     := null_Bounds;
      o.face_Count := 0;

      for Each in 1 .. o.skinned_geometry_Count
      loop
         o.Bounds     := Max (o.Bounds,  o.skinned_Geometries (Each).Geometry.Bounds);
         o.face_Count := o.face_Count + o.skinned_Geometries (Each).Geometry.face_Count;
      end loop;
   end Pre_calculate;

   overriding
   procedure destroy (o : in out Sprite)
   is
   begin
      null;
   end;

   overriding
   function face_Count (o : in Sprite) return Natural
   is
   begin
      return o.face_Count;
   end;

   overriding
   function Bounds (o : in Sprite) return GL.Geometry.Bounds_record
   is
   begin
      return o.Bounds;
   end;

   overriding
   procedure Display (o    : in out Sprite;
                      clip : in     Clipping_data)
   is
   begin
      null;   -- Actual display is done by the renderer (ie glut.Windows), which requests all skinned Geometry's
              -- and then applies 'gl state' sorting for performance, before drawing.
   end Display;

   overriding
   procedure set_Alpha (o    : in out Sprite;   Alpha : in GL.Double)
   is
   begin
      null;   -- todo
   end;

   overriding
   function  is_Transparent (o    : in Sprite) return Boolean
   is
   begin
      return o.is_Transparent;  -- todo: ensure this is updated when new primitives (with possible transparent appearance' are added.
   end;

end GLOBE_3D.Sprite;
