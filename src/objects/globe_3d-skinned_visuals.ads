with GL.Geometry, GL.Skinned_Geometry;

package GLOBE_3D.Skinned_Visuals is

   type Skinned_Visual is abstract new Visual with
      record
         is_Terrain : Boolean := False;
      end record;

   type p_Skinned_Visual is access all Skinned_Visual'Class;
   type Skinned_Visual_Array is array (Positive range <>) of Skinned_Visuals.p_Skinned_Visual;

   function Bounds (o : in Skinned_Visual) return GL.Geometry.Bounds_record is abstract;

   function Skinned_Geometries (o : in Skinned_Visual) return GL.Skinned_Geometry.Skinned_Geometries
     is abstract;

   function Width  (o : in Skinned_Visual'class) return Real;
   function Height (o : in Skinned_Visual'class) return Real;
   function Depth  (o : in Skinned_Visual'class) return Real;

end GLOBE_3D.Skinned_Visuals;
