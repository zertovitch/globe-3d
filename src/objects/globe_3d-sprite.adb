with GLOBE_3D.Textures,
     GLOBE_3D.Math;



package body GLOBE_3D.Sprite is


   package G3DT renames GLOBE_3D.Textures;
   package G3DM renames GLOBE_3D.Math;




   function skinned_Geometrys (o : in Sprite) return gl.skinned_geometry.skinned_Geometrys
   is
   begin
      return o.skinned_Geometrys (1 .. o.skinned_geometry_Count);
   end;





   procedure add (o : in out Sprite;   Geometry : access gl.geometry.Geometry'Class;
                                       Skin     : access gl.skins.Skin'Class)
   is
      new_skinned_Geometry : access gl.skinned_Geometry.skinned_Geometry := new gl.skinned_Geometry.skinned_Geometry;
   begin
      o.skinned_geometry_Count                       := o.skinned_geometry_Count + 1;
      o.skinned_Geometrys (o.skinned_geometry_Count) := (geometry => Geometry.all'access,
                                                         skin     => Skin.all'access,
                                                         veneer   => Skin.new_Veneer (for_geometry => Geometry.all));
   end;





   procedure Pre_calculate (o: in out Sprite)
   is
      use GL, gl.Geometry, G3DM;
   begin
      --vertex_cache_optimise (o);  -- tbd: doesn't seem to help !! ... :(
                                  -- at least with terrain ... (terrain dataset may already naturally be in optimal order ?)
                                  -- so need to test with other dataset

      o.Bounds     := null_Bounds;
      o.face_Count := 0;

      for Each in 1 .. o.skinned_geometry_Count loop
         o.Bounds     := max (o.Bounds,  o.skinned_Geometrys (Each).geometry.Bounds);
         o.face_Count := o.face_Count + o.skinned_Geometrys (Each).geometry.face_Count;
      end loop;


      -- setup bounding_sphere (for debug)
      --
--        declare
--           use GLU;
--        begin
--           if o.bounding_sphere_Quadric /= null then
--              glu.quadricDrawStyle (o.bounding_sphere_Quadric, glu.glu_LINE);
--           end if;
--        end;


    -- Ooof. Now we can certify:
      --o.pre_calculated:= True;
   end Pre_calculate;





   procedure destroy (o : in out Sprite)
   is
   begin
      null;
   end;




   function face_Count (o : in Sprite) return Natural
   is
   begin
      return o.face_Count;
   end;



   function Bounds (o : in Sprite) return gl.geometry.Bounds_record
   is
   begin
      return o.Bounds;
   end;





   procedure Display (o    : in out Sprite;
                      clip : in     Clipping_data)
   is
   begin
      null;   -- actual display is done by the renderer (ie glut.Windows), which requests all skinned Geometry's
              -- and then applies 'gl state' sorting for performance, before drawing.
   end Display;



   procedure set_Alpha (o    : in out Sprite;   Alpha : in gl.Double)
   is
   begin
      null;   -- tbd
   end;



   function  is_Transparent (o    : in Sprite) return Boolean
   is
   begin
      return o.is_Transparent;  -- tbd: ensure this is updated when new primitives (with possible transparent appearance' are added.
   end;




end GLOBE_3D.Sprite;

