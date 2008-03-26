
with math.geom.d3.Mesh.standard;




package math.geom.d3.Capsule is
   --
   -- a cylinder with hemi-sphere ends.


   type Item is new math.Geom.d3.item with private;

   type View is access all Item;
   type Id   is access all Item'class;
   type Ids  is array (Positive range <>) of Id;



   -- build
   --

   procedure destroy (Self : in out Item);



   -- attributes
   --

   function  Radius    (Self : access Item) return Number;
   procedure Radius_is (Self : access Item;
                        Now  : in     Number);


   function  Length    (Self : access Item) return Number;
   procedure Length_is (Self : access Item;
                        Now  : in     Number);


   function bounding_sphere_Radius (Self : access Item) return Number;
   function Volume_of              (Self : access Item) return Number;


--   function Mesh       (Self : access Item) return math.geom.d3.Mesh.view;




   -- methods
   --

   procedure expand (Self : access Item;
                     By   : in     Number);





   -- capsule model
   --

--     type quality_Level is range 2 .. 5_000; -- higher quality results in more precise model with more triangles.
--
--
--     function  Quality    (Self : access Item) return quality_Level;
--     procedure Quality_is (Self : access Item;
--                           Now  : in     quality_Level);


--     function to_capsule_Model  (Length  : in Number;
--                                 Radius  : in Number;
--                                 Quality : in capsule.Quality := 4) return math.geom.d3.mesh.Model;



   --function to_Model (Self : access Item;   of_Quality : in model_Quality := model_Quality'Last) return Model_view;

   subtype quality_Level is math.Number range 0.0 .. 1.0;

   type model_Options is new math.geom.d3.model_Options with
      record
         Quality : quality_Level := 0.25;
      end record;

   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view;








private


   type Item is new math.Geom.d3.item with
      record
         Radius           : Number;  -- tbd: defaults ?
         Length           : Number;
--         Quality          : quality_Level := 4;

         Size_has_changed : Boolean := True;  -- initially 'True' to force mesh rebuild.

--           --Mesh             : aliased math.geom.d3.Mesh.item;
--           Mesh             : aliased math.geom.d3.Mesh.standard.item;
      end record;


end math.geom.d3.Capsule;

