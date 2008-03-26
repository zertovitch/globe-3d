
with math.Geom.d2.polygon;



package math.geom.d2.polygon.Box is


   --type Item is new math.Geom.d2.item with private;
   type Item is new math.Geom.d2.polygon.item (vertex_Count => 4) with private;

   type Cast   is access all Item;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);


   function  Sides     (Self : in     Item)          return Vector_2;
   procedure Sides_are (Self : in out Item;   Now  : in     Vector_2);




   procedure expand (Self : access Item;
                     By   : in     Number);


--   function bounding_sphere_Radius (Self : access Item) return Number;
   function Area    (Self : in Item) return Number;


   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;





private

--   type Item is new math.Geom.d2.item with
   type Item is new math.Geom.d2.polygon.item (vertex_Count => 4) with
      record
         Sides : math.Vector_2;
      end record;


end math.geom.d2.polygon.Box;

