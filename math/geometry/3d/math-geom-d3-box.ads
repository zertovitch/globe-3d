
package math.geom.d3.Box is


   type Item is new math.Geom.d3.item with private;

   type Cast   is access all Item;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);


   function  Sides     (Self : access Item) return Vector_3;
   procedure Sides_are (Self : access Item; Now  : in     Vector_3);




   procedure expand (Self : access Item;
                     By   : in     Number);


   function bounding_sphere_Radius (Self : access Item) return Number;
   function Volume_of              (Self : access Item) return Number;


   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;





private

   type Item is new math.Geom.d3.item with
      record
         Sides : math.Vector_3;
      end record;


end math.geom.d3.Box;

