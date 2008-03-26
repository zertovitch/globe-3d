


package math.geom.d3.Sphere is


   type Item is new math.Geom.d3.item with private;

   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);


   function  Radius    (Self : access Item                          ) return Number;
   procedure Radius_is (Self : access Item;   Now    : in     Number);


   function  bounding_sphere_Radius (Self : access Item) return Number;
   function  Volume_of              (Self : access Item) return Number;


   procedure expand (Self : access Item;
                     By   : in     Number);




   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view;


   subtype quality_Level is math.Number range 0.0 .. 1.0;

   type model_Options is new math.geom.d3.model_Options with
      record
         Quality : quality_Level := 0.5;
      end record;

   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view;




private

   type Item is new math.Geom.d3.item with
      record
         Radius : Number;
      end record;


end math.geom.d3.Sphere;

