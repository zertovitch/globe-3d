
with Math;



package math.geom.d3.Plane is


   type Item is new math.Geom.d3.item with
      record
         Equation : math.Numbers (1 .. 4);
      end record;

   type Items is array (Positive range <>) of aliased Item;


   type Cast   is access all Item;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);


   function  Equation    (Self : access Item                                   ) return math.Numbers;
   procedure Equation_is (Self : access Item;   Now  : in     math.Numbers);




   procedure expand (Self : access Item;
                     By   : in     Number);

   function Volume_of              (Self : access Item) return Number;
   function bounding_sphere_Radius (Self : access Item) return Number;


   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;







end math.geom.d3.Plane;

