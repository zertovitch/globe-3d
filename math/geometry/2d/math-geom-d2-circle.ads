
package math.geom.d2.Circle is

   pragma optimize (Time);


   type Item is new math.Geom.d2.item with private;

   type Cast   is access all Item;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);



   function  Origin    (Self : access Item)   return access math.Vector_2;
   procedure Origin_is (Self : in out Item;   Now  : in     math.Vector_2);


   function  Radius    (Self : in     Item)          return Number;
   procedure Radius_is (Self :    out Item;   Now  : in     Number);


   procedure transform (Self : in out Item;   Site     : in math.Vector_2 := (0.0, 0.0);
                                              Attitude : in math.Number   := 0.0        );


   procedure expand (Self : access Item;
                     By   : in     Number);


--   function bounding_sphere_Radius (Self : access Item) return Number;
   function Area    (Self : in Item) return Number;


   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;





private

   type Item is new math.Geom.d2.item with
      record
         Radius :         math.Number;
         Origin : aliased math.Vector_2 := (0.0, 0.0);
      end record;


end math.geom.d2.Circle;

