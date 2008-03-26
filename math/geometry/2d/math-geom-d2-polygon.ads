
package math.geom.d2.Polygon is

   pragma optimize (Time);


   type Item (vertex_Count : math.Natural) is new math.Geom.d2.item with private;

   type Cast   is access all Item;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item);



   --function Vertices (Self : in     Item) return d2.Vertices;
   function Vertices (Self : access Item'Class) return access d2.Vertices;



   procedure transform (Self : in out Item;   Site     : in math.Vector_2 := (0.0, 0.0);
                                              Attitude : in math.Number   := 0.0        );




   procedure expand (Self : access Item;
                     By   : in     Number);


--   function bounding_sphere_Radius (Self : access Item) return Number;
   function Area    (Self : in Item) return Number;


   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;





private

   type Item (vertex_Count : math.Natural) is new math.Geom.d2.item with
      record
         Vertices : access math.geom.d2.Vertices := new math.geom.d2.Vertices (1 .. vertex_Count);
--         Vertices : aliased math.geom.d2.Vertices (1 .. vertex_Count);
      end record;




   pragma inline_always (Vertices);


end math.geom.d2.Polygon;

