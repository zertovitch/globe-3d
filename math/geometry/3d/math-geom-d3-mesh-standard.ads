
with math.geom.d3.Modeller_full_detail;

with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.ordered_Maps;
with ada.containers.hashed_Sets;

with ada.unchecked_Deallocation;
with ada.Finalization;





package math.geom.d3.Mesh.standard is
   --
   -- models a triangle mesh.


   type Item is new math.Geom.d3.Mesh.item with private;


   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;



   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);




   procedure add_Triangle (Self : access Item;   Vertex_1, Vertex_2, Vertex_3 : in math.geom.d3.Vertex);


   function  Model     (Self : access Item)                             return d3.Model_view;
   procedure add_Model (Self : access Item;   Now  : in     d3.Model_view);
   procedure clear     (Self : access Item);



   function Volume_of              (Self : access Item) return Number;
   function bounding_sphere_Radius (Self : access Item) return Number;


   procedure expand (Self : access Item;
                     By   : in     Number);



   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return d3.Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;


   -- heightmap mesh
   --

--   function to_Mesh (from_Heightmap : in math.Matrix.item'class) return Mesh.item'Class;






   -- polar model
   --

--   function to_Mesh        (From           : in polar_Model) return Mesh.item'class;







private


   type Item is new math.Geom.d3.Mesh.item with
      record
--         Data : Data_view;                  -- using view due to large size (ie avoid stack_overflow)
         Modeller : aliased math.geom.d3.Modeller_full_detail.item;
      end record;






end math.geom.d3.Mesh.standard;

