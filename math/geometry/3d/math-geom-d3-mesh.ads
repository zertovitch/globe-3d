
with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.ordered_Maps;
with ada.containers.hashed_Sets;

with ada.unchecked_Deallocation;
with ada.Finalization;



package math.geom.d3.Mesh is
   --
   -- models a triangle mesh.


   type Item is abstract new math.Geom.d3.item with private;


   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;



   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);










   -- mesh operations
   --

   procedure add_Triangle (Self : access Item;   Vertex_1, Vertex_2, Vertex_3 : in math.geom.d3.Vertex) is abstract;



   function  Model     (Self : access Item)                                  return Model_view  is abstract;
   procedure Model_is  (Self : access Item'Class;   Now  : in     d3.Model_view);

   procedure clear     (Self : access Item)                                                is abstract;
   procedure add_Model (Self : access Item;          Now  : in     d3.Model_view)                  is abstract;



   function Volume_of (Self : access Item) return Number;


   procedure expand (Self : access Item;            By   : in     Number);









   -- heightmap mesh
   --

   ----function to_mesh_Model  (From           : in polar_Model) return mesh.Model_item;
   --function to_Mesh (from_Heightmap : in math.Matrix.item'class) return Mesh.item'Class is abstract;
--     procedure add_Heightmap (Self          : access Item'Class;
--                              the_heightmap : in     math.Matrix.item'class);






   -- polar model
   --

   type latitude  is range -90 ..  90;
   type longitude is range   0 .. 360;


   type id_Vertex is
      record
         Id     : Positive;
         Vertex : d3.Vertex;
      end record;


   type longitude_line is array (latitude)  of id_Vertex;
   type polar_model    is array (longitude) of longitude_line;
   type polar_Model_view is access all polar_Model;


   procedure free is new ada.unchecked_deallocation (polar_Model, polar_Model_view);


   function to_polar_Model (Model_Filename : in String;
                            Scaled_by      : in Number := 1.0) return polar_Model;

   function to_Model (From           : in polar_Model)  return d3.Model;
   function to_Mesh  (From           : in polar_Model) return Mesh.item'Class        is abstract;







private


   type Item is abstract new math.Geom.d3.item with
      record
         null;
--         Data : Data_view;                  -- using view due to large size (ie avoid stack_overflow)
      end record;








end math.geom.d3.Mesh;

