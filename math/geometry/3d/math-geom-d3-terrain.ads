
with gl.Geometry;

with Swig;
with interfaces.C;
with ada.unchecked_Deallocation;



package math.geom.d3.Terrain is


   type Item is new math.Geom.d3.item with private;

   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;
   type Grid   is array (Positive range <>, Positive range <>) of View;


   procedure destroy (Self : in out Item);


   procedure free is new ada.unchecked_Deallocation (interfaces.c.c_Float, swig.float_Pointer);


   function  Scale    (Self : in     Item)     return Vector_3;
   procedure Scale_is (Self : access Item;   Now : in Vector_3);

   -- nb: these are raw heights and are *not* scaled.
   --
   function  Heights               (Self : access Item                                                           ) return Matrix;
   procedure Heights_are           (Self : access Item;   Now        : in         Matrix                         );

   function  Length_of             (Self : access Item                                                           ) return Number;
   procedure Length_of             (Self : access Item;   is_Now     : in     Number                             );

   function  num_Nodes_per_Side_of (Self : access Item                                                           ) return Integer;
   procedure num_Nodes_per_Side_of (Self : access Item;   is_Now     : in     Integer                            );

   function bounding_sphere_Radius (Self : access Item) return Number;
   function height_Offset          (Self : access Item) return Number;

   --tbd: rename to 'x_Extent' ?
   function Width                  (Self : access Item) return Number;     -- extent in X axis
   function Depth                  (Self : access Item) return Number;     -- extent in Z axis

   function  Volume_of             (Self : access Item                                                           ) return Number;
   procedure expand                (Self : access Item;   By         : in     Number                             );


   --function  to_Model              (Self : access Item;   of_Quality : in     model_Quality := model_Quality'Last) return Model_view;


   subtype quality_Level is math.Number range 0.0 .. 1.0;

   type model_Options is new math.geom.d3.model_Options with
      record
         Skirt : natural_Number := 0.0;
      end record;

   null_terrain_Options : model_Options := model_Options'(others => <>);    -- tbd: how to make this constant ?

   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view;



--   function to_gl_Strips           (Self : access Item) return gl.geometry.triangle_Strips;





private


--     type number_Block_view is access all math.number_Block;
--     procedure free is new ada.unchecked_deallocation (math.number_Block, number_Block_view);

   type Matrix_view is access all math.Matrix;
   procedure free is new ada.unchecked_deallocation (math.Matrix, Matrix_view);



   type Item is new math.Geom.d3.item with
      record
         Scale : Vector_3 := (1.0, 1.0, 1.0);

         --Heights            : Numbers_view;
         Heights            : Matrix_view;


         Length             : Number;  -- these are probly obsolete
         num_Nodes_per_Side : Integer; --

         min_Height             : Number;
         max_Height             : Number;
         height_Offset          : Number;

         bounding_sphere_Radius : Number;
      end record;


end math.geom.d3.Terrain;

