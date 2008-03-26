
with Math;

with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.hashed_Sets;
with ada.containers.ordered_Maps;

with ada.unchecked_Deallocation;
with ada.finalization;




package math.Geom.d2 is
   --
   -- the base class for all 3 dimensional (d3) geometry.

   pragma optimize (Time);



   type Item   is abstract new math.Geom.item with private;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure free    (Self : in out View);


   procedure transform (Self : in out Item;   Site     : in math.Vector_2 := (0.0, 0.0);
                                              Attitude : in math.Number   := 0.0        )   is abstract;
   --
   -- adjusts a polygon vertices or a circles centre to correspnd with the given site and attitude (rotation).



   -- vertices


   subtype Vertex is math.Vector_2;


   subtype Vertices is math.Vector_2_array;
   --type Vertices is array (Positive range <>) of Vertex;


   use type Vertex;
   package vertex_Vectors is new ada.containers.Vectors (Positive, Vertex);

   subtype vertex_Vector        is vertex_Vectors.Vector;
   subtype vertex_vector_Cursor is vertex_Vectors.Cursor;


   function Hash (Self : Vertex) return ada.containers.Hash_Type;
   function Equivalent (L, R : in Vertex) return Boolean;

   use type math.Integer;

   package vertex_natural_Maps is new ada.containers.hashed_Maps (Vertex, math.Natural, Hash, Equivalent);

   subtype vertex_natural_Map        is vertex_natural_Maps.Map;
   subtype vertex_natural_map_Cursor is vertex_natural_Maps.Cursor;


   function "-" (Left : in Vertex;   Right : in math.Vector_3) return Vertex;








   type Triangle  is array (Positive range 1 .. 3) of math.geom.d2.Vertex;
   type Triangles is array (Positive range   <>  ) of Triangle;




   -- index Triangles (ie  three indexs into an associated vertex array).

   type index_Triangle  is array (Positive range 1 .. 3) of Natural;
   type index_Triangles is array (Positive range   <>  ) of index_Triangle;

   function Image (Self : in index_Triangle) return String;
   function Image (Self : in index_Triangles) return String;





   function Hash       (Self        : in index_Triangle) return ada.containers.Hash_type;
   function Equivalent (Left, Right : in index_Triangle)   return Boolean;






   package index_triangle_Vectors is new ada.containers.Vectors (Positive, index_Triangle);

   subtype index_triangle_Vector is index_triangle_vectors.Vector;
   subtype index_triangle_Cursor is index_triangle_vectors.Cursor;



   package index_triangle_Sets is new ada.containers.hashed_Sets (index_Triangle, Hash, Equivalent);

   subtype index_triangle_Set        is index_triangle_Sets.Set;
   subtype index_triangle_set_Cursor is index_triangle_Sets.Cursor;





   -- model
   --


   type Model (vertex_Count   : Positive;
               triangle_Count : Positive) is
      record
         Vertices  : d2.Vertices     (1 .. vertex_Count);
         Triangles : index_Triangles (1 .. triangle_Count);
      end record;




   type Model_view is access all Model;


   procedure free is new ada.unchecked_deallocation (Model, Model_view);

--   function Image (Self : in Model) return String;













--     -- sculptor
--     --
--
--     type Sculpture is tagged private;
--
--
--
--
--     procedure add_Triangle (Self : in out Sculpture;   Vertex_1, Vertex_2, Vertex_3 : in Vertex);
--     --
--     -- adds a triangle based on vertices.
--
--
--     function detail_constrained_Model (Self : in     Sculpture;   detail_Radius : math.Number := math.Number'small) return Model;
--     --
--     -- returns a mesh model from the Factory, based on any triangles already added, with vertices constrained to the given detail_Radius.











   -- traits

   function Area                   (Self : in Item) return Number    is abstract;
--   function bounding_sphere_Radius (Self : access Item) return Number    is abstract;






   --subtype model_Quality is Number range 0.0 .. 1.0;

   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return Model_view is abstract;


   type model_Options is tagged
      record
         dummy : boolean := false; -- only here to allow 'null_Options' below ... (tbd)
      end record;


   null_Options : model_Options'Class := model_Options'(others => <>);    -- tbd: how to make this constant ?

   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view is abstract;









private


   type Item is abstract new math.Geom.item with
      record
         null;
      end record;




--     -- sculpture
--     --
--
--     type private_Sculpture;
--     type private_Sculpture_view is access all private_Sculpture;
--
--     type Sculpture is new ada.finalization.Controlled with
--        record
--           Privvy : private_Sculpture_view;
--        end record;
--
--
--
--     procedure initialize (Self : in out Sculpture);
--     procedure finalize   (Self : in out Sculpture);



end math.Geom.d2;

