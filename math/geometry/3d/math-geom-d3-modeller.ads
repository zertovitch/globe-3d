
with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.hashed_Sets;
with ada.containers.ordered_Maps;

with ada.unchecked_Deallocation;
with ada.finalization;




generic
   detail_Radius : math.Number := math.Number'small;


package math.Geom.d3.Modeller is

   type Item is tagged private;
   type View is access all Item;

   procedure clear        (Self : in out Modeller.item);

   procedure add          (Self     : in out Modeller.item;   the_Triangle                 : in Triangle);
   procedure add          (Self     : in out Modeller.item;   the_Triangles                : in Triangles);
   procedure add_Triangle (Self     : in out Modeller.item;   Vertex_1, Vertex_2, Vertex_3 : in Vertex);
   procedure add_Model    (Self     : in out Modeller.item;   the_Model                    : in Model_view);

   function triangle_Count (Self : in Modeller.item) return Natural;
   function Triangles      (Self : in Modeller.item) return Triangles;
   function Model          (Self : in Modeller.item) return Model_view;

   function bounding_sphere_Radius (Self : access Modeller.item) return Number;





private


   type my_Vertex is new Vertex;


   function my_Less_Than (L, R : in my_Vertex) return Boolean;
   --      function "="          (L, R : in my_Vertex) return Boolean;

   package vertex_index_Maps is new ada.containers.ordered_Maps (my_Vertex,
                                                                 Natural,
                                                                 my_Less_Than);

   subtype vertex_index_Map        is vertex_index_Maps.Map;
   subtype vertex_index_map_Cursor is vertex_index_Maps.Cursor;



   type Item is tagged
      record
         --Triangles        : index_triangle_Vector;
         Triangles        : index_triangle_Set;

         Vertices         : vertex_Vector;
         vertex_index_Map : modeller.vertex_index_Map;

         bounding_sphere_Radius : Number := Number'First;
      end record;



end math.Geom.d3.Modeller;

