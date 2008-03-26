
with gts.GtsSurface;
with gts.binding;

with gl.Geometry;

with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.ordered_Maps;
with ada.containers.hashed_Sets;

with ada.unchecked_Deallocation;
with ada.Finalization;




package math.geom.d3.Mesh.gts is
   --
   -- models a triangle mesh.


   type Item is new math.Geom.d3.Mesh.item with private;


   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;



   procedure destroy (Self : in out Item);
   procedure free    (Self : in out View);


   procedure generate_Sphere (Self : access Item;   geodesation_order : Positive);



   procedure add_Triangle (Self : access Item;   Vertex_1, Vertex_2, Vertex_3 : in math.geom.d3.Vertex);


   function  Model     (Self : access Item)                             return d3.Model_view;
   procedure add_Model (Self : access Item;   Now  : in     d3.Model_view);
   procedure clear    (Self : access Item);

   --function to_Model (Self : access Item;   of_Quality : model_Quality := model_Quality'Last) return d3.Model_view;
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view;


   function bounding_sphere_Radius (Self : access Item) return Number;

   function Volume_of (Self : access Item) return Number;


   procedure expand (Self : access Item;
                     By   : in     Number);



--     function to_gl_Strips (Self : access Item) return gl.geometry.triangle_Strips;
--  --   function to_lit_triangle_Strips (Self : access Item) return gl.geometry.triangle_Strips;


   -- operations
   --

   procedure coarsen (Self : access Item);



   -- heightmap mesh
   --

--   function to_Mesh (from_Heightmap : in math.Matrix.item'class) return Mesh.item'Class;






   -- polar model
   --

--   function to_Mesh        (From           : in polar_Model) return Mesh.item'class;







private

   use standard.gts.binding;


   type Item is new math.Geom.d3.Mesh.item with
      record
--         Data : Data_view;                  -- using view due to large size (ie avoid stack_overflow)
--         Model : math.geom.d3.mesh.Model_full_detail.item;
         --null;
         Surface : access standard.gts.GtsSurface.item := standard.gts.binding.gts_surface_new (gts_surface_class,
                                                                                                 gts_face_class,
                                                                                                 gts_edge_class,
                                                                                                 gts_vertex_class);

      end record;






end math.geom.d3.Mesh.gts;

