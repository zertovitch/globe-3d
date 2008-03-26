with lace.debug; use lace.debug;

with globe_3d;

with gts.Binding;      use gts.Binding;
with gts.GtsVertex;    use gts.GtsVertex;
with gts.GtsEdge;      use gts.GtsEdge;
with gts.GtsFace;      use gts.GtsFace;
with gts.GtsVolumeOptimizedParams;
with gts.a_GtsTriangle;
with gts.GtsTriangle;
with gts.GtsSurfaceTraverse;

with gts.GtsVertex;
with gts.a_GtsVertex;
with gts.p_p_gtsVertex;

with gts.gtskeyfunc;
with gts.GtsVolumeOptimizedParams;
with gts.GtsVertexClass;

with glibb.gPointer;
with glibb.gInt;
with glibb.gUInt;
with glibb.gdouble;
with glibb.gboolean;
with glibb.GSList;
with glibb.binding;

with math.geom.d3.Modeller_full_detail;
with math.Angle;                          use math.Angle;

with lace.Text;                           use lace.Text;

with ada.text_IO;                         use ada.text_IO;
with ada.unchecked_Deallocation;
with ada.unchecked_Conversion;
with ada.Strings.unbounded;               use ada.Strings.unbounded;
with ada.containers.hashed_Maps;

with ada.unchecked_Conversion;

with interfaces.C;
with System;
with System.Address_To_Access_Conversions;




package body math.geom.d3.Mesh.gts is


   package C renames interfaces.C;
   use interfaces.C;

   use Glibb;

   use math.Angle.functions;
   use type Number;

   use standard.Gts;



   -- conversions
   --
   function to_gPointer is new ada.unchecked_Conversion (system.Address, glibb.gPointer.item);


   package gtsTriangle_conversions is new System.Address_To_Access_Conversions (gtsTriangle.item);
   use gtsTriangle_conversions;

   package glibb_GSList_conversions is new System.Address_To_Access_Conversions (glibb.GSList.item);
   use glibb_GSList_conversions;








   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin
      raise constraint_error;
      return 0.0;
   end;




   procedure add_Triangle (Self                         : access Item;
                           Vertex_1, Vertex_2, Vertex_3 : in     math.geom.d3.Vertex)
   is
      use glibb;
      V1 : access standard.gts.GtsVertex.item := gts_vertex_new (gts_vertex_class, gDouble.item (Vertex_1 (1)),
                                                                                   gDouble.item (Vertex_1 (2)),
                                                                                   gDouble.item (Vertex_1 (3)));

      V2 : access standard.gts.GtsVertex.item := gts_vertex_new (gts_vertex_class, gDouble.item (Vertex_2 (1)),
                                                                                   gDouble.item (Vertex_2 (2)),
                                                                                   gDouble.item (Vertex_2 (3)));

      V3 : access standard.gts.GtsVertex.item := gts_vertex_new (gts_vertex_class, gDouble.item (Vertex_3 (1)),
                                                                                   gDouble.item (Vertex_3 (2)),
                                                                                   gDouble.item (Vertex_3 (3)));

      E1 : access standard.gts.GtsEdge.item := gts_edge_new (gts_edge_class,  V1, V2);
      E2 : access standard.gts.GtsEdge.item := gts_edge_new (gts_edge_class,  V3, V1);
      E3 : access standard.gts.GtsEdge.item := gts_edge_new (gts_edge_class,  V3, V2);

      Face : access standard.gts.GtsFace.item := gts_Face_new (gts_Face_class,  E1, E2, E3);


   begin
      put_Line ("AHA ... this is slow !!");
      gts_surface_add_face (self.Surface, Face);
   end;









   function Hash (Self : in index_Triangle) return ada.containers.Hash_type
   is
   begin
      return ada.containers.Hash_type (self (1)  +  self (2) * 2  +  self (3) * 3);
   end;





   function Equivalent (Left, Right : in index_Triangle)   return Boolean
   is
   begin
      return    Left = Right

--          or else (         Left (1) = Right (2)
--                   and then Left (2) = Right (3)
--                   and then Left (3) = Right (1))
--
--          or else (         Left (1) = Right (3)
--                   and then Left (2) = Right (1)
--                   and then Left (3) = Right (2));

        or else (         Right (1) = Left (2)
                 and then Right (2) = Left (3)
                 and then Right (3) = Left (1))

        or else (         Right (1) = Left (3)
                 and then Right (2) = Left (1)
                 and then Right (3) = Left (2));
   end;








   procedure destroy (Self : in out Item)
   is
   begin
      self.Clear;
   end;





   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'class, View);
   begin
      if Self /= null then
         destroy (Self.all);
      end if;

      deallocate (Self);
   end;










   function Volume_of (Self : access Item) return Number
   is
   begin
      raise Program_error; -- tbd
      return 0.0;
   end;




   procedure expand (Self : access Item;
                     By   : in     Number)
   is
   begin
      raise Program_error;  -- tbd
   end;






   procedure add_Model (Self : access Item;
                        Now  : in     d3.Model_view)
   is
      use glibb;

      the_Vertices : array (1 .. now.vertex_Count) of standard.gts.GtsVertex.view;



      type edge_Vertices is
         record
            V1 : Integer;
            V2 : Integer;
         end record;

      function hashed (Self : in edge_Vertices) return ada.containers.Hash_type
      is
      begin
--         return ada.containers.Hash_type (5 * self.V1 + self.V2);
         return ada.containers.Hash_type (self.V1);
      end;


      package edge_Vertices_gtsEdge_Maps is new ada.containers.hashed_Maps (edge_Vertices, standard.gts.GtsEdge.view, hash => hashed, equivalent_keys => "=");
      use edge_Vertices_gtsEdge_Maps;

      the_edge_Map : edge_Vertices_gtsEdge_Maps.Map;


      function demand_Edge (V1, V2 : Integer) return standard.gts.GtsEdge.view
      is
         Cursor : edge_Vertices_gtsEdge_Maps.Cursor := Find (the_edge_Map, (V1, V2));
         the_Edge : standard.gts.GtsEdge.view; -- := Element (the_edge_Map, (V1, V2));
      begin
         if has_Element (Cursor) then
            return Element (Cursor);
         end if;

         Cursor := find (the_edge_Map, (V2, V1));

         if has_Element (Cursor) then
            return Element (Cursor);
         end if;

         the_Edge := gts_edge_new (gts_edge_class,  the_Vertices (V1),  the_Vertices (V2)).all'access;
         insert (the_edge_Map,  (v1, V2),  the_Edge);
         --put_line ("... inserting new edge !");

         return the_Edge;
      end demand_Edge;

   begin
      dLog ("");
      dLog ("add_Model");

      for Each in the_Vertices'range loop
--           put_Line ("Vertices (" & integer'image (each) & "): "
--                     & image (number (now.Vertices (Each)(1)) ) & "  "
--                     & image (number (now.Vertices (Each)(3)) ));

         the_Vertices (Each) := gts_vertex_new (gts_vertex_class,  gDouble.item (now.Vertices (Each) (1)),
                                                                   gDouble.item (now.Vertices (Each) (2)),
                                                                   gDouble.item (now.Vertices (Each) (3))).all'access;
      end loop;


      for Each in now.Triangles'range loop
         declare
            the_Triangle : index_Triangle renames now.Triangles (Each);

--              E1 : standard.gts.GtsEdge.view := demand_Edge (the_Triangle (1), the_Triangle (2));
--              E2 : standard.gts.GtsEdge.view := demand_Edge (the_Triangle (2), the_Triangle (3));
--              E3 : standard.gts.GtsEdge.view := demand_Edge (the_Triangle (3), the_Triangle (1));

            E1 : standard.gts.GtsEdge.view := demand_Edge (math.Integer (the_Triangle (1)),
                                                           math.Integer (the_Triangle (2)));

            E2 : standard.gts.GtsEdge.view := demand_Edge (math.Integer (the_Triangle (2)),
                                                           math.Integer (the_Triangle (3)));

            E3 : standard.gts.GtsEdge.view := demand_Edge (math.Integer (the_Triangle (3)), math.Integer (the_Triangle (1)));

--              E2 : standard.gts.GtsEdge.view := demand_Edge (the_Triangle (3), the_Triangle (1));
--              E3 : standard.gts.GtsEdge.view := demand_Edge (the_Triangle (3), the_Triangle (2));

--              E1 : standard.gts.GtsEdge.view := gts_edge_new (gts_edge_class,  the_Vertices (the_Triangle (1)),  the_Vertices (the_Triangle (2)));
--              E2 : standard.gts.GtsEdge.view := gts_edge_new (gts_edge_class,  the_Vertices (the_Triangle (3)),  the_Vertices (the_Triangle (1)));
--              E3 : standard.gts.GtsEdge.view := gts_edge_new (gts_edge_class,  the_Vertices (the_Triangle (3)),  the_Vertices (the_Triangle (2)));

            Face : standard.gts.GtsFace.view := gts_Face_new (gts_Face_class,  E1, E2, E3).all'access;
         begin
 --           dLog ("tri orientation: " & image (number (gts_triangle_orientation (Face.Triangle'access))));

--            gts_triangle_set (Face.Triangle'access, E1, E2, E3);

            gts_surface_add_face (self.Surface, Face);

--           self.add_Triangle (now.Vertices (now.Triangles (Each) (1)),
--                              now.Vertices (now.Triangles (Each) (2)),
--                              now.Vertices (now.Triangles (Each) (3)));
         end;

      end loop;
   end;







   function add_all (the_Item : glibb.gPointer.item;  the_Data : glibb.gPointer.item) return glibb.gInt.item;
   pragma convention (C, add_all);


   function add_all (the_Item : glibb.gPointer.item;  the_Data : glibb.gPointer.item) return glibb.gInt.item
   is
      use Glibb;
      use standard.gts.binding;

      function to_View is new ada.unchecked_Conversion (gPointer.item, d3.Modeller_full_detail.view);
      function to_View is new ada.unchecked_Conversion (gPointer.item, standard.gts.GtsFace.view);

      the_Face  : standard.gts.GtsFace.view    := to_View (the_Item);
      the_Model : d3.Modeller_full_detail.view := to_View (the_Data);

      V1 : access standard.gts.GtsVertex.item := gts_triangle_vertex_n (the_face.Triangle'unchecked_access, 1);
      V2 : access standard.gts.GtsVertex.item := gts_triangle_vertex_n (the_face.Triangle'unchecked_access, 2);
      V3 : access standard.gts.GtsVertex.item := gts_triangle_vertex_n (the_face.Triangle'unchecked_access, 3);


   begin
      the_Model.add_Triangle ( (math.Number (v1.p.x), math.Number (v1.p.y), math.Number (v1.p.z)),
                               (math.Number (v2.p.x), math.Number (v2.p.y), math.Number (v2.p.z)),
                               (math.Number (v3.p.x), math.Number (v3.p.y), math.Number (v3.p.z)) );
      return 0;
   end;




   type coarsen_cost_Params is
      record
         Surface                 :         standard.gts.GtsSurface.view;
         Volume_Optimised_Params : aliased standard.gts.GtsVolumeOptimizedParams.item;
      end record;


   type coarsen_cost_params_view is access all coarsen_cost_Params;






   function  my_gts_volume_optimized_cost (e : glibb.gpointer.Item;  params : glibb.gpointer.Item) return glibb.gdouble.Item;
   pragma convention (C, my_gts_volume_optimized_cost);


   function  my_gts_volume_optimized_cost (e : glibb.gpointer.Item;  params : glibb.gpointer.Item) return glibb.gdouble.Item
   is
      function to_Edge   is new ada.unchecked_conversion (glibb.gpointer.Item, standard.gts.GtsEdge.view);
      --function to_Params is new ada.unchecked_conversion (glibb.gpointer.Item, standard.gts.GtsVolumeOptimizedParams.view);
      function to_Params is new ada.unchecked_conversion (glibb.gpointer.Item, coarsen_cost_params_view);
   begin
      if gts_edge_is_Boundary (to_Edge (e), to_Params (params).Surface) /= null then
         --dlog ("Here gghjkkl");
         return glibb.gdouble.Item'Last; --ts_volume_optimized_cost (to_Edge (e), to_Params (params).Volume_Optimised_Params'access) + 5_000_000.0;
      else
         return gts_volume_optimized_cost (to_Edge (e), to_Params (params).Volume_Optimised_Params'access);
      end if;
   end;






   function  my_gts_volume_optimized_vertex (e :  access standard.gts.GtsEdge.item;
                                             arg_3_2 :  access standard.gts.GtsVertexClass.item;
                                             arg_3_3 :  glibb.gpointer.Item) return access standard.gts.GtsVertex.item;
   pragma convention (C, my_gts_volume_optimized_vertex);


   function  my_gts_volume_optimized_vertex  (e :  access standard.gts.GtsEdge.item;
                                              arg_3_2 :  access standard.gts.GtsVertexClass.item;
                                              arg_3_3 :  glibb.gpointer.Item) return access standard.gts.GtsVertex.item
   is
      function to_Edge   is new ada.unchecked_conversion (glibb.gpointer.Item, standard.gts.GtsEdge.view);
      function to_Params is new ada.unchecked_conversion (glibb.gpointer.Item, standard.gts.GtsVolumeOptimizedParams.view);
   begin
      return gts_volume_optimized_vertex (e, arg_3_2, to_Params (arg_3_3));
   end;




--     function   (arg_3_1 :  gts.GtsEdge.View;
--  arg_3_2 :  gts.GtsVertexClass.View;
--  arg_3_3 :  glibb.gpointer.Item) return gts.GtsVertex.View;






   function  my_gts_coarsen_stop_number (arg_3_1 : glibb.gdouble.Item;
                                         vertex_Count : glibb.guint.Item;
                                         arg_3_3 : glibb.gpointer.Item)     return       glibb.gboolean.Item;
   pragma convention (C, my_gts_coarsen_stop_number);


   function  my_gts_coarsen_stop_number (arg_3_1 : glibb.gdouble.Item;
                                         vertex_Count : glibb.guint.Item;
                                         arg_3_3 : glibb.gpointer.Item)     return       glibb.gboolean.Item
   is
      use type glibb.gUint.item;
      function to_Data is new ada.unchecked_conversion (glibb.gpointer.Item, glibb.gUInt.view);
   begin
      if vertex_Count mod 10000 = 0 then
         put_Line ("my_gts_coarsen_stop_number: " & glibb.gdouble.Item'image (arg_3_1) & "   edge count: "
                   & glibb.guint.Item'image (vertex_Count) & "  stop when edge count <= " & glibb.gUInt.item'image (to_data (arg_3_3).all));
      end if;

      return gts_coarsen_stop_number (arg_3_1, vertex_Count, to_Data (arg_3_3));
   end;



--   function
--       (arg_3_1 : glibb.gdouble.Item;
--        arg_3_2 : glibb.guint.Item;
--        arg_3_3 : glibb.gpointer.Item)
--     return       glibb.gboolean.Item;







   procedure coarsen (Self : access Item)
   is
      use type glibb.gDouble.item;
      function to_Address is new ada.unchecked_Conversion (d3.Modeller_full_detail.view, system.Address);
      function to_Address is new ada.unchecked_Conversion (glibb.gUint.view, system.Address);

--      params    : standard.gts.GtsVolumeOptimizedParams.item := (0.5, 0.5, 0.0);
      params    : aliased coarsen_cost_params := (surface                 => self.Surface.all'access,
                                                  Volume_Optimised_Params => (volume_weight   => 0.5,
                                                                              boundary_weight => 0.5,
                                                                              shape_weight    => 0.0));

      the_Model : aliased d3.Modeller_full_detail.item;
      stop_Data : aliased glibb.gUint.item := glibb.gUint.item (math.Number (gts_surface_edge_number (self.Surface)) * (0.55));
      fold      : glibb.gDouble.item := 3.1415 / 180.0;

   begin
      log_stats (self.Surface);

      gts_surface_coarsen (self.Surface,
                           my_gts_volume_optimized_cost'access,   to_gPointer (params'address),  --cost_data,
                           my_gts_volume_optimized_vertex'access, to_gPointer (params'address),  --coarsen_func, coarsen_data,
                           my_gts_coarsen_stop_number'access,     to_gPointer (stop_data'address),
                           --my_gts_coarsen_stop_number'access,     glibb.gPointer.item (to_Address (stop_data'unchecked_access)),
                           fold);

      log_stats (self.Surface);

   end;









   function  Model (Self   : access Item) return d3.Model_view
   is
      use type glibb.gDouble.item;
      function to_Address is new ada.unchecked_Conversion (d3.Modeller_full_detail.view, system.Address);
      function to_Address is new ada.unchecked_Conversion (glibb.gUint.view, system.Address);

      params    : standard.gts.GtsVolumeOptimizedParams.item := (0.5, 0.5, 0.0);
      the_Model : aliased d3.Modeller_full_detail.item;
      stop_Data : aliased glibb.gUint.item := 500;
      fold      : glibb.gDouble.item := 3.1415 / 180.0;

   begin
      log_stats (self.Surface);
--        my_coarsen (self.Surface);
--        log_stats (self.Surface);

--        gts_surface_coarsen (self.Surface,
--                             null,   system.null_address,  --cost_data,
--                             null, system.null_address,  --coarsen_func, coarsen_data,
--                             my_gts_coarsen_stop_number'access,     to_Address (stop_data'unchecked_access),
--                             fold);

--        gts_surface_coarsen (self.Surface,
--                             my_gts_volume_optimized_cost'access,   params'address,  --cost_data,
--                             my_gts_volume_optimized_vertex'access, params'address,  --coarsen_func, coarsen_data,
--                             my_gts_coarsen_stop_number'access,     to_Address (stop_data'unchecked_access),
--                             fold);
--
--        log_stats (self.Surface);

      gts_surface_foreach_face (self.Surface, add_all'access, to_gPointer (the_Model'address));
      put_Line ("num tri's: " & standard.integer'image (the_model.triangle_Count));
      return the_Model.Model;
   end;





--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return d3.Model_view
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view
   is
   begin
      return self.Model;
   end;







   function rid_all (the_Item : glibb.gPointer.item;  the_Data : glibb.gPointer.item) return glibb.gInt.item;
   pragma convention (C, rid_all);

   function rid_all (the_Item : glibb.gPointer.item;  the_Data : glibb.gPointer.item) return glibb.gInt.item
   is
   begin
      return 1;
   end;




   procedure clear   (Self : access Item)
   is
      face_removed_Count : glibb.gUInt.item;
   begin
      face_removed_Count := gts_surface_foreach_face_remove (self.Surface, rid_all'access, null);
   end;






   procedure generate_Sphere (Self : access Item;   geodesation_order : Positive)
   is
      Result : access standard.gts.GtsSurface.item;
   begin
      Result := gts_surface_generate_sphere (self.Surface, guInt.item (geodesation_order));
   end;







   -- vertex pools
   --


   function Hash is new ada.unchecked_Conversion (standard.gts.GtsVertex.view, ada.containers.Hash_type);
   use type gl.geometry.vertex_Id;

   package gts_vertex_id_maps is new ada.containers.hashed_Maps (standard.gts.GtsVertex.view,
                                                                 gl.geometry.vertex_Id,
                                                                 hash            => Hash,
                                                                 equivalent_keys => "=");
   use gts_vertex_id_maps;


--   type vertex_pool_Map (last_Id : gl.geometry.vertex_Id) is
   type vertex_pool_Map (vertex_Count : gl.geometry.vertex_Id) is
      record
--         Pool           : access gl.geometry.lit_vertex_Pool := new gl.geometry.lit_vertex_Pool (last_id);
--         Pool           : gl.geometry.p_lit_vertex_Pool := new gl.geometry.lit_vertex_Pool (last_id);
         Pool           : gl.geometry.p_Vertex_array := new gl.geometry.Vertex_array (1 .. vertex_Count);
         Map            : gts_vertex_id_maps.Map;

         current_Vertex : gl.geometry.Vertex_Id := 1; --0;
      end record;


   package vertex_pool_Map_conversions is new System.Address_To_Access_Conversions (vertex_pool_Map);
   use vertex_pool_Map_conversions;


   package GtsVertex_conversions is new System.Address_To_Access_Conversions (standard.gts.GtsVertex.item);
   use GtsVertex_conversions;

--   package GtsTriangle_conversions is new System.Address_To_Access_Conversions (standard.gts.GtsTriangle.item);
   use GtsTriangle_conversions;




   -- gts Vertices



   function Normal_for (the_Vertex : in standard.gts.GtsVertex.view) return gl.geometry.Normal
   is
      use Gl, gl.Geometry, glibb.Binding, glibb.GSList;
      function to_Pointer is new ada.unchecked_Conversion (glibb.gPointer.item, standard.gts.GtsTriangle.view);

      common_Triangles : access glibb.GSList.item := gts_vertex_triangles (the_Vertex, null);
      triangle_Count   : gl.uInt                  := gl.uInt (g_slist_length (common_Triangles));

      the_Normal       : gl.geometry.Normal := (0.0, 0.0, 0.0);

      the_Triangle     : access  standard.gts.GtsTriangle.item;
      x, y, z          : aliased glibb.gDouble.item;
   begin

      for Each in 1 .. triangle_Count loop
         the_Triangle := to_Pointer (common_Triangles.data).all'access;

         gts_triangle_normal (the_Triangle,  x'unchecked_access,  y'unchecked_access,  z'unchecked_access);

         the_Normal (0) := the_Normal (0) + gl.Double (x);
         the_Normal (1) := the_Normal (1) + gl.Double (y);
         the_Normal (2) := the_Normal (2) + gl.Double (z);

         common_Triangles := common_Triangles.next;
      end loop;


      -- normalise
      --
      declare
         use globe_3d.ref;
         Length : gl.Double := SqRt (the_Normal (0) * the_Normal (0)  +  the_Normal (1) * the_Normal (1)  +  the_Normal (2) * the_Normal (2));
      begin
         the_Normal (0) := the_Normal (0) / Length;
         the_Normal (1) := the_Normal (1) / Length;
         the_Normal (2) := the_Normal (2) / Length;
      end;


      return the_Normal;
   end;








   -- callback for 'vertex_pool_Map_for' below ...
   --
   function add_Vertex (Vertex : in glibb.gPointer.item;   Data : in glibb.gPointer.item) return glibb.gInt.item;
   pragma convention (C, add_Vertex);


   function add_Vertex (Vertex : in glibb.gPointer.item;   Data : in glibb.gPointer.item) return glibb.gInt.item
   is
      use gl.Geometry;
      function to_Pointer is new ada.unchecked_Conversion (glibb.gPointer.item, standard.gts.GtsVertex.view);

      type vertex_pool_Map_view is access all vertex_pool_Map;
      function to_Pointer is new ada.unchecked_Conversion (glibb.gPointer.item, vertex_pool_map_view);

      the_Vertex : standard.gts.GtsVertex.view := to_Pointer (Vertex).all'access;
      the_Map    : vertex_pool_Map        renames to_Pointer (Data).all;

   begin
      the_Map.Pool (the_Map.current_Vertex)(0) := gl.Double (the_Vertex.P.x);
      the_Map.Pool (the_Map.current_Vertex)(1) := gl.Double (the_Vertex.P.y);
      the_Map.Pool (the_Map.current_Vertex)(2) := gl.Double (the_Vertex.P.z);

--        if the_Map.Pool.all in lit_vertex_Pool'Class then -- do normals
--           the_Map.Pool.Normals (the_Map.current_Vertex) := Normal_for (the_Vertex);
--        end if;

      insert (the_map.Map,  the_Vertex, the_Map.current_Vertex);

      the_Map.current_Vertex := the_Map.current_Vertex + 1;

      return 0; -- '0' means continue to next vertex (non '0' means halt)
   end;





   function vertex_pool_Map_for (Self : access Item) return vertex_pool_Map
   is
      use Glibb, gl.Geometry;
      the_Map : aliased vertex_pool_Map (vertex_count => vertex_Id (gts_surface_vertex_number (self.Surface)));
   begin
      gts_surface_foreach_vertex (self.Surface,  add_Vertex'access,  to_gPointer (the_Map'address));
      return the_Map;
   end;





   -- triangle strips
   --


--     function to_gl_Strips (Self : access Item) return gl.geometry.triangle_Strips
--     is
--        use glibb.GSList;
--        strips_List : glibb.GSList.view := gts_surface_Strip (self.Surface);
--
--        the_Strips    : gl.geometry.triangle_Strips (1 .. 5_000);
--        strip_Count   : Natural                                 := 0;
--
--        current_Strip : gl.geometry.p_triangle_Strip;
--
--        the_vertex_Map : vertex_pool_Map := vertex_pool_Map_for (Self);
--     begin
--
--        loop
--           strip_Count := strip_Count + 1;
--           dLog ("strip count:" & integer'image (strip_Count));
--
--           declare
--              use gl, gl.Geometry, glibb.binding;
--              triangles_List : glibb.GSList.view := to_Pointer (strips_List.data).all'access;
--
--              the_Triangle   : gtsTriangle.view  := to_Pointer (triangles_List.data).all'access;
--              prior_Triangle : gtsTriangle.view;
--
--
--              procedure dlog_tri
--              is
--                 v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--                 v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--                 v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--              begin
--                 dLog ("");
--                 dLog ("v1: " &   image (number (v1.p.x)) & " " &  image (number (v1.p.y)) & " " &  image (number (v1.p.z)));
--                 dLog ("v2: " &   image (number (v2.p.x)) & " " &  image (number (v2.p.y)) & " " &  image (number (v2.p.z)));
--                 dLog ("v3: " &   image (number (v3.p.x)) & " " &  image (number (v3.p.y)) & " " &  image (number (v3.p.z)));
--
--              end;
--
--
--  --              v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--  --              v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--  --              v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--              v1 : standard.gts.GtsVertex.view := the_Triangle.e1.segment.v1;
--              v2 : standard.gts.GtsVertex.view := the_Triangle.e1.segment.v2;
--              v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_other (the_Triangle);
--           begin
--  --            dLog ("to_tri_strips: tri orientation: " & image (number (gts_triangle_orientation (the_Triangle))));
--
--              current_Strip            := new_triangle_Strip (triangle_count => Positive (g_slist_length (triangles_List)),
--                                                              vertex_source  => the_vertex_Map.Pool.all'access);
--  --            the_Strips (strip_Count) := current_Strip;
--              the_Strips (strip_Count) := current_Strip.all'access;
--
--              --dlog_tri;
--
--  --              current_Strip.vertex_Pool.Vertices (0) := (gl.Double (v1.p.x),  gl.Double (v1.p.y),  gl.Double (v1.p.z));
--  --              current_Strip.vertex_Pool.Vertices (1) := (gl.Double (v2.p.x),  gl.Double (v2.p.y),  gl.Double (v2.p.z));
--  --              current_Strip.vertex_Pool.Vertices (2) := (gl.Double (v3.p.x),  gl.Double (v3.p.y),  gl.Double (v3.p.z));
--  --
--  --              current_Strip.Indices (1) := 2;
--  --              current_Strip.Indices (2) := 0;
--  --              current_Strip.Indices (3) := 1;
--
--  --              current_Strip.Indices (1) := Element (the_vertex_Map.Map, v2);  -- good terrain !
--  --              current_Strip.Indices (2) := Element (the_vertex_Map.Map, v1);
--  --              current_Strip.Indices (3) := Element (the_vertex_Map.Map, v3);
--
--  --            if gts_triangle_orientation (the_Triangle) <= 0.0 then
--
--  --                 current_Strip.Indices (1) := Element (the_vertex_Map.Map, v1);
--  --                 current_Strip.Indices (2) := Element (the_vertex_Map.Map, v2);
--  --                 current_Strip.Indices (3) := Element (the_vertex_Map.Map, v3);
--
--                 current_Strip.Indices (1) := Element (the_vertex_Map.Map, v1) - 1;
--                 current_Strip.Indices (2) := Element (the_vertex_Map.Map, v2) - 1;
--                 current_Strip.Indices (3) := Element (the_vertex_Map.Map, v3) - 1;
--
--  --              else
--  --                 current_Strip.Indices (1) := Element (the_vertex_Map.Map, v2);
--  --                 current_Strip.Indices (2) := Element (the_vertex_Map.Map, v3);
--  --                 current_Strip.Indices (3) := Element (the_vertex_Map.Map, v1);
--  --              end if;
--
--
--              declare
--  --               current_Vertex : gl.uInt := 2;
--                 current_Index  : Natural := 3;
--
--                 common_Edge     : standard.gts.GtsEdge.view;
--                 opposite_Vertex : standard.gts.GtsVertex.view;
--              begin
--
--                 while triangles_List.next /= null loop
--
--                    prior_Triangle := the_Triangle;
--
--                    triangles_List := triangles_List.next;
--                    the_Triangle   := to_Pointer (triangles_List.data).all'access;
--
--                    common_Edge     := gts_triangles_common_edge    (the_Triangle, prior_Triangle);
--                    opposite_Vertex := gts_triangle_vertex_opposite (the_Triangle, common_Edge);
--
--                    --dlog_tri;
--
--  --                    if current_Vertex mod 2 = 0 then
--  --                       v3 := gts_triangle_vertex_n (the_Triangle, 3);
--  --                    else
--  --                       v3 := gts_triangle_vertex_n (the_Triangle, 2);
--  --                    end if;
--  --              v1 := gts_triangle_vertex_n (the_Triangle, 1);
--  --              v2 := gts_triangle_vertex_n (the_Triangle, 2);
--  --              v3 := gts_triangle_vertex_n (the_Triangle, 3);
--
--
--  --                    current_Vertex                                      := current_Vertex + 1;
--  --                    current_Strip.vertex_Pool.Vertices (current_Vertex) := (gl.Double (opposite_Vertex.p.x),
--  --                                                                            gl.Double (opposite_Vertex.p.y),
--  --                                                                            gl.Double (opposite_Vertex.p.z));
--
--                    current_Index                         := current_Index + 1;
--                    current_Strip.Indices (current_Index) := Element (the_vertex_Map.Map, opposite_Vertex) - 1;
--                 end loop;
--              end;
--
--           end;
--
--
--           exit when strips_List.next = null;
--
--           strips_List := strips_List.next;
--        end loop;
--
--
--        return the_Strips (1 .. strip_Count);
--     end;







--     function to_triangle_Strips (Self : access Item) return gl.geometry.triangle_Strips
--     is
--        use glibb.GSList;
--        strips_List : glibb.GSList.view := gts_surface_Strip (self.Surface);
--
--        the_Strips    : gl.geometry.triangle_Strips (1 .. 5_000);
--        strip_Count   : Natural                                 := 0;
--
--        current_Strip : access gl.geometry.triangle_Strip;
--     begin
--
--        loop
--           strip_Count := strip_Count + 1;
--           dLog ("strip count:" & integer'image (strip_Count));
--
--           declare
--              use gl, gl.Geometry;
--              triangles_List : glibb.GSList.view := to_Pointer (strips_List.data).all'access;
--
--              the_Triangle   : gtsTriangle.view  := to_Pointer (triangles_List.data).all'access;
--              prior_Triangle : gtsTriangle.view;
--
--
--              procedure dlog_tri
--              is
--                 v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--                 v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--                 v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--              begin
--                 dLog ("");
--                 dLog ("v1: " &   image (number (v1.p.x)) & " " &  image (number (v1.p.y)) & " " &  image (number (v1.p.z)));
--                 dLog ("v2: " &   image (number (v2.p.x)) & " " &  image (number (v2.p.y)) & " " &  image (number (v2.p.z)));
--                 dLog ("v3: " &   image (number (v3.p.x)) & " " &  image (number (v3.p.y)) & " " &  image (number (v3.p.z)));
--
--              end;
--
--
--              v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--              v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--              v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--           begin
--
--              current_Strip            := new_triangle_Strip (Positive (glibb.binding.g_slist_length (triangles_List)));
--              the_Strips (strip_Count) := current_Strip.all'unchecked_access;
--
--              --dlog_tri;
--
--              current_Strip.Vertices (0) := (gl.Double (v1.p.x),  gl.Double (v1.p.y),  gl.Double (v1.p.z));
--              current_Strip.Vertices (1) := (gl.Double (v2.p.x),  gl.Double (v2.p.y),  gl.Double (v2.p.z));
--              current_Strip.Vertices (2) := (gl.Double (v3.p.x),  gl.Double (v3.p.y),  gl.Double (v3.p.z));
--
--              current_Strip.Indices (1) := 2;
--              current_Strip.Indices (2) := 0;
--              current_Strip.Indices (3) := 1;
--
--              declare
--                 current_Vertex : gl.uInt := 2;
--                 current_Index  : Natural := 3;
--
--                 common_Edge     : standard.gts.GtsEdge.view;
--                 opposite_Vertex : standard.gts.GtsVertex.view;
--              begin
--
--                 while triangles_List.next /= null loop
--
--                    prior_Triangle := the_Triangle;
--
--                    triangles_List := triangles_List.next;
--                    the_Triangle   := to_Pointer (triangles_List.data).all'access;
--
--                    common_Edge     := gts_triangles_common_edge    (the_Triangle, prior_Triangle);
--                    opposite_Vertex := gts_triangle_vertex_opposite (the_Triangle, common_Edge);
--
--                    --dlog_tri;
--
--  --                    if current_Vertex mod 2 = 0 then
--  --                       v3 := gts_triangle_vertex_n (the_Triangle, 3);
--  --                    else
--  --                       v3 := gts_triangle_vertex_n (the_Triangle, 2);
--  --                    end if;
--  --              v1 := gts_triangle_vertex_n (the_Triangle, 1);
--  --              v2 := gts_triangle_vertex_n (the_Triangle, 2);
--  --              v3 := gts_triangle_vertex_n (the_Triangle, 3);
--
--
--                    current_Vertex                          := current_Vertex + 1;
--                    current_Strip.Vertices (current_Vertex) := (gl.Double (opposite_Vertex.p.x),
--                                                                gl.Double (opposite_Vertex.p.y),
--                                                                gl.Double (opposite_Vertex.p.z));
--  --                  current_Strip.Vertices (current_Vertex) := (gl.Double (v3.p.x),  gl.Double (v3.p.y),  gl.Double (v3.p.z));
--
--                    current_Index                         := current_Index + 1;
--                    current_Strip.Indices (current_Index) := current_Vertex;
--                 end loop;
--              end;
--
--           end;
--
--
--           exit when strips_List.next = null;
--
--           strips_List := strips_List.next;
--        end loop;
--
--
--        return the_Strips (1 .. strip_Count);
--     end;








--     -- model
--     --
--
--
--     package body Model is
--
--
--        function my_Less_Than (L, R : in my_Vertex) return Boolean
--        is
--        begin
--
--           if    L (1) < R (1) then
--              return True;
--           elsif R (1) < L (1) then
--              return False;
--           end if;
--
--
--           if    L (2) < R (2) then
--              return True;
--           elsif R (2) < L (2) then
--              return False;
--           end if;
--
--
--           if    L (3) < R (3) then
--              return True;
--           elsif R (3) < L (3) then
--              return False;
--           end if;
--
--
--           return False;
--        end;
--
--  --        function my_Less_Than (L, R : in my_Vertex) return Boolean
--  --        is
--  --           X_diff : Number := abs (L (1) - R (1));
--  --           Y_diff : Number;
--  --           Z_diff : Number;
--  --        begin
--  --
--  --           if X_diff > detail_Radius then
--  --              return L (1) < R (1);
--  --           end if;
--  --
--  --
--  --           Y_diff := abs (L (2) - R (2));
--  --
--  --           if Y_diff > detail_Radius then
--  --              return L (2) < R (2);
--  --           end if;
--  --
--  --
--  --           Z_diff := abs (L (3) - R (3));
--  --
--  --           if Z_diff > detail_Radius then
--  --              return L (3) < R (3);
--  --           end if;
--  --
--  --
--  --           return false;   -- must be 'equivalent'
--  --        end;
--
--
--
--
--        function "=" (L, R : in my_Vertex) return Boolean
--        is
--        begin
--           put_Line ("In MY ===========");
--           return     abs (L (1) - R (1))  <  detail_Radius
--             and then abs (L (2) - R (2))  <  detail_Radius
--             and then abs (L (3) - R (3))  <  detail_Radius;
--        end;
--
--
--
--
--
--        function Rounded (Self : in my_Vertex) return my_Vertex
--        is
--           function Round (the_Number : in math.Number) return math.Number
--           is
--              Factor : constant Number := 1.0 / detail_Radius;
--           begin
--              --put_Line ("the_NUM: " & Number'Image (the_Number)    & "     FACT: " & number'image (Factor));
--              return Number (long_integer (Factor * the_Number)) / Factor;
--           end;
--
--        begin
--           return (Round (Self (1)),
--                   Round (Self (2)),
--                   Round (Self (3)));
--        end;
--
--
--
--
--
--        function demand_Index (Self       : access Model.item;
--                               for_Vertex : in     my_Vertex) return Natural
--        is
--           use vertex_index_Maps;
--           use vertex_Vectors;
--
--           rounded_Vertex : my_Vertex               := Rounded (for_Vertex);
--           map_Cursor     : vertex_index_map_Cursor := find (self.vertex_index_Map, rounded_Vertex);
--        begin
--           if has_Element (map_Cursor) then
--              return Element (map_Cursor);
--           end if;
--
--           append (self.Vertices,          Vertex (rounded_Vertex));
--
--  --           put_line ("inserting new vertex: "
--  --                     & "  " & number'image (rounded_Vertex (1))
--  --                     & "  " & number'image (rounded_Vertex (2))
--  --                     & "  " & number'image (rounded_Vertex (3)));
--
--           --insert (self.vertex_index_Map,  for_Vertex,  Natural (Length (self.Vertices)));
--           insert (self.vertex_index_Map,  rounded_Vertex,  Natural (Length (self.Vertices)));
--
--           return Natural (Length (self.Vertices));
--
--        exception
--           when constraint_Error =>  -- tbd: hack !!! ... the 'insert' above causes 'key already found' error sometimes when detail_Radius is high ... !
--              put_Line ("mesh.demand_Index:   wierd constraint_Error !");
--
--              declare
--                 use vertex_Vectors;
--
--                 Cursor : vertex_vector_Cursor := First (self.Vertices);
--                 Index  : Natural              := 0;
--
--                 function is_equivalent (L, R : in my_Vertex) return Boolean
--                 is
--                 begin
--                    return     abs (L (1) - R (1))  <  detail_Radius
--                      and then abs (L (2) - R (2))  <  detail_Radius
--                      and then abs (L (3) - R (3))  <  detail_Radius;
--                 end;
--
--              begin
--                 while has_Element (Cursor) loop
--                    Index := Index + 1;
--
--                    if is_equivalent (my_Vertex (Element (Cursor)),  rounded_Vertex) then
--                       return Index;
--                    end if;
--
--                    next (Cursor);
--                 end loop;
--
--                 raise program_Error;
--              end;
--
--        end;
--
--
--
--
--
--
--
--        procedure add_Triangle (Self     : in out Model.item;
--                                Vertex_1 : in     Vertex;
--                                Vertex_2 : in     Vertex;
--                                Vertex_3 : in     Vertex)
--        is
--           --use index_triangle_Vectors;
--           use index_triangle_Sets;
--
--           vertex_1_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_1));
--           vertex_2_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_2));
--           vertex_3_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_3));
--
--           new_Triangle           : index_Triangle := (vertex_1_Index, vertex_2_Index, vertex_3_Index);
--           new_Triangle_rotated_1 : index_Triangle := (vertex_3_Index, vertex_1_Index, vertex_2_Index);
--           new_Triangle_rotated_2 : index_Triangle := (vertex_2_Index, vertex_3_Index, vertex_1_Index);
--        begin
--  --           put_Line ("model: tri count: " & natural'image (natural (Length (self.Triangles))));
--
--
--
--           if              new_Triangle (1) = new_Triangle (2)
--                   or else new_Triangle (1) = new_Triangle (3)
--                   or else new_Triangle (2) = new_Triangle (3)
--           then
--              null;
--  --              put_line ("discard collapsed tri: "
--  --                        & natural'image (new_triangle (1))
--  --                        & natural'image (new_triangle (2))
--  --                        & natural'image (new_triangle (3)));
--           else
--
--              if        contains (self.triangles, new_triangle)
--                or else contains (self.triangles, new_triangle_rotated_1)
--                or else contains (self.triangles, new_triangle_rotated_2)
--              then
--                 null;
--  --                   put_Line ("!model: ALREADY present tri ... "
--  --                          & natural'image (new_triangle (1))
--  --                          & natural'image (new_triangle (2))
--  --                          & natural'image (new_triangle (3)));
--              else
--                 --insert (self.Triangles, new_Triangle);
--                 include (self.Triangles, new_Triangle);
--  --                 put_Line ("model: added new tri ... "
--  --                           & natural'image (new_triangle (1))
--  --                           & natural'image (new_triangle (2))
--  --                           & natural'image (new_triangle (3)));
--  --
--  --                 put_Line ("     " & number'image (vertex_1 (1))
--  --                           & "  " & number'image (vertex_1 (2))
--  --                           & "  " & number'image (vertex_1 (3)));
--  --
--  --                 put_Line ("     " & number'image (vertex_2 (1))
--  --                           & "  " & number'image (vertex_2 (2))
--  --                           & "  " & number'image (vertex_2 (3)));
--  --
--  --                 put_Line ("     " & number'image (vertex_3 (1))
--  --                           & "  " & number'image (vertex_3 (2))
--  --                           & "  " & number'image (vertex_3 (3)));
--              end if;
--
--           end if;
--        end;
--
--
--
--
--
--
--        procedure add (Self         : in out Model.item;
--                       the_Triangle : in     Triangle)
--        is
--        begin
--           self.add_Triangle (the_Triangle (1),  the_Triangle (2),  the_Triangle (3));
--        end;
--
--
--
--
--
--
--        procedure add (Self          : in out Model.item;
--                       the_Triangles : in     mesh.Triangles)
--        is
--        begin
--           for Each in the_Triangles'range loop
--              Self.add (the_Triangles (Each));
--           end loop;
--        end;
--
--
--
--
--
--
--        function triangle_Count (Self : in Model.item) return Natural
--        is
--           use index_triangle_Sets;
--        begin
--           return Natural (Length (self.Triangles));
--        end;
--
--
--
--
--
--        function Triangles (Self : in Model.item) return math.geom.d3.mesh.Triangles
--        is
--           use vertex_Vectors;
--           use index_triangle_Sets;
--
--           the_Triangles  : math.geom.d3.mesh.Triangles (1 .. Natural (Length (self.Triangles)));
--
--           each_Triangle  : index_triangle_Sets.Cursor := First (self.Triangles);
--           triangle_Count : Natural                    := 0;
--
--        begin
--
--           --for Each in the_Triangles'range loop
--           while has_Element (each_Triangle) loop
--              triangle_Count := triangle_Count + 1;
--
--              the_Triangles (triangle_Count) (1) := Element (self.Vertices,
--                                                   Element (each_Triangle) (1));
--              the_Triangles (triangle_Count) (2) := Element (self.Vertices,
--                                                   Element (each_Triangle) (2));
--              the_Triangles (triangle_Count) (3) := Element (self.Vertices,
--                                                             Element (each_Triangle) (3));
--
--              next (each_Triangle);
--           end loop;
--
--
--           return the_Triangles;
--        end;
--
--
--
--
--
--
--
--        function Data (Self : in Model.item) return mesh.Data_view
--        is
--           use index_triangle_Sets;
--           use vertex_Vectors;
--
--           the_Data       : mesh.Data_view             := new mesh.Data_item (Positive (Length (self.Vertices)),
--                                                                              Positive (Length (self.Triangles)));
--           each_Triangle  : index_triangle_Sets.Cursor := First (self.Triangles);
--           triangle_Count : Natural                    := 0;
--
--        begin
--
--           for Each in the_data.Vertices'range loop
--              the_data.Vertices (Each) := Element (self.Vertices, Each);
--           end loop;
--
--
--           while has_Element (each_Triangle) loop
--              triangle_Count                      := triangle_Count + 1;
--              the_data.Triangles (triangle_Count) := Element (each_Triangle);
--
--              next (each_Triangle);
--           end loop;
--
--
--           return the_Data;
--
--        end Data;
--
--
--
--     end Model;
--










--     -- sculpture
--     --
--
--     type private_Sculpture is
--        record
--           full_Model : math.geom.d3.mesh.Model_full_detail.item;
--        end record;
--
--
--
--
--
--     procedure initialize (Self : in out Sculpture)
--     is
--     begin
--        self.Privvy := new private_Sculpture;
--     end;
--
--
--
--
--
--
--     procedure finalize   (Self : in out Sculpture)
--     is
--        procedure deallocate is new ada.unchecked_deallocation (private_Sculpture, private_Sculpture_view);
--     begin
--        deallocate (self.Privvy);
--     end;
--
--
--
--
--
--
--     procedure add_Triangle (Self : in out Sculpture;   Vertex_1, Vertex_2, Vertex_3 : in Vertex)
--     is
--        use math.geom.d3.mesh.Model_full_detail;
--     begin
--        add_Triangle (self.privvy.full_Model,  Vertex_1, Vertex_2, Vertex_3);
--     end;
--
--
--
--
--
--
--     function Data (Self : in Sculpture;   detail_Radius : math.Number := math.Number'small) return mesh.Data_view
--     is
--        use math.geom.d3.mesh.Model_full_detail;
--     begin
--
--        if detail_Radius <= math.Number'small then   -- ie  is the default
--
--           return Data (self.privvy.full_Model);
--
--        else
--
--           declare
--              package my_Model is new Model (detail_Radius);   use my_Model;
--
--              the_Model : my_Model.item;
--           begin
--              the_Model.add (self.privvy.full_Model.Triangles);
--
--              return Data (the_Model);
--           end;
--
--        end if;
--
--     end Data;












--     -- heightmap mesh
--     --
--
--
--     function to_Mesh (from_Heightmap : in math.Matrix.item'class) return Mesh.item
--     is
--        use math.Vector.standard;
--        --the_Model :         mesh.Model_full_detail.item;   use mesh.Model_full_detail;
--        --package my_model is new mesh.model (detail_radius => 0.95);    use my_model;
--        package my_model is new mesh.model (detail_radius => 5.05);    use my_model;
--        --package my_model is new mesh.model (detail_radius => 0.000005);    use my_model;
--        the_Model   :         my_Model.item;
--
--
--        the_Mesh    : aliased Mesh.item;
--        --the_Heights : access  math.number_Block    :=  new math.number_Block'(from_Heightmap.Data); --(from_Heightmap'range(1), from_Heightmap'range(2));
--        the_Heights :         math.number_Block    :=  from_Heightmap.Data; --(from_Heightmap'range(1), from_Heightmap'range(2));
--
--        Height_min  :         math.Number         := math.Number'last;
--        Height_max  :         math.Number         := math.Number'first;
--        Midpoint    :         math.Vector.standard.item_3;
--
--     begin
--
--        for each_Row in 1 .. the_Heights'last(1) loop
--           for each_Col in 1 .. the_Heights'last(2) loop
--              the_Heights (each_Row, each_Col) := the_Heights (each_Row, each_Col) * 0.5; -- 0.1;
--
--              Height_min := math.Number'min (Height_min,  the_Heights (each_Row, each_Col));
--              Height_max := math.Number'max (Height_max,  the_Heights (each_Row, each_Col));
--           end loop;
--        end loop;
--
--
--        Midpoint := to_Vector ((  0.0, --Number (the_Heights'last(2))  / 2.0,
--                                100.0, --(Height_max - Height_min)     / 2.0,
--                                  0.0)); --Number (the_Heights'last(1))  / 2.0);
--
--
--
--        for each_Row in 1 .. the_Heights'last(1) - 1 loop
--           for each_Col in 1 .. the_Heights'last(2) - 1 loop
--              declare
--                 V1 : Vertex := Vertex'(Number (each_Col),      the_Heights (each_Row,     each_Col),      Number (each_Row)    );
--                 V2 : Vertex := Vertex'(Number (each_Col + 1),  the_Heights (each_Row + 1, each_Col + 1),  Number (each_Row + 1));
--                 V3 : Vertex := Vertex'(Number (each_Col + 1),  the_Heights (each_Row,     each_Col + 1),  Number (each_Row)    );
--              begin
--                 add_Triangle (the_Model,   V1 - midpoint,
--                                            V2 - midpoint,
--                                            V3 - midpoint);
--              end;
--
--              declare
--                 V1 : Vertex := Vertex'(Number (each_Col + 1),  the_Heights (each_Row + 1, each_Col + 1),  Number (each_Row + 1));
--                 V2 : Vertex := Vertex'(Number (each_Col),      the_Heights (each_Row,     each_Col),      Number (each_Row)    );
--                 V3 : Vertex := Vertex'(Number (each_Col),      the_Heights (each_Row + 1, each_Col),      Number (each_Row + 1));
--              begin
--                 add_Triangle (the_Model,   V1 - midpoint,
--                                            V2 - midpoint,
--                                            V3 - midpoint);
--              end;
--           end loop;
--        end loop;
--
--
--
--        the_mesh.Data_is (Data (the_Model));
--
--        return the_Mesh;
--     end;










   -- polar model: polar to euclidian shape models.
   --


   use math.Functions;

--   use type Number;



--     function to_polar_Model (Model_Filename : in String;
--                              Scaled_by      : in Number := 1.0) return polar_Model
--     is
--        -- tbd: mod to use a factory.
--
--        The_Text      : aliased lace.Text.Instance := Creation (get_String (lace.text.filename (Model_Filename)));
--
--        the_Latitude  : latitude;
--        the_Longitude : longitude;
--
--        Lat           : Radians;
--        Long          : Radians;
--
--        the_Distance  : Number;
--
--        the_Model     : polar_Model;
--
--     begin
--
--        while not at_End (the_Text) loop
--
--           the_Longitude := longitude (get_Integer (the_Text'access));
--           the_Latitude  := latitude  (get_Integer (the_Text'access));
--
--           Lat  := to_Radians (Degrees (the_Latitude));
--           Long := to_Radians (Degrees (the_Longitude));
--
--           the_Distance  := Number  (get_Float (the_Text'access)) * Scaled_by;
--
--           eat_white (the_Text);
--
--           the_Model (the_Longitude) (the_Latitude).Vertex (1) := Number (cos (Lat) * sin (Long)) * the_Distance;
--           the_Model (the_Longitude) (the_Latitude).Vertex (2) := Number (sin (Lat))              * the_Distance;
--           the_Model (the_Longitude) (the_Latitude).Vertex (3) := Number (cos (Lat) * cos (Long)) * the_Distance;
--
--        end loop;
--
--
--        return the_Model;
--
--     end;
--
--
--
--
--
--
--     function to_mesh_Data (From : in polar_Model) return mesh.Data_item
--     is
--        the_raw_model : polar_Model := From;
--
--        the_mesh_Model : mesh.Data_item (vertex_Count   => 2557,
--                                          triangle_Count => 73 * (16 * 4 + 6)); -- tbd: replace constants with a generalised solution
--
--        the_longitude  : longitude := 0;
--        the_latitude   : latitude ;
--
--        the_Vertex   : Positive := 1;
--        the_Triangle : Positive := 1;
--
--        the_North_Pole : Positive;
--        the_South_Pole : Positive;
--
--     begin
--
--        the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (0) (-90).Vertex;
--        the_North_Pole := the_Vertex;
--        the_raw_Model (0) (-90).Id := the_Vertex;
--        the_Vertex := the_Vertex + 1;
--
--
--        the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (0) (90).Vertex;
--        the_south_Pole := the_Vertex;
--        the_raw_Model (0) (90).Id := the_Vertex;
--        the_Vertex := the_Vertex + 1;
--
--
--        loop
--
--           the_latitude := -90;
--
--           loop
--
--              if the_Latitude = -90 then
--                 the_raw_Model (the_Longitude) (the_Latitude).Id := the_North_Pole;
--
--              elsif the_Latitude = 90 then
--                 the_raw_Model (the_Longitude) (the_Latitude).Id := the_South_Pole;
--              else
--                 the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (the_Longitude) (the_Latitude).Vertex;
--                 the_raw_Model (the_Longitude) (the_Latitude).Id := the_Vertex;
--                 the_Vertex := the_Vertex + 1;
--              end if;
--
--
--              exit when the_Latitude = 90;
--
--              the_Latitude := the_Latitude + 5;
--
--           end loop;
--
--
--           exit when the_Longitude = 360;
--
--           the_Longitude := the_Longitude + 5;
--
--        end loop;
--
--
--
--        the_Longitude := 0;
--
--        loop
--
--           if the_Longitude = 360 then
--              the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_North_Pole,
--                                                             2 => the_raw_Model (5) (-85).Id,
--                                                             3 => the_raw_Model (the_Longitude) (-85).Id);
--  --                                                      2 => the_raw_Model (the_Longitude) (-85).Id,
--  --                                                         3 => the_raw_Model (5) (-85).Id);
--           else
--              the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_North_Pole,
--                                                          2 => the_raw_Model (the_Longitude + 5) (-85).Id,
--                                                          3 => the_raw_Model (the_Longitude) (-85).Id);
--  --                                                    2 => the_raw_Model (the_Longitude) (-85).Id,
--  --                                                    3 => the_raw_Model (the_Longitude + 5) (-85).Id);
--           end if;
--
--           the_Triangle := the_Triangle + 1;
--
--
--           if the_Longitude = 360 then
--              the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_South_Pole,
--                                                             2 => the_raw_Model (the_Longitude) (85).Id,
--                                                             3 => the_raw_Model (5) (85).Id);
--  --                                                       2 => the_raw_Model (5) (85).Id,
--  --                                                       3 => the_raw_Model (the_Longitude) (85).Id);
--           else
--              the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_South_Pole,
--                                                             2 => the_raw_Model (the_Longitude) (85).Id,
--                                                             3 => the_raw_Model (the_Longitude + 5) (85).Id);
--  --                                                         2 => the_raw_Model (the_Longitude + 5) (85).Id,
--  --                                                         3 => the_raw_Model (the_Longitude) (85).Id);
--           end if;
--
--           the_Triangle := the_Triangle + 1;
--
--
--
--           the_latitude := -85;
--
--           loop
--
--              if the_Longitude = 360 then
--                 the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (the_Longitude) (the_Latitude).Id,
--                                                             2 => the_raw_Model (5) (the_Latitude).Id,
--                                                             3 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id);
--  --                                                         2 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--  --                                                         3 => the_raw_Model (5) (the_Latitude).Id);
--              else
--                 the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (the_Longitude) (the_Latitude).Id,
--                                                             2 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id,
--                                                             3 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id);
--  --                                                         2 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--  --                                                         3 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id);
--              end if;
--
--
--              the_Triangle := the_Triangle + 1;
--
--
--              if the_Longitude = 360 then
--                 the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--                                                             2 => the_raw_Model (5) (the_Latitude).Id,
--                                                             3 => the_raw_Model (5) (the_Latitude + 5).Id);
--  --                                                         2 => the_raw_Model (5) (the_Latitude + 5).Id,
--  --                                                         3 => the_raw_Model (5) (the_Latitude).Id);
--              else
--                 the_mesh_Model.Triangles (the_Triangle) :=  (1 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--                                                                2 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id,
--                                                                3 => the_raw_Model (the_Longitude + 5) (the_Latitude + 5).Id);
--  --                                                            2 => the_raw_Model (the_Longitude + 5) (the_Latitude + 5).Id,
--  --                                                            3 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id);
--              end if;
--
--
--              the_Triangle := the_Triangle + 1;
--
--
--              the_Latitude := the_Latitude + 5;
--
--              exit when the_Latitude = 85;
--
--           end loop;
--
--
--           exit when the_Longitude = 360;
--
--           the_Longitude := the_Longitude + 5;
--
--
--
--        end loop;
--
--
--  --        put_line  ("total num_vert: " & integer'image (the_vertex));
--  --        put_Line ("total num_tri: "  & integer'image (the_triangle));
--
--
--        return the_mesh_Model;
--
--     end;








--     function to_Mesh (From : in polar_Model) return Mesh.item
--     is
--     begin
--        return (Data => new mesh.Data_item'(to_mesh_Data (From)));
--     end;
--
--
--
--
--
--
--
--     function Image (Self : in Data_item) return String
--     is
--        the_Image : unbounded_String;
--     begin
--        append (the_Image,  "(");
--
--        for Each in self.Vertices'range loop
--           append (the_Image,  math.Image (self.Vertices (Each)));
--        end loop;
--
--        append (the_Image,  ") (");
--
--
--        for Each in self.Triangles'range loop
--           append (the_Image,  Image (self.Triangles (Each)));
--        end loop;
--
--        append (the_Image,  ")");
--
--        return to_String (the_Image);
--     end;
--
--
--
--
--
--
--     function Image (Self : in index_Triangle) return String
--     is
--     begin
--        return  "("  &  positive'Image (Self (1))  &  ", "  &  positive'Image (Self (2))  &  ", " & positive'Image (Self (3))  &  ")";
--     end;
--
--
--
--
--
--
--     function Image (Self : in index_Triangles) return String
--     is
--        the_Image : unbounded_String;
--     begin
--        append (the_Image,  "(");
--
--        for Each in Self'range loop
--           if Each /= 1 then
--              append (the_Image,  ", ");
--           end if;
--
--           append (the_Image,  Image (Self (Each)));
--        end loop;
--
--        append (the_Image,  ")");
--
--
--        return  to_String (the_Image);
--     end;


end math.geom.d3.Mesh.gts;

