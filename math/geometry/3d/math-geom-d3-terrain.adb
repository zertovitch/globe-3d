
with math.geom.d3.Modeller_full_detail;
with math.Algebra.linear;                use math.Algebra.linear;
with globe_3d.math; use globe_3d.math;

with ada.unchecked_deallocation;

with lace.Debug; use lace.Debug;




package body math.geom.d3.Terrain is


   use type math.Number;
   use type math.Integer;




   procedure update (Self : access Item)
   is
   begin
      if self.Heights = null then
         return;
      end if;

      declare
         use math.Functions;
         half_Width : Number := self.Width / 2.0;
         half_Depth : Number := self.Depth / 2.0;
      begin
         self.min_Height             := Min (self.Heights.all) * self.Scale (2);
         self.max_Height             := Max (self.Heights.all) * self.Scale (2);

         self.height_Offset          := self.min_Height + (self.max_Height - self.min_Height) / 2.0;

         self.bounding_sphere_Radius := sqrt (  half_Width * half_Width
                                              + half_Depth * half_Depth
                                              + ((self.max_Height - self.min_Height) / 2.0)  *  ((self.max_Height - self.min_Height) / 2.0));
      end;
   end;




   procedure destroy (Self : in out Item)
   is
   begin
      free (self.Heights);
   end;




   procedure Scale_is (Self : access Item;   Now : in Vector_3)
   is
   begin
      self.Scale := Now;
      update (Self);
   end;




   function  Scale (Self : in     Item)     return Vector_3
   is
   begin
      return self.Scale;
   end;





   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin
      return self.bounding_sphere_Radius;
   end;




   function height_Offset (Self : access Item) return Number
   is
   begin
      return self.height_Offset;
   end;




   function Width (Self : access Item) return Number
   is
   begin
      return Number (self.Heights'Length (2) - 1) * self.Scale (1);
   end;




   function Depth (Self : access Item) return Number
   is
   begin
      return Number (self.Heights'Length (1) - 1) * self.Scale (3);
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




   function  Heights (Self   : access Item) return Matrix
   is
   begin
      return self.Heights.all;
   end;





   procedure Heights_are (Self : access Item;
                          Now  : in     Matrix)
   is
   begin
      self.Heights := new Matrix' (Now);
      update (Self);
   end;





   function  Length_of (Self    : access Item) return Number
   is
   begin
      return self.Length;
   end;



   procedure Length_of (Self    : access Item;
                        is_Now  : in     Number)
   is
   begin
      self.Length := is_Now;
   end;






   function  num_Nodes_per_Side_of (Self   : access Item) return Integer
   is
   begin
      return self.num_Nodes_per_Side;
   end;



   procedure num_Nodes_per_Side_of (Self   : access Item;
                                    is_Now  : in    Integer)
   is
   begin
      self.num_Nodes_per_Side := is_Now;
   end;







--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return Model_view
   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view
   is
      function marshal_Options return terrain.model_Options is
      begin
         if Options = null_Options then   return null_terrain_Options;
         else                             return terrain.model_Options (Options);
         end if;
      end;

      the_Options : terrain.model_Options := marshal_Options;
      Midpoint    : Vector_3;
      the_Factory :     math.geom.d3.Modeller_full_detail.item;
                    use math.geom.d3.Modeller_full_detail;

   begin
      Midpoint := ((self.Width / 2.0)  / self.Scale (1),
                    self.height_Offset / self.Scale (2),
                   (self.Depth / 2.0)  / self.Scale (3) );

      for each_Row in 1 .. self.Heights'last(1) - 1 loop
         for each_Col in 1 .. self.Heights'last(2) - 1 loop
            declare
               procedure scale (the_Vertex : in out Vertex)
               is
               begin
                  the_Vertex (1) := the_Vertex (1) * self.Scale (1);
                  the_Vertex (2) := the_Vertex (2) * self.Scale (2);
                  the_Vertex (3) := the_Vertex (3) * self.Scale (3);
               end scale;

               NW : Vertex := Vertex'(Number (each_Col - 1),  self.Heights (each_Row,     each_Col),      Number (each_Row - 1))  -  midpoint;
               SW : Vertex := Vertex'(Number (each_Col - 1),  self.Heights (each_Row + 1, each_Col),      Number (each_Row    ))  -  midpoint;
               NE : Vertex := Vertex'(Number (each_Col    ),  self.Heights (each_Row,     each_Col + 1),  Number (each_Row - 1))  -  midpoint;
               SE : Vertex := Vertex'(Number (each_Col    ),  self.Heights (each_Row + 1, each_Col + 1),  Number (each_Row    ))  -  midpoint;
            begin
               scale (NW);   scale (SW);   scale (NE);   scale (SE);

               if    each_Row = 1 then
                  NW (3) := NW (3) - the_options.Skirt;
                  NE (3) := NE (3) - the_options.Skirt;

               elsif each_Row = self.Heights'last(1) - 1 then
                  SW (3) := SW (3) + the_options.Skirt;
                  SE (3) := SE (3) + the_options.Skirt;
               end if;


               if    each_Col = 1 then
                  NW (1) := NW (1) - the_options.Skirt;
                  SW (1) := SW (1) - the_options.Skirt;

               elsif each_Col = self.Heights'last(2) - 1 then
                  NE (1) := NE (1) + the_options.Skirt;
                  SE (1) := SE (1) + the_options.Skirt;
               end if;


               the_factory.add_Triangle (NW, SW, NE);
               the_factory.add_Triangle (NE, SW, SE);
            end;
         end loop;
      end loop;


      return the_Factory.Model;
   end;








   -- gl triangle strips
   --


--     function to_gl_Strips (Self : access Item) return gl.geometry.triangle_Strips
--     is
--        use math.Vector.standard, GL, gl.Geometry;
--
--  --        the_Strips    : gl.geometry.triangle_Strips (1 .. 5_000);
--  --        strip_Count   : Natural                                 := 0;
--
--        proper_triangle_Count     : Natural := (self.Heights.all'last (1) - 1) * (self.Heights.all'last (2) - 1) * 2;
--        degenerate_triangle_Count : Natural := self.Heights.all'last (1) - 1;
--
--        the_Strip  : gl.geometry.p_triangle_Strip := new_triangle_Strip (triangle_count => proper_triangle_Count + degenerate_triangle_Count);
--
--  --      the_vertex_Map : vertex_pool_Map := vertex_pool_Map_for (Self);
--  --        Midpoint       : math.Vector.standard.item_3  := to_Vector ((Number (self.Heights.all'last(2))  / 2.0, -- + 0.5,
--  --                                                                     self.height_Offset,  --0.0, --(Height_max - Height_min)     / 2.0,
--  --                                                                     Number (self.Heights.all'last(1))  / 2.0)); -- + 0.5));
--        Midpoint       : gl.geometry.Vertex  := ((gl.Double (self.Heights.all'last(2))  / 2.0, -- + 0.5,
--                                                  gl.Double (self.height_Offset),  --0.0, --(Height_max - Height_min)     / 2.0,
--                                                  gl.Double (self.Heights.all'last(1))  / 2.0)); -- + 0.5));
--
--        current_Vertex : vertex_Id;
--        current_Index  : Positive;
--
--     begin
--
--        for each_Row in 1 .. 1  loop --self.Heights'last(1) - 1 loop
--
--           for each_Col in 1 .. self.Heights'last(2) - 1 loop
--              declare
--                 Row_r : Integer := each_Row; -- self.Heights'last (1) - each_Row;    -- reflected row
--
--                 NW : gl.geometry.Vertex := (gl.Double (each_Col),      gl.double (self.Heights (Row_r,     each_Col)),      gl.Double (Row_r)    );
--                 SW : gl.geometry.Vertex := (gl.Double (each_Col),      gl.double (self.Heights (Row_r + 1, each_Col)),      gl.Double (Row_r + 1));
--                 NE : gl.geometry.Vertex := (gl.Double (each_Col + 1),  gl.double (self.Heights (Row_r,     each_Col + 1)),  gl.Double (Row_r)    );
--                 SE : gl.geometry.Vertex := (gl.Double (each_Col + 1),  gl.double (self.Heights (Row_r + 1, each_Col + 1)),  gl.Double (Row_r + 1));
--
--                 prior_row_final_N : vertex_Id;
--                 prior_row_final_S : vertex_Id;
--
--              begin
--
--                 if each_Row = 1 and then each_Col = 1 then
--                    current_Vertex                                  := 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := NW - Midpoint;
--                    current_Index                                   := 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;   -- '- 1' to adjust for c 0-based indexing
--
--                    current_Vertex                                  := current_Vertex + 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := SW - Midpoint;
--                    current_Index                                   := current_Index  + 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;
--                 end if;
--
--                 if each_Row mod 2 = 1 then  -- odd rows
--                    current_Vertex                                  := current_Vertex + 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := NE - Midpoint;
--                    current_Index                                   := current_Index  + 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;
--
--                    current_Vertex                                  := current_Vertex + 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := SE - Midpoint;
--                    current_Index                                   := current_Index  + 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;
--                 else                        -- even rows
--                    current_Vertex                                  := current_Vertex + 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := NW - Midpoint;
--                    current_Index                                   := current_Index  + 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;
--
--                    current_Vertex                                  := current_Vertex + 1;
--                    the_Strip.vertex_Pool.Vertices (current_Vertex) := SW - Midpoint;
--                    current_Index                                   := current_Index  + 1;
--                    the_Strip.Indices (current_Index)               := current_Vertex - 1;
--                 end if;
--
--                 if each_Col = self.Heights'last(2) - 1 then
--                    prior_row_final_N := the_Strip.Indices (current_Index - 1);
--                    prior_row_final_S := the_Strip.Indices (current_Index);
--                 end if;
--
--              end;
--           end loop;
--
--
--           if each_Row /= self.Heights'last(1) - 1 then  -- add degenerate triangles
--
--              if each_Row mod 2 = 1 then  -- at end of an odd row
--                 current_Index                                   := current_Index  + 1;
--                 the_Strip.Indices (current_Index)               := current_Vertex - 1;
--              else
--                 current_Index                                   := current_Index  + 1;
--                 the_Strip.Indices (current_Index)               := current_Vertex - 1;
--              end if;
--
--           end if;
--
--        end loop;
--
--        return (1 => the_Strip);
--
--
--
--  --
--  --
--  --        loop
--  --           strip_Count := strip_Count + 1;
--  --           dLog ("strip count:" & integer'image (strip_Count));
--  --
--  --           declare
--  --              use gl, gl.Geometry, glibb.binding;
--  --              triangles_List : glibb.GSList.view := to_Pointer (strips_List.data).all'access;
--  --
--  --              the_Triangle   : gtsTriangle.view  := to_Pointer (triangles_List.data).all'access;
--  --              prior_Triangle : gtsTriangle.view;
--  --
--  --
--  --              procedure dlog_tri
--  --              is
--  --                 v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--  --                 v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--  --                 v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--  --              begin
--  --                 dLog ("");
--  --                 dLog ("v1: " &   image (number (v1.p.x)) & " " &  image (number (v1.p.y)) & " " &  image (number (v1.p.z)));
--  --                 dLog ("v2: " &   image (number (v2.p.x)) & " " &  image (number (v2.p.y)) & " " &  image (number (v2.p.z)));
--  --                 dLog ("v3: " &   image (number (v3.p.x)) & " " &  image (number (v3.p.y)) & " " &  image (number (v3.p.z)));
--  --
--  --              end;
--  --
--  --
--  --  --              v1 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 1);
--  --  --              v2 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 2);
--  --  --              v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_n (the_Triangle, 3);
--  --              v1 : standard.gts.GtsVertex.view := the_Triangle.e1.segment.v1;
--  --              v2 : standard.gts.GtsVertex.view := the_Triangle.e1.segment.v2;
--  --              v3 : standard.gts.GtsVertex.view := gts_triangle_vertex_other (the_Triangle);
--  --           begin
--  --  --            dLog ("to_tri_strips: tri orientation: " & image (number (gts_triangle_orientation (the_Triangle))));
--  --
--  --              current_Strip            := new_triangle_Strip (triangle_count => Positive (g_slist_length (triangles_List)),
--  --                                                              vertex_source  => the_vertex_Map.Pool);
--  --  --            the_Strips (strip_Count) := current_Strip;
--  --              the_Strips (strip_Count) := current_Strip.all'access;
--  --
--  --              --dlog_tri;
--  --
--  --
--  --                 current_Strip.Indices (1) := Element (the_vertex_Map.Map, v1);
--  --                 current_Strip.Indices (2) := Element (the_vertex_Map.Map, v2);
--  --                 current_Strip.Indices (3) := Element (the_vertex_Map.Map, v3);
--  --
--  --
--  --
--  --              declare
--  --  --               current_Vertex : gl.uInt := 2;
--  --                 current_Index  : Natural := 3;
--  --
--  --                 common_Edge     : standard.gts.GtsEdge.view;
--  --                 opposite_Vertex : standard.gts.GtsVertex.view;
--  --              begin
--  --
--  --                 while triangles_List.next /= null loop
--  --
--  --                    prior_Triangle := the_Triangle;
--  --
--  --                    triangles_List := triangles_List.next;
--  --                    the_Triangle   := to_Pointer (triangles_List.data).all'access;
--  --
--  --                    common_Edge     := gts_triangles_common_edge    (the_Triangle, prior_Triangle);
--  --                    opposite_Vertex := gts_triangle_vertex_opposite (the_Triangle, common_Edge);
--  --
--  --                    --dlog_tri;
--  --
--  --  --                    if current_Vertex mod 2 = 0 then
--  --  --                       v3 := gts_triangle_vertex_n (the_Triangle, 3);
--  --  --                    else
--  --  --                       v3 := gts_triangle_vertex_n (the_Triangle, 2);
--  --  --                    end if;
--  --  --              v1 := gts_triangle_vertex_n (the_Triangle, 1);
--  --  --              v2 := gts_triangle_vertex_n (the_Triangle, 2);
--  --  --              v3 := gts_triangle_vertex_n (the_Triangle, 3);
--  --
--  --
--  --  --                    current_Vertex                                      := current_Vertex + 1;
--  --  --                    current_Strip.vertex_Pool.Vertices (current_Vertex) := (gl.Double (opposite_Vertex.p.x),
--  --  --                                                                            gl.Double (opposite_Vertex.p.y),
--  --  --                                                                            gl.Double (opposite_Vertex.p.z));
--  --
--  --                    current_Index                         := current_Index + 1;
--  --                    current_Strip.Indices (current_Index) := Element (the_vertex_Map.Map, opposite_Vertex);
--  --                 end loop;
--  --              end;
--  --
--  --           end;
--  --
--  --
--  --           exit when strips_List.next = null;
--  --
--  --           strips_List := strips_List.next;
--  --        end loop;
--  --
--  --
--  --        return the_Strips (1 .. strip_Count);
--     end;





end math.geom.d3.Terrain;

