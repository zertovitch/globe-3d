
with lace.Debug;                           use lace.Debug;

with ada.Strings.unbounded;                use ada.Strings.unbounded;
with ada.unchecked_deallocation;





package body math.geom.d3.Modeller is

   use type math.Number;
   use type math.Integer;



   function my_Less_Than (L, R : in my_Vertex) return Boolean  -- tbd: this is garbage !!!
   is
   begin

      if    L (1) < R (1) then return True;
      elsif R (1) < L (1) then return False;
      end if;


      if    L (2) < R (2) then return True;
      elsif R (2) < L (2) then return False;
      end if;


      if    L (3) < R (3) then return True;
      elsif R (3) < L (3) then return False;
      end if;

      return False;
   end;

--        function my_Less_Than (L, R : in my_Vertex) return Boolean
--        is
--           X_diff : Number := abs (L (1) - R (1));
--           Y_diff : Number;
--           Z_diff : Number;
--        begin
--
--           if X_diff > detail_Radius then
--              return L (1) < R (1);
--           end if;
--
--
--           Y_diff := abs (L (2) - R (2));
--
--           if Y_diff > detail_Radius then
--              return L (2) < R (2);
--           end if;
--
--
--           Z_diff := abs (L (3) - R (3));
--
--           if Z_diff > detail_Radius then
--              return L (3) < R (3);
--           end if;
--
--
--           return false;   -- must be 'equivalent'
--        end;




--        function "=" (L, R : in my_Vertex) return Boolean
--        is
--        begin
--           put_Line ("In MY ===========");
--           return     abs (L (1) - R (1))  <  detail_Radius
--             and then abs (L (2) - R (2))  <  detail_Radius
--             and then abs (L (3) - R (3))  <  detail_Radius;
--        end;





      function Rounded (Self : in my_Vertex) return my_Vertex
      is
         function Round (the_Number : in math.Number) return math.Number
         is
            Factor : constant Number := 1.0 / 0.1; --detail_Radius;
         begin
            --put_Line ("the_NUM: " & Number'Image (the_Number)    & "     FACT: " & number'image (Factor));
            return Number (long_integer (Factor * the_Number)) / Factor;
         end;

      begin
         return (Round (Self (1)),
                 Round (Self (2)),
                 Round (Self (3)));
      end;





      function demand_Index (Self       : access Modeller.item;
                             for_Vertex : in     my_Vertex) return Natural
      is
         use vertex_index_Maps;
         use vertex_Vectors;

         --rounded_Vertex : my_Vertex               := Rounded (for_Vertex);
         rounded_Vertex : my_Vertex               := for_Vertex;
         map_Cursor     : vertex_index_map_Cursor := find (self.vertex_index_Map, rounded_Vertex);
      begin
         if has_Element (map_Cursor) then
            return Element (map_Cursor);
         end if;

         append (self.Vertices,          Vertex (rounded_Vertex));

--           put_line ("inserting new vertex: "
--                     & "  " & number'image (rounded_Vertex (1))
--                     & "  " & number'image (rounded_Vertex (2))
--                     & "  " & number'image (rounded_Vertex (3)));

         --insert (self.vertex_index_Map,  for_Vertex,  Natural (Length (self.Vertices)));
         insert (self.vertex_index_Map,  rounded_Vertex,  Natural (Length (self.Vertices)));

         return Natural (Length (self.Vertices));
      end;







      procedure add_Triangle (Self     : in out Modeller.item;
                              Vertex_1 : in     Vertex;
                              Vertex_2 : in     Vertex;
                              Vertex_3 : in     Vertex)
      is
         --use index_triangle_Vectors;
         use index_triangle_Sets;

         vertex_1_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_1));
         vertex_2_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_2));
         vertex_3_Index : Natural := demand_Index (Self'access, my_Vertex (Vertex_3));

         new_Triangle           : index_Triangle := (vertex_1_Index, vertex_2_Index, vertex_3_Index);
         new_Triangle_rotated_1 : index_Triangle := (vertex_3_Index, vertex_1_Index, vertex_2_Index);
         new_Triangle_rotated_2 : index_Triangle := (vertex_2_Index, vertex_3_Index, vertex_1_Index);
      begin
--           put_Line ("model: tri count: " & natural'image (natural (Length (self.Triangles))));



         if              new_Triangle (1) = new_Triangle (2)
                 or else new_Triangle (1) = new_Triangle (3)
                 or else new_Triangle (2) = new_Triangle (3)
         then
            null;
--              put_line ("discard collapsed tri: "
--                        & natural'image (new_triangle (1))
--                        & natural'image (new_triangle (2))
--                        & natural'image (new_triangle (3)));
         else

            if        contains (self.triangles, new_triangle)
              or else contains (self.triangles, new_triangle_rotated_1)
              or else contains (self.triangles, new_triangle_rotated_2)
            then
               null;
--                   put_Line ("!model: ALREADY present tri ... "
--                          & natural'image (new_triangle (1))
--                          & natural'image (new_triangle (2))
--                          & natural'image (new_triangle (3)));
            else
               --insert (self.Triangles, new_Triangle);
               include (self.Triangles, new_Triangle);
--                 put_Line ("model: added new tri ... "
--                           & natural'image (new_triangle (1))
--                           & natural'image (new_triangle (2))
--                           & natural'image (new_triangle (3)));
--
--                 put_Line ("     " & number'image (vertex_1 (1))
--                           & "  " & number'image (vertex_1 (2))
--                           & "  " & number'image (vertex_1 (3)));
--
--                 put_Line ("     " & number'image (vertex_2 (1))
--                           & "  " & number'image (vertex_2 (2))
--                           & "  " & number'image (vertex_2 (3)));
--
--                 put_Line ("     " & number'image (vertex_3 (1))
--                           & "  " & number'image (vertex_3 (2))
--                           & "  " & number'image (vertex_3 (3)));
            end if;

         end if;
      end;



      procedure clear        (Self : in out Modeller.item)
      is
         use index_triangle_Sets, vertex_Vectors, modeller.vertex_index_Maps;
      begin
         self.Triangles.clear;
         self.Vertices.clear;
         self.vertex_index_Map.clear;
      end;




      procedure add_Model (Self      : in out Modeller.item;
                           the_Model : in     math.geom.d3.Model_view)
      is
      begin
         for Each in the_model.Triangles'range loop
            self.add_Triangle (the_Model.Vertices (math.Integer (the_model.Triangles (Each) (1))),
                               the_Model.Vertices (math.Integer (the_model.Triangles (Each) (2))),
                               the_Model.Vertices (math.Integer (the_model.Triangles (Each) (3))));
         end loop;
      end;




      procedure add (Self         : in out Modeller.item;
                     the_Triangle : in     Triangle)
      is
      begin
         self.add_Triangle (the_Triangle (1),  the_Triangle (2),  the_Triangle (3));
      end;






      procedure add (Self          : in out Modeller.item;
                     the_Triangles : in     d3.Triangles)
      is
      begin
         for Each in the_Triangles'range loop
            Self.add (the_Triangles (Each));
         end loop;
      end;






      function triangle_Count (Self : in Modeller.item) return Natural
      is
         use index_triangle_Sets;
      begin
         return Natural (Length (self.Triangles));
      end;





      function Triangles (Self : in Modeller.item) return math.geom.d3.Triangles
      is
         use vertex_Vectors;
         use index_triangle_Sets;

         the_Triangles  : math.geom.d3.Triangles (1 .. math.Integer (Length (self.Triangles)));

         each_Triangle  : index_triangle_Sets.Cursor := First (self.Triangles);
         triangle_Count : math.Integer               := 0;

      begin

         --for Each in the_Triangles'range loop
         while has_Element (each_Triangle) loop
            triangle_Count := triangle_Count + 1;

            the_Triangles (triangle_Count) (1) := Element (self.Vertices,  math.Integer (Element (each_Triangle) (1)));
            the_Triangles (triangle_Count) (2) := Element (self.Vertices,  math.Integer (Element (each_Triangle) (2)));
            the_Triangles (triangle_Count) (3) := Element (self.Vertices,  math.Integer (Element (each_Triangle) (3)));

            next (each_Triangle);
         end loop;


         return the_Triangles;
      end;







      function Model (Self : in Modeller.item) return d3.Model_view
      is
         use index_triangle_Sets;
         use vertex_Vectors;
      begin

--           dLog (  "'math.geom.d3.mesh' ~ num Vertices: "  & integer'image (Integer (Length (self.Vertices)))
--                 &                  "     num Triangles: " & integer'image (Integer (Length (self.Triangles))));

         declare
--              the_Data       : Model_view             := new d3.Model (Positive (Length (self.Vertices)),
--                                                                    Positive (Length (self.Triangles)));
            the_Model       : d3.Model_view := new d3.Model (vertex_Count   => Positive (Length (self.Vertices)),
                                                             triangle_Count => Positive (Length (self.Triangles)));

            each_Triangle  : index_triangle_Sets.Cursor := First (self.Triangles);
            triangle_Count : math.Integer               := 0;

         begin

            for Each in the_model.Vertices'range loop
               the_Model.Vertices (Each) := Element (self.Vertices, Each);
            end loop;


            while has_Element (each_Triangle) loop
               triangle_Count                       := triangle_Count + 1;
               the_model.Triangles (triangle_Count) := Element (each_Triangle);

               next (each_Triangle);
            end loop;


            return the_Model;
         end;

      end Model;




   function bounding_sphere_Radius (Self : access Modeller.item) return Number
   is
   begin

      if self.bounding_sphere_Radius = Number'First then
         declare
            use math.geom.d3,  math.geom.d3.vertex_Vectors, math.Functions;

            Cursor     : vertex_Vectors.Cursor := First (self.Vertices);
            the_Vertex : Vertex;
         begin
            while has_Element (Cursor) loop
               the_Vertex                  := Element (Cursor);
               self.bounding_sphere_Radius := Number'Max (self.bounding_sphere_Radius,
                                                          Sqrt (  the_Vertex (1) * the_Vertex (1)
                                                                + the_Vertex (2) * the_Vertex (2)
                                                                + the_Vertex (3) * the_Vertex (3)));

               next (Cursor);
            end loop;
         end;
      end if;

      return self.bounding_sphere_Radius;
   end;





end math.geom.d3.Modeller;

