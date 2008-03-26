
with math.Angle; use math.Angle;

with lace.Text;  use lace.Text;

with ada.text_IO; use ada.text_IO;
with ada.unchecked_Deallocation;
with ada.Strings.unbounded;        use ada.Strings.unbounded;



package body math.geom.d3.Mesh.standard is


   use math.Angle.functions;
   use type Number;




   procedure add_Triangle (Self                         : access Item;
                           Vertex_1, Vertex_2, Vertex_3 : in     math.geom.d3.Vertex)
   is
   begin
      self.Modeller.add_Triangle (Vertex_1, Vertex_2, Vertex_3);
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
      --free (self.Data);
      null;
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






   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin
      return self.Modeller.bounding_sphere_Radius;
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




   function  Model (Self   : access Item) return d3.Model_view
   is
   begin
      return self.Modeller.Model;
   end;



   procedure add_Model (Self : access Item;
                        Now  : in     d3.Model_view)
   is
   begin
      self.Modeller.add_Model (Now);
   end;



   procedure clear   (Self : access Item)
   is
   begin
      self.Modeller.clear;
   end;





--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return d3.Model_view
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view
   is
   begin
      return self.Modeller.Model;
   end;





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


end math.geom.d3.Mesh.standard;

