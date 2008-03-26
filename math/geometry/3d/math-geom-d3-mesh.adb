
with math.Angle;                   use math.Angle;

with lace.Text;                    use lace.Text;

with ada.text_IO;                  use ada.text_IO;
with ada.unchecked_Deallocation;
with ada.Strings.unbounded;        use ada.Strings.unbounded;




package body math.geom.d3.Mesh is


   use math.Angle.functions;

   use type math.Number;
   use type math.Integer;







   procedure destroy (Self : in out Item)
   is
   begin
      null;
      --free (self.Data);
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




--     function  Data (Self   : access Item) return Data_view
--     is
--     begin
--        return self.Data;
--     end;



--     procedure Data_is (Self : access Item;
--                         Now  : in     Data_view)
--     is
--     begin
--        self.Data := Now;
--     end;


   procedure Model_is  (Self : access Item'Class;   Now  : in     d3.Model_view)
   is
   begin
      self.clear;
      self.add_Model (Now);
   end;













   -- heightmap mesh (old + dodgy)
   --

--     procedure add_Heightmap (Self          : access Item'Class;
--                              the_heightmap : in     math.Matrix)
--     is
--        the_Heights : math.number_Block   :=  the_Heightmap.Data;
--
--        Height_min  : math.Number         := math.Number'last;
--        Height_max  : math.Number         := math.Number'first;
--        Midpoint    : math.Vector.standard.item_3;
--
--     begin
--
--        for each_Row in 1 .. the_Heights'last(1) loop
--           for each_Col in 1 .. the_Heights'last(2) loop
--              the_Heights (each_Row, each_Col) := the_Heights (each_Row, each_Col);
--
--              Height_min := math.Number'min (Height_min,  the_Heights (each_Row, each_Col));
--              Height_max := math.Number'max (Height_max,  the_Heights (each_Row, each_Col));
--           end loop;
--        end loop;
--
--
--        Midpoint := to_Vector ((  Number (the_Heights'last(2))  / 2.0,
--                                00.0, --(Height_max - Height_min)     / 2.0,
--                                  Number (the_Heights'last(1))  / 2.0));
--
--
--
--        for each_Row in 1 .. the_Heights'last(1) - 1 loop
--           for each_Col in 1 .. the_Heights'last(2) - 1 loop
--              declare
--                 border_Skirt : Number := 0.003;       -- fudge factor (helps prevents seams which may result from 'coarsen')
--
--                 function to_Row (Index : in Integer) return Number is
--                 begin
--                    if    Index = 1                   then return Number (Index) - border_Skirt;
--                    elsif Index = the_Heights'last(1) then return Number (Index) + border_Skirt;
--                    else                                   return Number (Index);
--                    end if;
--                 end;
--
--                 function to_Col (Index : in Integer) return Number is
--                 begin
--                    if    Index = 1                   then return Number (Index) - border_Skirt;
--                    elsif Index = the_Heights'last(2) then return Number (Index) + border_Skirt;
--                    else                                   return Number (Index);
--                    end if;
--                 end;
--
--                 V1, V2, V3 : Vertex;
--              begin
--                 V1 := Vertex'(to_Col (each_Col),      the_Heights (each_Row,     each_Col),      to_Row (each_Row)    );
--                 V2 := Vertex'(to_Col (each_Col + 1),  the_Heights (each_Row + 1, each_Col + 1),  to_Row (each_Row + 1));
--                 V3 := Vertex'(to_Col (each_Col + 1),  the_Heights (each_Row,     each_Col + 1),  to_Row (each_Row)    );
--
--                 add_Triangle (Self,  V1 - midpoint,
--                                      V2 - midpoint,
--                                      V3 - midpoint);
--
--
--                 V1 := Vertex'(to_Col (each_Col + 1),  the_Heights (each_Row + 1, each_Col + 1),  to_Row (each_Row + 1));
--                 V2 := Vertex'(to_Col (each_Col),      the_Heights (each_Row,     each_Col),      to_Row (each_Row)    );
--                 V3 := Vertex'(to_Col (each_Col),      the_Heights (each_Row + 1, each_Col),      to_Row (each_Row + 1));
--
--                 add_Triangle (Self,  V1 - midpoint,
--                                      V2 - midpoint,
--                                      V3 - midpoint);
--              end;
--           end loop;
--        end loop;
--
--
--
--  --      the_mesh.Data_is (Data (the_Model));
--  --      return the_Mesh;
--     end;










   -- polar model: polar to euclidian shape models.
   --


   use math.Functions;

--   use type Number;



   function to_polar_Model (Model_Filename : in String;
                            Scaled_by      : in Number := 1.0) return polar_Model
   is
      -- tbd: mod to use a factory.

      The_Text      : aliased lace.Text.Instance := Creation (get_String (lace.text.filename (Model_Filename)));

      the_Latitude  : latitude;
      the_Longitude : longitude;

      Lat           : Radians;
      Long          : Radians;

      the_Distance  : Number;

      the_Model     : polar_Model;

   begin

      while not at_End (the_Text) loop

         the_Longitude := longitude (get_Integer (the_Text'access));
         the_Latitude  := latitude  (get_Integer (the_Text'access));

         Lat  := to_Radians (Degrees (the_Latitude));
         Long := to_Radians (Degrees (the_Longitude));

         the_Distance  := Number  (get_Float (the_Text'access)) * Scaled_by;

         eat_white (the_Text);

         the_Model (the_Longitude) (the_Latitude).Vertex (1) := Number (cos (Lat) * sin (Long)) * the_Distance;
         the_Model (the_Longitude) (the_Latitude).Vertex (2) := Number (sin (Lat))              * the_Distance;
         the_Model (the_Longitude) (the_Latitude).Vertex (3) := Number (cos (Lat) * cos (Long)) * the_Distance;

      end loop;


      return the_Model;

   end;






   function to_Model (From : in polar_Model) return d3.Model
   is
      the_raw_model : polar_Model := From;

      the_mesh_Model : d3.Model (vertex_Count   => 2557,
                                 triangle_Count => 73 * (16 * 4 + 6)); -- tbd: replace constants with a generalised solution

      the_longitude  : longitude := 0;
      the_latitude   : latitude ;

      the_Vertex   : Positive := 1;
      the_Triangle : Positive := 1;

      the_North_Pole : Positive;
      the_South_Pole : Positive;

   begin

      the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (0) (-90).Vertex;
      the_North_Pole := the_Vertex;
      the_raw_Model (0) (-90).Id := the_Vertex;
      the_Vertex := the_Vertex + 1;


      the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (0) (90).Vertex;
      the_south_Pole := the_Vertex;
      the_raw_Model (0) (90).Id := the_Vertex;
      the_Vertex := the_Vertex + 1;


      loop

         the_latitude := -90;

         loop

            if the_Latitude = -90 then
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_North_Pole;

            elsif the_Latitude = 90 then
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_South_Pole;
            else
               the_mesh_Model.Vertices (the_Vertex)            :=  the_raw_model (the_Longitude) (the_Latitude).Vertex;
               the_raw_Model (the_Longitude) (the_Latitude).Id := the_Vertex;
               the_Vertex := the_Vertex + 1;
            end if;


            exit when the_Latitude = 90;

            the_Latitude := the_Latitude + 5;

         end loop;


         exit when the_Longitude = 360;

         the_Longitude := the_Longitude + 5;

      end loop;



      the_Longitude := 0;

      loop

         if the_Longitude = 360 then
            the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_North_Pole),
                                                           2 => math.Integer (the_raw_Model (5) (-85).Id),
                                                           3 => math.Integer (the_raw_Model (the_Longitude) (-85).Id));
--                                                      2 => the_raw_Model (the_Longitude) (-85).Id,
--                                                         3 => the_raw_Model (5) (-85).Id);
         else
            the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_North_Pole),
                                                        2 => math.Integer (the_raw_Model (the_Longitude + 5) (-85).Id),
                                                        3 => math.Integer (the_raw_Model (the_Longitude) (-85).Id));
--                                                    2 => the_raw_Model (the_Longitude) (-85).Id,
--                                                    3 => the_raw_Model (the_Longitude + 5) (-85).Id);
         end if;

         the_Triangle := the_Triangle + 1;


         if the_Longitude = 360 then
            the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_South_Pole),
                                                           2 => math.Integer (the_raw_Model (the_Longitude) (85).Id),
                                                           3 => math.Integer (the_raw_Model (5) (85).Id));
--                                                       2 => the_raw_Model (5) (85).Id,
--                                                       3 => the_raw_Model (the_Longitude) (85).Id);
         else
            the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_South_Pole),
                                                           2 => math.Integer (the_raw_Model (the_Longitude) (85).Id),
                                                           3 => math.Integer (the_raw_Model (the_Longitude + 5) (85).Id));
--                                                         2 => the_raw_Model (the_Longitude + 5) (85).Id,
--                                                         3 => the_raw_Model (the_Longitude) (85).Id);
         end if;

         the_Triangle := the_Triangle + 1;



         the_latitude := -85;

         loop

            if the_Longitude = 360 then
               the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude).Id),
                                                           2 => math.Integer (the_raw_Model (5) (the_Latitude).Id),
                                                           3 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude + 5).Id));
--                                                         2 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--                                                         3 => the_raw_Model (5) (the_Latitude).Id);
            else
               the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude).Id),
                                                           2 => math.Integer (the_raw_Model (the_Longitude + 5) (the_Latitude).Id),
                                                           3 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude + 5).Id));
--                                                         2 => the_raw_Model (the_Longitude) (the_Latitude + 5).Id,
--                                                         3 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id);
            end if;


            the_Triangle := the_Triangle + 1;


            if the_Longitude = 360 then
               the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude + 5).Id),
                                                           2 => math.Integer (the_raw_Model (5) (the_Latitude).Id),
                                                           3 => math.Integer (the_raw_Model (5) (the_Latitude + 5).Id));
--                                                         2 => the_raw_Model (5) (the_Latitude + 5).Id,
--                                                         3 => the_raw_Model (5) (the_Latitude).Id);
            else
               the_mesh_Model.Triangles (the_Triangle) :=  (1 => math.Integer (the_raw_Model (the_Longitude) (the_Latitude + 5).Id),
                                                              2 => math.Integer (the_raw_Model (the_Longitude + 5) (the_Latitude).Id),
                                                              3 => math.Integer (the_raw_Model (the_Longitude + 5) (the_Latitude + 5).Id));
--                                                            2 => the_raw_Model (the_Longitude + 5) (the_Latitude + 5).Id,
--                                                            3 => the_raw_Model (the_Longitude + 5) (the_Latitude).Id);
            end if;


            the_Triangle := the_Triangle + 1;


            the_Latitude := the_Latitude + 5;

            exit when the_Latitude = 85;

         end loop;


         exit when the_Longitude = 360;

         the_Longitude := the_Longitude + 5;



      end loop;


--        put_line  ("total num_vert: " & integer'image (the_vertex));
--        put_Line ("total num_tri: "  & integer'image (the_triangle));


      return the_mesh_Model;

   end;








--     function to_Mesh (From : in polar_Model) return Mesh.item
--     is
--     begin
--        return (Data => new mesh.Data_item'(to_mesh_Data (From)));
--     end;







end math.geom.d3.Mesh;

