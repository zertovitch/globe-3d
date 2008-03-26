
with math.Algebra.linear.d2; use math.Algebra.linear, math.Algebra.linear.d2;



package body math.geom.d2.Polygon is


   use type Number;



   function Vertices (Self : in    Item) return d2.Vertices
   is
   begin
      return self.Vertices.all;
   end;



   function Vertices (Self : access Item'Class) return access d2.Vertices
   is
   begin
      return self.Vertices;
   end;




   procedure transform (Self : in out Item;   Site     : in math.Vector_2 := (0.0, 0.0);
                                              Attitude : in math.Number   := 0.0        )
   is
      the_Rotation : access constant Matrix_2x2 := to_Rotation (Attitude);
   begin
      for Each in self.Vertices'range loop
         self.Vertices (Each) := mul (the_Rotation, self.Vertices (Each)'access);
         self.Vertices (Each) := self.Vertices (Each) + Site;
      end loop;
   end;





--     function bounding_sphere_Radius (Self : access Item) return Number
--     is
--        Corner : Vector := Vector (self.Sides) / 2.0;
--     begin
--        return Norm (Corner);
--     end;





   function Area (Self : in Item) return Number
   is
   begin
      raise constraint_Error; -- tbd:
      return 0.0;
   end;





   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end;






   procedure expand (Self : access Item;
                     By   : in     Number)
   is
   begin
      null; -- tbd:
      raise constraint_Error;
   end;






   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view
   is
      the_Model   : Model_view := new Model (vertex_Count => 8,  triangle_Count => 12);

--        half_Width  : constant Number := self.Sides (1) / 2.0;
--        half_Height : constant Number := self.Sides (2) / 2.0;
--
   begin

--        the_Model.Vertices := (1 => (-half_Width, -half_Height, half_Depth),   -- front bottom left
--                               2 => ( half_Width, -half_Height, half_Depth),   -- front bottom right
--                               3 => ( half_Width,  half_Height, half_Depth),   -- front top    right
--                               4 => (-half_Width,  half_Height, half_Depth),   -- front top    left
--
--                               5 => ( half_Width,  -half_Height,  -half_Depth),   -- rear bottom right
--                               6 => (-half_Width,  -half_Height,  -half_Depth),   -- rear bottom left
--                               7 => (-half_Width,   half_Height,  -half_Depth),   -- rear top    left
--                               8 => ( half_Width,   half_Height,  -half_Depth));  -- rear top    right
--
--        the_Model.Triangles := ((1, 2, 3),  (3, 4, 1),     -- front
--                                (2, 5, 8),  (8, 3, 2),     -- right
--                                (5, 6, 7),  (7, 8, 5),     -- rear
--                                (6, 1, 4),  (4, 7, 6),     -- left
--                                (4, 3, 8),  (8, 7, 4),     -- top
--                                (1, 6, 5),  (5, 2, 1));    -- bottom
--
--  --        the_Model.Triangles := ((1, 3, 2),  (3, 1, 4),     -- front
--  --                                (2, 8, 5),  (8, 2, 3),     -- right
--  --                                (5, 7, 6),  (7, 5, 8),     -- rear
--  --                                (6, 4, 1),  (4, 6, 7),     -- left
--  --                                (4, 8, 3),  (8, 4, 7),     -- top
--  --                                (1, 5, 6),  (5, 1, 2));    -- bottom

      raise program_error; -- tbd
      return the_Model;
   end;






end math.geom.d2.Polygon;

