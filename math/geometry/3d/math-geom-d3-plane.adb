


package body math.geom.d3.Plane is


   use type Number;




   function Volume_of (Self : access Item) return Number
   is
   begin
      return 0.0;
   end;





   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin
      return Number'Last;
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
      null;
   end;




   function Equation (Self : access Item) return math.Numbers
   is
   begin
      return self.Equation;
   end;




   procedure Equation_is (Self : access Item;
                          Now  : in     math.Numbers)
   is
   begin
      self.Equation := Now;
   end;





--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return Model_view
   function to_Model (Self : access Item;   Options : in model_Options'class := null_Options) return Model_view
   is
--        the_Model   : Model_view := new Model (vertex_Count => 8,  triangle_Count => 12);

--        half_Width  : constant Number := self.Sides.data (1) / 2.0;
--        half_Height : constant Number := self.Sides.data (2) / 2.0;
--        half_Depth  : constant Number := self.Sides.data (3) / 2.0;

   begin

--        the_Model.Vertices := (1 => (-half_Width, -half_Height, -half_Depth),   -- front bottom left
--                               2 => ( half_Width, -half_Height, -half_Depth),   -- front bottom right
--                               3 => ( half_Width,  half_Height, -half_Depth),   -- front top    right
--                               4 => (-half_Width,  half_Height, -half_Depth),   -- front top    left
--
--                               5 => ( half_Width,  -half_Height,  half_Depth),   -- rear bottom right
--                               6 => (-half_Width,  -half_Height,  half_Depth),   -- rear bottom left
--                               7 => (-half_Width,   half_Height,  half_Depth),   -- rear top    left
--                               8 => ( half_Width,   half_Height,  half_Depth));  -- rear top    right
--
--        the_Model.Triangles := ((1, 2, 3),  (3, 4, 1),     -- front
--                                (2, 5, 8),  (8, 3, 2),     -- right
--                                (5, 6, 7),  (7, 8, 5),     -- rear
--                                (6, 1, 4),  (4, 7, 6),     -- left
--                                (4, 3, 8),  (8, 7, 4),     -- top
--                                (1, 6, 5),  (5, 2, 1));    -- bottom
--
--        return the_Model;
      raise constraint_Error; -- tbd
      return null;
   end;






end math.geom.d3.Plane;

