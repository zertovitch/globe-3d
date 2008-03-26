



package body math.Angle is

   use type math.Number;
   use type math.Integer;


   function to_Radians (From : in Degrees) return Radians
   is
   begin
     return Radians (Number (From) * Pi / 180.0);
   end to_Radians;



end math.Angle;



-- notes
--

