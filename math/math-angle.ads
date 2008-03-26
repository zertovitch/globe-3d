

with Ada.Numerics.Generic_Elementary_Functions;



package math.Angle is


   pragma pure;



   type Degrees is new Number;
   type Radians is new Number;



   function to_Radians (From : in Degrees) return Radians;


   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Radians);


end math.Angle;


-- notes
--


