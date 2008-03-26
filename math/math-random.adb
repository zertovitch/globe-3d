
with ada.numerics.Float_random; use ada.numerics.Float_random;



package body math.Random is


   use type math.Number;
   use type math.Integer;



   the_Generator : ada.numerics.Float_random.Generator;






   function random_Boolean return Boolean
   is
   begin
      return Boolean'Val (random_Integer (0, 1));

   end;







   function random_Number (Lower : in Number := number'First;
                           Upper : in Number := number'Last) return Number
   is
      base_Roll     : Float;
      the_Roll      : Number;
   begin
      base_Roll := ada.numerics.Float_random.Random (the_Generator);
      the_Roll  := Number (base_Roll) * (Upper - Lower) + Lower;

      return the_Roll;
   end;






   function random_Integer (Lower : in integer := integer'First;
                            Upper : in integer := integer'Last) return Integer
   is
   begin
      return Integer (Number'adjacent (random_Number (0.0, 1.0) * (Number (Upper - Lower + 1))  -  0.5,
                                           0.0))
             + Lower;
   end;




begin


   reset (the_Generator);


end math.Random;



-- notes
--

