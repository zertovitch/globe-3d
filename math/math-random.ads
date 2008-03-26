



package math.Random is



   function random_Number (Lower : in Number := number'First;
                           Upper : in Number := number'Last) return Number;

   function random_Integer (Lower : in Integer := integer'First;
                            Upper : in Integer := integer'Last) return Integer;


   function random_Boolean return Boolean;

end math.Random;


-- notes
--


