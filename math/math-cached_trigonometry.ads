

generic
   slot_Count : standard.Positive;

package math.cached_Trigonometry is

   pragma optimize (Time);


   function cos (Angle : in Number) return Number;
   function sin (Angle : in Number) return Number;


   procedure get (Angle : in Number;   the_Cos : out Number;
                                       the_Sin : out Number);

   -- tbd: tan


private

   pragma inline_always (cos);
   pragma inline_always (sin);
   pragma inline_always (get);

end math.cached_Trigonometry;





-- notes:
--
