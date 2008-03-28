
with Ada.Numerics.Generic_Elementary_Functions;



generic
   type Float_Type is digits <>;
   with package float_elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type);

   slot_Count : standard.Positive;

package cached_Trigonometry is
   --
   -- tbd: doco


   pragma optimize (Time);


   function cos (Angle : in Float_Type) return Float_Type;
   function sin (Angle : in Float_Type) return Float_Type;


   procedure get (Angle : in Float_Type;   the_Cos : out Float_Type;
                                       the_Sin : out Float_Type);

   -- tbd: tan, arccos, etc ...


private

   pragma inline_always (cos);
   pragma inline_always (sin);
   pragma inline_always (get);

end cached_Trigonometry;





-- notes:
--
