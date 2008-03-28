
with cached_Rotation;


generic
package math.fast_Rotation is

   function to_Rotation (Angle : in Float_Type) return access constant Matrix_2x2;

private

   pragma inline_always (to_Rotation);


end math.fast_Rotation;



-- notes:
--
