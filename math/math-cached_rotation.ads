

generic
   slot_Count : standard.Positive;

package math.cached_Rotation is

   pragma optimize (Time);


   function to_Rotation (Angle : in Number) return access constant Matrix_2x2;




private

   pragma inline_always (to_Rotation);

end math.cached_Rotation;





-- notes:
--
