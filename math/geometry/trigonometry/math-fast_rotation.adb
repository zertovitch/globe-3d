


package body math.fast_Rotation is



   function to_Matrix_2x2 (m11, m12,
                           m21, m22 : Real) return Matrix_2x2
   is
   begin
      return (1 => (m11, m12),
              2 => (m21, m22));
   end;



   package the_Cache is new cached_Rotation (Real,
                                             math.Functions,
                                             Integer, vector_Base,
                                             --Matrix_type,
                                             Matrix,
                                             Matrix_2x2,
                                             to_Matrix_2x2,
                                             10_000);          -- tbd: make slot_Count a generic param of 'math' package.


   function to_Rotation (Angle : in Float_Type) return access constant Matrix_2x2
   is
   begin
      return the_Cache.to_Rotation (Angle);
   end;



end math.fast_Rotation;



-- notes:
--
