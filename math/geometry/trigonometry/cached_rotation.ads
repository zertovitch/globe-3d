
with Ada.Numerics.Generic_Elementary_Functions;



generic
   type Float_Type   is digits <>;
   with package float_elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type);

   type Index_Type   is range <>;
   vector_Base : Index_type;           -- specifiy '0' or '1' based indexing

   type Matrix_type     is array (Index_Type range <>,  Index_Type range <>) of aliased Float_Type'Base;
   type Matrix_2x2_type is private;
   with function to_Matrix_2x2 (m11, m12,
                                m21, m22 : Float_type) return Matrix_2x2_type;


   slot_Count : standard.Positive;

package cached_Rotation is
   --
   -- tbd: doco

   pragma optimize (Time);

--     subtype Matrix_2x2 is Matrix_type (Index_Type range vector_Base .. vector_Base + 1,
--                                        Index_Type range vector_Base .. vector_Base + 1);

   function to_Rotation (Angle : in Float_Type) return access constant Matrix_2x2_type;




private

   pragma inline_always (to_Rotation);

end cached_Rotation;





-- notes:
--
