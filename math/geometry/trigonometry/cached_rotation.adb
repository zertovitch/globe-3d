
with ada.Numerics;   use ada.Numerics;



package body cached_Rotation is

   use float_elementary_Functions;
   use type Float_type;


   the_Cache : array (0 .. slot_Count - 1) of aliased Matrix_2x2_type;

   Pix2            : constant        := 2.0 * Pi;
   last_slot_Index : constant Float_type := Float_type (slot_Count - 1);
   index_Factor    : constant Float_type := last_slot_Index / Pix2;



--   use type math.Integer;


   function to_Rotation (Angle : in Float_type) return access constant Matrix_2x2_type
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return the_Cache (the_Index)'access;
   end;





begin

   for Each in the_Cache'Range loop
      declare
         Angle : constant Float_type := (  Float_type (Each) / Float_type (slot_Count - 1)
                                         * Pix2);
         C : constant Float_type := cos (Angle);
         S : constant Float_type := sin (Angle);
      begin
         the_Cache (Each) := to_Matrix_2x2 (C, -S,
                                            S,  C);
      end;
   end loop;

end cached_Rotation;



-- notes:
--
