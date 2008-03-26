

package body math.cached_Rotation is

   use math.Functions;
   use type math.Number;


   the_Cache : array (0 .. slot_Count - 1) of aliased Matrix_2x2;

   Pix2            : constant        := 2.0 * Pi;
   last_slot_Index : constant Number := Number (slot_Count - 1);
   index_Factor    : constant Number := last_slot_Index / Pix2;



   use type math.Integer;


   function to_Rotation (Angle : in Number) return access constant Matrix_2x2
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
         Angle : constant Number := (  Number (Each) / Number (slot_Count - 1)
                                     * Pix2);
         C : constant Number := cos (Angle);
         S : constant Number := sin (Angle);
      begin
         the_Cache (Each) := ((C, -S),
                              (S,  C));
      end;
   end loop;

end math.cached_Rotation;



-- notes:
--
