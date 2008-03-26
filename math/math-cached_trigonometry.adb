

package body math.cached_Trigonometry is

   use math.Functions;
   use type math.Number;


   sin_Cache : array (0 .. slot_Count - 1) of Number;
   cos_Cache : array (0 .. slot_Count - 1) of Number;

   Pix2            : constant        := 2.0 * Pi;
   last_slot_Index : constant Number := Number (slot_Count - 1);
   index_Factor    : constant Number := last_slot_Index / pix2;



   use type math.Integer;


   function cos (Angle : in Number) return Number
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return cos_Cache (the_Index);
   end;





   function sin (Angle : in Number) return Number
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return sin_Cache (the_Index);
   end;



   procedure get (Angle : in Number;   the_Cos : out Number;
                                       the_Sin : out Number)
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      the_Sin := sin_Cache (the_Index);
      the_Cos := cos_Cache (the_Index);
   end;




   -- tbd: tan



begin

   for Each in cos_Cache'Range loop
      cos_Cache (Each) := math.functions.cos (  Number (Each) / Number (slot_Count - 1)
                                              * Pix2);
   end loop;


   for Each in sin_Cache'Range loop
      sin_Cache (Each) := math.functions.sin (  Number (Each) / Number (slot_Count - 1)
                                              * Pix2);
   end loop;

end math.cached_Trigonometry;



-- notes:
--
