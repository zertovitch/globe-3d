
with ada.Numerics;



package body cached_Trigonometry is

   use float_elementary_Functions;
   use type Float_Type;


   sin_Cache : array (0 .. slot_Count - 1) of Float_Type;
   cos_Cache : array (0 .. slot_Count - 1) of Float_Type;

   Pix2            : constant        := 2.0 * ada.numerics.Pi;
   last_slot_Index : constant Float_Type := Float_Type (slot_Count - 1);
   index_Factor    : constant Float_Type := last_slot_Index / pix2;



   --use type math.Integer;


   function cos (Angle : in Float_Type) return Float_Type
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return cos_Cache (the_Index);
   end;





   function sin (Angle : in Float_Type) return Float_Type
   is
      the_Index : standard.Integer := standard.Integer (Angle * index_Factor) mod slot_Count;
   begin
      if the_Index < 0 then
         the_index := the_Index + slot_Count;
      end if;

      return sin_Cache (the_Index);
   end;



   procedure get (Angle : in Float_Type;   the_Cos : out Float_Type;
                                       the_Sin : out Float_Type)
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
      cos_Cache (Each) := cos (  Float_Type (Each) / Float_Type (slot_Count - 1)
                               * Pix2);
   end loop;


   for Each in sin_Cache'Range loop
      sin_Cache (Each) := sin (  Float_Type (Each) / Float_Type (slot_Count - 1)
                               * Pix2);
   end loop;

end cached_Trigonometry;



-- notes:
--
