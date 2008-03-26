
with ada.unchecked_Conversion;






package body Math is -- provides math.


   use type math.Number;
   use type math.Integer;



   -- Integer

   procedure increment (Self : in out Integer;   By : in Integer := 1)
   is
   begin
      Self := Self + By;
   end;


   procedure decrement (Self : in out Integer;   By : in Integer := 1)
   is
   begin
      Self := Self - By;
   end;



   -- Number
   --

   function Clamped (Self : in Number;   Low  : in Number;
                                         High : in Number) return Number
   is
   begin
      return number'Max (Low,  number'Min (Self, High));
   end;




   -- Numbers



   function Image (Self : in Numbers) return String
   is
      the_Image : String (1 .. 1 * 1024 * 1024);   -- handles one megabtyre string ... excess is truncated
      Count     : standard.Natural := 0;

      procedure add (Text : in String)
      is
      begin
         the_Image (Count + 1 .. Count + text'Length) := Text;
         Count                                        := Count + text'Length;
      end;

   begin
      add ("(");

      for Each in Self'range loop
         if Each /= 1 then
            add (", ");
         end if;

         add (number'Image (Self (Each)));
      end loop;

      add (")");


      return the_Image (1 .. Count);

   exception
      when others =>
         return the_Image (1 .. Count);       -- tbd: refine this ...
   end;









   function to_Numbers (Self : in number_Block) return Numbers
   is
      subtype my_Numbers      is Numbers      (1 .. self'Length (1) * self'Length (2));
      subtype my_number_Block is number_Block (self'First (1) .. self'Last (1),   self'First (2) .. self'Last (2));

      function converted_Block is new ada.unchecked_Conversion (my_number_Block, my_Numbers);
   begin
      return converted_Block (Self);
   end;




   function Min (Self : in number_Block) return Number
   is
      the_Min : Number := Number'Last;
   begin

      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Min := Number'Min (the_Min,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Min;
   end;




   function Max (Self : in number_Block) return Number
   is
      the_Max : Number := Number'First;
   begin

      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Max := Number'Max (the_Max,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Max;
   end;



   function to_Radians (Degrees : in math.Number) return math.Number
   is
   begin
      return Degrees * Pi / 180.0;
   end;




   function to_Degrees (Radians : in math.Number) return math.Number
   is
   begin
      return Radians * 180.0 / Pi;
   end;








end Math;


-- notes:
--
