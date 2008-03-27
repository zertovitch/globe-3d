
with ada.unchecked_Conversion;






package body Math is -- provides math.


   use type math.Real;
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

   function Clamped (Self : in Real;   Low  : in Real;
                                         High : in Real) return Real
   is
   begin
      return Real'Max (Low,  Real'Min (Self, High));
   end;




   -- Numbers



   function Image (Self : in Reals) return String
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

         add (Real'Image (Self (Each)));
      end loop;

      add (")");


      return the_Image (1 .. Count);

   exception
      when others =>
         return the_Image (1 .. Count);       -- tbd: refine this ...
   end;









   function to_Numbers (Self : in Real_Block) return Reals
   is
      subtype my_Numbers      is Reals      (1 .. self'Length (1) * self'Length (2));
      subtype my_number_Block is real_Block (self'First (1) .. self'Last (1),   self'First (2) .. self'Last (2));

      function converted_Block is new ada.unchecked_Conversion (my_number_Block, my_Numbers);
   begin
      return converted_Block (Self);
   end;




   function Min (Self : in Real_Block) return Real
   is
      the_Min : Real := Real'Last;
   begin

      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Min := Real'Min (the_Min,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Min;
   end;




   function Max (Self : in Real_Block) return Real
   is
      the_Max : Real := Real'First;
   begin

      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Max := Real'Max (the_Max,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Max;
   end;



   function to_Radians (Degrees : in math.Real) return math.Real
   is
   begin
      return Degrees * Pi / 180.0;
   end;




   function to_Degrees (Radians : in math.Real) return math.Real
   is
   begin
      return Radians * 180.0 / Pi;
   end;








end Math;


-- notes:
--
