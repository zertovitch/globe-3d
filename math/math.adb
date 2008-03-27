
with ada.unchecked_Conversion;



package body Math is


   use type math.Real;
   use type math.Integer;



   -- Integers

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




   -- Reals
   --

   function Clamped (Self : in Real;   Low  : in Real;
                                         High : in Real) return Real
   is
   begin
      return Real'Max (Low,  Real'Min (Self, High));
   end;




   -- Angles
   --

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
