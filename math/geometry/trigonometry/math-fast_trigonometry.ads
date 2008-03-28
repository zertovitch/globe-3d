
with cached_Trigonometry;

generic
package math.fast_Trigonometry is

   --package default is new cached_Trigonometry (slot_count => 10_000);   -- tbd: make a generic parameter of 'Math' ?
   package default is new cached_Trigonometry (Real, Functions, 10_000);

end math.fast_Trigonometry;


-- notes:
--
