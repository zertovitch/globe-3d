
with Math;
with interfaces.C;


package c_Math is

   package C renames interfaces.C;

   type c_double_Vector is array (c.Int range <>)                 of aliased c.Double;
   type c_double_Matrix is array (c.Int range <>, c.Int range <>) of aliased c.Double;

   package Core is new Math (c.Int,
                             c.Double,
                             --c.Int,
                             0);
                             --c_double_Vector, c_double_Matrix);  -- nb: '0'-based indexing

end c_Math;


