
-- This package is almost identical to the Ada05 Ada.Containers.Generic_Constrained_Array_Sort, from GNAT.
-- Will eventually be replaced by formal Ada.Containers.Generic_Constrained_Array_Sort, when available on all Ada compilers.

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Array_Type is array (Index_Type) of Element_Type;

   with function "<" (Left, Right : Element_Type)
     return Boolean is <>;

procedure Ada_Containers_Generic_Constrained_Array_Sort
  (Container : in out Array_Type);

pragma Pure (Ada_Containers_Generic_Constrained_Array_Sort);
