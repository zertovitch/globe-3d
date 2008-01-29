
with Ada_Containers_Generic_Constrained_Array_Sort;

procedure Ada_Containers_Generic_Array_Sort
  (Container : in out Array_Type)
is
   subtype Index_Subtype is
     Index_Type range Container'First .. Container'Last;

   subtype Array_Subtype is
     Array_Type (Index_Subtype);

   procedure Sort is
      new Ada_Containers_Generic_Constrained_Array_Sort
       (Index_Type   => Index_Subtype,
        Element_Type => Element_Type,
        Array_Type   => Array_Subtype,
        "<"          => "<");

begin
   Sort (Container);
end Ada_Containers_Generic_Array_Sort;
