
with ada.unchecked_deallocation;



package body math.Geom is


   procedure free    (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_deallocation (Item'class, View);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;




   procedure dummy is begin null; end;


end math.Geom;

