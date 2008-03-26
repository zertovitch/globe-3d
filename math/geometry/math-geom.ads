


package math.Geom is
   --
   -- the base class for all geometry.

   pragma optimize (Time);


   type Item   is abstract tagged private;
   type View   is access all Item'class;
   type Views  is array (Positive range <>) of View;


   procedure destroy (Self : in out Item)                   is abstract;
   procedure free    (Self : in out View);


   procedure expand (Self : access Item;
                     By   : in     Number)                  is abstract;




private

   type Item  is abstract tagged
      record
         null;
      end record;


   procedure dummy;


end math.Geom;

