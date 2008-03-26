
with math.Vector.standard;
with math.geom.d3.Modeller;
with math.geom.d3.Modeller_full_detail;

with lace.Debug;                           use lace.Debug;

with ada.Strings.unbounded;                use ada.Strings.unbounded;
with ada.unchecked_deallocation;






package body math.Geom.d3.Sculpture is






   type opaque_Item is
      record
         Modeller : math.geom.d3.Modeller_full_detail.item;
      end record;





   procedure initialize (Self : in out Item)
   is
   begin
      self.Opaque := new opaque_Item;
   end;






   procedure finalize   (Self : in out Item)
   is
      procedure deallocate is new ada.unchecked_deallocation (opaque_Item, opaque_Item_view);
   begin
      deallocate (self.Opaque);
   end;






   procedure add_Triangle (Self : in out Item;   Vertex_1, Vertex_2, Vertex_3 : in Vertex)
   is
      use math.geom.d3.Modeller_full_detail;
   begin
      add_Triangle (self.opaque.Modeller,  Vertex_1, Vertex_2, Vertex_3);
   end;






   function detail_constrained_Model (Self : in Item;   detail_Radius : math.Number := math.Number'small) return Model
   is
      use math.geom.d3.Modeller_full_detail;
   begin

      if detail_Radius <= math.Number'small then   -- ie  is the default

         return self.opaque.Modeller.Model;

      else

         declare
            package my_Modeller is new d3.Modeller (detail_Radius);   use my_Modeller;

            the_Model : my_Modeller.item;
         begin
            the_Model.add (self.opaque.Modeller.Triangles);

            return the_Model.Model;
         end;

      end if;

   end;






end math.Geom.d3.Sculpture;

