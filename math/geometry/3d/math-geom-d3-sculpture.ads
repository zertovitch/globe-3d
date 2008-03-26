
with math.Vector;
--with math.Matrix;

with ada.containers.Vectors;
with ada.containers.hashed_Maps;
with ada.containers.hashed_Sets;
with ada.containers.ordered_Maps;

with ada.unchecked_Deallocation;
with ada.finalization;




package math.Geom.d3.Sculpture is


   type Item is tagged private;




   procedure add_Triangle (Self : in out Item;   Vertex_1, Vertex_2, Vertex_3 : in Vertex);
   --
   -- adds a triangle based on vertices.


   function detail_constrained_Model (Self : in     Item;   detail_Radius : math.Number := math.Number'small) return Model;
   --
   -- returns a mesh model from the Factory, based on any triangles already added, with vertices constrained to the given detail_Radius.








private



   type opaque_Item;
   type opaque_Item_view is access all opaque_Item;

   type Item is new ada.finalization.Controlled with
      record
         Opaque : opaque_Item_view;
      end record;



   procedure initialize (Self : in out Item);
   procedure finalize   (Self : in out Item);



end math.Geom.d3.Sculpture;

