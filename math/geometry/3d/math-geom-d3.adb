
with math.geom.d3.Modeller;

with lace.Debug;                           use lace.Debug;

with ada.Strings.unbounded;                use ada.Strings.unbounded;
with ada.unchecked_deallocation;





package body math.Geom.d3 is



   use type math.Number;
   use type math.Integer;




   function Hash (Self : Vertex) return ada.containers.Hash_Type
   is
   begin
      return ada.containers.Hash_type (abs (self (1)) +  abs (self (2)) * 2.0  +  abs (self (3)) * 3.0);
      -- tbd: better hash function
   end;



   function Equivalent (L, R : in Vertex) return Boolean
   is
      Allowance : constant := 0.000_000_1;
   begin
      return     abs (L (1) - R (1))  <  Allowance
        and then abs (L (2) - R (2))  <  Allowance
        and then abs (L (3) - R (3))  <  Allowance;

   end;






   procedure free    (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_deallocation (Item'class, View);
   begin
      if Self /= null then
         destroy (Self.all);
      end if;

      deallocate (Self);
   end;




   function "-" (Left : in Vertex;   Right : in math.Vector_3) return Vertex
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2),
              Left (3) - Right (3));
   end;















   function Hash (Self : in index_Triangle) return ada.containers.Hash_type
   is
   begin
      return ada.containers.Hash_type (self (1)  +  self (2) * 2  +  self (3) * 3);
   end;





   function Equivalent (Left, Right : in index_Triangle)   return Boolean
   is
   begin
      return    Left = Right
        or else (         Left (1) = Right (2)
                 and then Left (2) = Right (3)
                 and then Left (3) = Right (1))

        or else (         Left (1) = Right (3)
                 and then Left (2) = Right (1)
                 and then Left (3) = Right (2));
   end;













--     -- sculpture
--     --
--
--     type private_Sculpture is
--        record
--           Modeller : math.geom.d3.Modeller_full_detail.item;
--        end record;
--
--
--
--
--
--     procedure initialize (Self : in out Sculpture)
--     is
--     begin
--        self.Privvy := new private_Sculpture;
--     end;
--
--
--
--
--
--
--     procedure finalize   (Self : in out Sculpture)
--     is
--        procedure deallocate is new ada.unchecked_deallocation (private_Sculpture, private_Sculpture_view);
--     begin
--        deallocate (self.Privvy);
--     end;
--
--
--
--
--
--
--     procedure add_Triangle (Self : in out Sculpture;   Vertex_1, Vertex_2, Vertex_3 : in Vertex)
--     is
--        use math.geom.d3.Modeller_full_detail;
--     begin
--        add_Triangle (self.privvy.Modeller,  Vertex_1, Vertex_2, Vertex_3);
--     end;
--
--
--
--
--
--
--     function detail_constrained_Model (Self : in Sculpture;   detail_Radius : math.Number := math.Number'small) return Model
--     is
--        use math.geom.d3.Modeller_full_detail;
--     begin
--
--        if detail_Radius <= math.Number'small then   -- ie  is the default
--
--           return self.privvy.Modeller.Model;
--
--        else
--
--           declare
--              package my_Modeller is new d3.Modeller (detail_Radius);   use my_Modeller;
--
--              the_Model : my_Modeller.item;
--           begin
--              the_Model.add (self.privvy.Modeller.Triangles);
--
--              return the_Model.Model;
--           end;
--
--        end if;
--
--     end;











   -- Images
   --


   function Image (Self : in Model) return String
   is
      the_Image : unbounded_String;
   begin
      append (the_Image,  "(");

      for Each in self.Vertices'range loop
         append (the_Image,  math.Image (self.Vertices (Each)));
      end loop;

      append (the_Image,  ") (");


      for Each in self.Triangles'range loop
         append (the_Image,  Image (self.Triangles (Each)));
      end loop;

      append (the_Image,  ")");

      return to_String (the_Image);
   end;






   function Image (Self : in index_Triangle) return String
   is
   begin
      return  "("  &  math.positive'Image (Self (1))  &  ", "
                   &  math.positive'Image (Self (2))  &  ", "
                   &  math.positive'Image (Self (3))  &  ")";
   end;






   function Image (Self : in index_Triangles) return String
   is
      the_Image : unbounded_String;
   begin
      append (the_Image,  "(");

      for Each in Self'range loop
         if Each /= 1 then
            append (the_Image,  ", ");
         end if;

         append (the_Image,  Image (Self (Each)));
      end loop;

      append (the_Image,  ")");


      return  to_String (the_Image);
   end;




end math.Geom.d3;

