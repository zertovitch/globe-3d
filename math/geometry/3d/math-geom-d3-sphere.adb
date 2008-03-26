
with math.geom.d3.Modeller_full_detail;
with lace.Debug;                         use lace.Debug;




package body math.geom.d3.Sphere is


   use type math.Number;
   use type math.Integer;



   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end;



   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin
      return self.Radius;
   end;




   function Volume_of (Self : access Item) return Number
   is
   begin
      return (4.0 / 3.0) * Pi * Number (self.Radius * self.Radius * self.Radius);  --  (4/3)*pi*r^3
   end;





   procedure expand (Self : access Item;
                     By   : in     Number)
   is
   begin
      self.Radius := self.Radius + By;

      if self.Radius < 0.001 then
         self.Radius := 0.0001;
      end if;
   end;




   function Radius (Self : access Item) return Number
   is
   begin
      return self.Radius;
   end;



   procedure Radius_is (Self : access Item;
                        Now  : in     Number)
   is
   begin
      self.Radius := Now;
   end;





--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return Model_view
   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view
   is
      use math.Functions;

      --the_Options : sphere.model_Options := sphere.model_Options (Options);

      --capped_cylinder_Quality : constant := 4;
      min_quality_Level       : constant          :=   2;
      max_quality_Level       : constant          := 10;
      quality_Level           : constant Positive := 2; --4; --Positive (of_Quality * Number (max_quality_Level - min_quality_Level)) + min_quality_Level;
      sides_Count             : constant Positive := Positive (quality_Level * 4);        -- number of sides to the cylinder (divisible by 4):
      --  const int n = capped_cylinder_quality*4;

      type Edge is   -- 'barrel' edge
         record
            Fore : Vertex;
            Aft  : Vertex;
         end record;

      type Edges is array (Positive range 1 .. sides_Count) of Edge;

      the_Edges : Edges;


      type arch_Edges is array (Positive range 1 .. Positive (quality_Level)) of Vertices (1..sides_Count);


      tmp,
      nx, ny, nz,
      start_nx, start_ny,
      a, ca, sa          : Number;
      --L                  : Number := self.Length;


      the_Factory :     math.geom.d3.Modeller_full_detail.item;
                    use math.geom.d3.Modeller_full_detail;


   begin

      --L  := self.Length * 0.5;
      a  := Pi * 2.0 / Number (sides_Count);
      sa := sin (a);
      ca := cos (a);


      -- cylinder body
      --
      ny := 1.0;
      nz := 0.0;		  -- normal vector = (0,ny,nz)

      for Each in Edges'range loop
         the_Edges (Each).Fore (1) := ny * self.Radius;
         the_Edges (Each).Fore (2) := nz * self.Radius;
         the_Edges (Each).Fore (3) := 0.0;

         the_Edges (Each).Aft (1) := ny * self.Radius;
         the_Edges (Each).Aft (2) := nz * Self.Radius;
         the_Edges (Each).Aft (3) := 0.0;

--           put_Line ("Each: " & integer'image (Each) & "   Fore: " & Number'image (the_Edges (Each).Fore.X)
--                                                                   & Number'image (the_Edges (Each).Fore.Y)
--                                                                   & Number'image (the_Edges (Each).Fore.Z)
--                                                     & "   Aft: "  & Number'image (the_Edges (Each).Aft.X)
--                                                                   & Number'image (the_Edges (Each).Aft.Y)
--                                                                   & Number'image (the_Edges (Each).Aft.Z));

         -- rotate ny, nz
         tmp := ca * ny  -  sa * nz;
         nz  := sa * ny  +  ca * nz;
         ny  := tmp;
      end loop;



      for Each in edges'Range loop

         if Each /= edges'Last then
            add_Triangle (the_Factory,
                          the_Edges (Each)    .Fore,
                          the_Edges (Each)    .Aft,
                          the_Edges (Each + 1).Aft);
            add_Triangle (the_Factory,
                          the_Edges (Each + 1).Aft,
                          the_Edges (Each + 1).Fore,
                          the_Edges (Each)    .Fore);

         else
            add_Triangle (the_Factory,
                          the_Edges (Each)       .Fore,
                          the_Edges (Each)       .Aft,
                          the_Edges (edges'First).Aft);
            add_Triangle (the_Factory,
                          the_Edges (edges'First).Aft,
                          the_Edges (edges'First).Fore,
                          the_Edges (Each)       .Fore);
         end if;


      end loop;






      -- draw fore cylinder cap
      --
      declare
         the_arch_Edges : arch_Edges;
      begin

         start_nx := 0.0;
         start_ny := 1.0;

         for each_Hoop in 1 .. Positive (quality_Level) loop
            -- get start_n2 = rotated start_n

            declare
               start_nx2 : Number :=  ca * start_nx  +  sa * start_ny;
               start_ny2 : Number := -sa * start_nx  +  ca * start_ny;
            begin
               -- get n=start_n and n2=start_n2

               nx := start_nx;
               ny := start_ny;
               nz := 0.0;

               declare
                  nx2 : Number := start_nx2;
                  ny2 : Number := start_ny2;
                  nz2 : Number := 0.0;
               begin
                  for Each in 1 .. sides_Count loop
                     the_arch_Edges (each_Hoop)(Each) (1) := ny2 * self.Radius;
                     the_arch_Edges (each_Hoop)(Each) (2) := nz2 * self.Radius;
                     the_arch_Edges (each_Hoop)(Each) (3) := nx2 * self.Radius;
                     --the_arch_Edges (each_Hoop)(Each) (3) := L   + nx2 * self.Radius;

                     -- rotate n,n2

                     tmp := ca * ny  -  sa * nz;
                     nz  := sa * ny  +  ca * nz;
                     ny  := tmp;
                     tmp := ca * ny2  -  sa * nz2;
                     nz2 := sa * ny2  +  ca * nz2;
                     ny2 := tmp;

                  end loop;
               end;
               start_nx := start_nx2;
               start_ny := start_ny2;
            end;

         end loop;



         for Each in 1 .. sides_Count loop

            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each).Fore,
                             the_Edges (Each + 1).Fore,
                             the_arch_Edges (1)(Each));
            else
               add_Triangle (the_Factory,
                             the_Edges (Each).Fore,
                             the_Edges (1).Fore,
                             the_arch_Edges (1)(Each));
            end if;


            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each + 1).Fore,
                             the_arch_Edges (1)(Each + 1),
                             the_arch_Edges (1)(Each));
            else
               add_Triangle (the_Factory,
                             the_Edges (1).Fore,
                             the_arch_Edges (1)(1),
                             the_arch_Edges (1)(Each));
            end if;

         end loop;



         for each_Hoop in 1 .. Positive (quality_Level) - 1 loop
            --new_Line;

            for Each in 1 .. sides_Count loop
--                 put_Line ("hoop " & integer'image (each_hoop) & "   each: " & integer'image (each)
--                             & number'Image (the_arch_Edges (each_Hoop)(Each).X) &
--                           number'Image (the_arch_Edges (each_Hoop)(Each).Y) &
--                           number'Image (the_arch_Edges (each_Hoop)(Each).Z));

               declare

                  function next_hoop_Vertex return Positive
                  is
                  begin
                     if Each = sides_Count then return 1;
                     else                       return Each + 1;
                     end if;
                  end;

               begin

                  add_Triangle (the_Factory,
                                the_arch_Edges (each_Hoop)    (Each),
                                the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                the_arch_Edges (each_Hoop + 1)(Each));

                  if each_Hoop /= Positive (quality_Level) - 1 then
                     add_Triangle (the_Factory,
                                   the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                   the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex),
                                   the_arch_Edges (each_Hoop + 1)(Each));
                  end if;

               end;

            end loop;
         end loop;


      end;



      -- draw aft cylinder cap
      --
      declare
         the_arch_Edges : arch_Edges;
      begin

         start_nx := 0.0;
         start_ny := 1.0;

         for each_Hoop in 1 .. Positive (quality_Level) loop

            declare
               -- get start_n2 = rotated start_n
               start_nx2 : Number := ca * start_nx  -  sa * start_ny;
               start_ny2 : Number := sa * start_nx  +  ca * start_ny;
            begin
               -- get n=start_n and n2=start_n2

               nx := start_nx;
               ny := start_ny;
               nz := 0.0;

               declare
                  nx2 : Number := start_nx2;
                  ny2 : Number := start_ny2;
                  nz2 : Number := 0.0;
               begin
                  for Each in 1 .. sides_Count loop
                     the_arch_Edges (each_Hoop)(Each) (1) := ny2 * self.Radius;
                     the_arch_Edges (each_Hoop)(Each) (2) := nz2 * self.Radius;
                     the_arch_Edges (each_Hoop)(Each) (3) := nx2 * self.Radius;
                     --the_arch_Edges (each_Hoop)(Each) (3) := -L  + nx2 * self.Radius;


                     -- rotate n,n2

                     tmp := ca * ny  -  sa * nz;
                     nz  := sa * ny  +  ca * nz;
                     ny  := tmp;
                     tmp := ca * ny2  -  sa * nz2;
                     nz2 := sa * ny2  +  ca * nz2;
                     ny2 := tmp;

                  end loop;
               end;
               start_nx := start_nx2;
               start_ny := start_ny2;
            end;

         end loop;


         for Each in 1 .. sides_Count loop

            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each).Aft,
                             the_arch_Edges (1)(Each),
                             the_Edges (Each + 1).Aft);
            else
               add_Triangle (the_Factory,
                             the_Edges (Each).Aft,
                             the_arch_Edges (1)(Each),
                             the_Edges (1).Aft);
            end if;


            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each + 1).Aft,
                             the_arch_Edges (1)(Each),
                             the_arch_Edges (1)(Each + 1));
            else
               add_Triangle (the_Factory,
                             the_Edges (1).Aft,
                             the_arch_Edges (1)(Each),
                             the_arch_Edges (1)(1));
            end if;

         end loop;



         for each_Hoop in 1 .. Positive (quality_Level) - 1 loop
--              new_Line;

            for Each in 1 .. sides_Count loop
--                 put_Line ("hoop " & integer'image (each_hoop) & "   each: " & integer'image (each)
--                             & number'Image (the_arch_Edges (each_Hoop)(Each).X) &
--                           number'Image (the_arch_Edges (each_Hoop)(Each).Y) &
--                           number'Image (the_arch_Edges (each_Hoop)(Each).Z));

               declare

                  function next_hoop_Vertex return Positive
                  is
                  begin
                     if Each = sides_Count then return 1;
                     else                       return Each + 1;
                     end if;
                  end;

               begin

                  add_Triangle (the_Factory,
                                the_arch_Edges (each_Hoop)    (Each),
                                the_arch_Edges (each_Hoop + 1)(Each),
                                the_arch_Edges (each_Hoop)    (next_hoop_Vertex));

                  if each_Hoop /= quality_Level - 1 then
                     add_Triangle (the_Factory,
                                   the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                   the_arch_Edges (each_Hoop + 1)(Each),
                                   the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex));
                  end if;

               end;

            end loop;
         end loop;


      end;


      return the_Factory.Model;
   end;



end math.geom.d3.Sphere;

