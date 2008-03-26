
with math.geom.d3.Modeller_full_detail;
with ada.tags; use ada.tags;



package body math.geom.d3.Capsule is


   use math.geom.d3.Mesh.standard;

   use type math.Number;
   use type math.Integer;




   procedure destroy (Self : in out Item)
   is
   begin
      null;
      --destroy (self.Mesh);
   end;








   -- Capsule model
   --

   function to_Model (Self : access Item;   Options : in geom.d3.model_Options'class := null_Options) return Model_view
--     function to_Model (Self       : access Item;
--                        of_Quality : in     model_Quality := model_Quality'Last) return Model_view
   is
      use math.Functions;

      the_Options : capsule.model_Options;

   begin
      if Options'tag /= null_Options'tag then
         the_Options := capsule.model_Options (Options);
      end if;

      declare
      --capped_cylinder_Quality : constant := 4;
      min_quality_Level       : constant          :=  2;
      max_quality_Level       : constant          := 10;
      quality_Level           : constant Positive := 2; --Positive (the_options.Quality * Number (max_quality_Level - min_quality_Level)) + min_quality_Level;
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
      L                  : Number := self.Length;


      the_Factory :     math.geom.d3.Modeller_full_detail.item;
                    use math.geom.d3.Modeller_full_detail;


   begin

      L  := self.Length * 0.5;
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
         the_Edges (Each).Fore (3) := L;

         the_Edges (Each).Aft (1) := ny * self.Radius;
         the_Edges (Each).Aft (2) := nz * Self.Radius;
         the_Edges (Each).Aft (3) := -L;

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
--                            the_Edges (Each + 1).Aft,
--                            the_Edges (Each)    .Aft);
                          the_Edges (Each)    .Aft,
                          the_Edges (Each + 1).Aft);
            add_Triangle (the_Factory,
                          the_Edges (Each + 1).Aft,
--                            the_Edges (Each)    .Fore,
--                            the_Edges (Each + 1).Fore);
                          the_Edges (Each + 1).Fore,
                          the_Edges (Each)    .Fore);
         else
            add_Triangle (the_Factory,
                          the_Edges (Each)       .Fore,
--                            the_Edges (edges'First).Aft,
--                            the_Edges (Each)       .Aft);
                          the_Edges (Each)       .Aft,
                          the_Edges (edges'First).Aft);
            add_Triangle (the_Factory,
                          the_Edges (edges'First).Aft,
--                            the_Edges (Each)       .Fore,
--                            the_Edges (edges'First).Fore);
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
                     the_arch_Edges (each_Hoop)(Each) (3) := L   + nx2 * self.Radius;

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
--                               the_arch_Edges (1)(Each),
--                               the_Edges (Each + 1).Fore);
                             the_Edges (Each + 1).Fore,
                             the_arch_Edges (1)(Each));
            else
               add_Triangle (the_Factory,
                             the_Edges (Each).Fore,
--                               the_arch_Edges (1)(Each),
--                               the_Edges (1).Fore);
                             the_Edges (1).Fore,
                             the_arch_Edges (1)(Each));
            end if;


            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each + 1).Fore,
--                               the_arch_Edges (1)(Each),
--                               the_arch_Edges (1)(Each + 1));
                             the_arch_Edges (1)(Each + 1),
                             the_arch_Edges (1)(Each));
            else
               add_Triangle (the_Factory,
                             the_Edges (1).Fore,
--                               the_arch_Edges (1)(Each),
--                               the_arch_Edges (1)(1));
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
--                                  the_arch_Edges (each_Hoop + 1)(Each),
--                                  the_arch_Edges (each_Hoop)    (next_hoop_Vertex));
                                the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
                                the_arch_Edges (each_Hoop + 1)(Each));

                  if each_Hoop /= Positive (quality_Level) - 1 then
                     add_Triangle (the_Factory,
                                   the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(Each),
--                                     the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex));
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
                     the_arch_Edges (each_Hoop)(Each) (3) := -L  + nx2 * self.Radius;


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
--                               the_Edges (Each + 1).Aft,
--                               the_arch_Edges (1)(Each));
                             the_arch_Edges (1)(Each),
                             the_Edges (Each + 1).Aft);
            else
               add_Triangle (the_Factory,
                             the_Edges (Each).Aft,
--                               the_Edges (1).Aft,
--                               the_arch_Edges (1)(Each));
                             the_arch_Edges (1)(Each),
                             the_Edges (1).Aft);
            end if;


            if Each /= sides_Count then
               add_Triangle (the_Factory,
                             the_Edges (Each + 1).Aft,
--                               the_arch_Edges (1)(Each + 1),
--                               the_arch_Edges (1)(Each));
                             the_arch_Edges (1)(Each),
                             the_arch_Edges (1)(Each + 1));
            else
               add_Triangle (the_Factory,
                             the_Edges (1).Aft,
--                               the_arch_Edges (1)(1),
--                               the_arch_Edges (1)(Each));
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
--                                  the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                  the_arch_Edges (each_Hoop + 1)(Each));
                                the_arch_Edges (each_Hoop + 1)(Each),
                                the_arch_Edges (each_Hoop)    (next_hoop_Vertex));

                  if each_Hoop /= quality_Level - 1 then
                     add_Triangle (the_Factory,
                                   the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(Each));
                                   the_arch_Edges (each_Hoop + 1)(Each),
                                   the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex));
                  end if;

               end;

            end loop;
         end loop;


      end;


         return the_Factory.Model;
      end;
   end to_Model;





--     procedure rebuild_Mesh (Self : access Item)
--     is
--     begin
--        destroy (self.Mesh);
--
--        self.Mesh.Model_is (to_capsule_Model (self.Length, self.Radius));
--
--        self.Size_has_changed := False;
--     end;







--     function Mesh (Self : access Item) return math.geom.d3.Mesh.view
--     is
--     begin
--        if self.Size_has_changed then
--           rebuild_Mesh (Self);
--        end if;
--
--        return self.Mesh'access;
--     end;





--     function Mesh_Model       (Self : access Item) return math.geom.d3.Mesh.Model_view
--     is
--     begin
--        return self.mesh_Model;
--     end;




--     procedure mesh_Model_is (Self : access Item;
--                              Now  : in math.geom.d3.Mesh.Model_view)
--     is
--     begin
--        self.mesh_Model := Now;
--     end;





   -- class methods
   --


   procedure expand (Self : access Item;
                     By   : in     Number)
   is
   begin
      self.Radius := self.Radius + By;

      if self.Radius < 0.001 then
         self.Radius := 0.0001;
      end if;


      self.Length := self.Length + By;

      if self.Length < 0.001 then
         self.Length := 0.0001;
      end if;

   end;






   -- class attributes
   --



   function Radius (Self : access Item) return Number
   is
   begin
      return self.Radius;
   end;




   procedure Radius_is (Self   : access Item;
                        Now : in     Number)
   is
   begin
      self.Radius           := Now;
      self.Size_has_changed := True;
   end;




   function Length (Self : access Item) return Number
   is
   begin
      return self.Length;
   end;




   procedure Length_is (Self : access Item;
                        Now  : in     Number)
   is
   begin
      self.Length           := Now;
      self.Size_has_changed := True;
   end;





--     function  Quality    (Self : access Item) return quality_Level
--     is
--     begin
--        return self.Quality;
--     end;
--
--
--
--
--     procedure Quality_is (Self : access Item;
--                           Now  : in     quality_Level)
--     is
--     begin
--        self.Quality          := Now;
--        self.Size_has_changed := True;
--     end;







   function bounding_sphere_Radius (Self : access Item) return Number
   is
   begin

      return self.Length / 2.0 + self.Radius;
   end;






   function Volume_of (Self : access Item) return Number
   is
   begin
      return Number (self.Length * self.Radius * self.Radius); -- tbd: replace this rough approximation with proper equation
   end;








end math.geom.d3.Capsule;




-- working but wrong axes
--

--     function to_capsule_Model  (Length : in Number;
--                                 Radius : in Number) return math.geom.d3.mesh.Model
--     is
--        use math.Functions;
--
--        capped_cylinder_Quality : constant := 3;
--        sides_Count             : constant := capped_cylinder_Quality * 4;        -- number of sides to the cylinder (divisible by 4):
--        --  const int n = capped_cylinder_quality*4;
--
--        type Edge is   -- 'barrel' edge
--           record
--              Fore : Vertex;
--              Aft  : Vertex;
--           end record;
--
--        type Edges is array (Positive range 1 .. sides_Count) of Edge;
--
--        the_Edges : Edges;
--
--
--        type arch_Edges is array (Positive range 1 .. capped_cylinder_Quality) of Vertices (1..sides_Count);
--
--
--        tmp,
--        nx, ny, nz,
--        start_nx, start_ny,
--        a, ca, sa          : Number;
--        L                  : Number := Length;
--
--        the_Factory : math.geom.d3.mesh.Factory;
--
--     begin
--
--        L  := Length * 0.5;
--        a  := Pi * 2.0 / Number (sides_Count);
--        sa := sin (a);
--        ca := cos (a);
--
--
--        -- cylinder body
--        --
--        ny := 1.0;
--        nz := 0.0;		  -- normal vector = (0,ny,nz)
--
--        for Each in Edges'range loop
--           the_Edges (Each).Fore.X := ny * radius;
--           the_Edges (Each).Fore.Y := nz * radius;
--           the_Edges (Each).Fore.Z := L;
--
--           the_Edges (Each).Aft.X := ny * radius;
--           the_Edges (Each).Aft.Y := nz * radius;
--           the_Edges (Each).Aft.Z := -L;
--
--  --           put_Line ("Each: " & integer'image (Each) & "   Fore: " & Number'image (the_Edges (Each).Fore.X)
--  --                                                                   & Number'image (the_Edges (Each).Fore.Y)
--  --                                                                   & Number'image (the_Edges (Each).Fore.Z)
--  --                                                     & "   Aft: "  & Number'image (the_Edges (Each).Aft.X)
--  --                                                                   & Number'image (the_Edges (Each).Aft.Y)
--  --                                                                   & Number'image (the_Edges (Each).Aft.Z));
--
--           -- rotate ny, nz
--           tmp := ca * ny  -  sa * nz;
--           nz  := sa * ny  +  ca * nz;
--           ny  := tmp;
--        end loop;
--
--
--
--        for Each in edges'Range loop
--
--           if Each /= edges'Last then
--              add_Triangle (the_Factory,
--                            the_Edges (Each)    .Fore,
--                            the_Edges (Each)    .Aft,
--                            the_Edges (Each + 1).Aft);
--              add_Triangle (the_Factory,
--                            the_Edges (Each + 1).Aft,
--                            the_Edges (Each + 1).Fore,
--                            the_Edges (Each)    .Fore);
--
--           else
--              add_Triangle (the_Factory,
--                            the_Edges (Each)       .Fore,
--                            the_Edges (Each)       .Aft,
--                            the_Edges (edges'First).Aft);
--              add_Triangle (the_Factory,
--                            the_Edges (edges'First).Aft,
--                            the_Edges (edges'First).Fore,
--                            the_Edges (Each)       .Fore);
--           end if;
--
--
--        end loop;
--
--
--
--
--
--
--        -- draw fore cylinder cap
--        --
--        declare
--           the_arch_Edges : arch_Edges;
--        begin
--
--           start_nx := 0.0;
--           start_ny := 1.0;
--
--           for each_Hoop in 1 .. capped_cylinder_Quality loop
--              -- get start_n2 = rotated start_n
--
--              declare
--                 start_nx2 : Number :=  ca * start_nx  +  sa * start_ny;
--                 start_ny2 : Number := -sa * start_nx  +  ca * start_ny;
--              begin
--                 -- get n=start_n and n2=start_n2
--
--                 nx := start_nx;
--                 ny := start_ny;
--                 nz := 0.0;
--
--                 declare
--                    nx2 : Number := start_nx2;
--                    ny2 : Number := start_ny2;
--                    nz2 : Number := 0.0;
--                 begin
--                    for Each in 1 .. sides_Count loop
--                       the_arch_Edges (each_Hoop)(Each).X := ny2 * radius;
--                       the_arch_Edges (each_Hoop)(Each).Y := nz2 * radius;
--                       the_arch_Edges (each_Hoop)(Each).Z := L   + nx2 * Radius;
--
--                       -- rotate n,n2
--
--                       tmp := ca * ny  -  sa * nz;
--                       nz  := sa * ny  +  ca * nz;
--                       ny  := tmp;
--                       tmp := ca * ny2  -  sa * nz2;
--                       nz2 := sa * ny2  +  ca * nz2;
--                       ny2 := tmp;
--
--                    end loop;
--                 end;
--                 start_nx := start_nx2;
--                 start_ny := start_ny2;
--              end;
--
--           end loop;
--
--
--
--           for Each in 1 .. sides_Count loop
--
--              if Each /= sides_Count then
--                 add_Triangle (the_Factory,
--                               the_Edges (Each).Fore,
--                               the_Edges (Each + 1).Fore,
--                               the_arch_Edges (1)(Each));
--              else
--                 add_Triangle (the_Factory,
--                               the_Edges (Each).Fore,
--                               the_Edges (1).Fore,
--                               the_arch_Edges (1)(Each));
--              end if;
--
--
--              if Each /= sides_Count then
--                 add_Triangle (the_Factory,
--                               the_Edges (Each + 1).Fore,
--                               the_arch_Edges (1)(Each + 1),
--                               the_arch_Edges (1)(Each));
--              else
--                 add_Triangle (the_Factory,
--                               the_Edges (1).Fore,
--                               the_arch_Edges (1)(1),
--                               the_arch_Edges (1)(Each));
--              end if;
--
--           end loop;
--
--
--
--           for each_Hoop in 1 .. capped_cylinder_Quality - 1 loop
--              --new_Line;
--
--              for Each in 1 .. sides_Count loop
--  --                 put_Line ("hoop " & integer'image (each_hoop) & "   each: " & integer'image (each)
--  --                             & number'Image (the_arch_Edges (each_Hoop)(Each).X) &
--  --                           number'Image (the_arch_Edges (each_Hoop)(Each).Y) &
--  --                           number'Image (the_arch_Edges (each_Hoop)(Each).Z));
--
--                 declare
--
--                    function next_hoop_Vertex return Positive
--                    is
--                    begin
--                       if Each = sides_Count then return 1;
--                       else                       return Each + 1;
--                       end if;
--                    end;
--
--                 begin
--
--                    add_Triangle (the_Factory,
--                                  the_arch_Edges (each_Hoop)    (Each),
--                                  the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                  the_arch_Edges (each_Hoop + 1)(Each));
--
--                    if each_Hoop /= capped_Cylinder_Quality - 1 then
--                       add_Triangle (the_Factory,
--                                     the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(Each));
--                    end if;
--
--                 end;
--
--              end loop;
--           end loop;
--
--
--        end;
--
--
--
--        -- draw aft cylinder cap
--        --
--        declare
--           the_arch_Edges : arch_Edges;
--        begin
--
--           start_nx := 0.0;
--           start_ny := 1.0;
--
--           for each_Hoop in 1 .. capped_cylinder_Quality loop
--
--              declare
--                 -- get start_n2 = rotated start_n
--                 start_nx2 : Number := ca * start_nx  -  sa * start_ny;
--                 start_ny2 : Number := sa * start_nx  +  ca * start_ny;
--              begin
--                 -- get n=start_n and n2=start_n2
--
--                 nx := start_nx;
--                 ny := start_ny;
--                 nz := 0.0;
--
--                 declare
--                    nx2 : Number := start_nx2;
--                    ny2 : Number := start_ny2;
--                    nz2 : Number := 0.0;
--                 begin
--                    for Each in 1 .. sides_Count loop
--                       the_arch_Edges (each_Hoop)(Each).X := ny2 * radius;
--                       the_arch_Edges (each_Hoop)(Each).Y := nz2 * radius;
--                       the_arch_Edges (each_Hoop)(Each).Z := -L  + nx2 * Radius;
--
--
--                       -- rotate n,n2
--
--                       tmp := ca * ny  -  sa * nz;
--                       nz  := sa * ny  +  ca * nz;
--                       ny  := tmp;
--                       tmp := ca * ny2  -  sa * nz2;
--                       nz2 := sa * ny2  +  ca * nz2;
--                       ny2 := tmp;
--
--                    end loop;
--                 end;
--                 start_nx := start_nx2;
--                 start_ny := start_ny2;
--              end;
--
--           end loop;
--
--
--           for Each in 1 .. sides_Count loop
--
--              if Each /= sides_Count then
--                 add_Triangle (the_Factory,
--                               the_Edges (Each).Aft,
--                               the_arch_Edges (1)(Each),
--                               the_Edges (Each + 1).Aft);
--              else
--                 add_Triangle (the_Factory,
--                               the_Edges (Each).Aft,
--                               the_arch_Edges (1)(Each),
--                               the_Edges (1).Aft);
--              end if;
--
--
--              if Each /= sides_Count then
--                 add_Triangle (the_Factory,
--                               the_Edges (Each + 1).Aft,
--                               the_arch_Edges (1)(Each),
--                               the_arch_Edges (1)(Each + 1));
--              else
--                 add_Triangle (the_Factory,
--                               the_Edges (1).Aft,
--                               the_arch_Edges (1)(Each),
--                               the_arch_Edges (1)(1));
--              end if;
--
--           end loop;
--
--
--
--           for each_Hoop in 1 .. capped_cylinder_Quality - 1 loop
--  --              new_Line;
--
--              for Each in 1 .. sides_Count loop
--  --                 put_Line ("hoop " & integer'image (each_hoop) & "   each: " & integer'image (each)
--  --                             & number'Image (the_arch_Edges (each_Hoop)(Each).X) &
--  --                           number'Image (the_arch_Edges (each_Hoop)(Each).Y) &
--  --                           number'Image (the_arch_Edges (each_Hoop)(Each).Z));
--
--                 declare
--
--                    function next_hoop_Vertex return Positive
--                    is
--                    begin
--                       if Each = sides_Count then return 1;
--                       else                       return Each + 1;
--                       end if;
--                    end;
--
--                 begin
--
--                    add_Triangle (the_Factory,
--                                  the_arch_Edges (each_Hoop)    (Each),
--                                  the_arch_Edges (each_Hoop + 1)(Each),
--                                  the_arch_Edges (each_Hoop)    (next_hoop_Vertex));
--
--                    if each_Hoop /= capped_Cylinder_Quality - 1 then
--                       add_Triangle (the_Factory,
--                                     the_arch_Edges (each_Hoop)    (next_hoop_Vertex),
--                                     the_arch_Edges (each_Hoop + 1)(Each),
--                                     the_arch_Edges (each_Hoop + 1)(next_hoop_Vertex));
--                    end if;
--
--                 end;
--
--              end loop;
--           end loop;
--
--
--        end;
--
--
--  --        put_line  ("total num_vert: " & integer'image (the_vertex));
--  --        put_Line ("total num_tri: "  & integer'image (the_triangle));
--
--
--
--        --flip_YZ (the_Factory);
--
--        return new_Model (the_Factory);
--
--     end;
--
