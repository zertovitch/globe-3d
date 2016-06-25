with
     Ada.Text_IO,
     Ada.Unchecked_Conversion;
use
      Ada.Text_IO;

package body GLOBE_3D.tri_Mesh is

   procedure dummy is begin null; end;

   -- 'vertex_cache_optimise' is based on algorithm descibed here ... http://home.comcast.net/~tom_forsyth/papers/fast_vert_cache_opt.html
   --
   procedure vertex_cache_optimise (Vertices : in out GL.Geometry.Vertex_array;   Indices : in out GL.Geometry.vertex_Id_array)
   is
      use GL, GL.Geometry;

      --subtype vertex_Id   is Positive;
      subtype triangle_Id is positive_uInt;

      type triangle_Indices is array (Positive range <>) of triangle_Id;

      function Indices_Index (the_Face : in positive_uInt;   the_Vertex : in positive_uInt) return positive_uInt
      is
      begin
         return 3 * (the_Face - 1)  + the_Vertex;
      end;

      function face_vertex_Id (the_Face : in positive_uInt;   the_Vertex : in positive_uInt) return GL.Geometry.vertex_Id
      is
      begin
         return Indices (Indices_Index (the_Face, the_Vertex));
      end;

      Max_triangles_per_vertex : constant := 150;  -- tbd: what is a sensible size here ?
      MaxSizeVertexCache       : constant := 35;

      type vco_Vertex is
         record
            cache_Position : Integer   := -1;                                    -- Its position in the modelled cache (-1 if it is not in the cache)
            Score          : GL.Double;                                         -- Its current score

            Triangles         : triangle_Indices (1 .. Max_triangles_per_vertex); -- The list of triangle indices that use it, ordered so the triangle indices yet to be added are listed first,
                                                                                  -- followed by the triangle indices that have already been added to the draw list.

            tri_Count         : Natural := 0;                                    -- tbd: should only be needed for debugging
            tri_Count_unadded : Natural;                                         -- The number of triangles not yet added that use it
         end record;

      function Score_of (the_Vertex : in vco_Vertex) return GL.Double
      is
      begin

         if the_Vertex.tri_Count_unadded = 0 then  -- No tri needs this vertex!
            return -1.0;
         end if;

         declare
            use GLOBE_3D.REF;

            CacheDecayPower   : constant := 1.5;
            LastTriScore      : constant := 0.75;
            ValenceBoostScale : constant := 2.0;
            ValenceBoostPower : constant := 0.5;

            Score             : GL.Double      := 0.0;
            cache_Position    : Integer   renames the_Vertex.cache_Position;
         begin

            if cache_Position < 0 then   -- Vertex is not in LRU cache
               null;                     -- so no score.
            else

               if cache_Position < 3 then   -- This vertex was used in the last triangle, so it has a fixed score, whichever of the three
                  Score := LastTriScore;    -- it's in. Otherwise, you can get very different answers depending on whether you add
                                            -- the triangle 1,2,3 or 3,1,2 - which is silly.
               else
                  pragma Assert (cache_Position < MaxSizeVertexCache);

                  declare
                     Scaler : constant := 1.0 / (MaxSizeVertexCache - 3);
                  begin
                     Score := 1.0  -  GL.Double (cache_Position - 3) * Scaler;   -- Points for being high in the cache.
                     Score := Score ** CacheDecayPower;
                  end;
               end if;

            end if;

            declare
            valence_Boost : constant GL.Double := GL.Double (the_Vertex.tri_Count_unadded) ** (-ValenceBoostPower);
            begin
               Score := Score  +  ValenceBoostScale * valence_Boost;   -- Bonus points for having a low number of tris still to
            end;                                                       -- use the vert, so we get rid of lone verts quickly.

            return Score;
         end;
      end Score_of;

      procedure rid_Triangle (in_Vertex    : in out vco_Vertex;
                              the_Triangle : in     triangle_Id)
      is
         triangle_Found : Boolean := False;
      begin
         for Each in 1 .. in_Vertex.tri_Count_unadded loop

            if triangle_Found then
               in_Vertex.Triangles (Each - 1) := in_Vertex.Triangles (Each);

            elsif in_Vertex.Triangles (Each) = the_Triangle then
               triangle_Found := True;
            end if;

         end loop;

         in_Vertex.tri_Count_unadded := in_Vertex.tri_Count_unadded - 1;
      end;

      type vco_Triangle is
         record
            Added      : Boolean  := False;               -- Whether it has been added to the draw list or not
            Score      : GL.Double;                       -- the triangles score (the sum of the scores of its vertices)
         end record;

      type vco_vertex_Array is array (Vertices'Range) of vco_Vertex;
      type access_vco_vertex_Array is access all vco_vertex_Array;

      procedure free is new Ada.Unchecked_Deallocation (vco_vertex_Array, access_vco_vertex_Array);

      num_Faces     : constant positive_uInt := Indices'Length / 3;
      vco_Vertices  : access_vco_vertex_Array                      := new vco_vertex_Array;   -- can be very large, so create in the heap
      vco_Triangles : array (1 .. num_Faces)  of vco_Triangle;

      type LRU_Cache is array (Natural range <>) of GL.Geometry.vertex_Id;

      the_LRU_Cache  : LRU_Cache (0 .. MaxSizeVertexCache - 1);
      LRU_Cache_last : Integer  := -1;

      procedure add_recent_Vertices_to_LRU_Cache (v1, v2, v3 : in GL.Geometry.vertex_Id)
      is
      prior_Cache : constant LRU_Cache := the_LRU_Cache (0 .. LRU_Cache_last);
      begin
         the_LRU_Cache (0) := v1;
         the_LRU_Cache (1) := v2;
         the_LRU_Cache (2) := v3;

         LRU_Cache_last := 2;

         for Each in prior_Cache'Range loop

            if not (        prior_Cache (Each) = v1
                    or else prior_Cache (Each) = v2
                    or else prior_Cache (Each) = v3)
            then
               LRU_Cache_last                 := LRU_Cache_last + 1;
               the_LRU_Cache (LRU_Cache_last) := prior_Cache (Each);
            end if;

         end loop;

      end add_recent_Vertices_to_LRU_Cache;

      function tri_Score_of (triangle_Id : in positive_uInt) return GL.Double
      is
         --  use GL;
         --  the_Triangle : vco_Triangle renames vco_Triangles (triangle_Id);

         Base         : constant positive_uInt := positive_uInt (triangle_Id - 1) * 3;
         v1_Id        : GL.Geometry.vertex_Id renames Indices (Base + 1);
         v2_Id        : GL.Geometry.vertex_Id renames Indices (Base + 2);
         v3_Id        : GL.Geometry.vertex_Id renames Indices (Base + 3);

         Score        : GL.Double;
      begin
         Score :=         vco_Vertices (v1_Id).Score;
         Score := Score + vco_Vertices (v2_Id).Score;
         Score := Score + vco_Vertices (v3_Id).Score;

         return Score;
      end tri_Score_of;

      best_Triangle       : triangle_Id;
      best_Triangle_score : GL.Double  := GL.Double'First;

      new_face_Indices      : GL.Geometry.vertex_Id_array (Indices'Range);    -- the resulting optimised triangle indices.
--        new_face_Indices      : triangle_vertex_Indices (o.Face_Indices'range);    -- the resulting optimised triangle indices.
--  --      new_face_Indices_last : Natural := new_face_Indices'first - 1;
   begin
      --put_Line ("start optimise !");

      -- combined pass's: - increments the counter of the number of triangles that use each vertex
      --                  - adds the triangle to the vertex's triangle list, for each vertex.
      --
      for Each in 1 .. num_Faces loop
         declare
            procedure add_face_Vertex (which_vertex : positive_uInt)
            is
               the_Vertex : vco_Vertex renames vco_Vertices (Indices ((Each - 1) * 3  + which_vertex));
            begin
               the_Vertex.tri_Count                        := the_Vertex.tri_Count + 1;
               the_Vertex.Triangles (the_Vertex.tri_Count) := triangle_Id (Each);

               the_Vertex.tri_Count_unadded                := the_Vertex.tri_Count;
            exception
               when Constraint_Error =>
                  Put_Line ("vco_Triangles max exceeded ... increase Max_triangles_per_vertex !!");
                  raise;
            end;
         begin
            add_face_Vertex (1);
            add_face_Vertex (2);
            add_face_Vertex (3);
         end;
      end loop;

      -- calculate initial vertex scores
      --
      for Each in vco_Vertices'Range loop
         vco_Vertices (Each).Score := Score_of (vco_Vertices (Each));   -- tbd: 'Score_of' function should probably be 'set_Score' procedure ?
      end loop;

      -- calculate initial triangle scores
      --
      for Each in vco_Triangles'Range loop
         vco_Triangles (Each).Score := tri_Score_of (Each);   -- tbd: 'Score_of' function should probably be 'set_Score' procedure ?

         if vco_Triangles (Each).Score > best_Triangle_score then
            best_Triangle       := Each;
            best_Triangle_score := vco_Triangles (Each).Score;
         end if;
      end loop;

      -- re-order all triangle indices.
      --
      for Each in new_face_Indices'Range loop
         declare
         best_Triangle_v1 : constant vertex_Id := face_vertex_Id (best_Triangle, 1);
         best_Triangle_v2 : constant vertex_Id := face_vertex_Id (best_Triangle, 2);
         best_Triangle_v3 : constant vertex_Id := face_vertex_Id (best_Triangle, 3);
         begin
            -- add best triangle to new draw list & remove the best triangle from each of its vertices.
            --
            new_face_Indices (Each)             := Indices (best_Triangle);
            vco_Triangles (best_Triangle).Added := True;

            rid_Triangle (in_Vertex => vco_Vertices (best_Triangle_v1),   the_Triangle => best_Triangle);
            rid_Triangle (in_Vertex => vco_Vertices (best_Triangle_v2),   the_Triangle => best_Triangle);
            rid_Triangle (in_Vertex => vco_Vertices (best_Triangle_v3),   the_Triangle => best_Triangle);

            -- update LRU cache
            --
            add_recent_Vertices_to_LRU_Cache (best_Triangle_v1, best_Triangle_v2, best_Triangle_v3);

            -- update vertex cache position and calculate new score and new scores of the triangles which use the vertex.
            -- also finds new best triangle.
            --
            best_Triangle       := triangle_Id'Last;
            best_Triangle_score := GL.Double'First;

            for Each in 0 .. LRU_Cache_last loop
               declare
                  the_Vertex : vco_Vertex renames vco_Vertices (the_LRU_Cache (Each));
               begin
                  the_Vertex.cache_Position := Each;
                  the_Vertex.Score          := Score_of (the_Vertex);   -- re-score the vertex

                  for Each in 1 .. the_Vertex.tri_Count_unadded loop  -- update all unadded triangle scores, which use this vertex.
                     declare
                        tri_Id : triangle_Id renames the_Vertex.Triangles (Each);
                     begin
                        vco_Triangles (tri_Id).Score := tri_Score_of (tri_Id);   -- re-score the triangle

                        if vco_Triangles (tri_Id).Score > best_Triangle_score then
                           best_Triangle       := tri_Id;
                           best_Triangle_score := vco_Triangles (tri_Id).Score;
                        end if;
                     end;
                  end loop;

               end;
            end loop;

            LRU_Cache_last := Integer'Min (LRU_Cache_last,  (MaxSizeVertexCache - 1) - 3);  -- shrink LRU_Cache, if needed.

            if best_Triangle = triangle_Id'Last then   -- no vertex in the cache has any unadded triangles

               for Each in vco_Triangles'Range loop    -- find new best_Triangle from remaining unadded triangles

                  if         not vco_Triangles (Each).Added
                    and then vco_Triangles (Each).Score > best_Triangle_score
                  then
                     best_Triangle       := Each;
                     best_Triangle_score := vco_Triangles (Each).Score;
                  end if;

               end loop;

            end if;

         end;
      end loop;

      pragma Assert (best_Triangle = triangle_Id'Last);

      Indices := new_face_Indices;

      -- re-order vertices & re-map triangle indices to new vertex locations.
      --
      declare
         new_Vertices      : GL.Geometry.Vertex_array (Vertices'Range);
         new_Vertices_last : vertex_Id                                := 0;

         is_Added          : array (Vertices'Range)        of Boolean  := (others => False);
         Mapping           : array (Vertices'Range) of vertex_Id;
      begin

         for Each in 1 .. num_Faces loop
            declare

               procedure add_Vertex (old_vertex_Id : in vertex_Id)
               is
               begin
                  if not is_Added (old_vertex_Id) then
                     new_Vertices_last                := new_Vertices_last + 1;
                     new_Vertices (new_Vertices_last) := Vertices (old_vertex_Id);    -- add the vertex

                     Mapping  (old_vertex_Id)         := new_Vertices_last;          -- remember mapping
                     is_Added (old_vertex_Id)         := True;
                  end if;
               end add_Vertex;

            begin
               add_Vertex (face_vertex_Id (Each, 1));
               add_Vertex (face_vertex_Id (Each, 2));
               add_Vertex (face_vertex_Id (Each, 3));
            end;
         end loop;

         Vertices := new_Vertices;                                                 -- let Object use the new re-ordered vertices.

         for Each in 1 .. num_Faces loop
            Indices (Indices_Index (Each, 1)) := Mapping (Indices (Indices_Index (Each, 1)));       -- re-map each triangles Indices.
            Indices (Indices_Index (Each, 2)) := Mapping (Indices (Indices_Index (Each, 2)));
            Indices (Indices_Index (Each, 3)) := Mapping (Indices (Indices_Index (Each, 3)));
         end loop;

      end;

      free (vco_Vertices);        -- clean up
   end vertex_cache_optimise;

   pragma Unreferenced (vertex_cache_optimise);

end GLOBE_3D.tri_Mesh;
