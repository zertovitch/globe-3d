-------------------------------------------------------------------------
--  GL.Geometry - GL geometry primitives
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Math; use GL.Math;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body GL.Geometry is

   package REF is new Ada.Numerics.Generic_Elementary_functions (gl.Double);  -- tbd: make this public ?

   -- Plane
   --

   procedure normalise (the_Plane : in out Plane)
   is
      use REF;
      inv_Magnitude : constant GL.Double := 1.0 / Sqrt (  the_Plane (0) * the_Plane (0)
                                                        + the_Plane (1) * the_Plane (1)
                                                        + the_Plane (2) * the_Plane (2));
   begin
      the_Plane (0) := the_Plane (0) * inv_Magnitude;
      the_Plane (1) := the_Plane (1) * inv_Magnitude;
      the_Plane (2) := the_Plane (2) * inv_Magnitude;
      the_Plane (3) := the_Plane (3) * inv_Magnitude;
   end;

   -- Bounds
   --

   function Max (L, R : in Extent) return Extent
   is
      the_Max : Extent;
   begin
      the_Max.Min := GL.Double'Max (L.Min,  R.Min);
      the_Max.Max := GL.Double'Max (L.Max,  R.Max);

      return the_Max;
   end;

   function Max (L, R : in axis_aligned_bounding_Box) return axis_aligned_bounding_Box
   is
      the_Max : axis_aligned_bounding_Box;
   begin
      the_Max.X_Extent := Max (L.X_Extent,  R.X_Extent);
      the_Max.Y_Extent := Max (L.Y_Extent,  R.Y_Extent);
      the_Max.Z_Extent := Max (L.Z_Extent,  R.Z_Extent);

      return the_Max;
   end;

   function Max (L, R : in Bounds_record) return Bounds_record
   is
      the_Max : Bounds_record := null_Bounds;
   begin
      the_Max.sphere_Radius := GL.Double'Max (L.sphere_Radius, R.sphere_Radius);
      the_Max.Box           :=      Max (L.Box, R.Box);

      return the_Max;
   end;

   -- vertex_Id's
   --

   procedure increment (Self : in out vertex_Id_array)
   is
   begin
      for Each in Self'Range loop
         Self (Each) := Self (Each) + 1;
      end loop;
   end;

   procedure decrement (Self : in out vertex_Id_array)
   is
   begin
      for Each in Self'Range loop
         Self (Each) := Self (Each) - 1;
      end loop;
   end;

   -- vertices
   --

   function Image (Self : in     Vertex) return String
   is
   begin
      return "(" & Double'Image (Self (0)) & Double'Image (Self (1)) & Double'Image (Self (2)) & ")";
   end;

   function Bounds (Self : in     Vertex_array) return GL.Geometry.Bounds_record
   is
      use REF;
      the_Bounds     : Bounds_record := null_Bounds;
      max_Distance_2 : GL.Double     := 0.0;      -- current maximum distance squared.
   begin
      for p in Self'Range loop
         max_Distance_2 := GL.Double'Max (  Self (p)(0) * Self (p)(0)
                                          + Self (p)(1) * Self (p)(1)
                                          + Self (p)(2) * Self (p)(2),
                                          max_Distance_2);

         the_Bounds.Box.X_Extent.Min   := GL.Double'Min (the_Bounds.Box.X_Extent.Min,  Self (p)(0));
         the_Bounds.Box.X_Extent.Max   := GL.Double'Max (the_Bounds.Box.X_Extent.Max,  Self (p)(0));
         the_Bounds.Box.Y_Extent.Min   := GL.Double'Min (the_Bounds.Box.Y_Extent.Min,  Self (p)(1));
         the_Bounds.Box.Y_Extent.Max   := GL.Double'Max (the_Bounds.Box.Y_Extent.Max,  Self (p)(1));
         the_Bounds.Box.Z_Extent.Min   := GL.Double'Min (the_Bounds.Box.Z_Extent.Min,  Self (p)(2));
         the_Bounds.Box.Z_Extent.Max   := GL.Double'Max (the_Bounds.Box.Z_Extent.Max,  Self (p)(2));
      end loop;

      the_Bounds.sphere_Radius := Sqrt (max_Distance_2);

      return the_Bounds;
   end;

   function Bounds (Vertices : in     Vertex_array;   Indices : in   vertex_Id_array) return GL.Geometry.Bounds_record
   is
      use REF;
      the_Bounds     : Bounds_record := null_Bounds;
      max_Distance_2 : GL.Double     := 0.0;      -- current maximum distance squared.
   begin
      for Each in Indices'Range loop
         declare
            the_Point : Vertex renames Vertices (Indices (Each));
         begin
            max_Distance_2 := GL.Double'Max (  the_Point (0) * the_Point (0)
                                             + the_Point (1) * the_Point (1)
                                             + the_Point (2) * the_Point (2),   max_Distance_2);

            the_Bounds.Box.X_Extent.Min := GL.Double'Min (the_Bounds.Box.X_Extent.Min,  the_Point (0));
            the_Bounds.Box.X_Extent.Max := GL.Double'Max (the_Bounds.Box.X_Extent.Max,  the_Point (0));
            the_Bounds.Box.Y_Extent.Min := GL.Double'Min (the_Bounds.Box.Y_Extent.Min,  the_Point (1));
            the_Bounds.Box.Y_Extent.Max := GL.Double'Max (the_Bounds.Box.Y_Extent.Max,  the_Point (1));
            the_Bounds.Box.Z_Extent.Min := GL.Double'Min (the_Bounds.Box.Z_Extent.Min,  the_Point (2));
            the_Bounds.Box.Z_Extent.Max := GL.Double'Max (the_Bounds.Box.Z_Extent.Max,  the_Point (2));
         end;
      end loop;

      the_Bounds.sphere_Radius := Sqrt (max_Distance_2);

      return the_Bounds;
   end;

   function  face_Count    (Self : in     Geometry'Class) return Natural
   is
      the_Count : Natural;
   begin
      case primitive_Id (Self) is
         when POINTS =>
            the_Count := Natural (indices_Count (Self));

         when LINES =>
            the_Count := Natural (indices_Count (Self) / 2);

         when LINE_LOOP =>
            the_Count := Natural (indices_Count (Self));

         when LINE_STRIP =>
            the_Count := Natural'Max (Natural (indices_Count (Self) - 1),  0);

         when TRIANGLES =>
            the_Count := Natural (indices_Count (Self) / 3);

         when TRIANGLE_STRIP =>
            the_Count := Natural'Max (Natural (indices_Count (Self) - 2),  0);

         when TRIANGLE_FAN =>
            the_Count := Natural'Max (Natural (indices_Count (Self) - 2),  0);

         when QUADS =>
            the_Count := Natural (indices_Count (Self) / 4);

         when QUAD_STRIP =>
            the_Count := Natural (indices_Count (Self) / 2  -  1);

         when POLYGON =>
            the_Count := 1;
      end case;

      return the_Count;
   end;

   function Image  (Self : in     Vertex_array) return String
   is
      use Ada.Strings.Unbounded;
      the_Image : Unbounded_String;
      NL        : constant String := (1 => Ada.Characters.Latin_1.LF);   -- NL: New Line
   begin
      Append (the_Image, "(" & NL);
      for Each in Self'Range loop
         Append (the_Image, " " & vertex_Id'Image (Each) & " => " & Image (Self (Each)) & NL);
      end loop;
      Append (the_Image, ")" & NL);

      return To_String (the_Image);
   end;

   -- abstract base geometry class
   --

   procedure free (Self : in out p_Geometry)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Geometry'Class, p_Geometry);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;

   function vertex_Normals (Self : in     Geometry'Class) return Normal_array
   is
   begin
      case primitive_Id (Self) is
         when TRIANGLES =>
            declare
               the_Vertices : Vertex_array    renames Vertices (Self);
               the_Indices  : vertex_Id_array renames Indices (Self);
               the_Normals  : Normal_array (the_Vertices'Range);

               face_Count   : constant Positive         := the_Indices'Length / 3;
               face_Normals : Normals  (1 .. face_Count);

               N            : GL.Double_Vector_3D;
               length_N     : GL.Double;

               function vertex_Id_for (Face : in Positive;   point_Id : in Positive) return vertex_Id
               is
               begin
                  return the_Indices (positive_uInt (3 * (Face - 1) + point_Id));
               end;

            begin
               -- Geometry (Normal of unrotated face)
               --
               for each_Face in 1 .. face_Count loop
                  N        :=   (the_Vertices (vertex_Id_for (each_Face, 2)) - the_Vertices (vertex_Id_for (each_Face, 1)))
                              * (the_Vertices (vertex_Id_for (each_Face, 3)) - the_Vertices (vertex_Id_for (each_Face, 1))) ;
                  length_N := Norm( N );

                  if Almost_zero (length_N) then
                     face_Normals (each_Face) := N; -- 0 vector !
                  else
                     face_Normals (each_Face) := (1.0 / length_N) * N;
                  end if;
               end loop;

               -- Calculate normal at each vertex.
               --
               declare
                  vertex_adjacent_faces_Count : array (the_Vertices'Range) of Natural := (others => 0);
                  the_Vertex                  : vertex_Id;
                  length                      : Double;
               begin

                  for p in the_Vertices'Range loop
                     the_Normals (p):= (0.0, 0.0, 0.0);
                  end loop;

                  for f in 1 .. face_Count loop
                     for p in 1 .. 3 loop
                        the_Vertex := vertex_Id_for (f, p);

                        vertex_adjacent_faces_Count (the_Vertex) := vertex_adjacent_faces_Count (the_Vertex) + 1;
                        the_Normals (the_Vertex)                 := the_Normals (the_Vertex) + face_Normals (f);
                     end loop;
                  end loop;

                  declare
                     use GL.Math.REF;
                     max_Distance_2 : constant Double := 0.0;      -- current maximum distance squared.
                  begin
                     for p in the_Vertices'Range loop

                        length:= Norm (the_Normals (p));

                        if not Almost_zero(length) then
                           the_Normals (p) := (1.0 / length) * the_Normals (p);
                        else
                           null; --raise Constraint_Error;  -- tbd: proper exception as usual.
                        end if;
                     end loop;

                  end;
               end;

               return the_Normals;
            end;

         when others =>
            raise Constraint_Error; -- tbd: finish these
      end case;

      return Normal_array'(1..0 => (others => 0.0));
   end;

end GL.Geometry;
