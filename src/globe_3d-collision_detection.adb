-------------------------------------------------------------------------
--  GLOBE_3D.Collision_detection
--
--  Copyright (c) Gautier de Montmollin 1999 .. 2016
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

-------------------------------------------------------------------------

with GL.Math;

with GLOBE_3D.Options;

-- with Ada.Text_IO;                       use Ada.Text_IO; -- for debugging

package body GLOBE_3D.Collision_detection is

  check_normals: constant Boolean:= GLOBE_3D.Options.strict_geometry;

  procedure Reaction(
    o           : Object_3D'Class;
    ball        : Ball_type;
    method      : Reaction_method;
    step        : in out Vector_3D; -- Whole step (in: desired, out: effective)
    reacted     : out Real          -- in proportion to step
  )
  is
    use GL.Math;
    P_after_step, P_face: Point_3D;
    u,n : Vector_3D;
    dist_after, dist_before, nn: Real; -- distance orientee
    retour: Real:= 0.0;
    lstep0: constant Real:= Norm(step);

    -- This function check whether we are inside the prism above face f

    function Dans_prisme_epaissi(f: Positive) return Boolean is
      sfp1: Positive;
      Ps, Psp1: Point_3D;
      u, edge_vector, npa: Vector_3D;
      dist_edge, nnpa: Real;
      facteur: constant:= 1.05;
    begin
      -- Cycle through face's vertices
      for sf in reverse 1..o.face_internal(f).last_edge loop
        sfp1:= 1 + sf mod o.face_internal(f).last_edge;
        Ps  := o.point( o.face_internal(f).P_compact(sf)   );
        Psp1:= o.point( o.face_internal(f).P_compact(sfp1) );
        edge_vector:= Psp1 - Ps;
        npa:= n * edge_vector;
        nnpa:= Norm(npa);
        if Almost_zero(nnpa) then -- degenerated edge
          return False;
        end if;
        npa:= 1.0/nnpa * npa;
        -- npa points towards the prism's interior
        u:= P_after_step - (Ps + o.centre);
        dist_edge:= u * npa;
        if dist_edge < - ball.radius * facteur then
          return False;
        end if;
      end loop;
      return True;
    end Dans_prisme_epaissi;

  begin
    reacted:= 0.0;
    if Almost_zero(lstep0) then
      return;
    end if;

    P_after_step:= ball.centre + step;

    for face in reverse 1..o.Max_faces loop
      n:= o.face_internal(face).normal;
      if check_normals then
        nn:= Norm(n);
        if Almost_zero(nn) then
          raise Zero_normal;
        elsif abs(nn - 1.0) > 1.0e-7 then
          raise Not_one_normal with " norm = " & Real'Image(nn);
        end if;
      end if;
      --  put_line("step=" & step(0)'img & ' ' & step(1)'img & ' ' & step(2)'img);
      --  put_line("   n=" & n(0)'img & ' ' & n(1)'img & ' ' & n(2)'img);
      if step * n < 0.0 then
        P_face:= o.point(o.face_internal(face).P_compact(1)) + o.centre;
        -- ^ any point on the face, to measure distance to face's plane.
        u:= ball.centre - P_face;
        dist_before:= u * n;
        if dist_before > 0.0 then
          -- ^ Fine, we are on the right side of the face.
          --   Test added to Engine_3D's algo, since objects are
          --   not always hollow, convex polyhedrons anymore.
          u:= P_after_step - P_face;
          dist_after:= u * n;
          if dist_after < ball.radius
            -- ^ Ouch! React we must!
            -- This includes negatives values of dist_after, in cases
            -- the intended step makes going through the face!
          and then
             Dans_prisme_epaissi(face)
          then
            if o.face(face).skin /= invisible then
            -- ^ this assumes: invisible <=> can go through
              reacted:= reacted + retour / lstep0;
              -- !! seems wrong if reactions in different directions
              --    should be something like step * step0
              case method is
                when elastic =>
                  raise Unsupported with "elastic reaction";
                  -- should compute the time the "ball" takes from rebound to
                  -- next face or portal.
                when slide =>
                  retour:= ball.radius - dist_after; -- always > 0
                  step:= step + retour * n;
                  -- Since step and n have a negative dot product      -checked-
                  -- and dist(ball.centre+step_old,face) < ball.radius -checked-
                  -- then:
                  -- ||step_new|| < ||step_old|| --> decreasing algo :-)
              end case;
            end if;
          end if;
        end if;
      end if;
    end loop;
 end Reaction;

end GLOBE_3D.Collision_detection;
