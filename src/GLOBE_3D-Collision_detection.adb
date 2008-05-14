with GLOBE_3D.Math;                     use GLOBE_3D.Math;

package body GLOBE_3D.Collision_detection is

  procedure Reaction(
    o           : Object_3D;
    ball        : Ball_type;
    method      : Reaction_method;
    step        : in out Vector_3D; -- Whole step (in: desired, out: effective)
    reacted     : out Real          -- in proportion to step
  )
  is
    P0rf,P1rf: Point_3D;
    u,n : Vector_3D;
    dist: Real; -- distance orientee
    retour: Real:= 0.0;
    j1: Positive;
    lstep0: constant Real:= Norm(step);

    -- This function check whether we are inside the prism above face f

    function Dans_prisme_epaissi(f: Positive) return Boolean is
      sfp1: Positive;
      js, jsp1: Positive;
      Ps, Psp1: Point_3D;
      u,a,npa: Vector_3D;
      dsp: Real;
      facteur: constant:= 1.1;
    begin
      for sf in reverse 1..o.Face_invariant(f).last_edge loop
        sfp1:= 1 + sf mod o.Face_invariant(f).last_edge;
        js  := o.Face_invariant(f).P_compact(sf);
        jsp1:= o.Face_invariant(f).P_compact(sfp1);
        Ps  := o.point(js);
        Psp1:= o.point(jsp1);
        a:= Psp1 - Ps; -- vecteur arete
         -- npa: ortho a la face num. sf du prisme engendre
         --      par la face de l'objet et la normale au plan
        npa:= n*a; -- npa vers interieur du prisme
        npa:= 1.0/Norm(npa) * npa;
        u:= P1rf - (o.point(js) + o.Centre);
        dsp:= u * npa;
        if dsp < - ball.radius * facteur then
          return False;
        end if;
      end loop;
      return True;
    end Dans_prisme_epaissi;

  begin
    reacted:= 0.0;
    if Almost_Zero(lstep0) then
      return;
    end if;

    P0rf:= ball.centre;

    P1rf:= P0rf + step;
    for face in reverse 1..o.Max_faces loop
      n:= o.Face_invariant(face).normal;
      if step * n < 0.0 then
        j1:= o.Face_invariant(face).P_compact(1);
        u:= P1rf - (o.point(j1)+o.Centre);
        dist:= u * n;
        if dist < ball.radius -- ouch! react we must!
          -- this includes negatives values of dist
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
                null;
                -- to do !!
                -- should compute the time the "ball" takes from rebound to
                -- next face or portal.
              when slide =>
                retour:= ball.radius - dist; -- always > 0
                step:= step + retour * n;
                -- Since step and n have a negative dot product -checked-
                -- and dist(ball.centre,face) < ball.radius     -checked-
                -- then:
                -- ||step_new|| < ||step_old|| --> decreasing algo :-)
            end case;
          end if;
        end if;
      end if;
    end loop;
 end Reaction;

end GLOBE_3D.Collision_detection;
