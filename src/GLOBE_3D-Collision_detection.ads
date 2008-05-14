-- Change log
--
-- 14-May-2008: GM: created (re-used some of old Engine_3D)

package GLOBE_3D.Collision_detection is

  -- Reaction to an object - and the world connected to it

  type Reaction_method is ( elastic, slide );

  type Ball_type is record
    centre : Point_3D;
    radius : Real;
  end record;

  procedure Reaction(
    o           : Object_3D;
    ball        : Ball_type;
    method      : Reaction_method;
    step        : in out Vector_3D; -- Whole step (in: desired, out: effective)
    reacted     : out Real          -- in proportion to step
  );

end GLOBE_3D.Collision_detection;
