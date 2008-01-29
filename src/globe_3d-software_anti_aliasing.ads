-- GLOBE_3D.Software_Anti_Aliasing provides a software method for
-- smoothing pictures by displaying several times a scene with
-- subpixel translations.

generic

  with procedure Display;

package GLOBE_3D.Software_Anti_Aliasing is

  -- Returns the number of phases needed for anti-aliasing:
  -- 1 for clearing accum buffer + #jitterings + 1 for display
  function Anti_Alias_phases return Positive;

  -- Display only one layer of anti-aliasing:

  procedure Display_with_Anti_Aliasing(phase: Positive);

  type Quality is (Q1,Q3,Q4,Q11,Q16,Q29,Q90);
  -- Q1 means no aliasing at all

  procedure Set_quality(q: Quality);

end GLOBE_3D.Software_Anti_Aliasing;