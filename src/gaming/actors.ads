------------
-- Actors --
------------

-- Change log:
-- GM 18-May-2008: Changed from Ego to Actors: object-oriented,
--   no more ego-centric, esp.: globals removed, wrapping Actor type...

with Game_control, GLOBE_3D;

package Actors is

  ------------------
  -- Translations --
  ------------------

  procedure Translation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  );

  -- Limiting modifies a translation step, e.g. due to collision detection.
  generic
    with procedure Limiting(step: in out GLOBE_3D.Vector_3D);
  procedure Limited_Translation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  );

  ---------------
  -- Rotations --
  ---------------

  procedure Rotation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  );

  -- Version with a vector of angles in radians
  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    vector        : in out GLOBE_3D.Vector_3D;
    time_step     : GLOBE_3D.Real;
    rotation_speed: in out GLOBE_3D.Vector_3D
  );

  -- Version with a rotation matrix
  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    matrix        : in out GLOBE_3D.Matrix_33;
    time_step     : GLOBE_3D.Real;
    rotation_speed: in out GLOBE_3D.Vector_3D
  );

end Actors;
