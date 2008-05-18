-- GM 18-May-2008: Changed from Ego to Actors: object-oriented,
--   no more ego-centric, esp.: globals removed, wrapping Actor type...

with Game_control, GLOBE_3D;

package Actors is

  procedure Translation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  );

  procedure Rotation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  );

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
