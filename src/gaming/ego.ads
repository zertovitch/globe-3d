with Game_control, Globe_3D;

package Ego is

  eye           : aliased Globe_3D.Vector_3D:= ( 0.0, 0.0, 4.0 );
  world_rotation: aliased Globe_3D.Matrix_33:= Globe_3D.Id_33;
  view_direction:         Globe_3D.Vector_3D;

  procedure Eye_translation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    time_step     : Globe_3D.Real;
    the_eye           : access Globe_3D.Vector_3D := Ego.eye'access;
    the_world_rotation: access Globe_3D.Matrix_33 := Ego.world_rotation'access;
    the_speed         :        Globe_3D.p_Vector_3D := null
  );

  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    matrix        : in out Globe_3D.Matrix_33;
    time_step     : Globe_3D.Real;
    the_rotation_speed: Globe_3D.p_Vector_3D := null
  );

  procedure Eye_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    time_step     : Globe_3D.Real;
    the_world_rotation: access Globe_3D.Matrix_33   := Ego.world_rotation'access;
    the_rotation_speed:        Globe_3D.p_Vector_3D := null
  );

end Ego;
