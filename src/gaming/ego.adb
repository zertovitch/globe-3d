with GL;
with Globe_3D.Math;

package body Ego is

  use Globe_3D, Globe_3D.Math, Globe_3D.REF, Game_control, GL;


  -- defaults
  --
  speed, rotation_speed: aliased Vector_3D:= ( 0.0, 0.0, 0.0 );




  procedure Eye_translation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    time_step     : Globe_3D.Real;
    the_eye           : access Globe_3D.Vector_3D := Ego.eye'access;
    the_world_rotation: access Globe_3D.Matrix_33 := Ego.world_rotation'access;
    the_speed         :        p_Vector_3D        := null
  )
  is
    unitary_movement, eye_movement: Real;
    step: Vector_3D;
    my_speed: p_Vector_3D;
  begin

    if the_Speed = null then
       my_Speed := Ego.Speed'access;
    else
       my_Speed := the_Speed;
    end if;


    if gc( run_mode ) then
      unitary_movement:= 300.0;
    else
      unitary_movement:= 100.0;
    end if;
    unitary_movement:= unitary_movement * unitary_change;
    eye_movement:= unitary_movement * 2.0;

    if gc( go_forward ) then
      my_Speed(2):= my_Speed(2) - eye_movement;
    elsif gc( go_backwards ) then
      my_Speed(2):= my_Speed(2) + eye_movement;
    end if;

    if gc( slide_vertical_graduated ) then
      my_Speed(1):= my_Speed(1) + gy * 2.0 * unitary_movement;
    elsif gc( slide_down ) then
      my_Speed(1):= my_Speed(1) - eye_movement;
    elsif gc( slide_up ) then
      my_Speed(1):= my_Speed(1) + eye_movement;
    end if;

    if gc( slide_lateral_graduated ) then
      my_Speed(0):= my_Speed(0) + gx * 2.0 * unitary_movement;
    elsif gc( slide_left ) then
      my_Speed(0):= my_Speed(0) - eye_movement;
    elsif gc( slide_right ) then
      my_Speed(0):= my_Speed(0) + eye_movement;
    end if;

    step:=
      time_step * (Transpose(the_world_rotation.all) * my_Speed.all);
      --  ( speed(0),    -- lateral sliding
      --    speed(1),    -- vertical sliding
      --    speed(2) );  -- forward/backwards
      --   -- ^ vector in the local referential

    the_eye.all := the_eye.all + step;

    my_Speed.all:= deceleration * my_Speed.all;

  end Eye_translation;




  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    matrix        : in out Globe_3D.Matrix_33;
    time_step     : Globe_3D.Real;
    the_rotation_speed: Globe_3D.p_Vector_3D := null
  )
  is
    unitary_movement, mouse_rotation, key_rotation: Real;
    my_rotation_speed: Globe_3D.p_Vector_3D := null;
  begin

    if the_rotation_speed = null then
      my_rotation_speed := Ego.rotation_Speed'access;
    else
      my_rotation_speed := the_rotation_Speed;
    end if;


    if gc( run_mode ) then
      unitary_movement:= 40.0;
    else
      unitary_movement:= 20.0;
    end if;
    unitary_movement:= unitary_movement * unitary_change;
    mouse_rotation:= 2.0  * unitary_movement;
    key_rotation  := 0.17 * unitary_movement;

    if gc( swing_plus  ) then my_rotation_speed(2):= my_rotation_speed(2) + key_rotation; end if;
    if gc( swing_minus ) then my_rotation_speed(2):= my_rotation_speed(2) - key_rotation; end if;
    if gc( turn_left   ) then my_rotation_speed(1):= my_rotation_speed(1) + key_rotation; end if;
    if gc( turn_right  ) then my_rotation_speed(1):= my_rotation_speed(1) - key_rotation; end if;
    if gc( turn_up     ) then my_rotation_speed(0):= my_rotation_speed(0) - key_rotation; end if;
    if gc( turn_down   ) then my_rotation_speed(0):= my_rotation_speed(0) + key_rotation; end if;
    if gc( turn_lateral_graduated ) then
      my_rotation_speed(1):= my_rotation_speed(1) - gx * mouse_rotation;
    end if;
    if gc( turn_vertical_graduated ) then
      my_rotation_speed(0):= my_rotation_speed(0) - gy * mouse_rotation;
    end if;
    matrix:= matrix * XYZ_rotation(time_step * my_rotation_speed.all);
    my_rotation_speed.all:= deceleration * my_rotation_speed.all;
  end Abstract_rotation;


  procedure Eye_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : Globe_3D.Real;
    unitary_change: Globe_3D.Real;
    deceleration  : Globe_3D.Real;
    time_step     : Globe_3D.Real;
    the_world_rotation: access Globe_3D.Matrix_33   := Ego.world_rotation'access;
    the_rotation_speed:        Globe_3D.p_Vector_3D := null
  )
  is
    mat: Matrix_33:= ID_33;
  begin
    Abstract_rotation(gc,gx,gy,unitary_change,deceleration, mat, time_step, the_rotation_speed);
    the_world_rotation.all:= mat * the_world_rotation.all;
    Re_Orthonormalize(the_world_rotation.all);
  end Eye_rotation;

end Ego;
