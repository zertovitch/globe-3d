with GL;
with GLOBE_3D.Math;

package body Actors is

  use GLOBE_3D, GLOBE_3D.Math, GLOBE_3D.REF, Game_control, GL;

  procedure Limited_Translation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  )
  is
    unitary_movement, eye_movement: Real;
    step: Vector_3D;
  begin
    if gc( run_mode ) then
      unitary_movement:= 300.0;
    else
      unitary_movement:= 100.0;
    end if;
    unitary_movement:= unitary_movement * unitary_change;
    eye_movement:= unitary_movement * 2.0;

    if gc( go_forward ) then
      actor.speed(2):= actor.speed(2) - eye_movement;
    elsif gc( go_backwards ) then
      actor.speed(2):= actor.speed(2) + eye_movement;
    end if;

    if gc( slide_vertical_graduated ) then
      actor.speed(1):= actor.speed(1) + gy * 2.0 * unitary_movement;
    elsif gc( slide_down ) then
      actor.speed(1):= actor.speed(1) - eye_movement;
    elsif gc( slide_up ) then
      actor.speed(1):= actor.speed(1) + eye_movement;
    end if;

    if gc( slide_lateral_graduated ) then
      actor.speed(0):= actor.speed(0) + gx * 2.0 * unitary_movement;
    elsif gc( slide_left ) then
      actor.speed(0):= actor.speed(0) - eye_movement;
    elsif gc( slide_right ) then
      actor.speed(0):= actor.speed(0) + eye_movement;
    end if;

    step:=
      time_step * (Transpose(actor.world_rotation) * actor.speed);
      --  ( speed(0),    -- lateral sliding
      --    speed(1),    -- vertical sliding
      --    speed(2) );  -- forward/backwards
      --   -- ^ vector in the local referential

    Limiting(step);

    actor.clipper.eye_position:= actor.clipper.eye_position + step;

    actor.speed:= deceleration * actor.speed;

  end Limited_Translation;

  procedure No_Limitation(step: in out GLOBE_3D.Vector_3D) is
  null;

  procedure Translation_inst is new Limited_Translation(No_Limitation);

  procedure Translation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  )
  renames Translation_inst;

  procedure Rotation(
    actor         : in out GLOBE_3D.Camera;
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    time_step     : GLOBE_3D.Real
  )
  is
    incremental_rotation: Vector_3D := (0.0, 0.0, 0.0);
  begin
    Abstract_rotation(
      gc,gx,gy,
      unitary_change, deceleration, incremental_rotation, time_step,
      actor.rotation_speed
    );
    actor.rotation:= actor.rotation + incremental_rotation;
    if actor.compose_rotations then
      actor.world_rotation:=
        XYZ_rotation(incremental_rotation) * actor.world_rotation;
      Re_Orthonormalize(actor.world_rotation);
    else
      declare
        r: Vector_3D renames actor.rotation;
        -- We need to turn around the axes in this order: Y, X, Z
      begin
        actor.world_rotation:=
          XYZ_rotation(  0.0,  0.0, r(2) ) *  -- 3) turn around the nose
          XYZ_rotation( r(0),  0.0,  0.0 ) *  -- 2) lift or lower the head
          XYZ_rotation(  0.0, r(1),  0.0 )    -- 1) pivotate around the feet
        ;
      end;
    end if;
  end Rotation;

  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    vector        : in out GLOBE_3D.Vector_3D;
    time_step     : GLOBE_3D.Real;
    rotation_speed: in out GLOBE_3D.Vector_3D
  )
  is
    unitary_movement, mouse_rotation, key_rotation: Real;
  begin
    if gc( run_mode ) then
      unitary_movement:= 40.0;
    else
      unitary_movement:= 20.0;
    end if;
    unitary_movement:= unitary_movement * unitary_change;
    mouse_rotation:= 2.0  * unitary_movement;
    key_rotation  := 0.17 * unitary_movement;

    if gc( swing_plus  ) then rotation_speed(2):= rotation_speed(2) + key_rotation; end if;
    if gc( swing_minus ) then rotation_speed(2):= rotation_speed(2) - key_rotation; end if;
    if gc( turn_left   ) then rotation_speed(1):= rotation_speed(1) + key_rotation; end if;
    if gc( turn_right  ) then rotation_speed(1):= rotation_speed(1) - key_rotation; end if;
    if gc( turn_up     ) then rotation_speed(0):= rotation_speed(0) - key_rotation; end if;
    if gc( turn_down   ) then rotation_speed(0):= rotation_speed(0) + key_rotation; end if;
    if gc( turn_lateral_graduated ) then
      rotation_speed(1):= rotation_speed(1) - gx * mouse_rotation;
    end if;
    if gc( turn_vertical_graduated ) then
      rotation_speed(0):= rotation_speed(0) - gy * mouse_rotation;
    end if;
    vector:= vector + time_step * rotation_speed;
    rotation_speed:= deceleration * rotation_speed;
  end Abstract_rotation;

  procedure Abstract_rotation(
    gc            : Game_control.Command_set;
    gx,gy         : GLOBE_3D.Real;
    unitary_change: GLOBE_3D.Real;
    deceleration  : GLOBE_3D.Real;
    matrix        : in out GLOBE_3D.Matrix_33;
    time_step     : GLOBE_3D.Real;
    rotation_speed: in out GLOBE_3D.Vector_3D
  )
  is
    incremental_rotation: Vector_3D := (0.0, 0.0, 0.0);
  begin
    Abstract_rotation(
      gc, gx,gy,
      unitary_change, deceleration, incremental_rotation, time_step,
      rotation_speed
    );
    matrix:= matrix * XYZ_rotation(incremental_rotation);
  end Abstract_rotation;

end Actors;
