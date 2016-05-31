------------------------------------------------------------------------------
--  File:            Game_control.ads
--  Description:     Command set for games, based on GLUT
--  Copyright (c) Gautier de Montmollin 2002, 2005..2008
------------------------------------------------------------------------------
-- Cannibalized from Game_Driving (see Engine_3D)

--  To do: programmable behaviour

with GL, GLUT.Devices;

package Game_control is

  type Command is (
    go_forward,
    go_backwards,
    go_graduated,
        slide_left,
        slide_right,
        slide_lateral_graduated,
      turn_left,
      turn_right,
      turn_lateral_graduated,
          slide_up,
          slide_down,
          slide_vertical_graduated,
        turn_up,
        turn_down,
        turn_vertical_graduated,
    run_mode,
      ctrl_mode, -- "shoot", but useless with GLUT
    slide_mode,
      swing_plus,
      swing_minus,
    jump,
    special_plus,
    special_minus,
      photo, video,
    toggle_10,
    interrupt_game,
      n0,n1,n2,n3,n4,n5,n6,n7,n8,n9, -- numeric keys
    bogus_command  -- a control can be directed on this
  );

  type Command_set is array( Command ) of Boolean;

  -- The empty command set:
  no_command: constant Command_set:= (others=> False);

  -- Function Set_...

  --  keyboard_command_mapping: array( Multi_keys.key_value ) of Command :=
  --    ( others=> bogus_command ); -- for later !!

  --  mouse_command_mapping   : array( PC_Mouse.Mouse_button ) of Command :=
  --    ( others=> bogus_command ); -- for later !!

  -- Record game commands from peripherals (keyboard, mouse) --

  procedure Append_commands(
              size_x,
              size_y     : in     Integer;                  -- screen dimensions for mouse
              warp_mouse : in     Boolean;                  -- recenter mouse cursor
              c          : in out Game_control.Command_set; -- commands are added to c
              gx,gy      :    out GL.Double;                -- mouse movement since last call
              keyboard   : access GLUT.Devices.Keyboard := GLUT.Devices.default_Keyboard'Access;
              mouse      : access GLUT.Devices.Mouse    := GLUT.Devices.default_Mouse'Access
  );

end Game_control;
