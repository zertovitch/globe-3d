------------------------------------------------------------------------------
--  File:            game_control.ads
--  Description:     Command set for games, based on GLUT
--  Copyright (c) Gautier de Montmollin 2002, 2005 .. 2008
------------------------------------------------------------------------------

--  To do: programmable behaviour

with GL, GLUT.Devices;

package Game_Control is

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
      ctrl_mode,  --  "shoot", but useless with GLUT
    slide_mode,
      swing_plus,
      swing_minus,
    jump,
    special_plus,
    special_minus,
      photo, video,
    toggle_10,
    interrupt_game,
      n0, n1, n2, n3, n4, n5, n6, n7, n8, n9,  --  Numeric keys
    bogus_command);  --  a control can be directed on this

  type Command_Set is array (Command) of Boolean;

  --  The empty command set:
  no_command : constant Command_Set := (others => False);

  --  Function Set_...

  --  keyboard_command_mapping: array( Multi_keys.key_value ) of Command :=
  --    ( others=> bogus_command ); -- for later !!

  --  mouse_command_mapping   : array( PC_Mouse.Mouse_button ) of Command :=
  --    ( others=> bogus_command ); -- for later !!

  --  Record game commands from peripherals (keyboard, mouse) --

  procedure Append_Commands
    (size_x,
     size_y     : in     Integer;                   --  screen dimensions for mouse
     warp_mouse : in     Boolean;                   --  recenter mouse cursor
     c          : in out Game_Control.Command_Set;  --  commands are added to c
     gx, gy     :    out GL.Double;                 --  mouse movement since last call
     keyboard   :        GLUT.Devices.p_Keyboard := GLUT.Devices.default_keyboard'Access;
     mouse      :        GLUT.Devices.p_Mouse    := GLUT.Devices.default_mouse'Access);

end Game_Control;
