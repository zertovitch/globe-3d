-----------------------------------------------------------------------------
--  This file contains the body, please refer to specification (.ads file)
-----------------------------------------------------------------------------

package body Game_Control is

  procedure Append_Commands
    (size_x,
     size_y     : in     Integer;                   --  screen dimensions for mouse
     warp_mouse : in     Boolean;                   --  recenter mouse cursor
     c          : in out Game_Control.Command_Set;  --  commands are added to c
     gx, gy     :    out GL.Double;                 --  mouse movement since last call
     keyboard   :        GLUT.Devices.p_Keyboard := GLUT.Devices.default_keyboard'Access;
     mouse      :        GLUT.Devices.p_Mouse    := GLUT.Devices.default_mouse'Access)
  is
    use GLUT.Devices;

    sensib : constant := 8.0;
    dx, dy : Integer;
    use GL;

  begin
    --------------
    -- Keyboard --
    --------------

    --  Clavier: !! lettres: clavier CH

    c (slide_mode)    := keyboard.modif_set (GLUT.ACTIVE_ALT);

    if c (slide_mode) then
      c (slide_up)    := keyboard.special_set (GLUT.KEY_PAGE_UP);
      c (slide_down)  := keyboard.special_set (GLUT.KEY_PAGE_DOWN);
    else
      c (turn_up)     := keyboard.special_set (GLUT.KEY_PAGE_UP);
      c (turn_down)   := keyboard.special_set (GLUT.KEY_PAGE_DOWN);
      c (slide_left)  := keyboard.normal_set ('A');
      c (slide_right) := keyboard.normal_set ('D');
      c (slide_up)    := keyboard.normal_set ('R');
      c (slide_down)  := keyboard.normal_set ('F');
    end if;
    c (swing_plus)    := keyboard.normal_set ('E');
    c (swing_minus)   := keyboard.normal_set ('Q');
    c (special_plus)  := keyboard.normal_set ('+');
    c (special_minus) := keyboard.normal_set ('-');

    c (jump) := Strike_Once (' ', keyboard);
    for i in n0 .. n9 loop
      c (i) := Strike_Once
        (Character'Val
          (Command'Pos (i) - Command'Pos (n0) + Character'Pos ('0')),
         keyboard);
    end loop;

    c (photo)     := Strike_Once (GLUT.KEY_F12, keyboard);
    c (video)     := Strike_Once (GLUT.KEY_F11, keyboard);
    c (toggle_10) := Strike_Once (GLUT.KEY_F10, keyboard);

    c (interrupt_game) := keyboard.normal_set (ASCII.ESC);
    c (go_forward)     := keyboard.special_set (GLUT.KEY_UP)
                          or
                          keyboard.normal_set ('W');

    c (go_backwards) :=   keyboard.special_set (GLUT.KEY_DOWN)
                          or
                          keyboard.normal_set ('S');

    c (run_mode)  := keyboard.modif_set (GLUT.ACTIVE_SHIFT);
    c (ctrl_mode) := keyboard.modif_set (GLUT.ACTIVE_CTRL);

    -----------
    -- Mouse --
    -----------

    if mouse.button_state (GLUT.LEFT_BUTTON)  then c (go_forward) := True; end if;
    if mouse.button_state (GLUT.RIGHT_BUTTON) then c (slide_mode) := True; end if;

    dx := mouse.mx - mouse.oldx;
    dy := mouse.my - mouse.oldy;
    gx := 0.0;
    gy := 0.0;
    if abs dx <= 100 and then abs dy <= 100 then
      --  ^ avoid window in/out movements
      if dx /= 0 then
       gx := sensib * GL.Double (dx) / GL.Double (size_x);
       if c (slide_mode) then
         c (slide_lateral_graduated) := True;
       else
         c (turn_lateral_graduated) := True;
       end if;
      end if;
      if dy /= 0 then
       gy := -sensib * GL.Double (dy) / GL.Double (size_y);
       if c (slide_mode) then
         c (slide_vertical_graduated) := True;
       else
         c (turn_vertical_graduated) := True;
       end if;
      end if;
    end if;

    if warp_mouse and then
       (abs (mouse.mx - size_x / 2) > size_x / 4 or else
        abs (mouse.my - size_y / 2) > size_y / 4)
    then
      mouse.oldx := size_x / 2;
      mouse.oldy := size_y / 2;
      GLUT.WarpPointer (mouse.oldx, mouse.oldy);
    else
      mouse.oldx := mouse.mx;
      mouse.oldy := mouse.my;
    end if;

    if c (slide_mode) then
      c (slide_left)  :=    keyboard.special_set (GLUT.KEY_LEFT);
      c (slide_right) :=    keyboard.special_set (GLUT.KEY_RIGHT);
    else
      c (turn_left)  :=     keyboard.special_set (GLUT.KEY_LEFT);
      c (turn_right) :=     keyboard.special_set (GLUT.KEY_RIGHT);
    end if;

  end Append_Commands;

end Game_Control;
