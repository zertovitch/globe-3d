-----------------------------------------------------------------------------
--  This file contains the body, please refer to specification (.ads file)
-----------------------------------------------------------------------------

-- with Interfaces;
-- with Ada.Characters.Handling;           use Ada.Characters.Handling;

package body Game_control is


  procedure Append_commands(
              size_x,
              size_y       : in     Integer;     -- screen dimensions for mouse
              warp_mouse   : in     Boolean;     -- recenter mouse cursor
              c            : in out game_Control.Command_set; -- commands are added to c
              gx,gy        :    out GL.Double;   -- mouse movement since last call
              Keyboard   : access GLUT.devices.Keyboard := GLUT.devices.default_Keyboard'access;
              Mouse      : access GLUT.devices.Mouse    := GLUT.devices.default_Mouse'access
  )
   is
    use GLUT.Devices;

    sensib: constant:= 8.0;
    dx,dy: Integer;
    use GL;

  begin
    --------------
    -- Keyboard --
    --------------

    -- Clavier: !! lettres: clavier CH

    c( slide_mode ):=     Keyboard.modif_set( GLUT.Active_alt );

    if c( slide_mode ) then
      c( slide_up ):=       Keyboard.special_set( GLUT.key_page_up );
      c( slide_down ):=     Keyboard.special_set( GLUT.key_page_down );
    else
      c( turn_up ):=        Keyboard.special_set( GLUT.key_page_up );
      c( turn_down ):=      Keyboard.special_set( GLUT.key_page_down );
      c( slide_left ):=     Keyboard.normal_set( 'A' );
      c( slide_right ):=    Keyboard.normal_set( 'D' );
      c( slide_up ):=       Keyboard.normal_set( 'R' );
      c( slide_down ):=     Keyboard.normal_set( 'F' );
    end if;
    c( swing_plus ):=     Keyboard.normal_set( 'E' );
    c( swing_minus ):=    Keyboard.normal_set( 'Q' );
    c( special_plus ):=   Keyboard.normal_set( '+' );
    c( special_minus ):=  Keyboard.normal_set( '-' );
    c( jump ):=           Strike_once( ' ', Keyboard);
    for i in n0..n9 loop
      c( i ):= Strike_once(Character'Val(Command'Pos(i)-Command'Pos(n0)+Character'Pos('0')),
                           Keyboard);
    end loop;


    c( photo ):=          Strike_once( GLUT.key_F12, Keyboard );
    c( video ):=          Strike_once( GLUT.key_F11, Keyboard );
    c( toggle_10 ):=      Strike_once( GLUT.key_F10, Keyboard );

    c( interrupt_game ):= Keyboard.normal_set( ASCII.ESC );
    c( go_forward ):=     Keyboard.special_set( GLUT.key_up )
                          or
                          Keyboard.normal_set( 'W' );
    c( go_backwards ):=   Keyboard.special_set( GLUT.key_down )
                          or
                          Keyboard.normal_set( 'S' );
    c( run_mode ):=       Keyboard.modif_set( GLUT.Active_shift );
    c( ctrl_mode ):=      Keyboard.modif_set( GLUT.Active_ctrl );


    -----------
    -- Mouse --
    -----------

    if Mouse.button_state( GLUT.LEFT_BUTTON )  then c( go_forward ):= True; end if;
    if Mouse.button_state( GLUT.RIGHT_BUTTON ) then c( slide_mode ):= True; end if;

    dx:= Mouse.mx - Mouse.oldx;
    dy:= Mouse.my - Mouse.oldy;
    gx:= 0.0;
    gy:= 0.0;
    if abs dx <= 100 and then abs dy <= 100 then
      -- ^ avoid window in/out movements
      if dx/=0 then
       gx:= sensib * GL.Double(dx) / GL.Double(size_x);
       if c( slide_mode ) then
         c( slide_lateral_graduated ):= True;
       else
         c( turn_lateral_graduated ) := True;
       end if;
      end if;
      if dy/=0 then
       gy:= -sensib * GL.Double(dy) / GL.Double(size_y);
       if c( slide_mode ) then
         c( slide_vertical_graduated ):= True;
       else
         c( turn_vertical_graduated ):= True;
       end if;
      end if;
    end if;

    if warp_mouse and then
       (abs(Mouse.mx-size_x/2) > size_x/4 or abs(Mouse.my-size_y/2) > size_y/4)
    then
      Mouse.oldx:= size_x/2;
      Mouse.oldy:= size_y/2;
      GLUT.WarpPointer(Mouse.oldx, Mouse.oldy);
    else
      Mouse.oldx:= Mouse.mx;
      Mouse.oldy:= Mouse.my;
    end if;

    if c( slide_mode ) then
      c( slide_left ):=     Keyboard.special_set( GLUT.key_left );
      c( slide_right ):=    Keyboard.special_set( GLUT.key_right );
    else
      c( turn_left ):=      Keyboard.special_set( GLUT.key_left );
      c( turn_right ):=     Keyboard.special_set( GLUT.key_right );
    end if;

  end Append_commands;


end Game_control;
