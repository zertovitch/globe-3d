-----------------------------------------------------------------------------
--  This file contains the body, please refer to specification (.ads file)
-----------------------------------------------------------------------------

with GLUT.Windows;

with Ada.Characters.Handling,
     Ada.Unchecked_Conversion;

with Interfaces;

with System;

package body GLUT.Devices is

   use Ada.Characters.Handling;
   use GLUT.Windows;

   --  Current_Window : - for accessing the current GLUT window
   --                   - used by GLUT callbacks to determine the Window to which a callback event relates.
   --

   function Current_Window return Windows.Window_View
   is
      function to_Window is
        new Ada.Unchecked_Conversion (System.Address, Windows.Window_View);
   begin
      return to_Window (GLUT.GetWindowData);
   end Current_Window;

   --  Keyboard
   --

   function Current_Keyboard return p_Keyboard
   is
      the_current_Window : constant Windows.Window_View := Current_Window;
   begin
      if the_current_Window = null then
         return default_keyboard'Access;
      else
         return GLUT.Windows.Keyboard (the_current_Window);
      end if;
   end Current_Keyboard;

  procedure Affect_Modifier_Key (modif_code : Integer) is
    use Interfaces;
    m : constant Unsigned_32 := Unsigned_32 (modif_code);
    kb : constant p_Keyboard := Current_Keyboard;
  begin
    kb.modif_set (GLUT.ACTIVE_SHIFT) := (m and GLUT.ACTIVE_SHIFT) /= 0;
    kb.modif_set (GLUT.ACTIVE_CTRL)  := (m and GLUT.ACTIVE_CTRL) /= 0;
    kb.modif_set (GLUT.ACTIVE_ALT)   := (m and GLUT.ACTIVE_ALT) /= 0;
  end Affect_Modifier_Key;

  procedure Update_Modifier_Keys is
  begin
    Affect_Modifier_Key (GLUT.GetModifiers);
    --  During a callback, GetModifiers may be called
    --  to determine the state of modifier keys
    --  when the keystroke generating the callback occurred.
  end Update_Modifier_Keys;

  --------------------------------
  --  GLUT Callback procedures  --
  --------------------------------

  procedure Key (k : GLUT.Key_Type; x, y : Integer) is
  pragma Unreferenced (x, y);
  begin
    --  key k is pressed
    Current_Keyboard.normal_set (To_Upper (Character'Val (k))) := True;
    Update_Modifier_Keys;
  end Key;

  procedure Key_Up (k : GLUT.Key_Type; x, y : Integer) is
  pragma Unreferenced (x, y);
  begin
    Current_Keyboard.normal_set (To_Upper (Character'Val (k))) := False;  -- key k is unpressed
    Update_Modifier_Keys;
  end Key_Up;

  procedure Special_Key (k : Integer; x, y : Integer) is
  pragma Unreferenced (x, y);
  begin
    Current_Keyboard.special_set (k) := True;  -- key k is pressed
    Update_Modifier_Keys;
  end Special_Key;

  procedure Special_Key_Up (k : Integer; x, y : Integer) is
  pragma Unreferenced (x, y);
  begin
    Current_Keyboard.special_set (k) := False; -- key k is unpressed
    Update_Modifier_Keys;
  end Special_Key_Up;

   --  Mouse
   --

  function Current_Mouse return p_Mouse
  is
     the_current_Window : constant Windows.Window_View := Current_Window;
  begin
     if the_current_Window = null then
        return default_mouse'Access;
     else
        return GLUT.Windows.Mouse (the_current_Window);
     end if;
  end Current_Mouse;

  procedure Mouse_Event (button, state, x, y : Integer) is
  --  When a user presses and releases mouse buttons in the window,
  --  each press and each release generates a mouse callback.
    ms : constant p_Mouse := Current_Mouse;
  begin
    ms.mx := x;
    ms.my := y;
    if button in Current_Mouse.button_state'Range then  --  skip extra buttons (wheel, etc.)
      Current_Mouse.button_state (button) := state = GLUT.DOWN;
    end if;
    Update_Modifier_Keys;
  end Mouse_Event;

  procedure Motion (x, y : Integer) is
  --  The motion callback for a window is called when the mouse moves within the
  --  window while one or more mouse buttons are pressed.
    ms : constant p_Mouse := Current_Mouse;
  begin
    ms.mx := x;
    ms.my := y;
  end Motion;

  procedure Passive_Motion (x, y : Integer) is
  --  The passive motion callback for a window is called when
  --  the mouse moves within the window while no mouse buttons are pressed.
    ms : constant p_Mouse := Current_Mouse;
  begin
    ms.mx := x;
    ms.my := y;
  end Passive_Motion;

   --  Initialize
   --

  procedure Initialize is
  begin
    IgnoreKeyRepeat (1);
    KeyboardFunc      (Key'Address);
    KeyboardUpFunc    (Key_up'Address);
    SpecialFunc       (Special_Key'Address);
    SpecialUpFunc     (Special_Key_Up'Address);
    MouseFunc         (Mouse_Event'Address);
    MotionFunc        (Motion'Address);
    PassiveMotionFunc (Passive_Motion'Address);
  end Initialize;

   --  User input management
   --

  function Strike_Once
    (c  : Character;
     kb : p_Keyboard := default_keyboard'Access) return Boolean
  is
  begin
    if kb.normal_set (c) then
      if kb.normal_set_mem (c) then
        return False; -- already a reported strike
      else
        kb.normal_set_mem (c) := True; -- key is now recorded as pressed
        return True;
      end if;
    else
      kb.normal_set_mem (c) := False; -- unpressed -> next strike allowed
      return False;
    end if;
  end Strike_Once;

  function Strike_Once
    (special : Integer;
     kb      : p_Keyboard := default_keyboard'Access) return Boolean
  is
  begin
    if special not in Special_Key_Set'Range then
      return False;
    else
      if kb.special_set (special) then
        if kb.special_set_mem (special) then
          return False;  --  already a reported strike
        else
          kb.special_set_mem (special) := True;  --  key is now recorded as pressed
          return True;
        end if;
      else
        kb.special_set_mem (special) := False;  --  unpressed -> next strike allowed
        return False;
      end if;
    end if;
  end Strike_Once;

end GLUT.Devices;
