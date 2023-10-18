------------------------------------------------------------------------------
--  File:            glut-devices.ads
--  Description:     Command set for games, based on GLUT
--  Copyright (c) Gautier de Montmollin/Rod Kay 2007
------------------------------------------------------------------------------

package GLUT.Devices is

   procedure Initialize;
   --
   --  Sets up the GLUT mouse and keybaord devices.

   --  Keyboard
   --

   type Key_Set         is array (Character) of Boolean;
   type Modifier_Set    is array (GLUT.ACTIVE_SHIFT .. GLUT.ACTIVE_ALT) of Boolean;
   type Special_Set     is array (1 .. 200) of Boolean;
   type Special_Key_Set is array (1 .. 128) of Boolean;

   type Keyboard is
      record
         normal_set     : Key_Set := (others => False);
         normal_set_mem : Key_Set := (others => False);

         modif_set       : Devices.Modifier_Set    := (others => False);
         special_set     : Devices.Special_Set     := (others => False);
         special_set_mem : Devices.Special_Key_Set := (others => False);
      end record;

   type p_Keyboard is access all Keyboard;

   default_keyboard : aliased Keyboard;

   function Strike_Once
     (c  : Character;
      kb : p_Keyboard := default_keyboard'Access) return Boolean;

   function Strike_Once
     (special : Integer;
      kb      : p_Keyboard := default_keyboard'Access) return Boolean;

   --  Mouse
   --

   type Mouse_Button_Set is array (GLUT.LEFT_BUTTON .. GLUT.RIGHT_BUTTON) of Boolean;

   type Mouse is
      record
         oldx, oldy, mx, my : Integer          := 0;
         button_state       : Mouse_Button_Set := (others => False);
      end record;

   type p_Mouse is access all Mouse;

   default_mouse : aliased Mouse;

end GLUT.Devices;
