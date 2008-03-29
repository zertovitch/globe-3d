------------------------------------------------------------------------------
--  File:            Game_control.ads
--  Description:     Command set for games, based on GLUT
--  Copyright (c) Gautier de Montmollin/Rod Kay 2007
------------------------------------------------------------------------------

with GLOBE_3D;
with GL;
--with Game_control;

package glut.Devices is


   procedure Initialize;
   --
   -- Sets up the GLUT mouse and keybaord devices.



   -- Keyboard
   --

   type Key_set         is array (Character ) of Boolean;
   type Modifier_set    is array (glut.Active_shift .. glut.Active_alt) of Boolean;
   type Special_set     is array (1 .. 200) of Boolean;
   type Special_key_set is array (1 .. 128) of Boolean;

   type Keyboard is
      record
         normal_set     : Key_set   := (others=> False);
         normal_set_mem : Key_set   := (others=> False);

         modif_set       : devices.Modifier_set    := (others=> False);
         special_set     : devices.special_Set     := (others=> False);
         special_set_mem : devices.Special_key_set := (others=> False);
      end record;

   type p_Keyboard is access all Keyboard;


   function Strike_once( c: Character;
                         keyboard : access devices.Keyboard) return Boolean;

   function Strike_once( special: Integer;
                         keyboard : access devices.Keyboard) return Boolean;


   default_Keyboard : aliased Keyboard;



   -- Mouse
   --

   type mouse_button_Set is array( glut.LEFT_BUTTON .. glut.RIGHT_BUTTON ) of Boolean;

   type Mouse is
      record
         oldx, oldy, mx, my : Integer          := 0;
         button_state       : mouse_button_Set := (others=> False);
      end record;

   type p_Mouse is access all Mouse;


   default_Mouse : aliased Mouse;


end glut.Devices;
