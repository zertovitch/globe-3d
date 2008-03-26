-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2000 Hans-Frieder Vogt                                --
--                                                                           --
--  Adabindx is free software; you can redistribute it and/or modify it      --
--  under the terms of the GNU General Public License as published by the    --
--  Free Software Foundation; either version 2 of the License, or (at your   --
--  option) any later version.                                               --
--                                                                           --
--  This program is distributed in the hope that it will be useful, but      --
--  WITHOUT ANY WARRANTY; without even the implied warranty of               --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  See the GNU General Public License for more details.                     --
--                                                                           --
--  You should have received a copy of the GNU General Public License        --
--  along with this program; if not, write to the                            --
--  Free Software Foundation, Inc.,                                          --
--  59 Temple Place - Suite 330,                                             --
--  Boston, MA 02111-1307, USA.                                              --
--                                                                           --
--  As a special exception, if other files instantiate generics from this    --
--  unit, or you link this unit with other files to produce an executable,   --
--  this unit does not by itself cause the resulting executable to be        --
--  covered by the GNU General Public License. This exception does not       --
--  however invalidate any other reasons why the executable file might be    --
--  covered by the GNU General Public License.                               --
--                                                                           --
--  X Window System is copyrighted by the X Consortium                       --
--  Motif(tm)       is copyrighted by the Open Software Foundation, Inc.     --
--                  and by The Open Group                                    --
--                                                                           --
--                                                                           --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--
-- HISTORY:
--          June 20, 1998 begin of history
--
-------------------------------------------------------------------------------

package X_Lib.Screen_Saver is

   type Screen_Saver_Mode is (Screen_Saver_Reset, Screen_Saver_Activate);

   type Blanking_Mode is (Dont_Prefer_Blanking,
                          Prefer_Blanking,
                          Default_Blanking);

   type Exposures_Mode is (Dont_Allow_Exposures,
                           Allow_Exposures,
                           Default_Exposures);


   procedure X_Activate_Screen_Saver (Display : in Display_Pointer);

   procedure X_Reset_Screen_Saver (Display : in Display_Pointer);

   procedure X_Force_Screen_Saver (Display : in Display_Pointer;
                                   Mode    : in Screen_Saver_Mode);

   procedure X_Get_Screen_Saver (Display         : in  Display_Pointer;
                                 Timeout         : out Integer;
                                 Interval        : out Integer;
                                 Prefer_Blanking : out Blanking_Mode;
                                 Allow_Exposures : out Exposures_Mode);

   procedure X_Set_Screen_Saver (Display         : in Display_Pointer;
                                 Timeout         : in Integer;
                                 Interval        : in Integer;
                                 Prefer_Blanking : in Blanking_Mode;
                                 Allow_Exposures : in Exposures_Mode);

private

   for Screen_Saver_Mode use (Screen_Saver_Reset    => 0,
                              Screen_Saver_Activate => 1);
   for Screen_Saver_Mode'Size use Interfaces.C.int'Size;


   for Blanking_Mode use (Dont_Prefer_Blanking => 0,
                          Prefer_Blanking      => 1,
                          Default_Blanking     => 2);
   for Blanking_Mode'Size use Interfaces.C.int'Size;


   for Exposures_Mode use (Dont_Allow_Exposures => 0,
                           Allow_Exposures      => 1,
                           Default_Exposures    => 2);
   for Exposures_Mode'Size use Interfaces.C.int'Size;

   pragma Import (C, X_Activate_Screen_Saver, "XActivateScreenSaver");
   pragma Import (C, X_Reset_Screen_Saver,    "XResetScreenSaver");
   pragma Import (C, X_Force_Screen_Saver,    "XForceScreenSaver");
   pragma Import (C, X_Get_Screen_Saver,      "XGetScreenSaver");
   pragma Import (C, X_Set_Screen_Saver,      "XGetScreenSaver");

end X_Lib.Screen_Saver;
