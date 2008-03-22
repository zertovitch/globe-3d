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

package X_Lib.Std_Colormap is

   -- -------------------------------------------------------------------------
   --
   --   Standard Colormap
   --

   type Standard_Colormap_Type is record
      Colormap   : Colormap_ID;
      Red_Max    : Interfaces.C.unsigned_long;
      Red_Mult   : Interfaces.C.unsigned_long;
      Green_Max  : Interfaces.C.unsigned_long;
      Green_Mult : Interfaces.C.unsigned_long;
      Blue_Max   : Interfaces.C.unsigned_long;
      Blue_Mult  : Interfaces.C.unsigned_long;
      Base_Pixel : Pixel;
      Vis_ID     : Visual_ID;
      Kill_ID    : XID;
   end record;
   pragma Convention (C, Standard_Colormap_Type);

   Release_By_Freeing_Colormap : constant XID := XID (1);

   type Standard_Colormap_Array_Type is
      array (Natural range <>) of Standard_Colormap_Type;

   -- for Property parameter use one of the following Atoms
   --    XA_RGB_DEFAULT_MAP  default selection of predefined color values
   --    XA_RGB_BEST_MAP     as many different color values as possible
   --    XA_RGB_RED_MAP      as many color shades of red as possible
   --    XA_RGB_GREEN_MAP    same for green
   --    XA_RGB_BLUE_MAP     same for blue
   --    XA_RGB_GRAY_MAP     as many grey shades as possible
   --

   function X_Get_RGB_Colormaps
     (Display  : in Display_Pointer;
      W        : in Window_ID;
      Property : in Atom)
      return Standard_Colormap_Array_Type;

   procedure X_Set_RGB_Colormaps
     (Display       : in Display_Pointer;
      W             : in Window_ID;
      Std_Colormaps : in Standard_Colormap_Array_Type;
      Property      : in Atom);


   function X_Get_Standard_Colormap
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Property     : in Atom)
      return Standard_Colormap_Type;

   procedure X_Set_Standard_Colormap
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Std_Colormap : in Standard_Colormap_Type;
      Property     : in Atom);

end X_Lib.Std_Colormap;
