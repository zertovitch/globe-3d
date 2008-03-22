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

with Interfaces.C;
with X_Lib.Std_Colormap,        X_Toolkit;
use  X_Lib.Std_Colormap, X_Lib;

package X_Misc_Utils is

   Xmu_Error : exception;

   -- -------------------------------------------------------------------------
   --
   --  E D I T R E S
   --

   procedure X_Edit_Res_Check_Messages
     (W                 : in     X_Toolkit.Widget;
      Closure           : in     X_Toolkit.Xt_Pointer;
      Event             : in     X_Lib.X_Event;
      Cont_To_Dispatch  : in out X_Toolkit.Xt_Boolean);


   -- -------------------------------------------------------------------------
   --
   --  S T A N D A R D   C O L O R M A P
   --

   procedure Xmu_All_Standard_Colormaps (Display : in Display_Pointer);

   -- originally named XmuCreateColormap, but renamed here for a more
   -- appropriate name
   function Xmu_Create_Standard_Colormap (Display : in Display_Pointer)
      return Standard_Colormap_Type;


   procedure Xmu_Delete_Standard_Colormap
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Property : in Atom);


   procedure Xmu_Get_Colormap_Allocation
     (Vinfo    : in     X_Visual_Info;
      Property : in     Atom;
      Red_Max,
      Green_Max,
      Blue_Max :    out Interfaces.C.unsigned_long);


   procedure Xmu_Lookup_Standard_Colormap
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Vis_ID   : in Visual_ID;
      Depth    : in Color_Depth;
      Property : in Atom;
      Replace  : in Boolean;
      Retain   : in Boolean);


   function Xmu_Standard_Colormap
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Vis_ID   : in Visual_ID;
      Depth    : in Color_Depth;
      Property : in Atom;
      Cmap     : in Colormap_ID;
      Red_Max,
      Green_Max,
      Blue_Max : in Interfaces.C.unsigned_long)
      return Standard_Colormap_Type;


   procedure Xmu_Visual_Standard_Colormaps
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Vis_ID   : in Visual_ID;
      Depth    : in Color_Depth;
      Replace  : in Boolean;
      Retain   : in Boolean);


private

   pragma Import (C, X_Edit_Res_Check_Messages, "_XEditResCheckMessages");
   pragma Import (C, Xmu_Delete_Standard_Colormap, "XmuDeleteStandardColormap");

end X_Misc_Utils;
