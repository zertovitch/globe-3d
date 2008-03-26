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

with System;
package body X_Misc_Utils is

   -- -------------------------------------------------------------------------
   --
   --  S T A N D A R D   C O L O R M A P
   --

   procedure Xmu_All_Standard_Colormaps (Display : in Display_Pointer) is
      function XmuAllStandardColormaps (Display : in Display_Pointer)
         return Status_Type;
      pragma Import (C, XmuAllStandardColormaps, "XmuAllStandardColormaps");
   begin
      if XmuAllStandardColormaps (Display) = Error_Status then
         raise Xmu_Error;
      end if;
   end Xmu_All_Standard_Colormaps;


   function Xmu_Create_Standard_Colormap (Display : in Display_Pointer)
      return Standard_Colormap_Type is
      function XmuCreateColormap (Display  : in Display_Pointer;
                                  Colormap : in System.Address)
         return Status_Type;
      pragma Import (C, XmuCreateColormap, "XmuCreateColormap");
      Std_Colormap : Standard_Colormap_Type;
   begin
      if XmuCreateColormap (Display, Std_Colormap'Address) = Error_Status then
         raise Xmu_Error;
      end if;
      return Std_Colormap;
   end Xmu_Create_Standard_Colormap;


   procedure Xmu_Get_Colormap_Allocation
     (Vinfo    : in     X_Visual_Info;
      Property : in     Atom;
      Red_Max,
      Green_Max,
      Blue_Max :    out Interfaces.C.unsigned_long) is
      function XmuGetColormapAllocation
        (Vinfo    : in System.Address;
         Property : in Atom;
         Red_Max,
         Green_Max,
         Blue_Max : in System.Address)
      return Status_Type;
      pragma Import (C, XmuGetColormapAllocation, "XmuGetColormapAllocation");
   begin
      if XmuGetColormapAllocation (Vinfo'Address, Property, Red_Max'Address,
                                   Green_Max'Address, Blue_Max'Address) = Error_Status then
         raise Xmu_Error;
      end if;
   end Xmu_Get_Colormap_Allocation;


   procedure Xmu_Lookup_Standard_Colormap
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Vis_ID   : in Visual_ID;
      Depth    : in Color_Depth;
      Property : in Atom;
      Replace  : in Boolean;
      Retain   : in Boolean) is
      function XmuLookupStandardColormap
        (Display  : in Display_Pointer;
         Screen   : in Screen_Number;
         Vis_ID   : in Visual_ID;
         Depth    : in Color_Depth;
         Property : in Atom;
         Replace  : in X_Lib.X_Boolean;
         Retain   : in X_Lib.X_Boolean)
         return Status_Type;
      pragma Import (C, XmuLookupStandardColormap, "XmuLookupStandardColormap");
   begin
      if XmuLookupStandardColormap (Display, Screen, Vis_ID, Depth, Property,
                                    X_Lib.To_X_Boolean (Replace),
				    X_Lib.To_X_Boolean (Retain)) = Error_Status then
         raise Xmu_Error;
      end if;
   end Xmu_Lookup_Standard_Colormap;


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
      return Standard_Colormap_Type is

      type Standard_Colormap_Access is
         access all Standard_Colormap_Type;

      function XmuStandardColormap
        (Display  : in Display_Pointer;
         Screen   : in Screen_Number;
         Vis_ID   : in Visual_ID;
         Depth    : in Color_Depth;
         Property : in Atom;
         Cmap     : in Colormap_ID;
         Red_Max,
         Green_Max,
         Blue_Max : in Interfaces.C.unsigned_long)
         return Standard_Colormap_Access;
      pragma Import (C, XmuStandardColormap, "XmuStandardColormap");

      procedure XFree (What : in Standard_Colormap_Access);
      pragma Import (C, XFree, "XFree");

      Std_Colormap     : Standard_Colormap_Type;
      Std_Colormap_Acc : Standard_Colormap_Access;
   begin
      Std_Colormap_Acc := XmuStandardColormap (Display, Screen, Vis_ID, Depth,
                                               Property, Cmap,
                                               Red_Max, Green_Max, Blue_Max);
      if Std_Colormap_Acc = null then
         raise Xmu_Error;
      else
         Std_Colormap := Std_Colormap_Acc.all;
         XFree (Std_Colormap_Acc);
         return Std_Colormap;
      end if;
   end Xmu_Standard_Colormap;


   procedure Xmu_Visual_Standard_Colormaps
     (Display  : in Display_Pointer;
      Screen   : in Screen_Number;
      Vis_ID   : in Visual_ID;
      Depth    : in Color_Depth;
      Replace  : in Boolean;
      Retain   : in Boolean) is
      function XmuVisualStandardColormaps
        (Display  : in Display_Pointer;
         Screen   : in Screen_Number;
         Vis_ID   : in Visual_ID;
         Depth    : in Color_Depth;
         Replace  : in X_Lib.X_Boolean;
         Retain   : in X_Lib.X_Boolean)
         return Status_Type;
      pragma Import (C, XmuVisualStandardColormaps, "XmuVisualStandardColormaps");
   begin
      if XmuVisualStandardColormaps (Display, Screen, Vis_ID, Depth,
                                     X_Lib.To_X_Boolean (Replace),
				     X_Lib.To_X_Boolean (Retain)) = Error_Status then
         raise Xmu_Error;
      end if;
   end Xmu_Visual_Standard_Colormaps;


end X_Misc_Utils;
