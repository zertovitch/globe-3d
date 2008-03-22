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

with Ada.Unchecked_Conversion;
package body X_Lib.Std_Colormap is

   -- -------------------------------------------------------------------------
   --
   --   Standard Colormap
   --


   function X_Get_RGB_Colormaps
     (Display  : in Display_Pointer;
      W        : in Window_ID;
      Property : in Atom)
      return Standard_Colormap_Array_Type is

      function XGetRGBColormaps
        (Display  : in Display_Pointer;
         W        : in Window_ID;
         Stdcmap  : in System.Address;
         Count    : in System.Address;
         Property : in Atom)
         return Status_Type;
      pragma Import (C, XGetRGBColormaps, "XGetRGBColormaps");

      Number        : Natural;
      Hook_To_Array : System.Address;

   begin
      if XGetRGBColormaps (Display, W, Hook_To_Array'Address, Number'Address,
                           Property) = Error_Status then
         raise X_Error;
      end if;
      declare
         subtype This_Array_Type is Standard_Colormap_Array_Type (1 .. Number);
	 type This_Array_Access_Type is access This_Array_Type;
	 function To_This_Array_Access_Type is
	    new Ada.Unchecked_Conversion (System.Address, This_Array_Access_Type);
         Std_Colmaps : This_Array_Type;
      begin
         Std_Colmaps := To_This_Array_Access_Type (Hook_To_Array).all;
         XFree (Hook_To_Array);
         return Std_Colmaps;
      end;
   end X_Get_RGB_Colormaps;


   procedure X_Set_RGB_Colormaps
     (Display       : in Display_Pointer;
      W             : in Window_ID;
      Std_Colormaps : in Standard_Colormap_Array_Type;
      Property      : in Atom) is
      procedure XSetRGBColormaps
        (Display       : in Display_Pointer;
         W             : in Window_ID;
         Std_Colormaps : in System.Address;
         Count         : in Integer;
         Property      : in Atom);
      pragma Import (C, XSetRGBColormaps, "XSetRGBColormaps");
   begin
      XSetRGBColormaps (Display, W, Std_Colormaps'Address, Std_Colormaps'Length,
                        Property);
   end X_Set_RGB_Colormaps;
   pragma Inline (X_Set_RGB_Colormaps);


   function X_Get_Standard_Colormap
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Property     : in Atom)
      return Standard_Colormap_Type is
      function XGetStandardColormap
        (Display      : in Display_Pointer;
         W            : in Window_ID;
         Std_Colormap : in System.Address;
         Property     : in Atom)
	 return Status_Type;
      pragma Import (C, XGetStandardColormap, "XGetStandardColormap");
      Return_Std_Colormap : Standard_Colormap_Type;
   begin
      if XGetStandardColormap (Display, W, Return_Std_Colormap'Address, Property) = Error_Status then
         raise X_Color_Error;
      end if;
      return Return_Std_Colormap;
   end X_Get_Standard_Colormap;


   procedure X_Set_Standard_Colormap
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Std_Colormap : in Standard_Colormap_Type;
      Property     : in Atom) is
      procedure XSetStandardColormap
        (Display      : in Display_Pointer;
         W            : in Window_ID;
         Std_Colormap : in System.Address;
         Property     : in Atom);
      pragma Import (C, XSetStandardColormap, "XSetStandardColormap");
   begin
      XSetStandardColormap (Display, W, Std_Colormap'Address, Property);
   end X_Set_Standard_Colormap;
   pragma Inline (X_Set_Standard_Colormap);

end X_Lib.Std_Colormap;
