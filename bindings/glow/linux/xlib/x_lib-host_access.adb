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
package body X_Lib.Host_Access is


   function Length (List : in X_Host_Address_List) return Natural is
   begin
      return X_Host_Address_Lists.Length (X_Host_Address_Lists.Element_Access_List (List));
   end Length;
   pragma Inline (Length);


   function Element
     (List   : in X_Host_Address_List;
      Index  : in Natural)
      return X_Host_Address is
   begin
      return X_Host_Address_Lists.Element (X_Host_Address_Lists.Element_Access_List (List), Index);
   end Element;
   pragma Inline (Element);


   function "=" (Left, Right : in X_Host_Address_List) return Boolean is
   begin
      return X_Host_Address_Lists."=" (X_Host_Address_Lists.Element_Access_List (Left),
                                       X_Host_Address_Lists.Element_Access_List (Right));
   end "=";
   pragma Inline ("=");


   function "&" (Left, Right : in X_Host_Address_List) return X_Host_Address_List is
   begin
      return X_Host_Address_Lists."&" (X_Host_Address_Lists.Element_Access_List (Left),
                                       X_Host_Address_Lists.Element_Access_List (Right));
   end "&";
   pragma Inline ("&");


   function "&"
     (Left  : in X_Host_Address_List;
      Right : in X_Host_Address)
      return X_Host_Address_List is
   begin
      return X_Host_Address_Lists."&" (X_Host_Address_Lists.Element_Access_List (Left),
                                       Right);
   end "&";
   pragma Inline ("&");



   --
   -- C representation of a host address record
   --
   type X_Host_Address_Private is record
      Family         : Protocol_Family;
      Address_Length : Integer;
      Host_Name      : System.Address;
   end record;
   for X_Host_Address_Private use record
      Family         at 0 range 0 .. 31;   -- important: in Xlib.h this is an int
      Address_Length at 4 range 0 .. 31;
      Host_Name      at 8 range 0 .. 31;
   end record;

   type X_Host_Address_Private_Array is
      array (Positive range <>) of aliased X_Host_Address_Private;


   procedure X_Add_Host
      (Display         : in X_Lib.Display_Pointer;
       Host            : in X_Host_Address) is
      procedure XAddHost
         (Display         : in X_Lib.Display_Pointer;
          Host            : in System.Address);
      pragma Import (C, XAddHost, "XAddHost");

      C_Host_Address : X_Host_Address_Private := (Host.Family,
                                                  Host.Address_Length,
                                                  Host.Host_Name'Address);
   begin
      XAddHost (Display, C_Host_Address'Address);
   end X_Add_Host;


   procedure X_Add_Hosts
      (Display         : in X_Lib.Display_Pointer;
       Hosts           : in X_Host_Address_List) is
      procedure XAddHosts
         (Display         : in X_Lib.Display_Pointer;
          Hosts           : in System.Address;
          Num_Hosts       : in Integer);
      pragma Import (C, XAddHosts, "XAddHosts");
      N_Hosts   : constant Integer := X_Host_Address_Lists.Length (Hosts);
      Host_Arry : X_Host_Address_Lists.Element_Array (1 .. N_Hosts)
                := X_Host_Address_Lists.To_Element_Array (Hosts);
      Priv_Host_Arry : X_Host_Address_Private_Array (1 .. N_Hosts);
   begin
      for I in 1 .. N_Hosts loop
         Priv_Host_Arry (I).Family         := Host_Arry (I).Family;
         Priv_Host_Arry (I).Address_Length := Host_Arry (I).Address_Length;
         Priv_Host_Arry (I).Host_Name      := Host_Arry (I).Host_Name'Address;
      end loop;
      XAddHosts (Display, Priv_Host_Arry'Address, N_Hosts);
   end X_Add_Hosts;



   procedure X_List_Hosts
     (Display         : in     X_Lib.Display_Pointer;
      List            :    out X_Host_Address_List;
      Control_Enabled :    out Boolean) is

      function XListHosts (Display : in X_Lib.Display_Pointer;
                           N_Hosts : in System.Address;
                           State   : in System.Address)
         return System.Address;
      pragma Import (C, XListHosts, "XListHosts");

      Hook_To_Array   : System.Address;
      Number          : Integer;
      Access_State    : Integer;

   begin
      Hook_To_Array := XListHosts (Display, Number'Address, Access_State'Address);
      
      Control_Enabled := Access_State /= 0;
      declare
         subtype Our_Hosts is X_Host_Address_Private_Array (1 .. Number);
	 type Our_Hosts_Access is access all Our_Hosts;
	 
         function To_Access is
	    new Ada.Unchecked_Conversion (System.Address, Our_Hosts_Access);

         Host_List : Our_Hosts_Access := To_Access (Hook_To_Array);
      begin
         for I in 1 .. Number loop
            declare
               type XA_Acc is
                  access all Net_Address (1 .. Host_List (I).Address_Length);
               function To_Array is
                  new Ada.Unchecked_Conversion (System.Address, XA_Acc);
            begin
               X_Host_Address_Lists.Append (List,
                                            X_Host_Address'(Host_List (I).Address_Length,
                                            Host_List (I).Family,
                                            To_Array (Host_List (I).Host_Name).all));
            end;
         end loop;
      end;
      -- now that we have copied all interesting data we can free the
      -- list no longer needed
      XFree (Hook_To_Array);
   end X_List_Hosts;


   procedure X_Remove_Host
      (Display         : in X_Lib.Display_Pointer;
       Host            : in X_Host_Address) is
      procedure XRemoveHost
         (Display         : in X_Lib.Display_Pointer;
          Host            : in System.Address);
      pragma Import (C, XRemoveHost, "XRemoveHost");

      C_Host_Address : X_Host_Address_Private := (Host.Family,
                                                  Host.Address_Length,
                                                  Host.Host_Name'Address);
   begin
      XRemoveHost (Display, C_Host_Address'Address);
   end X_Remove_Host;


   procedure X_Remove_Hosts
      (Display         : in X_Lib.Display_Pointer;
       Hosts           : in X_Host_Address_List) is
      procedure XRemoveHosts
         (Display         : in X_Lib.Display_Pointer;
          Hosts           : in System.Address;
          Num_Hosts       : in Integer);
      pragma Import (C, XRemoveHosts, "XRemoveHosts");
      N_Hosts   : constant Integer := X_Host_Address_Lists.Length (Hosts);
      Host_Arry : X_Host_Address_Lists.Element_Array (1 .. N_Hosts)
                := X_Host_Address_Lists.To_Element_Array (Hosts);
      Priv_Host_Arry : X_Host_Address_Private_Array (1 .. N_Hosts);
   begin
      for I in 1 .. N_Hosts loop
         Priv_Host_Arry (I).Family         := Host_Arry (I).Family;
         Priv_Host_Arry (I).Address_Length := Host_Arry (I).Address_Length;
         Priv_Host_Arry (I).Host_Name      := Host_Arry (I).Host_Name'Address;
      end loop;
      XRemoveHosts (Display, Priv_Host_Arry'Address, N_Hosts);
   end X_Remove_Hosts;


   procedure X_Set_Access_Control
      (Display        : in X_Lib.Display_Pointer;
       Control_Enable : in Boolean) is
      procedure XSetAccessControl
         (Display        : in X_Lib.Display_Pointer;
          Control_Enable : in Integer);
      pragma Import (C, XSetAccessControl, "XSetAccessControl");
   begin
      if Control_Enable then
         XSetAccessControl (Display, 1);
      else
         XSetAccessControl (Display, 0);
      end if;
   end X_Set_Access_Control;


end X_Lib.Host_Access;
