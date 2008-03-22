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
--                        Host_Address is now in package X_Connection and
--                        called Net_Address
--
-------------------------------------------------------------------------------

with Generic_List_Access_Types,
     X_Connection;
package X_Lib.Host_Access is

   use X_Connection;


   type X_Host_Address (Address_Length : Natural) is record
      Family    : Protocol_Family;
      Host_Name : Net_Address (1 .. Address_Length);
   end record;
   type X_Host_Address_Access is access all X_Host_Address;

   package X_Host_Address_Lists is
      new Generic_List_Access_Types (X_Host_Address, X_Host_Address_Access);

   subtype X_Host_Address_List is X_Host_Address_Lists.Element_Access_List;
   Null_X_Host_Address_List : constant X_Host_Address_List
                            := X_Host_Address_Lists.Null_Element_Access_List;

   function Length (List : in X_Host_Address_List) return Natural;

   function Element
     (List   : in X_Host_Address_List;
      Index  : in Natural)
      return X_Host_Address;

   function "=" (Left, Right : in X_Host_Address_List) return Boolean;

   function "&" (Left, Right : in X_Host_Address_List) return X_Host_Address_List;
   function "&"
     (Left  : in X_Host_Address_List;
      Right : in X_Host_Address)
      return X_Host_Address_List;


   procedure X_Add_Host
      (Display         : in X_Lib.Display_Pointer;
       Host            : in X_Host_Address);

   procedure X_Add_Hosts
      (Display         : in X_Lib.Display_Pointer;
       Hosts           : in X_Host_Address_List);

   procedure X_List_Hosts
      (Display         : in     X_Lib.Display_Pointer;
       List            :    out X_Host_Address_List;
       Control_Enabled :    out Boolean);

   procedure X_Remove_Host
      (Display         : in X_Lib.Display_Pointer;
       Host            : in X_Host_Address);

   procedure X_Remove_Hosts
      (Display         : in X_Lib.Display_Pointer;
       Hosts           : in X_Host_Address_List);

   procedure X_Set_Access_Control
      (Display        : in X_Lib.Display_Pointer;
       Control_Enable : in Boolean);

   procedure X_Enable_Access_Control (Display : in X_Lib.Display_Pointer);

   procedure X_Disable_Access_Control (Display : in X_Lib.Display_Pointer);


private

   pragma Import (C, X_Enable_Access_Control, "XEnableAccessControl");
   pragma Import (C, X_Disable_Access_Control, "XDisableAccessControl");

end X_Lib.Host_Access;
