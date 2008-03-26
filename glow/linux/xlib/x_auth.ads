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
--          September 20, 1998 first version of package
--
-------------------------------------------------------------------------------

with Interfaces.C,
     String_List,
     X_Connection;
package X_Auth is

   use X_Connection;

   -- additional Families for the connection
   --
   Family_Local          : constant Protocol_Family := 256;   -- not part of X standard
   Family_Wild           : constant Protocol_Family := 65535;
   Family_Netname        : constant Protocol_Family := 254;   -- not part of X standard
   Family_Krb5_Principal : constant Protocol_Family := 253;   -- Kerberos 5 principal name
   Family_Local_Host     : constant Protocol_Family := 252;   -- for local non-net authentication


   type Byte_Data is
      array (Natural range <>) of Interfaces.C.unsigned_char;


   type Xauth (Address_Length,
               Number_Length,
	       Name_Length,
	       Data_Length : Natural) is record
      Family         : Protocol_Family;
      Address        : Net_Address (1 .. Address_Length);
      Display_Number : String (1 .. Number_Length);   
      Auth_Name      : String (1 .. Name_Length);
      Auth_Data      : Byte_Data (1 .. Data_Length);
   end record;

   type Xauth_Access is access all Xauth;
   type Xauth_List is
      array (Natural range <>) of Xauth_Access;


   -- errors
   --
   Xau_Error         : exception;   -- general error
   Xau_Timeout_Error : exception;


   -- return the file which is used to store the authorization information
   --
   function Xau_File_Name return String;


   -- return the complete authorization information which is stored in Auth_File
   -- WARNING: this function is NOT identical to the corresponding C-Interface
   --          function
   -- raises Ada.IO_Exceptions.Name_Error if Auth_File is not readable
   --
   function Xau_Read_Auth (Auth_File : in String) return Xauth_List;


   -- write authorization information to the authorization file Auth_File
   -- raises Ada.IO_Exceptions.Name_Error if Auth_File is not writable
   -- and Xau_Error if other errors occur
   --
   procedure Xau_Write_Auth (Auth_File : in String; Auth : in Xauth);

   -- same as Xau_Write_Auth, but information is appended, not just written
   --
   procedure Xau_Append_Auth (Auth_File : in String; Auth : in Xauth);


   -- synchronize the updating of the authorization file
   -- raises Xau_Error if something goes wrong with the filename or a
   -- system call,
   -- raises Xau_Timeout_Error if Retries retries failed (between two attempts
   -- Timeout seconds are waited)
   --
   procedure Xau_Lock_Auth
     (Auth_File : in String;
      Retries   : in Natural;
      Timeout   : in Natural;
      Dead      : in Interfaces.C.long);

   -- remove lock for the authorization file
   --
   procedure Xau_Unlock_Auth (Auth_File : in String);


   -- look for authorization information for the display Display_Name
   -- not any longer available in Xau-Library (as of Xfree86 4.1.0) ?!
   --
   -- function Xau_Get_Auth_By_Name (Display_Name : in String) return Xauth;


   -- look for authorization information
   --
   function Xau_Get_Auth_By_Addr
     (Family   : in Protocol_Family;
      Address  : in Net_Address;
      Number   : in String;
      Name     : in String)
      return Xauth;


   -- look for authorization information
   --
   function Xau_Get_Best_Auth_By_Addr
     (Family     : in Protocol_Family;
      Address    : in Net_Address;
      Number     : in String;
      Auth_Types : in String_List.Element_Access_List)
      return Xauth;

end X_Auth;
