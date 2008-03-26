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

with Ada.IO_Exceptions,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation,
     Interfaces.C.Strings,
     System;
package body X_Auth is

   type Internal_Xauth is record
      Family         : Protocol_Family;
      Address_length : Interfaces.C.unsigned_short;
      Address        : System.Address;
      Number_Length  : Interfaces.C.unsigned_short;
      Number         : System.Address;
      Name_Length    : Interfaces.C.unsigned_short;
      Name           : System.Address;
      Data_Length    : Interfaces.C.unsigned_short;
      Data           : System.Address;
   end record;

   type Internal_Xauth_Access is access all Internal_Xauth;


   -- C-Style FILE*
   --
   type C_File_Pointer is new System.Address;
   Null_C_File_Pointer : constant C_File_Pointer
                       := C_File_Pointer (System.Null_Address);

   function C_Fopen (Name, Modes : in System.Address) return C_File_Pointer;
   pragma Import (C, C_Fopen, "fopen");

   procedure C_Fclose (Stream : in C_File_Pointer);
   pragma Import (C, C_Fclose, "fclose");


   -- frees the storage associated with Internal_Xauth_Access
   --
   procedure XauDisposeAuth (Auth : in Internal_Xauth_Access);
   pragma Import (C, XauDisposeAuth, "XauDisposeAuth");


   -- a conversion we sometimes need
   --
   function To_Chars_Ptr is
      new Ada.Unchecked_Conversion (System.Address,
                                    Interfaces.C.Strings.Chars_Ptr);


   function Xau_File_Name return String is
      function XauFileName return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, XauFileName, "XauFileName");
      use Interfaces.C.Strings;
      Ptr : Chars_Ptr;
   begin
      Ptr := XauFileName;
      if Ptr = Null_Ptr then
         return "";
      else
         -- XauFileName returns a statically allocated string and therefore
	 -- musn't be freed!
         return Value (Ptr);
      end if;
   end Xau_File_Name;


   function Xau_Read_Auth (Auth_File : in String) return Xauth_List is
      function XauReadAuth (Auth_File : in C_File_Pointer)
         return Internal_Xauth_Access;
      pragma Import (C, XauReadAuth, "XauReadAuth");

      Auth_File_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Auth_File,
			                     Append_Nul => True);
      Modes_String     : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C ("r",
			                     Append_Nul => True);
      Filp             : C_File_Pointer;

      type Auth_List_Access is access all Xauth_List;
      procedure Free is
         new Ada.Unchecked_Deallocation (Xauth_List, Auth_List_Access);

      Alloc_Step      : constant Natural := 10;
      Num_XAuth_Found : Natural := 0;
      Xau_Acc         : Internal_Xauth_Access;
      Ret_List        : Auth_List_Access;
   begin
      Filp := C_Fopen (Auth_File_String'Address, Modes_String'Address);
      if Filp = Null_C_File_Pointer then
         raise Ada.IO_Exceptions.Name_Error;
      else
         Ret_List      := new Xauth_List (1 .. Alloc_Step);
         loop
	    Xau_Acc := XauReadAuth (Filp);
	    exit when Xau_Acc = null;
	    Num_XAuth_Found := Num_XAuth_Found + 1;

            -- if array Ret_List is to small for result, make it bigger
            if Num_Xauth_Found > Ret_List'Length then
	       declare
	          Tmp_Ret_List : Auth_List_Access;
	       begin
	          Tmp_Ret_List := new Xauth_List (1 .. Ret_List'Length+Alloc_Step);
                  for I in 1 .. Ret_List'Length loop
		     Tmp_Ret_List (I) := Ret_List (I);
		  end loop;
                  Free (Ret_List);
		  Ret_List := Tmp_Ret_List;
	       end;
	    end if;

            Ret_List (Num_XAuth_Found) := new Xauth (Natural (Xau_Acc.Address_Length),
                             	                     Natural (Xau_Acc.Number_Length),
						     Natural (Xau_Acc.Name_Length),
						     Natural (Xau_Acc.Data_Length));

            Ret_List (Num_XAuth_Found).Family  := Xau_Acc.Family;
            Ret_List (Num_XAuth_Found).Display_Number :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Number),
	                                   Interfaces.C.Size_T (Xau_Acc.Number_Length));
            Ret_List (Num_XAuth_Found).Auth_Name      :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Name),
	                                   Interfaces.C.Size_T (Xau_Acc.Name_Length));
            declare
	       subtype My_Net_Address is Net_Address (1 .. Natural (Xau_Acc.Address_Length));
	       type My_Net_Address_Access is access all My_Net_Address;
	       function To_My_Net_Address_Access is
	          new Ada.Unchecked_Conversion (System.Address,
		                                My_Net_Address_Access);
               subtype My_Byte_Data is Byte_Data (1 .. Natural (Xau_Acc.Data_Length));
               type My_Byte_Data_Access is access all My_Byte_Data;
	       function To_My_Byte_Data_Access is
	          new Ada.Unchecked_Conversion (System.Address,
		                                My_Byte_Data_Access);

               Host_Addr_Acc : My_Net_Address_Access
	                     := To_My_Net_Address_Access (Xau_Acc.Address);
               Byte_Data_Acc : My_Byte_Data_Access
	                     := To_My_Byte_Data_Access (Xau_Acc.Data);
	    begin
               Ret_List (Num_XAuth_Found).Address        := Host_Addr_Acc.all;
               Ret_List (Num_XAuth_Found).Auth_Data      := Byte_Data_Acc.all;
            end;
            XauDisposeAuth (Xau_Acc);
	 end loop;

         C_Fclose (Filp);

         return Ret_List (1 .. Num_XAuth_Found);
      end if;
   end Xau_Read_Auth;


   function XauWriteAuth
     (Auth_File : in C_File_Pointer;
      Auth	: in System.Address)
      return Integer;
   pragma Import (C, XauWriteAuth, "XauWriteAuth");


   procedure Xau_Write_Auth (Auth_File : in String; Auth : in Xauth) is
      Auth_File_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Auth_File,
			                     Append_Nul => True);
      Modes_String     : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C ("w",
			                     Append_Nul => True);
      Filp             : C_File_Pointer;
      Xau              : Internal_Xauth;
      Ret_Val          : Integer;
   begin
      Filp := C_Fopen (Auth_File_String'Address, Modes_String'Address);
      if Filp = Null_C_File_Pointer then
         raise Ada.IO_Exceptions.Name_Error;
      else
         Xau := (Family => Auth.Family,
	         Address_Length => Interfaces.C.unsigned_short (Auth.Address_Length),
		 Address        => Auth.Address'Address,
		 Number_Length  => Interfaces.C.unsigned_short (Auth.Number_Length),
		 Number         => Auth.Display_Number'Address,
		 Name_Length    => Interfaces.C.unsigned_short (Auth.Name_Length),
		 Name           => Auth.Auth_Name'Address,
		 Data_Length    => Interfaces.C.unsigned_short (Auth.Data_Length),
		 Data           => Auth.Auth_Data'Address);
         Ret_Val := XauWriteAuth (Filp, Xau'Address);
         C_Fclose (Filp);
         if Ret_Val /= 1 then
	    raise Xau_Error;
	 end if;
      end if;
   end Xau_Write_Auth;


   procedure Xau_Append_Auth (Auth_File : in String; Auth : in Xauth) is
      Auth_File_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Auth_File,
			                     Append_Nul => True);
      Modes_String     : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C ("a",
			                     Append_Nul => True);
      Filp             : C_File_Pointer;
      Xau              : Internal_Xauth;
      Ret_Val          : Integer;
   begin
      Filp := C_Fopen (Auth_File_String'Address, Modes_String'Address);
      if Filp = Null_C_File_Pointer then
         raise Ada.IO_Exceptions.Name_Error;
      else
         Xau := (Family => Auth.Family,
	         Address_Length => Interfaces.C.unsigned_short (Auth.Address_Length),
		 Address        => Auth.Address'Address,
		 Number_Length  => Interfaces.C.unsigned_short (Auth.Number_Length),
		 Number         => Auth.Display_Number'Address,
		 Name_Length    => Interfaces.C.unsigned_short (Auth.Name_Length),
		 Name           => Auth.Auth_Name'Address,
		 Data_Length    => Interfaces.C.unsigned_short (Auth.Data_Length),
		 Data           => Auth.Auth_Data'Address);
         Ret_Val := XauWriteAuth (Filp, Xau'Address);
         C_Fclose (Filp);
         if Ret_Val /= 1 then
	    raise Xau_Error;
	 end if;
      end if;
   end Xau_Append_Auth;


   Lock_Success : constant := 0;
   Lock_Error   : constant := 1;
   Lock_Timeout : constant := 2;


   procedure Xau_Lock_Auth
     (Auth_File : in String;
      Retries   : in Natural;
      Timeout   : in Natural;
      Dead      : in Interfaces.C.long) is
      function XauLockAuth
        (Auth_File : in System.Address;
         Retries   : in Natural;
         Timeout   : in Natural;
         Dead      : in Interfaces.C.long)
	 return Integer;
      pragma Import (C, XauLockAuth, "XauLockAuth");
      Auth_File_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Auth_File,
			                     Append_Nul => True);
   begin
      case XauLockAuth (Auth_File_String'Address, Retries, Timeout, Dead) is
         when Lock_Success =>
	    return;
	 when Lock_Timeout =>
	    raise Xau_Timeout_Error;
	 when others =>
	    raise Xau_Error;
      end case;
   end Xau_Lock_Auth;


   procedure Xau_Unlock_Auth (Auth_File : in String) is
      function XauUnlockAuth (Auth_File : in System.Address)
         return Integer;
      pragma Import (C, XauUnlockAuth, "XauUnlockAuth");
      Auth_File_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Auth_File,
			                     Append_Nul => True);
      Ret_Val : Integer;
   begin
      -- shouldn't we do some error checking here?
      Ret_Val := XauUnlockAuth (Auth_File_String'Address);
   end Xau_Unlock_Auth;


   -- look for authorization information for the display Display_Name
   -- not any longer available in Xau-Library (as of Xfree86 4.1.0) ?!
   --
   -- function Xau_Get_Auth_By_Name (Display_Name : in String) return Xauth is
   --    function XauGetAuthByName (Display_Name : in System.Address)
   --       return Internal_Xauth_Access;
   --    pragma Import (C, XauGetAuthByName, "XauGetAuthByName");
   --    Display_Name_String : constant Interfaces.C.Char_Array
   --                        := Interfaces.C.To_C (Display_Name,
   -- 			                        Append_Nul => True);
   --    Xau_Acc : Internal_Xauth_Access;
   -- begin
   --    Xau_Acc := XauGetAuthByName (Display_Name_String'Address);
   --    if Xau_Acc /= null then
   --       declare
   -- 	    Ret_Xauth : Xauth (Natural (Xau_Acc.Address_Length),
   --                             Natural (Xau_Acc.Number_Length),
   -- 			       Natural (Xau_Acc.Name_Length),
   -- 			       Natural (Xau_Acc.Data_Length));
   -- 
   -- 	    subtype My_Net_Address is Net_Address (1 .. Natural (Xau_Acc.Address_Length));
   -- 	    type My_Net_Address_Access is access all My_Net_Address;
   -- 	    function To_My_Net_Address_Access is
   -- 	       new Ada.Unchecked_Conversion (System.Address,
   -- 	     				     My_Net_Address_Access);
   --          subtype My_Byte_Data is Byte_Data (1 .. Natural (Xau_Acc.Data_Length));
   --          type My_Byte_Data_Access is access all My_Byte_Data;
   -- 	    function To_My_Byte_Data_Access is
   -- 	       new Ada.Unchecked_Conversion (System.Address,
   -- 	     				     My_Byte_Data_Access);
   -- 
   --          Host_Addr_Acc : My_Net_Address_Access
   -- 	    		  := To_My_Net_Address_Access (Xau_Acc.Address);
   --          Byte_Data_Acc : My_Byte_Data_Access
   -- 	    		  := To_My_Byte_Data_Access (Xau_Acc.Data);
   --       begin
   --          Ret_Xauth.Family  := Xau_Acc.Family;
   --          Ret_Xauth.Display_Number :=
   -- 	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Number),
   -- 	                                   Interfaces.C.Size_T (Xau_Acc.Number_Length));
   --          Ret_Xauth.Auth_Name      :=
   -- 	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Name),
   -- 	                                   Interfaces.C.Size_T (Xau_Acc.Name_Length));
   --          Ret_Xauth.Address        := Host_Addr_Acc.all;
   --          Ret_Xauth.Auth_Data      := Byte_Data_Acc.all;
   --          return Ret_Xauth;
   --       end;
   --    else
   --       raise Xau_Error;
   --    end if;
   -- end Xau_Get_Auth_By_Name;


   function Xau_Get_Auth_By_Addr
     (Family   : in Protocol_Family;
      Address  : in Net_Address;
      Number   : in String;
      Name     : in String)
      return Xauth is
-- Use64Bit
--!       function XauGetAuthByAddr
--!         (Family         : in Interfaces.C.unsigned;
--! 	 Address_Length : in Interfaces.C.unsigned;
--! 	 Address        : in System.Address;
--! 	 Number_Length  : in Interfaces.C.unsigned;
--! 	 Number         : in System.Address;
--! 	 Name_Length    : in Interfaces.C.unsigned;
--! 	 Name           : in System.Address)
--!          return Internal_Xauth_Access;
-- Not64Bit
      function XauGetAuthByAddr
        (Family         : in Protocol_Family;
	 Address_Length : in Interfaces.C.unsigned_short;
	 Address        : in System.Address;
	 Number_Length  : in Interfaces.C.unsigned_short;
	 Number         : in System.Address;
	 Name_Length    : in Interfaces.C.unsigned_short;
	 Name           : in System.Address)
         return Internal_Xauth_Access;
-- End64Bit
      pragma Import (C, XauGetAuthByAddr, "XauGetAuthByAddr");
      Xau_Acc : Internal_Xauth_Access;
   begin
-- Use64Bit
--!       Xau_Acc := XauGetAuthByAddr (Interfaces.C.unsigned (Family),
--!                                    Interfaces.C.unsigned (Address'Length),
--! 				   Address'Address,
--! 				   Interfaces.C.unsigned (Number'Length),
--! 				   Number'Address,
--! 				   Interfaces.C.unsigned (Name'Length),
--! 				   Name'Address);
-- Not64Bit
      Xau_Acc := XauGetAuthByAddr (Family,
                                   Interfaces.C.unsigned_short (Address'Length),
				   Address'Address,
				   Interfaces.C.unsigned_short (Number'Length),
				   Number'Address,
				   Interfaces.C.unsigned_short (Name'Length),
				   Name'Address);
-- End64Bit
      if Xau_Acc /= null then
         declare
	    Ret_Xauth : Xauth (Natural (Xau_Acc.Address_Length),
                               Natural (Xau_Acc.Number_Length),
			       Natural (Xau_Acc.Name_Length),
			       Natural (Xau_Acc.Data_Length));

	    subtype My_Net_Address is Net_Address (1 .. Natural (Xau_Acc.Address_Length));
	    type My_Net_Address_Access is access all My_Net_Address;
	    function To_My_Net_Address_Access is
	       new Ada.Unchecked_Conversion (System.Address,
	     				     My_Net_Address_Access);
            subtype My_Byte_Data is Byte_Data (1 .. Natural (Xau_Acc.Data_Length));
            type My_Byte_Data_Access is access all My_Byte_Data;
	    function To_My_Byte_Data_Access is
	       new Ada.Unchecked_Conversion (System.Address,
	     				     My_Byte_Data_Access);

            Host_Addr_Acc : My_Net_Address_Access
	    		  := To_My_Net_Address_Access (Xau_Acc.Address);
            Byte_Data_Acc : My_Byte_Data_Access
	    		  := To_My_Byte_Data_Access (Xau_Acc.Data);
         begin
            Ret_Xauth.Family  := Xau_Acc.Family;
            Ret_Xauth.Display_Number :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Number),
	                                   Interfaces.C.Size_T (Xau_Acc.Number_Length));
            Ret_Xauth.Auth_Name      :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Name),
	                                   Interfaces.C.Size_T (Xau_Acc.Name_Length));
            Ret_Xauth.Address        := Host_Addr_Acc.all;
            Ret_Xauth.Auth_Data      := Byte_Data_Acc.all;
            return Ret_Xauth;
         end;
      else
         raise Xau_Error;
      end if;
   end Xau_Get_Auth_By_Addr;


   function Xau_Get_Best_Auth_By_Addr
     (Family     : in Protocol_Family;
      Address    : in Net_Address;
      Number     : in String;
      Auth_Types : in String_List.Element_Access_List)
      return Xauth is
-- Use64Bit
--!       function XauGetBestAuthByAddr
--!         (Family         : in Interfaces.C.unsigned;
--!          Address_Length : in Interfaces.C.unsigned;
--!          Address        : in System.Address;
--!          Number_Length  : in Interfaces.C.unsigned;
--!          Number         : in System.Address;
--!          Types_Length   : in Integer;
--!          Types_Names    : in System.Address;
--!          Types_Lengths  : in System.Address)
--!          return Internal_Xauth_Access;
-- Not64Bit
      function XauGetBestAuthByAddr
        (Family         : in Protocol_Family;
	 Address_Length : in Interfaces.C.unsigned_short;
	 Address        : in System.Address;
	 Number_Length  : in Interfaces.C.unsigned_short;
	 Number         : in System.Address;
	 Types_Length	: in Integer;
	 Types_Names	: in System.Address;
	 Types_Lengths  : in System.Address)
         return Internal_Xauth_Access;
-- End64Bit
      pragma Import (C, XauGetBestAuthByAddr, "XauGetBestAuthByAddr");
      Xau_Acc : Internal_Xauth_Access;

      Elem_Array : constant String_List.Element_Array
                 := String_List.To_Element_Array (Auth_Types);
      type Address_List is array (Natural range <>) of System.Address;
      type Integer_List is array (Natural range <>) of Integer;

      Adr_List : Address_List (1 .. Elem_Array'Length);
      Int_List : Integer_List (1 .. Elem_Array'Length);
   begin
      for I in 1 .. Elem_Array'Length loop
         Adr_List (I) := Elem_Array (I)'Address;
	 Int_List (I) := Elem_Array (I).all'Length;
      end loop;
-- Use64Bit
--!       Xau_Acc := XauGetBestAuthByAddr (Interfaces.C.unsigned (Family),
--!                                        Interfaces.C.unsigned (Address'Length),
--! 				           Address'Address,
--! 				           Interfaces.C.unsigned (Number'Length),
--! 				           Number'Address,
--!                                        Elem_Array'Length,
--!                                        Adr_List'Address,
--!                                        Int_List'Address);
-- Not64Bit
      Xau_Acc := XauGetBestAuthByAddr (Family,
                                       Interfaces.C.unsigned_short (Address'Length),
				       Address'Address,
				       Interfaces.C.unsigned_short (Number'Length),
				       Number'Address,
				       Elem_Array'Length,
				       Adr_List'Address,
				       Int_List'Address);
-- End64Bit
      if Xau_Acc /= null then
         declare
	    Ret_Xauth : Xauth (Natural (Xau_Acc.Address_Length),
                               Natural (Xau_Acc.Number_Length),
			       Natural (Xau_Acc.Name_Length),
			       Natural (Xau_Acc.Data_Length));

	    subtype My_Net_Address is Net_Address (1 .. Natural (Xau_Acc.Address_Length));
	    type My_Net_Address_Access is access all My_Net_Address;
	    function To_My_Net_Address_Access is
	       new Ada.Unchecked_Conversion (System.Address,
	     				     My_Net_Address_Access);
            subtype My_Byte_Data is Byte_Data (1 .. Natural (Xau_Acc.Data_Length));
            type My_Byte_Data_Access is access all My_Byte_Data;
	    function To_My_Byte_Data_Access is
	       new Ada.Unchecked_Conversion (System.Address,
	     				     My_Byte_Data_Access);

            Host_Addr_Acc : My_Net_Address_Access
	    		  := To_My_Net_Address_Access (Xau_Acc.Address);
            Byte_Data_Acc : My_Byte_Data_Access
	    		  := To_My_Byte_Data_Access (Xau_Acc.Data);
         begin
            Ret_Xauth.Family  := Xau_Acc.Family;
            Ret_Xauth.Display_Number :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Number),
	                                   Interfaces.C.Size_T (Xau_Acc.Number_Length));
            Ret_Xauth.Auth_Name      :=
	       Interfaces.C.Strings.Value (To_Chars_Ptr (Xau_Acc.Name),
	                                   Interfaces.C.Size_T (Xau_Acc.Name_Length));
            Ret_Xauth.Address        := Host_Addr_Acc.all;
            Ret_Xauth.Auth_Data      := Byte_Data_Acc.all;
            return Ret_Xauth;
         end;
      else
         raise Xau_Error;
      end if;
   end Xau_Get_Best_Auth_By_Addr;


end X_Auth;
