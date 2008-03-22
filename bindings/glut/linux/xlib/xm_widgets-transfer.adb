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
package body Xm_Widgets.Transfer is

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Convert_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Convert_Callback_Struct,
	    Xm_Convert_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Ok));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Destination_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Destination_Callback_Struct,
	    Xm_Destination_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Ok));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Selection_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Selection_Callback_Struct,
	    Xm_Selection_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Ok));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   -- don't forget: we need a trailing Null_Convert_Callback_Rec
   --
   function Length (List : in Convert_Callback_List) return Natural is
   begin
      if List = Null_Convert_Callback_List then
         return 0;
      else
         return Convert_Callback_Lists.Length (Convert_Callback_Lists.Unbounded_List (List))-1;
      end if;
   end Length;


   procedure Append (List : in out Convert_Callback_List;
                     Rec  : in     Convert_Callback_Rec) is
   begin
      if List = Null_Convert_Callback_List then
         Convert_Callback_Lists.Append (Convert_Callback_Lists.Unbounded_List (List),
                                        Rec);
      else
         Convert_Callback_Lists.Replace_Element (Convert_Callback_Lists.Unbounded_List (List),
                                                 Convert_Callback_Lists.Length (Convert_Callback_Lists.Unbounded_List (List)),
                                                 Rec);
      end if;
      Convert_Callback_Lists.Append (Convert_Callback_Lists.Unbounded_List (List),
                                     Null_Convert_Callback_Rec);
   end Append;


   procedure Append (List      : in out Convert_Callback_List;
                     Callback  : in     Xm_Convert_Callback_Proc;
		     Closure   : in     Xt_Pointer) is
      Rec : Convert_Callback_Rec := (Callback, Closure);
   begin
      Append (List, Rec);
   end Append;
   pragma Inline (Append);


   function To_Long is
      new Ada.Unchecked_Conversion (Convert_Callback_Lists.Element_Access, Interfaces.C.long);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Convert_Callback_List) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Convert_Callback_Lists.Hook (Convert_Callback_Lists.Unbounded_List (Value))));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Xt_Add_Callback
     (To          : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Convert_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtAddCallback (W    : in Widget;
                               Kind : in Xt_N_Resource_String;
                               Proc : in Xm_Convert_Callback_Proc;
                               Data : in Xt_Pointer);
      pragma Import (C, XtAddCallback, "XtAddCallback");
   begin
      XtAddCallback (W    => To,
                     Kind => Kind,
                     Proc => Callback,
                     Data => Client_Data);
   end Xt_Add_Callback;
   pragma Inline (Xt_Add_Callback);


   procedure Xt_Remove_Callback
     (From        : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Convert_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtRemoveCallback (W    : in Widget;
                                  Kind : in Xt_N_Resource_String;
                                  Proc : in Xm_Convert_Callback_Proc;
                                  Data : in Xt_Pointer);
      pragma Import (C, XtRemoveCallback, "XtRemoveCallback");
   begin
      XtRemoveCallback (W    => From,
                        Kind => Kind,
                        Proc => Callback,
                        Data => Client_Data);
   end Xt_Remove_Callback;
   pragma Inline (Xt_Remove_Callback);



   -- don't forget: we need a trailing Null_Destination_Callback_Rec
   --
   function Length (List : in Destination_Callback_List) return Natural is
   begin
      if List = Null_Destination_Callback_List then
         return 0;
      else
         return Destination_Callback_Lists.Length (Destination_Callback_Lists.Unbounded_List (List))-1;
      end if;
   end Length;


   procedure Append (List : in out Destination_Callback_List;
                     Rec  : in     Destination_Callback_Rec) is
   begin
      if List = Null_Destination_Callback_List then
         Destination_Callback_Lists.Append (Destination_Callback_Lists.Unbounded_List (List),
                                            Rec);
      else
         Destination_Callback_Lists.Replace_Element (Destination_Callback_Lists.Unbounded_List (List),
						     Destination_Callback_Lists.Length (Destination_Callback_Lists.Unbounded_List (List)),
                                                     Rec);
      end if;
      Destination_Callback_Lists.Append (Destination_Callback_Lists.Unbounded_List (List),
                                         Null_Destination_Callback_Rec);
   end Append;


   procedure Append (List      : in out Destination_Callback_List;
                     Callback  : in     Xm_Destination_Callback_Proc;
		     Closure   : in     Xt_Pointer) is
      Rec : Destination_Callback_Rec := (Callback, Closure);
   begin
      Append (List, Rec);
   end Append;
   pragma Inline (Append);


   function To_Long is new Ada.Unchecked_Conversion (Destination_Callback_Lists.Element_Access, Interfaces.C.long);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Destination_Callback_List) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Destination_Callback_Lists.Hook (Destination_Callback_Lists.Unbounded_List (Value))));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Xt_Add_Callback
     (To          : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Destination_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtAddCallback (W    : in Widget;
                               Kind : in Xt_N_Resource_String;
                               Proc : in Xm_Destination_Callback_Proc;
                               Data : in Xt_Pointer);
      pragma Import (C, XtAddCallback, "XtAddCallback");
   begin
      XtAddCallback (W    => To,
                     Kind => Kind,
                     Proc => Callback,
                     Data => Client_Data);
   end Xt_Add_Callback;
   pragma Inline (Xt_Add_Callback);


   procedure Xt_Remove_Callback
     (From        : in Widget;
      Kind        : in Xt_N_Resource_String;
      Callback    : in Xm_Destination_Callback_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtRemoveCallback (W    : in Widget;
                                  Kind : in Xt_N_Resource_String;
                                  Proc : in Xm_Destination_Callback_Proc;
                                  Data : in Xt_Pointer);
      pragma Import (C, XtRemoveCallback, "XtRemoveCallback");
   begin
      XtRemoveCallback (W    => From,
                        Kind => Kind,
                        Proc => Callback,
                        Data => Client_Data);
   end Xt_Remove_Callback;
   pragma Inline (Xt_Remove_Callback);



   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (C interface procedure)
   --
   procedure XmTransferSetParameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in System.Address;
      Parm_Fmt    : in X_Lib.Data_Format_Type;
      Parm_Length : in Interfaces.C.unsigned_long;
      Parm_Type   : in X_Lib.Atom);
   pragma Import (C, XmTransferSetParameters, "XmTransferSetParameters");


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is an 8 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_8_Array_Type;
      Parm_Type   : in X_Lib.Atom) is
   begin
      XmTransferSetParameters (Transfer_ID, Parm'Address, X_Lib.Bits_8, Parm'Length, Parm_Type);
   end Xm_Transfer_Set_Parameters;
   pragma Inline (Xm_Transfer_Set_Parameters);


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is a 16 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_16_Array_Type;
      Parm_Type   : in X_Lib.Atom) is
   begin
      XmTransferSetParameters (Transfer_ID, Parm'Address, X_Lib.Bits_16, Parm'Length, Parm_Type);
   end Xm_Transfer_Set_Parameters;
   pragma Inline (Xm_Transfer_Set_Parameters);


   -- set parameters to be passed by the following call to
   -- Xm_Transfer_Value (Data is a 32 bit type)
   --
   procedure Xm_Transfer_Set_Parameters
     (Transfer_Id : in Transfer_ID_Type;
      Parm        : in X_Lib.Bits_32_Array_Type;
      Parm_Type   : in X_Lib.Atom) is
   begin
      XmTransferSetParameters (Transfer_ID, Parm'Address, X_Lib.Bits_32, Parm'Length, Parm_Type);
   end Xm_Transfer_Set_Parameters;
   pragma Inline (Xm_Transfer_Set_Parameters);


end Xm_Widgets.Transfer;
