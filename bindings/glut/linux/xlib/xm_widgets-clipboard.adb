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
--          November 8, 1998 HFVogt: rename exceptions
--
-------------------------------------------------------------------------------

with Interfaces.C.Pointers;
package body Xm_Widgets.Clipboard is

   -- Clipboard-Status
   XmClipboard_Fail         : constant Integer :=  0;
   XmClipboard_Success      : constant Integer :=  1; 
   XmClipboard_Truncate     : constant Integer :=  2;
   XmClipboard_Locked       : constant Integer :=  4;
   XmClipboard_Bad_Format   : constant Integer :=  5;
   XmClipboard_No_Data      : constant Integer :=  6;


   procedure Check_Return_Values (Ret_Val : in Integer) is
   begin
      case Ret_Val is
         when XmClipboard_Fail =>
            raise Fail_Error;
         when XmClipboard_Truncate =>
            raise Truncate_Error;
         when XmClipboard_Locked =>
            raise Locked_Error;
         when XmClipboard_Bad_Format =>
            raise Bad_Format_Error;
         when XmClipboard_No_Data =>
            raise No_Data_Error;
         when others =>
            null;
      end case;
   end Check_Return_Values;
   pragma Inline (Check_Return_Values);


   --
   -- Clipboard Begin Copy
   --
   procedure Xm_Clipboard_Begin_Copy
     (Display     : in X_Lib.Display_Pointer;
      Window      : in X_Lib.Window_ID;
      Label       : in Xm_String;
      W           : in Widget;
      Callback    : in Void_Proc;
      Itemid      : out ID_Type) is
      function XmClipboardBeginCopy
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Label       : in Xm_String;
         W	     : in Widget;
         Callback    : in Void_Proc;
         Itemid      : in System.Address)
	 return Integer;
      pragma Import (C, XmClipboardBeginCopy, "XmClipboardBeginCopy");
   begin
      Check_Return_Values (XmClipboardBeginCopy (Display,
                                                 Window,
                                                 Label,
                                                 W,
                                                 Callback,
                                                 Itemid'Address));
   end Xm_Clipboard_Begin_Copy;


   --
   -- Clipboard Cancel Copy
   --
   procedure Xm_Clipboard_Cancel_Copy
     (Display     : in X_Lib.Display_Pointer;
      Window	  : in X_Lib.Window_ID;
      Itemid	  : in ID_Type) is
      function XmClipboardCancelCopy
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Itemid      : in ID_Type)
         return Integer;
      pragma Import (C, XmClipboardCancelCopy, "XmClipboardCancelCopy");
   begin
      Check_Return_Values (XmClipboardCancelCopy (Display, Window, Itemid));
   end Xm_Clipboard_Cancel_Copy;


   --
   -- Clipboard Start Copy
   --
   procedure Xm_Clipboard_Start_Copy
     (Display     : in X_Lib.Display_Pointer;
      Window      : in X_Lib.Window_ID;
      Label       : in Xm_String;
      Timestamp   : in X_Lib.Server_Time;
      W           : in Widget;
      Callback    : in Xm_Cut_Paste_Proc;
      Itemid      : out ID_Type) is
      function XmClipboardStartCopy
        (Display    : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Label       : in Xm_String;
         Timestamp   : in X_Lib.Server_Time;
         W	     : in Widget;
         Callback    : in Xm_Cut_Paste_Proc;
         Itemid      : in System.Address)
         return Integer; 

      pragma Import (C, XmClipboardStartCopy, "XmClipboardStartCopy");
   begin
      Check_Return_Values (XmClipboardStartCopy (Display,
                                                 Window,
                                                 Label,
                                                 Timestamp,
                                                 W,
                                                 Callback,
                                                 Itemid'Address));
   end Xm_Clipboard_Start_Copy;


   --
   -- Clipboard Copy
   --
   procedure Xm_Clipboard_Copy
     (Display     : in  X_Lib.Display_Pointer;
      Window	  : in  X_Lib.Window_ID;
      Itemid	  : in  ID_Type;
      Format_Name : in  String;
      Buffer	  : in  System.Address;
      Length	  : in  Interfaces.C.unsigned_long;
      Privateid   : in  ID_Type;
      Dataid	  : out ID_Type) is
      function XmClipboardCopy
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Itemid      : in ID_Type;
         Format_Name : in System.Address;
         Buffer      : in System.Address;
         Length      : in Interfaces.C.unsigned_long;
         Privateid   : in ID_Type;
         Dataid      : in System.Address)
         return Integer;
      pragma Import (C, XmClipboardCopy, "XmClipboardCopy");

      Format_Name_String : constant Interfaces.C.Char_Array
                         := Interfaces.C.To_C (Format_Name, Append_Nul => True);
   begin
      Check_Return_Values (XmClipboardCopy (Display,
                                            Window,
                                            Itemid,
                                            Format_Name_String'Address,
                                            Buffer, Length,
                                            Privateid,
                                            Dataid'Address));
   end Xm_Clipboard_Copy;

 
   --
   -- Clipboard End Copy
   --
   procedure Xm_Clipboard_End_Copy
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID;
      Itemid  : in ID_Type) is
      function XmClipboardEndCopy
        (Display : in X_Lib.Display_Pointer;
         Window  : in X_Lib.Window_ID;
         Itemid  : in ID_Type) return Integer;
      pragma Import (C, XmClipboardEndCopy, "XmClipboardEndCopy");
   begin
      Check_Return_Values (XmClipboardEndCopy (Display,
                                               Window,
                                               Itemid));
   end Xm_Clipboard_End_Copy;


   --
   -- Clipboard Withdraw Format
   --
   procedure Xm_Clipboard_Withdraw_Format
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID;
      Dataid  : in ID_Type) is
      function XmClipboardWithdrawFormat
        (Display : in X_Lib.Display_Pointer;
         Window  : in X_Lib.Window_ID;
         Dataid  : in ID_Type)
         return Integer;
      pragma Import (C, XmClipboardWithdrawFormat, "XmClipboardWithdrawFormat");
   begin
      Check_Return_Values (XmClipboardWithdrawFormat (Display, Window, Dataid));
   end Xm_Clipboard_Withdraw_Format;



   --
   -- Clipboard Copy By Name
   --
   procedure Xm_Clipboard_Copy_By_Name
     (Display    : in X_Lib.Display_Pointer;
      Window	 : in X_Lib.Window_ID;
      Dataid	 : in ID_Type;
      Buffer	 : in System.Address;
      Length	 : in Interfaces.C.unsigned_long;
      Privateid  : in ID_Type) is
      function XmClipboardCopyByName
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Dataid      : in ID_Type;
         Buffer      : in System.Address;
         Length      : in Interfaces.C.unsigned_long;
         Privateid   : in ID_Type)
         return Integer; 
      pragma Import (C, XmClipboardCopyByName, "XmClipboardCopyByName");
   begin
      Check_Return_Values (XmClipboardCopyByName (Display,
                                                  Window,
                                                  Dataid,
                                                  Buffer,
                                                  Length,
                                                  Privateid));
   end Xm_Clipboard_Copy_By_Name;


   --
   -- Clipboard Start Retrieve
   --
   procedure Xm_Clipboard_Start_Retrieve
     (Display     : in X_Lib.Display_Pointer;
      Window	  : in X_Lib.Window_ID;
      Timestamp   : in X_Lib.Server_Time) is
      function XmClipboardStartRetrieve
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Timestamp   : in X_Lib.Server_Time)
         return Integer;
      pragma Import (C, XmClipboardStartRetrieve, "XmClipboardStartRetrieve");
   begin
      Check_Return_Values (XmClipboardStartRetrieve (Display, Window, Timestamp));
   end Xm_Clipboard_Start_Retrieve;


   --
   -- Clipboard End Retrieve
   --
   procedure Xm_Clipboard_End_Retrieve
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID) is
      function XmClipboardEndRetrieve
        (Display : in X_Lib.Display_Pointer;
         Window  : in X_Lib.Window_ID)
         return Integer;
      pragma Import (C, XmClipboardEndRetrieve, "XmClipboardEndRetrieve");
   begin
      Check_Return_Values (XmClipboardEndRetrieve (Display, Window));
   end Xm_Clipboard_End_Retrieve;


   --
   -- Clipboard Retrieve
   --
   procedure Xm_Clipboard_Retrieve
     (Display     : in     X_Lib.Display_Pointer;
      Window	  : in     X_Lib.Window_ID;
      Format	  : in     String;
      Buffer	  : in     System.Address;
      Length	  : in     Interfaces.C.unsigned_long;
      Out_Length  :    out Interfaces.C.unsigned_long;
      Privateid   :    out ID_Type) is
      function XmClipboardRetrieve
        (Display     : in  X_Lib.Display_Pointer;
         Window      : in  X_Lib.Window_ID;
         Format      : in  System.Address;
         Buffer      : in  System.Address;
         Length      : in  Interfaces.C.unsigned_long;
         Out_Length  : in  System.Address;
         Privateid   : in  System.Address)
         return Integer;
      pragma Import (C, XmClipboardRetrieve, "XmClipboardRetrieve");

      Format_String : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Format, Append_Nul => True);
   begin
      Check_Return_Values (XmClipboardRetrieve (Display,
                                                Window,
                                                Format_String'Address,
                                                Buffer,
                                                Length,
                                                Out_Length'Address,
                                                Privateid'Address));
   end Xm_Clipboard_Retrieve;



   --
   -- Clipboard Undo Copy
   --
   procedure Xm_Clipboard_Undo_Copy
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID) is
      function XmClipboardUndoCopy
        (Display : in X_Lib.Display_Pointer;
         Window  : in X_Lib.Window_ID)
         return Integer;
      pragma Import (C, XmClipboardUndoCopy, "XmClipboardUndoCopy");
   begin
      Check_Return_Values (XmClipboardUndoCopy (Display, Window));
   end Xm_Clipboard_Undo_Copy;


   --
   -- Clipboard Lock
   --
   procedure Xm_Clipboard_Lock
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID) is
      function XmClipboardLock
        (Display : in X_Lib.Display_Pointer;
         Window  : in X_Lib.Window_ID)
         return Integer;
      pragma Import (C, XmClipboardLock, "XmClipboardLock");
   begin
      Check_Return_Values (XmClipboardLock (Display, Window));
   end Xm_Clipboard_Lock;


   --
   -- Clipboard Unlock
   --
   procedure Xm_Clipboard_Unlock
     (Display    : in X_Lib.Display_Pointer;
      Window	 : in X_Lib.Window_ID;
      All_Levels : in Boolean) is
      function XmClipboardUnlock
        (Display    : in X_Lib.Display_Pointer;
         Window     : in X_Lib.Window_ID;
         All_Levels : in Xt_Boolean)
         return Integer;
      pragma Import (C, XmClipboardUnlock, "XmClipboardUnlock");
   begin
      Check_Return_Values (XmClipboardUnlock (Display, Window, To_Xt_Boolean (All_Levels)));
   end Xm_Clipboard_Unlock;


   --
   -- Clipboard Inquire Count
   --
   procedure Xm_Clipboard_Inquire_Count
     (Display   : in  X_Lib.Display_Pointer;
      Window	: in  X_Lib.Window_ID;
      Count	: out Integer;
      Maxlength : out Interfaces.C.unsigned_long) is
      function XmClipboardInquireCount
        (Display   : in  X_Lib.Display_Pointer;
         Window    : in  X_Lib.Window_ID;
         Count     : in  System.Address;
         Maxlength : in  System.Address)
         return Integer;
      pragma Import (C, XmClipboardInquireCount, "XmClipboardInquireCount");
   begin
      Check_Return_Values (XmClipboardInquireCount (Display, Window,
                                                    Count'Address,
                                                    Maxlength'Address));
   end Xm_Clipboard_Inquire_Count;


   --
   -- Clipboard Inquire Format
   --
   procedure Xm_Clipboard_Inquire_Format
     (Display     : in  X_Lib.Display_Pointer;
      Window	  : in  X_Lib.Window_ID;
      N 	  : in  Integer;
      Buffer	  : in  System.Address;
      Length	  : in  Interfaces.C.unsigned_long;
      Out_Length  : out Interfaces.C.unsigned_long) is
      function XmClipboardInquireFormat
        (Display     : in  X_Lib.Display_Pointer;
         Window      : in  X_Lib.Window_ID;
         N           : in  Integer;
         Buffer      : in  System.Address;
         Length      : in  Interfaces.C.unsigned_long;
         Out_Length  : in  System.Address)
         return Integer;
      pragma Import (C, XmClipboardInquireFormat, "XmClipboardInquireFormat");
   begin
      Check_Return_Values (XmClipboardInquireFormat (Display, Window, N,
                                                     Buffer, Length,
                                                     Out_Length'Address));
   end Xm_Clipboard_Inquire_Format;



   --
   -- Clipboard Inquire Length
   --
   procedure Xm_Clipboard_Inquire_Length
     (Display     : in  X_Lib.Display_Pointer;
      Window	  : in  X_Lib.Window_ID;
      Format	  : in  String;
      Length	  : out Interfaces.C.unsigned_long) is
      function XmClipboardInquireLength
        (Display     : in  X_Lib.Display_Pointer;
         Window      : in  X_Lib.Window_ID;
         Format      : in  System.Address;
         Length      : in  System.Address)
         return Integer;
      pragma Import (C, XmClipboardInquireLength, "XmClipboardInquireLength");

      Format_String : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Format, Append_Nul => True);
   begin
      Check_Return_Values (XmClipboardInquireLength (Display,
                                                     Window,
                                                     Format_String'Address,
                                                     Length'Address));
   end Xm_Clipboard_Inquire_Length;


   --
   -- Clipboard Inquire Pending Items
   --
   function Xm_Clipboard_Inquire_Pending_Items
     (Display     : in  X_Lib.Display_Pointer;
      Window	  : in  X_Lib.Window_ID;
      Format	  : in  String)
      return Xm_Clipboard_Pending_Array is

      package Clipboard_Pending_Interface is new
         Interfaces.C.Pointers (Positive,
                                Xm_Clipboard_Pending_Rec,
                                Xm_Clipboard_Pending_Array,
                                Xm_Clipboard_Pending_Rec'(Data_ID => 0,
                                                          Private_ID => 0));
      function XmClipboardInquirePendingItems
        (Display     : in X_Lib.Display_Pointer;
         Window      : in X_Lib.Window_ID;
         Format      : in System.Address;
         List	     : in System.Address;
         Nitems      : in System.Address)
         return Integer;
      pragma Import (C, XmClipboardInquirePendingItems, "XmClipboardInquirePendingItems");

      Hook_To_Array   : Clipboard_Pending_Interface.Pointer;
      Number          : Integer;

      Format_String : constant Interfaces.C.Char_Array
                    := Interfaces.C.To_C (Format, Append_Nul => True);
   begin
      Check_Return_Values (XmClipboardInquirePendingItems (Display,
                                                           Window,
                                                           Format_String'Address,
                                                           Hook_To_Array'Address,
                                                           Number'Address));
      return Clipboard_Pending_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end Xm_Clipboard_Inquire_Pending_Items;


   --
   -- Clipboard Register Format
   --
   procedure Xm_Clipboard_Register_Format
     (Display       : in X_Lib.Display_Pointer;
      Format_Name   : in String;
      Format_Length : in Integer) is
      function XmClipboardRegisterFormat
        (Display       : in X_Lib.Display_Pointer;
         Format_Name   : in System.Address;
         Format_Length : in Integer)
         return Integer;
      pragma Import (C, XmClipboardRegisterFormat, "XmClipboardRegisterFormat");

      Format_Name_String : constant Interfaces.C.Char_Array
                         := Interfaces.C.To_C (Format_Name, Append_Nul => True);
   begin
      Check_Return_Values (XmClipboardRegisterFormat (Display,
                                                      Format_Name_String'Address,
                                                      Format_Length));
   end Xm_Clipboard_Register_Format;


end Xm_Widgets.Clipboard;
