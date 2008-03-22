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

package Xm_Widgets.Clipboard is


   -- we don't use special return values
   -- but exceptions in case of errors
   Fail_Error       : exception;
   Truncate_Error   : exception;
   Locked_Error     : exception;
   Bad_Format_Error : exception;
   No_Data_Error    : exception;


   type ID_Type is new Interfaces.C.long;


   type Xm_Cut_Paste_Proc is
      access procedure
               (W           : in     Widget;
                Data_ID     : in out ID_Type;
                Private_ID  : in out ID_Type;
                Reason      : in out Integer);
   pragma Convention (C, Xm_Cut_Paste_Proc);
                                       

   type Void_Proc is
      access procedure
               (W           : in     Widget;
                Data_ID     : in out Integer;
                Private_ID  : in out Integer;
                Reason      : in out Integer);
   pragma Convention (C, Void_Proc);
                                       

   type Xm_Clipboard_Pending_Rec is record
      Data_ID     : ID_Type;
      Private_ID  : ID_Type;
   end record;

   type Xm_Clipboard_Pending_Array is
      array (Positive range <>) of aliased Xm_Clipboard_Pending_Rec;


   -- -------------------------------------------------------------------------
   --
   --  Clipboard routines
   --
   -- -------------------------------------------------------------------------


   --
   -- Clipboard Begin Copy
   --
   procedure Xm_Clipboard_Begin_Copy
     (Display     : in     X_Lib.Display_Pointer;
      Window      : in     X_Lib.Window_ID;
      Label       : in     Xm_String;
      W           : in     Widget;
      Callback    : in     Void_Proc;
      Itemid      :    out ID_Type);

   --
   -- Clipboard Start Copy
   --
   procedure Xm_Clipboard_Start_Copy
     (Display     : in     X_Lib.Display_Pointer;
      Window      : in     X_Lib.Window_ID;
      Label       : in     Xm_String;
      Timestamp   : in     X_Lib.Server_Time;
      W           : in     Widget;
      Callback    : in     Xm_Cut_Paste_Proc;
      Itemid      :    out ID_Type);

   --
   -- Clipboard Copy
   --
   procedure Xm_Clipboard_Copy
     (Display     : in     X_Lib.Display_Pointer;
      Window	  : in     X_Lib.Window_ID;
      Itemid	  : in     ID_Type;
      Format_Name : in     String;
      Buffer	  : in     System.Address;
      Length	  : in     Interfaces.C.unsigned_long;
      Privateid   : in     ID_Type;
      Dataid	  :    out ID_Type);

   --
   -- Clipboard Cancel Copy
   --
   procedure Xm_Clipboard_Cancel_Copy
     (Display     : in X_Lib.Display_Pointer;
      Window	  : in X_Lib.Window_ID;
      Itemid	  : in ID_Type);


   --
   -- Clipboard End Copy
   --
   procedure Xm_Clipboard_End_Copy
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID;
      Itemid  : in ID_Type);


   --
   -- Clipboard Withdraw Format
   --
   procedure Xm_Clipboard_Withdraw_Format
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID;
      Dataid  : in ID_Type);


   --
   -- Clipboard Copy By Name
   --
   procedure Xm_Clipboard_Copy_By_Name
     (Display    : in X_Lib.Display_Pointer;
      Window	 : in X_Lib.Window_ID;
      Dataid	 : in ID_Type;
      Buffer	 : in System.Address;
      Length	 : in Interfaces.C.unsigned_long;
      Privateid  : in ID_Type);


   --
   -- Clipboard Undo Copy
   --
   procedure Xm_Clipboard_Undo_Copy
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID);


   --
   -- Clipboard Lock
   --
   procedure Xm_Clipboard_Lock
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID);


   --
   -- Clipboard Unlock
   --
   procedure Xm_Clipboard_Unlock
     (Display    : in X_Lib.Display_Pointer;
      Window	 : in X_Lib.Window_ID;
      All_Levels : in Boolean);


   --
   -- Clipboard Start Retrieve
   --
   procedure Xm_Clipboard_Start_Retrieve
     (Display     : in X_Lib.Display_Pointer;
      Window	  : in X_Lib.Window_ID;
      Timestamp   : in X_Lib.Server_Time);

   --
   -- Clipboard End Retrieve
   --
   procedure Xm_Clipboard_End_Retrieve
     (Display : in X_Lib.Display_Pointer;
      Window  : in X_Lib.Window_ID);



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
      Privateid   :    out ID_Type);


   --
   -- Clipboard Inquire Count
   --
   procedure Xm_Clipboard_Inquire_Count
     (Display   : in     X_Lib.Display_Pointer;
      Window	: in     X_Lib.Window_ID;
      Count	:    out Integer;
      Maxlength :    out Interfaces.C.unsigned_long);

   --
   -- Clipboard Inquire Format
   --
   procedure Xm_Clipboard_Inquire_Format
     (Display     : in     X_Lib.Display_Pointer;
      Window	  : in     X_Lib.Window_ID;
      N 	  : in     Integer;
      Buffer	  : in     System.Address;
      Length	  : in     Interfaces.C.unsigned_long;
      Out_Length  :    out Interfaces.C.unsigned_long);


   --
   -- Clipboard Inquire Length
   --
   procedure Xm_Clipboard_Inquire_Length
     (Display     : in     X_Lib.Display_Pointer;
      Window	  : in     X_Lib.Window_ID;
      Format	  : in     String;
      Length	  :    out Interfaces.C.unsigned_long);

   --
   -- Clipboard Inquire Pending Items
   --
   function Xm_Clipboard_Inquire_Pending_Items
     (Display     : in  X_Lib.Display_Pointer;
      Window	  : in  X_Lib.Window_ID;
      Format	  : in  String)
      return Xm_Clipboard_Pending_Array;


   --
   -- Clipboard Register Format
   --
   procedure Xm_Clipboard_Register_Format
     (Display       : in X_Lib.Display_Pointer;
      Format_Name   : in String;
      Format_Length : in Integer);

end Xm_Widgets.Clipboard;
