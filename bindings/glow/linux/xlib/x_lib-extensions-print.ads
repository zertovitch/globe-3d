-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2002 Hans-Frieder Vogt                                --
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
--          September 5, 1998 first version of package
--          9 Feb 2002 H.-F. Vogt: add procedures Xp_Get_Locale_Net_String and
--                                 Xp_Notify_Pdm
--
-------------------------------------------------------------------------------

package X_Lib.Extensions.Print is

-- UseX11R6.3
   --  string that identifies this extension
   --
   Extension_String : constant String := "XpExtension";


   type Xp_Context_ID is new XID;
   Null_Xp_Context_ID : constant Xp_Context_ID;

   type Xp_Attr_Replacement is (Replace, Merge);
   for Xp_Attr_Replacement use (Replace => 1, Merge => 2);
   for Xp_Attr_Replacement'Size use 8;

   type Xp_Get_Status is (Finished, Second_Consumer);
   for Xp_Get_Status use (Finished => 0, Second_Consumer => 1);
   for Xp_Get_Status'Size use 8;

   type Xp_Save_Data is (Spool, Get_Data);
   for Xp_Save_Data use (Spool => 1, Get_Data => 2);
   for Xp_Save_Data'Size use 8;

   type Xp_Document_Type is (Normal, Raw);
   for Xp_Document_Type use (Normal => 1, Raw => 2);
   for Xp_Document_Type'Size use 8;


   type Resolution is new Integer;


   type Xp_Save_Proc is access procedure
     (Display     : in Display_Pointer;
      Context     : in Xp_Context_ID;
      Data        : in System.Address;
      Data_Len    : in Interfaces.C.unsigned;
      Client_Data : in X_Pointer);
   pragma Convention (C, Xp_Save_Proc);


   type Xp_Finish_Proc is access procedure
     (Display     : in Display_Pointer;
      Context     : in Xp_Context_ID;
      Status      : in Xp_Get_Status;
      Client_Data : in X_Pointer);
   pragma Convention (C, Xp_Finish_Proc);


   type Event_Mask is new Interfaces.C.unsigned_long;
   Print_Mask       : constant Event_Mask := Event_Mask (1);
   Attribute_Mask   : constant Event_Mask := Event_Mask (2);
   Data_Ready_Mask  : constant Event_Mask := Event_Mask (4); -- not clear

   No_Events        : constant Event_Mask := Event_Mask (0);
   All_Events       : constant Event_Mask := Event_Mask (7);



   -- new events of the print extension (First_Event has to be added to them!)
   --
   Print_Notify       : constant Event_Type := 0;
   Attribute_Notify   : constant Event_Type := 1;
   Data_Ready_Notify  : constant Event_Type := 2;


   type Notify_Detail is (Start_Job_Notify, End_Job_Notify,
                          Start_Doc_Notify, End_Doc_Notify,
			  Start_Page_Notify, End_Page_Notify);
   for Notify_Detail  use (Start_Job_Notify  => 0, End_Job_Notify  => 1,
                           Start_Doc_Notify  => 2, End_Doc_Notify  => 3,
			   Start_Page_Notify => 4, End_Page_Notify => 5);
   for Notify_Detail'Size use Interfaces.C.int'Size;


   type Xp_Attributes is new Interfaces.C.signed_char;
   Job_Attr          : constant Xp_Attributes := 1;
   Doc_Attr          : constant Xp_Attributes := 2;
   Page_Attr         : constant Xp_Attributes := 3;
   Printer_Attr      : constant Xp_Attributes := 4;
   Server_Attr       : constant Xp_Attributes := 5;

   Medium_Attr       : constant Xp_Attributes := 6;
   Font_Attr         : constant Xp_Attributes := 7;
   Res_Attr          : constant Xp_Attributes := 8;
   Trans_Attr        : constant Xp_Attributes := 9;
   Del_Attr          : constant Xp_Attributes := 10;
   Aux_Sheet_Package : constant Xp_Attributes := 11;
   Aux_Sheet         : constant Xp_Attributes := 12;
   Finish_Attr       : constant Xp_Attributes := 13;
   Output_Attr       : constant Xp_Attributes := 14;
   Imp_Attr          : constant Xp_Attributes := 15;
   Sched_Attr        : constant Xp_Attributes := 16;
   Int_Job_Attr      : constant Xp_Attributes := 17;
   Int_Doc_Attr      : constant Xp_Attributes := 18;
   Res_Con_Attr      : constant Xp_Attributes := 19;



   -- event of type Print_Notify
   --
   type Xp_Print_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Context        : Xp_Context_ID;
      Cancel         : Boolean;
      Detail         : Notify_Detail;
   end record;
   for Xp_Print_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Context        at 12 range 0 .. 31;
      Cancel         at 16 range 0 .. 31;
      Detail         at 20 range 0 .. 31;
   end record;

   type Xp_Print_Event_Access is access all Xp_Print_Event;

   -- event of type Attribute_Notify
   --
   type Xp_Attribute_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Context        : Xp_Context_ID;
      Detail         : Xp_Attributes;
   end record;
   for Xp_Attribute_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Context        at 12 range 0 .. 31;
      Detail         at 20 range 0 .. 31;
   end record;

   type Xp_Attribute_Event_Access is access all Xp_Attribute_Event;

   -- event of type Data_Ready_Notify
   --
   type Xp_Data_Ready_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Context        : Xp_Context_ID;
      Available      : Interfaces.C.unsigned_long;
   end record;
   for Xp_Data_Ready_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Context        at 12 range 0 .. 31;
      Available      at 20 range 0 .. 31;
   end record;

   type Xp_Data_Ready_Event_Access is access all Xp_Data_Ready_Event;

   -- try to convert an X_Event record to one of the print events
   -- if the event type is wrong, raise Invalid_Conversion_Error
   --
   function To_Print_Event
     (Ev          : in X_Event;
      First_Event : in Event_Type)
      return Xp_Print_Event;
   function To_Attribute_Event
     (Ev          : in X_Event;
      First_Event : in Event_Type)
      return Xp_Attribute_Event;
   function To_Data_Ready_Event
     (Ev          : in X_Event;
      First_Event : in Event_Type)
      return Xp_Data_Ready_Event;

   Invalid_Conversion_Error : exception;


   -- error codes of the print extension (First_Error has to be added to them!)
   --
   Bad_Context      : constant Error_Code_Type := 0;
   Bad_Sequence     : constant Error_Code_Type := 1;
   Bad_Resource_ID  : constant Error_Code_Type := 2;


   -- general Exception
   --
   Xp_Error : exception;


   -- Query existence of printer extension
   --
   procedure Xp_Query_Extension
     (Display      : in     Display_Pointer;
      First_Event  :	out Event_Type;
      First_Error  :	out Error_Code_Type);


   procedure Xp_Query_Version
     (Display       : in     Display_Pointer;
      Major_Version :    out Interfaces.C.short;
      Minor_Version :    out Interfaces.C.short);

   type Screen_Pointer_List is
      array (Natural range <>) of Screen_Pointer;

   function Xp_Query_Screens
     (Display : in Display_Pointer)
      return Screen_Pointer_List;


   -- procedures for contexts
   --

   -- create a context for printing on printer Printer_Name
   -- through print server on display Display
   -- possible names for Printer_Name can be asked for with
   -- Xp_Get_Printer_List
   --
   function Xp_Create_Context
     (Display      : in Display_Pointer;
      Printer_Name : in String)
      return Xp_Context_ID;


   -- set Print_Context as the current context for printing
   -- through print server on display Display
   --
   procedure Xp_Set_Context
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID);


   -- get the current context for printing
   -- through print server on display Display
   --
   function Xp_Get_Context
     (Display       : in Display_Pointer)
      return Xp_Context_ID;


   -- destroy the current context for printing
   -- through print server on display Display
   --
   procedure Xp_Destroy_Context
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID);


   -- information routines
   --

   -- get the screen pointer Screen_Pointer for printing with Print_Context
   -- through print server on display Display
   --
   function Xp_Get_Screen_Of_Context
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID)
      return Screen_Pointer;


   -- get dimension of a page for printing with Print_Context
   -- through print server on display Display
   --
   procedure Xp_Get_Page_Dimensions
     (Display           : in     Display_Pointer;
      Print_Context     : in     Xp_Context_ID;
      Width,
      Height            :    out Dimension;
      Reproducible_Area :    out Rectangle);


   -- control of print jobs
   --
   procedure Xp_Start_Job
     (Display       : in Display_Pointer;
      Save_Data     : in Xp_Save_Data);

   procedure Xp_End_Job
     (Display       : in Display_Pointer);

   procedure Xp_Cancel_Job
     (Display       : in Display_Pointer;
      Discard       : in Boolean);


   procedure Xp_Start_Doc
     (Display       : in Display_Pointer;
      Doc_Type      : in Xp_Document_Type);

   procedure Xp_End_Doc
     (Display       : in Display_Pointer);

   procedure Xp_Cancel_Doc
     (Display       : in Display_Pointer;
      Discard       : in Boolean);


   procedure Xp_Put_Document_Data
     (Display       : in Display_Pointer;
      Drawable      : in Drawable_ID;
      Data          : in System.Address;
      Data_Len      : in Interfaces.C.unsigned;
      Doc_Fmt       : in String;
      Options       : in String);

   procedure Xp_Get_Document_Data
     (Display       : in Display_Pointer;
      Context       : in Xp_Context_ID;
      Save_Proc     : in Xp_Save_Proc;
      Finish_Proc   : in Xp_Finish_Proc;
      Client_Data   : in X_Pointer);


   procedure Xp_Start_Page
     (Display       : in Display_Pointer;
      Window        : in Window_ID);

   procedure Xp_End_Page
     (Display       : in Display_Pointer);

   procedure Xp_Cancel_Page
     (Display       : in Display_Pointer;
      Discard       : in Boolean);


   -- select, which print events should be processed
   --
   procedure Xp_Select_Input
     (Display           : in Display_Pointer;
      Print_Context     : in Xp_Context_ID;
      Ev_Mask           : in Event_Mask);


   -- preliminary, might be changed
   --
   procedure Xp_Input_Selected
     (Display           : in     Display_Pointer;
      Print_Context     : in     Xp_Context_ID;
      Selected_Mask     :    out Event_Mask;
      All_Ev_Mask       :    out Event_Mask);


   procedure Xp_Set_Image_Resolution
     (Display       : in     Display_Pointer;
      Print_Context : in     Xp_Context_ID;
      Image_Res     : in     Resolution;
      Prev_Res      :    out Resolution);

   function Xp_Get_Image_Resolution
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID)
      return Resolution;


   function Xp_Get_Attributes
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID;
      Attr_Type     : in Xp_Attributes)
      return String;

   procedure Xp_Set_Attributes
     (Display           : in Display_Pointer;
      Print_Context     : in Xp_Context_ID;
      Attr_Type         : in Xp_Attributes;
      Pool              : in String;
      Replacement_Rule  : in Xp_Attr_Replacement);

   function Xp_Get_One_Attribute
     (Display         : in Display_Pointer;
      Print_Context   : in Xp_Context_ID;
      Attr_Type       : in Xp_Attributes;
      Attribute_Name  : in String)
      return String;


   type String_Access is access all String;

   type Printer_Rec is record
      Name : String_Access;
      Desc : String_Access;
   end record;
   type Printer_List is
      array (Natural range <>) of aliased Printer_Rec;


   --  get a list of printers with the name Printer_Name
   --
   function Xp_Get_Printer_List
     (Display       : in Display_Pointer;
      Printer_Name  : in String)
      return Printer_List;

   --  get a list of all printers available
   --
   function Xp_Get_Printer_List
     (Display       : in Display_Pointer)
      return Printer_List;

   procedure Xp_Rehash_Printer_List (Display : in Display_Pointer);


   --  raises Xp_Error on error
   --
   procedure Xp_Get_Pdm_Start_Params
     (Print_Display     : in     Display_Pointer;
      Print_Window      : in     Window_ID;
      Print_Context     : in     Xp_Context_ID;
      Video_Display     : in     Display_Pointer;  -- can be different
                                                   -- from Print_Display!
      Video_Window      : in     Window_ID;
      Selection_Display :    out Display_Pointer;
      Selection         :    out Atom;
      Selection_Type    :    out Atom;
      Format            :    out Data_Format_Type;
      Data              :    out System.Address;   -- WILL BE CHANGED!
      Num_Elements      :    out Integer);         -- WILL BE CHANGED!


   --  raises Xp_Error on error
   --
   procedure Xp_Get_Auth_Params
     (Print_Display     : in     Display_Pointer;
      Video_Display     : in     Display_Pointer;  -- can be different
                                                   -- from Print_Display!
      Selection_Display :    out Display_Pointer;
      Selection         :    out Atom;
      Target            :    out Atom);


   --  raises Xp_Error on error
   --
   procedure Xp_Send_Auth
     (Display       : in Display_Pointer;
      Window        : in Window_ID);


--  not yet implemented
--   procedure Xp_Send_One_Ticket
--     (Display       : in Display_Pointer;
--      Window        : in Window_ID;
--      Ticket        : in XAuth_Access;
--      More          : in Boolean);


   -- I have absolutely no idea what the following defines and functions are
   -- meant to do
   --
   type Xp_Hinter_Proc is access function return System.Address;
   pragma Convention (C, Xp_Hinter_Proc);


   procedure Xp_Set_Locale_Hinter
     (Hinter_Proc : in Xp_Hinter_Proc;
      Hinter_Desc : in String);

   function Xp_Get_Locale_Hinter
     (Hinter_Proc : in Xp_Hinter_Proc)
      return String;


   --  return the String for the current locale
   --
   function Xp_Get_Locale_Net_String return String;


   --  inform the print display manager (about what???)
   --
   function Xp_Notify_Pdm
     (Display       : in X_Lib.Display_Pointer;
      Window        : in X_Lib.Window_ID;
      Context       : in Xp_Context_ID;
      Video_Display : in X_Lib.Display_Pointer;
      Video_Window  : in X_Lib.Window_ID;
      Auth_Flag     : in Boolean)
      return String;

private

   Null_Xp_Context_ID : constant Xp_Context_ID := Xp_Context_ID (Null_XID);

   pragma Import (C, Xp_Set_Context, "XpSetContext");
   pragma Import (C, Xp_Get_Context, "XpGetContext");
   pragma Import (C, Xp_Destroy_Context, "XpDestroyContext");
   pragma Import (C, Xp_Get_Screen_Of_Context, "XpGetScreenOfContext");
   pragma Import (C, Xp_Start_Job, "XpStartJob");
   pragma Import (C, Xp_End_Job, "XpEndJob");
   pragma Import (C, Xp_Start_Doc, "XpStartDoc");
   pragma Import (C, Xp_End_Doc, "XpEndDoc");
   pragma Import (C, Xp_Get_Document_Data, "XpGetDocumentData");
   pragma Import (C, Xp_Start_Page, "XpStartPage");
   pragma Import (C, Xp_End_Page, "XpEndPage");
   pragma Import (C, Xp_Select_Input, "XpSelectInput");
   pragma Import (C, Xp_Get_Image_Resolution, "XpGetImageResolution");
   pragma Import (C, Xp_Rehash_Printer_List, "XpRehashPrinterList");

-- EndX11R6.3

end X_Lib.Extensions.Print;
