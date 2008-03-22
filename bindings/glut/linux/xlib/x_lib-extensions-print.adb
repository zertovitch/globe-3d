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

with Ada.Unchecked_Conversion,
     Interfaces.C.Strings;
package body X_Lib.Extensions.Print is

-- UseX11R6.3

   function To_Print_Event
     (Ev : in X_Event;
      First_Event : in Event_Type)
      return Xp_Print_Event is
      function To_Print_Event is
         new Ada.Unchecked_Conversion (System.Address, Xp_Print_Event_Access);
   begin
      if Ev.Ev_Type /= First_Event + Print_Notify then
         raise Invalid_Conversion_Error;
      end if;
      return To_Print_Event (Ev.Pad'Address).all;
   end To_Print_Event;


   function To_Attribute_Event
     (Ev : in X_Event;
      First_Event : in Event_Type)
      return Xp_Attribute_Event is
      function To_Attribute_Event is
         new Ada.Unchecked_Conversion (System.Address, Xp_Attribute_Event_Access);
   begin
      if Ev.Ev_Type /= First_Event + Attribute_Notify then
         raise Invalid_Conversion_Error;
      end if;
      return To_Attribute_Event (Ev.Pad'Address).all;
   end To_Attribute_Event;


   function To_Data_Ready_Event
     (Ev : in X_Event;
      First_Event : in Event_Type)
      return Xp_Data_Ready_Event is
      function To_Data_Ready_Event is
         new Ada.Unchecked_Conversion (System.Address, Xp_Data_Ready_Event_Access);
   begin
      if Ev.Ev_Type /= First_Event + Data_Ready_Notify then
         raise Invalid_Conversion_Error;
      end if;
      return To_Data_Ready_Event (Ev.Pad'Address).all;
   end To_Data_Ready_Event;


   -- Query existence of printer
   --
   procedure Xp_Query_Extension
     (Display      : in     Display_Pointer;
      First_Event  :	out Event_Type;
      First_Error  :	out Error_Code_Type) is

      function XpQueryExtension
        (Display      : in X_Lib.Display_Pointer;
         First_Event  : in System.Address;
         First_Error  : in System.Address)
	 return X_Boolean;
      pragma Import (C, XpQueryExtension, "XpQueryExtension");

      First_Error_Int : Integer;
   begin
      if XpQueryExtension (Display,
                           First_Event'Address,
                           First_Error_Int'Address) = X_Boolean'(False) then
         raise X_Lib.Extensions.Extension_Not_Existent_Error;
      end if;
      First_Error := Error_Code_Type (First_Error_Int);
   end Xp_Query_Extension;


   procedure Xp_Query_Version
     (Display       : in     Display_Pointer;
      Major_Version :    out Interfaces.C.short;
      Minor_Version :    out Interfaces.C.short) is
      function XpQueryVersion
        (Display       : in Display_Pointer;
         Major_Version : in System.Address;
         Minor_Version : in System.Address)
	 return Status_Type;
      pragma Import (C, XpQueryVersion, "XpQueryVersion");
   begin
      if XpQueryVersion (Display, Major_Version'Address, Minor_Version'Address)
         = Error_Status then
         raise X_Lib.Extensions.Extension_Not_Existent_Error; -- correct ???
      end if;
   end Xp_Query_Version;



   function Xp_Query_Screens
     (Display : in Display_Pointer)
      return Screen_Pointer_List is
      function XpQueryScreens
        (Display    : in Display_Pointer;
	 List_Count : in System.Address)
	 return System.Address;
      pragma Import (C, XpQueryScreens, "XpQueryScreens");
      Hook  : System.Address;
      Count : Integer;
   begin
      Hook := XpQueryScreens (Display, Count'Address);
      if System."=" (Hook, System.Null_Address) then
         return Screen_Pointer_List'(1 .. 0 => X_Lib.Null_Screen_Pointer);
      else
         declare
	    subtype My_Screen_Pointer_List is Screen_Pointer_List (1 .. Count);
	    type My_Screen_Pointer_List_Access is access all My_Screen_Pointer_List;
	    function To_Screen_Pointer_List is
	       new Ada.Unchecked_Conversion (System.Address,
	                                     My_Screen_Pointer_List_Access);
	 begin
            return To_Screen_Pointer_List (Hook).all;
	 end;
      end if;
   end Xp_Query_Screens;


   function Xp_Create_Context
     (Display      : in Display_Pointer;
      Printer_Name : in String)
      return Xp_Context_ID is
      function XpCreateContext
        (Display      : in Display_Pointer;
         Printer_Name : in System.Address)
         return Xp_Context_ID;
      pragma Import (C, XpCreateContext, "XpCreateContext");

      Printer_Name_String     : constant Interfaces.C.Char_Array
                              := Interfaces.C.To_C (Printer_Name, Append_Nul => True);
   begin
      return XpCreateContext (Display, Printer_Name_String'Address);
   end Xp_Create_Context;



   procedure Xp_Get_Page_Dimensions
     (Display           : in     Display_Pointer;
      Print_Context     : in     Xp_Context_ID;
      Width,
      Height            :    out Dimension;
      Reproducible_Area :    out Rectangle) is
      function XpGetPageDimensions
        (Display           : in Display_Pointer;
         Print_Context     : in Xp_Context_ID;
         Width,
         Height            : in System.Address;
         Reproducible_Area : in System.Address)
	 return Status_Type;
      pragma Import (C, XpGetPageDimensions, "XpGetPageDimensions");
   begin
      if XpGetPageDimensions (Display, Print_Context,
                              Width'Address, Height'Address,
			      Reproducible_Area'Address) = Error_Status then
         raise X_Error;
      end if;
   end Xp_Get_Page_Dimensions;


   -- control of print jobs
   --
   procedure Xp_Cancel_Job
     (Display       : in Display_Pointer;
      Discard       : in Boolean) is
      procedure XpCancelJob
        (Display       : in Display_Pointer;
         Discard       : in X_Boolean);
      pragma Import (C, XpCancelJob, "XpCancelJob");
   begin
      XpCancelJob (Display, To_X_Boolean (Discard));
   end Xp_Cancel_Job;


   procedure Xp_Cancel_Doc
     (Display       : in Display_Pointer;
      Discard       : in Boolean) is
      procedure XpCancelDoc
        (Display       : in Display_Pointer;
         Discard       : in X_Boolean);
      pragma Import (C, XpCancelDoc, "XpCancelDoc");
   begin
      XpCancelDoc (Display, To_X_Boolean (Discard));
   end Xp_Cancel_Doc;


   procedure Xp_Put_Document_Data
     (Display       : in Display_Pointer;
      Drawable      : in Drawable_ID;
      Data          : in System.Address;
      Data_Len      : in Interfaces.C.unsigned;
      Doc_Fmt       : in String;
      Options       : in String) is
      procedure XpPutDocumentData
        (Display       : in Display_Pointer;
         Drawable      : in Drawable_ID;
         Data          : in System.Address;
         Data_Len      : in Interfaces.C.unsigned;
         Doc_Fmt       : in System.Address;
         Options       : in System.Address);
      pragma Import (C, XpPutDocumentData, "XpPutDocumentData");
      Fmt_String     : constant Interfaces.C.Char_Array
                     := Interfaces.C.To_C (Doc_Fmt, Append_Nul => True);
      Options_String : constant Interfaces.C.Char_Array
                     := Interfaces.C.To_C (Options, Append_Nul => True);
   begin
      XpPutDocumentData (Display, Drawable, Data, Data_Len,
                         Fmt_String'Address, Options_String'Address);
   end Xp_Put_Document_Data;


   procedure Xp_Cancel_Page
     (Display       : in Display_Pointer;
      Discard       : in Boolean) is
      procedure XpCancelPage
        (Display       : in Display_Pointer;
         Discard       : in X_Boolean);
      pragma Import (C, XpCancelPage, "XpCancelPage");
   begin
      XpCancelPage (Display, To_X_Boolean (Discard));
   end Xp_Cancel_Page;


   procedure Xp_Input_Selected
     (Display           : in     Display_Pointer;
      Print_Context     : in     Xp_Context_ID;
      Selected_Mask     :    out Event_Mask;
      All_Ev_Mask       :    out Event_Mask) is
      function XpInputSelected
        (Display           : in Display_Pointer;
         Print_Context     : in Xp_Context_ID;
         All_Ev_Mask       : in System.Address)
	 return Event_Mask;
      pragma Import (C, XpInputSelected, "XpInputSelected");
   begin
      Selected_Mask := XpInputSelected (Display,
                                        Print_Context,
					All_Ev_Mask'Address);
   end Xp_Input_Selected;



   procedure Xp_Set_Image_Resolution
     (Display       : in     Display_Pointer;
      Print_Context : in     Xp_Context_ID;
      Image_Res     : in     Resolution;
      Prev_Res      :    out Resolution) is
      function XpSetImageResolution
        (Display       : in Display_Pointer;
         Print_Context : in Xp_Context_ID;
         Image_Res     : in Resolution;
         Prev_Res      : in System.Address)
	 return Status_Type;
      pragma Import (C, XpSetImageResolution, "XpSetImageResolution");
   begin
      if XpSetImageResolution (Display, Print_Context,
                               Image_Res, Prev_Res'Address) = Error_Status then
	 raise X_Error;
      end if;
   end Xp_Set_Image_Resolution;


   function Xp_Get_Attributes
     (Display       : in Display_Pointer;
      Print_Context : in Xp_Context_ID;
      Attr_Type     : in Xp_Attributes)
      return String is
      function XpGetAttributes
        (Display       : in Display_Pointer;
         Print_Context : in Xp_Context_ID;
         Attr_Type     : in Xp_Attributes)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XpGetAttributes, "XpGetAttributes");
      use Interfaces.C.Strings;
      Ptr : Chars_Ptr;
   begin
      Ptr := XpGetAttributes (Display, Print_Context, Attr_Type);
      if Ptr = Null_Ptr then
         return "";
      else
         -- the string ptr points to musn't be released! => no "free"
         return Value (Ptr);
      end if;
   end Xp_Get_Attributes;


   procedure Xp_Set_Attributes
     (Display           : in Display_Pointer;
      Print_Context     : in Xp_Context_ID;
      Attr_Type         : in Xp_Attributes;
      Pool              : in String;
      Replacement_Rule  : in Xp_Attr_Replacement) is
      procedure XpSetAttributes
        (Display           : in Display_Pointer;
         Print_Context     : in Xp_Context_ID;
         Attr_Type         : in Xp_Attributes;
         Pool              : in System.Address;
         Replacement_Rule  : in Xp_Attr_Replacement);
      pragma Import (C, XpSetAttributes, "XpSetAttributes");
      Pool_String     : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Pool, Append_Nul => True);
   begin
      XpSetAttributes (Display, Print_Context, Attr_Type,
                       Pool_String'Address, Replacement_Rule);
   end Xp_Set_Attributes;


   function Xp_Get_One_Attribute
     (Display         : in Display_Pointer;
      Print_Context   : in Xp_Context_ID;
      Attr_Type       : in Xp_Attributes;
      Attribute_Name  : in String)
      return String is
      function XpGetOneAttribute
        (Display         : in Display_Pointer;
         Print_Context   : in Xp_Context_ID;
         Attr_Type       : in Xp_Attributes;
         Attribute_Name  : in System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XpGetOneAttribute, "XpGetOneAttribute");
      use Interfaces.C.Strings;
      Ptr                   : Chars_Ptr;
      Attribute_Name_String : constant Interfaces.C.Char_Array
                            := Interfaces.C.To_C (Attribute_Name,
			                          Append_Nul => True);
   begin
      Ptr := XpGetOneAttribute (Display, Print_Context, Attr_Type,
                                Attribute_Name_String'Address);
      if Ptr = Null_Ptr then
         return "";
      else
         -- the string ptr points to musn't be released! => no "free"
         return Value (Ptr);
      end if;
   end Xp_Get_One_Attribute;


   type Internal_Printer_Rec is record
      Name : Interfaces.C.Strings.Chars_Ptr;
      Desc : Interfaces.C.Strings.Chars_Ptr;
   end record;

   function XpGetPrinterList
     (Display	    : in Display_Pointer;
      Printer_Name  : in System.Address;
      List_Count    : in System.Address)
      return System.Address;
   pragma Import (C, XpGetPrinterList, "XpGetPrinterList");

   procedure XpFreePrinterList (List : in System.Address);
   pragma Import (C, XpFreePrinterList, "XpFreePrinterList");

   function Xp_Get_Printer_List
     (Display       : in Display_Pointer;
      Printer_Name  : in String)
      return Printer_List is

      use Interfaces.C.Strings;

      Count : Integer;
      Hook  : System.Address;
      Printer_Name_String : constant Interfaces.C.Char_Array
                          := Interfaces.C.To_C (Printer_Name,
			                        Append_Nul => True);
   begin
      Hook := XpGetPrinterList (Display,
                                Printer_Name_String'Address,
                                Count'Address);
      if System."=" (Hook, System.Null_Address) then
         return Printer_List'(1 .. 0 => (null, null));
      else
         declare
	    type My_Printer_List is
	       array (Natural range 1 .. Count) of Internal_Printer_Rec;
	    type My_Printer_List_Access is access all My_Printer_List;
	    function To_Printer_List is
	       new Ada.Unchecked_Conversion (System.Address,
	                                     My_Printer_List_Access);
            List_Acc : My_Printer_List_Access := To_Printer_List (Hook);
	    Ret_List : Printer_List (1 .. Count);
	 begin
	    for I in 1 .. Count loop
	       if List_Acc (I).Name = Null_Ptr then
                  Ret_List (I).Name := new String'("");
               else
	          Ret_List (I).Name := new String'(Value (List_Acc (I).Name));
               end if;
	       if List_Acc (I).Desc = Null_Ptr then
                  Ret_List (I).Desc := new String'("");
               else
	          Ret_List (I).Desc := new String'(Value (List_Acc (I).Desc));
               end if;
	    end loop;
            XpFreePrinterList (Hook);
            return Ret_List;
	 end;
      end if;
   end Xp_Get_Printer_List;


   function Xp_Get_Printer_List
     (Display       : in Display_Pointer)
      return Printer_List is

      use Interfaces.C.Strings;

      Count : Integer;
      Hook  : System.Address;
   begin
      Hook := XpGetPrinterList (Display,
                                System.Null_Address,
                                Count'Address);
      if System."=" (Hook, System.Null_Address) then
         return Printer_List'(1 .. 0 => (null, null));
      else
         declare
	    type My_Printer_List is
	       array (Natural range 1 .. Count) of Internal_Printer_Rec;
	    type My_Printer_List_Access is access all My_Printer_List;
	    function To_Printer_List is
	       new Ada.Unchecked_Conversion (System.Address,
	                                     My_Printer_List_Access);
            List_Acc : My_Printer_List_Access := To_Printer_List (Hook);
	    Ret_List : Printer_List (1 .. Count);
	 begin
	    for I in 1 .. Count loop
	       if List_Acc (I).Name = Null_Ptr then
                  Ret_List (I).Name := new String'("");
               else
	          Ret_List (I).Name := new String'(Value (List_Acc (I).Name));
               end if;
	       if List_Acc (I).Desc = Null_Ptr then
                  Ret_List (I).Desc := new String'("");
               else
	          Ret_List (I).Desc := new String'(Value (List_Acc (I).Desc));
               end if;
	    end loop;
            XpFreePrinterList (Hook);
            return Ret_List;
	 end;
      end if;
   end Xp_Get_Printer_List;



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
      Num_Elements      :    out Integer) is       -- WILL BE CHANGED!
      function XpGetPdmStartParams
        (Print_Display     : in Display_Pointer;
         Print_Window      : in Window_ID;
         Print_Context     : in Xp_Context_ID;
         Video_Display     : in Display_Pointer;
         Video_Window      : in Window_ID;
         Selection_Display : in System.Address;
         Selection         : in System.Address;
         Selection_Type    : in System.Address;
         Format            : in System.Address;
         Data              : in System.Address;
         Num_Elements      : in System.Address)
	 return Status_Type;
      pragma Import (C, XpGetPdmStartParams, "XpGetPdmStartParams");

   begin
      if XpGetPdmStartParams (Print_Display, Print_Window, Print_Context,
                              Video_Display, Video_Window,
			      Selection_Display'Address, Selection'Address,
			      Selection_Type'Address, Format'Address,
			      Data'Address, Num_Elements'Address) =
	 Error_Status then
	 raise Xp_Error;
      end if;
   end Xp_Get_Pdm_Start_Params;


   procedure Xp_Get_Auth_Params
     (Print_Display     : in     Display_Pointer;
      Video_Display     : in     Display_Pointer;  -- can be different
                                                   -- from Print_Display!
      Selection_Display :    out Display_Pointer;
      Selection         :    out Atom;
      Target            :    out Atom) is
      function XpGetAuthParams
        (Print_Display     : in Display_Pointer;
         Video_Display     : in Display_Pointer;
         Selection_Display : in System.Address;
         Selection         : in System.Address;
         Target            : in System.Address)
	 return Status_Type;
      pragma Import (C, XpGetAuthParams, "XpGetAuthParams");
   begin
      if XpGetAuthParams (Print_Display, Video_Display,
                          Selection_Display'Address, Selection'Address,
			  Target'Address) = Error_Status then
	 raise Xp_Error;
      end if;
   end Xp_Get_Auth_Params;


   procedure Xp_Send_Auth
     (Display       : in Display_Pointer;
      Window        : in Window_ID) is
      function XpSendAuth
        (Display       : in Display_Pointer;
         Window        : in Window_ID)
	 return Status_Type;
      pragma Import (C, XpSendAuth, "XpSendAuth");
   begin
      if XpSendAuth (Display, Window) = Error_Status then
	 raise Xp_Error;
      end if;
   end Xp_Send_Auth;


   procedure Xp_Set_Locale_Hinter
     (Hinter_Proc : in Xp_Hinter_Proc;
      Hinter_Desc : in String) is
      procedure XpSetLocaleHinter
        (Hinter_Proc : in Xp_Hinter_Proc;
         Hinter_Desc : in System.Address);
      pragma Import (C, XpSetLocaleHinter, "XpSetLocaleHinter");
      Hinter_Desc_String : constant Interfaces.C.Char_Array
                         := Interfaces.C.To_C (Hinter_Desc,
			                       Append_Nul => True);
   begin
      XpSetLocaleHinter (Hinter_Proc, Hinter_Desc_String'Address);
   end Xp_Set_Locale_Hinter;


   function Xp_Get_Locale_Hinter
     (Hinter_Proc : in Xp_Hinter_Proc)
      return String is
      function XpGetLocaleHinter
        (Hinter_Proc : in Xp_Hinter_Proc)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XpGetLocaleHinter, "XpGetLocaleHinter");
      use Interfaces.C.Strings;
      Ptr : Chars_Ptr;
   begin
      Ptr := XpGetLocaleHinter (Hinter_Proc);
      if Ptr = Null_Ptr then
         return "";
      else
         -- I don't know if the string pointer to can be released afterwards,
	 -- so for the moment I leave it
         return Value (Ptr);
      end if;
   end Xp_Get_Locale_Hinter;


   --  return the String for the current locale
   --
   function Xp_Get_Locale_Net_String return String is
      function XpGetLocaleNetString return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XpGetLocaleNetString, "XpGetLocaleNetString");
      use Interfaces.C.Strings;
      Ptr : Chars_Ptr;
   begin
      Ptr := XpGetLocaleNetString;
      if Ptr = Null_Ptr then
         return "";
      else
         -- don't know if we can release the memory => no "free"
	 return Value (Ptr);
      end if;
   end Xp_Get_Locale_Net_String;


   --  inform the print display manager (about what???)
   --
   function Xp_Notify_Pdm
     (Display       : in X_Lib.Display_Pointer;
      Window        : in X_Lib.Window_ID;
      Context       : in Xp_Context_ID;
      Video_Display : in X_Lib.Display_Pointer;
      Video_Window  : in X_Lib.Window_ID;
      Auth_Flag     : in Boolean)
      return String is
      function XpNotifyPdm
        (Display       : in X_Lib.Display_Pointer;
         Window        : in X_Lib.Window_ID;
         Context       : in Xp_Context_ID;
         Video_Display : in X_Lib.Display_Pointer;
         Video_Window  : in X_Lib.Window_ID;
         Auth_Flag     : in X_Lib.X_Boolean)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XpNotifyPdm, "XpNotifyPdm");
      use Interfaces.C.Strings;
      Ptr : Chars_Ptr;
   begin
      Ptr := XpNotifyPdm (Display, Window, Context,
                          Video_Display, Video_Window,
                          X_Lib.To_X_Boolean (Auth_Flag));
      if Ptr = Null_Ptr then
         return "";
      else
         -- don't know if we can release the memory => no "free"
         return Value (Ptr);
      end if;
   end Xp_Notify_Pdm;

-- EndX11R6.3

end X_Lib.Extensions.Print;
