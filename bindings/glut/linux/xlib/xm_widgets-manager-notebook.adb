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

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Manager.Notebook is
 
-- UseMotif2.0 Motif2.1

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Notebook_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Notebook_Callback_Struct,
	    Xm_Notebook_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_None,
	                                2 => Cr_Page_Scroller_Increment,
					3 => Cr_Page_Scroller_Decrement,
					4 => Cr_Major_Tab,
					5 => Cr_Minor_Tab));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Is_Notebook (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Notebook_Widget_Class);
   end Xm_Is_Notebook;


   function Xm_Create_Notebook
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateNotebook
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateNotebook, "XmCreateNotebook");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateNotebook (Parent,
                               Name_String'Address,
                               X_Toolkit.Internal.Hook (Arglist),
                               Cardinal (Length (Arglist)));
   end Xm_Create_Notebook;


   procedure Xm_Notebook_Get_Page_Info (Notebook     : in Widget;
                                        Page_Number  : in Integer;
                                        Page_Info    : out Xm_Notebook_Page_Info) is
      function XmNotebookGetPageInfo (Notebook     : in Widget;
                                      Page_Number  : in Integer;
                                      Page_Info    : in System.Address)
                                      return Interfaces.C.unsigned_char;
      pragma Import (C, XmNotebookGetPageInfo, "XmNotebookGetPageInfo");

      Retval : Interfaces.C.unsigned_char;
   begin
      Retval := XmNotebookGetPageInfo (Notebook, Page_Number, Page_Info'Address);
      case Retval is
         when 0 =>
            return;
         when 1 =>
            raise Xm_Error_Page_Invalid;
         when 2 =>
            raise Xm_Error_Page_Empty;
         when 3 =>
            raise Xm_Error_Page_Duplicated;
         when others =>
            raise Xm_Error_Page_Invalid;
      end case;
   end Xm_Notebook_Get_Page_Info;


   function To_Integer is
      new Ada.Unchecked_Conversion (Binding_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Binding_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Binding_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Notebook_Child_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Notebook_Child_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Notebook_Child_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Notebook;
