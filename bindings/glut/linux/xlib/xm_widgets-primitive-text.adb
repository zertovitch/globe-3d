-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
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
--          22 Jun 2001 Vadim Godunko: workaround for bug in gnat 3.13p runtime
--                                     new conditional compile depending on
--                                     Gnat3.13p
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C.Strings,
     Interfaces.C.Wstrings,
     X_Toolkit.Internal;
package body Xm_Widgets.Primitive.Text is
 
   procedure XtFree (Str : in Interfaces.C.Strings.chars_ptr);
   procedure XtFree (Str : in Interfaces.C.Wstrings.wchars_ptr);
   pragma Import (C, XtFree, "XtFree");


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Text_Verify_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Text_Verify_Callback_Struct,
            Xm_Text_Verify_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Value_Changed,
	                                2 => Cr_Focus,
	                                3 => Cr_Losing_Focus,
					4 => Cr_Modifying_Text_Value,
				        5 => Cr_Moving_Insert_Cursor));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Text_Verify_Callback_Struct_Wcs_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Text_Verify_Callback_Struct_Wcs,
            Xm_Text_Verify_Callback_Struct_Wcs_Access,
            Callback_Reason_Array_Type'(1 => Cr_Value_Changed,
	                                2 => Cr_Focus,
	                                3 => Cr_Losing_Focus,
					4 => Cr_Modifying_Text_Value,
				        5 => Cr_Moving_Insert_Cursor));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);




   function Xm_Is_Text (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Text_Widget_Class);
   end Xm_Is_Text;


   function Xm_Is_Text_Field (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Text_Field_Widget_Class);
   end Xm_Is_Text_Field;


   function Xm_Create_Text
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateText
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateText, "XmCreateText");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateText (Parent,
                           Name_String'Address,
                           X_Toolkit.Internal.Hook (Arglist),
                           Cardinal (Length (Arglist)));
   end Xm_Create_Text;


   function Xm_Create_Scrolled_Text
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateScrolledText
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateScrolledText, "XmCreateScrolledText");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateScrolledText (Parent,
                           Name_String'Address,
                           X_Toolkit.Internal.Hook (Arglist),
                           Cardinal (Length (Arglist)));
   end Xm_Create_Scrolled_Text;


   function Xm_Create_Text_Field
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateTextField
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateTextField, "XmCreateTextField");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateTextField (Parent,
                                Name_String'Address,
                                X_Toolkit.Internal.Hook (Arglist),
                                Cardinal (Length (Arglist)));
   end Xm_Create_Text_Field;



   procedure Xm_Text_Set_String (W     : in Widget;
                                 Value : in String) is
      procedure XmTextSetString (W     : in Widget;
                                 Value : in System.Address);
      pragma Import (C, XmTextSetString, "XmTextSetString");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextSetString (W, Value_String'Address);
   end Xm_Text_Set_String;


   procedure Xm_Text_Set_String (W     : in Widget;
                                 Value : in Wide_String) is
      procedure XmTextSetStringWcs (W     : in Widget;
                                    Value : in System.Address);
      pragma Import (C, XmTextSetStringWcs, "XmTextSetStringWcs");

-- UseGnat3.13p
--!    begin
--!       --  the Gnat 3.13p runtime (and possibly earlies ones as well) raise
--!       --  STORAGE_ERROR for Value with 'Length = 0
--!       --  This is a workaround to this problem
--!       --
--!       if Value'Length = 0 then
--!          declare
--!             Value_String : constant Interfaces.C.Wchar_Array (0 .. 0) :=
--!               (others => Interfaces.C.wide_nul);
--!          begin
--!             XmTextFieldSetStringWcs (W, Value_String'Address);
--!          end;
--!       else
--!          declare
--!             Value_String : constant Interfaces.C.Wchar_Array
--!                          := Interfaces.C.To_C (Value, Append_Nul => True);
--!          begin
--!             XmTextFieldSetStringWcs (W, Value_String'Address);
--!          end;
--!       end if;
-- NotGnat3.13p
      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextSetStringWcs (W, Value_String'Address);
-- EndGnat3.13p
   end Xm_Text_Set_String;


   procedure Xm_Text_Field_Set_String (W     : in Widget;
                                       Value : in String) is
      procedure XmTextFieldSetString (W     : in Widget;
                                      Value : in System.Address);
      pragma Import (C, XmTextFieldSetString, "XmTextFieldSetString");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldSetString (W, Value_String'Address);
   end Xm_Text_Field_Set_String;


   procedure Xm_Text_Field_Set_String (W     : in Widget;
                                       Value : in Wide_String) is
      procedure XmTextFieldSetStringWcs (W     : in Widget;
                                         Value : in System.Address);
      pragma Import (C, XmTextFieldSetStringWcs, "XmTextFieldSetStringWcs");

      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldSetStringWcs (W, Value_String'Address);
   end Xm_Text_Field_Set_String;



   function Xm_Text_Get_String (W : Widget) return String is
      function XmTextGetString (W : Widget) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XmTextGetString, "XmTextGetString");

      X_S : Interfaces.C.Strings.chars_ptr;
   begin
      X_S := XmTextGetString (W);
      declare
         Return_String : constant String := Interfaces.C.Strings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Get_String;


   function Xm_Text_Get_String (W : Widget) return Wide_String is
      function XmTextGetStringWcs (W : Widget) return Interfaces.C.Wstrings.wchars_ptr;
      pragma Import (C, XmTextGetStringWcs, "XmTextGetStringWcs");

      X_S : Interfaces.C.Wstrings.wchars_ptr;
   begin
      X_S := XmTextGetStringWcs (W);
      declare
         Return_String : constant Wide_String := Interfaces.C.Wstrings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Get_String;


   function Xm_Text_Field_Get_String (W : Widget) return String is
      function XmTextFieldGetString (W : Widget) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XmTextFieldGetString, "XmTextFieldGetString");

      X_S : Interfaces.C.Strings.chars_ptr;
   begin
      X_S := XmTextFieldGetString (W);
      declare
         Return_String : constant String := Interfaces.C.Strings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Field_Get_String;


   function Xm_Text_Field_Get_String (W : Widget) return Wide_String is
      function XmTextFieldGetStringWcs (W : Widget) return Interfaces.C.Wstrings.wchars_ptr;
      pragma Import (C, XmTextFieldGetStringWcs, "XmTextFieldGetStringWcs");

      X_S : Interfaces.C.Wstrings.wchars_ptr;
   begin
      X_S := XmTextFieldGetStringWcs (W);
      declare
         Return_String : constant Wide_String := Interfaces.C.Wstrings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Field_Get_String;



   procedure Xm_Text_Replace (W        : in Widget;
                              From_Pos : in Xm_Text_Position;
                              To_Pos   : in Xm_Text_Position;
                              Value    : in String) is
      procedure XmTextReplace (W        : in Widget;
                              From_Pos : in Xm_Text_Position;
                              To_Pos   : in Xm_Text_Position;
                              Value    : in System.Address);
      pragma Import (C, XmTextReplace,"XmTextReplace");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextReplace (W, From_Pos, To_Pos, Value_String'Address);
   end Xm_Text_Replace;


   procedure Xm_Text_Replace (W        : in Widget;
                              From_Pos : in Xm_Text_Position;
                              To_Pos   : in Xm_Text_Position;
                              Value    : in Wide_String) is
      procedure XmTextReplaceWcs (W        : in Widget;
                                  From_Pos : in Xm_Text_Position;
                                  To_Pos   : in Xm_Text_Position;
                                  Value    : in System.Address);
      pragma Import (C, XmTextReplaceWcs,"XmTextReplaceWcs");

      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextReplaceWcs (W, From_Pos, To_Pos, Value_String'Address);
   end Xm_Text_Replace;


   procedure Xm_Text_Field_Replace (W        : in Widget;
                                    From_Pos : in Xm_Text_Position;
                                    To_Pos   : in Xm_Text_Position;
                                    Value    : in String) is
      procedure XmTextFieldReplace (W        : in Widget;
                                    From_Pos : in Xm_Text_Position;
                                    To_Pos   : in Xm_Text_Position;
                                    Value    : in System.Address);
      pragma Import (C, XmTextFieldReplace,"XmTextFieldReplace");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldReplace (W, From_Pos, To_Pos, Value_String'Address);
   end Xm_Text_Field_Replace;


   procedure Xm_Text_Field_Replace (W        : in Widget;
                                    From_Pos : in Xm_Text_Position;
                                    To_Pos   : in Xm_Text_Position;
                                    Value    : in Wide_String) is
      procedure XmTextFieldReplaceWcs (W        : in Widget;
                                       From_Pos : in Xm_Text_Position;
                                       To_Pos   : in Xm_Text_Position;
                                       Value    : in System.Address);
      pragma Import (C, XmTextFieldReplaceWcs,"XmTextFieldReplaceWcs");

      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldReplaceWcs (W, From_Pos, To_Pos, Value_String'Address);
   end Xm_Text_Field_Replace;


   procedure Xm_Text_Insert (W        : in Widget;
                             Position : in Xm_Text_Position;
                             Value    : in String) is
      procedure XmTextInsert (W        : in Widget;
                              Position : in Xm_Text_Position;
                              Value    : in System.Address);
      pragma Import (C, XmTextInsert,"XmTextInsert");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextInsert (W, Position, Value_String'Address);
   end Xm_Text_Insert;


   procedure Xm_Text_Insert (W        : in Widget;
                             Position : in Xm_Text_Position;
                             Value    : in Wide_String) is
      procedure XmTextInsertWcs (W        : in Widget;
                                 Position : in Xm_Text_Position;
                                 Value    : in System.Address);
      pragma Import (C, XmTextInsertWcs,"XmTextInsertWcs");

      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextInsertWcs (W, Position, Value_String'Address);
   end Xm_Text_Insert;


   procedure Xm_Text_Field_Insert (W        : in Widget;
                                   Position : in Xm_Text_Position;
                                   Value    : in String) is
      procedure XmTextFieldInsert (W        : in Widget;
                                   Position : in Xm_Text_Position;
                                   Value    : in System.Address);
      pragma Import (C, XmTextFieldInsert,"XmTextFieldInsert");

      Value_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldInsert (W, Position, Value_String'Address);
   end Xm_Text_Field_Insert;


   procedure Xm_Text_Field_Insert (W        : in Widget;
                                   Position : in Xm_Text_Position;
                                   Value    : in Wide_String) is
      procedure XmTextFieldInsertWcs (W        : in Widget;
                                      Position : in Xm_Text_Position;
                                      Value    : in System.Address);
      pragma Import (C, XmTextFieldInsertWcs,"XmTextFieldInsertWcs");

      Value_String : constant Interfaces.C.Wchar_Array
                   := Interfaces.C.To_C (Value, Append_Nul => True);
   begin
      XmTextFieldInsertWcs (W, Position, Value_String'Address);
   end Xm_Text_Field_Insert;



   procedure Xm_Text_Set_Add_Mode
     (W         : in Widget;
      State     : in Boolean) is
      procedure XmTextSetAddMode
        (W         : in Widget;
-- Use64Bit
--!          State     : in Integer);
-- Not64Bit
         State     : in Xt_Boolean);
-- End64Bit
      pragma Import (C, XmTextSetAddMode, "XmTextSetAddMode");
   begin
-- Use64Bit
--!       if State then
--!          XmTextSetAddMode (W, 1);
--!       else
--!          XmTextSetAddMode (W, 0);
--!       end if;
-- Not64Bit
      XmTextSetAddMode (W, To_Xt_Boolean (State));
-- End64Bit
   end Xm_Text_Set_Add_Mode;


   procedure Xm_Text_Field_Set_Add_Mode
     (W         : in Widget;
      State     : in Boolean) is
      procedure XmTextFieldSetAddMode
        (W         : in Widget;
-- Use64Bit
--!          State     : in Integer);
-- Not64Bit
         State     : in Xt_Boolean);
-- End64Bit
      pragma Import (C, XmTextFieldSetAddMode, "XmTextFieldSetAddMode");
   begin
-- Use64Bit
--!       if State then
--!          XmTextFieldSetAddMode (W, 1);
--!       else
--!          XmTextFieldSetAddMode (W, 0);
--!       end if;
-- Not64Bit
      XmTextFieldSetAddMode (W, To_Xt_Boolean (State));
-- End64Bit
   end Xm_Text_Field_Set_Add_Mode;



-- NotLesstif
   function Xm_Text_Get_Add_Mode (W : in Widget) return Boolean is
      function XmTextGetAddMode (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextGetAddMode, "XmTextGetAddMode");
   begin
      return XmTextGetAddMode (W) = Xt_Boolean'(True);
   end Xm_Text_Get_Add_Mode;


   function Xm_Text_Field_Get_Add_Mode (W : in Widget) return Boolean is
      function XmTextFieldGetAddMode (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextFieldGetAddMode, "XmTextFieldGetAddMode");
   begin
      return XmTextFieldGetAddMode (W) = Xt_Boolean'(True);
   end Xm_Text_Field_Get_Add_Mode;
-- EndLesstif



   procedure Xm_Text_Set_Editable
     (W         : in Widget;
      Editable  : in Boolean) is
      procedure XmTextSetEditable
        (W        : in Widget;
-- Use64Bit
--!          Editable  : in Integer);
-- Not64Bit
         Editable : in Xt_Boolean);
-- End64Bit
      pragma Import (C, XmTextSetEditable, "XmTextSetEditable");
   begin
-- Use64Bit
--!       if Editable then
--!          XmTextSetEditable (W, 1);
--!       else
--!          XmTextSetEditable (W, 0);
--!       end if;
-- Not64Bit
      XmTextSetEditable (W, To_Xt_Boolean (Editable));
-- End64Bit
   end Xm_Text_Set_Editable;


   procedure Xm_Text_Field_Set_Editable
     (W         : in Widget;
      Editable  : in Boolean) is
      procedure XmTextFieldSetEditable
        (W        : in Widget;
-- Use64Bit
--!          Editable  : in Integer);
-- Not64Bit
         Editable : in Xt_Boolean);
-- End64Bit
      pragma Import (C, XmTextFieldSetEditable, "XmTextFieldSetEditable");
   begin
-- Use64Bit
--!       if Editable then
--!          XmTextFieldSetEditable (W, 1);
--!       else
--!          XmTextFieldSetEditable (W, 0);
--!       end if;
-- Not64Bit
      XmTextFieldSetEditable (W, To_Xt_Boolean (Editable));
-- End64Bit
   end Xm_Text_Field_Set_Editable;



   function Xm_Text_Get_Editable (W : Widget) return Boolean is
      function XmTextGetEditable (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextGetEditable, "XmTextGetEditable");
   begin
      return XmTextGetEditable (W) = Xt_Boolean'(True);
   end Xm_Text_Get_Editable;


   function Xm_Text_Field_Get_Editable (W : Widget) return Boolean is
      function XmTextFieldGetEditable (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextFieldGetEditable, "XmTextFieldGetEditable");
   begin
      return XmTextFieldGetEditable (W) = Xt_Boolean'(True);
   end Xm_Text_Field_Get_Editable;



   procedure Xm_Text_Get_Selection_Position
     (W           : in     Widget;
      Left, Right :    out Xm_Text_Position) is
      function XmTextGetSelectionPosition
        (W           : in Widget;
         Left, Right : in System.Address) return Xt_Boolean;
      pragma Import (C, XmTextGetSelectionPosition,
                       "XmTextGetSelectionPosition");
   begin
      if XmTextGetSelectionPosition (W, Left'Address, Right'Address) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Get_Selection_Position;


   procedure Xm_Text_Field_Get_Selection_Position
     (W           : in     Widget;
      Left, Right :    out Xm_Text_Position) is
      function XmTextFieldGetSelectionPosition
        (W           : in Widget;
         Left, Right : in System.Address) return Xt_Boolean;
      pragma Import (C, XmTextFieldGetSelectionPosition,
                       "XmTextFieldGetSelectionPosition");
   begin
      if XmTextFieldGetSelectionPosition (W, Left'Address, Right'Address) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Get_Selection_Position;


   function Xm_Text_Get_Selection (W : in Widget) return String is
      function XmTextGetSelection (W : Widget) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XmTextGetSelection, "XmTextGetSelection");

      X_S : Interfaces.C.Strings.chars_ptr;
   begin
      X_S := XmTextGetSelection (W);
      declare
         Return_String : constant String := Interfaces.C.Strings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Get_Selection;


   function Xm_Text_Get_Selection (W : in Widget) return Wide_String is
      function XmTextGetSelectionWcs (W : Widget) return Interfaces.C.Wstrings.wchars_ptr;
      pragma Import (C, XmTextGetSelectionWcs, "XmTextGetSelectionWcs");

      X_S : Interfaces.C.Wstrings.wchars_ptr;
   begin
      X_S := XmTextGetSelectionWcs (W);
      declare
         Return_String : constant Wide_String := Interfaces.C.Wstrings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Get_Selection;


   function Xm_Text_Field_Get_Selection (W : in Widget) return String is
      function XmTextFieldGetSelection (W : in Widget) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XmTextFieldGetSelection,"XmTextFieldGetSelection");

      X_S : Interfaces.C.Strings.chars_ptr;
   begin
      X_S := XmTextFieldGetSelection (W);
      declare
         Return_String : constant String := Interfaces.C.Strings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Field_Get_Selection;


   function Xm_Text_Field_Get_Selection (W : in Widget) return Wide_String is
      function XmTextFieldGetSelectionWcs (W : in Widget) return Interfaces.C.Wstrings.wchars_ptr;
      pragma Import (C, XmTextFieldGetSelectionWcs,"XmTextFieldGetSelectionWcs");

      X_S : Interfaces.C.Wstrings.wchars_ptr;
   begin
      X_S := XmTextFieldGetSelectionWcs (W);
      declare
         Return_String : constant Wide_String := Interfaces.C.Wstrings.Value (X_S);
      begin
         XtFree (X_S);
         return Return_String;
      end;
   end Xm_Text_Field_Get_Selection;



   procedure Xm_Text_Remove (W : in Widget) is
      function XmTextRemove (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextRemove, "XmTextRemove");
   begin
      if XmTextRemove (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Remove;


   procedure Xm_Text_Field_Remove (W : in Widget) is
      function XmTextFieldRemove (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextFieldRemove, "XmTextFieldRemove");
   begin
      if XmTextFieldRemove (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Remove;



   procedure Xm_Text_Copy
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextCopy
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextCopy, "XmTextCopy");
   begin
      if XmTextCopy (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Copy;


   procedure Xm_Text_Field_Copy
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextFieldCopy
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextFieldCopy, "XmTextFieldCopy");
   begin
      if XmTextFieldCopy (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Copy;



-- UseMotif2.0 Motif2.1
   procedure Xm_Text_Copy_Link
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextCopyLink
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextCopyLink, "XmTextCopyLink");
   begin
      if XmTextCopyLink (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Copy_Link;


   procedure Xm_Text_Field_Copy_Link
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextFieldCopyLink
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextFieldCopyLink, "XmTextFieldCopyLink");
   begin
      if XmTextFieldCopyLink (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Copy_Link;
-- EndMotif2.0 Motif2.1



   procedure Xm_Text_Cut
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextCut
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextCut, "XmTextCut");
   begin
      if XmTextCut (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Cut;


   procedure Xm_Text_Field_Cut
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time) is
      function XmTextFieldCut
        (W         : in Widget;
         Clip_Time : in X_Lib.Server_Time)
         return Xt_Boolean;
      pragma Import (C, XmTextFieldCut, "XmTextFieldCut");
   begin
      if XmTextFieldCut (W, Clip_Time) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Cut;



   procedure Xm_Text_Paste (W : in Widget) is
      function XmTextPaste (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextPaste, "XmTextPaste");
   begin
      if XmTextPaste (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Paste;


   procedure Xm_Text_Field_Paste (W : in Widget) is
      function XmTextFieldPaste (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextFieldPaste, "XmTextFieldPaste");
   begin
      if XmTextFieldPaste (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Paste;


-- UseMotif2.0 Motif2.1
   procedure Xm_Text_Paste_Link (W : in Widget) is
      function XmTextPasteLink (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextPasteLink, "XmTextPasteLink");
   begin
      if XmTextPasteLink (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Paste_Link;


   procedure Xm_Text_Field_Paste_Link (W : in Widget) is
      function XmTextFieldPasteLink (W : in Widget) return Xt_Boolean;
      pragma Import (C, XmTextFieldPasteLink, "XmTextFieldPasteLink");
   begin
      if XmTextFieldPasteLink (W) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Dont_Own_Primary_Selection;
      end if;
   end Xm_Text_Field_Paste_Link;
-- EndMotif2.0 Motif2.1



   procedure Xm_Text_Pos_To_XY
     (W        : in     Widget;
      Pos      : in     Xm_Text_Position;
      X, Y     :    out X_Lib.Position) is
      function XmTextPosToXY
        (W        : in Widget;
         Pos      : in Xm_Text_Position;
         X, Y     : in System.Address)
         return Xt_Boolean;
      pragma Import (C, XmTextPosToXY, "XmTextPosToXY");
   begin
      if XmTextPosToXY (W, Pos, X'Address, Y'Address) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Not_In_Text_Widget;
      end if;
   end Xm_Text_Pos_To_XY;


   procedure Xm_Text_Field_Pos_To_XY
     (W        : in     Widget;
      Pos      : in     Xm_Text_Position;
      X, Y     :    out X_Lib.Position) is
      function XmTextFieldPosToXY
        (W        : in Widget;
         Pos      : in Xm_Text_Position;
         X, Y     : in System.Address)
         return Xt_Boolean;
      pragma Import (C, XmTextFieldPosToXY, "XmTextFieldPosToXY");
   begin
      if XmTextFieldPosToXY (W, Pos, X'Address, Y'Address) = Xt_Boolean'(False) then
         raise Xm_Text_Error_Not_In_Text_Widget;
      end if;
   end Xm_Text_Field_Pos_To_XY;


   function To_Integer is
      new Ada.Unchecked_Conversion (Edit_Mode, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Edit_Mode) is
   begin
      Append_Set (List   => List,
                  Name   => Name,
                  Value  => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Edit_Mode) is
   begin
      Append_Set (List   => List,
                  Name   => Name,
                  Value  => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


end Xm_Widgets.Primitive.Text;
