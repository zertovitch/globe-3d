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
--          19 May 2001 Vadim Godunko: addition of Xm_Text_Get_Centerline,
--                                     Xm_Text_Enable/Disable_Redisplay,
--                                     Xm_Text_Set/Get_Top_Character,
--                                     Xm_Text_Scroll
--
-------------------------------------------------------------------------------

package Xm_Widgets.Primitive.Text is
 
   Xm_Text_Widget_Class                : constant Widget_Class;
   Xm_Text_Field_Widget_Class          : constant Widget_Class;



   type Xm_Text_Verify_Callback_Struct is record
      Reason      : Callback_Reason;
      Event       : X_Lib.X_Event_Pointer;
      Doit        : Boolean;
      Curr_Insert,
      New_Insert  : Xm_Text_Position;
      Start_Pos,
      End_Pos     : Xm_Text_Position;
      Text        : Xm_Text_Block;
   end record;
   pragma Convention (C, Xm_Text_Verify_Callback_Struct);

   type Xm_Text_Verify_Callback_Struct_Access is
      access all Xm_Text_Verify_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Text_Verify_Callback_Struct_Access;


   type Xm_Text_Verify_Callback_Struct_Wcs is record
      Reason      : Callback_Reason;
      Event       : X_Lib.X_Event_Pointer;
      Doit        : Boolean;
      Curr_Insert,
      New_Insert  : Xm_Text_Position;
      Start_Pos,
      End_Pos     : Xm_Text_Position;
      Text        : Xm_Text_Block_Wcs;
   end record;
   pragma Convention (C, Xm_Text_Verify_Callback_Struct_Wcs);

   type Xm_Text_Verify_Callback_Struct_Wcs_Access is
      access all Xm_Text_Verify_Callback_Struct_Wcs;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Text_Verify_Callback_Struct_Wcs_Access;


   type Xm_Text_Source is private;


   function Xm_Is_Text (W : in Widget) return Boolean;
   function Xm_Is_Text_Field (W : in Widget) return Boolean;

   function Xm_Create_Text
     (Parent   : in Widget;
      Name     : in String;
      Arglist  : in Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Scrolled_Text
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Text_Field
     (Parent   : in Widget;
      Name     : in String;
      Arglist  : in Arg_List := Null_Arg_List)
      return Widget;



   function Xm_Text_Get_String (W : in Widget) return String;
   function Xm_Text_Get_String (W : in Widget) return Wide_String;

   function Xm_Text_Field_Get_String (W : in Widget) return String;
   function Xm_Text_Field_Get_String (W : in Widget) return Wide_String;

   -- procedure Xm_Text_Get_Substring  not yet implemented
   -- procedure Xm_Text_Get_Substring_Wcs  not yet implemented
   -- procedure Xm_Text_Field_Get_Substring  not yet implemented
   -- procedure Xm_Text_Field_Get_Substring_Wcs  not yet implemented


   function Xm_Text_Get_Last_Position (W : in Widget)
      return Xm_Text_Position;
   function Xm_Text_Field_Get_Last_Position (W : in Widget)
      return Xm_Text_Position;


   procedure Xm_Text_Set_String (W     : in Widget;
                                 Value : in String);
   procedure Xm_Text_Set_String (W     : in Widget;
                                 Value : in Wide_String);
   procedure Xm_Text_Field_Set_String (W     : in Widget;
                                       Value : in String);
   procedure Xm_Text_Field_Set_String (W     : in Widget;
                                       Value : in Wide_String);


   procedure Xm_Text_Replace (W        : in Widget;
                              From_Pos : in Xm_Text_Position;
                              To_Pos   : in Xm_Text_Position;
                              Value    : in String);
   procedure Xm_Text_Replace (W        : in Widget;
                              From_Pos : in Xm_Text_Position;
                              To_Pos   : in Xm_Text_Position;
                              Value    : in Wide_String);
   procedure Xm_Text_Field_Replace (W        : in Widget;
                                    From_Pos : in Xm_Text_Position;
                                    To_Pos   : in Xm_Text_Position;
                                    Value    : in String);
   procedure Xm_Text_Field_Replace (W        : in Widget;
                                    From_Pos : in Xm_Text_Position;
                                    To_Pos   : in Xm_Text_Position;
                                    Value    : in Wide_String);


   procedure Xm_Text_Insert (W        : in Widget;
                             Position : in Xm_Text_Position;
                             Value    : in String);
   procedure Xm_Text_Insert (W        : in Widget;
                             Position : in Xm_Text_Position;
                             Value    : in Wide_String);
   procedure Xm_Text_Field_Insert (W        : in Widget;
                                   Position : in Xm_Text_Position;
                                   Value    : in String);
   procedure Xm_Text_Field_Insert (W        : in Widget;
                                   Position : in Xm_Text_Position;
                                   Value    : in Wide_String);

   procedure Xm_Text_Set_Add_Mode
     (W         : in Widget;
      State     : in Boolean);
   procedure Xm_Text_Field_Set_Add_Mode
     (W         : in Widget;
      State     : in Boolean);


-- NotLesstif
   function Xm_Text_Get_Add_Mode (W : in Widget) return Boolean;
   function Xm_Text_Field_Get_Add_Mode (W : in Widget) return Boolean;
-- EndLesstif


   procedure Xm_Text_Set_Editable
     (W         : in Widget;
      Editable  : in Boolean);
   procedure Xm_Text_Field_Set_Editable
     (W         : in Widget;
      Editable  : in Boolean);

   function Xm_Text_Get_Editable (W : in Widget) return Boolean;
   function Xm_Text_Field_Get_Editable (W : in Widget) return Boolean;


   procedure Xm_Text_Set_Max_Length
     (W          : in Widget;
      Max_Length : in Integer);
   procedure Xm_Text_Field_Set_Max_Length
     (W          : in Widget;
      Max_Length : in Integer);

   function Xm_Text_Get_Max_Length (W : in Widget) return Integer;
   function Xm_Text_Field_Get_Max_Length (W : in Widget) return Integer;


   procedure Xm_Text_Set_Cursor_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);
   procedure Xm_Text_Field_Set_Cursor_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);

   function Xm_Text_Get_Cursor_Position (W : in Widget)
      return Xm_Text_Position;
   function Xm_Text_Field_Get_Cursor_Position (W : in Widget)
      return Xm_Text_Position;


   procedure Xm_Text_Set_Insertion_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);
   procedure Xm_Text_Field_Set_Insertion_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);

   function Xm_Text_Get_Insertion_Position (W : in Widget)
      return Xm_Text_Position;
   function Xm_Text_Field_Get_Insertion_Position (W : in Widget)
      return Xm_Text_Position;


   Xm_Text_Error_Dont_Own_Primary_Selection : exception;

   procedure Xm_Text_Get_Selection_Position
     (W           : in     Widget;
      Left, Right :    out Xm_Text_Position);
   procedure Xm_Text_Field_Get_Selection_Position
     (W           : in     Widget;
      Left, Right :    out Xm_Text_Position);


   function Xm_Text_Get_Selection (W : in Widget) return String; 
   function Xm_Text_Field_Get_Selection (W : in Widget) return String; 
   function Xm_Text_Get_Selection (W : in Widget) return Wide_String; 
   function Xm_Text_Field_Get_Selection (W : in Widget) return Wide_String; 


   procedure Xm_Text_Remove (W : in Widget);
   procedure Xm_Text_Field_Remove (W : in Widget);


   procedure Xm_Text_Copy
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);
   procedure Xm_Text_Field_Copy
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);


-- UseMotif2.0 Motif2.1
   procedure Xm_Text_Copy_Link
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);
   procedure Xm_Text_Field_Copy_Link
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);
-- EndMotif2.0 Motif2.1


   procedure Xm_Text_Cut
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);
   procedure Xm_Text_Field_Cut
     (W         : in Widget;
      Clip_Time : in X_Lib.Server_Time);


   procedure Xm_Text_Paste (W : in Widget);
   procedure Xm_Text_Field_Paste (W : in Widget);


-- UseMotif2.0 Motif2.1
   procedure Xm_Text_Paste_Link (W : in Widget);
   procedure Xm_Text_Field_Paste_Link (W : in Widget);
-- EndMotif2.0 Motif2.1


   procedure Xm_Text_Clear_Selection
     (W      : in Widget;
      Time   : in X_Lib.Server_Time);
   procedure Xm_Text_Field_Clear_Selection
     (W      : in Widget;
      Time   : in X_Lib.Server_Time);


   procedure Xm_Text_Set_Selection
     (W      :  Widget;
      First  :  Xm_Text_Position;
      Last   :  Xm_Text_Position;
      Time   :  X_Lib.Server_Time);
   procedure Xm_Text_Field_Set_Selection
     (W      :  Widget;
      First  :  Xm_Text_Position;
      Last   :  Xm_Text_Position;
      Time   :  X_Lib.Server_Time);


   function Xm_Text_XY_To_Pos
     (W    : in Widget;
-- Use64Bit
--!       X, Y : in Integer)
-- Not64Bit
      X, Y : in X_Lib.Position)
-- End64Bit
      return Xm_Text_Position;
   function Xm_Text_Field_XY_To_Pos
     (W    : in Widget;
-- Use64Bit
--!       X, Y : in Integer)
-- Not64Bit
      X, Y : in X_Lib.Position)
-- End64Bit
      return Xm_Text_Position;


   Xm_Text_Error_Not_In_Text_Widget : exception;


   procedure Xm_Text_Pos_To_XY
     (W        : in     Widget;
      Pos      : in     Xm_Text_Position;
      X, Y     :    out X_Lib.Position);
   procedure Xm_Text_Field_Pos_To_XY
     (W        : in     Widget;
      Pos      : in     Xm_Text_Position;
      X, Y     :    out X_Lib.Position);


   procedure Xm_Text_Show_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);
   procedure Xm_Text_Field_Show_Position
     (W        : in Widget;
      Position : in Xm_Text_Position);


   procedure Xm_Text_Set_Highlight
     (W     : in Widget;
      Left  : in Xm_Text_Position;
      Right : in Xm_Text_Position;
      Mode  : in Xm_Highlight_Mode);
   procedure Xm_Text_Field_Set_Highlight
     (W     : in Widget;
      Left  : in Xm_Text_Position;
      Right : in Xm_Text_Position;
      Mode  : in Xm_Highlight_Mode);


   function Xm_Text_Get_Baseline (W : in Widget) return Integer;
   function Xm_Text_Get_Centerline (W : in Widget) return Integer;
   function Xm_Text_Field_Get_Baseline (W : in Widget) return Integer;


   function  Xm_Text_Get_Source (W : in Widget) return Xm_Text_Source;

   procedure Xm_Text_Set_Source (W                : in Widget;
                                 Source           : in Xm_Text_Source;
                                 Top_Position     : in Xm_Text_Position;
                                 Cursor_Position  : in Xm_Text_Position);

   procedure Xm_Text_Disable_Redisplay (W : in Widget);
   procedure Xm_Text_Enable_Redisplay (W : in Widget);

   function Xm_Text_Get_Top_Character (W : in Widget) return Xm_Text_Position;
   procedure Xm_Text_Set_Top_Character
     (W        : in Widget;
      Position : in Xm_Text_Position);

   procedure Xm_Text_Scroll (W : in Widget; Lines : in Integer);


   -- -------------------------------------------------------------------------
   --
   -- new resource strings
   --
   Xm_N_Activate_Callback       : constant Xt_N_Resource_String;

   -- for Xm_N_Auto_Show_Cursor_Position use False or True
   --
   Xm_N_Auto_Show_Cursor_Position : constant Xt_N_Resource_String;

   Xm_N_Cursor_Position         : constant Xt_N_Resource_String;

   -- for Xm_N_Cursor_Position_Visible use False or True
   --
   Xm_N_Cursor_Position_Visible : constant Xt_N_Resource_String;

-- UseMotif2.0 Motif2.1
   Xm_N_Destination_Callback    : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1

   -- for Xm_N_Editable use False or True
   --
   Xm_N_Editable                : constant Xt_N_Resource_String;

   Xm_N_Edit_Mode               : constant Xt_N_Resource_String;

   type Edit_Mode is (Multi_Line_Edit, Single_Line_Edit);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Edit_Mode);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Edit_Mode);
   pragma Convention (C, Append_Get);

   Xm_N_Focus_Callback          : constant Xt_N_Resource_String;
   Xm_N_Gain_Primary_Callback   : constant Xt_N_Resource_String;
   Xm_N_Lose_Primary_Callback   : constant Xt_N_Resource_String;
   Xm_N_Losing_Focus_Callback   : constant Xt_N_Resource_String;
   Xm_N_Margin_Height           : constant Xt_N_Resource_String;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String;

   -- for Xm_N_Max_Length use some positive Integer value (or 0)
   --
   Xm_N_Max_Length              : constant Xt_N_Resource_String;

   Xm_N_Modify_Verify_Callback  : constant Xt_N_Resource_String;
   Xm_N_Modify_Verify_Callback_Wcs : constant Xt_N_Resource_String;
   Xm_N_Motion_Verify_Callback  : constant Xt_N_Resource_String;

   -- for Xm_N_Source use a value of type Xm_Text_Source
   --
   Xm_N_Source                  : constant Xt_N_Resource_String;

   -- for Xm_N_Top_Character use some positive Integer value (or 0)
   --
   Xm_N_Top_Character           : constant Xt_N_Resource_String;

-- UseMotif2.1
   Xm_N_Total_Lines             : constant Xt_N_Resource_String;
-- EndMotif2.1

   Xm_N_Value                   : constant Xt_N_Resource_String;
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String;
   Xm_N_Value_Wcs               : constant Xt_N_Resource_String;

   -- for Xm_N_Verify_Bell use False or True
   --
   Xm_N_Verify_Bell             : constant Xt_N_Resource_String;

   --
   -- new resource strings for text input
   --

   -- for Xm_N_Pending_Delete use False or True
   --
   Xm_N_Pending_Delete          : constant Xt_N_Resource_String;

   Xm_N_Selection_Array         : constant Xt_N_Resource_String;
   Xm_N_Selection_Array_Count   : constant Xt_N_Resource_String;
   Xm_N_Select_Threshold        : constant Xt_N_Resource_String;

   --
   -- new resource strings for text output
   --
   Xm_N_Blink_Rate              : constant Xt_N_Resource_String;
   Xm_N_Columns                 : constant Xt_N_Resource_String;
   Xm_N_Font_List               : constant Xt_N_Resource_String;
-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String;
-- EndMotif2.0 Motif2.1

   -- for Xm_N_Resize_Height use False or True
   --
   Xm_N_Resize_Height           : constant Xt_N_Resource_String;

   -- for Xm_N_Resize_Width use False or True
   --
   Xm_N_Resize_Width            : constant Xt_N_Resource_String;

   Xm_N_Rows                    : constant Xt_N_Resource_String;

   -- for Xm_N_Word_Wrap use False or True
   --
   Xm_N_Word_Wrap               : constant Xt_N_Resource_String;

   --
   -- resource strings for scrolled Text
   --
   Xm_N_Scroll_Horizontal       : constant Xt_N_Resource_String;
   Xm_N_Scroll_Left_Side        : constant Xt_N_Resource_String;
   Xm_N_Scroll_Top_Side         : constant Xt_N_Resource_String;
   Xm_N_Scroll_Vertical         : constant Xt_N_Resource_String;

private

   type Xm_Text_Source is new System.Address;

   for Edit_Mode use (Multi_Line_Edit => 0, Single_Line_Edit => 1);
   for Edit_Mode'Size use Interfaces.C.unsigned_char'Size;


   pragma Import (C, Xm_Text_Get_Last_Position, "XmTextGetLastPosition");
   pragma Import (C, Xm_Text_Field_Get_Last_Position, "XmTextFieldGetLastPosition");
   pragma Import (C, Xm_Text_Set_Max_Length,"XmTextSetMaxLength");
   pragma Import (C, Xm_Text_Field_Set_Max_Length,"XmTextFieldSetMaxLength");
   pragma Import (C, Xm_Text_Get_Max_Length,"XmTextGetMaxLength");
   pragma Import (C, Xm_Text_Field_Get_Max_Length,"XmTextFieldGetMaxLength");
   pragma Import (C, Xm_Text_Set_Cursor_Position, "XmTextSetCursorPosition");
   pragma Import (C, Xm_Text_Field_Set_Cursor_Position, "XmTextFieldSetCursorPosition");
   pragma Import (C, Xm_Text_Get_Cursor_Position, "XmTextGetCursorPosition");
   pragma Import (C, Xm_Text_Field_Get_Cursor_Position, "XmTextFieldGetCursorPosition");
   pragma Import (C, Xm_Text_Set_Insertion_Position,"XmTextSetInsertionPosition");
   pragma Import (C, Xm_Text_Field_Set_Insertion_Position,"XmTextFieldSetInsertionPosition");
   pragma Import (C, Xm_Text_Get_Insertion_Position,"XmTextGetInsertionPosition");
   pragma Import (C, Xm_Text_Field_Get_Insertion_Position,"XmTextFieldGetInsertionPosition");
   pragma Import (C, Xm_Text_Clear_Selection,"XmTextClearSelection");
   pragma Import (C, Xm_Text_Field_Clear_Selection,"XmTextFieldClearSelection");
   pragma Import (C, Xm_Text_Set_Selection,"XmTextSetSelection");
   pragma Import (C, Xm_Text_Field_Set_Selection,"XmTextFieldSetSelection");
   pragma Import (C, Xm_Text_XY_To_Pos, "XmTextXYToPos");
   pragma Import (C, Xm_Text_Field_XY_To_Pos, "XmTextFieldXYToPos");
   pragma Import (C, Xm_Text_Show_Position, "XmTextShowPosition");
   pragma Import (C, Xm_Text_Field_Show_Position, "XmTextFieldShowPosition");
   pragma Import (C, Xm_Text_Set_Highlight, "XmTextSetHighlight");
   pragma Import (C, Xm_Text_Field_Set_Highlight, "XmTextFieldSetHighlight");
   pragma Import (C, Xm_Text_Get_Baseline, "XmTextGetBaseline");
   pragma Import (C, Xm_Text_Get_Centerline, "XmTextGetCenterline");
   pragma Import (C, Xm_Text_Field_Get_Baseline, "XmTextFieldGetBaseline");

   pragma Import (C, Xm_Text_Get_Source,"XmTextGetSource");
   pragma Import (C, Xm_Text_Set_Source,"XmTextSetSource");

   pragma Import (C, Xm_Text_Disable_Redisplay, "XmTextDisableRedisplay");
   pragma Import (C, Xm_Text_Enable_Redisplay, "XmTextEnableRedisplay");
   pragma Import (C, Xm_Text_Get_Top_Character, "XmTextGetTopCharacter");
   pragma Import (C, Xm_Text_Set_Top_Character, "XmTextSetTopCharacter");
   pragma Import (C, Xm_Text_Scroll, "XmTextScroll");

   c_const_Xm_Text_Widget_Class                : Widget_Class;
   c_const_Xm_Text_Field_Widget_Class          : Widget_Class;

   pragma Import (C, c_const_Xm_Text_Widget_Class, "xmTextWidgetClass");
   pragma Import (C, c_const_Xm_Text_Field_Widget_Class, "xmTextFieldWidgetClass");

   Xm_Text_Widget_Class                : constant Widget_Class :=
    c_const_Xm_Text_Widget_Class;
   Xm_Text_Field_Widget_Class          : constant Widget_Class :=
    c_const_Xm_Text_Field_Widget_Class;


   --
   -- Text widget resource strings
   --
   Xm_N_Activate_Callback       : constant Xt_N_Resource_String :=
      Xm_Widgets.Primitive.Xm_N_Activate_Callback;
   Xm_N_Auto_Show_Cursor_Position : constant Xt_N_Resource_String :=
      To_Resource_String ("autoShowCursorPosition");
   Xm_N_Blink_Rate              : constant Xt_N_Resource_String :=
      To_Resource_String ("blinkRate");
   Xm_N_Columns                 : constant Xt_N_Resource_String :=
      To_Resource_String ("columns");
   Xm_N_Cursor_Position         : constant Xt_N_Resource_String :=
      To_Resource_String ("cursorPosition");
   Xm_N_Cursor_Position_Visible : constant Xt_N_Resource_String :=
      To_Resource_String ("cursorPositionVisible");
-- UseMotif2.0 Motif2.1
   Xm_N_Destination_Callback    : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Destination_Callback;
-- EndMotif2.0 Motif2.1
   Xm_N_Editable                : constant Xt_N_Resource_String :=
      To_Resource_String ("editable");
   Xm_N_Edit_Mode               : constant Xt_N_Resource_String :=
      To_Resource_String ("editMode");
   Xm_N_Focus_Callback          : constant Xt_N_Resource_String :=
      To_Resource_String ("focusCallback");
   Xm_N_Font_List               : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Font_List;
   Xm_N_Gain_Primary_Callback   : constant Xt_N_Resource_String :=
      To_Resource_String ("gainPrimaryCallback");
   Xm_N_Lose_Primary_Callback   : constant Xt_N_Resource_String :=
      To_Resource_String ("losePrimaryCallback");
   Xm_N_Losing_Focus_Callback   : constant Xt_N_Resource_String :=
      To_Resource_String ("losingFocusCallback");

   Xm_N_Margin_Height           : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Height;
   Xm_N_Margin_Width            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Margin_Width;
   Xm_N_Max_Length              : constant Xt_N_Resource_String :=
      To_Resource_String ("maxLength");
   Xm_N_Modify_Verify_Callback  : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Modify_Verify_Callback;
   Xm_N_Modify_Verify_Callback_Wcs : constant Xt_N_Resource_String :=
      To_Resource_String ("modifyVerifyCallbackWcs");
   Xm_N_Motion_Verify_Callback  : constant Xt_N_Resource_String :=
      To_Resource_String ("motionVerifyCallback");
   Xm_N_Pending_Delete          : constant Xt_N_Resource_String :=
      To_Resource_String ("pendingDelete");
-- UseMotif2.0 Motif2.1
   Xm_N_Render_Table            : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Render_Table;
-- EndMotif2.0 Motif2.1
   Xm_N_Resize_Height           : constant Xt_N_Resource_String :=
      To_Resource_String ("resizeHeight");
   Xm_N_Resize_Width            : constant Xt_N_Resource_String :=
      To_Resource_String ("resizeWidth");
   Xm_N_Rows                    : constant Xt_N_Resource_String :=
      To_Resource_String ("rows");
   Xm_N_Scroll_Horizontal       : constant Xt_N_Resource_String :=
      To_Resource_String ("scrollHorizontal");
   Xm_N_Scroll_Left_Side        : constant Xt_N_Resource_String :=
      To_Resource_String ("scrollLeftSide");
   Xm_N_Scroll_Top_Side         : constant Xt_N_Resource_String :=
      To_Resource_String ("scrollTopSide");
   Xm_N_Scroll_Vertical         : constant Xt_N_Resource_String :=
      To_Resource_String ("scrollVertical");
   Xm_N_Selection_Array         : constant Xt_N_Resource_String :=
      X_Toolkit.Xt_N_Selection_Array;
   Xm_N_Selection_Array_Count   : constant Xt_N_Resource_String :=
      To_Resource_String ("selectionArrayCount");
   Xm_N_Select_Threshold        : constant Xt_N_Resource_String :=
      To_Resource_String ("selectThreshold");
   Xm_N_Source                  : constant Xt_N_Resource_String :=
      To_Resource_String ("source");
   Xm_N_Top_Character           : constant Xt_N_Resource_String :=
      To_Resource_String ("topCharacter");
-- UseMotif2.1
   Xm_N_Total_Lines             : constant Xt_N_Resource_String :=
      To_Resource_String ("totalLines");
-- EndMotif2.1
   Xm_N_Value                   : constant Xt_N_Resource_String :=
      X_Toolkit.Xt_N_Value;
   Xm_N_Value_Changed_Callback  : constant Xt_N_Resource_String :=
      Xm_Widgets.Xm_N_Value_Changed_Callback;
   Xm_N_Value_Wcs               : constant Xt_N_Resource_String :=
      To_Resource_String ("valueWcs");
   Xm_N_Verify_Bell             : constant Xt_N_Resource_String :=
      To_Resource_String ("verifyBell");
   Xm_N_Word_Wrap               : constant Xt_N_Resource_String :=
      To_Resource_String ("wordWrap");

end Xm_Widgets.Primitive.Text;
