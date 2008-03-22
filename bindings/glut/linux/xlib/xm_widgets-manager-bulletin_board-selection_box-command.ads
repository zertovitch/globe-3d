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

package Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Command is

   Xm_Command_Widget_Class             : constant Widget_Class;


   type Xm_Command_Callback_Struct is record
      Reason  : Callback_Reason;
      Event   : X_Lib.X_Event_Pointer;
      Value   : Xm_String;
      Length  : Integer;
   end record;
   pragma Convention (C, Xm_Command_Callback_Struct);

   type Xm_Command_Callback_Struct_Access is
      access all Xm_Command_Callback_Struct;

   -- convert a Pointer (Call_Data of a callback function) into a
   -- callback struct access if possible
   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Command_Callback_Struct_Access;


   function Xm_Is_Command (W: in Widget) return Boolean;


   function Xm_Create_Command
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   function Xm_Create_Command_Dialog
     (Parent   : in  Widget;
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget;

   type Command_Child_Type is (None,
                               History_List,
                               Prompt_Label,
                               Command_Text);


   function Xm_Command_Get_Child (W      : in Widget;
                                  Child  : in Command_Child_Type)
      return Widget;

   procedure Xm_Command_Append_Value (W      : in Widget;
                                      Value  : in Xm_String);

   procedure Xm_Command_Set_Value (W      : in Widget;
                                   Value  : in Xm_String);


   procedure Xm_Command_Error (W      : in Widget;
                               Error  : in Xm_String);

   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Command                    : constant Xt_N_Resource_String;
   Xm_N_Command_Changed_Callback   : constant Xt_N_Resource_String;
   Xm_N_Command_Entered_Callback   : constant Xt_N_Resource_String;
   Xm_N_History_Items              : constant Xt_N_Resource_String;
   Xm_N_History_Item_Count         : constant Xt_N_Resource_String;
   Xm_N_History_Max_Items          : constant Xt_N_Resource_String;
   Xm_N_History_Visible_Item_Count : constant Xt_N_Resource_String;
   Xm_N_Prompt_String              : constant Xt_N_Resource_String;

private

   for Command_Child_Type use (None          => 0,
                               History_List  => 8,
                               Prompt_Label  => 11,
                               Command_Text  => 13);
   for Command_Child_Type'Size use Interfaces.C.unsigned_char'Size;


   pragma Import (C, Xm_Command_Get_Child, "XmCommandGetChild");
   pragma Import (C, Xm_Command_Append_Value, "XmCommandAppendValue");
   pragma Import (C, Xm_Command_Set_Value, "XmCommandSetValue");
   pragma Import (C, Xm_Command_Error, "XmCommandError");


   c_const_Xm_Command_Widget_Class             : Widget_Class;

   pragma Import (C, c_const_Xm_Command_Widget_Class, "xmCommandWidgetClass");

   Xm_Command_Widget_Class             : constant Widget_Class :=
    c_const_Xm_Command_Widget_Class;


   -- -------------------------------------------------------------------------
   --
   -- resource values
   --

   Xm_N_Command                : constant Xt_N_Resource_String
      := To_Resource_String ("command");
   Xm_N_Command_Changed_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("commandChangedCallback");
   Xm_N_Command_Entered_Callback : constant Xt_N_Resource_String
      := To_Resource_String ("commandEnteredCallback");
   Xm_N_History_Items          : constant Xt_N_Resource_String
      := To_Resource_String ("historyItems");
   Xm_N_History_Item_Count     : constant Xt_N_Resource_String
      := To_Resource_String ("historyItemCount");
   Xm_N_History_Max_Items      : constant Xt_N_Resource_String
      := To_Resource_String ("historyMaxItems");
   Xm_N_History_Visible_Item_Count : constant Xt_N_Resource_String
      := To_Resource_String ("historyVisibleItemCount");
   Xm_N_Prompt_String          : constant Xt_N_Resource_String
      := To_Resource_String ("promptString");

end Xm_Widgets.Manager.Bulletin_Board.Selection_Box.Command;
