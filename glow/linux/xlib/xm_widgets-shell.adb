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
--          July 07, 1998 HFVogt: changed type names as in specification
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Shell is


   function Xm_Is_Dialog_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Dialog_Shell_Widget_Class);
   end Xm_Is_Dialog_Shell;
   pragma Inline (Xm_Is_Dialog_Shell);


   function Xm_Is_Drag_Over_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Drag_Over_Shell_Widget_Class);
   end Xm_Is_Drag_Over_Shell;
   pragma Inline (Xm_Is_Drag_Over_Shell);


   function Xm_Is_Menu_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Menu_Shell_Widget_Class);
   end Xm_Is_Menu_Shell;
   pragma Inline (Xm_Is_Menu_Shell);


-- UseMotif2.0 Motif2.1
   function Xm_Is_Grab_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Grab_Shell_Widget_Class);
   end Xm_Is_Grab_Shell;
   pragma Inline (Xm_Is_Grab_Shell);
-- EndMotif2.0 Motif2.1


   function Xm_Is_Vendor_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Vendor_Shell_Widget_Class);
   end Xm_Is_Vendor_Shell;
   pragma Inline (Xm_Is_Vendor_Shell);


   function Xm_Is_Motif_WM_Running (Shell : in Widget) return Boolean is
      function XmIsMotifWMRunning (Shell : in Widget) return Xt_Boolean;
      pragma Import (C, XmIsMotifWMRunning, "XmIsMotifWMRunning");
   begin
      return XmIsMotifWMRunning (Shell) = Xt_Boolean'(True);
   end Xm_Is_Motif_WM_Running;
   pragma Inline (Xm_Is_Motif_WM_Running);


   -- -------------------------------------------------------------------------
   --
   --  create Dialog Shell
   --
   function Xm_Create_Dialog_Shell
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateDialogShell
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateDialogShell, "XmCreateDialogShell");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateDialogShell (Parent,
                                  Name_String'Address,
                                  X_Toolkit.Internal.Hook (Arglist),
                                  Cardinal (Length (Arglist)));
   end Xm_Create_Dialog_Shell;
   pragma Inline (Xm_Create_Dialog_Shell);


-- UseMotif2.0 Motif2.1
  -- -------------------------------------------------------------------------
  --
  --  create Grab Shell
  --
  function Xm_Create_Grab_Shell
    (Parent   : in  Widget; 
     Name     : in  String;
     Arglist  : in  Arg_List := Null_Arg_List)
     return Widget is
     function XmCreateGrabShell
       (Parent : in Widget;
        Name   : in System.Address;
        Args   : in X_Toolkit.Internal.Arg_Rec_Access;
        Count  : in Cardinal)
        return Widget;
     pragma Import (C, XmCreateGrabShell, "XmCreateGrabShell");
     Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
  begin
     return XmCreateGrabShell (Parent,
                               Name_String'Address,
                               X_Toolkit.Internal.Hook (Arglist),
                               Cardinal (Length (Arglist)));
  end Xm_Create_Grab_Shell;
  pragma Inline (Xm_Create_Grab_Shell);
-- EndMotif2.0 Motif2.1


   -- -------------------------------------------------------------------------
   --
   --  create Menu Shell
   --
   function Xm_Create_Menu_Shell
     (Parent   : in  Widget; 
      Name     : in  String;
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is
      function XmCreateMenuShell
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateMenuShell, "XmCreateMenuShell");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateMenuShell (Parent,
                                Name_String'Address,
                                X_Toolkit.Internal.Hook (Arglist),
                                Cardinal (Length (Arglist)));
   end Xm_Create_Menu_Shell;
   pragma Inline (Xm_Create_Menu_Shell);


   function To_Integer is
      new Ada.Unchecked_Conversion (X_Lib.Property.Window_State, Integer);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Initial_Window_State) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Initial_Window_State) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Audible_Warning_Kind, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Audible_Warning_Kind) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Audible_Warning_Kind) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Delete_Response, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Delete_Response) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Delete_Response) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Keyboard_Focus_Policy, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Keyboard_Focus_Policy) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Keyboard_Focus_Policy) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- UseMotif2.0 Motif2.1
   function To_Integer is
      new Ada.Unchecked_Conversion (Input_Policy, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Input_Policy) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Input_Policy) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1


end Xm_Widgets.Shell;
