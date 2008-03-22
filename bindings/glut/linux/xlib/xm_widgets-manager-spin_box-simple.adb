-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2002 Hans-Frieder Vogt                                --
--  This file also copyright (c) 2001 Vadim Godunko                          --
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
--          19 May 2001 Vadim Godunko: first definition of this file
--          12 Jan 2002 H.-F. Vogt: make Spin_Box.Simple only available in
--                                  Motif 2.1 and later
--
-------------------------------------------------------------------------------

with Interfaces.C;
with X_Toolkit.Internal;

package body Xm_Widgets.Manager.Spin_Box.Simple is

-- UseMotif2.1

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Simple_Spin_Box_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Simple_Spin_Box_Callback_Struct,
            Xm_Simple_Spin_Box_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Ok,
                                        2 => Cr_Spin_Next,
                                        3 => Cr_Spin_Prior));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   function Xm_Is_Simple_Spin_Box (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Simple_Spin_Box_Widget_Class);
   end Xm_Is_Simple_Spin_Box;


   function Xm_Create_Simple_Spin_Box
     (Parent   : in  Widget;
      Name     : in  Standard.String; -- nessesary because of definition of Spin_Button_Child_Type
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSimpleSpinBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSimpleSpinBox, "XmCreateSimpleSpinBox");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSimpleSpinBox (Parent,
                                    Name_String'Address,
                                    X_Toolkit.Internal.Hook (Arglist),
                                    Cardinal (Length (Arglist)));
   end Xm_Create_Simple_Spin_Box;

-- EndMotif2.1

end Xm_Widgets.Manager.Spin_Box.Simple;
