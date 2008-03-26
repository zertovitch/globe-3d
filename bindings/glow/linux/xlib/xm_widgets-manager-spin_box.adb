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
--          June 20, 1998 begin of history
--          19 May 2001 Vadim Godunko: include Cr_Ok in the
--                                     Callback_Reason_Array
--          26 Aug 2001 H.-F. Vogt: implementation of
--                                  Xm_Spin_Box_Validate_Position
--          29 Aug 2001 H.-F. Vogt: correct Xm_N_Spin_Box_Child_Type and
--                                  add Xm_N_Position_Type according
--                                  to a hint from Vadim Godunko
--          17 Nov 2001 Vadim Godunko: add Arrow_Orientation resource
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C,
     X_Toolkit.Internal;
package body Xm_Widgets.Manager.Spin_Box is

-- UseMotif2.0 Motif2.1


   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Spin_Box_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Spin_Box_Callback_Struct,
            Xm_Spin_Box_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Ok,
	                                2 => Cr_Spin_Next,
	                                3 => Cr_Spin_Prior,
					4 => Cr_Spin_First,
					5 => Cr_Spin_Last));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);



   function Xm_Is_Spin_Box (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Spin_Box_Widget_Class);
   end Xm_Is_Spin_Box;


   function Xm_Create_Spin_Box
     (Parent   : in  Widget;
      Name     : in  Standard.String; -- nessesary because of definition of Spin_Button_Child_Type
      Arglist  : in  Arg_List := Null_Arg_List)
      return Widget is

      function XmCreateSpinBox
        (Parent : in Widget;
         Name   : in System.Address;
         Args   : in X_Toolkit.Internal.Arg_Rec_Access;
         Count  : in Cardinal)
         return Widget;
      pragma Import (C, XmCreateSpinBox, "XmCreateSpinBox");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XmCreateSpinBox (Parent,
                              Name_String'Address,
                              X_Toolkit.Internal.Hook (Arglist),
                              Cardinal (Length (Arglist)));
   end Xm_Create_Spin_Box;


   procedure Xm_Spin_Box_Validate_Position
     (Text_Field    : in     Widget;
      Position      :    out Integer;
      Position_Type :    out Validate_Position_Type) is
      function XmSpinBoxValidatePosition
        (Text_Field    : in Widget;
	 Position      : in System.Address)
	 return Validate_Position_Type;
      pragma Import (C, XmSpinBoxValidatePosition, "XmSpinBoxValidatePosition");
   begin
      Position_Type := XmSpinBoxValidatePosition (Text_Field, Position'Address);
   end Xm_Spin_Box_Validate_Position;


   function To_Integer is
      new Ada.Unchecked_Conversion (Arrow_Layout_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Arrow_Layout_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Arrow_Layout_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- UseMotif2.1
   function To_Integer is
      new Ada.Unchecked_Conversion (Arrow_Orientation, Interfaces.C.signed_char);


   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Arrow_Orientation) is
   begin
      Append_Set (List => List,
		  Name => Name,
		  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Arrow_Orientation) is
   begin
      Append_Set (List => List,
		  Name => Name,
		  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);
-- EndMotif2.1


   function To_Integer is
      new Ada.Unchecked_Conversion (Arrow_Sensitivity_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Arrow_Sensitivity_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Arrow_Sensitivity_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Position_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Position_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Position_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Child_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Child_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Child_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


-- EndMotif2.0 Motif2.1

end Xm_Widgets.Manager.Spin_Box;
