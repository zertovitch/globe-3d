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

with Ada.Unchecked_Conversion;
package body Xm_Widgets.Primitive is
 

   function Xm_Is_Primitive (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Primitive_Widget_Class);
   end Xm_Is_Primitive;


   function Xm_Is_Gadget (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Gadget_Class);
   end Xm_Is_Gadget;


   function To_Integer is
      new Ada.Unchecked_Conversion (Navigation_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Navigation_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Navigation_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Integer is
      new Ada.Unchecked_Conversion (Multi_Click_Type, Interfaces.C.unsigned_char);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Multi_Click_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Integer (To_Integer (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Multi_Click_Type) is
   begin
      Append_Set (List => List,
                  Name => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


end Xm_Widgets.Primitive;