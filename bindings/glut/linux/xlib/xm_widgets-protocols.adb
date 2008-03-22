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
--          17 Nov 2001 Vadim Godunko: add default value Null_Xt_Pointer to
--                                     Closure in
--                                     Xm_Add_WM_Protocol_Callback,
--                                     Xm_Remove_WM_Protocol_Callback
--
-------------------------------------------------------------------------------

with X_Lib.Property;
package body Xm_Widgets.Protocols is

   function Xm_WM_Protocol_Atom (Shell : in Widget) return Atom is
   begin
      return X_Lib.Property.X_Intern_Atom (Xt_Display (Shell), "WM_PROTOCOLS", False);
   end Xm_WM_Protocol_Atom;
   pragma Inline (Xm_WM_Protocol_Atom);


   procedure Xm_Add_Protocols (Shell     : in Widget;
                               Property  : in Atom;
                               Protocols : in Atom_Array) is
      procedure XmAddProtocols (Shell     : in Widget;
                                Property  : in Atom;
                                Protocols : in System.Address;
                                Num_Protocol : in Cardinal);
      pragma Import (C, XmAddProtocols, "XmAddProtocols");
   begin
      XmAddProtocols (Shell, Property,
                      Protocols (Protocols'First)'Address,
                      Protocols'Length);
   end Xm_Add_Protocols;
   pragma Inline (Xm_Add_Protocols);

   
   procedure Xm_Add_WM_Protocols (Shell     : in Widget;
                                  Protocols : in Atom_Array) is
   begin
      Xm_Add_Protocols (Shell, XM_WM_PROTOCOL_ATOM (Shell), Protocols);
   end Xm_Add_WM_Protocols;
   pragma Inline (Xm_Add_WM_Protocols);


   procedure Xm_Remove_Protocols (Shell     : in Widget;
                                  Property  : in Atom;
                                  Protocols : in Atom_Array) is
      procedure XmRemoveProtocols (Shell     : in Widget;
                                   Property  : in Atom;
                                   Protocols : in System.Address;
                                   Num_Protocol : in Cardinal);
      pragma Import (C, XmRemoveProtocols, "XmRemoveProtocols");
   begin
      XmRemoveProtocols (Shell, Property,
                         Protocols (Protocols'First)'Address,
                      Protocols'Length);
   end Xm_Remove_Protocols;
   pragma Inline (Xm_Remove_Protocols);


   procedure Xm_Remove_WM_Protocols (Shell     : in Widget;
                                     Protocols : in Atom_Array) is
   begin
      Xm_Remove_Protocols (Shell, XM_WM_PROTOCOL_ATOM (Shell), Protocols);
   end Xm_Remove_WM_Protocols;
   pragma Inline (Xm_Remove_WM_Protocols);


   procedure Xm_Add_WM_Protocol_Callback (Shell      : in Widget;
                                          Protocol   : in Atom;
                                          Callback   : in Xt_Callback_Proc;
                                          Closure    : in Xt_Pointer := Null_Xt_Pointer) is
   begin
      Xm_Add_Protocol_Callback (Shell, XM_WM_PROTOCOL_ATOM (Shell),
                                Protocol, Callback, Closure);
   end Xm_Add_WM_Protocol_Callback;
   pragma Inline (Xm_Add_WM_Protocol_Callback);

   
   procedure Xm_Remove_WM_Protocol_Callback (Shell      : in Widget;
                                             Protocol   : in Atom;
                                             Callback   : in Xt_Callback_Proc;
                                             Closure    : in Xt_Pointer := Null_Xt_Pointer) is
   begin
      Xm_Remove_Protocol_Callback (Shell, XM_WM_PROTOCOL_ATOM (Shell),
                                   Protocol, Callback, Closure);
   end Xm_Remove_WM_Protocol_Callback;
   pragma Inline (Xm_Remove_WM_Protocol_Callback);

   

   procedure Xm_Activate_WM_Protocol (Shell      : in Widget;
                                      Protocol   : in Atom) is
   begin
      Xm_Activate_Protocol (Shell, XM_WM_PROTOCOL_ATOM (Shell), Protocol);
   end Xm_Activate_WM_Protocol;
   pragma Inline (Xm_Activate_WM_Protocol);

   
   procedure Xm_Deactivate_WM_Protocol (Shell      : in Widget;
                                        Protocol   : in Atom) is
   begin
      Xm_Deactivate_Protocol (Shell, XM_WM_PROTOCOL_ATOM (Shell), Protocol);
   end Xm_Deactivate_WM_Protocol;
   pragma Inline (Xm_Deactivate_WM_Protocol);

   
   procedure Xm_Set_WM_Protocol_Hooks (Shell        : in Widget;
                                       Protocol     : in Atom;
                                       Pre_Hook     : in Xt_Callback_Proc;
                                       Pre_Closure  : in Xt_Pointer;
                                       Post_Hook    : in Xt_Callback_Proc;
                                       Post_Closure : in Xt_Pointer) is
   begin
      Xm_Set_Protocol_Hooks (Shell, XM_WM_PROTOCOL_ATOM (Shell), Protocol,
                             Pre_Hook, Pre_Closure, Post_Hook, Post_Closure);
   end Xm_Set_WM_Protocol_Hooks;
   pragma Inline (Xm_Set_WM_Protocol_Hooks);


end Xm_Widgets.Protocols;
