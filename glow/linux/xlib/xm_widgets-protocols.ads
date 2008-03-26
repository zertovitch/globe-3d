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
--                                     Xm_Add_Protocol_Callback,
--                                     Xm_Add_WM_Protocol_Callback,
--                                     Xm_Remove_Protocol_Callback,
--                                     Xm_Remove_WM_Protocol_Callback
--
-------------------------------------------------------------------------------

with X_Lib;
use  X_Lib;
package Xm_Widgets.Protocols is

   function Xm_WM_Protocol_Atom (Shell : in Widget) return Atom;


   procedure Xm_Add_Protocols (Shell     : in Widget;
                               Property  : in Atom;
                               Protocols : in Atom_Array);


   procedure Xm_Add_WM_Protocols (Shell     : in Widget;
                                  Protocols : in Atom_Array);

   
   procedure Xm_Remove_Protocols (Shell     : in Widget;
                                  Property  : in Atom;
                                  Protocols : in Atom_Array);

   
   procedure Xm_Remove_WM_Protocols (Shell     : in Widget;
                                     Protocols : in Atom_Array);

   
   procedure Xm_Add_Protocol_Callback (Shell      : in Widget;
                                       Property   : in Atom;
                                       Proto_Atom : in Atom;
                                       Callback   : in Xt_Callback_Proc;
                                       Closure    : in Xt_Pointer := Null_Xt_Pointer);

   
   procedure Xm_Add_WM_Protocol_Callback (Shell      : in Widget;
                                          Protocol   : in Atom;
                                          Callback   : in Xt_Callback_Proc;
                                          Closure    : in Xt_Pointer := Null_Xt_Pointer);

   
   procedure Xm_Remove_Protocol_Callback (Shell      : in Widget;
                                          Property   : in Atom;
                                          Proto_Atom : in Atom;
                                          Callback   : in Xt_Callback_Proc;
                                          Closure    : in Xt_Pointer := Null_Xt_Pointer);

   
   procedure Xm_Remove_WM_Protocol_Callback (Shell      : in Widget;
                                             Protocol   : in Atom;
                                             Callback   : in Xt_Callback_Proc;
                                             Closure    : in Xt_Pointer := Null_Xt_Pointer);

   
   procedure Xm_Activate_Protocol (Shell      : in Widget;
                                   Property   : in Atom;
                                   Proto_Atom : in Atom);

   
   procedure Xm_Activate_WM_Protocol (Shell      : in Widget;
                                      Protocol   : in Atom);

   
   procedure Xm_Deactivate_Protocol (Shell      : in Widget;
                                     Property   : in Atom;
                                     Proto_Atom : in Atom);

   
   procedure Xm_Deactivate_WM_Protocol (Shell      : in Widget;
                                        Protocol   : in Atom);

   
   procedure Xm_Set_Protocol_Hooks (Shell        : in Widget;
                                    Property     : in Atom;
                                    Proto_Atom   : in Atom;
                                    Pre_Hook     : in Xt_Callback_Proc;
                                    Pre_Closure  : in Xt_Pointer;
                                    Post_Hook    : in Xt_Callback_Proc;
                                    Post_Closure : in Xt_Pointer);


   procedure Xm_Set_WM_Protocol_Hooks (Shell        : in Widget;
                                       Protocol     : in Atom;
                                       Pre_Hook     : in Xt_Callback_Proc;
                                       Pre_Closure  : in Xt_Pointer;
                                       Post_Hook    : in Xt_Callback_Proc;
                                       Post_Closure : in Xt_Pointer);


private

   pragma Import (C, Xm_Add_Protocol_Callback, "XmAddProtocolCallback");
   pragma Import (C, Xm_Remove_Protocol_Callback, "XmRemoveProtocolCallback");
   pragma Import (C, Xm_Activate_Protocol, "XmActivateProtocol");
   pragma Import (C, Xm_Deactivate_Protocol, "XmDeactivateProtocol");
   pragma Import (C, Xm_Set_Protocol_Hooks, "XmSetProtocolHooks");

end Xm_Widgets.Protocols;
