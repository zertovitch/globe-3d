-------------------------------------------------------------------------------
--                                                                           --
--  Ada Interface to the X Window System and Motif(tm)/Lesstif               --
--  Copyright (c) 1996-2001 Hans-Frieder Vogt                                --
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
--          20 Jun 2001 Vadim Godunko: first definition of this file
--          05 Aug 2001 H.-F. Vogt: add print shell widget
--
-------------------------------------------------------------------------------

with Interfaces.C;
with X_Toolkit.Internal;

package body Xm_Widgets.Print is

-- UseMotif2.1

   function To_Callback_Struct (Pointer : in Xt_Pointer)
      return Xm_Print_Shell_Callback_Struct_Access is
      function To_CS is
         new To_Generic_Callback_Struct_Access (Xm_Print_Shell_Callback_Struct,
	    Xm_Print_Shell_Callback_Struct_Access,
            Callback_Reason_Array_Type'(1 => Cr_Start_Job,
	                                2 => Cr_End_Job,
					3 => Cr_Page_Setup,
					4 => Cr_PDM_None,
					5 => Cr_PDM_Up,
					6 => Cr_PDM_Start_Error,
					7 => Cr_PDM_Start_VxAuth,
					8 => Cr_PDM_Start_PxAuth,
					9 => Cr_PDM_OK,
					10 => Cr_PDM_Cancel,
					11 => Cr_PDM_Exit_Error));
   begin
      return To_CS (Pointer);
   end To_Callback_Struct;
   pragma Inline (To_Callback_Struct);


   --  Xm_Is_Print_Shell
   --  test if widget belongs to Xm_Print_Shell_Widget of a subclass thereof
   --
   function Xm_Is_Print_Shell (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Xm_Print_Shell_Widget_Class);
   end Xm_Is_Print_Shell;


   --------------------
   -- Xm_Print_Setup --
   --------------------

   function Xm_Print_Setup
     (Video_Widget     : in Widget;
      Print_Screen     : in X_Lib.Screen_Pointer;
      Print_Shell_Name : in String;
      Arglist          : in Arg_List := Null_Arg_List)
      return Widget
   is
      function XmPrintSetup
        (Video_Widget     : in Widget;
         Print_Screen     : in X_Lib.Screen_Pointer;
         Print_Shell_Name : in System.Address;
         Args             : in X_Toolkit.Internal.Arg_Rec_Access;
         Count            : in Cardinal)
         return Widget;
      pragma Import (C, XmPrintSetup, "XmPrintSetup");

      Name_String : constant Interfaces.C.char_array :=
         Interfaces.C.To_C (Print_Shell_Name, True);
   begin
      return XmPrintSetup (Video_Widget,
                           Print_Screen,
                           Name_String'Address,
                           X_Toolkit.Internal.Hook (Arglist),
                           Cardinal (Length (Arglist)));
   end Xm_Print_Setup;

   ----------------------
   -- Xm_Print_To_File --
   ----------------------

   procedure Xm_Print_To_File
     (Display     : in X_Lib.Display_Pointer;
      File_Name   : in String;
      Finish_Proc : in X_Lib.Extensions.Print.Xp_Finish_Proc;
      Client_Data : in Xt_Pointer := Null_Xt_Pointer)
   is
      function XmPrintToFile
        (Display     : in X_Lib.Display_Pointer;
         File_Name   : in System.Address;
         Finish_Proc : in X_Lib.Extensions.Print.Xp_Finish_Proc;
         Client_Data : in Xt_Pointer)
         return Xt_Boolean;
      pragma Import (C, XmPrintToFile, "XmPrintToFile");

      Name_String : constant Interfaces.C.char_array :=
         Interfaces.C.To_C (File_Name, True);
   begin
      if not To_Boolean (XmPrintToFile (Display,
                                        Name_String'Address,
                                        Finish_Proc,
                                        Client_Data))
      then
         raise File_Error;
      end if;
   end Xm_Print_To_File;

-- EndMotif2.1

end Xm_Widgets.Print;
