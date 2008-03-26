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
--          November 8, 1998 HFVogt: added X11R6.3 conditional defines
--          June 14, 2000: added Append_Set/Append_Get for Atoms
--                         (thanks to Vadim Godunko)
--          21 Jun 2001 Vadim Godunko: add Xt_Initialize_Widget_Class,
--                                     Xt_Display_Initialize 
--                                     add default arguments for
--                                     Xt_App_Add_Time_Out and
--                                     Xt_App_Add_Work_Proc
--                                     correct return value of Xt_Work_Proc
--                                     (was: Boolean, now: Xt_Boolean)
--                                     add Xt_Set_Language_Proc as a procedure
--                                     change conversion routines To_Xt_Pointer
--                                     add type Xt_Order_Proc with
--                                     Append_Set/Append_Get routines
--          26 Aug 2001 H.-F. Vogt: implementation of
--                                  Xt_App_Set_Fallback_Resources
--                                  correct error when using Get_Argument_Count
--                                  with an Arg_Vector in Xt_App_Initialize,
--                                  Xt_Open_Application, Xt_Open_Display
--                                  free Resource_Strings after usage in
--                                  Xt_Open_Application
--
-------------------------------------------------------------------------------

with System,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation,
     Ada.Finalization,
     Interfaces.C.Strings,
     Interfaces.C.Pointers,
     String_List,
     X_Command_Line.Internal,
     X_Lib.Resource.Internal,
     X_Toolkit.Internal;
use  Ada.Finalization,
     Interfaces.C,
     Interfaces.C.Strings,
     String_List,
     X_Lib.Resource.Internal;
package body X_Toolkit is


-- ----------------------------------------------------------------------------
--
-- locally needed type conversion routines
--

   function To_Address is
      new Ada.Unchecked_Conversion (Integer, System.Address);
   function To_Integer is
      new Ada.Unchecked_Conversion (System.Address, Integer);
   function To_Integer is
      new Ada.Unchecked_Conversion (Xt_Boolean, Interfaces.C.signed_char);
   function To_Long is
      new Ada.Unchecked_Conversion (Float, Interfaces.C.long);
   function To_Long is
      new Ada.Unchecked_Conversion (Widget, Interfaces.C.long);
   function To_Long is
      new Ada.Unchecked_Conversion (System.Address, Interfaces.C.long);
   function To_Long is
      new Ada.Unchecked_Conversion (Callback_Lists.Element_Access, Interfaces.C.long);
   function Unsigned_Long_To_Long is
      new Ada.Unchecked_Conversion (Interfaces.C.unsigned_long, Interfaces.C.long);
   function To_Long is
      new Ada.Unchecked_Conversion (X_Lib.Pixel, Interfaces.C.long);
   function To_Long is
      new Ada.Unchecked_Conversion (X_Lib.XID, Interfaces.C.long);



   -- convertion routines from bitfields to integers and vice versa
   --
   type C_Modifiers is new Interfaces.C.unsigned;
   type Modifiers_Int is mod 2**8; --Modifiers'Size;
   function To_Mask_Int is
      new Ada.Unchecked_Conversion (X_Lib.Modifiers_Mask_Type,
                                    Modifiers_Int);
   function To_Mask_Type is
      new Ada.Unchecked_Conversion (Modifiers_Int,
                                    X_Lib.Modifiers_Mask_Type);
   
   function To_Long (Mask : in X_Lib.Modifiers_Mask_Type) return C_Modifiers is
   begin
      return C_Modifiers (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Modifiers) return X_Lib.Modifiers_Mask_Type is
   begin
      return To_Mask_Type (Modifiers_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   type C_Xt_Input_Mask is new Interfaces.C.unsigned_long;
   type Xt_Input_Mask_Int is mod 2**4; -- Xt_Input_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (Xt_Input_Mask,
                                                         Xt_Input_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (Xt_Input_Mask_Int,
                                                          Xt_Input_Mask);
   
   function To_Long (Mask : in Xt_Input_Mask) return C_Xt_Input_Mask is
   begin
      return C_Xt_Input_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Xt_Input_Mask) return Xt_Input_Mask is
   begin
      return To_Mask_Type (Xt_Input_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);



   function "=" (Left, Right : in Xt_Input_Mask) return Boolean is
   begin
      return (Left.Xevent = Right.Xevent) and then
             (Left.Timer  = Right.Timer)  and then
             (Left.Alternate_Input = Right.Alternate_Input) and then
             (Left.Signal = Right.Signal);
   end "=";


   function "or" (Left, Right : in Xt_Input_Mask) return Xt_Input_Mask is
      Return_Value : Xt_Input_Mask;
   begin
      Return_Value.Xevent := Left.Xevent or Right.Xevent;
      Return_Value.Timer  := Left.Timer  or Right.Timer;
      Return_Value.Alternate_Input := Left.Alternate_Input or Right.Alternate_Input;
      Return_Value.Signal := Left.Signal or Right.Signal;
      return Return_Value;
   end "or";


   function "and" (Left, Right : in Xt_Input_Mask) return Xt_Input_Mask is
      Return_Value : Xt_Input_Mask;
   begin
      Return_Value.Xevent := Left.Xevent and Right.Xevent;
      Return_Value.Timer  := Left.Timer  and Right.Timer;
      Return_Value.Alternate_Input := Left.Alternate_Input and Right.Alternate_Input;
      Return_Value.Signal := Left.Signal and Right.Signal;
      return Return_Value;
   end "and";


   function To_Xt_Boolean (Val : in Boolean) return Xt_Boolean is
   begin
      if Val then
         return Xt_Boolean'(True);
      else
         return Xt_Boolean'(False);
      end if;
   end To_Xt_Boolean;


   function To_Boolean (Val : in Xt_Boolean) return Boolean is
   begin
      if Val = Xt_Boolean'(True) then
         return Boolean'(True);
      else
         return Boolean'(False);
      end if;
   end To_Boolean;





-- ----------------------------------------------------------------------------
--
-- ACTIONS
--
--

   procedure Xt_App_Add_Actions
     (App_Context : in Xt_App_Context;
      Action_List : in Xt_Action_List) is
      procedure XtAppAddActions
        (App_Context : in Xt_App_Context;
         Action      : in System.Address;
         Num_Actions : in Cardinal);
      pragma Import (C, XtAppAddActions, "XtAppAddActions");
   begin
      XtAppAddActions (App_Context, Action_List'Address, Cardinal (Action_List'Length));
   end Xt_App_Add_Actions;
   pragma Inline (Xt_App_Add_Actions);


   procedure Xt_Call_Action_Proc
     (W           : in Widget;
      Action_Name : in String;
      Event       : in X_Lib.X_Event_Pointer;
      Params      : in String_List.Element_Access_List := String_List.Null_Element_Access_List) is
      procedure XtCallActionProc
        (W           : in Widget;
         Action_Name : in System.Address;
         Event       : in X_Lib.X_Event_Pointer;
         Params      : in String_List_Conversion.Chars_Ptr_List_Type;
         Num_Params  : in String_List_Conversion.Index_Type);
      pragma Import (C, XtCallActionProc, "XtCallActionProc");
      Action_Name_String : constant Interfaces.C.Char_Array
                         := Interfaces.C.To_C (Action_Name, Append_Nul => True);
      Params_Arry        : String_List_Conversion.Chars_Ptr_List_Type
                         := String_List_Conversion.To_Chars_Ptr_List (Params);
   begin
      XtCallActionProc (W, Action_Name_String'Address, Event,
                        Params_Arry,
			String_List_Conversion.Index_Type (Length (Params)));
      String_List_Conversion.Free (Params_Arry, String_List_Conversion.Index_Type (Length (Params)));
   end Xt_Call_Action_Proc;




   --
   -- everything to do with widget lists
   --
   function Length (List : in  Widget_List) return Natural is
   begin
      return Widget_Lists.Length (Widget_Lists.Unbounded_List (List));
   end Length;
   pragma Inline (Length);


   function Element
     (List   : in Widget_List;
      Index  : in Natural)
      return Widget is
   begin
      return Widget_Lists.Element (Widget_Lists.Unbounded_List (List), Index);
   end Element;
   pragma Inline (Element);


   function "=" (Left, Right : in Widget_List) return Boolean is
   begin
      return Widget_Lists."=" (Widget_Lists.Unbounded_List (Left),
                               Widget_Lists.Unbounded_List (Right));
   end "=";
   pragma Inline ("=");


   function "&" (Left, Right : in Widget_List) return Widget_List is
   begin
      return Widget_List (Widget_Lists."&" (Widget_Lists.Unbounded_List (Left),
                                            Widget_Lists.Unbounded_List (Right)));
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in Widget_List;
                 Right : in Widget) return Widget_List is
   begin
      return Widget_List (Widget_Lists."&" (Widget_Lists.Unbounded_List (Left),
                                            Right));
   end "&";
   pragma Inline ("&");


   procedure Append (List : in out Widget_List;
                     W    : in     Widget_List) is
   begin
      Widget_Lists.Append (Widget_Lists.Unbounded_List (List),
                           Widget_Lists.Unbounded_List (W));
   end Append;
   pragma Inline (Append);


   procedure Append (List : in out Widget_List;
                     W    : in     Widget) is
   begin
      Widget_Lists.Append (Widget_Lists.Unbounded_List (List),
                           W);
   end Append;
   pragma Inline (Append);


-- since  use the package generic_list_types, i can't access the elements
-- directly
--   function To_Address (List : in Widget_List) return System.Address is
--   begin
--      return List.List.all (List.List.all'First)'Address;
--   end To_Address;



   --
   -- everything to do with callback lists
   --

   --
   -- Length
   --
   function Length (List : in Callback_List) return Natural is
   begin
      if List = Null_Callback_List then
         return 0;
      else
         return Callback_Lists.Length (Callback_Lists.Unbounded_List (List))-1;
      end if;
   end Length;


   --
   -- Append
   --
   procedure Append (List : in out Callback_List;
                     Rec  : in     Callback_Rec) is
   begin
      if List = Null_Callback_List then
         Callback_Lists.Append (Callback_Lists.Unbounded_List (List),
                                Rec);
      else
         Callback_Lists.Replace_Element (Callback_Lists.Unbounded_List (List),
                                         Callback_Lists.Length (Callback_Lists.Unbounded_List (List)),
                                         Rec);
      end if;
      Callback_Lists.Append (Callback_Lists.Unbounded_List (List),
                             Null_Callback_Rec);
   end Append;


   procedure Append (List      : in out Callback_List;
                     Callback  : in     Xt_Callback_Proc;
		     Closure   : in     Xt_Pointer) is
      Rec : Callback_Rec := (Callback, Closure);
   begin
      Append (List, Rec);
   end Append;


       
   -- -------------------------------------------------------------------------
   --
   --  XtAddCallback(s)
   --

   procedure XtAddCallback (W    : in Widget;
                            Kind : in Xt_N_Resource_String;
                            Proc : in Xt_Callback_Proc;
                            Data : in Xt_Pointer);
   pragma Import (C, XtAddCallback, "XtAddCallback");

   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Xt_Pointer:= Null_Xt_Pointer) is
   begin
      XtAddCallback (W    => To,
                     Kind => Kind, 
                     Proc => Callback,
                     Data => Client_Data);
   end Xt_Add_Callback;
   pragma Inline (Xt_Add_Callback);


   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Widget) is
   begin
      XtAddCallback (W    => To,
                     Kind => Kind, 
                     Proc => Callback,
                     Data => To_Xt_Pointer (Client_Data));
   end Xt_Add_Callback;
   pragma Inline (Xt_Add_Callback);



   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Integer) is
   begin
      XtAddCallback (W    => To,
                     Kind => Kind, 
                     Proc => Callback,
                     Data => To_Xt_Pointer (To_Address (Client_Data)));
   end Xt_Add_Callback;
   pragma Inline (Xt_Add_Callback);


   procedure Xt_Add_Callbacks (To           : in Widget;
                               Kind         : in Xt_N_Resource_String;
                               Callbacks    : in Callback_List) is
      procedure XtAddCallbacks (To        : in Widget;
                                Kind      : in Xt_N_Resource_String;
                                Callbacks : in Callback_Lists.Element_Access);
      pragma Import (C, XtAddCallbacks, "XtAddCallbacks");

   begin
      if Length (Callbacks) > 0 then
         XtAddCallbacks (To,
                         Kind,
                         Callback_Lists.Hook (Callback_Lists.Unbounded_List (Callbacks)));
      end if; 
   end Xt_Add_Callbacks;
   pragma Inline (Xt_Add_Callback);


   -- -------------------------------------------------------------------------
   --
   --  XtCallCallbacks
   --

   procedure Xt_Call_Callbacks (From      : in Widget;
                                Kind      : in Xt_N_Resource_String;
                                Call_Data : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtCallCallbacks (From      : in Widget;
                                 Kind      : in Xt_N_Resource_String;
                                 Call_Data : in Xt_Pointer);
      pragma Import (C, XtCallCallbacks, "XtCallCallbacks");
   begin
      XtCallCallbacks (From,
                       Kind,
                       Call_Data);
   end Xt_Call_Callbacks;
   pragma Inline (Xt_Add_Callback);


   -- -------------------------------------------------------------------------
   --
   --  XtRemoveCallback(s)
   --

   procedure Xt_Remove_Callbacks (From          : in Widget;
                                  Kind          : in Xt_N_Resource_String;
                                  Callbacks     : in Callback_List) is
      procedure XtRemoveCallbacks (From      : in Widget;
                                   Kind      : in Xt_N_Resource_String;
                                   Callbacks : in Callback_Lists.Element_Access);
      pragma Import (C, XtRemoveCallbacks, "XtRemoveCallbacks");
   begin
      if Length (Callbacks) > 0 then
         XtRemoveCallbacks (From,
                            Kind,
                            Callback_Lists.Hook (Callback_Lists.Unbounded_List (Callbacks)));
      end if; 
   end Xt_Remove_Callbacks;
   pragma Inline (Xt_Add_Callback);


   procedure XtRemoveCallback (From_Widget : in Widget;
                               Kind        : in Xt_N_Resource_String;
                               Callback    : in Xt_Callback_Proc;
                               Client_Data : in Xt_Pointer);
   pragma Import (C, XtRemoveCallback, "XtRemoveCallback");


   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Xt_Pointer := Null_Xt_Pointer) is
   begin
      XtRemoveCallback (From, Kind, Callback, Client_Data);
   end Xt_Remove_Callback;
   pragma Inline (Xt_Remove_Callback);


   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Widget) is
   begin
      XtRemoveCallback (From, Kind, Callback, To_Xt_Pointer (Client_Data));
   end Xt_Remove_Callback;
   pragma Inline (Xt_Remove_Callback);


   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Integer) is
   begin
      XtRemoveCallback (From, Kind, Callback, To_Xt_Pointer (Client_Data));
   end Xt_Remove_Callback;
   pragma Inline (Xt_Remove_Callback);


   --
   -- everything to do with arg lists
   --
   --
   -- functions with arg lists
   --

   function Length (List : in  Arg_List) return Natural is
   begin
      return Arg_Lists.Length (Arg_Lists.Unbounded_List (List));
   end Length;
   pragma Inline (Length);


   procedure Append (List  : in out Arg_List;
                     Which : in     Arg_List) is
   begin
      Arg_Lists.Append (Arg_Lists.Unbounded_List (List),
                        Arg_Lists.Unbounded_List (Which));
   end Append;
   pragma Inline (Append);


   procedure Append (List  : in out Arg_List;
                     Which : in     Arg_Rec) is
   begin
      Arg_Lists.Append (Arg_Lists.Unbounded_List (List),
                        Which);
   end Append;
   pragma Inline (Append);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Interfaces.C.long) is
   begin
      Append (List, Arg_Rec'(Name, Value));
   end Append_Set;
   pragma Inline (Append_Set);

 
   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Interfaces.C.long) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);

 
   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Integer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Integer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Float) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Float) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Boolean) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (To_Integer (To_Xt_Boolean (Value))));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value :    out Boolean) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     System.Address) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    System.Address) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Pointer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (X_Lib.To_Address (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xt_Pointer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Widget) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Widget) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Long is
      new Ada.Unchecked_Conversion (Xt_Callback_Proc, Interfaces.C.long);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Callback_Proc) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Callback_List) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Callback_Lists.Hook (Callback_Lists.Unbounded_List (Value))));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Character) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Character'Pos(Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Character) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.XID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.XID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Drawable_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Font_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Cursor_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Colormap_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.GContext_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Key_Sym_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Visual_ID) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Pixel) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Pixel) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Dimension) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Dimension) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Position) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Position) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Screen_Pointer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (X_Lib.To_Address (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Screen_Pointer) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Interfaces.C.unsigned_char) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Value));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Interfaces.C.unsigned_char) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_lib.Atom) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Interfaces.C.long (Value));
   end Append_Set;
   pragma Inline (Append_Set);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Atom) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Accelerators) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (System.Address (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xt_Accelerators) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Translations) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => To_Long (System.Address (Value)));
   end Append_Set;
   pragma Inline (Append_Set);


   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xt_Translations) is
   begin
      Append_Set (List  => List,
                  Name  => Name,
                  Value => Value'Address);
   end Append_Get;
   pragma Inline (Append_Get);


   function To_Long is
      new Ada.Unchecked_Conversion (Xt_Order_Proc, Interfaces.C.long);

   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xt_Order_Proc) is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => To_Long (Value));
   end Append_Set;


   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xt_Order_Proc) is
   begin
      Append_Set (List  => List,
		  Name  => Name,
		  Value => Value'Address);
   end Append_Get;


-- ----------------------------------------------------------------------------
--
--       T O O L K I T   I N I T I A L I Z A T I O N
--                                               

   function XtAppInitialize
     (App_Context        : in System.Address;
      Application_Class  : in Xt_C_Resource_String;
      Options            : in System.Address;
      Option_Count       : in Cardinal;
      Arg_Count          : in System.Address;
      Arg_Vector         : in String_List_Conversion.Chars_Ptr_List_Type;
      Fallback_Resources : in String_List_Conversion.Chars_Ptr_List_Type;
      Args               : in X_Toolkit.Internal.Arg_Rec_Access;
      Num_Args           : in Cardinal)
      return Widget;
   pragma Import (C, XtAppInitialize, "XtAppInitialize"); 


   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List) is
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_App_Initialize (W, App_Context,
                         Application_Class_String,
                         Options, Fallback_Resources, Args);
      Resource_Strings.Free (Application_Class_String);
   end Xt_App_Initialize;


   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List) is

      --local variables
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;

      Fallb_Address : String_List_Conversion.Chars_Ptr_List_Type;

      Argc          : String_List_Conversion.Index_Type
                    := String_List_Conversion.Index_Type (X_Command_Line.Argument_Count + 1);

   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := Null_Address;
         Opt_Num     := 0;
      end if;
      if Fallback_Resources /= String_List.Null_Element_Access_List then
         Fallb_Address := String_List_Conversion.To_Chars_Ptr_List (Fallback_Resources,
	                                     Append_Null => True);
      else
         Fallb_Address := String_List_Conversion.Null_Chars_Ptr_List;
      end if;
      W := XtAppInitialize (App_Context'Address,
                            Application_Class,
                            Opt_Address,
                            Opt_Num,
                            Argc'Address,
                            X_Command_Line.Internal.Get_Argument_Hook,
                            Fallb_Address,
                            X_Toolkit.Internal.Hook (Args),
                            Cardinal (Length (Args)));
      String_List_Conversion.Free (Fallb_Address);
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Argc);
   end Xt_App_Initialize;


   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List) is
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_App_Initialize (W, App_Context,
                         Application_Class_String,
                         Options, Arg_Vector, Fallback_Resources, Args);
      Resource_Strings.Free (Application_Class_String);
   end Xt_App_Initialize;


   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List) is

      --local variables
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;

      Fallb_Address : String_List_Conversion.Chars_Ptr_List_Type;

      Argc          : String_List_Conversion.Index_Type
                    := String_List_Conversion.Index_Type (X_Command_Line.Argument_Count (Arg_Vector) + 1);

   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := Null_Address;
         Opt_Num     := 0;
      end if;
      if Fallback_Resources /= String_List.Null_Element_Access_List then
         Fallb_Address := String_List_Conversion.To_Chars_Ptr_List (Fallback_Resources,
	                                     Append_Null => True);
      else
         Fallb_Address := String_List_Conversion.Null_Chars_Ptr_List;
      end if;
      W := XtAppInitialize (App_Context'Address,
                            Application_Class,
                            Opt_Address,
                            Opt_Num,
                            Argc'Address,
                            X_Command_Line.Internal.Get_Argument_Hook (Arg_Vector),
                            Fallb_Address,
                            X_Toolkit.Internal.Hook (Args),
                            Cardinal (Length (Args)));
      String_List_Conversion.Free (Fallb_Address);
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Arg_Vector, Argc);
   end Xt_App_Initialize;


   procedure XtDisplayInitialize
     (App_Context	: in Xt_App_Context;
      Display		: in X_Lib.Display_Pointer;
      Application_Name  : in Xt_N_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options		: in System.Address;
      Option_Count	: in Cardinal;
      Arg_Count 	: in System.Address;
      Arg_Vector	: in String_List_Conversion.Chars_Ptr_List_Type);
   pragma Import (C, XtDisplayInitialize, "XtDisplayInitialize");


   procedure Xt_Display_Initialize
     (App_Context	: in Xt_App_Context;
      Display		: in X_Lib.Display_Pointer;
      Application_Name  : in String := "";
      Application_Class : in String;
      Options		: in Option_Description_List := Null_Option_Description_List)
   is
      Application_Name_String  : Xt_N_Resource_String := To_Resource_String (Application_Name);
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Display_Initialize (App_Context,
			     Display,
			     Application_Name_String,
			     Application_Class_String,
			     Options);
      Resource_Strings.Free (Application_Class_String);
      Resource_Strings.Free (Application_Name_String);
   end Xt_Display_Initialize;


   procedure Xt_Display_Initialize
     (App_Context	: in     Xt_App_Context;
      Display		: in     X_Lib.Display_Pointer;
      Application_Name  : in     String := "";
      Application_Class : in     String;
      Options		: in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector	: in out X_Command_Line.Argument_Vector_Type)
   is
      Application_Name_String  : Xt_N_Resource_String := To_Resource_String (Application_Name);
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Display_Initialize (App_Context,
			     Display,
			     Application_Name_String,
			     Application_Class_String,
			     Options,
			     Arg_Vector);
      Resource_Strings.Free (Application_Class_String);
      Resource_Strings.Free (Application_Name_String);
   end Xt_Display_Initialize;


   procedure Xt_Display_Initialize
     (App_Context	: in Xt_App_Context;
      Display		: in X_Lib.Display_Pointer;
      Application_Name  : in Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options		: in Option_Description_List := Null_Option_Description_List)
   is
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num	  : Cardinal;
      Argc	  : String_List_Conversion.Index_Type :=
		    String_List_Conversion.Index_Type (X_Command_Line.Argument_Count + 1);
   begin
      if Length (Options) > 0 then
	 Opt_Address := Opt_List (Opt_List.all'First)'Address;
	 Opt_Num     := Cardinal (Length (Options));
      else
	 Opt_Address := X_Toolkit.Null_Address;
	 Opt_Num     := 0;
      end if;
      XtDisplayInitialize (App_Context,
			   Display,
			   Application_Name,
			   Application_Class,
			   Opt_Address,
			   Opt_Num,
			   Argc'Address,
			   X_Command_Line.Internal.Get_Argument_Hook);
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Argc);
   end Xt_Display_Initialize;


   procedure Xt_Display_Initialize
     (App_Context	: in     Xt_App_Context;
      Display		: in     X_Lib.Display_Pointer;
      Application_Name  : in     Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in     Xt_C_Resource_String;
      Options		: in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector	: in out X_Command_Line.Argument_Vector_Type)
   is
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num	  : Cardinal;
      Argc	  : String_List_Conversion.Index_Type :=
		    String_List_Conversion.Index_Type (X_Command_Line.Argument_Count (Arg_Vector) + 1);
   begin
      if Length (Options) > 0 then
	 Opt_Address := Opt_List (Opt_List.all'First)'Address;
	 Opt_Num     := Cardinal (Length (Options));
      else
	 Opt_Address := X_Toolkit.Null_Address;
	 Opt_Num     := 0;
      end if;
      XtDisplayInitialize (App_Context,
			   Display,
			   Application_Name,
			   Application_Class,
			   Opt_Address,
			   Opt_Num,
			   Argc'Address,
			   X_Command_Line.Internal.Get_Argument_Hook (Arg_Vector));
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Arg_Vector, Argc);
   end Xt_Display_Initialize;


-- UseX11R6 X11R6.3
--
-- new more general initialization procedure (from X11R6 on)
--
   function XtOpenApplication
     (App_Context        : in System.Address;
      Application_Class  : in Xt_C_Resource_String;
      Options            : in System.Address;
      Option_Count       : in Cardinal;
      Arg_Count          : in System.Address;
      Arg_Vector         : in String_List_Conversion.Chars_Ptr_List_Type;
      Fallback_Resources : in String_List_Conversion.Chars_Ptr_List_Type;
      Widget_Cls         : in Widget_Class;
      Args               : in X_Toolkit.Internal.Arg_Rec_Access;
      Num_Args           : in Cardinal)
      return Widget;
   pragma Import (C, XtOpenApplication, "XtOpenApplication"); 


   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List) is
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Open_Application (W, App_Context,
                           Application_Class_String,
                           Options, Fallback_Resources, W_Class, Args);
      Resource_Strings.Free (Application_Class_String);
   end Xt_Open_Application;


   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List) is

      --local variables
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;

      Fallb_Address : String_List_Conversion.Chars_Ptr_List_Type;

      Argc          : String_List_Conversion.Index_Type
                    := String_List_Conversion.Index_Type (X_Command_Line.Argument_Count + 1);

   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := Null_Address;
         Opt_Num     := 0;
      end if;
      if Fallback_Resources /= String_List.Null_Element_Access_List then
         Fallb_Address := String_List_Conversion.To_Chars_Ptr_List (Fallback_Resources,
	                                     Append_Null => True);
      else
         Fallb_Address := String_List_Conversion.Null_Chars_Ptr_List;
      end if;
      W := XtOpenApplication (App_Context'Address,
                              Application_Class,
                              Opt_Address,
                              Opt_Num,
                              Argc'Address,
                              X_Command_Line.Internal.Get_Argument_Hook,
                              Fallb_Address,
                              W_Class,
                              X_Toolkit.Internal.Hook (Args),
                              Cardinal (Length (Args)));
      String_List_Conversion.Free (Fallb_Address);
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Argc);
   end Xt_Open_Application;


   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List) is
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Open_Application (W, App_Context,
                           Application_Class_String,
                           Options, Arg_Vector, Fallback_Resources, W_Class, Args);
      Resource_Strings.Free (Application_Class_String);
   end Xt_Open_Application;


   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List) is

      --local variables
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;

      Fallb_Address : String_List_Conversion.Chars_Ptr_List_Type;

      Argc          : String_List_Conversion.Index_Type
                    := String_List_Conversion.Index_Type (X_Command_Line.Argument_Count (Arg_Vector) + 1);

   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := Null_Address;
         Opt_Num     := 0;
      end if;
      if Fallback_Resources /= String_List.Null_Element_Access_List then
         Fallb_Address := String_List_Conversion.To_Chars_Ptr_List (Fallback_Resources,
	                                     Append_Null => True);
      else
         Fallb_Address := String_List_Conversion.Null_Chars_Ptr_List;
      end if;
      W := XtOpenApplication (App_Context'Address,
                              Application_Class,
                              Opt_Address,
                              Opt_Num,
                              Argc'Address,
                              X_Command_Line.Internal.Get_Argument_Hook (Arg_Vector),
                              Fallb_Address,
                              W_Class,
                              X_Toolkit.Internal.Hook (Args),
                              Cardinal (Length (Args)));
      String_List_Conversion.Free (Fallb_Address);
      Free (Opt_List);
      X_Command_Line.Internal.Actualize_Arguments (Arg_Vector, Argc);
   end Xt_Open_Application;

-- EndX11R6 X11R6.3

   function XtOpenDisplay
     (App_Context       : in Xt_App_Context;
      Display_Name      : in System.Address;
      Application_Name  : in Xt_N_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options           : in System.Address;
      Option_Count      : in Cardinal;
      Arg_Count         : in System.Address;
      Arg_Vector        : in String_List_Conversion.Chars_Ptr_List_Type)
      return X_Lib.Display_Pointer;
   pragma Import (C, XtOpenDisplay, "XtOpenDisplay");


   procedure Xt_Open_Display
     (Display           : out X_Lib.Display_Pointer;
      App_Context       : in Xt_App_Context;
      Display_Name      : in String := "";
      Application_Name  : in String := "";
      Application_Class : in String;
      Options           : in Option_Description_List :=
                             Null_Option_Description_List)
   is
      Application_Name_String  : Xt_N_Resource_String := To_Resource_String (Application_Name);
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Open_Display (Display,
                       App_Context,
                       Display_Name,
                       Application_Name_String,
                       Application_Class_String,
                       Options);
      Resource_Strings.Free (Application_Class_String);
      Resource_Strings.Free (Application_Name_String);
   end Xt_Open_Display;


   procedure Xt_Open_Display
     (Display           : out X_Lib.Display_Pointer;
      App_Context       : in Xt_App_Context;
      Display_Name      : in String := "";
      Application_Name  : in Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options           : in Option_Description_List :=
                             Null_Option_Description_List)
   is
      Name_String : constant Interfaces.C.Char_Array :=
                    Interfaces.C.To_C (Display_Name, Append_Nul => True);
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;
      Argc        : String_List_Conversion.Index_Type :=
                    String_List_Conversion.Index_Type (X_Command_Line.Argument_Count + 1);
   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := X_Toolkit.Null_Address;
         Opt_Num     := 0;
      end if;
      if Display_Name'Length > 0 then
         Display := XtOpenDisplay (App_Context,
                                   Name_String'Address,
                                   Application_Name,
                                   Application_Class,
                                   Opt_Address,
                                   Opt_Num,
                                   Argc'Address,
                                   X_Command_Line.Internal.Get_Argument_Hook);
      else
         Display := XtOpenDisplay (App_Context,
                                   X_Toolkit.Null_Address,
                                   Application_Name,
                                   Application_Class,
                                   Opt_Address,
                                   Opt_Num,
                                   Argc'Address,
                                   X_Command_Line.Internal.Get_Argument_Hook);
      end if;
   end Xt_Open_Display;


   procedure Xt_Open_Display
     (Display           : out X_Lib.Display_Pointer;
      App_Context       : in Xt_App_Context;
      Display_Name      : in String := "";
      Application_Name  : in String := "";
      Application_Class : in String;
      Options           : in Option_Description_List := Null_Option_Description_List;
      Arg_Vector        : in out X_Command_Line.Argument_Vector_Type)
   is
      Application_Name_String  : Xt_N_Resource_String := To_Resource_String (Application_Name);
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
   begin
      Xt_Open_Display (Display,
                       App_Context,
                       Display_Name,
                       Application_Name_String,
                       Application_Class_String,
                       Options,
                       Arg_Vector);
      Resource_Strings.Free (Application_Class_String);
      Resource_Strings.Free (Application_Name_String);
   end Xt_Open_Display;


   procedure Xt_Open_Display
     (Display           : out X_Lib.Display_Pointer;
      App_Context       : in Xt_App_Context;
      Display_Name      : in String := "";
      Application_Name  : in Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options           : in Option_Description_List := Null_Option_Description_List;
      Arg_Vector        : in out X_Command_Line.Argument_Vector_Type)
   is
      Name_String : constant Interfaces.C.Char_Array :=
                    Interfaces.C.To_C (Display_Name, Append_Nul => True);
      Opt_List    : Xrm_Option_Desc_List := To_Xrm_Option_Desc_List (Options);
      Opt_Address : System.Address;
      Opt_Num     : Cardinal;
      Argc        : String_List_Conversion.Index_Type :=
                    String_List_Conversion.Index_Type (X_Command_Line.Argument_Count (Arg_Vector) + 1);
   begin
      if Length (Options) > 0 then
         Opt_Address := Opt_List (Opt_List.all'First)'Address;
         Opt_Num     := Cardinal (Length (Options));
      else
         Opt_Address := X_Toolkit.Null_Address;
         Opt_Num     := 0;
      end if;
      if Display_Name'Length > 0 then
         Display := XtOpenDisplay (App_Context,
                                   Name_String'Address,
                                   Application_Name,
                                   Application_Class,
                                   Opt_Address,
                                   Opt_Num,
                                   Argc'Address,
                                   X_Command_Line.Internal.Get_Argument_Hook (Arg_Vector));
      else
         Display := XtOpenDisplay (App_Context,
                                   X_Toolkit.Null_Address,
                                   Application_Name,
                                   Application_Class,
                                   Opt_Address,
                                   Opt_Num,
                                   Argc'Address,
                                   X_Command_Line.Internal.Get_Argument_Hook (Arg_Vector));
      end if;
   end Xt_Open_Display;


   function Xt_App_Create_Shell
     (Application_Name   : in String;
      Application_Class  : in String;
      W_Class            : in Widget_Class;
      Disp               : in X_Lib.Display_Pointer;
      Args               : in Arg_List := Null_Arg_List)
      return Widget is

      Application_Name_String  : Xt_N_Resource_String := To_Resource_String (Application_Name);
      Application_Class_String : Xt_C_Resource_String := To_Resource_String (Application_Class);
      Return_Widget            : Widget;
   begin
      Return_Widget := Xt_App_Create_Shell (Application_Name_String,
                                            Application_Class_String,
                                            W_Class,
                                            Disp,
					    Args);
      Resource_Strings.Free (Application_Class_String);
      Resource_Strings.Free (Application_Name_String);
      return Return_Widget;
   end Xt_App_Create_Shell;


   function Xt_App_Create_Shell
     (Application_Name   : in Xt_N_Resource_String;
      Application_Class  : in Xt_C_Resource_String;
      W_Class            : in Widget_Class;
      Disp               : in X_Lib.Display_Pointer;
      Args               : in Arg_List := Null_Arg_List)
      return Widget is
      function XtAppCreateShell (Application_Name   : in Xt_N_Resource_String;
                                 Application_Class  : in Xt_C_Resource_String;
                                 W_Class            : in Widget_Class;
                                 Disp               : in X_Lib.Display_Pointer;
                                 Args               : in X_Toolkit.Internal.Arg_Rec_Access;
                                 Num_Args           : in Cardinal) return Widget;
      pragma Import (C, XtAppCreateShell, "XtAppCreateShell");

   begin
      return XtAppCreateShell (Application_Name,
                               Application_Class,
                               W_Class,
                               Disp,
                               X_Toolkit.Internal.Hook (Args),
                               Cardinal (Length (Args)));
   end Xt_App_Create_Shell;


   --
   -- get info about application class and name
   --
   procedure Xt_Get_Application_Name_And_Class
     (Display : in     X_Lib.Display_Pointer;
      Name    :    out Xt_N_Resource_String;
      Class   :    out Xt_C_Resource_String) is
      procedure XtGetApplicationNameAndClass
        (Display : in X_Lib.Display_Pointer;
         Name    : in System.Address;
         Class   : in System.Address);
      pragma Import (C, XtGetApplicationNameAndClass, "XtGetApplicationNameAndClass");
   begin
      XtGetApplicationNameAndClass (Display, Name'Address, Class'Address);
   end Xt_Get_Application_Name_And_Class;



   procedure Xt_App_Set_Fallback_Resources
     (App_Context        : in Xt_App_Context;
      Specification_List : in String_List.Element_Access_List) is
      procedure XtAppSetFallbackResources
        (App_Context        : in Xt_App_Context;
	 Specification_List : in String_List_Conversion.Chars_Ptr_List_Type);
      pragma Import (C, XtAppSetFallbackResources, "XtAppSetFallbackResources");

      Fallb_Address : String_List_Conversion.Chars_Ptr_List_Type;
   begin
      if Specification_List /= String_List.Null_Element_Access_List then
         Fallb_Address := String_List_Conversion.To_Chars_Ptr_List (Specification_List,
	                                     Append_Null => True);
      else
         Fallb_Address := String_List_Conversion.Null_Chars_Ptr_List;
      end if;
      XtAppSetFallbackResources (App_Context, Fallb_Address);
      --  don't free the strings, they are needed to remain until all displays
      --  are initialized => there is a memory leak!
      --
   end Xt_App_Set_Fallback_Resources;



   function Xt_App_Pending (App_Context : in Xt_App_Context) return Xt_Input_Mask is
      function XtAppPending (App_Context : in Xt_App_Context) return C_Xt_Input_Mask;
      pragma Import (C, XtAppPending, "XtAppPending");
   begin
      return To_Mask (XtAppPending (App_Context));
   end Xt_App_Pending;


   procedure Xt_App_Process_Event
     (App_Context : in Xt_App_Context;
      Mask        : in Xt_Input_Mask) is
      procedure XtAppProcessEvent
        (App_Context : in Xt_App_Context;
         Mask        : in C_Xt_Input_Mask);
      pragma Import (C, XtAppProcessEvent, "XtAppProcessEvent");
   begin
      XtAppProcessEvent (App_Context, To_Long (Mask));
   end Xt_App_Process_Event;



-- ----------------------------------------------------------------------------
--
--                  W I D G E T S
--


   function Xt_Create_Widget
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is

      Name_String    : Xt_N_Resource_String := To_Resource_String (Name);
      Return_Widget  : Widget;
   begin
      Return_Widget := Xt_Create_Widget (Name_String,
                                         Class,
                                         Parent,
					 Args);
      Resource_Strings.Free (Name_String);
      return Return_Widget;
   end Xt_Create_Widget;
   pragma Inline (Xt_Create_Widget);


   function Xt_Create_Widget
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is
      function XtCreateWidget (Name   : in Xt_N_Resource_String;
                               Class  : in Widget_Class;
                               Parent : in Widget;
                               Args   : in X_Toolkit.Internal.Arg_Rec_Access;
                               Count  : in Cardinal) return Widget;
      pragma Import (C, XtCreateWidget, "XtCreateWidget");
   begin
      return XtCreateWidget (Name,
                             Class,
                             Parent,
                             X_Toolkit.Internal.Hook (Args),
                             Cardinal (Length (Args)));
   end Xt_Create_Widget;
   pragma Inline (Xt_Create_Widget);



   function Xt_Create_Popup_Shell
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is

      Name_String    : Xt_N_Resource_String := To_Resource_String (Name);
      Return_Widget  : Widget;
   begin
      Return_Widget := Xt_Create_Popup_Shell (Name_String,
                                              Class,
                                              Parent,
					      Args);
      Resource_Strings.Free (Name_String);
      return Return_Widget;
   end Xt_Create_Popup_Shell;
   pragma Inline (Xt_Create_Popup_Shell);


   function Xt_Create_Popup_Shell
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is
      function XtCreatePopupShell (Name   : in Xt_N_Resource_String;
                                   Class  : in Widget_Class;
                                   Parent : in Widget;
                                   Args   : in X_Toolkit.Internal.Arg_Rec_Access;
                                   Count  : in Cardinal) return Widget;
      pragma Import (C, XtCreatePopupShell, "XtCreatePopupShell");

   begin
      return XtCreatePopupShell (Name,
                                 Class,
                                 Parent,
                                 X_Toolkit.Internal.Hook (Args),
                                 Cardinal (Length (Args)));
   end Xt_Create_Popup_Shell;
   pragma Inline (Xt_Create_Popup_Shell);



   function Xt_Create_Managed_Widget
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is

      Name_String    : Xt_N_Resource_String := To_Resource_String (Name);
      Return_Widget  : Widget;
   begin
      Return_Widget := Xt_Create_Managed_Widget (Name_String,
                                                 Class,
                                                 Parent,
						 Args);
      Resource_Strings.Free (Name_String);
      return Return_Widget;
   end Xt_Create_Managed_Widget;
   pragma Inline (Xt_Create_Managed_Widget);


   function Xt_Create_Managed_Widget
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget is
      function XtCreateManagedWidget (Name   : in Xt_N_Resource_String;
                                      Class  : in Widget_Class;
                                      Parent : in Widget;
                                      Args   : in X_Toolkit.Internal.Arg_Rec_Access;
                                      Count  : in Cardinal) return Widget;
      pragma Import (C, XtCreateManagedWidget, "XtCreateManagedWidget");

   begin
      return XtCreateManagedWidget (Name,
                                    Class,
                                    Parent,
                                    X_Toolkit.Internal.Hook (Args),
                                    Cardinal (Length (Args)));
   end Xt_Create_Managed_Widget;
   pragma Inline (Xt_Create_Managed_Widget);



   procedure Xt_Manage_Children (Children : in Widget_List_Type) is
      procedure XtManageChildren
        (Children     : in System.Address;
	 Num_Children : in Cardinal);
      pragma Import (C, XtManageChildren, "XtManageChildren");
   begin
      XtManageChildren (Children'Address,
                        Cardinal (Children'Length));
   end Xt_Manage_Children;
   pragma Inline (Xt_Manage_Children);


   procedure Xt_Unmanage_Children (Children : in Widget_List_Type) is
      procedure XtUnmanageChildren
        (Children     : in System.Address;
	 Num_Children : in Cardinal);
      pragma Import (C, XtUnmanageChildren, "XtUnmanageChildren");
   begin
      XtUnmanageChildren (Children'Address,
                          Cardinal (Children'Length));
   end Xt_Unmanage_Children;
   pragma Inline (Xt_Unmanage_Children);


   procedure Xt_Manage_Children (Children : in Widget_List) is 
      procedure XtManageChildren (Kids     : in X_Toolkit.Internal.Widget_Access;
                                  Num_Kids : in Cardinal);
      pragma Import (C, XtManageChildren, "XtManageChildren");
   begin
      if Children /= Null_Widget_List then
         XtManageChildren (X_Toolkit.Internal.Hook (Children),
                           Cardinal (Length (Children)));
      end if;
   end Xt_Manage_Children;



   procedure Xt_Unmanage_Children (Children : in Widget_List) is 
      procedure XtUnmanageChildren (Kids        : in X_Toolkit.Internal.Widget_Access;
                                    Number_Kids : in Cardinal);
      pragma Import (C, XtUnmanageChildren, "XtUnmanageChildren");
   begin
      if Children /= Null_Widget_List then
         XtUnManageChildren (X_Toolkit.Internal.Hook (Children),
                             Cardinal (Length (Children)));
      end if;
   end Xt_Unmanage_Children;



   procedure Xt_Map_Widget (W : in Widget) is
      procedure XMapWindow (Display : X_Lib.Display_Pointer; Window : X_Lib.Window_ID);
      pragma Import (C, XMapWindow, "XMapWindow");
   begin
      XMapWindow (Display => Xt_Display (W), 
                  Window  => Xt_Window (W));
   end Xt_Map_Widget;



   procedure Xt_Unmap_Widget (W : in Widget) is
      procedure XUnmapWindow (Display : X_Lib.Display_Pointer; Window : X_Lib.Window_ID);
      pragma Import (C, XUnmapWindow, "XUnmapWindow");
   begin 
      XUnmapWindow (Display => Xt_Display (W), 
                    Window  => Xt_Window (W));
   end Xt_Unmap_Widget;
   


   procedure Xt_Set_Values (W    : in Widget;
                            Args : in Arg_List) is
      procedure XtSetValues (W        : in Widget;
                             Args     : in X_Toolkit.Internal.Arg_Rec_Access;
                             Num_Args : in Cardinal); 
      pragma Import (C, XtSetValues, "XtSetValues");
   begin
      if Length (Args) > 0 then
         XtSetValues (W,
                      X_Toolkit.Internal.Hook (Args),
                      Cardinal (Length (Args)));
      end if;
   end Xt_Set_Values;



   procedure Xt_Get_Values (W    : in Widget;
                            Args : in Arg_List) is
      procedure XtGetValues (W      : in Widget;
                             Args   : in X_Toolkit.Internal.Arg_Rec_Access;
                             Count  : in Cardinal);
      pragma Import (C, XtGetValues, "XtGetValues");
   begin
      if Length (Args) > 0 then
         XtGetValues (W,
                      X_Toolkit.Internal.Hook (Args),
                      Cardinal (Length (Args)));
      end if;
   end Xt_Get_Values;
 

   procedure Xt_Set_Sensitive (W         : in Widget;
                               Sensitive : in Boolean) is
      procedure XtSetSensitive (W         : in Widget;
                                Sensitive : in Integer);
      pragma Import (C, XtSetSensitive, "XtSetSensitive");
   begin
      XtSetSensitive (W,
                      Integer (Boolean'Pos (Sensitive)));
   end Xt_Set_Sensitive;




   procedure Xt_Set_Mapped_When_Managed
     (W      : in Widget;
      Mapped : in Boolean) is
      procedure XtSetMappedWhenManaged
        (W                : in Widget;
         Map_When_Managed : in Xt_Boolean);
      pragma Import (C, XtSetMappedWhenManaged, "XtSetMappedWhenManaged");
   begin        
      XtSetMappedWhenManaged (W, To_Xt_Boolean (Mapped));
   end Xt_Set_Mapped_When_Managed;


   function Xt_Name_To_Widget
     (Reference : in Widget;
      Name      : in String)
      return Widget is

      Name_String    : Xt_N_Resource_String := To_Resource_String (Name);
      Return_Widget  : Widget;
   begin
      Return_Widget := Xt_Name_To_Widget (Reference, Name_String);
      Resource_Strings.Free (Name_String);
      return Return_Widget;
   end Xt_Name_To_Widget;


   function Xt_Name_To_Widget
     (Reference : in Widget;
      Name      : in Xt_N_Resource_String)
      return Widget is
      function XtNameToWidget
        (Reference : in Widget;
         Name      : in Xt_N_Resource_String)
         return Widget;
      pragma Import (C, XtNameToWidget, "XtNameToWidget");
   begin
      return XtNameToWidget (Reference, Name);
   end Xt_Name_To_Widget;
   pragma Inline (Xt_Name_To_Widget);



   -- -------------------------------------------------------------------------
   --
   --           Q U E R I E S
   --

   function Xt_Is_Managed (W : in Widget) return Boolean is
      function XtIsManaged (W : in Widget) return Xt_Boolean;
      pragma Import (C, XtIsManaged, "XtIsManaged");
   begin
      return XtIsManaged (W) = Xt_Boolean'(True);
   end Xt_Is_Managed;


   function Xt_Is_Subclass (W     : in Widget;
                            Class : in Widget_Class) return Boolean is
      function XtIsSubclass (W     : in Widget;
                             Class : in Widget_Class) return Xt_Boolean;
      pragma Import (C, XtIsSubclass, "XtIsSubclass");
   begin
      return XtIsSubclass (W, Class) = Xt_Boolean'(True);
   end Xt_Is_Subclass;


   function Xt_Is_Composite (W : in Widget) return Boolean is
   begin
      return Xt_Is_Subclass (W, Composite_Widget_Class);
   end Xt_Is_Composite;


   function Xt_Has_Callbacks (W    : in Widget;             
                              Kind : in Xt_N_Resource_String) return Xt_Callback_Status is
      function XtHasCallbacks (W    : in Widget;
                               Kind : in Xt_N_Resource_String) return Integer;
      pragma Import (C, XtHasCallbacks, "XtHasCallbacks");
   begin
      return Xt_Callback_Status'Val (XtHasCallbacks (W, Kind));
   end Xt_Has_Callbacks;


   function Xt_Is_Realized (W : in Widget) return Boolean is
      function XtIsRealized (W : in Widget) return Xt_Boolean;
      pragma Import (C, XtIsRealized, "XtIsRealized");
   begin
      return XtIsRealized (W) = Xt_Boolean'(True);
   end Xt_Is_Realized;


   function Xt_Name (Object : in Widget) return String is
      function XtName (Object : in Widget)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XtName, "XtName");
   begin
      -- the string returned ftom XtName musn't be freed afterwards!!
      return Interfaces.C.Strings.Value (XtName (Object));
   end;
   pragma Inline (Xt_Name);


   function Xt_Name (Object : in Widget) return Xt_N_Resource_String is
      function XtName (Object : in Widget) return Xt_N_Resource_String;
      pragma Import (C, XtName, "XtName");
   begin
      return XtName (Object);
   end;
   pragma Inline (Xt_Name);


   -- set the value of the WM_COLORMAP_WINDOWS property
   --
   procedure Xt_Set_WM_Colormap_Windows
     (W       : in Widget;
      List    : in Widget_List_Type) is
      procedure XtSetWMColormapWindows
        (W       : in Widget;
         List    : in System.Address;
	 Count   : in Cardinal);
      pragma Import (C, XtSetWMColormapWindows, "XtSetWMColormapWindows");
   begin
      XtSetWMColormapWindows (W,
                              List'Address,
			      Cardinal (List'Length));
   end Xt_Set_WM_Colormap_Windows;
   pragma Inline (Xt_Set_WM_Colormap_Windows);


-- ----------------------------------------------------------------------------
--
--              F I L E   S E A R C H I N G
--
--
   procedure XtFree (Str : in Interfaces.C.Strings.chars_ptr);
   pragma Import (C, XtFree, "XtFree");

   -- search for a file using the given path list (e.g. a colon-separated list),
   -- in which the string substitutions are performed. for every path the
   -- predicate function is called and the accepted path returned.
   -- a NULL_File_Predicate_Proc indicated to use the standard check procedure
   --
   function Xt_Find_File
     (Path         : in String;
      Substitution : in Substitution_List_Type := Null_Substitution_List;
      Predicate    : in Xt_File_Predicate_Proc := Null_File_Predicate_Proc)
      return String is
      function XtFindFile
        (Path             : in System.Address;
	 Subsitution      : in System.Address;
	 Num_Substitution : in Cardinal;
	 Predicate        : in Xt_File_Predicate_Proc)
	 return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XtFindFile, "XtFindFile");
      Path_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Path, Append_Nul => True);
      Subst : System.Address;
      Ptr   : Interfaces.C.Strings.chars_ptr;
   begin
      if Substitution'Length = 0 then
         Subst := System.Null_Address;
      else
         Subst := Substitution'Address;
      end if;
      Ptr := XtFindFile (Path_String'Address,
                         Subst, Cardinal (Substitution'Length),
                         Predicate);
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
	    Return_String : constant String := Interfaces.C.Strings.Value (Ptr);
         begin
	    XtFree (Ptr);
            return Return_String;
	 end;
      end if;
   end Xt_Find_File;


   -- search for a file using standard substitution
   --
   function Xt_Resolve_Pathname
     (Display      : in X_Lib.Display_Pointer;
      File_Type    : in String;
      File_Name    : in String;
      Suffix       : in String;
      Path         : in String;
      Substitution : in Substitution_List_Type := Null_Substitution_List;
      Predicate    : in Xt_File_Predicate_Proc := Null_File_Predicate_Proc)
      return String is
      function XtResolvePathname
        (Display          : in X_Lib.Display_Pointer;
	 File_Type        : in System.Address;
	 File_Name        : in System.Address;
	 Suffix           : in System.Address;
	 Path             : in System.Address;
	 Subsitution      : in System.Address;
	 Num_Substitution : in Cardinal;
	 Predicate        : in Xt_File_Predicate_Proc)
	 return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XtResolvePathname, "XtResolvePathname");
      Type_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (File_Type, Append_Nul => True);
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (File_Name, Append_Nul => True);
      Suffix_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Suffix, Append_Nul => True);
      Path_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Path, Append_Nul => True);
      Subst : System.Address;
      Ptr   : Interfaces.C.Strings.chars_ptr;
   begin
      if Substitution'Length = 0 then
         Subst := System.Null_Address;
      else
         Subst := Substitution'Address;
      end if;
      Ptr := XtResolvePathname (Display, Type_String'Address,
                                Name_String'Address,
				Suffix_String'Address,
				Path_String'Address,
                                Subst, Cardinal (Substitution'Length),
                                Predicate);
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
	    Return_String : constant String := Interfaces.C.Strings.Value (Ptr);
         begin
	    XtFree (Ptr);
            return Return_String;
	 end;
      end if;
   end Xt_Resolve_Pathname;





-- ----------------------------------------------------------------------------
--
-- EVENT related functions and procedures
--

   function To_Long is new Ada.Unchecked_Conversion (X_Lib.Event_Mask, Interfaces.C.long);


   -- -------------------------------------------------------------------------
   --
   -- XtAddEventHandler
   --
   procedure Xt_Add_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtAddEventHandler
        (W           : in Widget;
         Ev_Mask     : in Interfaces.C.long;
         Nonmaskable : in Xt_Boolean;
         Proc        : in Xt_Event_Handler_Proc;
         Closure     : in Xt_Pointer);
      pragma Import (C, XtAddEventHandler, "XtAddEventHandler");

   begin
      XtAddEventHandler (W, To_Long (Ev_Mask),
                         To_Xt_Boolean (Nonmaskable),
                         Proc, Closure);
   end Xt_Add_Event_Handler;
   pragma Inline (Xt_Add_Event_Handler);


   -- -------------------------------------------------------------------------
   --
   -- XtRemoveEventHandler
   --
   procedure Xt_Remove_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtRemoveEventHandler
        (W           : in Widget;
         Ev_Mask     : in Interfaces.C.long;
         Nonmaskable : in Xt_Boolean;
         Proc        : in Xt_Event_Handler_Proc;
         Closure     : in Xt_Pointer);
      pragma Import (C, XtRemoveEventHandler, "XtRemoveEventHandler");

   begin
      XtRemoveEventHandler (W, To_Long (Ev_Mask),
                            To_Xt_Boolean (Nonmaskable),
                            Proc, Closure);
   end Xt_Remove_Event_Handler;
   pragma Inline (Xt_Remove_Event_Handler);


   -- -------------------------------------------------------------------------
   --
   -- XtAddRawEventHandler
   --
   procedure Xt_Add_Raw_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtAddRawEventHandler
        (W           : in Widget;
         Ev_Mask     : in Interfaces.C.long;
         Nonmaskable : in Xt_Boolean;
         Proc        : in Xt_Event_Handler_Proc;
         Closure     : in Xt_Pointer);
      pragma Import (C, XtAddRawEventHandler, "XtAddRawEventHandler");

   begin
      XtAddRawEventHandler (W, To_Long (Ev_Mask),
                            To_Xt_Boolean (Nonmaskable),
                            Proc, Closure);
   end Xt_Add_Raw_Event_Handler;
   pragma Inline (Xt_Add_Raw_Event_Handler);


   -- -------------------------------------------------------------------------
   --
   -- XtRemoveRawEventHandler
   --
   procedure Xt_Remove_Raw_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer) is
      procedure XtRemoveRawEventHandler
        (W           : in Widget;
         Ev_Mask     : in Interfaces.C.long;
         Nonmaskable : in Xt_Boolean;
         Proc        : in Xt_Event_Handler_Proc;
         Closure     : in Xt_Pointer);
      pragma Import (C, XtRemoveRawEventHandler, "XtRemoveRawEventHandler");

   begin
      XtRemoveRawEventHandler (W, To_Long (Ev_Mask),
                               To_Xt_Boolean (Nonmaskable),
                               Proc, Closure);
   end Xt_Remove_Raw_Event_Handler;
   pragma Inline (Xt_Remove_Raw_Event_Handler);


   procedure Xt_App_Next_Event (App_Context : in Xt_App_Context;
                                Event       : out X_Lib.X_Event) is
      procedure XtAppNextEvent (App_Context : in Xt_App_Context;
                                Event       : in System.Address);
      pragma Import (C, XtAppNextEvent, "XtAppNextEvent");
   begin
      XtAppNextEvent (App_Context, Event'Address);
   end Xt_App_Next_Event;


   procedure Xt_App_Peek_Event (App_Context : in  Xt_App_Context;
                                Event       : out X_Lib.X_Event) is
      procedure XtAppPeekEvent (App_Context : in Xt_App_Context;
                                Event       : in System.Address);
      pragma Import (C, XtAppPeekEvent, "XtAppPeekEvent");
   begin
      XtAppPeekEvent (App_Context, Event'Address);
   end Xt_App_Peek_Event;


   function Xt_Dispatch_Event (Event : in X_Lib.X_Event) return Boolean is
      function XtDispatchEvent (Event : in System.Address) return Xt_Boolean;
      pragma Import (C, XtDispatchEvent, "XtDispatchEvent");
   begin
      return XtDispatchEvent (Event'Address) = Xt_Boolean'(True);
   end Xt_Dispatch_Event;
 



-- ----------------------------------------------------------------------------
--
-- Translation Table Manipulation 
--
--

   function Xt_Parse_Translation_Table (Table : in String) return Xt_Translations is
      function XtParseTranslationTable (T : System.Address) return Xt_Translations;
      pragma Import (C, XtParseTranslationTable, "XtParseTranslationTable");

      Table_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Table, Append_Nul => True);
   begin
      return XtParseTranslationTable (Table_String'Address);
   end Xt_Parse_Translation_Table;


   function Xt_Parse_Accelerator_Table (Table : in String) return Xt_Accelerators is
      function XtParseAcceleratorTable (Table : in System.Address)
         return Xt_Accelerators;
      pragma Import (C, XtParseAcceleratorTable,"XtParseAcceleratorTable"); 

      Table_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Table, Append_Nul => True);
   begin
      return XtParseAcceleratorTable (Table_String'Address);
   end Xt_Parse_Accelerator_Table;



-- ----------------------------------------------------------------------------
--
--  Graphics Context
--

   -- we have to define this function here because of the parameter Valuemask,
   -- which is just an unsigned long in C, but a record here
   -- gcc would pass a pointer to the record to the function, which would be
   -- wrong
   function Xt_Allocate_GC
     (Object         : in Widget;
      Depth          : in X_Lib.Color_Depth;
      Value_Mask     : in Xt_GC_Mask;
      Values         : in X_Lib.X_GC_Values;
      Dynamic_Mask   : in Xt_GC_Mask;
      Dont_Care_Mask : in Xt_GC_Mask) return X_Lib.GC_Pointer is
      function XtAllocateGC
        (Object         : in Widget;
         Depth          : in Cardinal;
         Value_Mask     : in X_Lib.C_X_GC_Valuemask;
         Values         : in System.Address;
         Dynamic_Mask   : in X_Lib.C_X_GC_Valuemask;
         Dont_Care_Mask : in X_Lib.C_X_GC_Valuemask) return X_Lib.GC_Pointer;
      pragma Import (C, XtAllocateGC, "XtAllocateGC");
   begin
      return XtAllocateGC (Object, Cardinal (Depth),
                           X_Lib.To_Long (Value_Mask),
                           Values'Address,
                           X_Lib.To_Long (Dynamic_Mask),
                           X_Lib.To_Long (Dont_Care_Mask));
   end Xt_Allocate_GC;
   pragma Inline (Xt_Allocate_GC);



   function Xt_Get_GC
     (W          : in Widget;
      Value_Mask : in Xt_GC_Mask;
      Values     : in X_Lib.X_GC_Values) return X_Lib.GC_Pointer is
      function XtGetGC
        (W          : in Widget;
         Value_Mask : in X_Lib.C_X_GC_Valuemask;
         Values     : in System.Address) return X_Lib.GC_Pointer;
      pragma Import (C, XtGetGC, "XtGetGC");
   begin
      return XtGetGC (W, X_Lib.To_Long (Value_Mask), Values'Address);
   end Xt_Get_GC;
   pragma Inline (Xt_Get_GC);


-- ----------------------------------------------------------------------------
--
-- P O P U P S
--
   procedure Xt_Add_Grab
     (W             : in Widget;
      Exclusive     : in Boolean;
      Spring_Loaded : in Boolean) is
      procedure XtAddGrab
        (W             : in Widget;
         Exclusive     : in Xt_Boolean;
         Spring_Loaded : in Xt_Boolean);
      pragma Import (C, XtAddGrab, "XtAddGrab");
   begin
      XtAddGrab (W, To_Xt_Boolean (Exclusive), To_Xt_Boolean (Spring_Loaded));
   end Xt_Add_Grab;



-- ----------------------------------------------------------------------------
--
--              E R R O R   H A N D L I N G
--
--

   procedure Xt_App_Error
     (App_Context : in Xt_App_Context;
      Message     : in String) is
      procedure XtAppError
        (App_Context : in Xt_App_Context;
         Message     : in System.Address);
      pragma Import (C, XtAppError, "XtAppError");
      Message_String : constant Interfaces.C.Char_Array
                     := Interfaces.C.To_C (Message, Append_Nul => True);
   begin
      XtAppError (App_Context, Message_String'Address);
   end Xt_App_Error;



-- ----------------------------------------------------------------------------
--
--              K E Y B O A R D
--
--

   procedure Xt_Get_Action_Keysym
     (Event  : in     X_Lib.X_Event_Pointer;
      Keysym :    out X_Lib.Key_Sym_ID;
      Mods   :    out X_Lib.Modifiers_Mask_Type) is
      function XtGetActionKeysym
        (Event  : in X_Lib.X_Event_Pointer;
         Mods   : in System.Address)
         return X_Lib.Key_Sym_ID;
      pragma Import (C, XtGetActionKeysym, "XtGetActionKeysym");
      Mod_Return : C_Modifiers;
   begin
      Keysym := XtGetActionKeysym (Event, Mod_Return'Address);
      Mods   := To_Mask (Mod_Return);
   end Xt_Get_Action_Keysym;




-- UseX11R6 X11R6.3
-- ----------------------------------------------------------------------------
--
-- T H R E A D S
--
   procedure Xt_Toolkit_Thread_Initialize is
      function XtToolkitThreadInitialize return Xt_Boolean;
      pragma Import (C, XtToolkitThreadInitialize, "XtToolkitThreadInitialize");
   begin
      if XtToolkitThreadInitialize = Xt_Boolean'(False) then
         raise Threads_Not_Supported;
      end if;
   end Xt_Toolkit_Thread_Initialize;


   function  Xt_App_Get_Exit_Flag (App_Context : in Xt_App_Context)
      return Boolean is
      function  XtAppGetExitFlag (App_Context : in Xt_App_Context)
         return Xt_Boolean;
      pragma Import (C, XtAppGetExitFlag, "XtAppGetExitFlag");
   begin
      return To_Boolean (XtAppGetExitFlag (App_Context));
   end Xt_App_Get_Exit_Flag;
   pragma Inline (Xt_App_Get_Exit_Flag);


-- EndX11R6 X11R6.3



-- ----------------------------------------------------------------------------
--
--        T Y P E   C O N V E R S I O N   R O U T I N E S
--

   function To_Address    (Source : in Widget)     return System.Address is
   begin
      return System.Address (Source);
   end To_Address;
   pragma Inline (To_Address);


   function To_Address    (Source : in Xt_Pointer)     return System.Address is
      function To_Adr is new Ada.Unchecked_Conversion (Xt_Pointer, System.Address);
   begin
      return To_Adr (Source);
   end To_Address;
   pragma Inline (To_Address);


   function To_Address    (Source : in Xt_Resource_String) return System.Address is
      function To_Adr is new Ada.Unchecked_Conversion (Xt_Resource_String, System.Address);
   begin
      return To_Adr (Source);
   end To_Address;
   pragma Inline (To_Address);


   function To_Address    (Source : in X_Strings.X_String) return System.Address is
      function To_Adr is new Ada.Unchecked_Conversion (X_Strings.X_String, System.Address);
   begin
      return To_Adr (Source);
   end To_Address;
   pragma Inline (To_Address);


   function To_Xt_Pointer (Source : in System.Address) return Xt_Pointer is
      function To_Point is new Ada.Unchecked_Conversion (System.Address, Xt_Pointer);
   begin
      return To_Point (Source);
   end To_Xt_Pointer;
   pragma Inline (To_Xt_Pointer);


   function To_Xt_Pointer (Source : in Widget) return Xt_Pointer is
      function To_Point is new Ada.Unchecked_Conversion (Widget, Xt_Pointer);
   begin
      return To_Point (Source);
   end To_Xt_Pointer;
   pragma Inline (To_Xt_Pointer);


   function To_Xt_Pointer (Source : in Integer) return Xt_Pointer is
      function To_Point is new Ada.Unchecked_Conversion (Integer, Xt_Pointer);
   begin
      return To_Point (Source);
   end To_Xt_Pointer;
   pragma Inline (To_Xt_Pointer);


   function To_Xt_Pointer (Source : in Xt_Resource_String) return Xt_Pointer is
      function To_Point is
         new Ada.Unchecked_Conversion (Xt_Resource_String, Xt_Pointer);
   begin
      return To_Point (Source);
   end To_Xt_Pointer;
   pragma Inline (To_Xt_Pointer);


   function To_Xt_Pointer (Source : in X_Strings.X_String) return Xt_Pointer is
      function To_Point is
         new Ada.Unchecked_Conversion (X_Strings.X_String, Xt_Pointer);
   begin
      return To_Point (Source);
   end To_Xt_Pointer;
   pragma Inline (To_Xt_Pointer);


   function To_Widget (Source : in Xt_Pointer) return Widget is
      function To_Wid is new Ada.Unchecked_Conversion (Xt_Pointer, Widget);
   begin
      return To_Wid (Source);
   end To_Widget;
   pragma Inline (To_Widget);


   function To_Widget (Source : in System.Address) return Widget is
   begin
      return Widget (Source);
   end To_Widget;
   pragma Inline (To_Widget);


   function To_X_String (Source : in Xt_Pointer) return X_Strings.X_String is
      function To_XS is new Ada.Unchecked_Conversion (Xt_Pointer, X_Strings.X_String);
   begin
      return To_XS (Source);
   end To_X_String;
   pragma Inline (To_X_String);


end X_Toolkit;
