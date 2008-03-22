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
--          November 8, 1998 HFVogt: added X11R6.3 conditional defines
--          February 29, 2000 HFVogt: include additions and changes from
--                                    Vadim Godunko (vadik@zto.rost.ru)
--          June 14, 2000: added Append_Set/Get for Atoms
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
--          13 Jan 2002 H.-F. Vogt: add type Cardinal_Array
--          03 Feb 2002 H.-F. Vogt: add exception Invalid_Widget_Error
--
-------------------------------------------------------------------------------

with Interfaces.C,
     System,
     Generic_List_Types,
     Resource_Strings,
     String_List,
     String_List_Conversion,
     X_Command_Line,
     X_Strings,
     X_Lib,
     X_Lib.Resource;

package X_Toolkit is

   use X_Lib.Resource;

-- ----------------------------------------------------------------------------
--
--  T Y P E S
--

   Null_Address : System.Address renames System.Null_Address;

   
   -- Widgets etc. 

   --type Widget       is private;
   --Null_Widget : constant Widget;
   --
   type Widget       is new System.Address;       
   Null_Widget       : constant Widget := Widget (Null_Address);

   Invalid_Widget_Error : exception;

   type Widget_Class is private;
   Null_Widget_Class : constant Widget_Class;


   subtype Xt_Pointer is X_Lib.X_Pointer;
   Null_Xt_Pointer : Xt_Pointer renames X_Lib.Null_X_Pointer;

   -- Common types for widget resources:

   subtype    Cardinal  is Interfaces.C.unsigned;
   type Cardinal_Array is array (Natural range <>) of Cardinal;

   subtype Milliseconds is Interfaces.C.unsigned_long;



   -- ***********************
   --
   --  we need here this special Boolean Type Xt_Boolean (called Boolean
   --  in Intrinsics.h) to convert Xt-Boolean -> Ada-Boolean
   --
   --  for CRAY: for Xt_Boolean'Size use Interfaces.C.long'Size;
   --
   -- X_Toolkit's own boolean type
   type Xt_Boolean is (False, True);
   for Xt_Boolean use (False => 0, True => 1);
   for Xt_Boolean'Size use Interfaces.C.signed_char'Size;

   function To_Xt_Boolean (Val : in Boolean) return Xt_Boolean;
   function To_Boolean (Val : in Xt_Boolean) return Boolean;


   -- types for resource strings
   --
   subtype Xt_Resource_String is Resource_Strings.Resource_String;
   Null_Xt_Resource_String : Xt_Resource_String
      renames Resource_Strings.Null_Resource_String;

   subtype Xt_N_Resource_String is Xt_Resource_String;  -- Name
   subtype Xt_C_Resource_String is Xt_Resource_String;  -- Class
   subtype Xt_R_Resource_String is Xt_Resource_String;  -- Resource

   function To_Resource_String (Item : in String) return Xt_Resource_String
      renames Resource_Strings.To_Resource_String;

   function To_Resource_String (Item : in X_Strings.X_String) return Xt_Resource_String
      renames Resource_Strings.To_Resource_String;

   function To_String          (Item : in Xt_Resource_String) return String
      renames Resource_Strings.To_String;



   -- Application Context
   type Xt_App_Context is private;
   Null_Xt_App_Context : constant Xt_App_Context;


   type Xt_Input_Mask is record
      Xevent          : Boolean;
      Timer           : Boolean;
      Alternate_Input : Boolean;
      Signal          : Boolean;
   end record;
   for Xt_Input_Mask use record
-- UseLittleEndian
      Xevent          at 0 range 0 .. 0;
      Timer           at 0 range 1 .. 1;
      Alternate_Input at 0 range 2 .. 2;
      Signal          at 0 range 3 .. 3;
-- NotLittleEndian
--!       Xevent          at 0 range 3 .. 3;
--!       Timer           at 0 range 2 .. 2;
--!       Alternate_Input at 0 range 1 .. 1;
--!       Signal          at 0 range 0 .. 0;
-- EndLittleEndian
   end record;
   pragma Pack (Xt_Input_Mask);
   for Xt_Input_Mask'Size use 4;


   --
   -- predefined constants for type Xt_Input_Mask
   --
   Xt_IM_Xevent          : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => True,
                                    Timer           => False,
                                    Alternate_Input => False,
                                    Signal          => False);
   Xt_IM_Timer           : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => False,
                                    Timer           => True,
                                    Alternate_Input => False,
                                    Signal          => False);
   Xt_IM_Alternate_Input : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => False,
                                    Timer           => False,
                                    Alternate_Input => True,
                                    Signal          => False);
   Xt_IM_Signal          : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => False,
                                    Timer           => False,
                                    Alternate_Input => False,
                                    Signal          => True);
   Xt_IM_All             : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => True,
                                    Timer           => True,
                                    Alternate_Input => True,
                                    Signal          => True);

   Xt_IM_None            : constant Xt_Input_Mask :=
                     Xt_Input_Mask'(Xevent          => False,
                                    Timer           => False,
                                    Alternate_Input => False,
                                    Signal          => False);


   function "="   (Left, Right : in Xt_Input_Mask) return Boolean;
   function "or"  (Left, Right : in Xt_Input_Mask) return Xt_Input_Mask;
   function "and" (Left, Right : in Xt_Input_Mask) return Xt_Input_Mask;


-- ----------------------------------------------------------------------------
--
-- ACTIONS
--
--

   --
   -- Action Hook
   --
   type Xt_Action_Hook_Id is private;
   Null_Xt_Action_Hook_Id : constant Xt_Action_Hook_Id;

   type Xt_Action_Hook_Proc is access
      procedure (W           : in Widget;
                 Client_Data : in Xt_Pointer;
                 Action_Name : in X_Strings.X_String;
                 Event       : in X_Lib.X_Event_Pointer;
                 Params      : in String_List_Conversion.Chars_Ptr_List_Type;
                 Num_Params  : access String_List_Conversion.Index_Type);
   pragma Convention (C, Xt_Action_Hook_Proc);
   Null_Action_Null_Proc : constant Xt_Action_Hook_Proc := null;


   function Xt_App_Add_Action_Hook
     (App_Context : in Xt_App_Context;
      Proc        : in Xt_Action_Hook_Proc;
      Client_Data : in Xt_Pointer)
      return Xt_Action_Hook_Id;


   procedure Xt_Remove_Action_Hook (Id : in Xt_Action_Hook_Id);


   --
   -- Action Proc
   --
   type Xt_Action_Proc is access
      procedure (W          : in Widget;
                 Event      : in X_Lib.X_Event_Pointer;
                 Params     : in String_List_Conversion.Chars_Ptr_List_Type;
                 Num_Params : access String_List_Conversion.Index_Type);
   pragma Convention (C, Xt_Action_Proc);
   Null_Action_Proc : constant Xt_Action_Proc := null;


   type Xt_Actions_Rec is record
      Name : X_Strings.X_String;
      Proc : Xt_Action_Proc;
   end record;


   type Xt_Action_List is array (Natural range <>) of Xt_Actions_Rec;


   procedure Xt_App_Add_Actions
     (App_Context : in Xt_App_Context;
      Action_List : in Xt_Action_List);


   procedure Xt_Call_Action_Proc
     (W           : in Widget;
      Action_Name : in String;
      Event       : in X_Lib.X_Event_Pointer;
      Params      : in String_List.Element_Access_List := String_List.Null_Element_Access_List);



-- ----------------------------------------------------------------------------
--
-- CALLBACKS
--
--

   --
   -- Callback Proc
   --
   type Xt_Callback_Proc is access procedure (W         : in Widget;
                                              Closure   : in Xt_Pointer;
                                              Call_Data : in Xt_Pointer);
   pragma Convention (C, Xt_Callback_Proc);
   Null_Callback_Proc : constant Xt_Callback_Proc := null;


   --
   -- Callback Record
   --
   type Callback_Rec is record
      Callback  : Xt_Callback_Proc := Null_Callback_Proc;
      Closure   : Xt_Pointer       := Null_Xt_Pointer;
   end record;

   Null_Callback_Rec : constant Callback_Rec :=
      (Callback  => Null_Callback_Proc,
       Closure   => Null_Xt_Pointer);


   --
   -- Callback List
   --
   package Callback_Lists is
      new Generic_List_Types (Callback_Rec);
   type Callback_List is
      new Callback_Lists.Unbounded_List;

   Null_Callback_List : constant Callback_List := Callback_List (Callback_Lists.Null_Unbounded_List);


   function Length (List : in Callback_List) return Natural;

   procedure Append (List : in out Callback_List;
                     Rec  : in     Callback_Rec);

   procedure Append (List      : in out Callback_List;
                     Callback  : in     Xt_Callback_Proc;
		     Closure   : in     Xt_Pointer);


   type Xt_Callback_Status is (Xt_Callback_No_List,
                               Xt_Callback_Has_None, 
                               Xt_Callback_Has_Some);

   -- -------------------------------------------------------------------------
   --
   -- PREDEFINED CALLBACK PROCEDURES
   --

   procedure Xt_Callback_None (Popup_Shell : in Widget;
                               Client_Data : in Xt_Pointer;
                               Call_Data   : in Xt_Pointer);

   procedure Xt_Callback_Nonexclusive (Popup_Shell : in Widget;
                                       Client_Data : in Xt_Pointer;
                                       Call_Data   : in Xt_Pointer);

   procedure Xt_Callback_Exclusive (Popup_Shell : in Widget;
                                    Client_Data : in Xt_Pointer;
                                    Call_Data   : in Xt_Pointer);

   procedure Xt_Callback_Popdown (Popup_Shell : in Widget;
                                  Client_Data : in Xt_Pointer;
                                  Call_Data   : in Xt_Pointer);



   -- -------------------------------------------------------------------------
   --
   --  XtAddCallback(s)
   --

   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Xt_Pointer := Null_Xt_Pointer);

   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Widget);

   procedure Xt_Add_Callback (To          : in Widget;
                              Kind        : in Xt_N_Resource_String;
                              Callback    : in Xt_Callback_Proc;
                              Client_Data : in Integer);


   procedure Xt_Add_Callbacks (To           : in Widget;
                               Kind         : in Xt_N_Resource_String;
                               Callbacks    : in Callback_List);


   -- -------------------------------------------------------------------------
   --
   --  XtCallCallbacks
   --
   procedure Xt_Call_Callbacks (From      : in Widget;
                                Kind      : in Xt_N_Resource_String;
                                Call_Data : in Xt_Pointer := Null_Xt_Pointer);

   -- -------------------------------------------------------------------------
   --
   --  XtRemoveCallback(s)
   --
   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Xt_Pointer := Null_Xt_Pointer);

   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Widget);

   procedure Xt_Remove_Callback (From        : in Widget;
                                 Kind        : in Xt_N_Resource_String;
                                 Callback    : in Xt_Callback_Proc;
                                 Client_Data : in Integer);

   procedure Xt_Remove_Callbacks (From      : in Widget;
                                  Kind      : in Xt_N_Resource_String;
                                  Callbacks : in Callback_List);

   procedure Xt_Remove_All_Callbacks (From  : in Widget;
                                      Kind  : in Xt_N_Resource_String);



-- ----------------------------------------------------------------------------
--
--                    A R G U M E N T   H A N D L I N G
--


   type Arg_Rec is record
      Name  : Xt_Resource_String;
      Value : Interfaces.C.long;
   end record;

   Null_Arg_Rec : constant Arg_Rec := (Null_Xt_Resource_String, 0);

   package Arg_Lists is
      new Generic_List_Types (Arg_Rec);
   type Arg_List is
      new Arg_Lists.Unbounded_List;

   Null_Arg_List : constant Arg_List := Arg_List (Arg_Lists.Null_Unbounded_List);


   function Length (List : in  Arg_List) return Natural;


   procedure Append (List  : in out Arg_List;
                     Which : in     Arg_List);

   procedure Append (List  : in out Arg_List;
                     Which : in     Arg_Rec);


   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Interfaces.C.long);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Interfaces.C.long);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Integer);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Integer);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Float);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Float);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Boolean);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Boolean);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     System.Address);  

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    System.Address);  
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Pointer);  

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Xt_Pointer);  
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Widget);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Widget);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Callback_Proc);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Callback_List);

   -- procedure Append_Get (List  : in out Arg_List;
   --                       Name  : in     Xt_N_Resource_String;
   --                       Value : out    Callback_List);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Character);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Character);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.XID);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.XID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Drawable_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Font_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Cursor_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Colormap_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.GContext_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Key_Sym_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Visual_ID);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Pixel);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Pixel);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Dimension);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Dimension);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Position);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Position);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_Lib.Screen_Pointer);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Screen_Pointer);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Interfaces.C.unsigned_char);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    Interfaces.C.unsigned_char);
   pragma Convention (C, Append_Get);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     X_lib.Atom);

   procedure Append_Get (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : out    X_Lib.Atom);
   pragma Convention (C, Append_Get);


-- ----------------------------------------------------------------------------
--
--       T O O L K I T   I N I T I A L I Z A T I O N
--                                               

   -- -------------------------------------------------------------------------
   --
   -- XtToolkitInitialize
   --
   procedure Xt_Toolkit_Initialize;



   --
   -- Functions using Application Context
   --
   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List);

   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List);

   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List);

   procedure Xt_App_Initialize
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List         := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      Args               : in     Arg_List                        := Null_Arg_List);


   procedure Xt_Display_Initialize
     (App_Context	: in Xt_App_Context;
      Display		: in X_Lib.Display_Pointer;
      Application_Name  : in String := "";
      Application_Class : in String;
      Options		: in Option_Description_List := Null_Option_Description_List);

   procedure Xt_Display_Initialize
     (App_Context	: in     Xt_App_Context;
      Display		: in     X_Lib.Display_Pointer;
      Application_Name  : in     String := "";
      Application_Class : in     String;
      Options		: in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector	: in out X_Command_Line.Argument_Vector_Type);

   procedure Xt_Display_Initialize
     (App_Context	: in Xt_App_Context;
      Display		: in X_Lib.Display_Pointer;
      Application_Name  : in Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in Xt_C_Resource_String;
      Options		: in Option_Description_List := Null_Option_Description_List);

   procedure Xt_Display_Initialize
     (App_Context	: in     Xt_App_Context;
      Display		: in     X_Lib.Display_Pointer;
      Application_Name  : in     Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in     Xt_C_Resource_String;
      Options		: in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector	: in out X_Command_Line.Argument_Vector_Type);


-- UseX11R6 X11R6.3
   --
   -- new more general initialization procedure (from X11R6 on)
   --
   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List);

   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List);

   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List);

   procedure Xt_Open_Application
     (W                  :    out Widget;
      App_Context        :    out Xt_App_Context;
      Application_Class  : in     Xt_C_Resource_String;
      Options            : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector         : in out X_Command_Line.Argument_Vector_Type;
      Fallback_Resources : in     String_List.Element_Access_List := String_List.Null_Element_Access_List;
      W_Class            : in     Widget_Class;
      Args               : in     Arg_List := Null_Arg_List);

-- EndX11R6 X11R6.3

   procedure Xt_Open_Display
     (Display           :    out X_Lib.Display_Pointer;
      App_Context       : in     Xt_App_Context;
      Display_Name      : in     String := "";
      Application_Name  : in     String := "";
      Application_Class : in     String;
      Options           : in     Option_Description_List :=
                                 Null_Option_Description_List);

   procedure Xt_Open_Display
     (Display           :    out X_Lib.Display_Pointer;
      App_Context       : in     Xt_App_Context;
      Display_Name      : in     String := "";
      Application_Name  : in     Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in     Xt_C_Resource_String;
      Options           : in     Option_Description_List :=
                                 Null_Option_Description_List);

   procedure Xt_Open_Display
     (Display           :    out X_Lib.Display_Pointer;
      App_Context       : in     Xt_App_Context;
      Display_Name      : in     String := "";
      Application_Name  : in     String := "";
      Application_Class : in     String;
      Options           : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector        : in out X_Command_Line.Argument_Vector_Type);

   procedure Xt_Open_Display
     (Display           :    out X_Lib.Display_Pointer;
      App_Context       : in     Xt_App_Context;
      Display_Name      : in     String := "";
      Application_Name  : in     Xt_N_Resource_String := Null_Xt_Resource_String;
      Application_Class : in     Xt_N_Resource_String;
      Options           : in     Option_Description_List := Null_Option_Description_List;
      Arg_Vector        : in out X_Command_Line.Argument_Vector_Type);


   -- create shell widget as root of widget tree
   function Xt_App_Create_Shell
     (Application_Name   : in String;
      Application_Class  : in String;
      W_Class            : in Widget_Class;
      Disp               : in X_Lib.Display_Pointer;
      Args               : in Arg_List := Null_Arg_List)
      return Widget;

   function Xt_App_Create_Shell
     (Application_Name   : in Xt_N_Resource_String;
      Application_Class  : in Xt_C_Resource_String;
      W_Class            : in Widget_Class;
      Disp               : in X_Lib.Display_Pointer;
      Args               : in Arg_List := Null_Arg_List)
      return Widget;


   --
   -- get info about application class and name
   --
   procedure Xt_Get_Application_Name_And_Class
     (Display : in     X_Lib.Display_Pointer;
      Name    :    out Xt_N_Resource_String;
      Class   :    out Xt_C_Resource_String);


   --
   -- other functions with the application context
   --
   function Xt_Create_Application_Context return Xt_App_Context;

   procedure Xt_Destroy_Application_Context (App_Context : in Xt_App_Context);

   function Xt_Display_To_Application_Context
     (Display : in X_Lib.Display_Pointer)
      return Xt_App_Context;

   function Xt_Widget_To_Application_Context
     (W : in Widget)
      return Xt_App_Context;


   procedure Xt_App_Set_Fallback_Resources
     (App_Context        : in Xt_App_Context;
      Specification_List : in String_List.Element_Access_List);


   function Xt_App_Pending
     (App_Context : in Xt_App_Context)
      return Xt_Input_Mask;

   procedure Xt_App_Process_Event
     (App_Context : in Xt_App_Context;
      Mask        : in Xt_Input_Mask);


   procedure Xt_App_Main_Loop (App_Context : in Xt_App_Context);


   --
   -- functions that work directly with the display
   --
   procedure Xt_Close_Display (Disp : in X_Lib.Display_Pointer);


-- ----------------------------------------------------------------------------
--
--                  W I D G E T S
--

   -- -------------------------------------------------------------------------
   --
   --  widget classes of the intrinsics
   --

   Core_Widget_Class             : constant Widget_Class;
   Composite_Widget_Class        : constant Widget_Class;
   Constraint_Widget_Class       : constant Widget_Class;

   --
   -- Widget List Type (simple list)
   --
   type Widget_List_Type is array (Natural range <>) of aliased Widget;


   --
   -- Widget List
   --
   package Widget_Lists is
      new Generic_List_Types (Widget);
   type Widget_List is
      new Widget_Lists.Unbounded_List;

   Null_Widget_List : constant Widget_List := Widget_List (Widget_Lists.Null_Unbounded_List);


   function Length (List : in  Widget_List) return Natural;

   function Element
     (List   : in Widget_List;
      Index  : in Natural)
      return Widget;

   function "=" (Left, Right : in Widget_List) return Boolean;

   function "&" (Left, Right : in Widget_List) return Widget_List;
   function "&" (Left  : in Widget_List;
                 Right : in Widget) return Widget_List;

   procedure Append (List : in out Widget_List;
                     W    : in     Widget_List);

   procedure Append (List : in out Widget_List;
                     W    : in     Widget);



   function Xt_Create_Widget
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;
         
   function Xt_Create_Widget
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;
         

   function Xt_Create_Popup_Shell
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;
 
   function Xt_Create_Popup_Shell
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;
 

   function Xt_Create_Managed_Widget
     (Name    : in String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;

   function Xt_Create_Managed_Widget
     (Name    : in Xt_N_Resource_String;
      Class   : in Widget_Class;
      Parent  : in Widget;
      Args    : in Arg_List := Null_Arg_List)
      return Widget;

   -- -------------------------------------------------------------------------
   --
   -- XtInitializeWidgetClass
   --
   procedure Xt_Initialize_Widget_Class (Object_Class : in Widget_Class);


   procedure Xt_Manage_Children (Children : in Widget_List_Type);

   procedure Xt_Unmanage_Children (Children : in Widget_List_Type);

   procedure Xt_Manage_Children (Children : in Widget_List);

   procedure Xt_Unmanage_Children (Children : in Widget_List);

   procedure Xt_Manage_Child (Child : in Widget);

   procedure Xt_Unmanage_Child (Child : in Widget);



   procedure Xt_Map_Widget (W : in Widget);

   procedure Xt_Unmap_Widget (W : in Widget);

   procedure Xt_Realize_Widget (W : in Widget);

   procedure Xt_Unrealize_Widget (W : in Widget);

   

   procedure Xt_Destroy_Widget (W : in Widget);


   procedure Xt_Configure_Widget
     (W             : in Widget;
      X, Y          : in X_Lib.Position;
      Width, Height : in X_Lib.Dimension;
      Border_Width  : in X_Lib.Dimension);

   procedure Xt_Move_Widget
     (W    : in Widget;
      X, Y : in X_Lib.Position);

   procedure Xt_Resize_Widget
     (W             : in Widget;
      Width, Height : in X_Lib.Dimension;
      Border_Width  : in X_Lib.Dimension);

   procedure Xt_Resize_Window (W : in Widget);


   procedure Xt_Set_Values (W    : in Widget;
                            Args : in Arg_List);


   procedure Xt_Get_Values  (W    : in Widget;
                             Args : in Arg_List);


   procedure Xt_Set_Sensitive (W          : in Widget;
                               Sensitive  : in Boolean);


   procedure Xt_Set_Mapped_When_Managed
     (W      : in Widget;
      Mapped : in Boolean);


   function Xt_Name_To_Widget
     (Reference : in Widget;
      Name      : in String)
      return Widget;

   function Xt_Name_To_Widget
     (Reference : in Widget;
      Name      : in Xt_N_Resource_String)
      return Widget;


-- ----------------------------------------------------------------------------
--
--                   P O P U P S
--

   type Xt_Grab_Kind is (Grab_None,
                         Grab_Nonexclusive,
                         Grab_Exclusive);

   
   procedure Xt_Popup  (Popup_Shell : in Widget;
                        Grab_Kind   : in Xt_Grab_Kind);


   procedure Xt_Popdown (Popup_Shell : in Widget);


   procedure Xt_Add_Grab
     (W             : in Widget;
      Exclusive     : in Boolean;
      Spring_Loaded : in Boolean);



   -- -------------------------------------------------------------------------
   --
   --           Q U E R I E S
   --
             
   function Xt_Class (W : in Widget) return Widget_Class;

   function Xt_Superclass (W : in Widget) return Widget_Class;
   
   function Xt_Is_Subclass (W     : in Widget;
                            Class : in Widget_Class) return Boolean;

   function Xt_Is_Composite (W : in Widget) return Boolean;

   function Xt_Is_Realized (W : in Widget) return Boolean;

   function Xt_Is_Managed (W : in Widget) return Boolean;
                                      
   function Xt_Has_Callbacks
     (W    : in Widget;             
      Kind : in Xt_N_Resource_String)
      return Xt_Callback_Status;

   function Xt_Name (Object : in Widget) return String;
   function Xt_Name (Object : in Widget) return Xt_N_Resource_String;

   function Xt_Parent (W : in Widget) return Widget;

   function Xt_Display (W : in Widget) return X_Lib.Display_Pointer;
   function Xt_Display_Of_Object (Object : in Widget) return X_Lib.Display_Pointer;

   function Xt_Screen (W : in Widget) return X_Lib.Screen_Pointer;
   function Xt_Screen_Of_Object (Object : in Widget) return X_Lib.Screen_Pointer;

   function Xt_Window (W : in Widget) return X_Lib.Window_ID;
   function Xt_Window_Of_Object (Object : in Widget) return X_Lib.Window_ID;

   function Xt_Window_To_Widget (Display : in X_Lib.Display_Pointer;
                                 Window  : in X_Lib.Window_ID) return Widget;

   -- set the value of the WM_COLORMAP_WINDOWS property
   --
   procedure Xt_Set_WM_Colormap_Windows
     (W       : in Widget;
      List    : in Widget_List_Type);


   -- associate the specified drawable with the widget W
   -- future calls to Xt_Window_To_Widget (Display, Drawable) will return
   -- the specified widget W
   --
   procedure Xt_Register_Drawable
     (Display  : in X_Lib.Display_Pointer;
      Drawable : in X_Lib.Drawable_ID;
      W        : in Widget);

   procedure Xt_Unregister_Drawable
     (Display  : in X_Lib.Display_Pointer;
      Drawable : in X_Lib.Drawable_ID);



-- ----------------------------------------------------------------------------
--
--              F I L E   S E A R C H I N G
--
--
   -- type specifying the subsitution of %-preceded characters in search strings
   --
   type Substitution_Rec_Type is record
      Match        : Character;
      Substitution : X_Strings.X_String;
   end record;
   type Substitution_List_Type is
      array (Natural range <>) of aliased Substitution_Rec_Type;
   Null_Substitution_List : constant Substitution_List_Type
                          := (1 .. 0 => (Match        => Character'Val (0),
			                 Substitution => X_Strings.Null_X_String));

   type Xt_File_Predicate_Proc is
      access function (Filename : in X_Strings.X_String)
         return Xt_Boolean;
   pragma Convention (C, Xt_File_Predicate_Proc);
   Null_File_Predicate_Proc : constant Xt_File_Predicate_Proc := null;

   -- search for a file using the given path list (e.g. a colon-separated list),
   -- in which the string substitutions are performed. for every path the
   -- predicate function is called and the accepted path returned.
   -- a NULL_File_Predicate_Proc indicated to use the standard check procedure
   --
   function Xt_Find_File
     (Path         : in String;
      Substitution : in Substitution_List_Type := Null_Substitution_List;
      Predicate    : in Xt_File_Predicate_Proc := Null_File_Predicate_Proc)
      return String;


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
      return String;

       
-- ----------------------------------------------------------------------------
--
--              S E L E C T I O N S
--
--

   function Xt_App_Get_Selection_Timeout (App_Context : in Xt_App_Context)
      return Milliseconds;


   procedure Xt_App_Set_Selection_Timeout
     (App_Context : in Xt_App_Context;
      Timeout     : in Milliseconds);


   procedure Xt_Disown_Selection
     (W         : in Widget;
      Selection : in X_Lib.Atom;
      Timestamp : in X_Lib.Server_Time);


-- ----------------------------------------------------------------------------
--
--              T Y P E   C O N V E R T I O N
--
--

--     NOT YET IMPLEMENTED

--   procedure Xt_App_Set_Type_Converter
--     (From_Type    : in Xt_R_Resource_String;
--      To_Type      : in Xt_R_Resource_String;
--      Converter    : in Xt_Type_Converter;
--      Convert_Args : in Convert_Arg_List;
--      Cache_Type   : in Xt_Cache_Type;
--      Destructor   : in Xt_Destructor);

--   function Xt_Call_Converter
--     (Display   : in X_Lib.Display_Pointer;
--      Converter : in Xt_Type_Converter;
--            ...

--   procedure Xt_Convert
--     (W         : in Widget;
--      From_Type : in Xt_R_Resource_String;
--          ...

--   procedure Xt_Convert_And_Store
--     (W         : in Widget;
--      From_Type : in Xt_R_Resource_String;
--          ...


-- ----------------------------------------------------------------------------
--
--              E R R O R   H A N D L I N G
--
--

   procedure Xt_App_Error
     (App_Context : in Xt_App_Context;
      Message     : in String);


-- ----------------------------------------------------------------------------
--
--              E V E N T S
--

   type Interval_ID  is new X_Lib.XID;
   Null_Interval_ID : constant Interval_ID;

   type Work_Proc_ID is new X_Lib.XID;
   Null_Work_Proc_ID : constant Work_Proc_ID;

   type Input_ID is new X_Lib.XID;
   Null_Input_ID : constant Input_ID;


   -- -------------------------------------------------------------------------
   --
   -- T I M E O U T   P R O C
   --
   type Xt_Timer_Callback_Proc is
      access procedure (Closure : in     Xt_Pointer;
                        ID      : in out Interval_ID);
   pragma Convention (C, Xt_Timer_Callback_Proc);
   Null_Timer_Callback_Proc : constant Xt_Timer_Callback_Proc := null;


   function Xt_App_Add_Time_Out
     (App_Context  : in Xt_App_Context;
      Interval     : in Milliseconds;
      Callback     : in Xt_Timer_Callback_Proc;
      Client_Data  : in Xt_Pointer := Null_Xt_Pointer)
      return Interval_ID;

   procedure Xt_Remove_Time_Out (Timer : in Interval_ID);


   -- -------------------------------------------------------------------------
   --
   -- W O R K   P R O C
   --
   type Xt_Work_Proc is access function (Client_Data : in Xt_Pointer)
      return Xt_Boolean;

   pragma Convention (C, Xt_Work_Proc);
   Null_Work_Proc : constant Xt_Work_Proc := null;


   function Xt_App_Add_Work_Proc (App_Context  : in Xt_App_Context;
                                  Proc         : in Xt_Work_Proc;
                                  Client_Data  : in Xt_Pointer := Null_Xt_Pointer)
      return Work_Proc_ID;

   procedure Xt_Remove_Work_Proc (Id : in Work_Proc_ID);


   -- -------------------------------------------------------------------------
   --
   -- I N P U T   C A L L B A C K   P R O C
   --
   type Xt_Input_Callback_Proc is
      access procedure (Closure : in     Xt_Pointer;
                        Source  : in out Integer;
                        Id      : in out Input_ID);
   pragma Convention (C, Xt_Input_Callback_Proc);
   Null_Input_Callback_Proc : constant Xt_Input_Callback_Proc := null;


   function Xt_App_Add_Input
     (App_Context : in Xt_App_Context;
      Source      : in Integer;
      Condition   : in Xt_Pointer;
      Proc        : in Xt_Input_Callback_Proc;
      Closure     : in Xt_Pointer)
      return Input_Id;

   procedure Xt_Remove_Input (Id : in Input_ID);


-- UseX11R6 X11R6.3
   -- -------------------------------------------------------------------------
   --
   -- registration of a signal handler  (from X11R6)
   --
   type Signal_ID  is new X_Lib.XID;
   Null_Signal_ID : constant Signal_ID;

   type Xt_Signal_Callback_Proc is access procedure (Client_Data : in     Xt_Pointer;
                                                     ID          : in out Signal_ID);
   pragma Convention (C, Xt_Signal_Callback_Proc);
   Null_Signal_Callback_Proc : constant Xt_Signal_Callback_Proc := null;


   function Xt_App_Add_Signal (App_Context  : in Xt_App_Context;
                               Callback     : in Xt_Signal_Callback_Proc;
                               Client_Data  : in Xt_Pointer) return Signal_ID;

   procedure Xt_Remove_Signal (Signal : in Signal_ID);

   procedure Xt_Notice_Signal (Signal : in Signal_ID);

-- EndX11R6 X11R6.3


   -- -------------------------------------------------------------------------
   --
   -- EVENT HANDLING
   --

   type Xt_Event_Handler_Proc is access procedure (
                 W                 : in     Widget;
                 Closure           : in     Xt_Pointer;
                 Event             : in     X_Lib.X_Event;
                 Cont_To_Dispatch  : in out Xt_Boolean);
   pragma Convention (C, Xt_Event_Handler_Proc);
   Null_Event_Handler_Proc : constant Xt_Event_Handler_Proc := null;

   -- -------------------------------------------------------------------------
   --
   -- XtAddEventHandler
   --
   procedure Xt_Add_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer);


   -- -------------------------------------------------------------------------
   --
   -- XtRemoveEventHandler
   --
   procedure Xt_Remove_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer);


   -- -------------------------------------------------------------------------
   --
   -- XtAddRawEventHandler
   --
   procedure Xt_Add_Raw_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer);


   -- -------------------------------------------------------------------------
   --
   -- XtRemoveRawEventHandler
   --
   procedure Xt_Remove_Raw_Event_Handler
     (W           : in Widget;
      Ev_Mask     : in X_Lib.Event_Mask;
      Nonmaskable : in Boolean;
      Proc        : in Xt_Event_Handler_Proc;
      Closure     : in Xt_Pointer := Null_Xt_Pointer);


   procedure Xt_App_Next_Event (App_Context : in  Xt_App_Context;
                                Event       : out X_Lib.X_Event);

   procedure Xt_App_Peek_Event (App_Context : in  Xt_App_Context;
                                Event       : out X_Lib.X_Event);

   function Xt_Dispatch_Event (Event : in X_Lib.X_Event) return Boolean;
 

   function Xt_Build_Event_Mask (W : in Widget) return X_Lib.Event_Mask;


   -- return the last event passed to Xt_Dispatch_Event
   --        null if there has been no event
   --
   function Xt_Last_Event_Processed
     (Display : in X_Lib.Display_Pointer)
      return X_Lib.X_Event_Pointer;


   -- return the timestamp of the last event passed to Xt_Dispatch_Event
   --        Server_Time (0) if there has been no event
   --
   function Xt_Last_Timestamp_Processed
     (Display : in X_Lib.Display_Pointer)
      return X_Lib.Server_Time;


-- ----------------------------------------------------------------------------
--
--             T R A N S L A T I O N  and  A C C E L E R A T O R
--


   type Xt_Translations is private;
   Null_Translations : constant Xt_Translations;

   type Xt_Accelerators is private;
   Null_Accelerators : constant Xt_Accelerators;

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Accelerators);

   procedure Append_Set (List  : in out Arg_List;
                         Name  : in     Xt_N_Resource_String;
                         Value : in     Xt_Translations);



   function  Xt_Parse_Translation_Table (Table : in String) return Xt_Translations;

   procedure Xt_Augment_Translations
     (W            : in Widget;
      Translations : in Xt_Translations);

   procedure Xt_Override_Translations
     (W            : in Widget;
      Translations : in Xt_Translations);

   function  Xt_Parse_Accelerator_Table (Table : in String) return Xt_Accelerators;

   procedure Xt_Install_Accelerators (Destination : in Widget;
                                      Source      : in Widget);


-- ----------------------------------------------------------------------------
--
--  Graphics Context
--

   subtype Xt_GC_Mask is X_Lib.X_GC_Valuemask;

   function Xt_Allocate_GC
     (Object         : in Widget;
      Depth          : in X_Lib.Color_Depth;
      Value_Mask     : in Xt_GC_Mask;
      Values         : in X_Lib.X_GC_Values;
      Dynamic_Mask   : in Xt_GC_Mask;
      Dont_Care_Mask : in Xt_GC_Mask) return X_Lib.GC_Pointer;


   function Xt_Get_GC
     (W          : in Widget;
      Value_Mask : in Xt_GC_Mask;
      Values     : in X_Lib.X_GC_Values) return X_Lib.GC_Pointer;
      

   procedure Xt_Release_GC
     (W  : in Widget;
      GC : in X_Lib.GC_Pointer);


-- ----------------------------------------------------------------------------
--
--              K E Y B O A R D
--
--

   procedure Xt_Convert_Case
     (Display : in     X_Lib.Display_Pointer;
      Keysym  : in     X_Lib.Key_Sym_ID;
      Lower   :    out X_Lib.Key_Sym_ID;
      Upper   :    out X_Lib.Key_Sym_ID);


   procedure Xt_Get_Action_Keysym
     (Event  : in     X_Lib.X_Event_Pointer;
      Keysym :    out X_Lib.Key_Sym_ID;
      Mods   :    out X_Lib.Modifiers_Mask_Type);

--   function Xt_Get_Keysym_Table
--     (Display : in X_Lib.Display_Pointer;
--           ...

   function Xt_Get_Multi_Click_Time
     (Display : in X_Lib.Display_Pointer)
      return Milliseconds;  -- in the Intrinsics, it is an int!


   procedure Xt_Set_Multi_Click_Time
     (Display    : in X_Lib.Display_Pointer;
      Click_Time : in Milliseconds);


   procedure Xt_Set_Keyboard_Focus
     (Subtree    : in Widget;
      Descendent : in Widget);
 

   function Xt_Get_Keyboard_Focus_Widget
     (W : in Widget)
     return Widget;



-- UseX11R6 X11R6.3
-- ----------------------------------------------------------------------------
--
-- T H R E A D S
--
   Threads_Not_Supported : exception;

   procedure Xt_Toolkit_Thread_Initialize;

   procedure Xt_App_Lock (App_Context : in Xt_App_Context);
   procedure Xt_App_Unlock (App_Context : in Xt_App_Context);
   
   procedure Xt_Process_Lock;
   procedure Xt_Process_Unlock;


   procedure Xt_App_Set_Exit_Flag (App_Context : in Xt_App_Context);
   function  Xt_App_Get_Exit_Flag (App_Context : in Xt_App_Context)
      return Boolean;


-- EndX11R6 X11R6.3


-- ----------------------------------------------------------------------------
--
--        M I S C E L L A N E O U S    R O U T I N E S
--

   procedure Xt_Add_Exposure_To_Region
     (Event : in X_Lib.X_Event_Pointer;
      Reg   : in X_Lib.Region);


   procedure Xt_Translate_Coords
     (W          : in     Widget;
      X,
      Y          : in     X_Lib.Position;
      X_Rel_Root,
      Y_Rel_Root :    out X_Lib.Position);


-- ----------------------------------------------------------------------------
--
--
--

   type Xt_Language_Proc is access
      function (App_Context : in Xt_App_Context;
                String      : in X_Strings.X_String;
                Client_Data : in Xt_Pointer)
         return X_Strings.X_String;

   Null_Xt_Language_Proc : constant Xt_Language_Proc := null;

   function Xt_Set_Language_Proc
     (App_Context : in Xt_App_Context   := Null_Xt_App_Context;
      Proc        : in Xt_Language_Proc := Null_Xt_Language_Proc;
      Client_Data : in Xt_Pointer       := Null_Xt_Pointer)
      return Xt_Language_Proc;

   --  same as above, but return value is ignored
   --
   procedure Xt_Set_Language_Proc
     (App_Context : in Xt_App_Context   := Null_Xt_App_Context;
      Proc        : in Xt_Language_Proc := Null_Xt_Language_Proc;
      Client_Data : in Xt_Pointer       := Null_Xt_Pointer);


-- ----------------------------------------------------------------------------
--
--        T Y P E   C O N V E R S I O N   R O U T I N E S
--

   function To_Address    (Source : in Widget)         return System.Address;
   function To_Address    (Source : in Xt_Pointer)     return System.Address;
   function To_Address    (Source : in Xt_Resource_String) return System.Address;
   function To_Address    (Source : in X_Strings.X_String) return System.Address;

   function To_Xt_Pointer (Source : in System.Address) return Xt_Pointer;
   function To_Xt_Pointer (Source : in Widget)         return Xt_Pointer;
   function To_Xt_Pointer (Source : in Integer)        return Xt_Pointer;
   function To_Xt_Pointer (Source : in Xt_Resource_String) return Xt_Pointer;
   function To_Xt_Pointer (Source : in X_Strings.X_String) return Xt_Pointer;

   function To_Widget     (Source : in Xt_Pointer)     return Widget;
   function To_Widget     (Source : in System.Address) return Widget;

   function To_X_String   (Source : in Xt_Pointer)     return X_Strings.X_String;


-- ----------------------------------------------------------------------------
--
-- resource strings
--

   -- -------------------------------------------------------------------------
   --
   -- resource strings of core intrinsics
   --
   Xt_N_Accelerators        : constant Xt_N_Resource_String;
   Xt_N_Ancestor_Sensitive  : constant Xt_N_Resource_String;
   Xt_N_Background          : constant Xt_N_Resource_String;
   Xt_N_Background_Pixmap   : constant Xt_N_Resource_String;
   Xt_N_Border_Color        : constant Xt_N_Resource_String;
   Xt_N_Border_Pixmap       : constant Xt_N_Resource_String;
   Xt_N_Border_Width        : constant Xt_N_Resource_String;
   Xt_N_Colormap            : constant Xt_N_Resource_String;
   Xt_N_Depth               : constant Xt_N_Resource_String;
   Xt_N_Destroy_Callback    : constant Xt_N_Resource_String;
   Xt_N_Height              : constant Xt_N_Resource_String;
   Xt_N_Initial_Resources_Persistent : constant Xt_N_Resource_String;
   Xt_N_Mapped_When_Managed : constant Xt_N_Resource_String;
   Xt_N_Screen              : constant Xt_N_Resource_String;
   Xt_N_Sensitive           : constant Xt_N_Resource_String;
   Xt_N_Translations        : constant Xt_N_Resource_String;
   Xt_N_Width               : constant Xt_N_Resource_String;
   Xt_N_X                   : constant Xt_N_Resource_String;
   Xt_N_Y                   : constant Xt_N_Resource_String;

   -- -------------------------------------------------------------------------
   --
   -- resource strings of composites
   --
   Xt_N_Children            : constant Xt_N_Resource_String;
   Xt_N_Insert_Position     : constant Xt_N_Resource_String;
   Xt_N_Num_Children        : constant Xt_N_Resource_String;

   type Xt_Order_Proc is access
      function (W : in Widget) return Cardinal;
   pragma Convention (C, Xt_Order_Proc);

   procedure Append_Set (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value : in	Xt_Order_Proc);

   procedure Append_Get (List  : in out Arg_List;
			 Name  : in	Xt_N_Resource_String;
			 Value :    out Xt_Order_Proc);
   pragma Convention (C, Append_Get);
 

   -- -------------------------------------------------------------------------
   --
   -- resource strings of dont know what
   --
   Xt_N_Border              : constant Xt_N_Resource_String;
   Xt_N_Bitmap              : constant Xt_N_Resource_String;
   Xt_N_Callback            : constant Xt_N_Resource_String;
   Xt_N_Edit_Type           : constant Xt_N_Resource_String;
   Xt_N_File                : constant Xt_N_Resource_String;
   Xt_N_Font                : constant Xt_N_Resource_String;
   Xt_N_Font_Set            : constant Xt_N_Resource_String;
   Xt_N_Force_Bars          : constant Xt_N_Resource_String;
   Xt_N_Foreground          : constant Xt_N_Resource_String;
   Xt_N_Function            : constant Xt_N_Resource_String;
   Xt_N_Highlight           : constant Xt_N_Resource_String;
   Xt_N_Index               : constant Xt_N_Resource_String;
   Xt_N_Inner_Height        : constant Xt_N_Resource_String;
   Xt_N_Inner_Width         : constant Xt_N_Resource_String;
   Xt_N_Inner_Window        : constant Xt_N_Resource_String;
   Xt_N_Internal_Height     : constant Xt_N_Resource_String;
   Xt_N_Internal_Width      : constant Xt_N_Resource_String;
   Xt_N_Jump_Proc           : constant Xt_N_Resource_String;
   Xt_N_Justify             : constant Xt_N_Resource_String;
   Xt_N_Length              : constant Xt_N_Resource_String;
   Xt_N_Lower_Right         : constant Xt_N_Resource_String;
   Xt_N_Menu_Entry          : constant Xt_N_Resource_String;
   Xt_N_Name                : constant Xt_N_Resource_String;
   Xt_N_Notify              : constant Xt_N_Resource_String;
   Xt_N_Orientation         : constant Xt_N_Resource_String;
   Xt_N_Parameter           : constant Xt_N_Resource_String;
   Xt_N_Pixmap              : constant Xt_N_Resource_String;
   Xt_N_Resize              : constant Xt_N_Resource_String;
   Xt_N_Reverse_Video       : constant Xt_N_Resource_String;
   Xt_N_Scroll_D_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Scroll_H_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Scroll_L_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Scroll_Proc         : constant Xt_N_Resource_String;
   Xt_N_Scroll_R_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Scroll_U_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Scroll_V_Cursor     : constant Xt_N_Resource_String;
   Xt_N_Selection           : constant Xt_N_Resource_String;
   Xt_N_Selection_Array     : constant Xt_N_Resource_String;
   Xt_N_Shown               : constant Xt_N_Resource_String;
   Xt_N_Space               : constant Xt_N_Resource_String;
   Xt_N_String              : constant Xt_N_Resource_String;
   Xt_N_Text_Options        : constant Xt_N_Resource_String;
   Xt_N_Text_Sink           : constant Xt_N_Resource_String;
   Xt_N_Text_Source         : constant Xt_N_Resource_String;
   Xt_N_Thickness           : constant Xt_N_Resource_String;
   Xt_N_Thumb               : constant Xt_N_Resource_String;
   Xt_N_Thumb_Proc          : constant Xt_N_Resource_String;
   Xt_N_Top                 : constant Xt_N_Resource_String;
   Xt_N_Update              : constant Xt_N_Resource_String;
   Xt_N_Use_Bottom          : constant Xt_N_Resource_String;
   Xt_N_Use_Right           : constant Xt_N_Resource_String;
   Xt_N_Value               : constant Xt_N_Resource_String;
   Xt_N_Window              : constant Xt_N_Resource_String;


   -- -------------------------------------------------------------------------
   --
   -- resource strings for resources
   --
   Xt_R_Accelerator_Table   : constant Xt_R_Resource_String;
   Xt_R_Atom                : constant Xt_R_Resource_String;
   Xt_R_Bitmap              : constant Xt_R_Resource_String;
   Xt_R_Bool                : constant Xt_R_Resource_String;
   Xt_R_Boolean             : constant Xt_R_Resource_String;
   Xt_R_Callback            : constant Xt_R_Resource_String;
   Xt_R_Call_Proc           : constant Xt_R_Resource_String;
   Xt_R_Cardinal            : constant Xt_R_Resource_String;
   Xt_R_Color               : constant Xt_R_Resource_String;
   Xt_R_Colormap            : constant Xt_R_Resource_String;
-- UseX11R6 X11R6.3
   Xt_R_Command_Arg_Array   : constant Xt_R_Resource_String;
-- EndX11R6 X11R6.3
   Xt_R_Cursor              : constant Xt_R_Resource_String;
   Xt_R_Dimension           : constant Xt_R_Resource_String;
-- UseX11R6 X11R6.3
   Xt_R_Directory_String    : constant Xt_R_Resource_String;
-- EndX11R6 X11R6.3
   Xt_R_Display             : constant Xt_R_Resource_String;
   Xt_R_Edit_Mode           : constant Xt_R_Resource_String;
   Xt_R_Enum                : constant Xt_R_Resource_String;
-- UseX11R6 X11R6.3
   Xt_R_Environment_Array   : constant Xt_R_Resource_String;
-- EndX11R6 X11R6.3
   Xt_R_File                : constant Xt_R_Resource_String;
   Xt_R_Float               : constant Xt_R_Resource_String;
   Xt_R_Font                : constant Xt_R_Resource_String;
   Xt_R_Font_Set            : constant Xt_R_Resource_String;
   Xt_R_Font_Struct         : constant Xt_R_Resource_String;
   Xt_R_Function            : constant Xt_R_Resource_String;
   Xt_R_Geometry            : constant Xt_R_Resource_String;
   Xt_R_Gravity             : constant Xt_R_Resource_String;
   Xt_R_Immediate           : constant Xt_R_Resource_String;
   Xt_R_Initial_State       : constant Xt_R_Resource_String;
   Xt_R_Int                 : constant Xt_R_Resource_String;
   Xt_R_Justify             : constant Xt_R_Resource_String;
   Xt_R_Object              : constant Xt_R_Resource_String;
   Xt_R_Orientation         : constant Xt_R_Resource_String;
   Xt_R_Pixel               : constant Xt_R_Resource_String;
   Xt_R_Pixmap              : constant Xt_R_Resource_String;
   Xt_R_Pointer             : constant Xt_R_Resource_String;
   Xt_R_Position            : constant Xt_R_Resource_String;
-- UseX11R6 X11R6.3
   Xt_R_Restart_Style       : constant Xt_R_Resource_String;
-- EndX11R6 X11R6.3
   Xt_R_Screen              : constant Xt_R_Resource_String;
   Xt_R_Short               : constant Xt_R_Resource_String;
-- UseX11R6 X11R6.3
   Xt_R_Smc_Conn            : constant Xt_R_Resource_String;
-- EndX11R6 X11R6.3
   Xt_R_String              : constant Xt_R_Resource_String;
   Xt_R_String_Array        : constant Xt_R_Resource_String;
   Xt_R_String_Table        : constant Xt_R_Resource_String;
   Xt_R_Unsigned_Char       : constant Xt_R_Resource_String;
   Xt_R_Translation_Table   : constant Xt_R_Resource_String;
   Xt_R_Visual              : constant Xt_R_Resource_String;
   Xt_R_Widget              : constant Xt_R_Resource_String;
   Xt_R_Widget_Class        : constant Xt_R_Resource_String;
   Xt_R_Widget_List         : constant Xt_R_Resource_String;
   Xt_R_Window              : constant Xt_R_Resource_String;


private

   type Widget_Class is new System.Address;
   Null_Widget_Class : constant Widget_Class := Widget_Class (Null_Address);


   type Xt_App_Context is new System.Address;
   Null_Xt_App_Context : constant Xt_App_Context := Xt_App_Context (Null_Address);


   type Xt_Action_Hook_Id is new System.Address;
   Null_Xt_Action_Hook_Id : constant Xt_Action_Hook_Id := Xt_Action_Hook_Id (Null_Address);


   Null_Interval_ID  : constant Interval_ID  := Interval_ID (X_Lib.Null_XID);
   Null_Work_Proc_ID : constant Work_Proc_ID := Work_Proc_ID (X_Lib.Null_XID);
   Null_Input_ID     : constant Input_ID     := Input_ID (X_Lib.Null_XID);
-- UseX11R6 X11R6.3
   Null_Signal_ID    : constant Signal_ID    := Signal_ID (X_Lib.Null_XID);
-- EndX11R6 X11R6.3


   type Xt_Translations is new System.Address;
   Null_Translations : constant Xt_Translations := Xt_Translations (Null_Address);

   type Xt_Accelerators is new System.Address;
   Null_Accelerators : constant Xt_Accelerators := Xt_Accelerators (Null_Address);

   for Xt_Grab_Kind use (Grab_None         => 0,
                         Grab_Nonexclusive => 1,
                         Grab_Exclusive    => 2);
   for Xt_Grab_Kind'Size use Interfaces.C.int'Size;




   pragma Import (C, Xt_App_Add_Action_Hook, "XtAppAddActionHook");
   pragma Import (C, Xt_Remove_Action_Hook, "XtRemoveActionHook");

   pragma Import (C, Xt_Callback_None, "XtCallbackNone");
   pragma Import (C, Xt_Callback_Nonexclusive, "XtCallbackNonexclusive");
   pragma Import (C, Xt_Callback_Exclusive, "XtCallbackExclusive");
   pragma Import (C, Xt_Callback_Popdown, "XtCallbackPopdown");

   pragma Import (C, Xt_App_Get_Selection_Timeout, "XtAppGetSelectionTimeout");
   pragma Import (C, Xt_App_Set_Selection_Timeout, "XtAppSetSelectionTimeout");
   pragma Import (C, Xt_Disown_Selection, "XtDisownSelection");

   pragma Import (C, Xt_Toolkit_Initialize, "XtToolkitInitialize");
   pragma Import (C, Xt_App_Main_Loop, "XtAppMainLoop");

   pragma Import (C, Xt_App_Add_Time_Out, "XtAppAddTimeOut");
   pragma Import (C, Xt_Remove_Time_Out, "XtRemoveTimeOut");

   pragma Import (C, Xt_App_Add_Work_Proc, "XtAppAddWorkProc");
   pragma Import (C, Xt_Remove_Work_Proc, "XtRemoveWorkProc");

   pragma Import (C, Xt_App_Add_Input, "XtAppAddInput");
   pragma Import (C, Xt_Remove_Input, "XtRemoveInput");

-- UseX11R6 X11R6.3
   pragma Import (C, Xt_App_Add_Signal, "XtAppAddSignal");
   pragma Import (C, Xt_Remove_Signal,  "XtRemoveSignal");
   pragma Import (C, Xt_Notice_Signal, "XtNoticeSignal");
-- EndX11R6 X11R6.3

   pragma Import (C, Xt_Build_Event_Mask, "XtBuildEventMask");
   pragma Import (C, Xt_Create_Application_Context, "XtCreateApplicationContext");
   pragma Import (C, Xt_Destroy_Application_Context, "XtDestroyApplicationContext");
   pragma Import (C, Xt_Display_To_Application_Context, "XtDisplayToApplicationContext");
   pragma Import (C, Xt_Widget_To_Application_Context, "XtWidgetToApplicationContext");

   pragma Import (C, Xt_Remove_All_Callbacks, "XtRemoveAllCallbacks");
   pragma Import (C, Xt_Initialize_Widget_Class, "XtInitializeWidgetClass");
   pragma Import (C, Xt_Manage_Child, "XtManageChild");
   pragma Import (C, Xt_Unmanage_Child, "XtUnmanageChild");
   pragma Import (C, Xt_Realize_Widget, "XtRealizeWidget");
   pragma Import (C, Xt_Unrealize_Widget, "XtUnrealizeWidget");
   pragma Import (C, Xt_Destroy_Widget, "XtDestroyWidget");
   pragma Import (C, Xt_Configure_Widget, "XtConfigureWidget");
   pragma Import (C, Xt_Move_Widget, "XtMoveWidget");
   pragma Import (C, Xt_Resize_Widget, "XtResizeWidget");
   pragma Import (C, Xt_Resize_Window, "XtResizeWindow");

   pragma Import (C, Xt_Popup, "XtPopup");
   pragma Import (C, Xt_Popdown, "XtPopdown");
   pragma Import (C, Xt_Class, "XtClass");
   pragma Import (C, Xt_Superclass, "XtSuperclass");

   pragma Import (C, Xt_Parent, "XtParent");
   pragma Import (C, Xt_Display, "XtDisplay");
   pragma Import (C, Xt_Display_Of_Object, "XtDisplayOfObject");
   pragma Import (C, Xt_Screen, "XtScreen");
   pragma Import (C, Xt_Screen_Of_Object, "XtScreenOfObject");
   pragma Import (C, Xt_Window, "XtWindow");
   pragma Import (C, Xt_Window_Of_Object, "XtWindowOfObject");
   pragma Import (C, Xt_Window_To_Widget, "XtWindowToWidget");
   pragma Import (C, Xt_Register_Drawable, "XtRegisterDrawable");
   pragma Import (C, Xt_Unregister_Drawable, "XtUnregisterDrawable");

   pragma Import (C, Xt_Close_Display, "XtCloseDisplay");
   pragma Import (C, Xt_Install_Accelerators, "XtInstallAccelerators");

   pragma Import (C, Xt_Release_GC, "XtReleaseGC");

   pragma Import (C, Xt_Convert_Case, "XtConvertCase");
   pragma Import (C, Xt_Get_Multi_Click_Time, "XtGetMultiClickTime");
   pragma Import (C, Xt_Set_Multi_Click_Time, "XtSetMultiClickTime");
   pragma Import (C, Xt_Set_Keyboard_Focus, "XtSetKeyboardFocus");
   pragma Import (C, Xt_Get_Keyboard_Focus_Widget, "XtGetKeyboardFocusWidget");

   pragma Import (C, Xt_Augment_Translations, "XtAugmentTranslations");
   pragma Import (C, Xt_Override_Translations, "XtOverrideTranslations");
-- UseX11R6 X11R6.3
   pragma Import (C, Xt_App_Lock, "XtAppLock");
   pragma Import (C, Xt_App_Unlock, "XtAppUnlock");
   pragma Import (C, Xt_Process_Lock, "XtProcessLock");
   pragma Import (C, Xt_Process_Unlock, "XtProcessUnlock");
   pragma Import (C, Xt_App_Set_Exit_Flag, "XtAppSetExitFlag");
-- EndX11R6 X11R6.3

   pragma Import (C, Xt_Add_Exposure_To_Region, "XtAddExposureToRegion");
   pragma Import (C, Xt_Translate_Coords, "XtTranslateCoords");

   pragma Import (C, Xt_Last_Event_Processed, "XtLastEventProcessed");
   pragma Import (C, Xt_Last_Timestamp_Processed, "XtLastTimestampProcessed");

   pragma Import (C, Xt_Set_Language_Proc, "XtSetLanguageProc");


   c_const_Core_Widget_Class             : Widget_Class;
   c_const_Composite_Widget_Class        : Widget_Class;
   c_const_Constraint_Widget_Class       : Widget_Class;

   pragma Import (C, c_const_Core_Widget_Class, "coreWidgetClass");
   pragma Import (C, c_const_Composite_Widget_Class, "compositeWidgetClass");
   pragma Import (C, c_const_Constraint_Widget_Class, "constraintWidgetClass");

   Core_Widget_Class             : constant Widget_Class := c_const_Core_Widget_Class;
   Composite_Widget_Class        : constant Widget_Class := c_const_Composite_Widget_Class;
   Constraint_Widget_Class       : constant Widget_Class := c_const_Constraint_Widget_Class;


-- ----------------------------------------------------------------------------
--
-- resource strings
--
   Xt_N_Accelerators        : constant Xt_N_Resource_String := To_Resource_String ("accelerators");
   Xt_N_Ancestor_Sensitive  : constant Xt_N_Resource_String := To_Resource_String ("ancestorSensitive");
   Xt_N_Background          : constant Xt_N_Resource_String := To_Resource_String ("background");
   Xt_N_Background_Pixmap   : constant Xt_N_Resource_String := To_Resource_String ("backgroundPixmap");
   Xt_N_Border_Color        : constant Xt_N_Resource_String := To_Resource_String ("borderColor");
   Xt_N_Border_Pixmap       : constant Xt_N_Resource_String := To_Resource_String ("borderPixmap");
   Xt_N_Border_Width        : constant Xt_N_Resource_String := To_Resource_String ("borderWidth");
   Xt_N_Colormap            : constant Xt_N_Resource_String := To_Resource_String ("colormap");
   Xt_N_Depth               : constant Xt_N_Resource_String := To_Resource_String ("depth");
   Xt_N_Destroy_Callback    : constant Xt_N_Resource_String := To_Resource_String ("destroyCallback");
   Xt_N_Height              : constant Xt_N_Resource_String := To_Resource_String ("height");
   Xt_N_Initial_Resources_Persistent : constant Xt_N_Resource_String := To_Resource_String ("initialResourcesPersistent");
   Xt_N_Mapped_When_Managed : constant Xt_N_Resource_String := To_Resource_String ("mappedWhenManaged");
   Xt_N_Screen              : constant Xt_N_Resource_String := To_Resource_String ("screen");
   Xt_N_Sensitive           : constant Xt_N_Resource_String := To_Resource_String ("sensitive");
   Xt_N_Translations        : constant Xt_N_Resource_String := To_Resource_String ("translations");
   Xt_N_Width               : constant Xt_N_Resource_String := To_Resource_String ("width");
   Xt_N_X                   : constant Xt_N_Resource_String := To_Resource_String ("x"); 
   Xt_N_Y                   : constant Xt_N_Resource_String := To_Resource_String ("y");


   Xt_N_Children            : constant Xt_N_Resource_String := To_Resource_String ("children");
   Xt_N_Insert_Position     : constant Xt_N_Resource_String := To_Resource_String ("insertPosition");
   Xt_N_Num_Children        : constant Xt_N_Resource_String := To_Resource_String ("numChildren");


   Xt_N_Border              : constant Xt_N_Resource_String := To_Resource_String ("border");
   Xt_N_Bitmap              : constant Xt_N_Resource_String := To_Resource_String ("bitmap");
   Xt_N_Callback            : constant Xt_N_Resource_String := To_Resource_String ("callback");
   Xt_N_Edit_Type           : constant Xt_N_Resource_String := To_Resource_String ("editType");
   Xt_N_File                : constant Xt_N_Resource_String := To_Resource_String ("file");
   Xt_N_Font                : constant Xt_N_Resource_String := To_Resource_String ("font");
   Xt_N_Font_Set            : constant Xt_N_Resource_String := To_Resource_String ("fontSet");
   Xt_N_Force_Bars          : constant Xt_N_Resource_String := To_Resource_String ("forceBars");
   Xt_N_Foreground          : constant Xt_N_Resource_String := To_Resource_String ("foreground");
   Xt_N_Function            : constant Xt_N_Resource_String := To_Resource_String ("function");
   Xt_N_Highlight           : constant Xt_N_Resource_String := To_Resource_String ("highlight");
   Xt_N_Index               : constant Xt_N_Resource_String := To_Resource_String ("index");
   Xt_N_Inner_Height        : constant Xt_N_Resource_String := To_Resource_String ("innerHeight");
   Xt_N_Inner_Width         : constant Xt_N_Resource_String := To_Resource_String ("innerWidth");
   Xt_N_Inner_Window        : constant Xt_N_Resource_String := To_Resource_String ("innerWindow");
   Xt_N_Internal_Height     : constant Xt_N_Resource_String := To_Resource_String ("internalHeight");
   Xt_N_Internal_Width      : constant Xt_N_Resource_String := To_Resource_String ("internalWidth");
   Xt_N_Jump_Proc           : constant Xt_N_Resource_String := To_Resource_String ("jumpProc");
   Xt_N_Justify             : constant Xt_N_Resource_String := To_Resource_String ("justify");
   Xt_N_Length              : constant Xt_N_Resource_String := To_Resource_String ("length");
   Xt_N_Lower_Right         : constant Xt_N_Resource_String := To_Resource_String ("lowerRight");
   Xt_N_Menu_Entry          : constant Xt_N_Resource_String := To_Resource_String ("menuEntry");
   Xt_N_Name                : constant Xt_N_Resource_String := To_Resource_String ("name");
   Xt_N_Notify              : constant Xt_N_Resource_String := To_Resource_String ("notify");
   Xt_N_Orientation         : constant Xt_N_Resource_String := To_Resource_String ("orientation");
   Xt_N_Parameter           : constant Xt_N_Resource_String := To_Resource_String ("parameter");
   Xt_N_Pixmap              : constant Xt_N_Resource_String := To_Resource_String ("pixmap");
   Xt_N_Resize              : constant Xt_N_Resource_String := To_Resource_String ("resize");
   Xt_N_Reverse_Video       : constant Xt_N_Resource_String := To_Resource_String ("reverseVideo");
   Xt_N_Scroll_D_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollDCursor");
   Xt_N_Scroll_H_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollHCursor");
   Xt_N_Scroll_L_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollLCursor");
   Xt_N_Scroll_Proc         : constant Xt_N_Resource_String := To_Resource_String ("scrollProc");
   Xt_N_Scroll_R_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollRCursor");
   Xt_N_Scroll_U_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollUCursor");
   Xt_N_Scroll_V_Cursor     : constant Xt_N_Resource_String := To_Resource_String ("scrollVCursor");
   Xt_N_Selection           : constant Xt_N_Resource_String := To_Resource_String ("selection");
   Xt_N_Selection_Array     : constant Xt_N_Resource_String := To_Resource_String ("selectionArray");
   Xt_N_Shown               : constant Xt_N_Resource_String := To_Resource_String ("shown");
   Xt_N_Space               : constant Xt_N_Resource_String := To_Resource_String ("space");
   Xt_N_String              : constant Xt_N_Resource_String := To_Resource_String ("string");
   Xt_N_Text_Options        : constant Xt_N_Resource_String := To_Resource_String ("textOptions");
   Xt_N_Text_Sink           : constant Xt_N_Resource_String := To_Resource_String ("textSink");
   Xt_N_Text_Source         : constant Xt_N_Resource_String := To_Resource_String ("textSource");
   Xt_N_Thickness           : constant Xt_N_Resource_String := To_Resource_String ("thickness");
   Xt_N_Thumb               : constant Xt_N_Resource_String := To_Resource_String ("thumb");
   Xt_N_Thumb_Proc          : constant Xt_N_Resource_String := To_Resource_String ("thumbProc");
   Xt_N_Top                 : constant Xt_N_Resource_String := To_Resource_String ("top");
   Xt_N_Update              : constant Xt_N_Resource_String := To_Resource_String ("update");
   Xt_N_Use_Bottom          : constant Xt_N_Resource_String := To_Resource_String ("useBottom");
   Xt_N_Use_Right           : constant Xt_N_Resource_String := To_Resource_String ("useRight");
   Xt_N_Value               : constant Xt_N_Resource_String := To_Resource_String ("value");
   Xt_N_Window              : constant Xt_N_Resource_String := To_Resource_String ("window");

   -- -------------------------------------------------------------------------
   --
   -- resource strings for resources
   --
   Xt_R_Accelerator_Table   : constant Xt_R_Resource_String := To_Resource_String ("AcceleratorTable");
   Xt_R_Atom                : constant Xt_R_Resource_String := To_Resource_String ("Atom");
   Xt_R_Bitmap              : constant Xt_R_Resource_String := To_Resource_String ("Bitmap");
   Xt_R_Bool                : constant Xt_R_Resource_String := To_Resource_String ("Bool");
   Xt_R_Boolean             : constant Xt_R_Resource_String := To_Resource_String ("Boolean");
   Xt_R_Callback            : constant Xt_R_Resource_String := To_Resource_String ("Callback");
   Xt_R_Call_Proc           : constant Xt_R_Resource_String := To_Resource_String ("CallProc");
   Xt_R_Cardinal            : constant Xt_R_Resource_String := To_Resource_String ("Cardinal");
   Xt_R_Color               : constant Xt_R_Resource_String := To_Resource_String ("Color");
   Xt_R_Colormap            : constant Xt_R_Resource_String := To_Resource_String ("Colormap");
-- UseX11R6 X11R6.3
   Xt_R_Command_Arg_Array   : constant Xt_R_Resource_String := To_Resource_String ("CommandArgArray");
-- EndX11R6 X11R6.3
   Xt_R_Cursor              : constant Xt_R_Resource_String := To_Resource_String ("Cursor");
   Xt_R_Dimension           : constant Xt_R_Resource_String := To_Resource_String ("Dimension");
-- UseX11R6 X11R6.3
   Xt_R_Directory_String    : constant Xt_R_Resource_String := To_Resource_String ("DirectoryString");
-- EndX11R6 X11R6.3
   Xt_R_Display             : constant Xt_R_Resource_String := To_Resource_String ("Display");
   Xt_R_Edit_Mode           : constant Xt_R_Resource_String := To_Resource_String ("EditMode");
   Xt_R_Enum                : constant Xt_R_Resource_String := To_Resource_String ("Enum");
-- UseX11R6 X11R6.3
   Xt_R_Environment_Array   : constant Xt_R_Resource_String := To_Resource_String ("EnvironmentArray");
-- EndX11R6 X11R6.3
   Xt_R_File                : constant Xt_R_Resource_String := To_Resource_String ("File");
   Xt_R_Float               : constant Xt_R_Resource_String := To_Resource_String ("Float");
   Xt_R_Font                : constant Xt_R_Resource_String := To_Resource_String ("Font");
   Xt_R_Font_Set            : constant Xt_R_Resource_String := To_Resource_String ("FontSet");
   Xt_R_Font_Struct         : constant Xt_R_Resource_String := To_Resource_String ("FontStruct");
   Xt_R_Function            : constant Xt_R_Resource_String := To_Resource_String ("Function");
   Xt_R_Geometry            : constant Xt_R_Resource_String := To_Resource_String ("Geometry");
   Xt_R_Gravity             : constant Xt_R_Resource_String := To_Resource_String ("Gravity");
   Xt_R_Immediate           : constant Xt_R_Resource_String := To_Resource_String ("Immediate");
   Xt_R_Initial_State       : constant Xt_R_Resource_String := To_Resource_String ("InitialState");
   Xt_R_Int                 : constant Xt_R_Resource_String := To_Resource_String ("Int");
   Xt_R_Justify             : constant Xt_R_Resource_String := To_Resource_String ("Justify");
   Xt_R_Object              : constant Xt_R_Resource_String := To_Resource_String ("Object");
   Xt_R_Orientation         : constant Xt_R_Resource_String := To_Resource_String ("Orientation");
   Xt_R_Pixel               : constant Xt_R_Resource_String := To_Resource_String ("Pixel");
   Xt_R_Pixmap              : constant Xt_R_Resource_String := To_Resource_String ("Pixmap");
   Xt_R_Pointer             : constant Xt_R_Resource_String := To_Resource_String ("Pointer");
   Xt_R_Position            : constant Xt_R_Resource_String := To_Resource_String ("Position");
-- UseX11R6 X11R6.3
   Xt_R_Restart_Style       : constant Xt_R_Resource_String := To_Resource_String ("RestartStyle");
-- EndX11R6 X11R6.3
   Xt_R_Screen              : constant Xt_R_Resource_String := To_Resource_String ("Screen");
   Xt_R_Short               : constant Xt_R_Resource_String := To_Resource_String ("Short");
-- UseX11R6 X11R6.3
   Xt_R_Smc_Conn            : constant Xt_R_Resource_String := To_Resource_String ("SmcConn");
-- EndX11R6 X11R6.3
   Xt_R_String              : constant Xt_R_Resource_String := To_Resource_String ("String");
   Xt_R_String_Array        : constant Xt_R_Resource_String := To_Resource_String ("StringArray");
   Xt_R_String_Table        : constant Xt_R_Resource_String := To_Resource_String ("StringTable");
   Xt_R_Unsigned_Char       : constant Xt_R_Resource_String := To_Resource_String ("UnsignedChar");
   Xt_R_Translation_Table   : constant Xt_R_Resource_String := To_Resource_String ("TranslationTable");
   Xt_R_Visual              : constant Xt_R_Resource_String := To_Resource_String ("Visual");
   Xt_R_Widget              : constant Xt_R_Resource_String := To_Resource_String ("Widget");
   Xt_R_Widget_Class        : constant Xt_R_Resource_String := To_Resource_String ("WidgetClass");
   Xt_R_Widget_List         : constant Xt_R_Resource_String := To_Resource_String ("WidgetList");
   Xt_R_Window              : constant Xt_R_Resource_String := To_Resource_String ("Window");

end X_Toolkit;
