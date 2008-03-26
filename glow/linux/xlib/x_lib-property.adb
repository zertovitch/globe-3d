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

with Ada.Unchecked_Conversion,
     Interfaces.C.Pointers,
     Interfaces.C.Strings,
     String_List_Conversion,
     X_Strings;
package body X_Lib.Property is

   package C renames interfaces.C;


   use type Interfaces.C.long;

   -- convertion routines from bitfields to integers and vice versa
   --
   type C_Size_Hints_Mask is new Interfaces.C.unsigned_long;
   type Size_Hints_Mask_Int is mod 2**10; -- Size_Hints_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (Size_Hints_Mask,
                                                         Size_Hints_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (Size_Hints_Mask_Int,
                                                         Size_Hints_Mask);

   function To_Long (Mask : in Size_Hints_Mask) return C_Size_Hints_Mask is
   begin
      return C_Size_Hints_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Size_Hints_Mask) return Size_Hints_Mask is
   begin
      return To_Mask_Type (Size_Hints_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   type C_WM_Hints_Mask is new Interfaces.C.unsigned_long;
   type WM_Hints_Mask_Int is mod 2**9; -- WM_Hints_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (WM_Hints_Mask,
                                                         WM_Hints_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (WM_Hints_Mask_Int,
                                                          WM_Hints_Mask);

   function To_Long (Mask : in WM_Hints_Mask) return C_WM_Hints_Mask is
   begin
      return C_WM_Hints_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_WM_Hints_Mask) return WM_Hints_Mask is
   begin
      return To_Mask_Type (WM_Hints_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   type C_Geometry_Valid_Mask is new Interfaces.C.unsigned_long;
   type Geometry_Valid_Mask_Int is mod 2**6; -- Geometry_Valid_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (Geometry_Valid_Mask,
                                                         Geometry_Valid_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (Geometry_Valid_Mask_Int,
                                                          Geometry_Valid_Mask);

   function To_Long (Mask : in WM_Hints_Mask) return C_Geometry_Valid_Mask is
   begin
      return C_Geometry_Valid_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Geometry_Valid_Mask) return Geometry_Valid_Mask is
   begin
      return To_Mask_Type (Geometry_Valid_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);



   -- C representation of type Text_Property_Type
   --
   type C_Text_Property_Type is record
      Value    : System.Address;
      Encoding : Atom;
      Format   : Data_Format_Type;
      NItems   : Interfaces.C.long;  -- normally this should be Long_Unsigned
   end record;
   pragma Convention (C, C_Text_Property_Type);

   -- to free the Value-Member of the above record
   procedure XFree (What : in System.Address);
   pragma Import (C, XFree, "XFree");


   package Bits_8_Ptr_Package is new
      Interfaces.C.Pointers (Natural, Bits_8_Type, Bits_8_Array_Type,
                             Bits_8_Type (0));
   function To_Bits_8_Pointer is
      new Ada.Unchecked_Conversion (System.Address, Bits_8_Ptr_Package.Pointer);
   function To_Address is
      new Ada.Unchecked_Conversion (Bits_8_Ptr_Package.Pointer, System.Address);

   package Bits_16_Ptr_Package is new
      Interfaces.C.Pointers (Natural, Bits_16_Type, Bits_16_Array_Type,
                             Bits_16_Type (0));
   function To_Bits_16_Pointer is
      new Ada.Unchecked_Conversion (System.Address, Bits_16_Ptr_Package.Pointer);
   function To_Address is
      new Ada.Unchecked_Conversion (Bits_16_Ptr_Package.Pointer, System.Address);

   package Bits_32_Ptr_Package is new
      Interfaces.C.Pointers (Natural, Bits_32_Type, Bits_32_Array_Type,
                             Bits_32_Type (0));
   function To_Bits_32_Pointer is
      new Ada.Unchecked_Conversion (System.Address, Bits_32_Ptr_Package.Pointer);
   function To_Address is
      new Ada.Unchecked_Conversion (Bits_32_Ptr_Package.Pointer, System.Address);


   function To_Text_Property_Type (C_Type : in C_Text_Property_Type)
      return Text_Property_Type is
      -- there is a trailing zero, which is not counted
      Return_Text_Property : Text_Property_Type (C_Type.Format, Integer
      (C_Type.NItems + 1));
   begin
      Return_Text_Property.Encoding := C_Type.Encoding;
      case C_Type.Format is
         when Invalid =>
	    null;
         when Bits_8 =>
	    Return_Text_Property.Value_8 (1 .. Integer (C_Type.NItems)) := Bits_8_Ptr_Package.Value
				           (To_Bits_8_Pointer (C_Type.Value),
				       Interfaces.C.ptrdiff_t (C_Type.NItems));
            Return_Text_Property.Value_8 (Integer (C_Type.NItems + 1)) := 0;
	 when Bits_16 =>
	    Return_Text_Property.Value_16 (1 .. Integer (C_Type.NItems)) := Bits_16_Ptr_Package.Value
				           (To_Bits_16_Pointer (C_Type.Value),
				       Interfaces.C.ptrdiff_t (C_Type.NItems));
            Return_Text_Property.Value_16 (Integer (C_Type.NItems + 1)) := 0;
	 when Bits_32 =>
	    Return_Text_Property.Value_32 (1 .. Integer (C_Type.NItems)) := Bits_32_Ptr_Package.Value
				           (To_Bits_32_Pointer (C_Type.Value),
				       Interfaces.C.ptrdiff_t (C_Type.NItems));
            Return_Text_Property.Value_32 (Integer (C_Type.NItems + 1)) := 0;
      end case;
      return Return_Text_Property;
   end To_Text_Property_Type;


   -- conversion routines to and from Text_Property_Type
   --
   function To_Text_Property (List : in String_List.Element_Access_List)
      return Text_Property_type is
      function XStringListToTextProperty
        (List     : in String_List_Conversion.Chars_Ptr_List_Type;
	 Count    : in Integer;
         Property : in System.Address)
	 return Status_Type;
      pragma Import (C, XStringListToTextProperty, "XStringListToTextProperty");
      C_Return_Property : C_Text_Property_Type;
      C_List            : String_List_Conversion.Chars_Ptr_List_Type :=
                             String_List_Conversion.To_Chars_Ptr_List (List);
   begin
      if XStringListToTextProperty (C_List,
                                    String_List.Length (List),
				    C_Return_Property'Address) = Error_Status then
         raise X_Error;
      end if;
      String_List_Conversion.Free (C_List,
                                   String_List_Conversion.Index_Type (String_List.Length (List)));
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Return_Property);
      begin
         XFree (C_Return_Property.Value);
         return Return_Property;
      end;
   end To_Text_Property;


   function To_Text_Property (List : in String)
      return Text_Property_type is
      function XStringListToTextProperty
        (List     : in System.Address;
	 Count    : in Integer;
         Property : in System.Address)
	 return Status_Type;
      pragma Import (C, XStringListToTextProperty, "XStringListToTextProperty");
      C_Return_Property : C_Text_Property_Type;
      List_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (List, Append_Nul => True);
      List_String_Address : System.Address := List_String'Address;
   begin
      if XStringListToTextProperty (List_String_Address'Address, 1,
                                    C_Return_Property'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Return_Property);
      begin
         XFree (C_Return_Property.Value);
         return Return_Property;
      end;
   end To_Text_Property;


   function To_String_List (Property : in Text_Property_Type)
      return String_List.Element_Access_List is
      function XTextPropertyToStringList
        (Text_Prop    : in System.Address;
	 List_Return  : in System.Address;
	 Count_Return : in System.Address)
	 return Status_Type;
      pragma Import (C, XTextPropertyToStringList, "XTextPropertyToStringList");

      procedure XFreeStringList (List : in String_List_Conversion.Chars_Ptr_List_Type);
      pragma Import (C, XFreeStringList, "XFreeStringList");

      C_Property  : C_Text_Property_Type;
      C_List      : String_List_Conversion.Chars_Ptr_List_Type;
      C_Count     : Integer;
      Return_List : String_List.Element_Access_List;
   begin
      case Property.Format is
         when Invalid =>
	    return String_List.Null_Element_Access_List;
         when Bits_8 =>
	    C_Property.Value := Property.Value_8'Address;
	 when Bits_16 =>
	    C_Property.Value := Property.Value_16'Address;
	 when Bits_32 =>
	    C_Property.Value := Property.Value_32'Address;
      end case;
      C_Property.Encoding := Property.Encoding;
      C_Property.Format   := Property.Format;
      -- the trailing zero is not counted
      C_Property.NItems   := Interfaces.C.long (Property.Length-1);
      if XTextPropertyToStringList (C_Property'Address,
                                    C_List'Address,
				    C_Count'Address) = Error_Status then
         raise X_Error;
      end if;
      Return_List := String_List_Conversion.To_String_Access_List (C_List,
                        String_List_Conversion.Index_Type (C_Count));
      XFreeStringList (C_List);
      return Return_List;
   end To_String_List;



   procedure X_Set_WM_Properties
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Window_Name  : in Text_Property_Type;
      Icon_Name    : in Text_Property_Type;
      Args         : in System.Address;
      Argc         : in Integer;
      Normal_Hints : access Size_Hints_Type;
      WM_Hints     : access WM_Hints_Type;
      Class_Hints  : access Class_Hint_Type)
   is

      procedure C_X_Set_WM_Properties
        (Display      : in Display_Pointer;
         W            : in Window_ID;
         Window_Name  : access C_Text_Property_Type;
         Icon_Name    : access C_Text_Property_Type;
         Args         : in System.Address;
         Argc         : in Integer;
         Normal_Hints : access Size_Hints_Type;
         WM_Hints     : access WM_Hints_Type;
         Class_Hints  : access Class_Hint_Type);

      pragma Import (C, C_X_Set_WM_Properties, "XSetWMProperties");

      C_Window_Name : aliased C_Text_Property_Type := (Value    => Window_Name.Value_8 (1)'address,
                                                       Encoding => Window_Name.Encoding,
                                                       Format   => Window_Name.Format,
                                                       NItems   => c.Long (Window_Name.Length));

      C_Icon_Name   : aliased C_Text_Property_Type := (Value    => Icon_Name.Value_8 (1)'address,
                                                       Encoding => Icon_Name.Encoding,
                                                       Format   => Icon_Name.Format,
                                                       NItems   => c.Long (Icon_Name.Length));

   begin

      C_X_Set_WM_Properties (Display,
                             W,
                             C_Window_Name'access,
                             C_Icon_Name'access,
                             Args,
                             Argc,
                             Normal_Hints,
                             WM_Hints,
                             Class_Hints);

   end;




   function XChangeProperty
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Data_Format : in Data_Format_Type;
      Mode        : in Change_Mode_Type;
      Data        : in System.Address;
      N_Elements  : in Integer)
      return Integer;
   pragma Import (C, XChangeProperty, "XChangeProperty");


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_8_Array_Type) is
      Ret_Value : Integer;
   begin
      Ret_Value := XChangeProperty (Display, W, Property, Data_Type,
                                    Bits_8, Mode, Data'Address, Data'Length);
   end X_Change_Property;


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_16_Array_Type) is
      Ret_Value : Integer;
   begin
      Ret_Value := XChangeProperty (Display, W, Property, Data_Type,
                                    Bits_16, Mode, Data'Address, Data'Length);
   end X_Change_Property;


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_32_Array_Type) is
      Ret_Value : Integer;
   begin
      Ret_Value := XChangeProperty (Display, W, Property, Data_Type,
                                    Bits_32, Mode, Data'Address, Data'Length);
   end X_Change_Property;


   function X_Get_Text_Property
     (Display  : in Display_Pointer;
      W        : in Window_ID;
      Property : in Atom)
      return Text_Property_Type is
      function XGetTextProperty
        (Display          : in Display_Pointer;
	 W                : in Window_ID;
	 Text_Prop_Return : in System.Address;
	 Property         : in Atom)
	 return Status_Type;
      pragma Import (C, XGetTextProperty, "XGetTextProperty");
      C_Prop : C_Text_Property_Type;
   begin
      if XGetTextProperty (Display, W, C_Prop'Address, Property) = Error_Status then
         raise X_Error;
      end if;
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Prop);
      begin
         XFree (C_Prop.Value);
         return Return_Property;
      end;
   end X_Get_Text_Property;


   procedure X_Set_Text_Property
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type;
      Property  : in Atom) is
      procedure XSetTextProperty
        (Display   : in Display_Pointer;
	 W         : in Window_ID;
	 Text_Prop : in System.Address;
	 Property  : in Atom);
      pragma Import (C, XSetTextProperty, "XSetTextProperty");
      C_Prop : C_Text_Property_Type;
   begin
      case Text_Prop.Format is
         when Invalid =>
	    -- at the moment just return
	    return;
         when Bits_8 =>
	    C_Prop.Value := Text_Prop.Value_8'Address;
	 when Bits_16 =>
	    C_Prop.Value := Text_Prop.Value_16'Address;
	 when Bits_32 =>
	    C_Prop.Value := Text_Prop.Value_32'Address;
      end case;
      C_Prop.Encoding := Text_Prop.Encoding;
      C_Prop.Format   := Text_Prop.Format;
      -- the trailing zero is not counted
      C_Prop.NItems   := Interfaces.C.long (Text_Prop.Length-1);
      XSetTextProperty (Display, W, C_Prop'Address, Property);
   end X_Set_Text_Property;


   function X_Get_Window_Property
     (Display       : in     Display_Pointer;
      W             : in     Window_ID;
      Property      : in     Atom;
      Long_Offset   : in     Natural;
      Long_Length   : in     Natural;
      Delete        : in     Boolean;
      Req_Type      : in     Atom)
      return Get_Window_Property_Return_Type is
      function XGetWindowProperty
        (Display       : in Display_Pointer;
         W             : in Window_ID;
         Property      : in Atom;
         Long_Offset   : in Natural;
         Long_Length   : in Natural;
         Delete        : in X_Boolean;
         Req_Type      : in Atom;
         Actual_Type   : in System.Address;
         Actual_Format : in System.Address;
         N_Items       : in System.Address;
         Bytes_After   : in System.Address;
         Prop          : in System.Address)
	 return Integer;
      pragma Import (C, XGetWindowProperty, "XGetWindowProperty");

      Bytes_After, N_Items   : Natural;
      Actual_Type   : Atom;
      Actual_Format : Data_Format_Type;
      Prop_Return   : System.Address;
   begin
      if XGetWindowProperty (Display, W, Property, Long_Offset, Long_Length,
                             To_X_Boolean (Delete), Req_Type,
			     Actual_Type'Address,
			     Actual_Format'Address,
			     N_Items'Address,
			     Bytes_After'Address,
			     Prop_Return'Address) /= 0 then
         raise X_Lib.X_Error;
      end if;
      declare
         -- the trailing NULL is not counted
         Return_Value : Get_Window_Property_Return_Type (Actual_Format, N_Items+1);
      begin
         Return_Value.Value_Type := Actual_Type;
         case Actual_Format is
	    when Invalid =>
	       null;
            when Bits_8 =>
	       Return_Value.Value_8 (1 .. N_Items) := Bits_8_Ptr_Package.Value
				           (To_Bits_8_Pointer (Prop_Return),
				       Interfaces.C.ptrdiff_t (N_Items));
               Return_Value.Value_8 (N_Items+1) := 0;
	    when Bits_16 =>
	       Return_Value.Value_16 (1 .. N_Items) := Bits_16_Ptr_Package.Value
				           (To_Bits_16_Pointer (Prop_Return),
				       Interfaces.C.ptrdiff_t (N_Items));
               Return_Value.Value_16 (N_Items+1) := 0;
	    when Bits_32 =>
	       Return_Value.Value_32 (1 .. N_Items) := Bits_32_Ptr_Package.Value
				           (To_Bits_32_Pointer (Prop_Return),
				       Interfaces.C.ptrdiff_t (N_Items));
               Return_Value.Value_32 (N_Items+1) := 0;
         end case;
	 Return_Value.Bytes_After := Bytes_After;
	 XFree (Prop_Return);
	 return Return_Value;
      end;
   end X_Get_Window_Property;


   -- set XA_WM_CLIENT_MACHINE property
   --
   procedure X_Set_WM_Client_Machine
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type) is
      procedure XSetWMClientMachine
        (Display   : in Display_Pointer;
	 W         : in Window_ID;
	 Text_Prop : in System.Address);
      pragma Import (C, XSetWMClientMachine, "XSetWMClientMachine");
      C_Prop : C_Text_Property_Type;
   begin
      C_Prop.Format   := Text_Prop.Format;
      C_Prop.Encoding := Text_Prop.Encoding;
      case Text_Prop.Format is
         when Invalid =>
	    return;
         when Bits_8 =>
	    C_Prop.Value := Text_Prop.Value_8'Address;
	 when Bits_16 =>
	    C_Prop.Value := Text_Prop.Value_16'Address;
	 when Bits_32 =>
	    C_Prop.Value := Text_Prop.Value_32'Address;
      end case;
      C_Prop.NItems := Interfaces.C.long (Text_Prop.Length) - 1; -- the trailing Null is not counted
      XSetWMClientMachine (Display, W, C_Prop'Address);
   end X_Set_WM_Client_Machine;


   function X_Get_WM_Client_Machine
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type is
      function XGetWMClientMachine
        (Display          : in Display_Pointer;
	 W                : in Window_ID;
	 Text_Prop_Return : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMClientMachine, "XGetWMClientMachine");
      C_Prop : C_Text_Property_Type;
   begin
      if XGetWMClientMachine (Display, W, C_Prop'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Prop);
      begin
         XFree (C_Prop.Value);
         return Return_Property;
      end;
   end X_Get_WM_Client_MAchine;


   -- set XA_WM_COLORMAP_WINDOWS property
   --
   procedure X_Set_WM_Colormap_Windows
     (Display          : in Display_Pointer;
      W                : in Window_ID;
      Colormap_Windows : in Window_ID_Array_Type) is

      function XSetWMColormapWindows
        (Display          : in Display_Pointer;
         W                : in Window_ID;
         Colormap_Windows : in System.Address;
         Count            : in Integer)
	 return Status_Type;
      pragma Import (C, XSetWMColormapWindows, "XSetWMColormapWindows");
   begin
      if XSetWMColormapWindows (Display, W, Colormap_Windows'Address,
                                Colormap_Windows'Length) = Error_Status then
         raise X_Error;
      end if;
   end X_Set_WM_Colormap_Windows;


   function X_Get_WM_Colormap_Windows
     (Display          : in Display_Pointer;
      W                : in Window_ID)
      return Window_ID_Array_Type is

      function XGetWMColormapWindows
        (Display          : in Display_Pointer;
         W                : in Window_ID;
	 Windows_Return   : in System.Address;
	 Count_Return     : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMColormapWindows, "XGetWMColormapWindows");

      Hook    : System.Address;
      Number  : Integer;
   begin
      if XGetWMColormapWindows (Display, W,
                                Hook'Address, Number'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         subtype This_Array_Type is Window_ID_Array_Type (1 .. Number);
	 type This_Array_Access_Type is access This_Array_Type;
         function To_This_Array_Access_Type is
	    new Ada.Unchecked_Conversion (System.Address, This_Array_Access_Type);

	 List : This_Array_Type;
      begin
         List := To_This_Array_Access_Type (Hook).all;
	 XFree (Hook);
	 return List;
      end;
   end X_Get_WM_Colormap_Windows;


   function X_Get_WM_Hints
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return WM_Hints_Type is
      type WM_Hints_Access_Type is access all WM_Hints_Type;
      function XGetWMHints
        (Display   : in Display_Pointer;
         W         : in Window_ID)
         return WM_Hints_Access_Type;
      pragma Import (C, XGetWMHints, "XGetWMHints");
      Return_Hints : WM_Hints_Access_Type;
   begin
      Return_Hints := XGetWMHints (Display, W);
      if Return_Hints = null then
         raise X_Error;
      else
         return Return_Hints.all;
      end if;
   end X_Get_WM_Hints;


   -- set XA_WM_ICON_NAME property
   --
   procedure X_Set_WM_Icon_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type) is
      procedure XSetWMIconName
        (Display   : in Display_Pointer;
	 W         : in Window_ID;
	 Text_Prop : in System.Address);
      pragma Import (C, XSetWMIconName, "XSetWMIconName");
      C_Prop : C_Text_Property_Type;
   begin
      C_Prop.Format   := Text_Prop.Format;
      C_Prop.Encoding := Text_Prop.Encoding;
      case Text_Prop.Format is
         when Invalid =>
	    return;
         when Bits_8 =>
	    C_Prop.Value := Text_Prop.Value_8'Address;
	 when Bits_16 =>
	    C_Prop.Value := Text_Prop.Value_16'Address;
	 when Bits_32 =>
	    C_Prop.Value := Text_Prop.Value_32'Address;
      end case;
      C_Prop.NItems := Interfaces.C.long (Text_Prop.Length) - 1; -- the trailing Null is not counted
      XSetWMIconName (Display, W, C_Prop'Address);
   end X_Set_WM_Icon_Name;


   function X_Get_WM_Icon_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type is
      function XGetWMIconName
        (Display          : in Display_Pointer;
         W                : in Window_ID;
	 Text_Prop_Return : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMIconName, "XGetWMIconName");
      C_Prop : C_Text_Property_Type;
   begin
      if XGetWMIconName (Display, W, C_Prop'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Prop);
      begin
         XFree (C_Prop.Value);
         return Return_Property;
      end;
   end X_Get_WM_Icon_Name;


   -- set XA_WM_NAME property
   --
   procedure X_Set_WM_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type) is
      procedure XSetWMName
        (Display   : in Display_Pointer;
	 W         : in Window_ID;
	 Text_Prop : in System.Address);
      pragma Import (C, XSetWMName, "XSetWMName");
      C_Prop : C_Text_Property_Type;
   begin
      C_Prop.Format   := Text_Prop.Format;
      C_Prop.Encoding := Text_Prop.Encoding;
      case Text_Prop.Format is
         when Invalid =>
	    return;
         when Bits_8 =>
	    C_Prop.Value := Text_Prop.Value_8'Address;
	 when Bits_16 =>
	    C_Prop.Value := Text_Prop.Value_16'Address;
	 when Bits_32 =>
	    C_Prop.Value := Text_Prop.Value_32'Address;
      end case;
      C_Prop.NItems := Interfaces.C.long (Text_Prop.Length) - 1; -- the trailing Null is not counted
      XSetWMName (Display, W, C_Prop'Address);
   end X_Set_WM_Name;


   function X_Get_WM_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type is
      function XGetWMName
        (Display          : in Display_Pointer;
         W                : in Window_ID;
	 Text_Prop_Return : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMName, "XGetWMName");
      C_Prop     : C_Text_Property_Type;
   begin
      if XGetWMName (Display, W, C_Prop'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         Return_Property : constant Text_Property_Type :=
                                    To_Text_Property_Type (C_Prop);
      begin
         XFree (C_Prop.Value);
         return Return_Property;
      end;
   end X_Get_WM_Name;


   procedure X_Get_WM_Normal_Hints
     (Display   : in     Display_Pointer;
      W         : in     Window_ID;
      Hints     :    out Size_Hints_Type;
      Supplied  :    out Size_Hints_Mask) is
      function XGetWMNormalHints
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Hints     : in System.Address;
         Supplied  : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMNormalHints, "XGetWMNormalHints");
      Supplied_Long : C_Size_Hints_Mask;
   begin
      if XGetWMNormalHints (Display, W,
                            Hints'Address, Supplied_Long'Address) = Error_Status then
         raise X_Error;
      end if;
      Supplied := To_Mask (Supplied_Long);
   end X_Get_WM_Normal_Hints;


   procedure X_Get_WM_Size_Hints
     (Display   : in     Display_Pointer;
      W         : in     Window_ID;
      Hints     :    out Size_Hints_Type;
      Property  : in     Atom;
      Supplied  :    out Size_Hints_Mask) is
      function XGetWMSizeHints
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Hints     : in System.Address;
	 Property  : in Atom;
         Supplied  : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetWMSizeHints, "XGetWMSizeHints");
      Supplied_Long : C_Size_Hints_Mask;
   begin
      if XGetWMSizeHints (Display, W,
                          Hints'Address,
			  Property, Supplied_Long'Address) = Error_Status then
         raise X_Error;
      end if;
      Supplied := To_Mask (Supplied_Long);
   end X_Get_WM_Size_Hints;


   function X_Get_Transient_For_Hint
     (Display     : in Display_Pointer;
      W           : in Window_ID)
      return Window_ID is
      function XGetTransientForHint
        (Display     : in Display_Pointer;
         W           : in Window_ID;
	 Prop_Window : in System.Address)
         return Status_Type;
      pragma Import (C, XGetTransientForHint, "XGetTransientForHint");
      Return_Window : Window_ID;
   begin
      if XGetTransientForHint (Display, W, Return_Window'Address) = Error_Status then
         raise X_Error;
      end if;
      return Return_Window;
   end X_Get_Transient_For_Hint;



   -- set XA_WM_PROTOCOLS property
   --
   procedure X_Set_WM_Protocols
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Protocols : in Atom_Array) is
      procedure XSetWMProtocols
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Protocols : in System.Address;
         Count     : in Integer);
      pragma Import (C, XSetWMProtocols, "XSetWMProtocols");
   begin
      XSetWMProtocols (Display, W, Protocols'Address, Protocols'Length);
   end X_Set_WM_Protocols;
   pragma Inline (X_Set_WM_Protocols);


   function X_Get_WM_Protocols
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Atom_Array is

      function XGetWMProtocols
        (Display          : in Display_Pointer;
         W                : in Window_ID;
	 Protocols_Return : in System.Address;
	 Count_Return     : in System.Address)
      return Status_Type;
      pragma Import (C, XGetWMProtocols, "XGetWMProtocols");

      Hook   : System.Address;
      Number : Integer;
   begin
      if XGetWMProtocols (Display, W,
                          Hook'Address, Number'Address) = Error_Status then
         raise X_Error;
      end if;
      declare
         subtype This_Array_Type is Atom_Array (1 .. Number);
	 type This_Array_Access_Type is access This_Array_Type;
	 function To_This_Array_Access_Type is
	    new Ada.Unchecked_Conversion (System.Address, This_Array_Access_Type);
         List : This_Array_Type;
      begin
         List := To_This_Array_Access_Type (Hook).all;
	 XFree (Hook);
         return List;
      end;
   end X_Get_WM_Protocols;



   -- -------------------------------------------------------------------------
   --
   -- parse window geometry
   --
   function No_Value   (Valid : Geometry_Valid_Mask) return Boolean is
   begin
      return not (Valid.X_Value or else Valid.Y_Value or else
                  Valid.Width_Value or else Valid.Height_Value);
   end No_Value;


   function All_Values (Valid : Geometry_Valid_Mask) return Boolean is
   begin
      return (Valid.X_Value     and then Valid.Y_Value and then
              Valid.Width_Value and then Valid.Height_Value);
   end All_Values;


   procedure X_Parse_Geometry
     (Parsestring : in     String;
      X, Y        :    out Integer;
      Width,
      Height      :    out Interfaces.C.unsigned;
      Valid       :    out Geometry_Valid_Mask) is
      function XParseGeometry
        (Parsestring : in System.Address;
         X, Y        : in System.Address;
         Width,
         Height      : in System.Address)
	 return C_Geometry_Valid_Mask;
      pragma Import (C, XParseGeometry, "XParseGeometry");
      Parse_String : constant Interfaces.C.Char_Array
                   := Interfaces.C.To_C (Parsestring, Append_Nul => True);
   begin
      Valid := To_Mask (XParseGeometry (Parse_String'Address,
                                        X'Address, Y'Address,
			                Width'Address, Height'Address));
   end X_Parse_Geometry;


   procedure X_WM_Geometry
     (Display          : in     Display_Pointer;
      Screen           : in     Screen_Number;
      User_Geometry    : in     String;
      Default_Geometry : in     String;
      Border_Width     : in     Natural;
      Hints            : in     Size_Hints_Type;
      X, Y             :    out Integer;
      Width,
      Height           :    out Interfaces.C.unsigned;
      Gravity          :    out Gravity_Type;
      Valid_Geometry   :    out Geometry_Valid_Mask) is
      function XWMGeometry
        (Display          : in Display_Pointer;
         Screen           : in Screen_Number;
         User_Geometry    : in System.Address;
         Default_Geometry : in System.Address;
         Border_Width     : in Integer;
         Hints            : in System.Address;
         X, Y             : in System.Address;
         Width,
         Height           : in System.Address;
         Gravity          : in System.Address)
         return C_Geometry_Valid_Mask;
      pragma Import (C, XWMGeometry, "XWMGeometry");
      User_Geometry_String    : constant Interfaces.C.Char_Array
                              := Interfaces.C.To_C (User_Geometry, Append_Nul => True);
      Default_Geometry_String : constant Interfaces.C.Char_Array
                              := Interfaces.C.To_C (Default_Geometry, Append_Nul => True);
   begin
      Valid_Geometry := To_Mask (XWMGeometry (Display, Screen,
                                     User_Geometry_String'Address,
                                     Default_Geometry_String'Address,
				     Border_Width,
				     Hints'Address,
				     X'Address, Y'Address,
				     Width'Address, Height'Address,
				     Gravity'Address));
   end X_WM_Geometry;


   procedure X_Reconfigure_WM_Window
     (Display          : in Display_Pointer;
      W                : in Window_ID;
      Screen           : in Screen_Number;
      Mask             : in Window_Changes_Mask;
      Changes          : in Window_Changes_Type) is
      function XReconfigureWMWindow
        (Display          : in Display_Pointer;
         W                : in Window_ID;
         Screen           : in Screen_Number;
         Mask             : in C_Window_Changes_Mask;
         Changes          : in System.Address)
	 return Status_Type;
      pragma Import (C, XReconfigureWMWindow, "XReconfigureWMWindow");
   begin
      if XReconfigureWMWindow (Display, W, Screen,
                               To_Int (Mask), Changes'Address) = Error_Status then
         raise X_Error;
      end if;
   end X_Reconfigure_WM_Window;



   function X_Intern_Atom (Display        : in Display_Pointer;
                           Name           : in String;
                           Only_If_Exists : in Boolean) return Atom is

      function XInternAtom (Display        : in Display_Pointer;
                            Name           : in System.Address;
                            Only_If_Exists : in Integer) return Atom;
      pragma Import (C, XInternAtom, "XInternAtom");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      if Only_If_Exists then
         return (XInternAtom (Display, Name_String'Address, Integer (1)));
      else
         return (XInternAtom (Display, Name_String'Address, Integer (0)));
      end if;
   end X_Intern_Atom;


   function X_Get_Atom_Name (Display : in Display_Pointer;
                             Atom    : in X_Lib.Atom) return String is

      function XGetAtomName
        (Display : in Display_Pointer;
         Atom    : in X_Lib.Atom)
	 return Interfaces.C.Strings.Chars_Ptr;
      pragma Import (C, XGetAtomName, "XGetAtomName");

      procedure XFree (What : in Interfaces.C.Strings.chars_ptr);
      pragma Import (C, XFree, "XFree");

      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := XGetAtomName (Display, Atom);
      if Interfaces.C.Strings."=" (Ptr, Interfaces.C.Strings.Null_Ptr) then
         return "";
      else
         declare
            Ret_String : constant String := Interfaces.C.Strings.Value (Ptr);
         begin
	    -- free the returned String
	    XFree (Ptr);
	    return Ret_String;
         end;
      end if;
   end X_Get_Atom_Name;


end X_Lib.Property;
