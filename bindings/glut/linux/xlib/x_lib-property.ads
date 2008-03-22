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
--                        renamed State_Type to Window_State
--
-------------------------------------------------------------------------------

package X_Lib.Property is

   -- -------------------------------------------------------------------------
   --
   --   Size Hints
   --
   type Size_Hints_Mask is record
      US_Position   : Boolean;
      US_Size       : Boolean;
      P_Position    : Boolean;
      P_Size        : Boolean;
      P_Min_Size    : Boolean;
      P_Max_Size    : Boolean;
      P_Resize_Inc  : Boolean;
      P_Aspect      : Boolean;
      P_Base_Size   : Boolean;
      P_Win_Gravity : Boolean;
   end record;
   for Size_Hints_Mask use record
-- UseLittleEndian
      US_Position   at 0 range  0 ..  0;
      US_Size       at 0 range  1 ..  1;
      P_Position    at 0 range  2 ..  2;
      P_Size        at 0 range  3 ..  3;
      P_Min_Size    at 0 range  4 ..  4;
      P_Max_Size    at 0 range  5 ..  5;
      P_Resize_Inc  at 0 range  6 ..  6;
      P_Aspect      at 0 range  7 ..  7;
      P_Base_Size   at 0 range  8 ..  8;
      P_Win_Gravity at 0 range  9 ..  9;
-- NotLittleEndian
--!       US_Position   at 0 range  9 ..  9;
--!       US_Size       at 0 range  8 ..  8;
--!       P_Position    at 0 range  7 ..  7;
--!       P_Size        at 0 range  6 ..  6;
--!       P_Min_Size    at 0 range  5 ..  5;
--!       P_Max_Size    at 0 range  4 ..  4;
--!       P_Resize_Inc  at 0 range  3 ..  3;
--!       P_Aspect      at 0 range  2 ..  2;
--!       P_Base_Size   at 0 range  1 ..  1;
--!       P_Win_Gravity at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   pragma Pack (Size_Hints_Mask);
   for Size_Hints_Mask'Size use 10;

   type Aspect_Ratio_Type is record
      Numerator   : Integer;
      Denominator : Integer;
   end record;


   type Size_Hints_Type is record
      Flags         : Size_Hints_Mask;
      X, Y          : Integer;
      Width, Height : Integer;
      Min_Width,
      Min_Height    : Integer;
      Max_Width,
      Max_Height    : Integer;
      Width_Inc,
      Height_Inc    : Integer;
      Min_Aspect,
      Max_Aspect    : Aspect_Ratio_Type;
      Base_Width,
      Base_Height   : Integer;
      Win_Gravity   : Gravity_Type;
   end record;
   for Size_Hints_Type use record
-- UseLittleEndian
      Flags         at 0 range  0 ..  9;
-- NotLittleEndian
--!       Flags         at 0 range 22 .. 31;
-- EndLittleEndian
      X             at  4 range  0 .. 31;
      Y             at  8 range  0 .. 31;
      Width         at 12 range  0 .. 31;
      Height        at 16 range  0 .. 31;
      Min_Width     at 20 range  0 .. 31;
      Min_Height    at 24 range  0 .. 31;
      Max_Width     at 28 range  0 .. 31;
      Max_Height    at 32 range  0 .. 31;
      Width_Inc     at 36 range  0 .. 31;
      Height_Inc    at 40 range  0 .. 31;
      Min_Aspect    at 44 range  0 .. 63;
      Max_Aspect    at 52 range  0 .. 63;
      Base_Width    at 60 range  0 .. 31;
      Base_Height   at 64 range  0 .. 31;
      Win_Gravity   at 68 range  0 .. 31;
   end record;
   pragma Convention (C, Size_Hints_Type);

   -- -------------------------------------------------------------------------
   --
   --   WN Hints
   --
   type WM_Hints_Mask is record
      Input         : Boolean;
      State         : Boolean;
      Icon_Pixmap   : Boolean;
      Icon_Window   : Boolean;
      Icon_Position : Boolean;
      Icon_Mask     : Boolean;
      Window_Group  : Boolean;
      Pad_7         : Boolean := False;
      Urgent        : Boolean;
   end record;
   for WM_Hints_Mask use record
-- UseLittleEndian
      Input         at 0 range  0 ..  0;
      State         at 0 range  1 ..  1;
      Icon_Pixmap   at 0 range  2 ..  2;
      Icon_Window   at 0 range  3 ..  3;
      Icon_Position at 0 range  4 ..  4;
      Icon_Mask     at 0 range  5 ..  5;
      Window_Group  at 0 range  6 ..  6;
      Pad_7         at 0 range  7 ..  7;
      Urgent        at 0 range  8 ..  8;
-- NotLittleEndian
--!       Input         at 0 range  8 ..  8;
--!       State         at 0 range  7 ..  7;
--!       Icon_Pixmap   at 0 range  6 ..  6;
--!       Icon_Window   at 0 range  5 ..  5;
--!       Icon_Position at 0 range  4 ..  4;
--!       Icon_Mask     at 0 range  3 ..  3;
--!       Window_Group  at 0 range  2 ..  2;
--!       Pad_7         at 0 range  1 ..  1;
--!       Urgent        at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   pragma Pack (WM_Hints_Mask);
   for WM_Hints_Mask'Size use 9;

   All_WM_Hints : constant WM_Hints_Mask := (Input | State | Icon_Pixmap |
                                             Icon_Window | Icon_Position |
				             Icon_Mask | Window_Group => True,
					     others => False);

   type Window_State is (Withdrawn, Normal, Iconic);
   for Window_State use (Withdrawn => 0, Normal => 1, Iconic => 3);
   for Window_State'Size use Interfaces.C.int'Size;

   type WM_Hints_Type is record
      Flags         : WM_Hints_Mask;
      Input         : Boolean;
      Initial_State : Window_State;
      Icon_Pixmap   : Pixmap_ID;
      Icon_Window   : Window_ID;
      Icon_X,
      Icon_Y        : Integer;
      Icon_Mask     : Pixmap_ID;
      Window_Group  : XID;
   end record;
   for WM_Hints_Type use record
-- UseLittleEndian
      Flags          at  0 range  0 ..  8;
-- NotLittleEndian
--!       Flags          at  0 range  23 .. 31;
-- EndLittleEndian
      Input          at  4 range  0 .. 31;
      Initial_State  at  8 range  0 .. 31;
      Icon_Pixmap    at 12 range  0 .. 31;
      Icon_Window    at 16 range  0 .. 31;
      Icon_X         at 20 range  0 .. 31;
      Icon_Y         at 24 range  0 .. 31;
      Icon_Mask      at 28 range  0 .. 31;
      Window_Group   at 32 range  0 .. 31;
   end record;
   pragma Convention (C, WM_Hints_Type);


   subtype Name_String is String;
   type Name_String_Access is access all Name_String;

   subtype Class_String is String;
   type Class_String_Access is access all Class_String;

   type Class_Hint_Type is record
      Res_Name  : Name_String_Access;
      Res_Class : Class_String_Access;
   end record;



   type Text_Property_Type (Format : Data_Format_Type;
                            Length : Natural) is record
      Encoding : Atom;
      case Format is
         when Invalid =>
	    null;
         when Bits_8 =>
            Value_8 : Bits_8_Array_Type (1 .. Length);
         when Bits_16 =>
            Value_16 : Bits_16_Array_Type (1 .. Length);
         when Bits_32 =>
            Value_32 : Bits_32_Array_Type (1 .. Length);
      end case;
   end record;


   type ICC_Encoding_Style_Type is new Interfaces.C.signed_char;
   Style_String        : constant ICC_Encoding_Style_Type := 0;
   Style_Compound_Text : constant ICC_Encoding_Style_Type := 1;
   Style_Text          : constant ICC_Encoding_Style_Type := 2;
   Style_Std_ICC_Text  : constant ICC_Encoding_Style_Type := 3;


   -- conversion routines to and from Text_Property_Type
   --
   function To_Text_Property (List : in String_List.Element_Access_List)
      return Text_Property_Type;

   function To_Text_Property (List : in String)
      return Text_Property_Type;

   function To_String_List (Property : in Text_Property_Type)
      return String_List.Element_Access_List;


--   X_Error_No_Memory : exception; -- already defined in X_Lib (-1)
   X_Error_Locale_Not_Supported : exception; -- return-Value -2
   X_Error_Converter_Not_Found  : exception; -- return-Value -3

--   procedure Xmb_Text_List_To_Text_Property
--     (Display  : in     Display_Pointer;
--      List...,
--      Count...,
--      Style    : in     ICC_Encoding_Style_Type;
--      Property :    out Text_Property_type);

--   procedure Xwc_Text_List_To_Text_Property
--     (Display  : in     Display_Pointer;
--      List...,
--      Count...,
--      Style    : in     ICC_Encoding_Style_Type;
--      Property :    out Text_Property_type);

   type Change_Mode_Type is (Replace, Prepend, Append);
   for Change_Mode_Type use (Replace => 0, Prepend => 1, Append => 2);
   for Change_Mode_Type'Size use Interfaces.C.int'Size;


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_8_Array_Type);


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_16_Array_Type);


   procedure X_Change_Property
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Property    : in Atom;
      Data_Type   : in Atom;
      Mode        : in Change_Mode_Type;
      Data        : in Bits_32_Array_Type);


   procedure X_Delete_Property
     (Display  : in Display_Pointer;
      W        : in Window_ID;
      Property : in Atom);


   function X_Get_Text_Property
     (Display  : in Display_Pointer;
      W        : in Window_ID;
      Property : in Atom)
      return Text_Property_Type;


   procedure X_Set_Text_Property
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type;
      Property  : in Atom);


   type Get_Window_Property_Return_Type (Format : Data_Format_Type;
                                         Length : Natural) is record
      Bytes_After : Natural;
      Value_Type : Atom;
      case Format is
         when Invalid =>
	    null;
         when Bits_8 =>
            Value_8 : Bits_8_Array_Type (1 .. Length);
         when Bits_16 =>
            Value_16 : Bits_16_Array_Type (1 .. Length);
         when Bits_32 =>
            Value_32 : Bits_32_Array_Type (1 .. Length);
      end case;
   end record;


   function X_Get_Window_Property
     (Display       : in     Display_Pointer;
      W             : in     Window_ID;
      Property      : in     Atom;
      Long_Offset   : in     Natural;
      Long_Length   : in     Natural;
      Delete        : in     Boolean;
      Req_Type      : in     Atom)
      return Get_Window_Property_Return_Type;


   -- set XA_WM_CLIENT_MACHINE property
   --
   procedure X_Set_WM_Client_Machine
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type);

   function X_Get_WM_Client_Machine
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type;


   -- set XA_WM_COLORMAP_WINDOWS property
   --
   procedure X_Set_WM_Colormap_Windows
     (Display          : in Display_Pointer;
      W                : in Window_ID;
      Colormap_Windows : in Window_ID_Array_Type);

   function X_Get_WM_Colormap_Windows
     (Display          : in Display_Pointer;
      W                : in Window_ID)
      return Window_ID_Array_Type;


   -- set window manager hints
   procedure X_Set_WM_Hints
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      WM_Hints  : in WM_Hints_Type);

   function X_Get_WM_Hints
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return WM_Hints_Type;


   -- set XA_WM_ICON_NAME property
   --
   procedure X_Set_WM_Icon_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type);

   function X_Get_WM_Icon_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type;


   -- set XA_WM_NAME property
   --
   procedure X_Set_WM_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Text_Prop : in Text_Property_Type);

   function X_Get_WM_Name
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Text_Property_Type;


   -- set XA_WM_NORMAL_HINTS property
   --
   procedure X_Set_WM_Normal_Hints
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Hints     : in Size_Hints_Type);

   procedure X_Get_WM_Normal_Hints
     (Display   : in     Display_Pointer;
      W         : in     Window_ID;
      Hints     :    out Size_Hints_Type;
      Supplied  :    out Size_Hints_Mask);


   -- set standard window properties
   --
   procedure X_Set_WM_Properties
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Window_Name  : in Text_Property_Type;
      Icon_Name    : in Text_Property_Type;
      Args         : in System.Address;
      Argc         : in Integer;
      Normal_Hints : access Size_Hints_Type;
      WM_Hints     : access WM_Hints_Type;
      Class_Hints  : access Class_Hint_Type);

--     procedure X_Set_WM_Properties
--       (Display      : in Display_Pointer;
--        W            : in Window_ID;
--        Window_Name  : in Text_Property_Type;
--        Icon_Name    : in Text_Property_Type;
--        Args         : in System.Address;
--        Argc         : in Integer;
--        Normal_Hints : in Size_Hints_Type;
--        WM_Hints     : in WM_Hints_Type;
--        Class_Hints  : in Class_Hint_Type);


   -- set XA_WM_PROTOCOLS property
   --
   procedure X_Set_WM_Protocols
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Protocols : in Atom_Array);

   function X_Get_WM_Protocols
     (Display   : in Display_Pointer;
      W         : in Window_ID)
      return Atom_Array;


   -- set XA_WM_SIZE_HINTS property
   --
   procedure X_Set_WM_Size_Hints
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Hints     : in Size_Hints_Type;
      Property  : in Atom);

   procedure X_Get_WM_Size_Hints
     (Display   : in     Display_Pointer;
      W         : in     Window_ID;
      Hints     :    out Size_Hints_Type;
      Property  : in     Atom;
      Supplied  :    out Size_Hints_Mask);


   -- set XA_WM_TRANSIENT_FOR property
   --
   procedure X_Set_Transient_For_Hint
     (Display     : in Display_Pointer;
      W           : in Window_ID;
      Prop_Window : in Window_ID);

   function X_Get_Transient_For_Hint
     (Display     : in Display_Pointer;
      W           : in Window_ID)
      return Window_ID;


   -- -------------------------------------------------------------------------
   --
   -- parse window geometry
   --
   type Geometry_Valid_Mask is record
      X_Value      : Boolean;
      Y_Value      : Boolean;
      Width_Value  : Boolean;
      Height_Value : Boolean;
      X_Negative   : Boolean;
      Y_Negative   : Boolean;
   end record;
   for Geometry_Valid_Mask use record
-- UseLittleEndian
      X_Value       at 0 range  0 ..  0;
      Y_Value       at 0 range  1 ..  1;
      Width_Value   at 0 range  2 ..  2;
      Height_Value  at 0 range  3 ..  3;
      X_Negative    at 0 range  4 ..  4;
      Y_Negative    at 0 range  5 ..  5;
-- NotLittleEndian
--!       X_Value       at 0 range  5 ..  5;
--!       Y_Value       at 0 range  4 ..  4;
--!       Width_Value   at 0 range  3 ..  3;
--!       Height_Value  at 0 range  2 ..  2;
--!       X_Negative    at 0 range  1 ..  1;
--!       Y_Negative    at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   pragma Pack (Geometry_Valid_Mask);
   for Geometry_Valid_Mask'Size use 6;

   function No_Value   (Valid : Geometry_Valid_Mask) return Boolean;
   function All_Values (Valid : Geometry_Valid_Mask) return Boolean;

   procedure X_Parse_Geometry
     (Parsestring : in     String;
      X, Y        :    out Integer;
      Width,
      Height      :    out Interfaces.C.unsigned;
      Valid       :    out Geometry_Valid_Mask);

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
      Valid_Geometry   :    out Geometry_Valid_Mask);


   procedure X_Reconfigure_WM_Window
     (Display          : in Display_Pointer;
      W                : in Window_ID;
      Screen           : in Screen_Number;
      Mask             : in Window_Changes_Mask;
      Changes          : in Window_Changes_Type);


   function X_Intern_Atom
     (Display        : in Display_Pointer;
      Name           : in String;
      Only_If_Exists : in Boolean)
      return Atom;


   function X_Get_Atom_Name
     (Display        : in Display_Pointer;
      Atom           : in X_Lib.Atom)
      return String;



private

   pragma Import (C, X_Delete_Property, "XDeleteProperty");
   pragma Import (C, X_Set_WM_Hints, "XSetWMHints");
   pragma Import (C, X_Set_WM_Normal_Hints, "XSetWMNormalHints");
--   pragma Import (C, X_Set_WM_Properties, "XSetWMProperties");
   pragma Import (C, X_Set_WM_Size_Hints, "XSetWMSizeHints");
   pragma Import (C, X_Set_Transient_For_Hint, "XSetTransientForHint");

end X_Lib.Property;
