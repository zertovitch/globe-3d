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
--          July 07, 1998 HFVogt: changed Boolean_Type to X_Boolean to prevent
--                                confusion
--                                changed IM_Type to Input_Method, OM_Type to
--                                Output_Method, IC_Type to Input_Context and
--                                OC_Type to Output_Context
--          September 5, 1998 extract X_Lib.Extensions
--          November 8, 1998 HFVogt: added X11R6.3 conditional defines
--          June 14, 2000: included additions from Joerg Schaefer:
--                         type Char_Struct, X_Query_Text_Extents,
--                         X_Resize_Window, X_Query_Pointer
--          19 May 2001 Vadim Godunko: error handling procs
--          17 Nov 2001 Vadim Godunko: add X_Create_Simple_Window
--          09 Feb 2002 H.-F. Vogt: moved X_*_Event representative clause into
--                                  private area to satisfy GNAT 3.14p
--                                  add No_Connection_Error exception
--
-------------------------------------------------------------------------------

with Interfaces.C,
     System,
     String_List,
     Generic_List_Types;

with interfaces.C.strings;



package X_Lib is

   use type Interfaces.C.short;

   Null_Address : System.Address renames System.Null_Address;

   type Long_Array  is array (Natural range <>) of Interfaces.C.long;



   -- Key masks. Used as modifiers to GrabButton and GrabKey, results of QueryPointer,
   -- state in various key-, mouse-, and button-related events. ... (from X.h)
   --
   ShiftMask   : constant := 2**0;
   LockMask    : constant := 2**1;
   ControlMask : constant := 2**2;
   Mod1Mask    : constant := 2**3;
   Mod2Mask    : constant := 2**4;
   Mod3Mask    : constant := 2**5;
   Mod4Mask    : constant := 2**6;
   Mod5Mask    : constant := 2**7;



-- ----------------------------------------------------------------------------
--
-- Type-Definitions
--

   -- -------------------------------------------------------------------------
   --
   --    E R R O R S
   --
   type Error_Code_Type is new Interfaces.C.unsigned_char;

   Success            : constant Error_Code_Type :=  0;
   Bad_Request        : constant Error_Code_Type :=  1;
   Bad_Value          : constant Error_Code_Type :=  2;
   Bad_Window         : constant Error_Code_Type :=  3;
   Bad_Pixmap         : constant Error_Code_Type :=  4;
   Bad_Atom           : constant Error_Code_Type :=  5;
   Bad_Cursor         : constant Error_Code_Type :=  6;
   Bad_Font           : constant Error_Code_Type :=  7;
   Bad_Match          : constant Error_Code_Type :=  8;
   Bad_Drawable       : constant Error_Code_Type :=  9;
   Bad_Access         : constant Error_Code_Type := 10;
   Bad_Alloc          : constant Error_Code_Type := 11;
   Bad_Color          : constant Error_Code_Type := 12;
   Bad_GC             : constant Error_Code_Type := 13;
   Bad_ID_Choice      : constant Error_Code_Type := 14;
   Bad_Name           : constant Error_Code_Type := 15;
   Bad_Length         : constant Error_Code_Type := 16;
   Bad_Implementation : constant Error_Code_Type := 17;

   First_Extension_Error : constant Error_Code_Type := 128;
   Last_Extension_Error  : constant Error_Code_Type := 255;


   X_Error : exception;


   type Opcode_Type is new Integer;


   -- -------------------------------------------------------------------------
   --
   -- server resource IDs
   --
   type XID is new Interfaces.C.unsigned_long;
   Null_XID : constant XID;

   type Drawable_ID   is new XID;
   Null_Drawable_ID : constant Drawable_ID;

   subtype Window_ID  is Drawable_ID;
   Null_Window_ID   : constant Window_ID;

   subtype Pixmap_ID  is Drawable_ID;
   Null_Pixmap_ID   : constant Pixmap_ID;

   type Font_ID       is new XID;
   Null_Font_ID     : constant Font_ID;

   type Cursor_ID     is new XID;
   Null_Cursor_ID   : constant Cursor_ID;

   type Colormap_ID   is new XID;
   Null_Colormap_ID : constant Colormap_ID;

   type GContext_ID   is new XID;
   Null_GContext_ID : constant GContext_ID;

   type Key_Sym_ID    is new XID;
   Null_Key_Sym_ID  : constant Key_Sym_ID;

   type Visual_ID     is new XID;
   Null_Visual_ID   : constant Visual_ID;


   type Window_ID_Array_Type is array (Natural range <>) of Window_ID;


   -- -------------------------------------------------------------------------
   --
   --  key code (representation of a physical or logical key on the keyboard)
   --
   type Key_Code_Type is new Interfaces.C.unsigned_char;

   -- specifies the index into the vector of key codes
   subtype Key_Codes_Vector_Index_Type is Natural;


   -- -------------------------------------------------------------------------
   --
   -- Atom
   --
   type Atom  is new Interfaces.C.unsigned_long;
   Null_Atom : constant Atom;

   -- -------------------------------------------------------------------------
   --
   -- Region
   --
   type Region is private;
   Null_Region : constant Region;


   type X_Font_Struct_Pointer is new System.Address;
   Null_Font_Struct_Pointer : constant X_Font_Struct_Pointer
                            := X_Font_Struct_Pointer (Null_Address);

   type Display_Pointer is private;
   Null_Display_Pointer : constant Display_Pointer;

   type Screen_Pointer  is private;
   Null_Screen_Pointer : constant Screen_Pointer;

   type GC_Pointer  is private;
   Null_GC_Pointer : constant GC_Pointer;


   type X_Pointer is private;
   Null_X_Pointer : constant X_Pointer;


   -- normally this is declared in Xt, but it is already used in Xlib
   type Pixel is new Interfaces.C.unsigned_long;


   type Screen_Number is
      new Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   type Color_Depth is
      new Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   type Color_Plane_Mask is mod 2 ** Pixel'Size;



   -- in Xlib this is simply called TIME, but I rename it here for not
   -- to become confused
   type Server_Time is new Interfaces.C.unsigned_long;
   Current_Server_Time : constant Server_Time := Server_Time (0);


   -- return status for Xlib functions, used only internally
   type Status_Type is new Interfaces.C.int;
   Error_Status : constant Status_Type := Status_Type (0);

   -- Xlib's own boolean type
   type X_Boolean is (False, True);
   for X_Boolean use (False => 0, True => 1);
   for X_Boolean'Size use Interfaces.C.int'Size;

   function To_X_Boolean (Val : in Boolean) return X_Boolean;
   function To_Boolean (Val : in X_Boolean) return Boolean;

   -- general 8 bit, 16 bit and 32 bit types
   --
   type Data_Format_Type is (Invalid, Bits_8, Bits_16, Bits_32);
   for  Data_Format_Type use (Invalid => 0, Bits_8 => 8, Bits_16 => 16, Bits_32 => 32);
   for  Data_Format_Type'Size use Interfaces.C.int'Size;


   type Bits_8_Type  is mod 2**Interfaces.C.signed_char'Size;
   type Bits_16_Type is mod 2**Interfaces.C.short'Size;
   type Bits_32_Type is mod 2**Interfaces.C.long'Size;  -- not necessarily 32 bits!

   type Bits_8_Array_Type   is array (Natural range <>) of aliased Bits_8_Type;
   type Bits_16_Array_Type  is array (Natural range <>) of aliased Bits_16_Type;
   type Bits_32_Array_Type  is array (Natural range <>) of aliased Bits_32_Type;


   function To_Bits_32_Type (Source : in XID) return Bits_32_Type;
   function To_XID (Source : in Bits_32_Type) return XID;

   function To_Bits_32_Type (Source : in X_Pointer) return Bits_32_Type;
   function To_X_Pointer (Source : in Bits_32_Type) return X_Pointer;



   -- opaque types used for internationalization
   --
   -- input method
   type Input_Method is private;
   Null_Input_Method : constant Input_Method;

   -- output method
   type Output_Method is private;
   Null_Output_Method : constant Output_Method;

   -- input context
   type Input_Context is private;
   Null_Input_Context : constant Input_Context;

   -- output context
   type Output_Context is private;
   Null_Output_Context : constant Output_Context;

   -- font set
   type Font_Set_Type is private;
   Null_Font_Set : constant Font_Set_Type;


   function X_Protocol_Version (Display : in Display_Pointer)
      return Integer;

   function X_Protocol_Revision (Display : in Display_Pointer)
      return Integer;

   function X_Server_Vendor (Display : in Display_Pointer)
      return String;

   function X_Vendor_Release (Display : in Display_Pointer)
      return Integer;


   function X_Warp_Pointer (Display    : in Display_Pointer;
                            src_w      : in Window_Id;
                            dest_w     : in Window_Id;
                            src_x      : in interfaces.c.Int;
                            src_y      : in interfaces.c.Int;
                            src_width  : in interfaces.c.Unsigned;
                            src_height : in interfaces.c.Unsigned;
                            dest_x     : in interfaces.c.Int;
                            dest_y     : in interfaces.c.Int) return interfaces.c.Int;

   pragma import (C, X_Warp_Pointer, "XWarpPointer");


   --     extern int XWarpPointer(
   --      Display*		/* display */,
   --      Window		/* src_w */,
   --      Window		/* dest_w */,
   --      int			/* src_x */,
   --      int			/* src_y */,
   --      unsigned int	/* src_width */,
   --      unsigned int	/* src_height */,
   --      int			/* dest_x */,
   --      int			/* dest_y */
   --  );



   function X_Max_Request_Size (Display : in Display_Pointer)
      return Interfaces.C.long;

   function X_Extended_Max_Request_Size (Display : in Display_Pointer)
      return Interfaces.C.long;

   function X_Display_Motion_Buffer_Size (Display : in Display_Pointer)
      return Interfaces.C.unsigned_long;



   -- -------------------------------------------------------------------------
   --
   --   Classes
   --
   type Window_Class is  (Copy_From_Parent, Input_Output, Input_Only);
   for  Window_Class use (Copy_From_Parent => 0,
                          Input_Output     => 1,
                          Input_Only       => 2);
   for Window_Class'Size use Interfaces.C.unsigned'Size;


   type Visual_Class is (Static_Gray,
                         Gray_Scale,
                         Static_Color,
                         Pseudo_Color,
                         True_Color,
                         Direct_Color);

   for Visual_Class use (Static_Gray   => 0,
                         Gray_Scale    => 1,
                         Static_Color  => 2,
                         Pseudo_Color  => 3,
                         True_Color    => 4,
                         Direct_Color  => 5);
   for Visual_Class'Size use Interfaces.C.int'Size;



   -- -------------------------------------------------------------------------
   --
   --   Hook for additional Information
   --
   type X_Ext_Data_Pointer is new System.Address;
   Null_Ext_Data_Pointer : constant X_Ext_Data_Pointer := X_Ext_Data_Pointer (Null_Address);



   -- -------------------------------------------------------------------------
   --
   --   geometry definition
   --
   -- previously (and originally) defined in the Intrinsics, but already
   -- needed here

-- Use64Bit
--!    subtype    Dimension  is Interfaces.C.unsigned;
--!    subtype    Position   is Interfaces.C.int;
-- Not64Bit
   subtype    Dimension  is Interfaces.C.unsigned_short;
   subtype    Position   is Interfaces.C.short;
-- End64Bit
   subtype Natural_Position is Position range 0 .. Position'Last;

   type Angle is digits 6; -- this is enough for angles

   type Arc is record
      X, Y           : Position;
      Width, Height  : Dimension;
      Angle1, Angle2 : Angle;
   end record;
   type Arc_Access is access all Arc;

   type Arc_Array is array (Natural range <>) of aliased Arc;


   type Point is record
      X, Y : Position;
   end record;
   type Point_Access is access all Point;

   type Point_Array is array (Natural range <>) of aliased Point;


   type Rectangle is record
      X, Y          : Position;
      Width, Height : Dimension;
   end record;
   type Rectangle_Access is access all Rectangle;

   type Rectangle_Array is array (Natural range <>) of aliased Rectangle;


   type Segment is record
      X1, Y1, X2, Y2 : Position;
   end record;
   type Segment_Access is access all Segment;

   type Segment_Array is array (Natural range <>) of aliased Segment;



   --
   -- XID List
   --
   package XID_Lists is
      new Generic_List_Types (XID);
   type XID_List is new XID_Lists.Unbounded_List;

   Null_XID_List : constant XID_List := XID_List (XID_Lists.Null_Unbounded_List);



   function Length (List : in  XID_List) return Natural;

   function Element
     (List   : in XID_List;
      Index  : in Natural)
      return XID;

   function "=" (Left, Right : in XID_List) return Boolean;

   function "&" (Left, Right : in XID_List) return XID_List;
   function "&" (Left  : in XID_List;
                 Right : in XID) return XID_List;

   procedure Append (List : in out XID_List;
                     W    : in     XID_List);

   procedure Append (List : in out XID_List;
                     W    : in     XID);


   subtype Window_ID_List is XID_List;


   procedure X_Query_Tree
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Root     :    out Window_ID;
      Parent   :    out Window_ID;
      Children :    out Window_ID_List);


   -- -------------------------------------------------------------------------
   --
   --  keyboard
   --
   function X_String_To_Keysym (Keysym_Name : in String) return Key_Sym_ID;


   function X_Keysym_To_String (Keysym : in Key_Sym_ID) return String;


   function X_Keycode_To_Keysym
     (Display : in Display_Pointer;
      Keycode : in Key_Code_Type;
      Index   : in Key_Codes_Vector_Index_Type)
      return Key_Sym_ID;


   function X_Keysym_To_Keycode
     (Display : in Display_Pointer;
      Keysym  : in Key_Sym_ID)
      return Key_Code_Type;


   procedure X_Convert_Case
     (Keysym  : in     Key_Sym_ID;
      Lower   :    out Key_Sym_ID;
      Upper   :    out Key_Sym_ID);



   -- -------------------------------------------------------------------------
   --
   --   Graphics Context
   --

   type GX_Functions is (GX_Clear, GX_And, GX_And_Reverse, GX_Copy,
                         GX_And_Inverted, GX_Noop, GX_Xor, GX_Or,
                         GX_Nor, GX_Equiv, GX_Invert, GX_Or_Reverse,
                         GX_Copy_Inverted, GX_Or_Inverted, GX_Nand, GX_Set);
   for GX_Functions use (GX_Clear => 0, GX_And => 1, GX_And_Reverse => 2, GX_Copy => 3,
                         GX_And_Inverted => 4, GX_Noop => 5, GX_Xor => 6, GX_Or => 7,
                         GX_Nor => 8, GX_Equiv => 9, GX_Invert => 10, GX_Or_Reverse => 11,
                         GX_Copy_Inverted => 12, GX_Or_Inverted => 13, GX_Nand => 14, GX_Set => 15);
   for GX_Functions'Size use Interfaces.C.int'Size;

   type Line_Styles is (Solid, On_Off_Dash, Double_Dash);
   for Line_Styles use (Solid => 0, On_Off_Dash => 1, Double_Dash => 2);
   for Line_Styles'Size use Interfaces.C.int'Size;

   type Cap_Styles is (Not_Last, Butt, Round, Projecting);
   for Cap_Styles use (Not_Last => 0, Butt => 1, Round => 2, Projecting => 3);
   for Cap_Styles'Size use Interfaces.C.int'Size;

   type Join_Styles is (Miter, Round, Bevel);
   for Join_Styles use (Miter => 0, Round => 1, Bevel => 2);
   for Join_Styles'Size use Interfaces.C.int'Size;

   type Fill_Styles is (Solid, Tiled, Stippled, Opaque_Stippled);
   for Fill_Styles use (Solid => 0, Tiled => 1, Stippled => 2, Opaque_Stippled => 3);
   for Fill_Styles'Size use Interfaces.C.int'Size;

   type Fill_Rules is (Even_Odd_Rule, Winding_Rule);
   for Fill_Rules use (Even_Odd_Rule => 0, Winding_Rule => 1);
   for Fill_Rules'Size use Interfaces.C.int'Size;

   type Arc_Modes is (Chord, Pie_Slice);
   for Arc_Modes use (Chord => 0, Pie_Slice => 1);
   for Arc_Modes'Size use Interfaces.C.int'Size;

   type Subwindow_Modes is (Clip_By_Children, Include_Inferiors);
   for Subwindow_Modes use (Clip_By_Children => 0, Include_Inferiors => 1);
   for Subwindow_Modes'Size use Interfaces.C.int'Size;

   type X_GC_Values is record
      GX_Function        : GX_Functions;
      Plane_Mask         : Color_Plane_Mask;
      Foreground,
      Background         : Pixel;
      Line_Width         : Interfaces.C.unsigned;
      Line_Style         : Line_Styles;
      Cap_Style          : Cap_Styles;
      Join_Style         : Join_Styles;
      Fill_Style         : Fill_Styles;
      Fill_Rule          : Fill_Rules;
      Arc_Mode           : Arc_Modes;
      Tile               : Pixmap_ID;
      Stipple            : Pixmap_ID;
      TS_X_Origin,
      TS_Y_Origin        : Interfaces.C.int;
      Font               : Font_ID;
      Subwindow_Mode     : Subwindow_Modes;
      Graphics_Exposures : X_Boolean;
      Clip_X_Origin,
      Clip_Y_Origin      : Interfaces.C.int;
      Clip_Mask          : Pixmap_ID;
      Dash_Offset        : Interfaces.C.int;
      Dashes             : Interfaces.C.signed_char;
   end record;
   for X_GC_Values use record
      GX_Function        at  0 range 0 .. 31;
      Plane_Mask         at  4 range 0 .. 31;
      Foreground         at  8 range 0 .. 31;
      Background         at 12 range 0 .. 31;
      Line_Width         at 16 range 0 .. 31;
      Line_Style         at 20 range 0 .. 31;
      Cap_Style          at 24 range 0 .. 31;
      Join_Style         at 28 range 0 .. 31;
      Fill_Style         at 32 range 0 .. 31;
      Fill_Rule          at 36 range 0 .. 31;
      Arc_Mode           at 40 range 0 .. 31;
      Tile               at 44 range 0 .. 31;
      Stipple            at 48 range 0 .. 31;
      TS_X_Origin        at 52 range 0 .. 31;
      TS_Y_Origin        at 56 range 0 .. 31;
      Font               at 60 range 0 .. 31;
      Subwindow_Mode     at 64 range 0 .. 31;
      Graphics_Exposures at 68 range 0 .. 31;
      Clip_X_Origin      at 72 range 0 .. 31;
      Clip_Y_Origin      at 76 range 0 .. 31;
      Clip_Mask          at 80 range 0 .. 31;
      Dash_Offset        at 84 range 0 .. 31;
      Dashes             at 88 range 0 .. 31;
   end record;
   pragma Convention (C, X_GC_Values);


   type X_GC_Valuemask is record
      GC_Function           : Boolean;
      GC_Plane_Mask         : Boolean;
      GC_Foreground         : Boolean;
      GC_Background         : Boolean;
      GC_Line_Width         : Boolean;
      GC_Line_Style         : Boolean;
      GC_Cap_Style          : Boolean;
      GC_Join_Style         : Boolean;
      GC_Fill_Style         : Boolean;
      GC_Fill_Rule          : Boolean;
      GC_Tile               : Boolean;
      GC_Stipple            : Boolean;
      GC_Tile_Stip_X_Origin : Boolean;
      GC_Tile_Stip_Y_Origin : Boolean;
      GC_Font               : Boolean;
      GC_Subwindow_Mode     : Boolean;
      GC_Graphics_Exposures : Boolean;
      GC_Clip_X_Origin      : Boolean;
      GC_Clip_Y_Origin      : Boolean;
      GC_Clip_Mask          : Boolean;
      GC_Dash_Offset        : Boolean;
      GC_Dashes             : Boolean;
      GC_Arc_Mode           : Boolean;
   end record;
   for X_GC_Valuemask use record
-- UseLittleEndian
      GC_Function           at 0 range  0 ..  0;
      GC_Plane_Mask         at 0 range  1 ..  1;
      GC_Foreground         at 0 range  2 ..  2;
      GC_Background         at 0 range  3 ..  3;
      GC_Line_Width         at 0 range  4 ..  4;
      GC_Line_Style         at 0 range  5 ..  5;
      GC_Cap_Style          at 0 range  6 ..  6;
      GC_Join_Style         at 0 range  7 ..  7;
      GC_Fill_Style         at 0 range  8 ..  8;
      GC_Fill_Rule          at 0 range  9 ..  9;
      GC_Tile               at 0 range 10 .. 10;
      GC_Stipple            at 0 range 11 .. 11;
      GC_Tile_Stip_X_Origin at 0 range 12 .. 12;
      GC_Tile_Stip_Y_Origin at 0 range 13 .. 13;
      GC_Font               at 0 range 14 .. 14;
      GC_Subwindow_Mode     at 0 range 15 .. 15;
      GC_Graphics_Exposures at 0 range 16 .. 16;
      GC_Clip_X_Origin      at 0 range 17 .. 17;
      GC_Clip_Y_Origin      at 0 range 18 .. 18;
      GC_Clip_Mask          at 0 range 19 .. 19;
      GC_Dash_Offset        at 0 range 20 .. 20;
      GC_Dashes             at 0 range 21 .. 21;
      GC_Arc_Mode           at 0 range 22 .. 22;
-- NotLittleEndian
--!       GC_Function           at 0 range 22 .. 22;
--!       GC_Plane_Mask         at 0 range 21 .. 21;
--!       GC_Foreground         at 0 range 20 .. 20;
--!       GC_Background         at 0 range 19 .. 19;
--!       GC_Line_Width         at 0 range 18 .. 18;
--!       GC_Line_Style         at 0 range 17 .. 17;
--!       GC_Cap_Style          at 0 range 16 .. 16;
--!       GC_Join_Style         at 0 range 15 .. 15;
--!       GC_Fill_Style         at 0 range 14 .. 14;
--!       GC_Fill_Rule          at 0 range 13 .. 13;
--!       GC_Tile               at 0 range 12 .. 12;
--!       GC_Stipple            at 0 range 11 .. 11;
--!       GC_Tile_Stip_X_Origin at 0 range 10 .. 10;
--!       GC_Tile_Stip_Y_Origin at 0 range  9 ..  9;
--!       GC_Font               at 0 range  8 ..  8;
--!       GC_Subwindow_Mode     at 0 range  7 ..  7;
--!       GC_Graphics_Exposures at 0 range  6 ..  6;
--!       GC_Clip_X_Origin      at 0 range  5 ..  5;
--!       GC_Clip_Y_Origin      at 0 range  4 ..  4;
--!       GC_Clip_Mask          at 0 range  3 ..  3;
--!       GC_Dash_Offset        at 0 range  2 ..  2;
--!       GC_Dashes             at 0 range  1 ..  1;
--!       GC_Arc_Mode           at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   pragma Pack (X_GC_Valuemask);
   for X_GC_Valuemask'Size use 23;


   function X_Default_GC
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return GC_Pointer;

   function X_Default_GC_Of_Screen (Screen : in Screen_Pointer)
      return GC_Pointer;


   function X_Create_GC
     (Display   : in Display_Pointer;
      Drawable  : in Drawable_ID;
      Valuemask : in X_GC_Valuemask;
      Values    : in X_GC_Values)
      return GC_Pointer;

   function X_Create_GC
     (Display   : in Display_Pointer;
      Drawable  : in Drawable_ID)
      return GC_Pointer;

   procedure X_Free_GC
     (Display   : in Display_Pointer;
      GC        : in GC_Pointer);


   procedure X_Flush_GC
     (Display   : in Display_Pointer;
      GC        : in GC_Pointer);


   procedure X_Change_GC
     (Display   : in Display_Pointer;
      GC        : in GC_Pointer;
      Valuemask : in X_GC_Valuemask;
      Values    : in X_GC_Values);


   procedure X_Copy_GC
     (Display   : in Display_Pointer;
      Source    : in GC_Pointer;
      Valuemask : in X_GC_Valuemask;
      Dest      : in GC_Pointer);


   function X_GContext_From_GC (GC : in GC_Pointer) return GContext_ID;


   procedure X_Get_GC_Values
     (Display   : in     Display_Pointer;
      GC        : in     GC_Pointer;
      Valuemask : in     X_GC_Valuemask;
      Values    :    out X_GC_Values);


   -- -------------------------------------------------------------------------
   --
   --   Convenience functions
   --
   procedure X_Set_Function
     (Display   : in Display_Pointer;
      GC        : in GC_Pointer;
      Func      : in GX_Functions);

   procedure X_Set_Plane_Mask
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Plane_Mask : in Color_Plane_Mask);

   procedure X_Set_Foreground
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Foreground : in Pixel);

   procedure X_Set_Background
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Background : in Pixel);

   procedure X_Set_Line_Attributes
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Line_Width : in Interfaces.C.unsigned;
      Line_Style : in Line_Styles;
      Cap_Style  : in Cap_Styles;
      Join_Style : in Join_Styles);

   procedure X_Set_Fill_Style
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Fill_Style : in Fill_Styles);

   procedure X_Set_Fill_Rule
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Fill_Rule  : in Fill_Rules);

   procedure X_Set_Arc_Mode
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Arc_Mode   : in Arc_Modes);

   procedure X_Set_Tile
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Tile       : in Pixmap_ID);

   procedure X_Set_Stipple
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Stipple    : in Pixmap_ID);

   procedure X_Set_TS_Origin
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      TS_X_Origin,
      TS_Y_Origin : in Integer);

   procedure X_Set_Font
     (Display    : in Display_Pointer;
      GC         : in GC_Pointer;
      Font       : in Font_ID);

   procedure X_Set_Subwindow_Mode
     (Display        : in Display_Pointer;
      GC             : in GC_Pointer;
      Subwindow_Mode : in Subwindow_Modes);

   procedure X_Set_Graphics_Exposures
     (Display            : in Display_Pointer;
      GC                 : in GC_Pointer;
      Graphics_Exposures : in Boolean);

   procedure X_Set_Clip_Origin
     (Display            : in Display_Pointer;
      GC                 : in GC_Pointer;
      Clip_X_Origin,
      Clip_Y_Origin      : in Integer);

   procedure X_Set_Clip_Mask
     (Display            : in Display_Pointer;
      GC                 : in GC_Pointer;
      Clip_Mask          : in Pixmap_ID);

   type Rectangle_Ordering is (Unsorted, Y_Sorted, YX_Sorted, YX_Banded);
   for Rectangle_Ordering use (Unsorted => 0, Y_Sorted => 1,
                               YX_Sorted => 2, YX_Banded => 3);

   procedure X_Set_Clip_Rectangles
     (Display        : in Display_Pointer;
      GC             : in GC_Pointer;
      Clip_X_Origin,
      Clip_Y_Origin  : in Integer;
      Rectangles     : in Rectangle_Array;
      Ordering       : in Rectangle_Ordering);


   type Dash_List_Type is array (Natural range <>) of Interfaces.C.signed_char;

   procedure X_Set_Dashes
     (Display     : in Display_Pointer;
      GC          : in GC_Pointer;
      Dash_Offset : in Integer;
      Dash_List   : in Dash_List_Type);

   procedure X_Set_State
     (Display     : in Display_Pointer;
      GC          : in GC_Pointer;
      Foreground,
      Background  : in Pixel;
      Func        : in GX_Functions;
      Plane_Mask  : in Color_Plane_Mask);


   -- -------------------------------------------------------------------------
   --
   --   Display
   --

   --  error which is raised if no connection to the X Server could
   --  be established
   --
   No_Connection_Error : exception;


   --  open a connection to an X Server. if it fails, No_Connection_Error
   --  is raised
   --
   function X_Open_Display (Display_Name : in String := "")
      return Display_Pointer;

   procedure X_Close_Display (Display : in Display_Pointer);


   function X_Display_Of_Screen (Screen : in Screen_Pointer)
      return Display_Pointer;


   function X_Display_Name (Display_Name : in String := "")
      return String;

   function X_Display_String (Display : in Display_Pointer)
      return String;


   function X_Resource_Manager_String (Display : in Display_Pointer)
      return String;

   type Volume_Percentage_Type is range -100 .. 100;
   for  Volume_Percentage_Type'Size use Interfaces.C.int'Size;

   procedure X_Bell
     (Display : in Display_Pointer;
      Percent : in Volume_Percentage_Type);

   -- -------------------------------------------------------------------------
   --
   --   Visual
   --
   type Visual is record
      Ext_Data      : X_Ext_Data_Pointer;
      VisualID      : Visual_ID;
      Class         : Visual_Class;
      Red_Mask,
      Green_Mask,
      Blue_Mask     : Color_Plane_Mask;
      Bits_Per_Rgb  : Interfaces.C.int; -- >= 0
      Map_Entries   : Interfaces.C.int; -- >= 0
   end record;
   pragma Convention (C, Visual);

   type Visual_Pointer is access all Visual;


   type X_Visual_Info is record
      Visual        : Visual_Pointer;
      VisualID      : Visual_ID;
      Screen        : Screen_Number;
      Depth         : Color_Depth;
      Class         : Visual_Class;
      Red_Mask,
      Green_Mask,
      Blue_Mask     : Color_Plane_Mask;
      Colormap_Size : Interfaces.C.int; -- >= 0
      Bits_Per_Rgb  : Interfaces.C.int; -- >= 0
   end record;
   pragma Convention (C, X_Visual_Info);

   type X_Visual_Info_Array is array (Positive range <>) of aliased X_Visual_Info;


   type X_Visual_Info_Mask is record
      ID            : Boolean;
      Screen        : Boolean;
      Depth         : Boolean;
      Class         : Boolean;
      Red_Mask      : Boolean;
      Green_Mask    : Boolean;
      Blue_Mask     : Boolean;
      Colormap_Size : Boolean;
      Bits_Per_Rgb  : Boolean;
   end record;
   for X_Visual_Info_Mask use record
-- UseLittleEndian
      ID            at  0 range  0 ..  0;
      Screen        at  0 range  1 ..  1;
      Depth         at  0 range  2 ..  2;
      Class         at  0 range  3 ..  3;
      Red_Mask      at  0 range  4 ..  4;
      Green_Mask    at  0 range  5 ..  5;
      Blue_Mask     at  0 range  6 ..  6;
      Colormap_Size at  0 range  7 ..  7;
      Bits_Per_Rgb  at  0 range  8 ..  8;
-- NotLittleEndian
--!       ID            at  0 range  8 ..  8;
--!       Screen        at  0 range  7 ..  7;
--!       Depth         at  0 range  6 ..  6;
--!       Class         at  0 range  5 ..  5;
--!       Red_Mask      at  0 range  4 ..  4;
--!       Green_Mask    at  0 range  3 ..  3;
--!       Blue_Mask     at  0 range  2 ..  2;
--!       Colormap_Size at  0 range  1 ..  1;
--!       Bits_Per_Rgb  at  0 range  0 ..  0;
-- EndLittleEndian
   end record;
   for X_Visual_Info_Mask'Size use 9;

   Visual_No_Mask  : constant X_Visual_Info_Mask := (others => False);
   Visual_All_Mask : constant X_Visual_Info_Mask := (others => True);


   function X_Default_Visual
     (Display   : in Display_Pointer;
      Screen    : in Screen_Number)
      return Visual_Pointer;

   function X_Default_Visual_Of_Screen (Screen : in Screen_Pointer)
      return Visual_Pointer;

   function X_Visual_ID_From_Visual (Visual : in Visual_Pointer)
      return Visual_ID;


   function X_Get_Visual_Info
     (Display        : in Display_Pointer;
      Vinfo_Mask     : in X_Visual_Info_Mask;
      Vinfo_Template : in X_Visual_Info)
      return X_Visual_Info_Array;

   function X_Match_Visual_Info
     (Display      : in  Display_Pointer;
      Screen       : in  Screen_Number;
      Depth        : in  Color_Depth;
      Class        : in  Visual_Class)
      return X_Visual_Info;


   -- -------------------------------------------------------------------------
   --
   --   XImage
   --

   -- forward declaration
   type X_Image;

   type X_Image_Pointer is access all X_Image;

   -- functions for modifying XImages
   type X_Image_Create_Image is
      access function return X_Image_Pointer;

   type X_Image_Destroy_Image is
      access procedure (Xi : in X_Image_Pointer);

   type X_Image_Get_Pixel is
      access function
         (Xi          : in X_Image_Pointer;
          X, Y        : in Integer)
          return Pixel;

   type X_Image_Put_Pixel is
      access procedure
         (Xi          : in X_Image_Pointer;
          X, Y        : in Integer;
          Pix         : in Pixel);

   type X_Image_Sub_Image is
      access function
         (Xi                    : in X_Image_Pointer;
          From_X, From_Y        : in Integer;
          Width, Height         : in Natural)
          return X_Image_Pointer;

   type X_Image_Add_Pixel is
      access procedure
        (Xi          : in X_Image_Pointer;
         Pix         : in Pixel);

   type X_Image_Funcs is record
      Create_Image  : X_Image_Create_Image;
      Destroy_Image : X_Image_Destroy_Image;
      Get_Pixel     : X_Image_Get_Pixel;
      Put_Pixel     : X_Image_Put_Pixel;
      Sub_Image     : X_Image_Sub_Image;
      Add_Pixel     : X_Image_Add_Pixel;
   end record;
   pragma Convention (C, X_Image_Funcs);

   type Image_Format is (XY_Bitmap, XY_Pixmap, Z_Pixmap);
   for Image_Format use (XY_Bitmap => 0, XY_Pixmap => 1, Z_Pixmap => 2);
   for Image_Format'Size use Interfaces.C.int'Size;

   type Data_Order is (LSB_First, MSB_First);
   for Data_Order use (LSB_First => 0, MSB_First => 1);
   for Data_Order'Size use Interfaces.C.int'Size;

   type Data_Unit is (Unit_Char, Unit_Short, Unit_Long);
   for Data_Unit use (Unit_Char => 8, Unit_Short => 16, Unit_Long => 32);
   for Data_Unit'Size use Interfaces.C.int'Size;


   type X_Image is record
      Width, Height    : Interfaces.C.int; -- >= 0
      Offset           : Interfaces.C.int;
      Format           : Image_Format;
      Data             : X_Pointer;
      Byte_Order       : Data_Order;
      Bitmap_Unit      : Data_Unit;
      Bitmap_Bit_Order : Data_Order;
      Bitmap_Pad       : Data_Unit;
      Depth            : Color_Depth;
      Bytes_Per_Line   : Interfaces.C.int; -- >= 0
      Bits_Per_Pixel   : Interfaces.C.int; -- >= 0  only for Z_Pixmap
      Red_Mask         : Color_Plane_Mask;
      Green_Mask       : Color_Plane_Mask;
      Blue_Mask        : Color_Plane_Mask;
      Ob_Data_Hook     : X_Pointer;
      F                : X_Image_Funcs;
   end record;
   pragma Convention (C, X_Image);


   function X_Create_Image
     (Display        : in Display_Pointer;
      Visual         : in Visual_Pointer;
      Depth          : in Color_Depth;
      Format         : in Image_Format;
      Offset         : in Integer;
      Data           : in X_Pointer;
      Width, Height  : in Natural;
      Bitmap_Pad     : in Data_Unit;
      Bytes_Per_Line : in Natural)
      return X_Image_Pointer;

-- UseX11R6 X11R6.3
   procedure X_Init_Image (Image : in X_Image_Pointer);
-- EndX11R6 X11R6.3

   function X_Get_Image
     (Display        : in Display_Pointer;
      D              : in Drawable_ID;
      X, Y           : in Integer;
      Width, Height  : in Natural;
      Plane_Mask     : in Color_Plane_Mask;
      Format         : in Image_Format)
      return X_Image_Pointer;

   function X_Get_Sub_Image
     (Display        : in Display_Pointer;
      D              : in Drawable_ID;
      X, Y           : in Integer;
      Width, Height  : in Natural;
      Plane_Mask     : in Color_Plane_Mask;
      Format         : in Image_Format;
      Dest_Image     : in X_Image_Pointer;
      Dest_X, Dest_Y : in Integer)
      return X_Image_Pointer;

   procedure X_Destroy_Image (Image : in X_Image_Pointer);

   function X_Get_Pixel
     (Image : in X_Image_Pointer;
      X, Y  : in Integer)
      return Pixel;

   procedure X_Put_Image
     (Display        : in Display_Pointer;
      D              : in Drawable_ID;
      GC             : in GC_Pointer;
      Image          : in X_Image_Pointer;
      Src_X, Src_Y   : in Integer;
      Dest_X, Dest_Y : in Integer;
      Width, Height  : in Natural);


   procedure X_Put_Pixel
     (Image : in X_Image_Pointer;
      X, Y  : in Integer;
      Pix   : in Pixel);

   function X_Sub_Image
     (Image : in X_Image_Pointer;
      X, Y  : in Integer;
      Width, Height : in Integer)
      return X_Image_Pointer;

   procedure X_Add_Pixel
     (Image : in X_Image_Pointer;
      Pix   : in Pixel);


   -- -------------------------------------------------------------------------
   --
   --   drawing functions
   --

   -- draw an arc, centered at (X, Y), with half-axis of length Width in
   -- X-Direction and half-axis of length Height in Y-Direction, from
   -- Angle1 to Angle2 (3 o'clock is 0)
   --
   procedure X_Draw_Arc
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension;
      Angle1,
      Angle2  : in Angle);

   procedure X_Draw_Arcs
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Arcs    : in Arc_Array);

   -- draw a line from Point (X1, Y1) to Point (X2, Y2)
   --
   procedure X_Draw_Line
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X1, Y1,
      X2, Y2  : in Position);

   type Coord_Mode is (Origin, Previous);
   for Coord_Mode use (Origin => 0, Previous => 1);

   type Shape_Mode is (Complex, Nonconvex, Convex);
   for Shape_Mode use (Complex => 0, Nonconvex => 1, Convex => 2);

   procedure X_Draw_Lines
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Mode    : in Coord_Mode);

   procedure X_Draw_Point
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position);

   procedure X_Draw_Points
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Mode    : in Coord_Mode);

   procedure X_Draw_Rectangle
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension);

   procedure X_Draw_Rectangles
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Rectangles : in Rectangle_Array);

   procedure X_Draw_Segments
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Segments   : in Segment_Array);

   procedure X_Draw_String
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Str     : in String);

   procedure X_Draw_Image_String
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Str     : in String);

   procedure X_Fill_Arc
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension;
      Angle1,
      Angle2  : in Angle);

   procedure X_Fill_Arcs
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Arcs    : in Arc_Array);

   procedure X_Fill_Polygon
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Shape   : in Shape_Mode;
      Mode    : in Coord_Mode);

   procedure X_Fill_Rectangle
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension);

   procedure X_Fill_Rectangles
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Rectangles : in Rectangle_Array);

   procedure X_Copy_Area
     (Display        : in Display_Pointer;
      Src            : in Drawable_ID;
      Dest           : in Drawable_ID;
      GC             : in GC_Pointer;
      Src_X, Src_Y   : in Position;
      Width,
      Height         : in Dimension;
      Dest_X, Dest_Y : in Position);

   procedure X_Copy_Plane
     (Display        : in Display_Pointer;
      Src            : in Drawable_ID;
      Dest           : in Drawable_ID;
      GC             : in GC_Pointer;
      Src_X, Src_Y   : in Position;
      Width,
      Height         : in Dimension;
      Dest_X, Dest_Y : in Position;
      Plane          : in Color_Plane_Mask);

   procedure X_Clear_Area
     (Display    : in Display_Pointer;
      Window     : in Window_ID;
      X, Y       : in Position;
      Width,
      Height     : in Dimension;
      Exposures  : in Boolean);

   procedure X_Clear_Window
     (Display    : in Display_Pointer;
      Window     : in Window_ID);


   -- -------------------------------------------------------------------------
   --
   --   Depth
   --

   function X_Default_Depth
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Color_Depth;

   function X_Default_Depth_Of_Screen (Screen : in Screen_Pointer)
      return Color_Depth;


   -- -------------------------------------------------------------------------
   --
   --   Screen and screen number
   --

   function X_Screen_Count (Display : in Display_Pointer)
      return Screen_Number;

   function X_Default_Screen (Display : in Display_Pointer)
      return Screen_Number;

   function X_Default_Screen_Of_Display (Display : in Display_Pointer)
      return Screen_Pointer;

   function X_Screen_Of_Display
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Screen_Pointer;

   function X_Screen_Number_Of_Screen (Screen : in Screen_Pointer)
      return Screen_Number;

   function X_Screen_Resource_String (Screen : in Screen_Pointer)
      return String;

   type Backing_Store is  (Never, When_Mapped, Always);
   for  Backing_Store use (Never => 0, When_Mapped => 1, Always => 2);
   for  Backing_Store'Size use Interfaces.C.int'Size;

   function X_Does_Backing_Store (Screen : in Screen_Pointer)
      return Backing_Store;

   function X_Does_Save_Unders (Screen : in Screen_Pointer)
      return Boolean;


   -- -------------------------------------------------------------------------
   --
   --   Colormap
   --

   function X_Default_Colormap
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Colormap_ID;

   function X_Default_Colormap_Of_Screen (Screen : in Screen_Pointer)
      return Colormap_ID;


   type Alloc_Mode is (Alloc_None, Alloc_All);
   for Alloc_Mode use (Alloc_None => 0, Alloc_All => 1);
   for Alloc_Mode'Size use Interfaces.C.int'Size;


   function X_Create_Colormap
     (Display : in Display_Pointer;
      W       : in Window_ID;
      Visual  : in Visual_Pointer;
      Alloc   : in Alloc_Mode)
      return Colormap_ID;


   function X_Copy_Colormap_And_Free
     (Display : in Display_Pointer;
      Cmap    : in Colormap_ID)
      return Colormap_ID;


   procedure X_Free_Colormap
     (Display : in Display_Pointer;
      Cmap    : in Colormap_ID);


   procedure X_Install_Colormap
     (Display : in Display_Pointer;
      Cmap    : in Colormap_ID);

   procedure X_Uninstall_Colormap
     (Display : in Display_Pointer;
      Cmap    : in Colormap_ID);


   -- -------------------------------------------------------------------------
   --
   --   Color
   --

   --
   --  Info routines
   --

   function X_Display_Cells
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Natural;

   function X_Cells_Of_Screen
     (Screen : in Screen_Pointer)
      return Natural;

   function X_Display_Planes
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Natural;

   function X_Planes_Of_Screen
     (Screen : in Screen_Pointer)
      return Natural;


   type Color_Component_Mask is record
      Do_Red   : Boolean;
      Do_Green : Boolean;
      Do_Blue  : Boolean;
   end record;
   for Color_Component_Mask use record
-- UseLittleEndian
      Do_Red   at 0 range 0 .. 0;
      Do_Green at 0 range 1 .. 1;
      Do_Blue  at 0 range 2 .. 2;
-- NotLittleEndian
--!       Do_Red   at 0 range 2 .. 2;
--!       Do_Green at 0 range 1 .. 1;
--!       Do_Blue  at 0 range 0 .. 0;
-- EndLittleEndian
   end record;
   for Color_Component_Mask'Size use 3;

   type X_Color is record
      Pix     : Pixel;
      Red,
      Green,
      Blue    : Interfaces.C.unsigned_short;
      Flags   : Color_Component_Mask;
      Pad     : Interfaces.C.signed_char;
   end record;
   for X_Color use record
      Pix      at  0 range  0 .. 31;
      Red      at  4 range  0 .. 15;
      Green    at  6 range  0 .. 15;
      Blue     at  8 range  0 .. 15;
-- UseLittleEndian
      Flags    at 10 range  0 ..  2;
-- NotLittleEndian
--!       Flags    at 10 range  5 ..  7;
-- EndLittleEndian
      Pad      at 11 range  0 ..  7;
   end record;
   for X_Color'Size use 3*Interfaces.C.int'Size;

   type X_Color_Array is array (Natural range <>) of aliased X_Color;

   type Pixel_Array_Type is array (Natural range <>) of Pixel;


   type Color_Plane_Mask_Array_Type is
      array (Natural range <>) of Color_Plane_Mask;


   X_Color_Error : exception;

   -- allocate shared color cells (i.e. color cells allocated with these
   -- routines musn't be modified)
   --
   procedure X_Alloc_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorcell_Def : in out X_Color);

   procedure X_Alloc_Named_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorname     : in     String;
      Colorcell_Def :    out X_Color;
      RGB_DB_Def    :    out X_Color);

   -- allocate private color cells
   --
   procedure X_Alloc_Color_Cells
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Contig        : in     Boolean;
      Plane_Masks   :    out Color_Plane_Mask_Array_Type;
      Pixels        :    out Pixel_Array_Type);

   procedure X_Alloc_Color_Planes
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Contig        : in     Boolean;
      Pixels        :    out Pixel_Array_Type;
      N_Red_Planes,
      N_Green_Planes,
      N_Blue_Planes : in     Natural;
      Red_Mask      :    out Color_Plane_Mask;
      Green_Mask    :    out Color_Plane_Mask;
      Blue_Mask     :    out Color_Plane_Mask);

   -- store color definition in private color cells
   --
   procedure X_Store_Color
     (Display  : in     Display_Pointer;
      C_Map    : in     Colormap_ID;
      Color    : in out X_Color);

   procedure X_Store_Colors
     (Display  : in     Display_Pointer;
      C_Map    : in     Colormap_ID;
      Colors   : in out X_Color_Array);



   -- tbd: !!!!!!!!!!!!!!!!!!!!!!!!
   --

   procedure X_Store_Name
     (Display : in Display_Pointer;
      Window  : in Window_ID;
      Name    : in String);

--  extern int XStoreName(
--      Display*		/* display */,
--      Window		/* w */,
--      _Xconst char*	/* window_name */
--  );



   procedure X_Store_Named_Color
     (Display    : in Display_Pointer;
      C_Map      : in Colormap_ID;
      Colorname  : in String;
      Pix        : in Pixel;
      Flags      : in Color_Component_Mask);


   procedure X_Lookup_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorname     : in     String;
      RGB_DB_Def    :    out X_Color;
      Hardware_Def  :    out X_Color);

   procedure X_Parse_Color
     (Display    : in     Display_Pointer;
      C_Map      : in     Colormap_ID;
      Spec       : in     String;
      RGB_DB_Def :    out X_Color);

   procedure X_Query_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorcell_Def : in out X_Color);

   procedure X_Query_Colors
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorcell_Def : in out X_Color_Array);

   -- free color cells obtained by X_Alloc_Color, X_Alloc_Named_Color,
   -- X_Alloc_Color_Cells and X_Alloc_Color_Planes
   --
   procedure X_Free_Colors
     (Display  : in Display_Pointer;
      C_Map    : in Colormap_ID;
      Pixels   : in Pixel_Array_Type;
      Planes   : in Color_Plane_Mask := Color_Plane_Mask'(0));



   -- -------------------------------------------------------------------------
   --
   -- define how to handle resources after the program has finished
   --
   type Close_Mode_Type is (Destroy_All, Retain_Permanent, Retain_Temporary);
   for Close_Mode_Type use (Destroy_All => 0, Retain_Permanent => 1, Retain_Temporary => 2);
   for Close_Mode_Type'Size use Interfaces.C.int'Size;

   procedure X_Set_Close_Down_Mode
     (Display    : in Display_Pointer;
      Close_Mode : in Close_Mode_Type := Destroy_All);


   -- -------------------------------------------------------------------------
   --
   -- detroy a client or its remaining resources
   --
   -- special XID. When passed to X_Kill_Client all temporary resources
   -- will be killed
   All_Temporary : constant XID;

   procedure X_Kill_Client
     (Display   : in Display_Pointer;
      Resource  : in X_Lib.XID);


   -- -------------------------------------------------------------------------
   --
   --   Window
   --


   procedure X_Add_To_Save_Set
     (Display : in Display_Pointer;
      Window  : in Window_ID);

   procedure X_Remove_From_Save_Set
     (Display : in Display_Pointer;
      Window  : in Window_ID);

   -- -------------------------------------------------------------------------
   --
   -- function returning root windows
   --
   function X_Root_Window
     (Display        : in Display_Pointer;
      Screen_No      : in Screen_Number)
      return Window_ID;

   function X_Default_Root_Window (Display : in Display_Pointer)
      return Window_ID;

   function X_Root_Window_Of_Screen (Screen : in Screen_Pointer)
      return Window_ID;

   -- -------------------------------------------------------------------------
   --
   -- Window appearance
   --

   procedure X_Set_Window_Background
     (Display          : in Display_Pointer;
      W                : in Window_ID;
      Background_Pixel : in Pixel);

   procedure X_Set_Window_Background_Pixmap
     (Display           : in Display_Pointer;
      W                 : in Window_ID;
      Background_Pixmap : in Pixmap_ID);


   procedure X_Set_Window_Border
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Border_Pixel : in Pixel);

   procedure X_Set_Window_Border_Pixmap
     (Display       : in Display_Pointer;
      W             : in Window_ID;
      Border_Pixmap : in Pixmap_ID);


   procedure X_Set_Window_Border_Width
     (Display : in Display_Pointer;
      W       : in Window_ID;
      Width   : in Natural);


   procedure X_Set_Window_Colormap
     (Display : in Display_Pointer;
      W       : in Window_ID;
      Cmap    : in Colormap_ID);


   procedure X_Iconify_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Screen_No : in Screen_Number);


   procedure X_Withdraw_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Screen_No : in Screen_Number);

   procedure X_Raise_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Lower_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Destroy_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Destroy_Subwindows
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Map_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Unmap_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Map_Raised
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Map_Subwindows
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Unmap_Subwindows
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   type Circulate_Direction is  (Raise_Lowest, Lower_Highest);
   for  Circulate_Direction use (Raise_Lowest => 0, Lower_Highest => 1);
   for  Circulate_Direction'Size use Interfaces.C.int'Size;

   procedure X_Circulate_Subwindows
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Direction : in Circulate_Direction);

   procedure X_Circulate_Subwindows_Up
     (Display   : in Display_Pointer;
      W         : in Window_ID);

   procedure X_Circulate_Subwindows_Down
     (Display   : in Display_Pointer;
      W         : in Window_ID);



   procedure X_Restack_Windows
     (Display     : in Display_Pointer;
      Windows     : in Window_ID_Array_Type);


   -- obtain geometry of window or drawable
   --
   procedure X_Get_Geometry
     (Display       : in     Display_Pointer;
      D             : in     Drawable_ID;
      Root_Win      :    out Window_ID;
      X, Y          :    out Integer;
      Width, Height :    out Natural;
      Border_Width  :    out Natural;
      Depth         :    out Color_Depth);


   -- -------------------------------------------------------------------------
   --
   -- predefined Pixel values
   --
   function X_Black_Pixel (Display        : in Display_Pointer;
                           Screen_No      : in Screen_Number) return Pixel;

   function X_White_Pixel (Display        : in Display_Pointer;
                           Screen_No      : in Screen_Number) return Pixel;

   function X_Black_Pixel_Of_Screen (Screen : in Screen_Pointer) return Pixel;

   function X_White_Pixel_Of_Screen (Screen : in Screen_Pointer) return Pixel;

   function X_All_Planes return Color_Plane_Mask;


   -- -------------------------------------------------------------------------
   --
   --  Font routines
   --

   type Char_Struct is record
      Lbearing   : Dimension;       --  origin to left edge of raster
      Rbearing   : Dimension;       --  origin to right edge of raster
      Width      : Dimension;       --  advance to next char's origin
      Ascent     : Dimension;       --  baseline to top edge of raster
      Descent    : Dimension;       --  baseline to bottom edge of raster
      Attributes : Interfaces.C.unsigned_short;  --  per char flags
   end record;
   type Char_Struct_Access is access Char_Struct;


   function X_Load_Font (Display : in Display_Pointer;
                         Name    : in String)
      return Font_ID;


   function X_Load_Query_Font (Display : in Display_Pointer;
                               Name    : in String)
      return X_Font_Struct_Pointer;

   type Draw_Direction is (Left_To_Right, Right_To_Left, Direction_Change);

   procedure X_Query_Text_Extents
     (Display   : in Display_Pointer;
      FontId    : in Font_Id;
      Text      : in String;
      Direction : out Draw_Direction;
      Ascent    : out Dimension;
      Descent   : out Dimension;
      Overall   : out Char_Struct);


   -- -------------------------------------------------------------------------
   --
   --  Pixmaps
   --

   X_Error_Open_Failed  : exception;
   X_Error_File_Invalid : exception;
   X_Error_No_Memory    : exception;


   function X_Create_Pixmap
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      Width,
      Height  : in Dimension;
      Depth   : in Color_Depth)
      return Pixmap_ID;

   procedure X_Free_Pixmap
     (Display : in Display_Pointer;
      Pixmap  : in Pixmap_ID);


   -- general bitmap type (1 bit/pixel):
   -- the bitmap has the size 'Length (1) by 'Length (2)*8
   --
   type Bitmap_Type is
      array (Natural range <>, Natural range <>) of Bits_8_Type;

   function X_Create_Bitmap_From_Data
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      Data    : in Bitmap_Type)
      return Pixmap_ID;


   function X_Create_Pixmap_From_Bitmap_Data
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      Data    : in Bitmap_Type;
      FG, BG  : in Pixel;
      Depth   : in Color_Depth)
      return Pixmap_ID;


   procedure X_Read_Bitmap_File
     (Display     : in  Display_Pointer;
      Drawable    : in  Drawable_ID;
      Filename    : in  String;
      Width       : out Dimension;
      Height      : out Dimension;
      Bitmap      : out Pixmap_ID;
      X_Hot       : out Position;
      Y_Hot       : out Position);


   procedure X_Write_Bitmap_File
     (Display     : in Display_Pointer;
      Filename    : in String;
      Bitmap      : in Pixmap_ID;
      Width       : in Dimension;
      Height      : in Dimension;
      X_Hot       : in Position := Position'(-1);
      Y_Hot       : in Position := Position'(-1));



   -- -------------------------------------------------------------------------
   --
   --  I N F O
   --

   -- -------------------------------------------------------------------------
   --
   --  best size
   --
   procedure X_Query_Best_Stipple
     (Display     : in     Display_Pointer;
      D           : in     Drawable_ID;
      Width,
      Height      : in     Dimension;
      Best_Width,
      Best_Height :    out Dimension);

   procedure X_Query_Best_Tile
     (Display     : in     Display_Pointer;
      D           : in     Drawable_ID;
      Width,
      Height      : in     Dimension;
      Best_Width,
      Best_Height :    out Dimension);


   -- -------------------------------------------------------------------------
   --
   --  fonts
   --
   function X_List_Fonts (Display   : in X_Lib.Display_Pointer;
                          Pattern   : in String;
                          Max_Names : in Natural)
      return String_List.Element_Access_List;


   -- -------------------------------------------------------------------------
   --
   --  pixmap formats
   --
   type X_Pixmap_Format_Values is record
      Depth          : Color_Depth;
      Bits_Per_Pixel : Interfaces.C.int; -- >= 0
      Scanline_Pad   : Interfaces.C.int; -- >= 0
   end record;
   pragma Convention (C, X_Pixmap_Format_Values);

   type X_Pixmap_Format_Values_Array is
      array (Natural range <>) of aliased X_Pixmap_Format_Values;

   function X_List_Pixmap_Formats (Display : in Display_Pointer)
      return X_Pixmap_Format_Values_Array;


   -- -------------------------------------------------------------------------
   --
   --  installed colormaps
   --
   type Colormap_ID_Array is
      array (Natural range <>) of aliased Colormap_ID;

   function X_List_Installed_Colormaps
     (Display : in Display_Pointer;
      W       : in Window_ID)
      return Colormap_ID_Array;


   -- -------------------------------------------------------------------------
   --
   --  atoms
   --
   type Atom_Array is
      array (Natural range <>) of aliased Atom;

   function X_List_Properties
     (Display : in Display_Pointer;
      W       : in Window_ID)
      return Atom_Array;


   -- -------------------------------------------------------------------------
   --
   --  depth of screen
   --
   type Color_Depth_Array is
      array (Natural range <>) of aliased Color_Depth;

   function X_List_Depths
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Color_Depth_Array;


   -- -------------------------------------------------------------------------
   --
   --  dimension of screen
   --
   function X_Display_Height
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Integer;

   function X_Display_Height_MM
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Integer;

   function X_Height_Of_Screen (Screen : in Screen_Pointer) return Integer;

   function X_Height_MM_Of_Screen (Screen : in Screen_Pointer) return Integer;

   function X_Display_Width
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Integer;

   function X_Display_Width_MM
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Integer;

   function X_Width_Of_Screen (Screen : in Screen_Pointer) return Integer;

   function X_Width_MM_Of_Screen (Screen : in Screen_Pointer) return Integer;



   -- -------------------------------------------------------------------------
   --
   --  bit/byte order
   --
   function X_Image_Byte_Order (Display : in Display_Pointer)
      return Data_Order;

   function X_Bitmap_Bit_Order (Display : in Display_Pointer)
      return Data_Order;

   function X_Bitmap_Pad (Display : in Display_Pointer)
      return Integer;

   function X_Bitmap_Unit (Display : in Display_Pointer)
      return Integer;


-- ----------------------------------------------------------------------------
--
--  I N T E R N A T I O N A L I Z A T I O N
--
   type Font_Set_Extents_Type is record
      Max_Ink_Extent     : Rectangle;
      Max_Logical_Extent : Rectangle;
   end record;


   function X_Extents_Of_Font_Set
     (Font_Set : in Font_Set_Type)
      return Font_Set_Extents_Type;



-- UseX11R6 X11R6.3
-- ----------------------------------------------------------------------------
--
--  T H R E A D S
--

   -- from Release 6 onward
   X_Error_No_Threads : exception;

   procedure X_Init_Threads;

   procedure X_Lock_Display   (Display : in Display_Pointer);
   procedure X_Unlock_Display (Display : in Display_Pointer);

-- EndX11R6 X11R6.3

   -- -------------------------------------------------------------------------
   --
   --   E V E N T _ M A S K
   --

   -- this type is defined in the Intrinsics, but the defines are
   -- already defined in X11/X.h, therefore I define it here
   --
   type Event_Mask is record
      Key_Press              : Boolean;
      Key_Release            : Boolean;
      Button_Press           : Boolean;
      Button_Release         : Boolean;
      Enter_Window           : Boolean;
      Leave_Window           : Boolean;
      Pointer_Motion         : Boolean;
      Pointer_Motion_Hint    : Boolean;
      Button1_Motion         : Boolean;
      Button2_Motion         : Boolean;
      Button3_Motion         : Boolean;
      Button4_Motion         : Boolean;
      Button5_Motion         : Boolean;
      Button_Motion          : Boolean;
      Keymap_State           : Boolean;
      Exposure               : Boolean;
      Visibility_Change      : Boolean;
      Structure_Notify       : Boolean;
      Resize_Redirect        : Boolean;
      Substructure_Notify    : Boolean;
      Substructure_Redirect  : Boolean;
      Focus_Change           : Boolean;
      Property_Change        : Boolean;
      Colormap_Change        : Boolean;
      Owner_Grab_Button      : Boolean;
      Pad_25, Pad_26, Pad_27, Pad_28, Pad_29, Pad_30, Pad_31 : Boolean;
   end record;
   for Event_Mask use record
-- UseLittleEndian
      Key_Press              at 0 range  0 ..  0;
      Key_Release            at 0 range  1 ..  1;
      Button_Press           at 0 range  2 ..  2;
      Button_Release         at 0 range  3 ..  3;
      Enter_Window           at 0 range  4 ..  4;
      Leave_Window           at 0 range  5 ..  5;
      Pointer_Motion         at 0 range  6 ..  6;
      Pointer_Motion_Hint    at 0 range  7 ..  7;
      Button1_Motion         at 0 range  8 ..  8;
      Button2_Motion         at 0 range  9 ..  9;
      Button3_Motion         at 0 range 10 .. 10;
      Button4_Motion         at 0 range 11 .. 11;
      Button5_Motion         at 0 range 12 .. 12;
      Button_Motion          at 0 range 13 .. 13;
      Keymap_State           at 0 range 14 .. 14;
      Exposure               at 0 range 15 .. 15;
      Visibility_Change      at 0 range 16 .. 16;
      Structure_Notify       at 0 range 17 .. 17;
      Resize_Redirect        at 0 range 18 .. 18;
      Substructure_Notify    at 0 range 19 .. 19;
      Substructure_Redirect  at 0 range 20 .. 20;
      Focus_Change           at 0 range 21 .. 21;
      Property_Change        at 0 range 22 .. 22;
      Colormap_Change        at 0 range 23 .. 23;
      Owner_Grab_Button      at 0 range 24 .. 24;
      Pad_25                 at 0 range 25 .. 25;
      Pad_26                 at 0 range 26 .. 26;
      Pad_27                 at 0 range 27 .. 27;
      Pad_28                 at 0 range 28 .. 28;
      Pad_29                 at 0 range 29 .. 29;
      Pad_30                 at 0 range 30 .. 30;
      Pad_31                 at 0 range 31 .. 31;
-- NotLittleEndian
--!       Key_Press              at 0 range 31 .. 31;
--!       Key_Release            at 0 range 30 .. 30;
--!       Button_Press           at 0 range 29 .. 29;
--!       Button_Release         at 0 range 28 .. 28;
--!       Enter_Window           at 0 range 27 .. 27;
--!       Leave_Window           at 0 range 26 .. 26;
--!       Pointer_Motion         at 0 range 25 .. 25;
--!       Pointer_Motion_Hint    at 0 range 24 .. 24;
--!       Button1_Motion         at 0 range 23 .. 23;
--!       Button2_Motion         at 0 range 22 .. 22;
--!       Button3_Motion         at 0 range 21 .. 21;
--!       Button4_Motion         at 0 range 20 .. 20;
--!       Button5_Motion         at 0 range 19 .. 19;
--!       Button_Motion          at 0 range 18 .. 18;
--!       Keymap_State           at 0 range 17 .. 17;
--!       Exposure               at 0 range 16 .. 16;
--!       Visibility_Change      at 0 range 15 .. 15;
--!       Structure_Notify       at 0 range 14 .. 14;
--!       Resize_Redirect        at 0 range 13 .. 13;
--!       Substructure_Notify    at 0 range 12 .. 12;
--!       Substructure_Redirect  at 0 range 11 .. 11;
--!       Focus_Change           at 0 range 10 .. 10;
--!       Property_Change        at 0 range  9 ..  9;
--!       Colormap_Change        at 0 range  8 ..  8;
--!       Owner_Grab_Button      at 0 range  7 ..  7;
--!       Pad_25                 at 0 range  6 ..  6;
--!       Pad_26                 at 0 range  5 ..  5;
--!       Pad_27                 at 0 range  4 ..  4;
--!       Pad_28                 at 0 range  3 ..  3;
--!       Pad_29                 at 0 range  2 ..  2;
--!       Pad_30                 at 0 range  1 ..  1;
--!       Pad_31                 at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   for Event_Mask'Size use Interfaces.C.long'Size;

   No_Events   : constant Event_Mask := (others => False);
   All_Events  : constant Event_Mask := (others => True);



   -- Keymap
   --

   function X_Query_Keymap (Display : Display_Pointer;
                            keys_return : interfaces.C.strings.chars_ptr) return interfaces.C.Int;

   pragma import (C, X_Query_Keymap, "XQueryKeymap");

--  extern int XQueryKeymap(
--      Display*		/* display */,
--      char [32]		/* keys_return */
--  );




   -- -------------------------------------------------------------------------
   --
   --   Window Attributes
   --

   type Gravity_Type is (Forget,
                         North_West, North, North_East,
                         West, Center, East,
                         South_West, South, South_East,
                         Static);
   for Gravity_Type use (Forget => 0,
                         North_West => 1, North => 2, North_East => 3,
                         West => 4, Center => 5, East => 6,
                         South_West => 7, South => 8, South_East => 9,
                         Static => 10);
   for Gravity_Type'Size use Interfaces.C.int'Size;

   Unmap_Gravity : constant Gravity_Type := Forget;


   type Set_Window_Attributes_Type is record
      Background_Pixmap     : Pixmap_ID;
      Background_Pixel      : Pixel;
      Border_Pixmap         : Pixmap_ID;
      Border_Pixel          : Pixel;
      Bit_Gravity           : Gravity_Type;
      Win_Gravity           : Gravity_Type;
      Backing_Store         : X_Lib.Backing_Store;
      Backing_Planes        : Color_Plane_Mask;
      Backing_Pixel         : Pixel;
      Save_Under            : Boolean;
      Ev_Mask               : Event_Mask;
      Do_Not_Propagate_Mask : Event_Mask;
      Override_Redirect     : Boolean;
      Colormap              : Colormap_ID;
      Cursor                : Cursor_ID;
   end record;
   for Set_Window_Attributes_Type use record
      Background_Pixmap     at  0 range  0 .. 31;
      Background_Pixel      at  4 range  0 .. 31;
      Border_Pixmap         at  8 range  0 .. 31;
      Border_Pixel          at 12 range  0 .. 31;
      Bit_Gravity           at 16 range  0 .. 31;
      Win_Gravity           at 20 range  0 .. 31;
      Backing_Store         at 24 range  0 .. 31;
      Backing_Planes        at 28 range  0 .. 31;
      Backing_Pixel         at 32 range  0 .. 31;
      Save_Under            at 36 range  0 .. 31;
      Ev_Mask               at 40 range  0 .. 31;
      Do_Not_Propagate_Mask at 44 range  0 .. 31;
      Override_Redirect     at 48 range  0 .. 31;
      Colormap              at 52 range  0 .. 31;
      Cursor                at 56 range  0 .. 31;
   end record;
   for Set_Window_Attributes_Type'Size use 15 * Interfaces.C.int'Size;
   pragma Convention (C, Set_Window_Attributes_Type);

   type Set_Window_Attributes_Mask is record
      Back_Pixmap       : Boolean;
      Back_Pixel        : Boolean;
      Border_Pixmap     : Boolean;
      Border_Pixel      : Boolean;
      Bit_Gravity       : Boolean;
      Win_Gravity       : Boolean;
      Backing_Store     : Boolean;
      Backing_Planes    : Boolean;
      Backing_Pixel     : Boolean;
      Override_Redirect : Boolean;
      Save_Under        : Boolean;
      Ev_Mask           : Boolean;
      Dont_Propagate    : Boolean;
      Colormap          : Boolean;
      Cursor            : Boolean;
   end record;
   for Set_Window_Attributes_Mask use record
-- UseLittleEndian
      Back_Pixmap       at 0 range  0 ..  0;
      Back_Pixel        at 0 range  1 ..  1;
      Border_Pixmap     at 0 range  2 ..  2;
      Border_Pixel      at 0 range  3 ..  3;
      Bit_Gravity       at 0 range  4 ..  4;
      Win_Gravity       at 0 range  5 ..  5;
      Backing_Store     at 0 range  6 ..  6;
      Backing_Planes    at 0 range  7 ..  7;
      Backing_Pixel     at 0 range  8 ..  8;
      Override_Redirect at 0 range  9 ..  9;
      Save_Under        at 0 range 10 .. 10;
      Ev_Mask           at 0 range 11 .. 11;
      Dont_Propagate    at 0 range 12 .. 12;
      Colormap          at 0 range 13 .. 13;
      Cursor            at 0 range 14 .. 14;
-- NotLittleEndian
--!       Back_Pixmap       at 0 range 14 .. 14;
--!       Back_Pixel        at 0 range 13 .. 13;
--!       Border_Pixmap     at 0 range 12 .. 12;
--!       Border_Pixel      at 0 range 11 .. 11;
--!       Bit_Gravity       at 0 range 10 .. 10;
--!       Win_Gravity       at 0 range  9 ..  9;
--!       Backing_Store     at 0 range  8 ..  8;
--!       Backing_Planes    at 0 range  7 ..  7;
--!       Backing_Pixel     at 0 range  6 ..  6;
--!       Override_Redirect at 0 range  5 ..  5;
--!       Save_Under        at 0 range  4 ..  4;
--!       Ev_Mask           at 0 range  3 ..  3;
--!       Dont_Propagate    at 0 range  2 ..  2;
--!       Colormap          at 0 range  1 ..  1;
--!       Cursor            at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   for Set_Window_Attributes_Mask'Size use 15;


   procedure X_Change_Window_Attributes
     (Display    : in Display_Pointer;
      W          : in Window_ID;
      Valuemask  : in Set_Window_Attributes_Mask;
      Attributes : in Set_Window_Attributes_Type);

   function X_Create_Window
     (Display       : in Display_Pointer;
      Parent        : in Window_ID;
      X, Y          : in Position;
      Width, Height : in Dimension;
      Border_Width  : in Natural;
      Depth         : in Color_Depth;
      Class         : in Window_Class;
      Visual        : in Visual_Pointer;
      Valuemask     : in Set_Window_Attributes_Mask;
      Attributes    : in Set_Window_Attributes_Type)
      return Window_ID;

   function X_Create_Simple_Window
     (Display	    : in Display_Pointer;
      Parent	    : in Window_ID;
      X, Y	    : in Position;
      Width, Height : in Dimension;
      Border_Width  : in Natural;
      Border	    : in Pixel;
      Background    : in Pixel)
      return Window_ID;


   type Window_Stacking_Mode_Type is  (Above, Below, Top_If, Bottom_If, Opposite);
   for  Window_Stacking_Mode_Type use (Above => 0, Below => 1, Top_If => 2,
                            Bottom_If => 3, Opposite => 4);
   for Window_Stacking_Mode_Type'Size use Interfaces.C.int'Size;

   type Window_Changes_Type is record
      X, Y         : Interfaces.C.int;
      Width,
      Height       : Interfaces.C.int;
      Border_Width : Interfaces.C.int;
      Sibling      : Window_ID;
      Stack_Mode   : Window_Stacking_Mode_Type;
   end record;
   pragma Convention (C, Window_Changes_Type);

   type Window_Changes_Mask is record
      X            : Boolean;
      Y            : Boolean;
      Width        : Boolean;
      Height       : Boolean;
      Border_Width : Boolean;
      Sibling      : Boolean;
      Stack_Mode   : Boolean;
   end record;
   for Window_Changes_Mask use record
-- UseLittleEndian
      X             at 0 range 0 .. 0;
      Y             at 0 range 1 .. 1;
      Width         at 0 range 2 .. 2;
      Height        at 0 range 3 .. 3;
      Border_Width  at 0 range 4 .. 4;
      Sibling       at 0 range 5 .. 5;
      Stack_Mode    at 0 range 6 .. 6;
-- NotLittleEndian
--!       X             at 0 range 6 .. 6;
--!       Y             at 0 range 5 .. 5;
--!       Width         at 0 range 4 .. 4;
--!       Height        at 0 range 3 .. 3;
--!       Border_Width  at 0 range 2 .. 2;
--!       Sibling       at 0 range 1 .. 1;
--!       Stack_Mode    at 0 range 0 .. 0;
-- EndLittleEndian
   end record;
   pragma Pack (Window_Changes_Mask);
   for Window_Changes_Mask'Size use 7;



   function X_Connection_Number (Display : in Display_Pointer) return Integer;
--  extern int XConnectionNumber(
--      Display*		/* display */
--  );



   procedure X_Configure_Window
     (Display    : in Display_Pointer;
      W          : in Window_ID;
      Value_Mask : in Window_Changes_Mask;
      Values     : in Window_Changes_Type);

   procedure X_Resize_Window
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      Width,
      Height       : in Dimension);

   procedure X_Move_Window
     (Display      : in Display_Pointer;
      W            : in Window_ID;
      X,
      Y            : in Dimension);


   -- -------------------------------------------------------------------------
   --
   --  G R A B   F U N C T I O N S
   --
   X_Error_Already_Grabbed   : exception;
   X_Error_Grab_Invalid_Time : exception;
   X_Error_Grab_Not_Viewable : exception;
   X_Error_Grab_Frozen       : exception;


   type Grab_Mode_Type is (Sync, Async);
   for Grab_Mode_Type use (Sync => 0, Async => 1);
   for Grab_Mode_Type'Size use Interfaces.C.int'Size;

   type Button_Type is (Any_Button, Button_1, Button_2, Button_3, Button_4, Button_5);
   for Button_Type use (Any_Button => 0, Button_1 => 1, Button_2 => 2,
                        Button_3   => 3, Button_4 => 4, Button_5 => 5);
   for Button_Type'Size use Interfaces.C.unsigned'Size;


   type Modifiers_Mask_Type is record
      Shift      : Boolean;
      Lock       : Boolean;
      Control    : Boolean;
      Modifier_1 : Boolean;
      Modifier_2 : Boolean;
      Modifier_3 : Boolean;
      Modifier_4 : Boolean;
      Modifier_5 : Boolean;
   end record;
   for Modifiers_Mask_Type use record
-- UseLittleEndian
      Shift      at 0 range 0 .. 0;
      Lock       at 0 range 1 .. 1;
      Control    at 0 range 2 .. 2;
      Modifier_1 at 0 range 3 .. 3;
      Modifier_2 at 0 range 4 .. 4;
      Modifier_3 at 0 range 5 .. 5;
      Modifier_4 at 0 range 6 .. 6;
      Modifier_5 at 0 range 7 .. 7;
-- NotLittleEndian
--!       Shift      at 0 range 7 .. 7;
--!       Lock       at 0 range 6 .. 6;
--!       Control    at 0 range 5 .. 5;
--!       Modifier_1 at 0 range 4 .. 4;
--!       Modifier_2 at 0 range 3 .. 3;
--!       Modifier_3 at 0 range 2 .. 2;
--!       Modifier_4 at 0 range 1 .. 1;
--!       Modifier_5 at 0 range 0 .. 0;
-- EndLittleEndian
   end record;
   pragma Pack (Modifiers_Mask_Type);
   for Modifiers_Mask_Type'Size use 8;


   procedure X_Grab_Button
     (Display       : in Display_Pointer;
      Button        : in Button_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Ev_Mask       : in Event_Mask;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type;
      Confine_To    : in Window_ID := Null_Window_ID;
      Cursor        : in Cursor_ID := Null_Cursor_ID);


   procedure X_Ungrab_Button
     (Display       : in Display_Pointer;
      Button        : in Button_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID);


   procedure X_Grab_Pointer
     (Display       : in Display_Pointer;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Ev_Mask       : in Event_Mask;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type;
      Confine_To    : in Window_ID := Null_Window_ID;
      Cursor        : in Cursor_ID := Null_Cursor_ID;
      Time          : in Server_Time);


   procedure X_Ungrab_Pointer
     (Display : in Display_Pointer;
      Time    : in Server_Time);


   procedure X_Change_Active_Pointer_Grab
     (Display       : in Display_Pointer;
      Ev_Mask       : in Event_Mask;
      Cursor        : in Cursor_ID := Null_Cursor_ID;
      Time          : in Server_Time);


   procedure X_Grab_Key
     (Display       : in Display_Pointer;
      Keycode       : in Key_Code_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type);


   procedure X_Ungrab_Key
     (Display       : in Display_Pointer;
      Keycode       : in Key_Code_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID);


   procedure X_Grab_Keyboard
     (Display       : in Display_Pointer;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type;
      Time          : in Server_Time);


   procedure X_Ungrab_Keyboard
     (Display : in Display_Pointer;
      Time    : in Server_Time);


   procedure X_Grab_Server (Display : in Display_Pointer);


   procedure X_Ungrab_Server (Display : in Display_Pointer);


-- ----------------------------------------------------------------------------
--
--  EVENTS
--


   type Event_Type is new Interfaces.C.int;

   Reserved_For_Errors  : constant Event_Type := 0;
   Reserved_For_Replies : constant Event_Type := 1;
   Key_Press            : constant Event_Type := 2;
   Key_Release          : constant Event_Type := 3;
   Button_Press         : constant Event_Type := 4;
   Button_Release       : constant Event_Type := 5;
   Motion_Notify        : constant Event_Type := 6;
   Enter_Notify         : constant Event_Type := 7;
   Leave_Notify         : constant Event_Type := 8;
   Focus_In             : constant Event_Type := 9;
   Focus_Out            : constant Event_Type := 10;
   Keymap_Notify        : constant Event_Type := 11;
   Expose               : constant Event_Type := 12;
   Graphics_Expose      : constant Event_Type := 13;
   No_Expose            : constant Event_Type := 14;
   Visibility_Notify    : constant Event_Type := 15;
   Create_Notify        : constant Event_Type := 16;
   Destroy_Notify       : constant Event_Type := 17;
   Unmap_Notify         : constant Event_Type := 18;
   Map_Notify           : constant Event_Type := 19;
   Map_Request          : constant Event_Type := 20;
   Reparent_Notify      : constant Event_Type := 21;
   Configure_Notify     : constant Event_Type := 22;
   Configure_Request    : constant Event_Type := 23;
   Gravity_Notify       : constant Event_Type := 24;
   Resize_Request       : constant Event_Type := 25;
   Circulate_Notify     : constant Event_Type := 26;
   Circulate_Request    : constant Event_Type := 27;
   Property_Notify      : constant Event_Type := 28;
   Selection_Clear      : constant Event_Type := 29;
   Selection_Request    : constant Event_Type := 30;
   Selection_Notify     : constant Event_Type := 31;
   Colormap_Notify      : constant Event_Type := 32;
   Client_Message       : constant Event_Type := 33;
   Mapping_Notify       : constant Event_Type := 34;


   type Modifier_And_Button_Mask is record
      Shift   : Boolean;
      Lock    : Boolean;
      Control : Boolean;
      Mod1    : Boolean;
      Mod2    : Boolean;
      Mod3    : Boolean;
      Mod4    : Boolean;
      Mod5    : Boolean;
      Button1 : Boolean;
      Button2 : Boolean;
      Button3 : Boolean;
      Button4 : Boolean;
      Button5 : Boolean;
   end record;
   for Modifier_And_Button_Mask use record
-- UseLittleEndian
      Shift   at 0 range  0 ..  0;
      Lock    at 0 range  1 ..  1;
      Control at 0 range  2 ..  2;
      Mod1    at 0 range  3 ..  3;
      Mod2    at 0 range  4 ..  4;
      Mod3    at 0 range  5 ..  5;
      Mod4    at 0 range  6 ..  6;
      Mod5    at 0 range  7 ..  7;
      Button1 at 0 range  8 ..  8;
      Button2 at 0 range  9 ..  9;
      Button3 at 0 range 10 .. 10;
      Button4 at 0 range 11 .. 11;
      Button5 at 0 range 12 .. 12;
-- NotLittleEndian
--!       Shift   at 0 range 12 .. 12;
--!       Lock    at 0 range 11 .. 11;
--!       Control at 0 range 10 .. 10;
--!       Mod1    at 0 range  9 ..  9;
--!       Mod2    at 0 range  8 ..  8;
--!       Mod3    at 0 range  7 ..  7;
--!       Mod4    at 0 range  6 ..  6;
--!       Mod5    at 0 range  5 ..  5;
--!       Button1 at 0 range  4 ..  4;
--!       Button2 at 0 range  3 ..  3;
--!       Button3 at 0 range  2 ..  2;
--!       Button4 at 0 range  1 ..  1;
--!       Button5 at 0 range  0 ..  0;
-- EndLittleEndian
   end record;
   for Modifier_And_Button_Mask'Size use 13;


   subtype Physical_Button_Type is Button_Type range Button_1 .. Button_5;

   type X_Button_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Root           : Window_ID;
      SubWindow      : Window_ID;
      Time           : Server_Time;
      X, Y           : Position;
      X_Root, Y_Root : Position;
      State          : Modifier_And_Button_Mask;
      Button         : Physical_Button_Type;
      Same_Screen    : Boolean;
   end record;


   subtype X_Button_Pressed_Event  is X_Button_Event;
   subtype X_Button_Released_Event is X_Button_Event;

   type Circulate_Placement is (Place_On_Top, Place_On_Bottom);
   for  Circulate_Placement use (Place_On_Top => 0, Place_On_Bottom => 1);
   for  Circulate_Placement'Size use Interfaces.C.int'Size;

   type X_Circulate_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Event          : Window_ID;
      Window         : Window_ID;
      Place          : Circulate_Placement;
   end record;


   type X_Circulate_Request_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Parent         : Window_ID;
      Window         : Window_ID;
      Place          : Circulate_Placement;
   end record;


   type Client_Message_Format is (Char_Format, Short_Format, Long_Format);
   for  Client_Message_Format use (Char_Format  => 8,
                                   Short_Format => 16,
                                   Long_Format  => 32);
   for  Client_Message_Format'Size use Interfaces.C.int'Size;

   type X_Client_Message_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Message_Type   : Atom;
      Format         : Client_Message_Format;
      Data           : Long_Array (1 .. 5);
   end record;


   type Colormap_State is  (Uninstalled, Installed);
   for  Colormap_State use (Uninstalled => 0, Installed   => 1);
   for  Colormap_State'Size use Interfaces.C.int'Size;

   type X_Colormap_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Colormap       : Colormap_ID;
      Is_New         : Boolean;
      State          : Colormap_State;
   end record;


   type X_Configure_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Event             : Window_ID;
      Window            : Window_ID;
      X, Y              : Position;
      Width, Height     : Dimension;
      Border_Width      : Dimension;
      Window_Above      : Window_ID;
      Override_Redirect : Boolean;
   end record;


   type X_Configure_Request_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Parent            : Window_ID;
      Window            : Window_ID;
      X, Y              : Position;
      Width, Height     : Dimension;
      Border_Width      : Dimension;
      Window_Above      : Window_ID;
      Detail            : Window_Stacking_Mode_Type;
      Value_Mask        : Window_Changes_Mask;
   end record;


   type X_Create_Window_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Parent            : Window_ID;
      Window            : Window_ID;
      X, Y              : Position;
      Width, Height     : Dimension;
      Border_Width      : Dimension;
      Window_Above      : Window_ID;
      Override_Redirect : Boolean;
   end record;


   type X_Destroy_Window_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Event             : Window_ID;
      Window            : Window_ID;
   end record;

   type Notify_Mode is (Notify_Normal, Notify_Grab, Notify_Ungrab);
   for  Notify_Mode use (Notify_Normal => 0, Notify_Grab => 1, Notify_Ungrab => 2);
   for  Notify_Mode'Size use Interfaces.C.int'Size;

   type Notify_Detail is (Notify_Ancestor,
                          Notify_Virtual,
                          Notify_Inferior,
                          Notify_Nonlinear,
                          Notify_Nonlinear_Virtual,
                          Notify_Pointer,
                          Notify_Pointer_Root,
                          Notify_Detail_None);
   for  Notify_Detail use (Notify_Ancestor => 0,
                           Notify_Virtual => 1,
                           Notify_Inferior => 2,
                           Notify_Nonlinear => 3,
                           Notify_Nonlinear_Virtual => 4,
                           Notify_Pointer => 5,
                           Notify_Pointer_Root => 6,
                           Notify_Detail_None => 7);
   for  Notify_Detail'Size use Interfaces.C.int'Size;

   subtype Crossing_Detail is Notify_Detail range Notify_Ancestor .. Notify_Nonlinear_Virtual;

   type X_Crossing_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Root           : Window_ID;
      SubWindow      : Window_ID;
      Time           : Server_Time;
      X, Y           : Position;
      X_Root, Y_Root : Position;
      Mode           : Notify_Mode;
      Detail         : Crossing_Detail;
      Same_Screen    : Boolean;
      Focus          : Boolean;
      State          : Modifier_And_Button_Mask;
   end record;

   subtype X_Enter_Window_Event is X_Crossing_Event;
   subtype X_Leave_Window_Event is X_Crossing_Event;

   type X_Focus_Change_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Mode           : Notify_Mode;
      Detail         : Notify_Detail;
   end record;

   subtype X_Focus_In_Event is X_Focus_Change_Event;
   subtype X_Focus_Out_Event is X_Focus_Change_Event;


   type X_Error_Event is record
      Display        : Display_Pointer;
      ResourceId     : XID;
      Serial         : Interfaces.C.unsigned_long;
      Error_Code     : Error_Code_Type;
      Request_Code   : Interfaces.C.unsigned_char;
      Minor_Code     : Interfaces.C.unsigned_char;
   end record;


   type X_Expose_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      X, Y           : Position;
      Width, Height  : Dimension;
      Count          : Integer;
   end record;


   type X_Graphics_Expose_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Drawable       : Drawable_ID;
      X, Y           : Position;
      Width, Height  : Dimension;
      Count          : Integer;
      Major_Code     : Integer;
      Minor_Code     : Integer;
   end record;


   type X_No_Expose_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Drawable       : Drawable_ID;
      Major_Code     : Integer;
      Minor_Code     : Integer;
   end record;


   type X_Gravity_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Event          : Drawable_ID;
      Window         : Window_ID;
      X, Y           : Position;
   end record;


   type Boolean_Array_Type is array (Natural range <>) of Boolean;
   pragma Pack (Boolean_Array_Type);

   type X_Keymap_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Key_Vector     : Boolean_Array_Type (1 .. 256);
   end record;


   type X_Key_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Root           : Window_ID;
      SubWindow      : Window_ID;
      Time           : Server_Time;
      X, Y           : Position;
      X_Root, Y_Root : Position;
      State          : Modifier_And_Button_Mask;
      Key_Code       : Interfaces.C.unsigned;
      Same_Screen    : Boolean;
   end record;

   subtype X_Key_Pressed_Event is X_Key_Event;
   subtype X_Key_Released_Event is X_Key_Event;

   type X_Map_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Event             : Window_ID;
      Window            : Window_ID;
      Override_Redirect : Boolean;
   end record;




   type X_Unmap_Event is record
      Serial          : Interfaces.C.unsigned_long;
      Send_Event      : Boolean;
      Display         : Display_Pointer;
      Event           : Window_ID;
      Window          : Window_ID;
      From_Configure  : Boolean;
   end record;


   type Mapping_Change_Type is (Mapping_Modifier,
                                Mapping_Keyboard,
                                Mapping_Pointer);
   for Mapping_Change_Type use (Mapping_Modifier => 0,
                                Mapping_Keyboard => 1,
                                Mapping_Pointer  => 2);
   for Mapping_Change_Type'Size use Interfaces.C.int'Size;


   type X_Mapping_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Request        : Mapping_Change_Type;
      First_Keycode  : Integer;
      Count          : Integer;
   end record;


   type X_Map_Request_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Parent         : Window_ID;
      Window         : Window_ID;
   end record;


   type Motion_Hint_Type is (Notify_Normal, Notify_Hint);
   for Motion_Hint_Type use (Notify_Normal => 0, Notify_Hint => 1);
   for Motion_Hint_Type'Size use Interfaces.C.signed_char'Size;

   type X_Motion_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Root           : Window_ID;
      SubWindow      : Window_ID;
      Time           : Server_Time;
      X, Y           : Position;
      X_Root, Y_Root : Position;
      State          : Modifier_And_Button_Mask;
      Is_Hint        : Motion_Hint_Type;
      Same_Screen    : Boolean;
   end record;

   subtype X_Pointer_Moved_Event is X_Motion_Event;

   type Property_State_Type is (Property_New_Value, Property_Delete);
   for Property_State_Type use (Property_New_Value => 0, Property_Delete => 1);
   for Property_State_Type'Size use Interfaces.C.int'Size;

   type X_Property_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Property       : Atom;
      Time           : Server_Time;
      State          : Property_State_Type;
   end record;


   type X_Reparent_Event is record
      Serial            : Interfaces.C.unsigned_long;
      Send_Event        : Boolean;
      Display           : Display_Pointer;
      Event             : Window_ID;
      Window            : Window_ID;
      Parent            : Window_ID;
      X, Y              : Position;
      Override_Redirect : Boolean;
   end record;


   type X_Resize_Request_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Width, Height  : Dimension;
   end record;


   type X_Selection_Clear_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      Selection      : Atom;
      Time           : Server_Time;
   end record;


   type X_Selection_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Requestor      : Window_ID;
      Selection      : Atom;
      Target         : Atom;
      Property       : Atom;
      Time           : Server_Time;
   end record;


   type X_Selection_Request_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Owner          : Window_ID;
      Requestor      : Window_ID;
      Selection      : Atom;
      Target         : Atom;
      Property       : Atom;
      Time           : Server_Time;
   end record;


   type Visibility_State_Type is (Visibility_Unobscured,
                                  Visibility_Partially_Obscured,
                                  Visibility_Fully_Obscured);
   for Visibility_State_Type use (Visibility_Unobscured         => 0,
                                  Visibility_Partially_Obscured => 1,
                                  Visibility_Fully_Obscured     => 2);
   for Visibility_State_Type'Size use Interfaces.C.int'Size;


   type X_Visibility_Event is record
      Serial         : Interfaces.C.unsigned_long;
      Send_Event     : Boolean;
      Display        : Display_Pointer;
      Window         : Window_ID;
      State          : Visibility_State_Type;
   end record;



   type X_Event (Ev_Type : Event_Type := Reserved_For_Errors) is record
      case Ev_Type is
         when Button_Press | Button_Release =>
            X_Button             : X_Button_Event;
         when Circulate_Notify =>
            X_Circulate          : X_Circulate_Event;
         when Circulate_Request =>
            X_Circulate_Request  : X_Circulate_Request_Event;
         when Client_Message =>
            X_Client             : X_Client_Message_Event;
         when Colormap_Notify =>
            X_Colormap           : X_Colormap_Event;
         when Configure_Notify =>
            X_Configure          : X_Configure_Event;
         when Configure_Request =>
            X_Configure_Request  : X_Configure_Request_Event;
         when Create_Notify =>
            X_Create_Window      : X_Create_Window_Event;
         when Enter_Notify | Leave_Notify =>
            X_Crossing           : X_Crossing_Event;
         when Destroy_Notify =>
            X_Destroy_Window     : X_Destroy_Window_Event;
         when Reserved_For_Errors | Reserved_For_Replies =>
            X_Error              : X_Error_Event;
         when Expose =>
            X_Expose             : X_Expose_Event;
         when Focus_In | Focus_Out =>
            X_Focus_Change       : X_Focus_Change_Event;
         when Graphics_Expose =>
            X_Graphics_Expose    : X_Graphics_Expose_Event;
         when No_Expose =>
            X_No_Expose          : X_No_Expose_Event;
         when Gravity_Notify =>
            X_Gravity            : X_Gravity_Event;
         when Keymap_Notify =>
            X_Keymap             : X_Keymap_Event;
         when Key_Press | Key_Release =>
            X_Key                : aliased X_Key_Event;
         when Map_Notify =>
            X_Map                : X_Map_Event;
         when Unmap_Notify =>
            X_Unmap              : X_Unmap_Event;
         when Mapping_Notify =>
            X_Mapping            : X_Mapping_Event;
         when Map_Request =>
            X_Map_Request        : X_Map_Request_Event;
         when Motion_Notify =>
            X_Motion             : X_Motion_Event;
         when Property_Notify =>
            X_Property           : X_Property_Event;
         when Reparent_Notify =>
            X_Reparent           : X_Reparent_Event;
         when Resize_Request =>
            X_Resize_Request     : X_Resize_Request_Event;
         when Selection_Clear =>
            X_Selection_Clear    : X_Selection_Clear_Event;
         when Selection_Notify =>
            X_Selection          : X_Selection_Event;
         when Selection_Request =>
            X_Selection_Request  : X_Selection_Request_Event;
         when Visibility_Notify =>
            X_Visibility         : X_Visibility_Event;
         when others =>
            Pad : Long_Array (1 .. 23);
      end case;
   end record;

   type X_Event_Pointer is access all X_Event;



   function X_Refresh_Keyboard_Mapping (map_Event : access X_Event) return interfaces.C.Int;
   pragma import (C, X_Refresh_Keyboard_Mapping, "XRefreshKeyboardMapping");

--  extern int XRefreshKeyboardMapping(
--      XMappingEvent*	/* event_map */
--  );




   function X_Get_Pointer_Mapping (Display    : in Display_Pointer;
                                   Map_return : in interfaces.C.strings.chars_ptr;
                                   nMap       : in interfaces.C.Int) return interfaces.C.Int;
   pragma import (C, X_Get_Pointer_Mapping, "XGetPointerMapping");

--  extern int XGetPointerMapping(
--      Display*		/* display */,
--      unsigned char*	/* map_return */,
--      int			/* nmap */
--  );



   function X_Event_Mask_Of_Screen (Screen : in Screen_Pointer)
      return Event_Mask;

   -- -------------------------------------------------------------------------
   --
   -- flush the request buffer, i.e. display all queued events
   --
   procedure X_Flush (Display : in Display_Pointer);

   -- -------------------------------------------------------------------------
   --
   -- flush the output buffer and wait, until all events have been received
   -- AND PROCESSED by the X-Server
   --
   procedure X_Sync
     (Display        : in Display_Pointer;
      Discard_Events : in Boolean := False);



   type Allow_Events_Type is (Async_Pointer, Sync_Pointer, Replay_Pointer,
                              Async_Keyboard, Sync_Keyboard, Replay_Keyboard,
                              Async_Both, Sync_Both);
   for Allow_Events_Type use (Async_Pointer => 0, Sync_Pointer => 1, Replay_Pointer => 2,
                              Async_Keyboard => 3, Sync_Keyboard => 4, Replay_Keyboard => 5,
                              Async_Both => 6, Sync_Both => 7);
   for Allow_Events_Type'Size use Interfaces.C.int'Size;


   procedure X_Allow_Events
     (Display  : in Display_Pointer;
      Ev_Mode  : in Allow_Events_Type;
      Time     : in Server_Time);


   type Events_Count_Type is (Queued_Already, Queued_After_Reading,
                              Queued_Afted_Flush);
   for Events_Count_Type use (Queued_Already => 0, Queued_After_Reading => 1,
                              Queued_Afted_Flush => 2);
   for Events_Count_Type'Size use Interfaces.C.int'Size;


   function X_Events_Queued
     (Display  : in Display_Pointer;
      Mode     : in Events_Count_Type)
      return Natural;


   procedure X_Select_Input
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Ev_Mask   : in Event_Mask);


   procedure X_Send_Event
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Propagate : in Boolean;
      Ev_Mask   : in Event_Mask;
      Event     : in X_Event);


   function X_Pending (Display : in Display_Pointer) return Natural;


   procedure X_Next_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event);

   procedure X_Peek_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event);


   procedure X_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event);


   procedure X_Mask_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event);


   procedure X_Check_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure X_Check_Mask_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure X_Check_Typed_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure X_Check_Typed_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure X_Put_Back_Event
     (Display  : in Display_Pointer;
      Event    : in X_Event);


   type Revert_To_Type is (None, Pointer_Root, Parent);
   for Revert_To_Type use (None => 0, Pointer_Root => 1, Parent => 2);
   for Revert_To_Type'Size use Interfaces.C.int'Size;

   Pointer_Root_Window_ID : constant Window_ID;

   procedure X_Set_Input_Focus
     (Display   : in Display_Pointer;
      Focus     : in Window_ID;
      Revert_To : in Revert_To_Type;
      Time      : in Server_Time);


   procedure X_Get_Input_Focus
     (Display   : in     Display_Pointer;
      Focus     :    out Window_ID;
      Revert_To :    out Revert_To_Type);



   type if_event_Predicate is access function (Display : in Display_Pointer;
                                               Event   : in X_Event_Pointer;
                                               Arg     : in X_Pointer) return Boolean;


   procedure X_If_Event (Display      : in   Display_Pointer;
                         Event_Return : in   X_Event_Pointer;
                         Predicate    : in   if_event_Predicate;
                         Arg          : in   X_Pointer);




   -- new interface for better convenience
   --

   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event);


   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event);


   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Event    :    out X_Event;
      Found    :    out Boolean);


   procedure X_Query_Pointer
     (Display        : in Display_Pointer;
      W              : in Window_Id;
      Root, Child    : out Window_Id;
      Root_X, Root_Y : out Position;
      Win_X, Win_Y   : out Position;
      Keys_Buttons   : out Modifier_And_Button_Mask;
      Valid          : out Boolean);


-- ----------------------------------------------------------------------------
--
--  error handling
--

   type X_Error_Handler_Proc is
      access function (Display     : in Display_Pointer;
		       Error_Event : in X_Event_Pointer)
	 return Integer;
   pragma Convention (C, X_Error_Handler_Proc);

   Null_X_Error_Handler_Proc : constant X_Error_Handler_Proc := null;

   function X_Set_Error_Handler (Proc : in X_Error_Handler_Proc)
      return X_Error_Handler_Proc;

   procedure X_Set_Error_Handler (Proc : in X_Error_Handler_Proc);


   type X_IO_Error_Handler_Proc is
      access procedure (Display : in Display_Pointer);
   pragma Convention (C, X_IO_Error_Handler_Proc);

   Null_X_IO_Error_Handler_Proc : constant X_IO_Error_Handler_Proc := null;

   function X_Set_IO_Error_Handler (Proc : in X_IO_Error_Handler_Proc)
      return X_IO_Error_Handler_Proc;

   procedure X_Set_IO_Error_Handler (Proc : in X_IO_Error_Handler_Proc);


-- ----------------------------------------------------------------------------
--
--  conversion functions
--

   function To_Address (Source: in Display_Pointer) return System.Address;
   function To_Address (Source: in Screen_Pointer)  return System.Address;
   function To_Address (Source: in GC_Pointer)      return System.Address;
   function To_Address (Source: in X_Pointer)       return System.Address;

   function To_Display_Pointer (Source : in System.Address) return Display_Pointer;
   function To_Screen_Pointer  (Source : in System.Address) return Screen_Pointer;
   function To_GC_Pointer      (Source : in System.Address) return GC_Pointer;
   function To_X_Pointer       (Source : in System.Address) return X_Pointer;

   -- needeed in X_Toolkit
   type C_X_GC_Valuemask is new Interfaces.C.unsigned_long;
   function To_Long (Mask : in X_GC_Valuemask)     return C_X_GC_Valuemask;
   function To_Mask (C_Mask : in C_X_GC_Valuemask) return X_GC_Valuemask;



   -- procedure needed in sub-packages (formerly private ~ rak)
   --
   procedure XFree (What : in System.Address);
   pragma Import (C, XFree, "XFree");



private

   Null_XID          : constant XID         := XID (0);
   Null_Drawable_ID  : constant Drawable_ID := Drawable_ID (Null_XID);
   Null_Window_ID    : constant Window_ID   := Window_ID(Null_Drawable_ID);
   Null_Pixmap_ID    : constant Pixmap_ID   := Pixmap_ID(Null_Drawable_ID);
   Null_Font_ID      : constant Font_ID     := Font_ID (Null_XID);
   Null_Cursor_ID    : constant Cursor_ID   := Cursor_ID (Null_XID);
   Null_Colormap_ID  : constant Colormap_ID := Colormap_ID (Null_XID);
   Null_GContext_ID  : constant GContext_ID := GContext_ID (Null_XID);
   Null_Key_Sym_ID   : constant Key_Sym_ID  := Key_Sym_ID (Null_XID);
   Null_Visual_ID    : constant Visual_ID   := Visual_ID (Null_XID);


   Null_Atom         : constant Atom        := Atom (0);

   type Region is new System.Address;
   Null_Region : constant Region := Region (Null_Address);


   type Display_Pointer is new System.Address;
   Null_Display_Pointer : constant Display_Pointer := Display_Pointer (Null_Address);

   type Screen_Pointer  is new System.Address;
   Null_Screen_Pointer : constant Screen_Pointer := Screen_Pointer (Null_Address);

   type GC_Pointer  is new System.Address;
   Null_GC_Pointer : constant GC_Pointer := GC_Pointer (Null_Address);

   type X_Pointer is new System.Address;
   Null_X_Pointer : constant X_Pointer := X_Pointer (Null_Address);

   -- used for X_Set/Get_Input_Focus
   Pointer_Root_Window_ID : constant Window_ID := Window_ID(1);

   -- opaque types used for internationalization
   --
   -- input method
   type Input_Method is new System.Address;
   Null_Input_Method : constant Input_Method := Input_Method (Null_Address);

   -- output method
   type Output_Method is new System.Address;
   Null_Output_Method : constant Output_Method := Output_Method (Null_Address);

   -- input context
   type Input_Context is new System.Address;
   Null_Input_Context : constant Input_Context := Input_Context (Null_Address);

   -- output context
   type Output_Context is new System.Address;
   Null_Output_Context : constant Output_Context := Output_Context (Null_Address);

   -- font set
   type Font_Set_Type is new System.Address;
   Null_Font_Set : constant Font_Set_Type := Font_Set_Type (Null_Address);

   for X_Button_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Root           at 16 range 0 .. 31;
      SubWindow      at 20 range 0 .. 31;
      Time           at 24 range 0 .. 31;
      X              at 28 range 0 .. 31;
      Y              at 32 range 0 .. 31;
      X_Root         at 36 range 0 .. 31;
      Y_Root         at 40 range 0 .. 31;
-- UseLittleEndian
      State          at 44 range 0 .. 12;
-- NotLittleEndian
--!       State          at 44 range 19 .. 31;
-- EndLittleEndian
      Button         at 48 range 0 .. 31;
      Same_Screen    at 52 range 0 .. 31;
   end record;

   for X_Circulate_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Event          at 12 range 0 .. 31;
      Window         at 16 range 0 .. 31;
      Place          at 20 range 0 .. 31;
   end record;

   for X_Circulate_Request_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Parent         at 12 range 0 .. 31;
      Window         at 16 range 0 .. 31;
      Place          at 20 range 0 .. 31;
   end record;

   for X_Client_Message_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Message_Type   at 16 range 0 .. 31;
      Format         at 20 range 0 .. 31;
      Data           at 24 range 0 .. 159;
   end record;

   for X_Colormap_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Colormap       at 16 range 0 .. 31;
      Is_New         at 20 range 0 .. 31;
      State          at 24 range 0 .. 31;
   end record;

   for X_Configure_Event use record
      Serial             at  0 range 0 .. 31;
      Send_Event         at  4 range 0 .. 31;
      Display            at  8 range 0 .. 31;
      Event              at 12 range 0 .. 31;
      Window             at 16 range 0 .. 31;
      X                  at 20 range 0 .. 31;
      Y                  at 24 range 0 .. 31;
      Width              at 28 range 0 .. 31;
      Height             at 32 range 0 .. 31;
      Border_Width       at 36 range 0 .. 31;
      Window_Above       at 40 range 0 .. 31;
      Override_Redirect  at 44 range 0 .. 31;
   end record;

   for X_Configure_Request_Event use record
      Serial            at  0 range 0 .. 31;
      Send_Event        at  4 range 0 .. 31;
      Display           at  8 range 0 .. 31;
      Parent            at 12 range 0 .. 31;
      Window            at 16 range 0 .. 31;
      X                 at 20 range 0 .. 31;
      Y                 at 24 range 0 .. 31;
      Width             at 28 range 0 .. 31;
      Height            at 32 range 0 .. 31;
      Border_Width      at 36 range 0 .. 31;
      Window_Above      at 40 range 0 .. 31;
      Detail            at 44 range 0 .. 31;
-- UseLittleEndian
      Value_Mask        at 48 range 0 .. 6;
-- NotLittleEndian
--!       Value_Mask        at 48 range 25 .. 31;
-- EndLittleEndian
   end record;
   for X_Configure_Request_Event'Size use 416;

   for X_Create_Window_Event use record
      Serial            at  0 range 0 .. 31;
      Send_Event        at  4 range 0 .. 31;
      Display           at  8 range 0 .. 31;
      Parent            at 12 range 0 .. 31;
      Window            at 16 range 0 .. 31;
      X                 at 20 range 0 .. 31;
      Y                 at 24 range 0 .. 31;
      Width             at 28 range 0 .. 31;
      Height            at 32 range 0 .. 31;
      Border_Width      at 36 range 0 .. 31;
      Window_Above      at 40 range 0 .. 31;
      Override_Redirect at 44 range 0 .. 31;
   end record;

   for X_Destroy_Window_Event use record
      Serial            at  0 range 0 .. 31;
      Send_Event        at  4 range 0 .. 31;
      Display           at  8 range 0 .. 31;
      Event             at 12 range 0 .. 31;
      Window            at 16 range 0 .. 31;
   end record;

   for X_Crossing_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Root           at 16 range 0 .. 31;
      SubWindow      at 20 range 0 .. 31;
      Time           at 24 range 0 .. 31;
      X              at 28 range 0 .. 31;
      Y              at 32 range 0 .. 31;
      X_Root         at 36 range 0 .. 31;
      Y_Root         at 40 range 0 .. 31;
      Mode           at 44 range 0 .. 31;
      Detail         at 48 range 0 .. 31;
      Same_Screen    at 52 range 0 .. 31;
      Focus          at 56 range 0 .. 31;
-- UseLittleEndian
      State          at 60 range 0 .. 12;
-- NotLittleEndian
--!       State          at 60 range 19 .. 31;
-- EndLittleEndian
   end record;

   for X_Focus_Change_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Mode           at 16 range 0 .. 31;
      Detail         at 20 range 0 .. 31;
   end record;

   for X_Error_Event use record
      Display        at  0 range 0 .. 31;
      ResourceId     at  4 range 0 .. 31;
      Serial         at  8 range 0 .. 31;
      Error_Code     at 12 range 0 .. 7;
      Request_Code   at 13 range 0 .. 7;
      Minor_Code     at 14 range 0 .. 7;
   end record;

   for X_Expose_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      X              at 16 range 0 .. 31;
      Y              at 20 range 0 .. 31;
      Width          at 24 range 0 .. 31;
      Height         at 28 range 0 .. 31;
      Count          at 32 range 0 .. 31;
   end record;

   for X_Graphics_Expose_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Drawable       at 12 range 0 .. 31;
      X              at 16 range 0 .. 31;
      Y              at 20 range 0 .. 31;
      Width          at 24 range 0 .. 31;
      Height         at 28 range 0 .. 31;
      Count          at 32 range 0 .. 31;
      Major_Code     at 36 range 0 .. 31;
      Minor_Code     at 40 range 0 .. 31;
   end record;

   for X_No_Expose_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Drawable       at 12 range 0 .. 31;
      Major_Code     at 16 range 0 .. 31;
      Minor_Code     at 20 range 0 .. 31;
   end record;

   for X_Gravity_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Event          at 12 range 0 .. 31;
      Window         at 16 range 0 .. 31;
      X              at 20 range 0 .. 31;
      Y              at 24 range 0 .. 31;
   end record;

   for X_Keymap_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Key_Vector     at 16 range 0 .. 255;
   end record;
   for X_Keymap_Event'Size use 12*Interfaces.C.int'Size;

   for X_Key_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Root           at 16 range 0 .. 31;
      SubWindow      at 20 range 0 .. 31;
      Time           at 24 range 0 .. 31;
      X              at 28 range 0 .. 31;
      Y              at 32 range 0 .. 31;
      X_Root         at 36 range 0 .. 31;
      Y_Root         at 40 range 0 .. 31;
-- UseLittleEndian
      State          at 44 range 0 .. 12;
-- NotLittleEndian
--!       State          at 44 range 19 .. 31;
-- EndLittleEndian
      Key_Code       at 48 range 0 .. 31;
      Same_Screen    at 52 range 0 .. 31;
   end record;

   for  X_Map_Event use record
      Serial             at  0 range 0 .. 31;
      Send_Event         at  4 range 0 .. 31;
      Display            at  8 range 0 .. 31;
      Event              at 12 range 0 .. 31;
      Window             at 16 range 0 .. 31;
      Override_Redirect  at 20 range 0 .. 31;
   end record;

   for  X_Unmap_Event use record
      Serial          at  0 range 0 .. 31;
      Send_Event      at  4 range 0 .. 31;
      Display         at  8 range 0 .. 31;
      Event           at 12 range 0 .. 31;
      Window          at 16 range 0 .. 31;
      From_Configure  at 20 range 0 .. 31;
   end record;

   for X_Mapping_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Request        at 16 range 0 .. 31;
      First_Keycode  at 20 range 0 .. 31;
      Count          at 24 range 0 .. 31;
   end record;

   for X_Map_Request_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Parent         at 12 range 0 .. 31;
      Window         at 16 range 0 .. 31;
   end record;

   for X_Motion_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Root           at 16 range 0 .. 31;
      SubWindow      at 20 range 0 .. 31;
      Time           at 24 range 0 .. 31;
      X              at 28 range 0 .. 31;
      Y              at 32 range 0 .. 31;
      X_Root         at 36 range 0 .. 31;
      Y_Root         at 40 range 0 .. 31;
-- UseLittleEndian
      State          at 44 range 0 .. 12;
-- NotLittleEndian
--!       State          at 44 range 19 .. 31;
-- EndLittleEndian
      Is_Hint        at 48 range 0 .. 31;
      Same_Screen    at 52 range 0 .. 31;
   end record;

   for X_Property_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Property       at 16 range 0 .. 31;
      Time           at 20 range 0 .. 31;
      State          at 24 range 0 .. 31;
   end record;

   for X_Reparent_Event use record
      Serial            at  0 range 0 .. 31;
      Send_Event        at  4 range 0 .. 31;
      Display           at  8 range 0 .. 31;
      Event             at 12 range 0 .. 31;
      Window            at 16 range 0 .. 31;
      Parent            at 20 range 0 .. 31;
      X                 at 24 range 0 .. 31;
      Y                 at 28 range 0 .. 31;
      Override_Redirect at 32 range 0 .. 31;
   end record;

   for X_Resize_Request_Event use record
      Serial            at  0 range 0 .. 31;
      Send_Event        at  4 range 0 .. 31;
      Display           at  8 range 0 .. 31;
      Window            at 12 range 0 .. 31;
      Width             at 16 range 0 .. 31;
      Height            at 20 range 0 .. 31;
   end record;

   for X_Selection_Clear_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      Selection      at 16 range 0 .. 31;
      Time           at 20 range 0 .. 31;
   end record;

   for X_Selection_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Requestor      at 12 range 0 .. 31;
      Selection      at 16 range 0 .. 31;
      Target         at 20 range 0 .. 31;
      Property       at 24 range 0 .. 31;
      Time           at 28 range 0 .. 31;
   end record;

   for X_Selection_Request_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Owner          at 12 range 0 .. 31;
      Requestor      at 16 range 0 .. 31;
      Selection      at 20 range 0 .. 31;
      Target         at 24 range 0 .. 31;
      Property       at 28 range 0 .. 31;
      Time           at 32 range 0 .. 31;
   end record;

   for X_Visibility_Event use record
      Serial         at  0 range 0 .. 31;
      Send_Event     at  4 range 0 .. 31;
      Display        at  8 range 0 .. 31;
      Window         at 12 range 0 .. 31;
      State          at 16 range 0 .. 31;
   end record;

   for X_Event'Size use 768;
   pragma Convention (C, X_Event);

   pragma Import (C, X_Bell, "XBell");

   pragma Import (C, X_Protocol_Version, "XProtocolVersion");
   pragma Import (C, X_Protocol_Revision, "XProtocolRevision");
   pragma Import (C, X_Vendor_Release, "XVendorRelease");
   pragma Import (C, X_Max_Request_Size, "XMaxRequestSize");
   pragma Import (C, X_Extended_Max_Request_Size, "XExtendedMaxRequestSize");
   pragma Import (C, X_Display_Motion_Buffer_Size, "XDisplayMotionBufferSize");

   pragma Import (C, X_Keycode_To_Keysym, "XKeycodeToKeysym");
   pragma Import (C, X_Keysym_To_Keycode, "XKeysymToKeycode");
   pragma Import (C, X_Convert_Case, "XConvertCase");

   pragma Import (C, X_Default_GC, "XDefaultGC");
   pragma Import (C, X_Default_GC_Of_Screen, "XDefaultGCOfScreen");
   pragma Import (C, X_Free_GC, "XFreeGC");
   pragma Import (C, X_Flush_GC, "XFlushGC");
   pragma Import (C, X_GContext_From_GC, "XGContextFromGC");
   pragma Import (C, X_Set_Function, "XSetFunction");
   pragma Import (C, X_Set_Plane_Mask, "XSetPlaneMask");
   pragma Import (C, X_Set_Foreground, "XSetForeground");
   pragma Import (C, X_Set_Background, "XSetBackground");
   pragma Import (C, X_Set_Line_Attributes, "XSetLineAttributes");
   pragma Import (C, X_Set_Fill_Style, "XSetFillStyle");
   pragma Import (C, X_Set_Fill_Rule, "XSetFillRule");
   pragma Import (C, X_Set_Arc_Mode, "XSetArcMode");
   pragma Import (C, X_Set_Tile, "XSetTile");
   pragma Import (C, X_Set_Stipple, "XSetStipple");
   pragma Import (C, X_Set_TS_Origin, "XSetTSOrigin");
   pragma Import (C, X_Set_Font, "XSetFont");
   pragma Import (C, X_Set_Subwindow_Mode, "XSetSubwindowMode");
   pragma Import (C, X_Set_Clip_Origin, "XSetClipOrigin");
   pragma Import (C, X_Set_Clip_Mask, "XSetClipMask");
   pragma Import (C, X_Set_State, "XSetState");

   pragma Import (C, X_Create_Simple_Window, "XCreateSimpleWindow");

   pragma Import (C, X_Create_Image, "XCreateImage");
   pragma Import (C, X_Get_Image, "XGetImage");
   pragma Import (C, X_Get_Sub_Image, "XGetSubImage");
   pragma Import (C, X_Put_Image, "XPutImage");

   pragma Import (C, X_Clear_Window, "XClearWindow");

   pragma Import (C, X_Query_Color, "XQueryColor");

   pragma Import (C, X_Default_Depth, "XDefaultDepth");
   pragma Import (C, X_Default_Depth_Of_Screen, "XDefaultDepthOfScreen");

   pragma Import (C, X_Screen_Count, "XScreenCount");
   pragma Import (C, X_Default_Screen, "XDefaultScreen");
   pragma Import (C, X_Default_Screen_Of_Display, "XDefaultScreenOfDisplay");
   pragma Import (C, X_Screen_Of_Display, "XScreenOfDisplay");
   pragma Import (C, X_Screen_Number_Of_Screen, "XScreenNumberOfScreen");
   pragma Import (C, X_Does_Backing_Store, "XDoesBackingStore");
   pragma Import (C, X_Event_Mask_Of_Screen, "XEventMaskOfScreen");

   pragma Import (C, X_Default_Colormap, "XDefaultColormap");
   pragma Import (C, X_Default_Colormap_Of_Screen, "XDefaultColormapOfScreen");
   pragma Import (C, X_Create_Colormap, "XCreateColormap");
   pragma Import (C, X_Copy_Colormap_And_Free, "XCopyColormapAndFree");
   pragma Import (C, X_Free_Colormap, "XFreeColormap");
   pragma Import (C, X_Install_Colormap, "XInstallColormap");
   pragma Import (C, X_Uninstall_Colormap, "XUninstallColormap");

   pragma Import (C, X_Display_Cells, "XDisplayCells");
   pragma Import (C, X_Display_Planes, "XDisplayPlanes");
   pragma Import (C, X_Cells_Of_Screen, "XCellsOfScreen");
   pragma Import (C, X_Planes_Of_Screen, "XPlanesOfScreen");

   All_Temporary : constant XID := Null_XID;

   pragma Import (C, X_Kill_Client, "XKillClient");

   pragma Import (C, X_Add_To_Save_Set, "XAddToSaveSet");
   pragma Import (C, X_Remove_From_Save_Set, "XRemoveFromSaveSet");
   pragma Import (C, X_Root_Window, "XRootWindow");
   pragma Import (C, X_Default_Root_Window, "XDefaultRootWindow");
   pragma Import (C, X_Root_Window_Of_Screen, "XRootWindowOfScreen");
   pragma Import (C, X_Set_Window_Background, "XSetWindowBackground");
   pragma Import (C, X_Set_Window_Background_Pixmap, "XSetWindowBackgroundPixmap");
   pragma Import (C, X_Set_Window_Border, "XSetWindowBorder");
   pragma Import (C, X_Set_Window_Border_Pixmap, "XSetWindowBorderPixmap");
   pragma Import (C, X_Set_Window_Border_Width, "XSetWindowBorderWidth");
   pragma Import (C, X_Set_Window_Colormap, "XSetWindowColormap");
   pragma Import (C, X_Raise_Window, "XRaiseWindow");
   pragma Import (C, X_Lower_Window, "XLowerWindow");
   pragma Import (C, X_Destroy_Window, "XDestroyWindow");
   pragma Import (C, X_Destroy_Subwindows, "XDestroySubwindows");
   pragma Import (C, X_Circulate_Subwindows, "XCirculateSubwindows");
   pragma Import (C, X_Circulate_Subwindows_Up, "XCirculateSubwindowsUp");
   pragma Import (C, X_Circulate_Subwindows_Down, "XCirculateSubwindowsDown");
   pragma Import (C, X_Map_Window, "XMapWindow");
   pragma Import (C, X_Unmap_Window, "XUnmapWindow");
   pragma Import (C, X_Map_Raised, "XMapRaised");
   pragma Import (C, X_Map_Subwindows, "XMapSubwindows");
   pragma Import (C, X_Unmap_Subwindows, "XUnmapSubwindows");

   pragma Import (C, X_Close_Display, "XCloseDisplay");
   pragma Import (C, X_Display_Of_Screen, "XDisplayOfScreen");

   pragma Import (C, X_Default_Visual, "XDefaultVisual");
   pragma Import (C, X_Default_Visual_Of_Screen, "XDefaultVisualOfScreen");
   pragma Import (C, X_Visual_ID_From_Visual, "XVisualIDFromVisual");
   pragma Import (C, X_Black_Pixel, "XBlackPixel");
   pragma Import (C, X_White_Pixel, "XWhitePixel");
   pragma Import (C, X_Black_Pixel_Of_Screen, "XBlackPixelOfScreen");
   pragma Import (C, X_White_Pixel_Of_Screen, "XWhitePixelOfScreen");
   pragma Import (C, X_All_Planes, "XAllPlanes");

   --  for font routines
   --
   for  Draw_Direction use (Left_To_Right => 0, Right_To_Left => 1,
                            Direction_Change => 255);
   for  Draw_Direction'Size use Interfaces.C.int'Size;

   pragma Import (C, X_Create_Pixmap, "XCreatePixmap");
   pragma Import (C, X_Free_Pixmap, "XFreePixmap");

-- UseX11R6 X11R6.3
   pragma Import (C, X_Lock_Display, "XLockDisplay");
   pragma Import (C, X_Unlock_Display, "XUnlockDisplay");
-- EndX11R6 X11R6.3

   pragma Import (C, X_Display_Height, "XDisplayHeight");
   pragma Import (C, X_Display_Height_MM, "XDisplayHeightMM");
   pragma Import (C, X_Height_Of_Screen, "XHeightOfScreen");
   pragma Import (C, X_Height_MM_Of_Screen, "XHeightMMOfScreen");
   pragma Import (C, X_Display_Width, "XDisplayWidth");
   pragma Import (C, X_Display_Width_MM, "XDisplayWidthMM");
   pragma Import (C, X_Width_Of_Screen, "XWidthOfScreen");
   pragma Import (C, X_Width_MM_Of_Screen, "XWidthMMOfScreen");
   pragma Import (C, X_Image_Byte_Order, "XImageByteOrder");
   pragma Import (C, X_Bitmap_Bit_Order, "XBitmapBitOrder");
   pragma Import (C, X_Bitmap_Pad, "XBitmapPad");
   pragma Import (C, X_Bitmap_Unit, "XBitmapUnit");

   pragma Import (C, X_Ungrab_Keyboard, "XUngrabKeyboard");
   pragma Import (C, X_Grab_Server, "XGrabServer");
   pragma Import (C, X_Ungrab_Server, "XUngrabServer");

   pragma Import (C, X_Flush, "XFlush");
   pragma Import (C, X_Pending, "XPending");
   pragma Import (C, X_Allow_Events, "XAllowEvents");
   pragma Import (C, X_Events_Queued, "XEventsQueued");
   pragma Import (C, X_Set_Input_Focus, "XSetInputFocus");
   pragma Import (C, X_Get_Input_Focus, "XGetInputFocus");

   pragma Import (C, X_If_Event, "XIfEvent");


   pragma Import (C, X_Set_Error_Handler, "XSetErrorHandler");
   pragma Import (C, X_Set_IO_Error_Handler, "XSetIOErrorHandler");




   -- things needed in sub-packages
   --
   type C_Window_Changes_Mask is new Interfaces.C.unsigned;

   function To_Int (Mask : in Window_Changes_Mask)
      return C_Window_Changes_Mask;




   pragma import (C, X_Connection_Number, "XConnectionNumber");

end X_Lib;
