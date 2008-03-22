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
--          July 07, 1998 HFVogt: changed Boolean_Type to X_Boolean to prevent
--                                confusion
--          November 8, 1998 HFVogt: added X11R6.3 conditional defines
--          June 14, 2000: included additions from Joerg Schaefer:
--                         type Char_Struct, X_Query_Text_Extents,
--                         X_Resize_Window, X_Query_Pointer
--          7 Oct 2001 H.-F. Vogt: make To_X_Boolean and To_Boolean being
--                                 inlined + minor improvement to To_Boolean
--          9 Feb 2002 H.-F. Vogt: raise No_Connection_Error exception if
--                                 connection to the X-Server can't be gained
--          2 Mar 2002 H.-F. Vogt: correct silly error in X_Query_Text_Extent
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion,
     Interfaces.C.Strings,
     Interfaces.C.Pointers,
     X_Strings;
use Interfaces.C.Strings;

package body X_Lib is

   use type Interfaces.C.unsigned;

   function To_X_Boolean (Val : in Boolean) return X_Boolean is
   begin
      if Val then
         return X_Boolean'(True);
      else
         return X_Boolean'(False);
      end if;
   end To_X_Boolean;
   pragma Inline (To_X_Boolean);


   function To_Boolean (Val : in X_Boolean) return Boolean is
   begin
      return Val = X_Boolean'(True);
   end To_Boolean;
   pragma Inline (To_Boolean);


   function To_Bits_32_Type (Source : in XID) return Bits_32_Type is
      function To_Target is new Ada.Unchecked_Conversion (XID, Bits_32_Type);
   begin
      return To_Target (Source);
   end To_Bits_32_Type;
   pragma Inline (To_Bits_32_Type);


   function To_XID (Source : in Bits_32_Type) return XID is
      function To_Target is new Ada.Unchecked_Conversion (Bits_32_Type, XID);
   begin
      return To_Target (Source);
   end To_XID;
   pragma Inline (To_XID);


   function To_Bits_32_Type (Source : in X_Pointer) return Bits_32_Type is
      function To_Target is new Ada.Unchecked_Conversion (X_Pointer, Bits_32_Type);
   begin
      return To_Target (Source);
   end To_Bits_32_Type;
   pragma Inline (To_Bits_32_Type);


   function To_X_Pointer (Source : in Bits_32_Type) return X_Pointer is
      function To_Target is new Ada.Unchecked_Conversion (Bits_32_Type, X_Pointer);
   begin
      return To_Target (Source);
   end To_X_Pointer;
   pragma Inline (To_X_Pointer);



   --
   -- XID List
   --
   function Length (List : in  XID_List) return Natural is
   begin
      return XID_Lists.Length (XID_Lists.Unbounded_List (List));
   end Length;


   function Element
     (List   : in XID_List;
      Index  : in Natural)
      return XID is
   begin
      return XID_Lists.Element (XID_Lists.Unbounded_List (List), Index);
   end Element;
   pragma Inline (Element);


   function "=" (Left, Right : in XID_List) return Boolean is
   begin
      return XID_Lists."=" (XID_Lists.Unbounded_List (Left),
                            XID_Lists.Unbounded_List (Right));
   end "=";
   pragma Inline ("=");


   function "&" (Left, Right : in XID_List) return XID_List is
   begin
      return XID_List (XID_Lists."&" (XID_Lists.Unbounded_List (Left),
                                      XID_Lists.Unbounded_List (Right)));
   end "&";
   pragma Inline ("&");


   function "&" (Left  : in XID_List;
                 Right : in XID) return XID_List is
   begin
      return XID_List (XID_Lists."&" (XID_Lists.Unbounded_List (Left),
                                      Right));
   end "&";
   pragma Inline ("&");


   procedure Append (List : in out XID_List;
                     W    : in     XID_List) is
   begin
      XID_Lists.Append (XID_Lists.Unbounded_List (List),
                        XID_Lists.Unbounded_List (W));
   end Append;
   pragma Inline (Append);


   procedure Append (List : in out XID_List;
                     W    : in     XID) is
   begin
      XID_Lists.Append (XID_Lists.Unbounded_List (List),
                        W);
   end Append;
   pragma Inline (Append);



   procedure X_Query_Tree
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Root     :    out Window_ID;
      Parent   :    out Window_ID;
      Children :    out Window_ID_List) is
      function XQueryTree
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Root       : in System.Address;
         Parent     : in System.Address;
         Children   : in System.Address;
         N_Children : in System.Address)
	 return Status_Type;
      pragma Import (C, XQueryTree, "XQueryTree");

      Num_Children : Integer;
      Children_Adr : System.Address;
   begin
      if XQueryTree (Display, W, Root'Address, Parent'Address,
                     Children_Adr'Address, Num_Children'Address) = Error_Status then
         raise X_Error;
      end if;
      if Num_Children < 1 then
         Children := Window_ID_List (Null_XID_List);
      else
         declare
            subtype Our_Array is XID_Lists.Element_Array (1 .. Num_Children);
	    type Our_Array_Access is access all Our_Array;
	    function To_Hook is
	       new Ada.Unchecked_Conversion (System.Address, Our_Array_Access);
            Hook : Our_Array_Access;
	 begin
	    Hook     := To_Hook (Children_Adr);
            Children := Window_ID_List (XID_Lists.To_Unbounded_List (Hook.all));
            XFree (Children_Adr);
	 end;
      end if;
   end X_Query_Tree;



   -- -------------------------------------------------------------------------
   --
   --  keyboard
   --
   function X_String_To_Keysym (Keysym_Name : in String) return Key_Sym_ID is
      function XStringToKeysym (Keysym_Name : in System.Address) return Key_Sym_ID;
      pragma Import (C, XStringToKeysym, "XStringToKeysym");
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Keysym_Name, Append_Nul => True);
   begin
      return XStringToKeysym (Name_String'Address);
   end X_String_To_Keysym;


   function X_Keysym_To_String (Keysym : in Key_Sym_ID) return String is
      function XKeysymToString (Keysym : in Key_Sym_ID)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XKeysymToString, "XKeysymToString");
      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := XKeysymToString (Keysym);
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         -- the string ptr points to musn't be released! => no "free"
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end X_Keysym_To_String;


   -- convertion routines from bitfields to integers and vice versa
   --
   -- already defined in X_Lib specification
   -- type C_X_GC_Valuemask is new Interfaces.C.unsigned_long;
   type X_GC_Valuemask_Int is mod 2**23; -- X_GC_Valuemask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (X_GC_Valuemask,
                                                         X_GC_Valuemask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (X_GC_Valuemask_Int,
                                                          X_GC_Valuemask);

   function To_Long (Mask : in X_GC_Valuemask) return C_X_GC_Valuemask is
   begin
      return C_X_GC_Valuemask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_X_GC_Valuemask) return X_GC_Valuemask is
   begin
      return To_Mask_Type (X_GC_Valuemask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   type C_X_Visual_Info_Mask is new Interfaces.C.unsigned_long;
   type X_Visual_Info_Mask_Int is mod 2**9; --X_Visual_Info_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (X_Visual_Info_Mask,
                                                         X_Visual_Info_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (X_Visual_Info_Mask_Int,
                                                          X_Visual_Info_Mask);

   function To_Long (Mask : in X_Visual_Info_Mask) return C_X_Visual_Info_Mask is
   begin
      return C_X_Visual_Info_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_X_Visual_Info_Mask) return X_Visual_Info_Mask is
   begin
      return To_Mask_Type (X_Visual_Info_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   type C_Set_Window_Attributes_Mask is new Interfaces.C.unsigned_long;
   type Set_Window_Attributes_Mask_Int is mod 2**15; -- Set_Window_Attributes_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (Set_Window_Attributes_Mask,
                                                         Set_Window_Attributes_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (Set_Window_Attributes_Mask_Int,
                                                          Set_Window_Attributes_Mask);

   function To_Long (Mask : in Set_Window_Attributes_Mask) return C_Set_Window_Attributes_Mask is
   begin
      return C_Set_Window_Attributes_Mask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Set_Window_Attributes_Mask) return Set_Window_Attributes_Mask is
   begin
      return To_Mask_Type (Set_Window_Attributes_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   -- already defined in private part of X_Lib specification
   -- type C_Window_Changes_Mask is new Interfaces.C.unsigned;
   type Window_Changes_Mask_Int is mod 2**7; -- Window_Changes_Mask'Size;
   function To_Mask_Int is new Ada.Unchecked_Conversion (Window_Changes_Mask,
                                                         Window_Changes_Mask_Int);
   function To_Mask_Type is new Ada.Unchecked_Conversion (Window_Changes_Mask_Int,
                                                          Window_Changes_Mask);

   function To_Int (Mask : in Window_Changes_Mask) return C_Window_Changes_Mask is
   begin
      return C_Window_Changes_Mask (To_Mask_Int (Mask));
   end To_Int;
   pragma Inline (To_Int);


   function To_Mask (C_Mask : in C_Window_Changes_Mask) return Window_Changes_Mask is
   begin
      return To_Mask_Type (Window_Changes_Mask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);




   function X_Server_Vendor (Display : in Display_Pointer)
      return String is
      function XServerVendor (Display : in Display_Pointer)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XServerVendor, "XServerVendor");
   begin
      return Interfaces.C.Strings.Value (XServerVendor (Display));
   end X_Server_Vendor;



   -- -------------------------------------------------------------------------
   --
   --  internal representation of simple graphic objects
   --
   type X_Arc is record
      X, Y           : Interfaces.C.short;
      Width, Height  : Interfaces.C.unsigned_short;
      Angle1, Angle2 : Interfaces.C.short;
   end record;
   type X_Arc_Access is access all X_Arc;

   type X_Arc_Array is array (Natural range <>) of aliased X_Arc;


   type X_Point is record
      X, Y : Interfaces.C.short;
   end record;
   type X_Point_Access is access all X_Point;

   type X_Point_Array is array (Natural range <>) of aliased X_Point;


   type X_Rectangle is record
      X, Y          : Interfaces.C.short;
      Width, Height : Interfaces.C.unsigned_short;
   end record;
   type X_Rectangle_Access is access all X_Rectangle;

   type X_Rectangle_Array is array (Natural range <>) of aliased X_Rectangle;

   function To_X_Rectangle (Rect : in Rectangle) return X_Rectangle is
   begin
      return X_Rectangle'(X      => Interfaces.C.short (Rect.X),
                          Y      => Interfaces.C.short (Rect.Y),
                          Width  => Interfaces.C.unsigned_short (Rect.Width),
                          Height => Interfaces.C.unsigned_short (Rect.Height));
   end To_X_Rectangle;
   pragma Inline (To_X_Rectangle);

   function To_Rectangle (X_Rect : in X_Rectangle) return Rectangle is
   begin
      return Rectangle'(X      => X_Lib.Position (X_Rect.X),
                        Y      => X_Lib.Position (X_Rect.Y),
                        Width  => X_Lib.Dimension (X_Rect.Width),
                        Height => X_Lib.Dimension (X_Rect.Height));
   end To_Rectangle;
   pragma Inline (To_Rectangle);


   type X_Segment is record
      X1, Y1, X2, Y2 : Interfaces.C.short;
   end record;
   type X_Segment_Access is access all X_Segment;

   type X_Segment_Array is array (Natural range <>) of aliased X_Segment;




   -- -------------------------------------------------------------------------
   --
   --   Graphics Context
   --

   -- we have to define this function here because of the parameter Valuemask,
   -- which is just an unsigned long in C, but a record here
   -- gcc would pass a pointer to the record to the function, which would be
   -- wrong
   function X_Create_GC
     (Display   : in Display_Pointer;
      Drawable  : in Drawable_ID;
      Valuemask : in X_GC_Valuemask;
      Values    : in X_GC_Values)
      return GC_Pointer is
      function XCreateGC
        (Display   : in Display_Pointer;
         Drawable  : in Drawable_ID;
         Valuemask : in C_X_GC_Valuemask;
         Values    : in System.Address)
         return GC_Pointer;
      pragma Import (C, XCreateGC, "XCreateGC");
   begin
      return XCreateGC (Display, Drawable,
                        To_Long (Valuemask),
                        Values'Address);
   end X_Create_GC;
   pragma Inline (X_Create_GC);


   function X_Create_GC
     (Display   : in Display_Pointer;
      Drawable  : in Drawable_ID)
      return GC_Pointer is
      function XCreateGC
        (Display   : in Display_Pointer;
         Drawable  : in Drawable_ID;
         Valuemask : in C_X_GC_Valuemask;
         Values    : in System.Address)
         return GC_Pointer;
      pragma Import (C, XCreateGC, "XCreateGC");
   begin
      return XCreateGC (Display, Drawable,
                        0,
                        Null_Address);
   end X_Create_GC;
   pragma Inline (X_Create_GC);


   procedure X_Change_GC
     (Display   : in Display_Pointer;
      GC        : in GC_Pointer;
      Valuemask : in X_GC_Valuemask;
      Values    : in X_GC_Values) is
      procedure XChangeGC
        (Display   : in Display_Pointer;
         GC        : in GC_Pointer;
         Valuemask : in C_X_GC_Valuemask;
         Values    : in System.Address);
      pragma Import (C, XChangeGC, "XChangeGC");
   begin
      XChangeGC (Display, GC,
                 To_Long (Valuemask),
                 Values'Address);
   end X_Change_GC;
   pragma Inline (X_Change_GC);



   procedure X_Copy_GC
     (Display   : in Display_Pointer;
      Source    : in GC_Pointer;
      Valuemask : in X_GC_Valuemask;
      Dest      : in GC_Pointer) is
      procedure XCopyGC
        (Display   : in Display_Pointer;
         Source    : in GC_Pointer;
         Valuemask : in C_X_GC_Valuemask;
         Dest      : in GC_Pointer);
      pragma Import (C, XCopyGC, "XCopyGC");
   begin
      XCopyGC (Display, Source,
               To_Long (Valuemask),
               Dest);
   end X_Copy_GC;
   pragma Inline (X_Copy_GC);


   procedure X_Get_GC_Values
     (Display   : in     Display_Pointer;
      GC        : in     GC_Pointer;
      Valuemask : in     X_GC_Valuemask;
      Values    :    out X_GC_Values) is
      function XGetGCValues
        (Display   : in Display_Pointer;
         GC        : in GC_Pointer;
         Valuemask : in C_X_GC_Valuemask;
         Values    : in System.Address)
	 return Status_Type;
      pragma Import (C, XGetGCValues, "XGetGCValues");
   begin
      if XGetGCValues (Display, GC,
                       To_Long (Valuemask),
                       Values'Address) = Error_Status then
         raise X_Error;
      end if;
   end X_Get_GC_Values;
   pragma Inline (X_Get_GC_Values);


   -- -------------------------------------------------------------------------
   --
   --   Convenience functions
   --
   procedure X_Set_Graphics_Exposures
     (Display            : in Display_Pointer;
      GC                 : in GC_Pointer;
      Graphics_Exposures : in Boolean) is
      procedure XSetGraphicsExposures
        (Display            : in Display_Pointer;
         GC                 : in GC_Pointer;
         Graphics_Exposures : in X_Boolean);
      pragma Import (C, XSetGraphicsExposures, "XSetGraphicsExposures");
   begin
      XSetGraphicsExposures (Display, GC, To_X_Boolean (Graphics_Exposures));
   end X_Set_Graphics_Exposures;


   type Rect_Ordering_Int is range 0 .. 2**Rectangle_Ordering'Size-1;
   function To_Rect_Ordering_Int is
      new Ada.Unchecked_Conversion (Rectangle_Ordering, Rect_Ordering_Int);

   procedure X_Set_Clip_Rectangles
     (Display        : in Display_Pointer;
      GC             : in GC_Pointer;
      Clip_X_Origin,
      Clip_Y_Origin  : in Integer;
      Rectangles     : in Rectangle_Array;
      Ordering       : in Rectangle_Ordering) is
      procedure XSetClipRectangles
        (Display        : in Display_Pointer;
         GC             : in GC_Pointer;
         Clip_X_Origin,
         Clip_Y_Origin  : in Integer;
         Rectangles     : in System.Address;
         N_Rects        : in Integer;
         Ordering       : in Integer);
      pragma Import (C, XSetClipRectangles, "XSetClipRectangles");
      Rect_Arry    : X_Rectangle_Array (Rectangles'Range);
   begin
      for I in Rectangles'Range loop
         Rect_Arry (I) := To_X_Rectangle (Rectangles (I));
      end loop;
      XSetClipRectangles (Display, GC,
                          Clip_X_Origin, Clip_Y_Origin,
                          Rect_Arry'Address, Rectangles'Length,
                          Integer (To_Rect_Ordering_Int (Ordering)));
   end X_Set_Clip_Rectangles;


   procedure X_Set_Dashes
     (Display            : in Display_Pointer;
      GC                 : in GC_Pointer;
      Dash_Offset        : in Integer;
      Dash_List          : in Dash_List_Type) is
      procedure XSetDashes
        (Display            : in Display_Pointer;
         GC                 : in GC_Pointer;
         Dash_Offset        : in Integer;
         Dash_List          : in System.Address;
         N                  : in Integer);
      pragma Import (C, XSetDashes, "XSetDashes");
   begin
      XSetDashes (Display, GC, Dash_Offset, Dash_List'Address, Dash_List'Length);
   end X_Set_Dashes;



   -- -------------------------------------------------------------------------
   --
   --  Display routines
   --
   function X_Open_Display (Display_Name : in String := "")
      return Display_Pointer is
      function XOpenDisplay
        (Display_Name : in System.Address)
	 return Display_Pointer;
      pragma Import (C, XOpenDisplay, "XOpenDisplay");
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Display_Name, Append_Nul => True);
      Ret_Display : Display_Pointer;
   begin
      if Display_Name'Length > 0 then
         Ret_Display := XOpenDisplay (Name_String'Address);
      else
         Ret_Display := XOpenDisplay (Null_Address);
      end if;
      if Ret_Display /= Null_Display_Pointer then
         return Ret_Display;
      else
         raise No_Connection_Error;
      end if;
   end X_Open_Display;


   function X_Display_Name (Display_Name : in String := "")
      return String is
      function XDisplayName (Display_Name : in System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XDisplayName, "XDisplayName");
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Display_Name, Append_Nul => True);
      Name        : Interfaces.C.Strings.chars_ptr;
   begin
      if Display_Name'Length > 0 then
         Name := XDisplayName (Name_String'Address);
      else
         Name := XDisplayName (Null_Address);
      end if;
      if Name = Interfaces.C.Strings.null_ptr then
         return String'("");
      else
         -- I'm not sure whether the returned string can be freed, therefore
	 -- I don't free it
         return Interfaces.C.Strings.Value (Name);
      end if;
   end X_Display_Name;



   function X_Display_String (Display : in Display_Pointer)
      return String is
      function XDisplayString (Display : in Display_Pointer)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XDisplayString, "XDisplayString");
      Str : Interfaces.C.Strings.chars_ptr;
   begin
      Str := XDisplayString (Display);
      if Str = Interfaces.C.Strings.null_ptr then
         return String'("");
      else
         -- I'm not sure whether the returned string can be freed, therefore
	 -- I don't free it
         return Interfaces.C.Strings.Value (Str);
      end if;
   end X_Display_String;


   function X_Resource_Manager_String (Display : in Display_Pointer)
      return String is
      function XResourceManagerString (Display : in Display_Pointer)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XResourceManagerString, "XResourceManagerString");
      Str : Interfaces.C.Strings.chars_ptr;
   begin
      Str := XResourceManagerString (Display);
      if Str = Interfaces.C.Strings.null_ptr then
         return String'("");
      else
         -- the returned string must not be freed!
         return Interfaces.C.Strings.Value (Str);
      end if;
   end X_Resource_Manager_String;






   -- -------------------------------------------------------------------------
   --
   --  Visual routines
   --
   function X_Get_Visual_Info (Display        : in Display_Pointer;
                               Vinfo_Mask     : in X_Visual_Info_Mask;
                               Vinfo_Template : in X_Visual_Info)
      return X_Visual_Info_Array is

      One_Visual_Info : X_Visual_Info; -- just one element of the type

      package Visual_Array_Interface is new
         Interfaces.C.Pointers (Positive,
                                X_Visual_Info,
                                X_Visual_Info_Array,
                                One_Visual_Info);

      function XGetVisualInfo
        (Display        : in Display_Pointer;
         Vinfo_Mask     : in C_X_Visual_Info_Mask;
         Vinfo_Template : in System.Address;
         Nitems         : in System.Address)
         return Visual_Array_Interface.Pointer;
      pragma Import (C, XGetVisualInfo, "XGetVisualInfo");

      Hook_To_Array   : Visual_Array_Interface.Pointer;
      Number          : Integer;

   begin
      Hook_To_Array := XGetVisualInfo (Display, To_Long (Vinfo_Mask),
                                       Vinfo_Template'Address,
                                       Number'Address);
      return Visual_Array_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end X_Get_Visual_Info;


   function X_Match_Visual_Info (Display      : in  Display_Pointer;
                                 Screen       : in  Screen_Number;
                                 Depth        : in  Color_Depth;
                                 Class        : in  Visual_Class)
      return X_Visual_Info is
      function XMatchVisualInfo (Display        : in Display_Pointer;
                                 Screen         : in Screen_Number;
                                 Depth          : in Color_Depth;
                                 Class          : in Visual_Class;
                                 Vinfo          : in System.Address)
         return Status_Type;
      pragma Import (C, XMatchVisualInfo, "XMatchVisualInfo");

      Ret_Info : X_Visual_Info;
   begin
      if XMatchVisualInfo (Display, Screen, Depth, Class,
                           Ret_Info'Address) = Error_Status then
         raise X_Error;
      else
         return Ret_Info;
      end if;
   end X_Match_Visual_Info;


   -- -------------------------------------------------------------------------
   --
   --  XImage
   --

-- UseX11R6 X11R6.3
   procedure X_Init_Image (Image : in X_Image_Pointer) is
      function XInitImage (Image : in X_Image_Pointer) return Status_Type;
      pragma Import (C, XInitImage, "XInitImage");
   begin
      if XInitImage (Image) = Error_Status then
         raise X_Error;
      end if;
   end X_Init_Image;
-- EndX11R6 X11R6.3


   procedure X_Destroy_Image (Image : in X_Image_Pointer) is
   begin
      Image.F.Destroy_Image (Image);
   end X_Destroy_Image;


   function X_Get_Pixel (Image : in X_Image_Pointer;
                         X, Y  : in Integer) return Pixel is
   begin
      return Image.F.Get_Pixel (Image, X, Y);
   end X_Get_Pixel;


   procedure X_Put_Pixel (Image : in X_Image_Pointer;
                          X, Y  : in Integer;
                          Pix   : in Pixel) is
   begin
      Image.F.Put_Pixel (Image, X, Y, Pix);
   end X_Put_Pixel;


   function X_Sub_Image (Image  : in X_Image_Pointer;
                         X, Y   : in Integer;
                         Width,
			 Height : in Integer)
                         return X_Image_Pointer is
   begin
      return Image.F.Sub_Image (Image, X, Y, Width, Height);
   end X_Sub_Image;


   procedure X_Add_Pixel (Image : in X_Image_Pointer;
                          Pix   : in Pixel) is
   begin
      Image.F.Add_Pixel (Image, Pix);
   end X_Add_Pixel;


   -- -------------------------------------------------------------------------
   --
   --   drawing functions
   --
   procedure X_Draw_Arc
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension;
      Angle1,
      Angle2  : in Angle) is
      procedure XDrawArc
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Width,
         Height  : in Interfaces.C.unsigned;
         Angle1,
         Angle2  : in Interfaces.C.int);
      pragma Import (C, XDrawArc, "XDrawArc");
   begin
      XDrawArc (Display, D, GC, Interfaces.C.int (X), Interfaces.C.int (Y),
                Interfaces.C.unsigned (Width),
                Interfaces.C.unsigned (Height),
                Interfaces.C.int (Angle1*64.0),
                Interfaces.C.int (Angle2*64.0));
   end X_Draw_Arc;
   pragma Inline (X_Draw_Arc);



   procedure X_Draw_Arcs
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Arcs    : in Arc_Array) is
      procedure XDrawArcs
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         Arcs    : in System.Address;
         N_Arcs  : in Interfaces.C.int);
      pragma Import (C, XDrawArcs, "XDrawArcs");
      Arc_Arry     : X_Arc_Array (Arcs'Range);
   begin
      for I in Arc_Arry'Range loop
         Arc_Arry (I) := (X      => Interfaces.C.short (Arcs (I).X),
                          Y      => Interfaces.C.short (Arcs (I).Y),
                          Width  => Interfaces.C.unsigned_short (Arcs (I).Width),
                          Height => Interfaces.C.unsigned_short (Arcs (I).Height),
                          Angle1 => Interfaces.C.short (Arcs (I).Angle1*64.0),
                          Angle2 => Interfaces.C.short (Arcs (I).Angle2*64.0));
      end loop;
      XDrawArcs (Display, D, GC, Arc_Arry'Address, Arcs'Length);
   end X_Draw_Arcs;


   procedure X_Draw_Line
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X1, Y1,
      X2, Y2  : in Position) is
      procedure XDrawLine
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X1, Y1,
         X2, Y2  : in Interfaces.C.int);
      pragma Import (C, XDrawLine, "XDrawLine");
   begin
      XDrawLine (Display, D, GC,
                 Interfaces.C.int (X1), Interfaces.C.int (Y1),
                 Interfaces.C.int (X2), Interfaces.C.int (Y2));
   end X_Draw_Line;
   pragma Inline (X_Draw_Line);


   type Coord_Mode_Int is range 0 .. 2**Coord_Mode'Size - 1;
   function To_Integer is
      new Ada.Unchecked_Conversion (Coord_Mode, Coord_Mode_Int);

   type Shape_Mode_Int is range 0 .. 2**Shape_Mode'Size - 1;
   function To_Integer is
      new Ada.Unchecked_Conversion (Shape_Mode, Shape_Mode_Int);

   procedure X_Draw_Lines
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Mode    : in Coord_Mode) is
      procedure XDrawLines
        (Display  : in Display_Pointer;
         D        : in Drawable_ID;
         GC       : in GC_Pointer;
         Points   : in System.Address;
         N_Points : in Interfaces.C.int;
         Mode     : in Interfaces.C.int);
      pragma Import (C, XDrawLines, "XDrawLines");
      Point_Arry   : X_Point_Array (Points'Range);
   begin
      for I in Points'Range loop
         Point_Arry (I) := (X  => Interfaces.C.short (Points (I).X),
                            Y  => Interfaces.C.short (Points (I).Y));
      end loop;
      XDrawLines (Display, D, GC,
                  Point_Arry'Address, Points'Length,
                  Interfaces.C.int (To_Integer (Mode)));
   end X_Draw_Lines;



   procedure X_Draw_Point
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position) is
      procedure XDrawPoint
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int);
      pragma Import (C, XDrawPoint, "XDrawPoint");
   begin
      XDrawPoint (Display, D, GC, Interfaces.C.int (X), Interfaces.C.int (Y));
   end X_Draw_Point;
   pragma Inline (X_Draw_Point);



   procedure X_Draw_Points
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Mode    : in Coord_Mode) is
      procedure XDrawPoints
        (Display  : in Display_Pointer;
         D        : in Drawable_ID;
         GC       : in GC_Pointer;
         Points   : in System.Address;
         N_Points : in Interfaces.C.int;
         Mode     : in Interfaces.C.int);
      pragma Import (C, XDrawPoints, "XDrawPoints");
      Point_Arry   : X_Point_Array (Points'Range);
   begin
      for I in Points'Range loop
         Point_Arry (I) := (X  => Interfaces.C.short (Points (I).X),
                            Y  => Interfaces.C.short (Points (I).Y));
      end loop;
      XDrawPoints (Display, D, GC,
                   Point_Arry'Address, Points'Length,
                   Interfaces.C.int (To_Integer (Mode)));
   end X_Draw_Points;


   procedure X_Draw_Rectangle
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension) is
      procedure XDrawRectangle
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Width,
         Height  : in Interfaces.C.unsigned);
      pragma Import (C, XDrawRectangle, "XDrawRectangle");
   begin
      XDrawRectangle (Display, D, GC,
                      Interfaces.C.int (X), Interfaces.C.int (Y),
                      Interfaces.C.unsigned (Width),
                      Interfaces.C.unsigned (Height));
   end X_Draw_Rectangle;
   pragma Inline (X_Draw_Rectangle);


   procedure X_Draw_Rectangles
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Rectangles : in Rectangle_Array) is
      procedure XDrawRectangles
        (Display      : in Display_Pointer;
         D            : in Drawable_ID;
         GC           : in GC_Pointer;
         Rectangles   : in System.Address;
         N_Rectangles : in Interfaces.C.int);
      pragma Import (C, XDrawRectangles, "XDrawRectangles");
      Rect_Arry    : X_Rectangle_Array (Rectangles'Range);
   begin
      for I in Rectangles'Range loop
         Rect_Arry (I) := (X      => Interfaces.C.short (Rectangles (I).X),
                           Y      => Interfaces.C.short (Rectangles (I).Y),
                           Width  => Interfaces.C.unsigned_short (Rectangles (I).Width),
                           Height => Interfaces.C.unsigned_short (Rectangles (I).Height));
      end loop;
      XDrawRectangles (Display, D, GC,
                       Rect_Arry'Address, Rectangles'Length);
   end X_Draw_Rectangles;


   procedure X_Draw_Segments
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Segments   : in Segment_Array) is
      procedure XDrawSegments
        (Display    : in Display_Pointer;
         D          : in Drawable_ID;
         GC         : in GC_Pointer;
         Segments   : in System.Address;
         N_Segments : in Interfaces.C.int);
      pragma Import (C, XDrawSegments, "XDrawSegments");
      Seg_Arry     : X_Segment_Array (Segments'Range);
   begin
      for I in Segments'Range loop
         Seg_Arry (I) := (X1  => Interfaces.C.short (Segments (I).X1),
                          Y1  => Interfaces.C.short (Segments (I).Y1),
                          X2  => Interfaces.C.short (Segments (I).X2),
                          Y2  => Interfaces.C.short (Segments (I).Y2));
      end loop;
      XDrawSegments (Display, D, GC,
                     Seg_Arry'Address, Segments'Length);
   end X_Draw_Segments;



   procedure X_Draw_String
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Str     : in String) is
      procedure XDrawString
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Str     : in System.Address;
         Length  : in Interfaces.C.int);
      pragma Import (C, XDrawString, "XDrawString");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Str, Append_Nul => True);
   begin
      XDrawString (Display, D, GC,
                   Interfaces.C.int (X), Interfaces.C.int (Y),
                   Name_String'Address, Str'Length);
   end X_Draw_String;
   pragma Inline (X_Draw_String);


   procedure X_Draw_Image_String
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Str     : in String) is
      procedure XDrawImageString
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Str     : in System.Address;
         Length  : in Interfaces.C.int);
      pragma Import (C, XDrawImageString, "XDrawImageString");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Str, Append_Nul => True);
   begin
      XDrawImageString (Display, D, GC,
                        Interfaces.C.int (X), Interfaces.C.int (Y),
                        Name_String'Address, Str'Length);
   end X_Draw_Image_String;
   pragma Inline (X_Draw_Image_String);



   procedure X_Fill_Arc
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension;
      Angle1,
      Angle2  : in Angle) is
      procedure XFillArc
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Width,
         Height  : in Interfaces.C.unsigned;
         Angle1,
         Angle2  : in Interfaces.C.int);
      pragma Import (C, XFillArc, "XFillArc");
   begin
      XFillArc (Display, D, GC, Interfaces.C.int (X), Interfaces.C.int (Y),
                Interfaces.C.unsigned (Width), Interfaces.C.unsigned (Height),
                Interfaces.C.int (Angle1*64.0), Interfaces.C.int (Angle2*64.0));
   end X_Fill_Arc;
   pragma Inline (X_Fill_Arc);


   procedure X_Fill_Arcs
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Arcs    : in Arc_Array) is
      procedure XFillArcs
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         Arcs    : in System.Address;
         N_Arcs  : in Interfaces.C.int);
      pragma Import (C, XFillArcs, "XFillArcs");
      Arc_Arry : X_Arc_Array (Arcs'Range);
   begin
      for I in Arc_Arry'Range loop
         Arc_Arry (I) := (X      => Interfaces.C.short (Arcs (I).X),
                          Y      => Interfaces.C.short (Arcs (I).Y),
                          Width  => Interfaces.C.unsigned_short (Arcs (I).Width),
                          Height => Interfaces.C.unsigned_short (Arcs (I).Height),
                          Angle1 => Interfaces.C.short (Arcs (I).Angle1*64.0),
                          Angle2 => Interfaces.C.short (Arcs (I).Angle2*64.0));
      end loop;
      XFillArcs (Display, D, GC, Arc_Arry'Address, Arcs'Length);
   end X_Fill_Arcs;


   procedure X_Fill_Polygon
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      Points  : in Point_Array;
      Shape   : in Shape_Mode;
      Mode    : in Coord_Mode) is
      procedure XFillPolygon
        (Display  : in Display_Pointer;
         D        : in Drawable_ID;
         GC       : in GC_Pointer;
         Points   : in System.Address;
         N_Points : in Interfaces.C.int;
         Shape    : in Interfaces.C.int;
         Mode     : in Interfaces.C.int);
      pragma Import (C, XFillPolygon, "XFillPolygon");
      Point_Arry : X_Point_Array (Points'Range);
   begin
      for I in Points'Range loop
         Point_Arry (I) := (X  => Interfaces.C.short (Points (I).X),
                            Y  => Interfaces.C.short (Points (I).Y));
      end loop;
      XFillPolygon (Display, D, GC, Point_Arry'Address, Points'Length,
                    Interfaces.C.int (To_Integer (Shape)),
		    Interfaces.C.int (To_Integer (Mode)));
   end X_Fill_Polygon;



   procedure X_Fill_Rectangle
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      GC      : in GC_Pointer;
      X, Y    : in Position;
      Width,
      Height  : in Dimension) is
      procedure XFillRectangle
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         GC      : in GC_Pointer;
         X, Y    : in Interfaces.C.int;
         Width,
         Height  : in Interfaces.C.unsigned);
      pragma Import (C, XFillRectangle, "XFillRectangle");
   begin
      XFillRectangle (Display, D, GC,
                      Interfaces.C.int (X), Interfaces.C.int (Y),
                      Interfaces.C.unsigned (Width), Interfaces.C.unsigned (Height));
   end X_Fill_Rectangle;
   pragma Inline (X_Fill_Rectangle);


   procedure X_Fill_Rectangles
     (Display    : in Display_Pointer;
      D          : in Drawable_ID;
      GC         : in GC_Pointer;
      Rectangles : in Rectangle_Array) is
      procedure XFillRectangles
        (Display      : in Display_Pointer;
         D            : in Drawable_ID;
         GC           : in GC_Pointer;
         Rectangles   : in System.Address;
         N_Rectangles : in Interfaces.C.int);
      pragma Import (C, XFillRectangles, "XFillRectangles");
      Rect_Arry : X_Rectangle_Array (Rectangles'Range);
   begin
      for I in Rectangles'Range loop
         Rect_Arry (I) := (X      => Interfaces.C.short (Rectangles (I).X),
                           Y      => Interfaces.C.short (Rectangles (I).Y),
                           Width  => Interfaces.C.unsigned_short (Rectangles (I).Width),
                           Height => Interfaces.C.unsigned_short (Rectangles (I).Height));
      end loop;
      XFillRectangles (Display, D, GC, Rect_Arry'Address, Rectangles'Length);
   end X_Fill_Rectangles;



   procedure X_Copy_Area
     (Display        : in Display_Pointer;
      Src            : in Drawable_ID;
      Dest           : in Drawable_ID;
      GC             : in GC_Pointer;
      Src_X, Src_Y   : in Position;
      Width,
      Height         : in Dimension;
      Dest_X, Dest_Y : in Position) is
      procedure XCopyArea
        (Display        : in Display_Pointer;
         Src            : in Drawable_ID;
         Dest           : in Drawable_ID;
         GC             : in GC_Pointer;
         Src_X, Src_Y   : in Interfaces.C.int;
         Width,
         Height         : in Interfaces.C.unsigned;
         Dest_X, Dest_Y : in Interfaces.C.int);
      pragma Import (C, XCopyArea, "XCopyArea");
   begin
      XCopyArea (Display, Src, Dest, GC,
                 Interfaces.C.int (Src_X), Interfaces.C.int (Src_Y),
                 Interfaces.C.unsigned (Width), Interfaces.C.unsigned (Height),
                 Interfaces.C.int (Dest_X), Interfaces.C.int (Dest_Y));
   end X_Copy_Area;
   pragma Inline (X_Copy_Area);



   procedure X_Copy_Plane
     (Display        : in Display_Pointer;
      Src            : in Drawable_ID;
      Dest           : in Drawable_ID;
      GC             : in GC_Pointer;
      Src_X, Src_Y   : in Position;
      Width,
      Height         : in Dimension;
      Dest_X, Dest_Y : in Position;
      Plane          : in Color_Plane_Mask) is
      procedure XCopyPlane
        (Display        : in Display_Pointer;
         Src            : in Drawable_ID;
         Dest           : in Drawable_ID;
         GC             : in GC_Pointer;
         Src_X, Src_Y   : in Interfaces.C.int;
         Width,
         Height         : in Interfaces.C.unsigned;
         Dest_X, Dest_Y : in Interfaces.C.int;
         Plane          : in Color_Plane_Mask);
      pragma Import (C, XCopyPlane, "XCopyPlane");
   begin
      XCopyPlane (Display, Src, Dest, GC,
                  Interfaces.C.int (Src_X), Interfaces.C.int (Src_Y),
                  Interfaces.C.unsigned (Width), Interfaces.C.unsigned (Height),
                  Interfaces.C.int (Dest_X), Interfaces.C.int (Dest_Y),
                  Plane);
   end X_Copy_Plane;
   pragma Inline (X_Copy_Plane);



   procedure X_Clear_Area
     (Display    : in Display_Pointer;
      Window     : in Window_ID;
      X, Y       : in Position;
      Width,
      Height     : in Dimension;
      Exposures  : in Boolean) is
      procedure XClearArea
        (Display    : in Display_Pointer;
         Window     : in Window_ID;
         X, Y       : in Interfaces.C.int;
         Width,
         Height     : in Interfaces.C.unsigned;
         Exposures  : in X_Boolean);
      pragma Import (C, XClearArea, "XClearArea");
   begin
      XClearArea (Display, Window,
                  Interfaces.C.int (X), Interfaces.C.int (Y),
                  Interfaces.C.unsigned (Width), Interfaces.C.unsigned (Height),
		  To_X_Boolean (Exposures));
   end X_Clear_Area;


   -- -------------------------------------------------------------------------
   --
   --   Screen
   --
   function X_Screen_Resource_String (Screen : in Screen_Pointer)
      return String is
      function XScreenResourceString (Screen : in Screen_Pointer)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, XScreenResourceString, "XScreenResourceString");
      -- the returned string has to be freed!
      procedure XFree (X : in Interfaces.C.Strings.chars_ptr);
      pragma Import (C, XFree, "XFree");

      Resource_Str : Interfaces.C.Strings.chars_ptr;
   begin
      Resource_Str := XScreenResourceString (Screen);
      if Resource_Str = Interfaces.C.Strings.null_ptr then
         return String'("");
      else
         declare
            Return_String : constant String :=Interfaces.C.Strings.Value (Resource_Str);
         begin
            XFree (Resource_Str);
            return String'(Return_String);
         end;
      end if;
   end X_Screen_Resource_String;


   function X_Does_Save_Unders (Screen : in Screen_Pointer)
      return Boolean is
      function XDoesSaveUnders (Screen : in Screen_Pointer)
         return X_Boolean;
      pragma Import (C, XDoesSaveUnders, "XDoesSaveUnders");
   begin
      return To_Boolean (XDoesSaveUnders (Screen));
   end X_Does_Save_Unders;



   -- -------------------------------------------------------------------------
   --
   --   Color
   --

   -- allocate shared color cells (i.e. color cells allocated with these
   -- routines musn't be modified)
   --
   procedure X_Alloc_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorcell_Def : in out X_Color) is
      function XAllocColor
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Colorcell_Def : in System.Address)
	 return Status_Type;
      pragma Import (C, XAllocColor, "XAllocColor");
   begin
      if XAllocColor (Display, C_Map, Colorcell_Def'Address) = Error_Status then
         raise X_Color_Error;
      end if;
   end X_Alloc_Color;


   procedure X_Alloc_Named_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorname     : in     String;
      Colorcell_Def :    out X_Color;
      RGB_DB_Def    :    out X_Color) is
      function XAllocNamedColor
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Colorname     : in System.Address;
         Colorcell_Def : in System.Address;
         RGB_DB_Def    : in System.Address)
	 return Status_Type;
      pragma Import (C, XAllocNamedColor, "XAllocNamedColor");
      Colorname_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Colorname, Append_Nul => True);
   begin
      if XAllocNamedColor (Display, C_Map, Colorname_String'Address,
                           Colorcell_Def'Address,
                           RGB_DB_Def'Address) = Error_Status then
         raise X_Color_Error;
      end if;
   end X_Alloc_Named_Color;


   -- allocate private color cells
   --
   procedure X_Alloc_Color_Cells
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Contig        : in     Boolean;
      Plane_Masks   :    out Color_Plane_Mask_Array_Type;
      Pixels        :    out Pixel_Array_Type) is
      function XAllocColorCells
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Contig        : in X_Boolean;
         Plane_Masks   : in System.Address;
	 N_Plane_Masks : in Interfaces.C.unsigned;
         Pixels        : in System.Address;
	 N_Pixels      : in Interfaces.C.unsigned)
	 return Status_Type;
      pragma Import (C, XAllocColorCells, "XAllocColorCells");
   begin
      if XAllocColorCells (Display, C_Map, To_X_Boolean (Contig),
                           Plane_Masks'Address,
                           Interfaces.C.unsigned (Plane_Masks'Length),
			   Pixels'Address,
                           Interfaces.C.unsigned (Pixels'Length)) = Error_Status then
         raise X_Color_Error;
      end if;
   end X_Alloc_Color_Cells;


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
      Blue_Mask     :    out Color_Plane_Mask) is
      function XAllocColorPlanes
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Contig        : in X_Boolean;
         Pixels        : in System.Address;
	 N_Pixels      : in Interfaces.C.int;
         N_Red_Planes,
         N_Green_Planes,
         N_Blue_Planes : in Interfaces.C.int;
         Red_Mask      : in System.Address;
         Green_Mask    : in System.Address;
         Blue_Mask     : in System.Address)
	 return Status_Type;
      pragma Import (C, XAllocColorPlanes, "XAllocColorPlanes");
   begin
      if XAllocColorPlanes (Display, C_Map, To_X_Boolean (Contig),
                            Pixels'Address, Pixels'Length,
			    Interfaces.C.int (N_Red_Planes),
			    Interfaces.C.int (N_Green_Planes),
			    Interfaces.C.int (N_Blue_Planes),
			    Red_Mask'Address, Green_Mask'Address,
			    Blue_Mask'Address) = Error_Status then
         raise X_Color_Error;
      end if;
   end X_Alloc_Color_Planes;


   -- store color definition in private color cells
   --
   procedure X_Store_Color
     (Display  : in     Display_Pointer;
      C_Map    : in     Colormap_ID;
      Color    : in out X_Color) is
      procedure XStoreColor
        (Display  : in Display_Pointer;
         C_Map    : in Colormap_ID;
         Color    : in System.Address);
      pragma Import (C, XStoreColor, "XStoreColor");
   begin
      XStoreColor (Display, C_Map, Color'Address);
   end X_Store_Color;
   pragma Inline (X_Store_Color);


   procedure X_Store_Colors
     (Display  : in     Display_Pointer;
      C_Map    : in     Colormap_ID;
      Colors   : in out X_Color_Array) is
      procedure XStoreColors
        (Display  : in Display_Pointer;
         C_Map    : in Colormap_ID;
         Colors   : in System.Address;
	 N_Colors : in Interfaces.C.int);
      pragma Import (C, XStoreColors, "XStoreColors");
   begin
      XStoreColors (Display, C_Map, Colors'Address, Colors'Length);
   end X_Store_Colors;
   pragma Inline (X_Store_Colors);



   procedure X_Store_Name
     (Display : in Display_Pointer;
      Window  : in Window_ID;
      Name    : in String)
   is
      procedure XStoreName
        (Display : in Display_Pointer;
         Window  : in Window_ID;
         Name    : in interfaces.C.strings.chars_ptr);
      pragma Import (C, XStoreName, "XStoreName");

      c_Name : interfaces.C.strings.chars_ptr := new_String (Name);
   begin
      XStoreName (Display, Window, C_Name);
      free (c_Name);
   end;




   procedure X_Store_Named_Color
     (Display    : in Display_Pointer;
      C_Map      : in Colormap_ID;
      Colorname  : in String;
      Pix        : in Pixel;
      Flags      : in Color_Component_Mask) is

      procedure XStoreNamedColor
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Colorname     : in System.Address;
	 Pix           : in Pixel;
	 Flags         : in Interfaces.C.int);
      pragma Import (C, XStoreNamedColor, "XStoreNamedColor");

      use Interfaces.C;
      Colorname_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Colorname, Append_Nul => True);
      C_Flags      : Interfaces.C.int := 0;
   begin
      if Flags.Do_Red then
         C_Flags := C_Flags + 1;
      end if;
      if Flags.Do_Green then
         C_Flags := C_Flags + 2;
      end if;
      if Flags.Do_Blue then
         C_Flags := C_Flags + 4;
      end if;
      XStoreNamedColor (Display, C_Map,
                        Colorname_String'Address,
                        Pix, C_Flags);
   end X_Store_Named_Color;



   procedure X_Lookup_Color
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorname     : in     String;
      RGB_DB_Def    :    out X_Color;
      Hardware_Def  :    out X_Color) is
      function XLookupColor
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Colorname     : in System.Address;
         RGB_DB_Def    : in System.Address;
         Hardware_Def  : in System.Address)
	 return Status_Type;
      pragma Import (C, XLookupColor, "XLookupColor");
      Colorname_String : constant Interfaces.C.Char_Array
                       := Interfaces.C.To_C (Colorname, Append_Nul => True);
   begin
      if XLookupColor (Display, C_Map, Colorname_String'Address,
                       RGB_DB_Def'Address,
                       Hardware_Def'Address) = Error_Status then
         raise X_Color_Error;
      end if;
   end X_Lookup_Color;


   procedure X_Parse_Color
     (Display    : in     Display_Pointer;
      C_Map      : in     Colormap_ID;
      Spec       : in     String;
      RGB_DB_Def :    out X_Color) is
      function XParseColor
        (Display    : in Display_Pointer;
         C_Map      : in Colormap_ID;
         Spec       : in System.Address;
         RGB_DB_Def : in System.Address)
	 return Status_Type;
      pragma Import (C, XParseColor, "XParseColor");
      Spec_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Spec, Append_Nul => True);
   begin
      if XParseColor (Display, C_Map, Spec_String'Address, RGB_DB_Def'Address) =
         Error_Status then
         raise X_Color_Error;
      end if;
   end X_Parse_Color;



   procedure X_Query_Colors
     (Display       : in     Display_Pointer;
      C_Map         : in     Colormap_ID;
      Colorcell_Def : in out X_Color_Array) is
      procedure XQueryColors
        (Display       : in Display_Pointer;
         C_Map         : in Colormap_ID;
         Colorcell_Def : in System.Address;
         N_Colors      : in Interfaces.C.int);
      pragma Import (C, XQueryColors, "XQueryColors");
   begin
      XQueryColors (Display, C_Map, Colorcell_Def'Address, Colorcell_Def'Length);
   end X_Query_Colors;


   -- free color cells obtained by X_Alloc_Color, X_Alloc_Named_Color,
   -- X_Alloc_Color_Cells and X_Alloc_Color_Planes
   --
   procedure X_Free_Colors
     (Display  : in Display_Pointer;
      C_Map    : in Colormap_ID;
      Pixels   : in Pixel_Array_Type;
      Planes   : in Color_Plane_Mask := Color_Plane_Mask'(0)) is
      procedure XFreeColors
        (Display  : in Display_Pointer;
         C_Map    : in Colormap_ID;
         Pixels   : in System.Address;
	 N_Pixels : in Interfaces.C.int;
         Planes   : in Color_Plane_Mask);
      pragma Import (C, XFreeColors, "XFreeColors");
   begin
      XFreeColors (Display, C_Map,
                   Pixels'Address, Pixels'Length,
		   Planes);
   end X_Free_Colors;
   pragma Inline (X_Free_Colors);


   -- -------------------------------------------------------------------------
   --
   -- define how to handle resources after the program has finished
   --
   procedure X_Set_Close_Down_Mode
     (Display    : in Display_Pointer;
      Close_Mode : in Close_Mode_Type := Destroy_All) is
      procedure XSetCloseDownMode
        (Display    : in Display_Pointer;
         Close_Mode : in Close_Mode_Type);
      pragma Import (C, XSetCloseDownMode, "XSetCloseDownMode");
   begin
      XSetCloseDownMode (Display, Close_Mode);
   end X_Set_Close_Down_Mode;
   pragma Inline (X_Set_Close_Down_Mode);


   -- -------------------------------------------------------------------------
   --
   -- Window appearance
   --

   procedure X_Iconify_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Screen_No : in Screen_Number) is
      function XIconifyWindow
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Screen_No : in Screen_Number)
         return Status_Type;
      pragma Import (C, XIconifyWindow, "XIconifyWindow");
   begin
      if XIconifyWindow (Display, W, Screen_No) = Error_Status then
         raise X_Error;
      end if;
   end X_Iconify_Window;


   procedure X_Withdraw_Window
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Screen_No : in Screen_Number) is
      function XWithdrawWindow
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Screen_No : in Screen_Number)
         return Status_Type;
      pragma Import (C, XWithdrawWindow, "XWithdrawWindow");
   begin
      if XWithdrawWindow (Display, W, Screen_No) = Error_Status then
         raise X_Error;
      end if;
   end X_Withdraw_Window;


   procedure X_Restack_Windows
     (Display     : in Display_Pointer;
      Windows     : in Window_ID_Array_Type) is
      procedure XRestackWindows
        (Display     : in Display_Pointer;
         Windows     : in System.Address;
	 N_Windows   : in Interfaces.C.int);
      pragma Import (C, XRestackWindows, "XRestackWindows");
   begin
      XRestackWindows (Display, Windows'Address, Windows'Length);
   end X_Restack_Windows;
   pragma Inline (X_Restack_Windows);


   procedure X_Get_Geometry
     (Display       : in     Display_Pointer;
      D             : in     Drawable_ID;
      Root_Win      :    out Window_ID;
      X, Y          :    out Integer;
      Width, Height :    out Natural;
      Border_Width  :    out Natural;
      Depth         :    out Color_Depth) is
      function XGetGeometry
        (Display       : in Display_Pointer;
         D             : in Drawable_ID;
         Root_Win      : in System.Address;
         X, Y          : in System.Address;
         Width, Height : in System.Address;
         Border_Width  : in System.Address;
         Depth         : in System.Address)
         return Status_Type;
      pragma Import (C, XGetGeometry, "XGetGeometry");
   begin
      if XGetGeometry (Display, D, Root_Win'Address, X'Address, Y'Address,
                       Width'Address, Height'Address, Border_Width'Address,
                       Depth'Address) = Error_Status then
         raise X_Error;
      end if;
   end X_Get_Geometry;





   -- -------------------------------------------------------------------------
   --
   --  Font routines
   --

   function X_Load_Font (Display : in Display_Pointer;
                         Name    : in String)
      return Font_ID is
      function XLoadFont (Display : in Display_Pointer;
                          Name    : in System.Address)
         return Font_ID;
      pragma Import (C, XLoadFont, "XLoadFont");
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
      return XLoadFont (Display, Name_String'Address);
   end X_Load_Font;
   pragma Inline (X_Load_Font);


   function X_Load_Query_Font (Display : in Display_Pointer;
                               Name    : in String)
      return X_Font_Struct_Pointer is
      function XLoadQueryFont (Display : in Display_Pointer;
                               Name    : in System.Address)
         return X_Font_Struct_Pointer;
      pragma Import (C, XLoadQueryFont,"XLoadQueryFont");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Name, Append_Nul => True);
   begin
     return XLoadQueryFont (Display, Name_String'Address);
   end X_Load_Query_Font;
   pragma Inline (X_Load_Query_Font);


   procedure X_Query_Text_Extents
     (Display   : in Display_Pointer;
      FontId    : in Font_Id;
      Text      : in String;
      Direction : out Draw_Direction;
      Ascent    : out Dimension;
      Descent   : out Dimension;
      Overall   : out Char_Struct) is

      procedure XQueryTextExtents
        (Display   : in Display_Pointer;
         FontId    : in Font_Id;
         Text      : in System.Address;
         Nchars    : in Interfaces.C.int;
         direction : in System.address;
         Ascent    : in System.address;
         Descent   : in System.Address;
         Overall   : in System.Address);
      pragma Import (C, XQueryTextExtents, "XQueryTextExtents");

      Text_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Text, Append_Nul => True);
      Ascent_Int, Descent_Int : Interfaces.C.int;
   begin
      XQueryTextExtents (Display, FontId, Text_String'Address, Text'Length,
                         Direction'Address, Ascent_Int'Address,
                         Descent_Int'Address, Overall'Address);
      Ascent  := Dimension (Ascent_Int);
      Descent := Dimension (Descent_Int);
   end X_Query_Text_Extents;


   -- -------------------------------------------------------------------------
   --
   --  Pixmaps
   --

   function X_Create_Bitmap_From_Data
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      Data    : in Bitmap_Type)
      return X_Lib.Pixmap_ID is
      function XCreateBitmapFromData
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         Data    : in System.Address;
	 Width,
	 Height  : in Interfaces.C.unsigned)
         return X_Lib.Pixmap_ID;
      pragma Import (C, XCreateBitmapFromData, "XCreateBitmapFromData");
   begin
      return XCreateBitmapFromData (Display, D, Data'Address,
				    Data'Length (2)*Bits_8_Type'Size,
				    Data'Length (1));
   end X_Create_Bitmap_From_Data;


   function X_Create_Pixmap_From_Bitmap_Data
     (Display : in Display_Pointer;
      D       : in Drawable_ID;
      Data    : in Bitmap_Type;
      FG, BG  : in Pixel;
      Depth   : in Color_Depth)
      return Pixmap_ID is
      function XCreatePixmapFromBitmapData
        (Display : in Display_Pointer;
         D       : in Drawable_ID;
         Data    : in System.Address;
	 Width,
	 Height  : in Interfaces.C.unsigned;
	 FG, BG  : in Pixel;
	 Depth   : in Color_Depth)
         return X_Lib.Pixmap_ID;
      pragma Import (C, XCreatePixmapFromBitmapData, "XCreatePixmapFromBitmapData");
   begin
      return XCreatePixmapFromBitmapData (Display, D, Data'Address,
				          Data'Length (2)*Bits_8_Type'Size,
                                          Data'Length (1),
					  FG, BG, Depth);
   end X_Create_Pixmap_From_Bitmap_Data;


   procedure X_Read_Bitmap_File
     (Display     : in  Display_Pointer;
      Drawable    : in  Drawable_ID;
      Filename    : in  String;
      Width       : out Dimension;
      Height      : out Dimension;
      Bitmap      : out Pixmap_ID;
      X_Hot       : out Position;
      Y_Hot       : out Position) is

      function XReadBitmapFile (Display     : in Display_Pointer;
                                Drawable    : in Drawable_ID;
                                Filename    : in System.Address;
                                Width       : in System.Address;
                                Height      : in System.Address;
                                Bitmap      : in System.Address;
                                X_Hot       : in System.Address;
                                Y_Hot       : in System.Address)
         return Integer;
      pragma Import (C, XReadBitmapFile, "XReadBitmapFile");

      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Filename, Append_Nul => True);
      X_Hot_Return, Y_Hot_Return  : Interfaces.C.int;
      Width_Return, Height_Return : Interfaces.C.unsigned;
   begin
      case XReadBitmapFile (Display, Drawable, Name_String'Address,
                            Width_Return'Address, Height_Return'Address,
                            Bitmap'Address,
                            X_Hot_Return'Address, Y_Hot_Return'Address) is
         when 0 =>
            null;
         when 1 =>   -- BitmapOpenFailed
            raise X_Error_Open_Failed;
         when 2 =>   -- BitmapFileInvalid
            raise X_Error_File_Invalid;
         when 3 =>   -- BitmapNoMemory
            raise X_Error_No_Memory;
         when others =>    -- ???
            raise X_Error;
      end case;
      Width  := Dimension (Width_Return);
      Height := Dimension (Height_Return);
      X_Hot  := Position  (X_Hot_Return);
      Y_Hot  := Position  (Y_Hot_Return);
   end X_Read_Bitmap_File;



   procedure X_Write_Bitmap_File
     (Display     : in Display_Pointer;
      Filename    : in String;
      Bitmap      : in Pixmap_ID;
      Width       : in Dimension;
      Height      : in Dimension;
      X_Hot       : in Position := Position'(-1);
      Y_Hot       : in Position := Position'(-1)) is
      function XWriteBitmapFile
        (Display     : in Display_Pointer;
         Filename    : in System.Address;
         Bitmap      : in Pixmap_ID;
         Width       : in Interfaces.C.unsigned;
         Height      : in Interfaces.C.unsigned;
         X_Hot       : in Interfaces.C.int;
         Y_Hot       : in Interfaces.C.int)
         return Integer;
      pragma Import (C, XWriteBitmapFile, "XWriteBitmapFile");
      Name_String : constant Interfaces.C.Char_Array
                  := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      case XWriteBitmapFile (Display, Name_String'Address,
                             Bitmap, Interfaces.C.unsigned (Width),
                             Interfaces.C.unsigned (Height),
                             Interfaces.C.int (X_Hot),
                             Interfaces.C.int (Y_Hot)) is
         when 0 =>
            return;
         when 1 =>   -- BitmapOpenFailed
            raise X_Error_Open_Failed;
         when 3 =>   -- BitmapNoMemory
            raise X_Error_No_Memory;
         when others =>    -- ???
            raise X_Error;
      end case;
   end X_Write_Bitmap_File;




   type C_String_Array is array (Natural range <>) of aliased Interfaces.C.Strings.Chars_Ptr;


   function X_List_Fonts (Display : in X_Lib.Display_Pointer;
                          Pattern : in String;
                          Max_Names : in Natural)
      return String_List.Element_Access_List is

      package String_Pointers is new
         Interfaces.C.Pointers (Natural,
                                Interfaces.C.Strings.Chars_Ptr,
                                C_String_Array,
                                Interfaces.C.Strings.Null_Ptr);

      function XListFonts
        (Display    : in X_Lib.Display_Pointer;
         Pattern    : in System.Address;
         Max_Names  : in Interfaces.C.int;
         N_Fonts    : in System.Address)
	 return String_Pointers.Pointer;
      pragma Import (C, XListFonts, "XListFonts");
      procedure XFreeFontNames (List : in String_Pointers.Pointer);
      pragma Import (C, XFreeFontNames, "XFreeFontNames");

      Pattern_String : constant Interfaces.C.Char_Array
                     := Interfaces.C.To_C (Pattern, Append_Nul => True);
      Hook : String_Pointers.Pointer;
      Number : Interfaces.C.int;

      Return_List : String_List.Element_Access_List;
   begin
      Hook := XListFonts  (Display, Pattern_String'Address,
                           Interfaces.C.int (Max_Names), Number'Address);
      declare
         List : constant C_String_Array (1 .. Natural (Number))
	      := String_Pointers.Value (Hook, Interfaces.C.Ptrdiff_T (Number));
      begin
         for I in List'Range loop
            String_List.Append (Return_List, Interfaces.C.Strings.Value (List (I)));
         end loop;
      end;
      XFreeFontNames (Hook); -- we don't need the list any more
      return Return_List;
   end X_List_Fonts;



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
      Best_Height :    out Dimension) is
      function XQueryBestStipple
        (Display     : in Display_Pointer;
         D           : in Drawable_ID;
         Width,
         Height      : in Interfaces.C.unsigned;
         Best_Width,
         Best_Height : in System.Address)
	 return Status_Type;
      pragma Import (C, XQueryBestStipple, "XQueryBestStipple");
      Best_W, Best_H : Interfaces.C.unsigned;
   begin
      if XQueryBestStipple (Display, D, Interfaces.C.unsigned (Width),
                            Interfaces.C.unsigned (Height),
                            Best_W'Address, Best_H'Address) = Error_Status then
         raise X_Error;
      end if;
      Best_Width  := Dimension (Best_W);
      Best_Height := Dimension (Best_H);
   end X_Query_Best_Stipple;



   procedure X_Query_Best_Tile
     (Display     : in     Display_Pointer;
      D           : in     Drawable_ID;
      Width,
      Height      : in     Dimension;
      Best_Width,
      Best_Height :    out Dimension) is
      function XQueryBestTile
        (Display     : in Display_Pointer;
         D           : in Drawable_ID;
         Width,
         Height      : in Interfaces.C.unsigned;
         Best_Width,
         Best_Height : in System.Address)
	 return Status_Type;
      pragma Import (C, XQueryBestTile, "XQueryBestTile");
      Best_W, Best_H : Interfaces.C.unsigned;
   begin
      if XQueryBestTile (Display, D, Interfaces.C.unsigned (Width),
                         Interfaces.C.unsigned (Height),
                         Best_W'Address, Best_H'Address) = Error_Status then
         raise X_Error;
      end if;
      Best_Width  := Dimension (Best_W);
      Best_Height := Dimension (Best_H);
   end X_Query_Best_Tile;



   function X_List_Pixmap_Formats (Display : in Display_Pointer)
      return X_Pixmap_Format_Values_Array is

      package Pixmap_Array_Interface is new
         Interfaces.C.Pointers (Natural,
                                X_Pixmap_Format_Values,
                                X_Pixmap_Format_Values_Array,
                                X_Pixmap_Format_Values'(Depth => 0,
                                                        Bits_Per_Pixel => 0,
                                                        Scanline_Pad => 0));
      function XListPixmapFormats
        (Display : in Display_Pointer;
         Number  : in System.Address)
         return Pixmap_Array_Interface.Pointer;
      pragma Import (C, XListPixmapFormats, "XListPixmapFormats");

      Hook_To_Array   : Pixmap_Array_Interface.Pointer;
      Number          : Interfaces.C.int;

   begin
      Hook_To_Array := XListPixmapFormats (Display, Number'Address);
      return Pixmap_Array_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end X_List_Pixmap_Formats;


   function X_List_Installed_Colormaps (Display : in Display_Pointer;
                                        W       : in Window_ID)
      return Colormap_ID_Array is

      package Colormap_ID_Array_Interface is new
         Interfaces.C.Pointers (Natural,
                                Colormap_ID,
                                Colormap_ID_Array,
                                Colormap_ID (0));
      function XListInstalledColormaps
        (Display : in Display_Pointer;
         W       : in Window_ID;
         Number  : in System.Address)
         return Colormap_ID_Array_Interface.Pointer;
      pragma Import (C, XListInstalledColormaps, "XListInstalledColormaps");

      Hook_To_Array   : Colormap_ID_Array_Interface.Pointer;
      Number          : Interfaces.C.int;

   begin
      Hook_To_Array := XListInstalledColormaps (Display, W, Number'Address);
      return Colormap_ID_Array_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end X_List_Installed_Colormaps;


   function X_List_Properties (Display : in Display_Pointer;
                               W       : in Window_ID)
      return Atom_Array is

      package Atom_Array_Interface is new
         Interfaces.C.Pointers (Natural,
                                Atom,
                                Atom_Array,
                                Atom (0));
      function XListProperties (Display : in Display_Pointer;
                                W       : in Window_ID;
                                Number  : in System.Address)
         return Atom_Array_Interface.Pointer;
      pragma Import (C, XListProperties, "XListProperties");

      Hook_To_Array   : Atom_Array_Interface.Pointer;
      Number          : Interfaces.C.int;

   begin
      Hook_To_Array := XListProperties (Display, W, Number'Address);
      return Atom_Array_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end X_List_Properties;



   function X_List_Depths
     (Display   : in Display_Pointer;
      Screen_No : in Screen_Number)
      return Color_Depth_Array is

      package Color_Depth_Interface is new
         Interfaces.C.Pointers (Natural,
                                Color_Depth,
                                Color_Depth_Array,
                                Color_Depth (0));
      function XListDepths (Display   : in Display_Pointer;
                            Screen_No : in Screen_Number;
                            Number    : in System.Address)
         return Color_Depth_Interface.Pointer;
      pragma Import (C, XListDepths, "XListDepths");

      Hook_To_Array   : Color_Depth_Interface.Pointer;
      Number          : Interfaces.C.int;

   begin
      Hook_To_Array := XListDepths (Display, Screen_No, Number'Address);
      return Color_Depth_Interface.Value (Hook_To_Array, Interfaces.C.Ptrdiff_T (Number));
   end X_List_Depths;



-- ----------------------------------------------------------------------------
--
--  internationalization
--
   type X_Font_Set_Extents is record
      Max_Ink_Extent     : X_Rectangle;
      Max_Logical_Extent : X_Rectangle;
   end record;

   type X_Font_Set_Extents_Access is access all X_Font_Set_Extents;


   function X_Extents_Of_Font_Set
     (Font_Set : in Font_Set_Type)
      return Font_Set_Extents_Type is

      function XExtentsOfFontSet
        (Font_Set : in Font_Set_Type)
         return X_Font_Set_Extents_Access;
      pragma Import (C, XExtentsOfFontSet, "XExtentsOfFontSet");

      Extents_Acc : X_Font_Set_Extents_Access;
   begin
      -- DON'T FREE memory of X_Font_Set_Extents!
      Extents_Acc := XExtentsOfFontSet (Font_Set);
      if Extents_Acc /= null then
         -- return copy of struct
         return Font_Set_Extents_Type'(Max_Ink_Extent     => To_Rectangle (Extents_Acc.Max_Ink_Extent),
	                               Max_Logical_Extent => To_Rectangle (Extents_Acc.Max_Logical_Extent));
      else
         raise X_Error;
      end if;
   end X_Extents_Of_Font_Set;


-- UseX11R6 X11R6.3
   -- -------------------------------------------------------------------------
   --
   --  Threads routines
   --
   procedure X_Init_Threads is
      function XInitThreads return Status_Type;
      pragma Import (C, XInitThreads, "XInitThreads");
   begin
      if XInitThreads = Error_Status then
        raise X_Error_No_Threads;
      end if;
   end X_Init_Threads;

-- EndX11R6 X11R6.3


   -- -------------------------------------------------------------------------
   --
   --   Window Attributes
   --

   procedure X_Change_Window_Attributes
     (Display    : in Display_Pointer;
      W          : in Window_ID;
      Valuemask  : in Set_Window_Attributes_Mask;
      Attributes : in Set_Window_Attributes_Type) is
      procedure XChangeWindowAttributes
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Valuemask  : in C_Set_Window_Attributes_Mask;
         Attributes : in System.Address);
      pragma Import (C, XChangeWindowAttributes, "XChangeWindowAttributes");
   begin
      XChangeWindowAttributes (Display, W,
                               To_Long (Valuemask),
                               Attributes'Address);
   end X_Change_Window_Attributes;


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
      return Window_ID is
      function XCreateWindow
        (Display       : in Display_Pointer;
         Parent        : in Window_ID;
         X, Y          : in Interfaces.C.int;
         Width, Height : in Interfaces.C.unsigned;
         Border_Width  : in Interfaces.C.unsigned;
         Depth         : in Color_Depth;
         Class         : in Window_Class;
         Visual        : in Visual_Pointer;
         Valuemask     : in C_Set_Window_Attributes_Mask;
         Attributes    : in System.Address)
         return Window_ID;
      pragma Import (C, XCreateWindow, "XCreateWindow");
   begin
      return XCreateWindow (Display, Parent,
                            Interfaces.C.int (X), Interfaces.C.int (Y),
			    Interfaces.C.unsigned (Width),
			    Interfaces.C.unsigned (Height),
			    Interfaces.C.unsigned (Border_Width),
                            Depth, Class, Visual, To_Long (Valuemask),
                            Attributes'Address);
   end X_Create_Window;


   procedure X_Configure_Window
     (Display    : in Display_Pointer;
      W          : in Window_ID;
      Value_Mask : in Window_Changes_Mask;
      Values     : in Window_Changes_Type) is
      procedure XConfigureWindow
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Value_Mask : in C_Window_Changes_Mask;
         Values     : in System.Address);
      pragma Import (C, XConfigureWindow, "XConfigureWindow");
   begin
      XConfigureWindow (Display, W, To_Int (Value_Mask), Values'Address);
   end X_Configure_Window;


   procedure X_Resize_Window
     (Display       : in Display_Pointer;
      W             : in Window_ID;
      Width, Height : in Dimension) is
      procedure XResizeWindow
        (Display       : in Display_Pointer;
         W             : in Window_ID;
         Width, Height : in Interfaces.C.unsigned);
      pragma Import (C, XResizeWindow, "XResizeWindow");
   begin
      XResizeWindow (Display, W, Interfaces.C.unsigned (Width),
                     Interfaces.C.unsigned (Height));
   end X_Resize_Window;
   pragma Inline (X_Resize_Window);



   procedure X_Move_Window
     (Display       : in Display_Pointer;
      W             : in Window_ID;
      X, Y : in Dimension) is
      procedure XMoveWindow
        (Display       : in Display_Pointer;
         W             : in Window_ID;
         Width, Height : in Interfaces.C.unsigned);
      pragma Import (C, XMoveWindow, "XMoveWindow");
   begin
      XMoveWindow (Display, W, Interfaces.C.unsigned (X),
                               Interfaces.C.unsigned (Y));
   end X_Move_Window;
   pragma Inline (X_Move_Window);


   -- -------------------------------------------------------------------------
   --
   --  G R A B   F U N C T I O N S
   --

   function To_Unsigned is
      new Ada.Unchecked_Conversion (Event_Mask, Interfaces.C.unsigned);


   type Modifiers_Mask_Int is mod 2**8; -- 2**Modifiers_Mask_Type'Size;
   function To_Unsigned is
      new Ada.Unchecked_Conversion (Modifiers_Mask_Type, Modifiers_Mask_Int);

   function To_Unsigned (Mod_Mask : in Modifiers_Mask_Type)
      return Interfaces.C.unsigned is
   begin
      return
         Interfaces.C.unsigned (Modifiers_Mask_Int'(To_Unsigned (Mod_Mask)));
   end To_Unsigned;
   pragma Inline (To_Unsigned);


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
      Cursor        : in Cursor_ID := Null_Cursor_ID) is
      procedure XGrabButton
        (Display       : in Display_Pointer;
         Button        : in Button_Type;
         Modifiers     : in Interfaces.C.unsigned;
         Grab_Window   : in Window_ID;
         Owner_Events  : in X_Boolean;
         Ev_Mask       : in Interfaces.C.unsigned;
         Pointer_Mode  : in Grab_Mode_Type;
         Keyboard_Mode : in Grab_Mode_Type;
         Confine_To    : in Window_ID;
         Cursor        : in Cursor_ID);
      pragma Import (C, XGrabButton, "XGrabButton");
   begin
      XGrabButton (Display, Button, To_Unsigned (Modifiers),
                   Grab_Window, To_X_Boolean (Owner_Events),
		   To_Unsigned (Ev_Mask),
		   Pointer_Mode, Keyboard_Mode,
		   Confine_To, Cursor);
   end X_Grab_Button;


   procedure X_Ungrab_Button
     (Display       : in Display_Pointer;
      Button        : in Button_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID) is
      procedure XUngrabButton
        (Display       : in Display_Pointer;
         Button        : in Button_Type;
         Modifiers     : in Interfaces.C.unsigned;
         Grab_Window   : in Window_ID);
      pragma Import (C, XUngrabButton, "XUngrabButton");
   begin
      XUngrabButton (Display, Button, To_Unsigned (Modifiers), Grab_Window);
   end X_Ungrab_Button;



   procedure X_Grab_Pointer
     (Display       : in Display_Pointer;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Ev_Mask       : in Event_Mask;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type;
      Confine_To    : in Window_ID := Null_Window_ID;
      Cursor        : in Cursor_ID := Null_Cursor_ID;
      Time          : in Server_Time) is
      function XGrabPointer
        (Display       : in Display_Pointer;
         Grab_Window   : in Window_ID;
         Owner_Events  : in X_Boolean;
         Ev_Mask       : in Interfaces.C.unsigned;
         Pointer_Mode  : in Grab_Mode_Type;
         Keyboard_Mode : in Grab_Mode_Type;
         Confine_To    : in Window_ID;
         Cursor        : in Cursor_ID;
         Time          : in Server_Time)
	 return Integer;
      pragma Import (C, XGrabPointer, "XGrabPointer");
   begin
      case XGrabPointer (Display, Grab_Window, To_X_Boolean (Owner_Events),
                         To_Unsigned (Ev_Mask), Pointer_Mode, Keyboard_Mode,
			 Confine_To, Cursor, Time) is
         when 0 =>
	    return;
	 when 1 =>
	    raise X_Error_Already_Grabbed;
	 when 2 =>
	    raise X_Error_Grab_Invalid_Time;
	 when 3 =>
	    raise X_Error_Grab_Not_Viewable;
	 when 4 =>
	    raise X_Error_Grab_Frozen;
         when others =>
	    raise X_Error;
      end case;
   end X_Grab_Pointer;


   procedure X_Ungrab_Pointer
     (Display : in Display_Pointer;
      Time    : in Server_Time) is
      procedure XUngrabPointer
        (Display : in Display_Pointer;
         Time    : in Server_Time);
      pragma Import (C, XUngrabPointer, "XUngrabPointer");
   begin
      XUngrabPointer (Display, Time);
   end X_Ungrab_Pointer;


   procedure X_Change_Active_Pointer_Grab
     (Display       : in Display_Pointer;
      Ev_Mask       : in Event_Mask;
      Cursor        : in Cursor_ID := Null_Cursor_ID;
      Time          : in Server_Time) is
      procedure XChangeActivePointerGrab
        (Display       : in Display_Pointer;
         Ev_Mask       : in Interfaces.C.unsigned;
         Cursor        : in Cursor_ID;
         Time          : in Server_Time);
      pragma Import (C, XChangeActivePointerGrab, "XChangeActivePointerGrab");
   begin
      XChangeActivePointerGrab (Display, To_Unsigned (Ev_Mask), Cursor, Time);
   end X_Change_Active_Pointer_Grab;
   pragma Inline (X_Change_Active_Pointer_Grab);


   procedure X_Grab_Key
     (Display       : in Display_Pointer;
      Keycode       : in Key_Code_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type) is
      procedure XGrabKey
        (Display       : in Display_Pointer;
         Keycode       : in Key_Code_Type;
         Modifiers     : in Interfaces.C.unsigned;
         Grab_Window   : in Window_ID;
         Owner_Events  : in X_Boolean;
         Pointer_Mode  : in Grab_Mode_Type;
         Keyboard_Mode : in Grab_Mode_Type);
      pragma Import (C, XGrabKey, "XGrabKey");
   begin
      XGrabKey (Display, Keycode, To_Unsigned (Modifiers),
                Grab_Window, To_X_Boolean (Owner_Events),
		Pointer_Mode, Keyboard_Mode);
   end X_Grab_Key;


   procedure X_Ungrab_Key
     (Display       : in Display_Pointer;
      Keycode       : in Key_Code_Type;
      Modifiers     : in Modifiers_Mask_Type;
      Grab_Window   : in Window_ID) is
      procedure XUngrabKey
        (Display       : in Display_Pointer;
         Keycode       : in Key_Code_Type;
         Modifiers     : in Interfaces.C.unsigned;
         Grab_Window   : in Window_ID);
      pragma Import (C, XUngrabKey, "XUngrabKey");
   begin
      XUngrabKey (Display, Keycode, To_Unsigned (Modifiers),
                  Grab_Window);
   end X_Ungrab_Key;


   procedure X_Grab_Keyboard
     (Display       : in Display_Pointer;
      Grab_Window   : in Window_ID;
      Owner_Events  : in Boolean;
      Pointer_Mode  : in Grab_Mode_Type;
      Keyboard_Mode : in Grab_Mode_Type;
      Time          : in Server_Time) is
      function XGrabKeyboard
        (Display       : in Display_Pointer;
         Grab_Window   : in Window_ID;
         Owner_Events  : in X_Boolean;
         Pointer_Mode  : in Grab_Mode_Type;
         Keyboard_Mode : in Grab_Mode_Type;
         Time          : in Server_Time)
	 return Integer;
      pragma Import (C, XGrabKeyboard, "XGrabKeyboard");
   begin
      case XGrabKeyboard (Display, Grab_Window, To_X_Boolean (Owner_Events),
                          Pointer_Mode, Keyboard_Mode, Time) is
         when 0 =>
	    return;
	 when 1 =>
	    raise X_Error_Already_Grabbed;
	 when 2 =>
	    raise X_Error_Grab_Invalid_Time;
	 when 3 =>
	    raise X_Error_Grab_Not_Viewable;
	 when 4 =>
	    raise X_Error_Grab_Frozen;
         when others =>
	    raise X_Error;
      end case;
   end X_Grab_Keyboard;


   -- -------------------------------------------------------------------------
   --
   --   E V E N T S
   --
   function To_Int is
      new Ada.Unchecked_Conversion (Event_Mask, Interfaces.C.long);


   procedure X_Select_Input
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Ev_Mask   : in Event_Mask) is
      procedure XSelectInput
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Ev_Mask   : in Interfaces.C.long);
      pragma Import (C, XSelectInput, "XSelectInput");
   begin
      XSelectInput (Display, W, To_Int (Ev_Mask));
   end X_Select_Input;


   procedure X_Send_Event
     (Display   : in Display_Pointer;
      W         : in Window_ID;
      Propagate : in Boolean;
      Ev_Mask   : in Event_Mask;
      Event     : in X_Event) is
      function XSendEvent
        (Display   : in Display_Pointer;
         W         : in Window_ID;
         Propagate : in X_Boolean;
         Ev_Mask   : in Interfaces.C.long;
         Event     : in System.Address)
      return Status_Type;
      pragma Import (C, XSendEvent, "XSendEvent");
   begin
      if XSendEvent (Display, W, To_X_Boolean (Propagate),
                     To_Int (Ev_Mask), Event'Address) = Error_Status then
         raise X_Error;
      end if;
   end X_Send_Event;


   procedure X_Next_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event) is
      procedure XNextEvent
        (Display  : in Display_Pointer;
         Event    : in System.Address);
      pragma Import (C, XNextEvent, "XNextEvent");
   begin
      XNextEvent (Display, Event'Address);
   end X_Next_Event;
   pragma Inline (X_Next_Event);


   procedure X_Peek_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event) is
      procedure XPeekEvent
        (Display  : in Display_Pointer;
         Event    : in System.Address);
      pragma Import (C, XPeekEvent, "XPeekEvent");
   begin
      XPeekEvent (Display, Event'Address);
   end X_Peek_Event;
   pragma Inline (X_Peek_Event);


   procedure X_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event) is
      procedure XWindowEvent
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Ev_Mask    : in Interfaces.C.long;
         Event      : in System.Address);
      pragma Import (C, XWindowEvent, "XWindowEvent");
   begin
      XWindowEvent (Display, W, To_Int (Ev_Mask), Event'Address);
   end X_Window_Event;


   procedure X_Mask_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event) is
      procedure XMaskEvent
        (Display    : in Display_Pointer;
         Ev_Mask    : in Interfaces.C.long;
         Event      : in System.Address);
      pragma Import (C, XMaskEvent, "XMaskEvent");
   begin
      XMaskEvent (Display, To_Int (Ev_Mask), Event'Address);
   end X_Mask_Event;



   procedure X_Check_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean) is
      function XCheckWindowEvent
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Ev_Mask    : in Interfaces.C.long;
         Event      : in System.Address)
         return X_Boolean;
      pragma Import (C, XCheckWindowEvent, "XCheckWindowEvent");
   begin
      Found := XCheckWindowEvent (Display, W, To_Int (Ev_Mask), Event'Address) = X_Boolean'(True);
   end X_Check_Window_Event;
   pragma Inline (X_Check_Window_Event);


   procedure X_Check_Mask_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean) is
      function XCheckMaskEvent
        (Display    : in Display_Pointer;
         Ev_Mask    : in Interfaces.C.long;
         Event      : in System.Address)
         return X_Boolean;
      pragma Import (C, XCheckMaskEvent, "XCheckMaskEvent");
   begin
      Found := XCheckMaskEvent (Display, To_Int (Ev_Mask), Event'Address) = X_Boolean'(True);
   end X_Check_Mask_Event;
   pragma Inline (X_Check_Mask_Event);


   procedure X_Check_Typed_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event;
      Found    :    out Boolean) is
      function XCheckTypedEvent
        (Display    : in Display_Pointer;
         Ev_Type    : in Event_Type;
         Event      : in System.Address)
         return X_Boolean;
      pragma Import (C, XCheckTypedEvent, "XCheckTypedEvent");
   begin
      Found := XCheckTypedEvent (Display, Event.Ev_Type, Event'Address) = X_Boolean'(True);
   end X_Check_Typed_Event;
   pragma Inline (X_Check_Typed_Event);


   procedure X_Check_Typed_Window_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Event    :    out X_Event;
      Found    :    out Boolean) is
      function XCheckTypedWindowEvent
        (Display    : in Display_Pointer;
         W          : in Window_ID;
         Ev_Type    : in Event_Type;
         Event      : in System.Address)
         return X_Boolean;
      pragma Import (C, XCheckTypedWindowEvent, "XCheckTypedWindowEvent");
   begin
      Found := XCheckTypedWindowEvent (Display, W, Event.Ev_Type, Event'Address) = X_Boolean'(True);
   end X_Check_Typed_Window_Event;
   pragma Inline (X_Check_Typed_Window_Event);


   procedure X_Put_Back_Event
     (Display  : in Display_Pointer;
      Event    : in X_Event) is
      procedure XPutBackEvent
        (Display  : in Display_Pointer;
         Event    : in System.Address);
      pragma Import (C, XPutBackEvent, "XPutBackEvent");
   begin
      XPutBackEvent (Display, Event'Address);
   end X_Put_Back_Event;
   pragma Inline (X_Put_Back_Event);


   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event) is
   begin
      X_Next_Event (Display, Event);
   end Wait_For_Event;
   pragma Inline (Wait_For_Event);


   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event) is
   begin
      X_Mask_Event (Display, Ev_Mask, Event);
   end Wait_For_Event;
   pragma Inline (Wait_For_Event);


   procedure Wait_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event) is
   begin
      X_Window_Event (Display, W, Ev_Mask, Event);
   end Wait_For_Event;
   pragma Inline (Wait_For_Event);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean) is
   begin
      X_Check_Window_Event (Display, W, Ev_Mask, Event, Found);
   end Look_For_Event;
   pragma Inline (Look_For_Event);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      Ev_Mask  : in     Event_Mask;
      Event    :    out X_Event;
      Found    :    out Boolean) is
   begin
      X_Check_Mask_Event (Display, Ev_Mask, Event, Found);
   end Look_For_Event;
   pragma Inline (Look_For_Event);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      Event    :    out X_Event;
      Found    :    out Boolean) is
   begin
      X_Check_Typed_Event (Display, Event, Found);
   end Look_For_Event;
   pragma Inline (Look_For_Event);


   procedure Look_For_Event
     (Display  : in     Display_Pointer;
      W        : in     Window_ID;
      Event    :    out X_Event;
      Found    :    out Boolean) is
   begin
      X_Check_Typed_Window_Event (Display, W, Event, Found);
   end Look_For_Event;
   pragma Inline (Look_For_Event);


   -- -------------------------------------------------------------------------
   --
   -- flush the output buffer and wait, until all events have been received
   -- AND PROCESSED by the X-Server
   --
   procedure X_Sync
     (Display        : in Display_Pointer;
      Discard_Events : in Boolean := False) is
      procedure XSync
        (Display        : in Display_Pointer;
         Discard_Events : in X_Boolean);
      pragma Import (C, XSync, "XSync");
   begin
      XSync (Display, To_X_Boolean (Discard_Events));
   end X_Sync;


   procedure X_Query_Pointer
     (Display        : in     Display_Pointer;
      W              : in     Window_Id;
      Root, Child    :    out Window_Id;
      Root_X, Root_Y :    out Position;
      Win_X, Win_Y   :    out Position;
      Keys_Buttons   :    out Modifier_And_Button_Mask;
      Valid          :    out Boolean) is

      function XQueryPointer
        (Display        : in Display_Pointer;
         W              : in Window_Id;
         Root, Child    : in System.Address;
         Root_X, Root_Y : in System.Address;
         Win_X, Win_Y   : in System.Address;
         Keys_Buttons   : in System.Address)
         return X_Boolean;
      pragma Import (C, XQueryPointer, "XQueryPointer");
      Root_X_Int, Root_Y_Int, Win_X_Int, Win_Y_Int : Interfaces.C.int;
   begin
      Valid := To_Boolean (XQueryPointer (Display, W,
                              Root'Address, Child'Address,
                              Root_X_Int'Address, Root_Y_Int'Address,
                              Win_X_Int'Address, Win_Y_Int'Address,
                              Keys_buttons'Address));
      Root_X := Position (Root_X_Int);
      Root_Y := Position (Root_Y_Int);
      Win_X  := Position (Win_X_Int);
      Win_Y  := Position (Win_Y_Int);
   end X_Query_Pointer;


   -- -------------------------------------------------------------------------
   --
   --  Conversion routines
   --
   function To_Address (Source: in Display_Pointer) return System.Address is
   begin
      return System.Address (Source);
   end To_Address;


   function To_Display_Pointer (Source : in System.Address) return Display_Pointer is
   begin
      return Display_Pointer (Source);
   end To_Display_Pointer;


   function To_Address (Source: in Screen_Pointer)  return System.Address is
   begin
      return System.Address (Source);
   end To_Address;


   function To_Screen_Pointer  (Source : in System.Address) return Screen_Pointer is
   begin
      return Screen_Pointer (Source);
   end To_Screen_Pointer;


   function To_Address (Source: in GC_Pointer) return System.Address is
   begin
      return System.Address (Source);
   end To_Address;


   function To_GC_Pointer (Source : in System.Address) return GC_Pointer is
   begin
      return GC_Pointer (Source);
   end To_GC_Pointer;


   function To_Address (Source: in X_Pointer) return System.Address is
   begin
      return System.Address (Source);
   end To_Address;


   function To_X_Pointer (Source : in System.Address) return X_Pointer is
   begin
      return X_Pointer (Source);
   end To_X_Pointer;


end X_Lib;
