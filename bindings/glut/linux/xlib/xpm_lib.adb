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

with Interfaces.C.Pointers,
     Interfaces.C.Strings,
     Interfaces.C.Strings.String_Lists,
     System,
     Ada.Unchecked_Conversion,
     Ada.Text_IO;
package body Xpm_Lib is

   use type Interfaces.C.unsigned;

   package ICS  renames Interfaces.C.Strings;
   package ICSS renames Interfaces.C.Strings.String_Lists;


   -- convertion routines from bitfields to integers and vice versa
   --
   type C_Xpm_Attr_Valuemask is new Interfaces.C.long;
   type Xpm_Attr_Valuemask_Int is mod 2**17; --Xpm_Attr_Valuemask'Size;
   function To_Mask_Int is
      new Ada.Unchecked_Conversion (Xpm_Attr_Valuemask,
                                    Xpm_Attr_Valuemask_Int);
   function To_Mask_Type is
      new Ada.Unchecked_Conversion (Xpm_Attr_Valuemask_Int,
                                    Xpm_Attr_Valuemask);
   
   function To_Long (Mask : in Xpm_Attr_Valuemask) return C_Xpm_Attr_Valuemask is
   begin
      return C_Xpm_Attr_Valuemask (To_Mask_Int (Mask));
   end To_Long;
   pragma Inline (To_Long);


   function To_Mask (C_Mask : in C_Xpm_Attr_Valuemask) return Xpm_Attr_Valuemask is
   begin
      return To_Mask_Type (Xpm_Attr_Valuemask_Int (C_Mask));
   end To_Mask;
   pragma Inline (To_Mask);


   package Int_Io is new Ada.Text_Io.Integer_Io (Integer);


   type Xpm_Attributes_Private is record
      Value_Mask       : C_Xpm_Attr_Valuemask;

      -- Image/Pixmap creation directives
      Visual           : Visual_Pointer;
      Colormap         : Colormap_ID;
      Depth            : Color_Depth;

      -- data related to the xpm file
      Width            : Interfaces.C.unsigned;
      Height           : Interfaces.C.unsigned;
      X_Hotspot        : Interfaces.C.unsigned;
      Y_Hotspot        : Interfaces.C.unsigned;
      Cpp              : Interfaces.C.unsigned;

      Pixels           : System.Address;
      N_Pixels         : Interfaces.C.unsigned;
      Color_Symbols    : System.Address;
      Num_Symbols      : Interfaces.C.unsigned;

      Rgb_Fname        : X_Strings.X_String;

      N_Extensions     : Interfaces.C.unsigned;
      Extensions       : System.Address;
      N_Colors         : Interfaces.C.unsigned;
      Color_Table      : System.Address;
      -- 3.2 backward compatibility code
      Hints_Cmt        : X_Strings.X_String;
      Colors_Cmt       : X_Strings.X_String;
      Pixels_Cmt       : X_Strings.X_String;
      -- end 3.2 backward compatibility code
      Mask_Pixel       : Pixel;
      -- color allocation directives
      Exact_Colors     : Interfaces.C.unsigned;
      Closeness        : Interfaces.C.unsigned;
      Red_Closeness    : Interfaces.C.unsigned;
      Green_Closeness  : Interfaces.C.unsigned;
      Blue_Closeness   : Interfaces.C.unsigned;
      Color_Key        : Xpm_Color_Key_Class;
      -- new from Xpm-Lib 3.4g
      Alloc_Pixels     : System.Address;
      N_Alloc_Pixels   : Interfaces.C.unsigned;
   end record;


   function To_Attributes_Private (Attr : in Xpm_Attributes)
      return Xpm_Attributes_Private is

      Return_Attr : Xpm_Attributes_Private;

      use System;
   begin
      Return_Attr.Value_Mask       := To_Long (Attr.Value_Mask);
      Return_Attr.Visual           := Attr.Visual;
      Return_Attr.Colormap         := Attr.Colormap;
      Return_Attr.Depth            := Attr.Depth;
      if Attr.Value_Mask.Size then
         Return_Attr.Width            := Interfaces.C.unsigned (Attr.Width);
         Return_Attr.Height           := Interfaces.C.unsigned (Attr.Height);
      end if;
      if Attr.Value_Mask.Hotspot then
         Return_Attr.X_Hotspot        := Interfaces.C.unsigned (Attr.X_Hotspot);
         Return_Attr.Y_Hotspot        := Interfaces.C.unsigned (Attr.Y_Hotspot);
      end if;
      if Attr.Value_Mask.Chars_Per_Pixel then
         Return_Attr.Cpp              := Interfaces.C.unsigned (Attr.Cpp);
      end if;
      if Attr.Pixels = null then
         Return_Attr.Pixels        := System.Null_Address;
         Return_Attr.N_Pixels      := 0;
      else
         Return_Attr.Pixels           := Attr.Pixels.all (Attr.Pixels.all'First)'Address;
         Return_Attr.N_Pixels         := Attr.Pixels.all'Length;
      end if;
      if Attr.Color_Symbols = null then
         Return_Attr.Color_Symbols    := System.Null_Address;
         Return_Attr.Num_Symbols      := 0;
      else
         Return_Attr.Color_Symbols    := Attr.Color_Symbols.all (Attr.Color_Symbols.all'First)'Address;
         Return_Attr.Num_Symbols      := Attr.Color_Symbols.all'Length;
      end if;
      Return_Attr.Rgb_Fname        := Attr.Rgb_FName;
      if Attr.Extensions = null then
         Return_Attr.N_Extensions  := 0;
         Return_Attr.Extensions    := System.Null_Address;
      else
         Return_Attr.N_Extensions     := Attr.Extensions.all'Length;
         Return_Attr.Extensions       := Attr.Extensions.all (Attr.Extensions.all'First)'Address;
      end if;
      if Attr.Color_Table = null then
         Return_Attr.N_Colors    := 0;
         Return_Attr.Color_Table := System.Null_Address;
      else
         Return_Attr.N_Colors         := Attr.Color_Table.all'Length;
         Return_Attr.Color_Table      := Attr.Color_Table.all (Attr.Color_Table.all'First)'Address;
      end if;
      -- 3.2 backward compatibility code
      Return_Attr.Hints_Cmt        := X_Strings.Null_X_String;
      Return_Attr.Colors_Cmt       := X_Strings.Null_X_String;
      Return_Attr.Pixels_Cmt       := X_Strings.Null_X_String;
      -- end 3.2 backward compatibility code
      Return_Attr.Mask_Pixel       := Attr.Mask_Pixel;
      -- color allocation directives
      if Attr.Exact_Colors then
         Return_Attr.Exact_Colors := 1;
      else
         Return_Attr.Exact_Colors := 0;
      end if;
      Return_Attr.Closeness        := Attr.Closeness;
      Return_Attr.Red_Closeness    := Attr.Red_Closeness;
      Return_Attr.Green_Closeness  := Attr.Green_Closeness;
      Return_Attr.Blue_Closeness   := Attr.Blue_Closeness;
      Return_Attr.Color_Key        := Attr.Color_Key;
      -- new from Xpm-Lib 3.4g
      if Attr.Alloc_Pixels = null then
         Return_Attr.Alloc_Pixels := System.Null_Address;
         Return_Attr.N_Alloc_Pixels := 0;
      else
         Return_Attr.Alloc_Pixels     := Attr.Alloc_Pixels.all (Attr.Alloc_Pixels.all'First)'Address;
         Return_Attr.N_Alloc_Pixels   := Attr.Alloc_Pixels.all'Length;
      end if;
      return Return_Attr;
   end To_Attributes_Private;



   function To_Attributes (Attr_P : in Xpm_Attributes_Private)
      return Xpm_Attributes is
      Return_Attr : Xpm_Attributes;

      package Pixels_Pointers is
         new Interfaces.C.Pointers (Natural,
                                    Pixel,
                                    Pixel_Array,
                                    Pixel (0));
      function To_Access is new Ada.Unchecked_Conversion (System.Address, Pixels_Pointers.Pointer);
      package Color_Symbol_Pointers is
         new Interfaces.C.Pointers (Natural,
                                    Xpm_Color_Symbol,
                                    Xpm_Color_Symbol_Array,
                                    Xpm_Color_Symbol'(Name  => X_Strings.Null_X_String,
                                                      Value => X_Strings.Null_X_String,
                                                      Pix   => Pixel (0)));
      function To_Access is new Ada.Unchecked_Conversion (System.Address, Color_Symbol_Pointers.Pointer);
      Null_Extension : Xpm_Extension;
      package Extension_Pointers is
         new Interfaces.C.Pointers (Natural,
                                    Xpm_Extension,
                                    Xpm_Extension_Array,
                                    Null_Extension);
      function To_Access is new Ada.Unchecked_Conversion (System.Address, Extension_Pointers.Pointer);
      package Color_Pointers is
         new Interfaces.C.Pointers (Natural,
                                    Xpm_Color,
                                    Xpm_Color_Array,
                                    Xpm_Color'(Str => X_Strings.Null_X_String,
                                               Symbolic => X_Strings.Null_X_String,
                                               M_Color => X_Strings.Null_X_String,
                                               G4_Color => X_Strings.Null_X_String,
                                               G_Color => X_Strings.Null_X_String,
                                               C_Color => X_Strings.Null_X_String));
      function To_Access is new Ada.Unchecked_Conversion (System.Address, Color_Pointers.Pointer);

      -- it is more secure this way
      Pix_Arry       : Pixel_Array (1 .. Natural (Attr_P.N_Pixels));
      Col_Sym_Arry   : Xpm_Color_Symbol_Array (1 .. Natural (Attr_P.Num_Symbols));
      Ext_Arry       : Xpm_Extension_Array (1 .. Natural (Attr_P.N_Extensions));
      Col_Arry       : Xpm_Color_Array (1 .. Natural (Attr_P.N_Colors));
      Pix_Alloc_Arry : Pixel_Array (1 .. Natural (Attr_P.N_Alloc_Pixels));

      use System, X_Strings;
   begin
      Return_Attr.Value_Mask := To_Mask (Attr_P.Value_Mask);
      Return_Attr.Visual     := Attr_P.Visual;
      Return_Attr.Colormap   := Attr_P.Colormap;
      Return_Attr.Depth      := Attr_P.Depth;
      if Return_Attr.Value_Mask.Size then
         Return_Attr.Width      := Dimension (Attr_P.Width);
         Return_Attr.Height     := Dimension (Attr_P.Height);
      end if;
      if Return_Attr.Value_Mask.Hotspot then
         Return_Attr.X_Hotspot  := Hotspot_Position (Attr_P.X_Hotspot);
         Return_Attr.Y_Hotspot  := Hotspot_Position (Attr_P.Y_Hotspot);
      end if;
      if Return_Attr.Value_Mask.Chars_Per_Pixel then
         Return_Attr.Cpp        := Natural (Attr_P.Cpp);
      end if;
      if Attr_P.Pixels = System.Null_Address or else Attr_P.N_Pixels = 0 then
         Return_Attr.Pixels := null;
      else
         Pix_Arry :=
             Pixels_Pointers.Value (To_Access (Attr_P.Pixels), Interfaces.C.Ptrdiff_T (Attr_P.N_Pixels));
         Return_Attr.Pixels           := new Pixel_Array'(Pix_Arry);
      end if;
      if Attr_P.Color_Symbols = System.Null_Address or else Attr_P.Num_Symbols = 0 then
         Return_Attr.Color_Symbols := null;
      else
         Col_Sym_Arry :=
             Color_Symbol_Pointers.Value (To_Access (Attr_P.Color_Symbols), Interfaces.C.Ptrdiff_T (Attr_P.Num_Symbols));
         Return_Attr.Color_Symbols    := new Xpm_Color_Symbol_Array'(Col_Sym_Arry);
      end if;
      -- duplicate the rgb filename
      if Return_Attr.Value_Mask.Rgb_Filename and then
         Attr_P.Rgb_FName /= Null_X_String then
         Return_Attr.Rgb_Fname        := To_X_String (To_String (Attr_P.Rgb_FName));
      else
         Return_Attr.Rgb_Fname        := Null_X_String;
      end if;
      if Attr_P.Extensions = System.Null_Address or else Attr_P.N_Extensions = 0 then
         Return_Attr.Extensions := null;
      else
         Ext_Arry :=
             Extension_Pointers.Value (To_Access (Attr_P.Extensions), Interfaces.C.Ptrdiff_T (Attr_P.N_Extensions));
         Return_Attr.Extensions       := new Xpm_Extension_Array'(Ext_Arry);
      end if;
      if Attr_P.Color_Table = System.Null_Address or else Attr_P.N_Colors = 0 then
         Return_Attr.Color_Table := null;
      else
         Col_Arry :=
             Color_Pointers.Value (To_Access (Attr_P.Color_Table), Interfaces.C.Ptrdiff_T (Attr_P.N_Colors));
         Return_Attr.Color_Table      := new Xpm_Color_Array'(Col_Arry);
      end if;

      Return_Attr.Mask_Pixel       := Attr_P.Mask_Pixel;
      -- color allocation directives
      Return_Attr.Exact_Colors     := Attr_P.Exact_Colors /= 0;
      Return_Attr.Closeness        := Attr_P.Closeness;
      Return_Attr.Red_Closeness    := Attr_P.Red_Closeness;
      Return_Attr.Green_Closeness  := Attr_P.Green_Closeness;
      Return_Attr.Blue_Closeness   := Attr_P.Blue_Closeness;
      Return_Attr.Color_Key        := Attr_P.Color_Key;

      -- new from Xpm-Lib 3.4g
      if Attr_P.Alloc_Pixels = System.Null_Address or else Attr_P.N_Alloc_Pixels = 0 then
         Return_Attr.Alloc_Pixels := null;
      else
         Pix_Alloc_Arry :=
             Pixels_Pointers.Value (To_Access (Attr_P.Alloc_Pixels), Interfaces.C.Ptrdiff_T (Attr_P.N_Alloc_Pixels));
         Return_Attr.Alloc_Pixels     := new Pixel_Array'(Pix_Alloc_Arry);
      end if;
      return Return_Attr;
   end To_Attributes;


   procedure Xpm_Free (Item : in out String_List_Conversion.Chars_Ptr_List_Type) is
      procedure XpmFree (Item : in String_List_Conversion.Chars_Ptr_List_Type);
-- should be XpmFree, but this exists only in more recent versions of the
-- Xpm library
--      pragma Import (C, XpmFree, "XpmFree");
      pragma Import (C, XpmFree, "free");
   begin
      XpmFree (Item);
      Item := String_List_Conversion.Null_Chars_Ptr_List;
   end Xpm_Free;
   pragma Inline (Xpm_Free);


   procedure Xpm_Free_Attributes (Attributes : in out Xpm_Attributes_Private) is
      procedure XpmFreeAttributes (Attr : in System.Address);
      pragma Import (C, XpmFreeAttributes, "XpmFreeAttributes");
   begin
      XpmFreeAttributes (Attributes'Address);
   end Xpm_Free_Attributes;
   pragma Inline (Xpm_Free_Attributes);


   procedure Xpm_Free_Extensions (Exts : in out Xpm_Extension_Array_Access) is
      procedure XpmFreeExtensions (Exts : in System.Address;
                                   N_Exts : in Integer);
      pragma Import (C, XpmFreeExtensions, "XpmFreeExtensions");
   begin
      XpmFreeExtensions (Exts.all (Exts.all'First)'Address, Exts.all'Length);
   end Xpm_Free_Extensions;
   pragma Inline (Xpm_Free_Extensions);


   procedure Check_Error_Code (Error : in Integer) is
   begin
      case Error is
         when 1 =>
            raise Xpm_Error_Color_Error;
         when 0 =>
            return;
         when -1 =>
            raise Xpm_Error_Open_Failed;
         when -2 =>
            raise Xpm_Error_File_Invalid;
         when -3 =>
            raise Xpm_Error_No_Memory;
         when -4 =>
            raise Xpm_Error_Color_Failed;
         when others =>
            raise Xpm_Error_Unknown;
      end case;
   end Check_Error_Code;
   pragma Inline (Check_Error_Code);


   -- convert a string list (C style char**) to a Pixmap_Data_Type
   -- (which is a String_List.Element_Access_List)
   -- can't simply be done by the routines in String_List_Conversion, because
   -- no trailing Null_Ptr is available and the number of lines isn't known
   --
   function To_Pixmap_Data_Type (Source : in String_List_Conversion.Chars_Ptr_List_Type)
      return Pixmap_Data_Type is

      Result : Pixmap_Data_Type;

      Tmp_Str   : ICS.chars_ptr;
      Width, Height, NColors, Cpp : Integer;
      Has_Ext                     : Boolean := False;
      Num_Lines : Interfaces.C.size_t;

      Ext_Identifier     : constant String := "XPMEXT";
      Ext_End_Identifier : constant String := "XPMENDEXT";
   begin
      -- read the first line
      Tmp_Str := ICSS.Value (Source, 0);
      -- we now try to interpret the first line
      declare
         First_Line : constant String := ICS.Value (Tmp_Str);
         Last_Idx : Integer;
      begin
         Int_Io.Get (First_Line, Width, Last_Idx);
         Int_Io.Get (First_Line (Last_Idx+1 .. First_Line'Last), Height, Last_Idx);
         Int_Io.Get (First_Line (Last_Idx+1 .. First_Line'Last), NColors, Last_Idx);
         Int_Io.Get (First_Line (Last_Idx+1 .. First_Line'Last), Cpp, Last_Idx);
         -- search if there is an XPMEXT identifier
         for I in Last_Idx+1 .. First_Line'Last - Ext_Identifier'Length + 1 loop
            if First_Line (I .. I+Ext_Identifier'Length-1) = Ext_Identifier then
               Has_Ext := True;
               exit;
            end if;
         end loop;
      end;
      Num_Lines := Interfaces.C.size_t (1 + NColors + Height);
      if Has_Ext then
         Tmp_Str   := ICSS.Value (Source, Num_Lines);
	 Num_Lines := Interfaces.C.size_t'Succ (Num_Lines);
         while ICS.Value (Tmp_Str) /= Ext_End_Identifier loop
            Tmp_Str := ICSS.Value (Source, Num_Lines);
	    Num_Lines := Interfaces.C.size_t'Succ (Num_Lines);
         end loop;
      end if;

      return String_List_Conversion.To_String_Access_List (Source, Num_Lines);
   end To_Pixmap_Data_Type;


   -- -------------------------------------------------------------------------
   --
   --  Read  FILE -> X_Image
   --

   function XpmReadFileToImage
     (Display		: in Display_Pointer;
      Filename  	: in System.Address;
      Image_Return	: in System.Address;
      Shapeimage_Return : in System.Address;
      Attr		: in System.Address)
      return Integer;
   pragma Import (C, XpmReadFileToImage, "XpmReadFileToImage");


   procedure Xpm_Read_File_To_Image
     (Display     : in	  Display_Pointer;
      Filename    : in	  String;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer;
      Attr        : in out Xpm_Attributes) is

      Error           : Integer;
      Attr_Private    : Xpm_Attributes_Private := To_Attributes_Private (Attr);

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error :=  XpmReadFileToImage (Display,
				    Filename_String'Address,
                                    Image'Address,
                                    Shape_Image'Address,
				    Attr_Private'Address);
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Read_File_To_Image;


   procedure Xpm_Read_File_To_Image
     (Display     : in	  Display_Pointer;
      Filename    : in	  String;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer) is

      Error           : Integer;
      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error :=  XpmReadFileToImage (Display,
				    Filename_String'Address,
                                    Image'Address,
                                    Shape_Image'Address,
				    Null_Address);
      Check_Error_Code (Error);
   end Xpm_Read_File_To_Image;


   -- -------------------------------------------------------------------------
   --
   --  Read  FILE -> Pixmap_ID
   --

   function XpmReadFileToPixmap
     (Display	  : in Display_Pointer;
      Drawable    : in Drawable_ID;
      Filename    : in System.Address;
      Pix_Return  : in System.Address;
      Mask_Return : in System.Address;
      Attr	  : in System.Address)
      return Integer;
   pragma Import (C, XpmReadFileToPixmap, "XpmReadFileToPixmap");


   procedure Xpm_Read_File_To_Pixmap
     (Display    : in     Display_Pointer;
      Drawable   : in     Drawable_ID;
      Filename   : in     String;
      Pix        :    out Pixmap_ID;
      Shape_Mask :    out Pixmap_ID;
      Attr       : in out Xpm_Attributes) is

      Error           : Integer;
      Attr_Private    : Xpm_Attributes_Private := To_Attributes_Private (Attr);

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error :=  XpmReadFileToPixmap (Display,
                                     Drawable,
                                     Filename_String'Address,
                                     Pix'Address,
                                     Shape_Mask'Address,
				     Attr_Private'Address);
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Read_File_To_Pixmap;


   procedure Xpm_Read_File_To_Pixmap
     (Display    : in     Display_Pointer;
      Drawable   : in     Drawable_ID;
      Filename   : in     String;
      Pix        :    out Pixmap_ID;
      Shape_Mask :    out Pixmap_ID) is

      Error           : Integer;
      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error :=  XpmReadFileToPixmap (Display,
                                     Drawable,
                                     Filename_String'Address,
                                     Pix'Address,
                                     Shape_Mask'Address,
				     Null_Address);
      Check_Error_Code (Error);
   end Xpm_Read_File_To_Pixmap;


   -- -------------------------------------------------------------------------
   --
   --  Read  FILE -> Pixmap_Data_Type
   --

   procedure Xpm_Read_File_To_Data
     (Filename    : in     String;
      Data        :    out Pixmap_Data_Type) is

      function XpmReadFileToData
        (Filename    : in System.Address;
         Data_Return : in System.Address)
         return Integer;
      pragma Import (C, XpmReadFileToData, "XpmReadFileToData");

      Error           : Integer;
      Tmp_Data        : String_List_Conversion.Chars_Ptr_List_Type;

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error := XpmReadFileToData (Filename_String'Address,
                                  Tmp_Data'Address);
      Check_Error_Code (Error);

      -- if no error, convert Tmp_Data to Pixmap_Data_Type
      Data := To_Pixmap_Data_Type (Tmp_Data);
      Xpm_Free (Tmp_Data);
   end Xpm_Read_File_To_Data;


   -- -------------------------------------------------------------------------
   --
   --  Create  DATA -> X_Image
   --

   function XpmCreateImageFromData
     (Display		 : in Display_Pointer;
      Data		 : in String_List_Conversion.Chars_Ptr_List_Type;
      Image_Return	 : in System.Address;
      Shape_Image_Return : in System.Address;
      Attr		 : in System.Address)
      return Integer;
   pragma Import (C, XpmCreateImageFromData , "XpmCreateImageFromData");


   procedure Xpm_Create_Image_From_Data
     (Display     : in     Display_Pointer;
      Data        : in     Pixmap_Data_Type;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer;
      Attr        : in out Xpm_Attributes) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);
   begin
      Tmp_Data := String_List_Conversion.To_Chars_Ptr_List (Data, Append_Null => False);
      Error := XpmCreateImageFromData (Display,
                                       Tmp_Data,
                                       Image'Address,
                                       Shape_Image'Address,
				       Attr_Private'Address);
      -- now free the temporarily needed Chars_Ptr_List_Type
      String_List_Conversion.Free (Tmp_Data,
                                   String_List_Conversion.Index_Type (String_List.Length (Data)));
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Create_Image_From_Data;


   procedure Xpm_Create_Image_From_Data
     (Display     : in     Display_Pointer;
      Data        : in     Pixmap_Data_Type;
      Image       :    out X_Image_Pointer;
      Shape_Image :    out X_Image_Pointer) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
   begin
      Tmp_Data := String_List_Conversion.To_Chars_Ptr_List (Data, Append_Null => False);
      Error := XpmCreateImageFromData (Display,
                                       Tmp_Data,
                                       Image'Address,
                                       Shape_Image'Address,
				       Null_Address);
      -- now free the temporarily needed Chars_Ptr_List_Type
      String_List_Conversion.Free (Tmp_Data,
                                   String_List_Conversion.Index_Type (String_List.Length (Data)));
      Check_Error_Code (Error);
   end Xpm_Create_Image_From_Data;


   -- -------------------------------------------------------------------------
   --
   --  Create  DATA -> Pixmap_ID
   --

   function XpmCreatePixmapFromData
     (Display	  : in Display_Pointer;
      Drawable    : in Drawable_ID;
      Data	  : in String_List_Conversion.Chars_Ptr_List_Type;
      Pix_Return  : in System.Address;
      Mask_Return : in System.Address;
      Attr	  : in System.Address)
      return Integer;
   pragma Import (C, XpmCreatePixmapFromData , "XpmCreatePixmapFromData");


   procedure Xpm_Create_Pixmap_From_Data
     (Display     : in     Display_Pointer;
      Drawable    : in     Drawable_ID;
      Data        : in     Pixmap_Data_Type;
      Pix         :    out Pixmap_ID;
      Shape_Mask  :    out Pixmap_ID;
      Attr        : in out Xpm_Attributes) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);
   begin
      Tmp_Data := String_List_Conversion.To_Chars_Ptr_List (Data, Append_Null => False);
      Error := XpmCreatePixmapFromData (Display,
                                        Drawable,
					Tmp_Data,
                                        Pix'Address,
                                        Shape_Mask'Address,
					Attr_Private'Address);
      -- now free the temporarily needed Chars_Ptr_List_Type
      String_List_Conversion.Free (Tmp_Data,
                                   String_List_Conversion.Index_Type (String_List.Length (Data)));
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Create_Pixmap_From_Data;


   procedure Xpm_Create_Pixmap_From_Data
     (Display     : in     Display_Pointer;
      Drawable    : in     Drawable_ID;
      Data        : in     Pixmap_Data_Type;
      Pix         :    out Pixmap_ID;
      Shape_Mask  :    out Pixmap_ID) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
   begin
      Tmp_Data := String_List_Conversion.To_Chars_Ptr_List (Data, Append_Null => False);
      Error := XpmCreatePixmapFromData (Display,
                                        Drawable,
					Tmp_Data,
                                        Pix'Address,
                                        Shape_Mask'Address,
					Null_Address);
      -- now free the temporarily needed Chars_Ptr_List_Type
      String_List_Conversion.Free (Tmp_Data,
                                   String_List_Conversion.Index_Type (String_List.Length (Data)));
      Check_Error_Code (Error);
   end Xpm_Create_Pixmap_From_Data;


   -- -------------------------------------------------------------------------
   --
   --  Write  X_Image -> DATA
   --

   function XpmCreateDataFromImage
     (Display	   : in Display_Pointer;
      Data_Return  : in System.Address;
      Image	   : in X_Image_Pointer;
      Shape_Image  : in X_Image_Pointer;
      Attr	   : in System.Address)
      return Integer;
   pragma Import (C, XpmCreateDataFromImage, "XpmCreateDataFromImage");


   procedure Xpm_Create_Data_From_Image
     (Display      : in     Display_Pointer;
      Image        : in     X_Image_Pointer;
      Shape_Image  : in     X_Image_Pointer;
      Data         :    out Pixmap_Data_Type;
      Attr         : in out Xpm_Attributes) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);
   begin
      Error := XpmCreateDataFromImage (Display,
                                       Tmp_Data'Address,
                                       Image,
				       Shape_Image,
				       Attr_Private'Address);
      Check_Error_Code (Error);

      -- if no error, convert Tmp_Data to Pixmap_Data_Type
      Data := To_Pixmap_Data_Type (Tmp_Data);
      Xpm_Free (Tmp_Data);

      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Create_Data_From_Image;


   procedure Xpm_Create_Data_From_Image
     (Display      : in     Display_Pointer;
      Image        : in     X_Image_Pointer;
      Shape_Image  : in     X_Image_Pointer;
      Data         :    out Pixmap_Data_Type) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
   begin
      Error := XpmCreateDataFromImage (Display,
                                       Tmp_Data'Address,
                                       Image,
				       Shape_Image,
				       Null_Address);
      Check_Error_Code (Error);

      -- if no error, convert Tmp_Data to Pixmap_Data_Type
      Data := To_Pixmap_Data_Type (Tmp_Data);
      Xpm_Free (Tmp_Data);
   end Xpm_Create_Data_From_Image;


   -- -------------------------------------------------------------------------
   --
   --  Write  Pixmap_ID -> DATA
   --

   function XpmCreateDataFromPixmap
     (Display	  : in Display_Pointer;
      Data_Return : in System.Address;
      Pix	  : in Pixmap_ID;
      Shape_Mask  : in Pixmap_ID;
      Attr	  : in System.Address)
      return Integer;
   pragma Import (C, XpmCreateDataFromPixmap, "XpmCreateDataFromPixmap");


   procedure Xpm_Create_Data_From_Pixmap
     (Display     : in     Display_Pointer;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Data        :    out Pixmap_Data_Type;
      Attr        : in out Xpm_Attributes) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);
   begin
      Error := XpmCreateDataFromPixmap (Display,
                                        Tmp_Data'Address,
                                        Pix,
					Shape_Mask,
					Attr_Private'Address);
      Check_Error_Code (Error);
      -- if no error, convert Tmp_Data to Pixmap_Data_Type
      Data := To_Pixmap_Data_Type (Tmp_Data);
      Xpm_Free (Tmp_Data);

      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Create_Data_From_Pixmap;


   procedure Xpm_Create_Data_From_Pixmap
     (Display     : in     Display_Pointer;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Data        :    out Pixmap_Data_Type) is

      Error        : Integer;
      Tmp_Data     : String_List_Conversion.Chars_Ptr_List_Type;
   begin
      Error := XpmCreateDataFromPixmap (Display,
                                        Tmp_Data'Address,
                                        Pix,
					Shape_Mask,
					Null_Address);
      Check_Error_Code (Error);
      -- if no error, convert Tmp_Data to Pixmap_Data_Type
      Data := To_Pixmap_Data_Type (Tmp_Data);
      Xpm_Free (Tmp_Data);
   end Xpm_Create_Data_From_Pixmap;


 
   -- -------------------------------------------------------------------------
   --
   --  Write  X_Image -> FILE
   --

   function XpmWriteFileFromImage
     (Display	  : in Display_Pointer;
      Filename    : in System.Address;
      Image	  : in X_Image_Pointer;
      Shapeimage  : in X_Image_Pointer;
      Attr	  : in System.Address)
      return Integer;
   pragma Import (C, XpmWriteFileFromImage, "XpmWriteFileFromImage");


   procedure Xpm_Write_File_From_Image
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Image       : in     X_Image_Pointer;
      Shape_Image : in     X_Image_Pointer;
      Attr        : in out Xpm_Attributes) is

      Error        : Integer;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error := XpmWriteFileFromImage (Display, Filename_String'Address,
                                      Image, Shape_Image, Attr_Private'Address);
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Write_File_From_Image;


   procedure Xpm_Write_File_From_Image
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Image       : in     X_Image_Pointer;
      Shape_Image : in     X_Image_Pointer) is

      Error           : Integer;

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error := XpmWriteFileFromImage (Display, Filename_String'Address,
                                      Image, Shape_Image, Null_Address);
      Check_Error_Code (Error);
   end Xpm_Write_File_From_Image;


   -- -------------------------------------------------------------------------
   --
   --  Write  Pixmap_ID -> FILE
   --

   function XpmWriteFileFromPixmap
     (Display	  : in Display_Pointer;
      Filename    : in System.Address;
      Pix	  : in Pixmap_ID;
      Shape_Mask  : in Pixmap_ID;
      Attr	  : in System.Address)
      return Integer;
   pragma Import (C, XpmWriteFileFromPixmap, "XpmWriteFileFromPixmap");


   procedure Xpm_Write_File_From_Pixmap
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID;
      Attr        : in out Xpm_Attributes) is

      Error : Integer;
      Attr_Private : Xpm_Attributes_Private := To_Attributes_Private (Attr);

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error := XpmWriteFileFromPixmap (Display, Filename_String'Address,
                                       Pix, Shape_Mask, Attr_Private'Address);
      Check_Error_Code (Error);
      Attr := To_Attributes (Attr_Private);
      -- prevent these arrays to be freed
      Attr_Private.Color_Symbols := Null_Address;
      Attr_Private.Color_Table   := Null_Address;
      Attr_Private.Extensions    := Null_Address;
      Xpm_Free_Attributes (Attr_Private);
   end Xpm_Write_File_From_Pixmap;


   procedure Xpm_Write_File_From_Pixmap
     (Display     : in     Display_Pointer;
      Filename    : in     String;
      Pix         : in     Pixmap_ID;
      Shape_Mask  : in     Pixmap_ID) is

      Error           : Integer;

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Error := XpmWriteFileFromPixmap (Display, Filename_String'Address,
                                       Pix, Shape_Mask, Null_Address);
      Check_Error_Code (Error);
   end Xpm_Write_File_From_Pixmap;


   -- -------------------------------------------------------------------------
   --
   --  Write  DATA -> FILE
   --

   procedure Xpm_Write_File_From_Data
     (Filename    : in String;
      Data        : in Pixmap_Data_Type) is

      function XpmWriteFileFromData
        (Filename : in System.Address;
         Data     : in String_List_Conversion.Chars_Ptr_List_Type)
         return Integer;
      pragma Import (C, XpmWriteFileFromData, "XpmWriteFileFromData");

      Error           : Integer;
      Tmp_Data        : String_List_Conversion.Chars_Ptr_List_Type;

      Filename_String : constant Interfaces.C.Char_Array
                      := Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Tmp_Data := String_List_Conversion.To_Chars_Ptr_List (Data, Append_Null => False);
      Error := XpmWriteFileFromData (Filename_String'Address,
                                     Tmp_Data);
      -- now free the temporarily needed Chars_Ptr_List_Type
      String_List_Conversion.Free (Tmp_Data,
                                   String_List_Conversion.Index_Type (String_List.Length (Data)));
      Check_Error_Code (Error);
   end Xpm_Write_File_From_Data;


end Xpm_Lib;
