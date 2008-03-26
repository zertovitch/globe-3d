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

with Unchecked_Conversion,
     Interfaces.C.Pointers,
     X_Toolkit.Internal;
package body X_Toolkit.Resource is

   use type Interfaces.C.unsigned;

   function To_Cardinal is new Unchecked_Conversion (System.Address, Cardinal);

   function To_C_Size (Ada_Size : in Cardinal) return Cardinal is
   begin
      return (Ada_Size + Character'Size - 1) / Character'Size;
   end To_C_Size;
   pragma Inline (To_C_Size);

   function To_Ada_Size (C_Size : in Cardinal) return Cardinal is
   begin
      return C_Size * Character'Size;
   end To_Ada_Size;
   pragma Inline (To_Ada_Size);



   function Length (List : in Resource_List) return Natural is
   begin
      return Resource_Lists.Length (Resource_Lists.Unbounded_List (List));
   end Length;
   pragma Inline (Length);


   function Element
     (List   : in Resource_List;
      Index  : in Natural)
      return Resource_Rec is
      Ret_Res : Resource_Rec;
   begin
      Ret_Res := Resource_Lists.Element (Resource_Lists.Unbounded_List (List), Index);
      Ret_Res.Resource_Size := To_Ada_Size (Ret_Res.Resource_Size);
      return Ret_Res;
   end Element;
   pragma Inline (Element);



   procedure Append
     (List            : in out Resource_List;
      Resource_Name   : in     Xt_N_Resource_String;
      Resource_Class  : in     Xt_C_Resource_String;
      Resource_Type   : in     Xt_R_Resource_String;
      Resource_Size   : in     Cardinal;              -- use R.C'Size
      Resource_Offset : in     Cardinal;              -- use R.C'Position
      Default_Type    : in     Xt_R_Resource_String;
      Default_Address : in     Xt_Pointer) is
      -- change Resource_Size value (Ada-Style: number of bits, C-Style:
      -- number of bytes)
      Res_Rec : Resource_Rec := (Resource_Name, Resource_Class, Resource_Type,
                                 To_C_Size (Resource_Size), Resource_Offset,
                                 Default_Type, Default_Address);
   begin
      Resource_Lists.Append (Resource_Lists.Unbounded_List (List), Res_Rec);
   end Append;
   pragma Inline (Append);


   procedure Append
     (List            : in out Resource_List;
      Rec             : in     Resource_Rec) is
      Res_Rec : Resource_Rec := (Rec.Resource_Name, Rec.Resource_Class, Rec.Resource_Type,
                                 To_C_Size (Rec.Resource_Size),
				 Rec.Resource_Offset,
                                 Rec.Default_Type, Rec.Default_Address);
   begin
      Resource_Lists.Append (Resource_Lists.Unbounded_List (List), Res_Rec);
   end Append;
   pragma Inline (Append);


   function "&"
     (Left            : in Resource_List;
      Right           : in Resource_Rec) return Resource_List is
      Right_Rec : Resource_Rec := (Right.Resource_Name, Right.Resource_Class, Right.Resource_Type,
                                   To_C_Size (Right.Resource_Size),
				   Right.Resource_Offset,
                                   Right.Default_Type, Right.Default_Address);
   begin
      return Resource_List (Resource_Lists."&" (Resource_Lists.Unbounded_List (Left), Right_Rec));
   end "&";
   pragma Inline ("&");




   procedure Xt_Get_Application_Resources
     (Object    : in Widget;
      Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List := Null_Arg_List) is
      procedure XtGetApplicationResources
        (Object        : in Widget;
         Base          : in Xt_Pointer;
         Resources     : in Resource_Lists.Element_Access;
         Num_Resources : in Cardinal;
         Args          : in X_Toolkit.Internal.Arg_Rec_Access;
         Num_Args      : in Cardinal);
      pragma Import (C, XtGetApplicationResources, "XtGetApplicationResources");
   begin
      XtGetApplicationResources (Object, Base,
               Resource_Lists.Hook (Resource_Lists.Unbounded_List (Resources)),
               Cardinal (Resource_Lists.Length (Resource_Lists.Unbounded_List (Resources))),
               X_Toolkit.Internal.Hook (Args),
               Cardinal (Length (Args)));
   end Xt_Get_Application_Resources;


   Null_Resource_Rec : Resource_Rec; -- just to define something

   package Resource_Pointers is new Interfaces.C.Pointers (Natural,
                                                           Resource_Rec,
                                                           Resource_Lists.Element_Array,
                                                           Null_Resource_Rec);

   procedure XtFree (Ptr : in Resource_Pointers.Pointer);
   pragma Import (C, XtFree, "XtFree");


   procedure Xt_Get_Constraint_Resource_List
     (Class     : in     Widget_Class;
      Resources :    out Resource_List) is
      procedure XtGetConstraintResourceList
        (Class                : in     Widget_Class;
         Resources_Return     :    out Resource_Pointers.Pointer;
         Num_Resources_Return :    out Cardinal);
      pragma Import (C, XtGetConstraintResourceList, "XtGetConstraintResourceList");

      Hook : Resource_Pointers.Pointer;
      Num  : Cardinal;
   begin
      XtGetConstraintResourceList (Class, Hook, Num);
      if Num > 0 then
         declare
            Arry : constant Resource_Lists.Element_Array (1 .. Natural (Num)) :=
                            Resource_Pointers.Value (Hook, Interfaces.C.Ptrdiff_T (Num));
         begin
            Resources := Resource_List (Resource_Lists.To_Unbounded_List (Arry));
            XtFree (Hook);
         end;
      else
         Resources := Null_Resource_List;
      end if;
   end Xt_Get_Constraint_Resource_List;


   procedure Xt_Get_Resource_List
     (Class     : in     Widget_Class;
      Resources :    out Resource_List) is
      procedure XtGetResourceList
        (Class                : in     Widget_Class;
         Resources_Return     :    out Resource_Pointers.Pointer;
         Num_Resources_Return :    out Cardinal);
      pragma Import (C, XtGetResourceList, "XtGetResourceList");

      Hook : Resource_Pointers.Pointer;
      Num  : Cardinal;
   begin
      XtGetResourceList (Class, Hook, Num);
      if Num > 0 then
         declare
            Arry : constant Resource_Lists.Element_Array (1 .. Natural (Num)) :=
                            Resource_Pointers.Value (Hook, Interfaces.C.Ptrdiff_T (Num));
         begin
            Resources := Resource_List (Resource_Lists.To_Unbounded_List (Arry));
            XtFree (Hook);
         end;
      else
         Resources := Null_Resource_List;
      end if;
   end Xt_Get_Resource_List;





   procedure Xt_Get_Subresources
     (Object       : in Widget;
      Base         : in Xt_Pointer;
      Subpart_Name : in Xt_N_Resource_String;
      Class        : in Xt_C_Resource_String;
      Resources    : in Resource_List;
      Args         : in Arg_List := Null_Arg_List) is
      procedure XtGetSubresources
        (Object        : in Widget;
         Base          : in Xt_Pointer;
         Subpart_Name  : in Xt_N_Resource_String;
         Class         : in Xt_C_Resource_String;
         Resources     : in Resource_Lists.Element_Access;
         Num_Resources : in Cardinal;
         Args          : in X_Toolkit.Internal.Arg_Rec_Access;
         Num_Args      : in Cardinal);
      pragma Import (C, XtGetSubresources, "XtGetSubresources");
   begin
      XtGetSubresources (Object, Base,
               Subpart_Name, Class,
               Resource_Lists.Hook (Resource_Lists.Unbounded_List (Resources)),
               Cardinal (Resource_Lists.Length (Resource_Lists.Unbounded_List (Resources))),
               X_Toolkit.Internal.Hook (Args),
               Cardinal (Length (Args)));
   end Xt_Get_Subresources;



   procedure Xt_Get_Subvalues
     (Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List) is
      procedure XtGetSubvalues
        (Base          : in Xt_Pointer;
         Resources     : in Resource_Lists.Element_Access;
         Num_Resources : in Cardinal;
         Args          : in X_Toolkit.Internal.Arg_Rec_Access;
         Num_Args      : in Cardinal);
      pragma Import (C, XtGetSubvalues, "XtGetSubvalues");
   begin
      if Length (Args) > 0 then
         XtGetSubvalues (Base,
               Resource_Lists.Hook (Resource_Lists.Unbounded_List (Resources)),
               Cardinal (Resource_Lists.Length (Resource_Lists.Unbounded_List (Resources))),
                      X_Toolkit.Internal.Hook (Args),
                      Cardinal (Length (Args)));
      end if;
   end Xt_Get_Subvalues;


   procedure Xt_Set_Subvalues
     (Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List) is
      procedure XtSetSubvalues
        (Base          : in Xt_Pointer;
         Resources     : in Resource_Lists.Element_Access;
         Num_Resources : in Cardinal;
         Args          : in X_Toolkit.Internal.Arg_Rec_Access;
         Num_Args      : in Cardinal);
      pragma Import (C, XtSetSubvalues, "XtSetSubvalues");
   begin
      if Length (Args) > 0 then
         XtSetSubvalues (Base,
               Resource_Lists.Hook (Resource_Lists.Unbounded_List (Resources)),
               Cardinal (Resource_Lists.Length (Resource_Lists.Unbounded_List (Resources))),
                      X_Toolkit.Internal.Hook (Args),
                      Cardinal (Length (Args)));
      end if;
   end Xt_Set_Subvalues;



end X_Toolkit.Resource;
