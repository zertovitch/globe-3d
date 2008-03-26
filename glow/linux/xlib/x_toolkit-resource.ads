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

with Generic_List_Types;
package X_Toolkit.Resource is

   type Resource_Rec is record
      Resource_Name   : Xt_N_Resource_String;
      Resource_Class  : Xt_C_Resource_String;
      Resource_Type   : Xt_R_Resource_String;
      Resource_Size   : Cardinal;
      Resource_Offset : Cardinal;
      Default_Type    : Xt_R_Resource_String;
      Default_Address : Xt_Pointer;
   end record;

   type Resource_List is private;

   Null_Resource_List : constant Resource_List;

   function Length (List : in Resource_List) return Natural;

   function Element
     (List   : in Resource_List;
      Index  : in Natural)
      return Resource_Rec;



   procedure Append
     (List            : in out Resource_List;
      Resource_Name   : in     Xt_N_Resource_String;
      Resource_Class  : in     Xt_C_Resource_String;
      Resource_Type   : in     Xt_R_Resource_String;
      Resource_Size   : in     Cardinal;              -- use R.C'Size
      Resource_Offset : in     Cardinal;              -- use R.C'Position
      Default_Type    : in     Xt_R_Resource_String;
      Default_Address : in     Xt_Pointer);

   procedure Append
     (List            : in out Resource_List;
      Rec             : in     Resource_Rec);

   function "&"
     (Left            : in Resource_List;
      Right           : in Resource_Rec) return Resource_List;


   procedure Xt_Get_Application_Resources
     (Object    : in Widget;
      Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List := Null_Arg_List);


   procedure Xt_Get_Constraint_Resource_List
     (Class     : in     Widget_Class;
      Resources :    out Resource_List);


   procedure Xt_Get_Resource_List
     (Class     : in     Widget_Class;
      Resources :    out Resource_List);


   procedure Xt_Get_Subresources
     (Object       : in Widget;
      Base         : in Xt_Pointer;
      Subpart_Name : in Xt_N_Resource_String;
      Class        : in Xt_C_Resource_String;
      Resources    : in Resource_List;
      Args         : in Arg_List := Null_Arg_List);


   procedure Xt_Get_Subvalues
     (Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List);


   procedure Xt_Set_Subvalues
     (Base      : in Xt_Pointer;
      Resources : in Resource_List;
      Args      : in Arg_List);


   
private

   package Resource_Lists is new Generic_List_Types (Resource_Rec);
   type Resource_List is new Resource_Lists.Unbounded_List;

   Null_Resource_List : constant Resource_List := Resource_List (Resource_Lists.Null_Unbounded_List);

end X_Toolkit.Resource;
