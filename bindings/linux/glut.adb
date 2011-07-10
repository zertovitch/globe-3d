with Ada.Unchecked_Conversion;
with Ada.Command_Line;
with Ada.Finalization;

package body GLUT is

   -- finalization - free Argv strings
   --
   -- RK 23-Oct-2006, to remove the memory leak in question.
   --

   type Argvz is array (0 .. 500) of aliased interfaces.c.strings.Chars_Ptr;

   type Arg_Type is new ada.finalization.Controlled with record
     v       : Argvz:= (others => interfaces.c.strings.Null_Ptr);
     v_Count : Natural:= 0;
   end record;

   procedure Finalize (Self : in out Arg_Type)
   is
      use interfaces.c.strings;
   begin
      if Self.v(0) /= interfaces.c.strings.Null_Ptr then
        free (Self.v (0));
      end if;

      for I in 1 .. Self.v_Count loop
         free (Self.v (I));
      end loop;
   end Finalize;

   Arg : Arg_Type;

   procedure Glutinit (Argcp : access Integer;
      Argv : access Interfaces.C.Strings.Chars_Ptr);
   -- pragma Import (C, Glutinit, "glutInit", "glutInit"); -- APEX
   pragma Import (StdCall, GlutInit, "glutInit"); -- GNAT/OA

   -- Pure Ada method, from IBM / Rational Apex support:

   -- "This procedure may be a useful replacement when porting an
   --  Ada program written for Gnat, which imports argc and argv like this:
   --  argc : aliased integer;
   --  pragma Import (C, argc, "gnat_argc");
   --
   --  argv : chars_ptr_ptr;
   --  pragma Import (C, argv, "gnat_argv");
   -- "

   -- http://www-1.ibm.com/support/docview.wss?uid=swg21125019

   procedure Init is
      use Ada.Command_Line;
      use Interfaces.C.Strings;

      Argc : aliased Integer := Argument_Count + 1;
   begin
      Arg.v_Count := Argument_Count;

      Arg.v (0) := New_String (Command_Name);
      for I in 1 .. Arg.v_Count loop
          Arg.v (I) := New_String (Argument (I));
      end loop;

      Glutinit (Argc'Access, Arg.v (0)'Access);
   end Init;

   function CreateWindow (Title : String) return Integer is
      Result : Integer;
      C_Title : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Title);
   begin
      Result := CreateWindow (C_Title);
      Interfaces.C.Strings.Free (C_Title);
      return Result;
   end CreateWindow;

   procedure InitDisplayString (Name : String) is
      C_Name : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Name);
   begin
      InitDisplayString (C_Name);
      Interfaces.C.Strings.Free (C_Name);
   end InitDisplayString;

   procedure SetWindowTitle (Title : String) is
      C_Title : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Title);
   begin
      SetWindowTitle (C_Title);
      Interfaces.C.Strings.Free (C_Title);
   end SetWindowTitle;

   procedure SetIconTitle (Title : String) is
      C_Title : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Title);
   begin
      SetIconTitle (C_Title);
      Interfaces.C.Strings.Free (C_Title);
   end SetIconTitle;

   procedure AddMenuEntry (Label : String; Value : Integer) is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      AddMenuEntry (C_Label, Value);
      Interfaces.C.Strings.Free (C_Label);
   end AddMenuEntry;

   procedure AddSubMenu (Label : String; Submenu : Integer) is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      AddSubMenu (C_Label, Submenu);
      Interfaces.C.Strings.Free (C_Label);
   end AddSubMenu;

   procedure ChangeToMenuEntry
     (Item  : Integer;
      Label : String;
      Value : Integer)
   is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      ChangeToMenuEntry (Item, C_Label, Value);
      Interfaces.C.Strings.Free (C_Label);
   end ChangeToMenuEntry;

   procedure ChangeToSubMenu
     (Item    : Integer;
      Label   : String;
      Submenu : Integer)
   is
      C_Label : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Label);
   begin
      ChangeToSubMenu (Item, C_Label, Submenu);
      Interfaces.C.Strings.Free (C_Label);
   end ChangeToSubMenu;

   function ExtensionSupported (Name : String) return Integer is
      Result : Integer;
      C_Name : Interfaces.C.Strings.Chars_Ptr
        := Interfaces.C.Strings.New_String (Name);
   begin
      Result := ExtensionSupported (C_Name);
      Interfaces.C.Strings.Free (C_Name);
      return Result;
   end ExtensionSupported;

   -----------------------------------------------------
   -- GdM 2005: callbacks with the 'Address attribute --
   -----------------------------------------------------

  -- This method is functionally identical as GNAT's Unrestricted_Access
  -- but has no type safety (cf GNAT Docs)

   function CreateMenu (P1 : System.Address) return Integer is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_1);
   begin
     return CreateMenu( Cvt(P1) );
   end CreateMenu;

   procedure DisplayFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_2);
   begin
     DisplayFunc( Cvt(P1) );
   end DisplayFunc;

   procedure ReshapeFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_3);
   begin
     ReshapeFunc( Cvt(P1) );
   end ReshapeFunc;

   procedure KeyboardFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_4);
   begin
     KeyboardFunc( Cvt(P1) );
   end KeyboardFunc;

   procedure KeyboardUpFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_KeyUpFunc);
   begin
     KeyboardUpFunc( Cvt(P1) );
   end KeyboardUpFunc;

   procedure MouseFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_5);
   begin
     MouseFunc( Cvt(P1) );
   end MouseFunc;

   procedure MotionFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_6);
   begin
     MotionFunc( Cvt(P1) );
   end MotionFunc;

   procedure PassiveMotionFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_7);
   begin
     PassiveMotionFunc( Cvt(P1) );
   end PassiveMotionFunc;

   procedure IdleFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_10);
   begin
     IdleFunc( Cvt(P1) );
   end IdleFunc;

   procedure SpecialFunc (P1 : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_Proc_13);
   begin
     SpecialFunc( Cvt(P1) );
   end SpecialFunc;

   procedure SpecialUpFunc (Func : System.Address) is
     function Cvt is new Ada.Unchecked_Conversion(System.Address,Glut_SpecialUp);
   begin
     SpecialUpFunc( Cvt(Func) );
   end SpecialUpFunc;

end GLUT;
