-------------------------------------------------------------------------
--  GL.Errors - GL error support
--
--  Copyright (c) Rod Kay 20016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GLU,
     Interfaces.C.Strings,
     Ada.Unchecked_Conversion,
     Ada.Text_IO;

use Interfaces.C.Strings;

package body GL.Errors is

   function Current return String
   is
      Error : constant ErrorEnm := GL.GetError;
   begin
      if Error = GL.NO_ERROR then
         return "";
      end if;

      declare
         function to_chars_ptr is new Ada.Unchecked_Conversion (GL.ubytePtr, chars_ptr);
      begin
         return Value (to_chars_ptr (GLU.ErrorString (GL.GetError)));
      end;
   end;

   procedure Log (Prefix : in String := "")
   is
      use Ada.Text_IO;
      current_Error : constant String:= Current;
   begin
      if current_Error = "" then
         return;
      end if;

      if Prefix = "" then
         Put_Line ("openGL error: '" & current_Error & "'");
      else
         Put_Line (Prefix & ": '" & current_Error & "'");
      end if;

      raise openGL_Error with current_Error;
   end;

   procedure Log (Prefix : in String := "";   error_Occurred : out Boolean)
   is
      use Ada.Text_IO;
      current_Error : constant String:= Current;
   begin
      if current_Error = "" then
         error_Occurred := False;
         return;
      end if;

      error_Occurred := True;

      if Prefix = "" then
         Put_Line ("openGL error: '" & current_Error & "'");
      else
         Put_Line (Prefix & ": '" & current_Error & "'");
      end if;
   end;

end GL.Errors;
