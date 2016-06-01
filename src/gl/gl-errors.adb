-------------------------------------------------------------------------
--  GL.Errors - GL error support
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GLU;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

package body GL.Errors is

   function Current return String
   is
      function to_chars_ptr is new Ada.Unchecked_Conversion (GL.ubytePtr, chars_ptr);
   begin
      return Value (to_chars_ptr (GLU.ErrorString (GL.GetError)));
   end;

   procedure Log (Prefix : in String := "")
   is
      current_Error : String renames Current;
   begin
      if current_Error = "no error" then
         return;
      end if;

      if Prefix = "" then
         Ada.Text_IO.Put_Line ("openGL error: '" & current_Error & "'");
      else
         Ada.Text_IO.Put_Line (Prefix & ": '" & current_Error & "'");
      end if;

      raise openGL_Error with current_Error;
   end;

   procedure Log (Prefix : in String := "";   error_Occurred : out Boolean)
   is
      current_Error : String renames Current;
   begin
      if current_Error = "no error" then
         error_Occurred := False;
         return;
      end if;

      error_Occurred := True;

      if Prefix = "" then
         Ada.Text_IO.Put_Line ("openGL error: '" & current_Error & "'");
      else
         Ada.Text_IO.Put_Line (Prefix & ": '" & current_Error & "'");
      end if;
   end;

end GL.Errors;
