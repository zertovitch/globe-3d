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
with interfaces.C.Strings; use interfaces.C.Strings;
with ada.unchecked_Conversion;
with ada.Text_IO;



package body ogl.Errors is


   function Current return String
   is
      function to_chars_ptr is new ada.unchecked_Conversion (gl.uBytePtr, chars_ptr);
   begin
      return Value (to_chars_ptr (glu.errorString (gl.getError)));
   end;





   procedure log (Prefix : in String := "")
   is
      current_Error : String renames Current;
   begin
      if current_Error = "no error" then
         return;
      end if;

      if Prefix = "" then
         ada.text_io.put_Line ("openGL error: '" & current_Error & "'");
      else
         ada.text_io.put_Line (Prefix & ": '" & current_Error & "'");
      end if;

      raise openGL_Error;  -- tbd: use ada.exceptions to attach the openg error string to the exception.
   end;





   procedure log (Prefix : in String := "";   error_Occurred : out Boolean)
   is
      current_Error : String renames Current;
   begin
      if current_Error = "no error" then
         error_Occurred := False;
         return;
      end if;

      error_Occurred := True;

      if Prefix = "" then
         ada.text_io.put_Line ("openGL error: '" & current_Error & "'");
      else
         ada.text_io.put_Line (Prefix & ": '" & current_Error & "'");
      end if;
   end;




end ogl.Errors;
