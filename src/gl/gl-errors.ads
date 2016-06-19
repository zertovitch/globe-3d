-------------------------------------------------------------------------
--  GL.Errors - error support sub-programs.
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package GL.Errors is

   openGL_Error : exception;

   function Current return String;
   --
   -- Returns a descriptive string of the last occuring openGL error.
   -- Returns "", when no error exists.
   -- Clears any existing error.

   procedure Log (Prefix : in String := "");
   --
   -- Displays 'Current' error via Ada.Text_IO.Put_Line.
   -- Clears any existing error.
   -- Raises 'openGL_Error' when an opengl error has been detected.

   procedure Log (Prefix : in String := "";   error_Occurred : out Boolean);
   --
   -- Displays 'Current' via Ada.Text_IO.Put_Line.
   -- Clears any existing error.
   -- Sets error_Occurred to true, if a GL error was detected.

end GL.Errors;
