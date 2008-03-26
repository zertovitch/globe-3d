-------------------------------------------------------------------------
--  GL.Errors - error support sub-programs.
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------


package gl.Errors is

   openGL_Error : exception;



   function Current return String;
   --
   -- returns a descriptive string of the last occuring openGL error.
   -- returns "", when no error exists.
   -- clears any existing error.


   procedure log (Prefix : in String := "");
   --
   -- displays 'Current' error via ada.Text_IO.put_Line.
   -- clears any existing error.
   -- raises 'openGL_Error' when an opengl error has been detected.

   procedure log (Prefix : in String := "";   error_Occurred : out Boolean);
   --
   -- displays 'Current' via ada.Text_IO.put_Line.
   -- clears any existing error.
   -- sets error_Occurred to true, if a GL error was detected.

end gl.Errors;
