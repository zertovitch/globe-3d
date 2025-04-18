--  * Output of max2ada.ms, a GMax / 3D Studio Max script for exporting to GLOBE_3D
--
--  * Copy and paste these lines from the Listener into a
--    text editor, and save the package as an .ada file.
--  * Alternatively, use the GMaxSLGRAB.exe tool.
--  * For GNAT, you must save the specification as an .ads file
--    and the body as an .adb file, or run gnatchop on the whole .ada file.

--  Title: A319
--
--  Manually reworked (a bit, around materials)
--  Model author: Claus Vendelboe Holmberg
--  http://www.fsdeveloper.com/forum/downloads.php?do=file&id=80
--

--  File name: Airbus319FSX.gmax
--  File path: R:\Animated_Airbus319_FSX\

with GLOBE_3D;

package A319 is

  procedure Create
    (object: in out GLOBE_3D.p_Object_3D;
     scale :        GLOBE_3D.Real;
     centre:        GLOBE_3D.Point_3D);
end A319;
