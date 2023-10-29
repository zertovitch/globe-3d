--  *** This package was generated by max2ada.ms, a GMax / 3D Studio Max
--        script for exporting models to GLOBE_3D.
--
--  * Copy and paste these lines from the Listener (F11) into a
--      text editor, and save the package as an .ada file.
--  * Alternatively, you can use the GMaxSLGRAB.exe tool.
--  * For GNAT, you must save the specification as an .ads file
--      and the body as an .adb file, or run gnatchop on the whole .ada file.

--  Author: Xenodroid
--  Title: Dreadnought
--  Subject: Dreadnought class Heavy Cruiser: a space cruiser inspired by Sulaco from Aliens.
--  Comments: Reworked a bit...

--  File name: dreadnought_g3d.gmax
--  File path: C:\Ada\g3d\tools\gmax\

with GLOBE_3D;

package Dreadnought is

  procedure Create
    (object : in out GLOBE_3D.p_Object_3D;
     scale  :        GLOBE_3D.Real;
     centre :        GLOBE_3D.Point_3D;
     alum_001,
     grumnoir,
     tole_001,
     alum_002 : GLOBE_3D.Image_ID);

end Dreadnought;
