--  This was moved from the main GLOBE_3D package (23-June-2016).
--  Reason: the codes depends on GL extensions (VBOs), which is cumbersome on some platforms.
--  Example: the version of gprbuild, GNAT GPL 2016 in Windows fails at linking stage
--  but the deprecated gnatmake succeeds. Plus, all the mess with GLEE, GLEW, etc.

package GLOBE_3D.Visuals_Rendering is

   --  clears the color buffer and renders each of the visuals.

   procedure Render (the_Visuals : in Visual_Array; the_Camera : in Camera'Class);

end GLOBE_3D.Visuals_Rendering;
