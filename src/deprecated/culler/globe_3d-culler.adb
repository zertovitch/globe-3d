
package body GLOBE_3D.Culler is

   procedure Viewer_is (Self : in out Culler'Class;   Now : GLUT.Windows.p_Window)
   is
   begin
      Self.Viewer := Now.all'Access;
   end Viewer_is;

   function Viewer (Self : in     Culler'Class) return GLUT.Windows.p_Window
   is
   begin
      return Self.Viewer;
   end Viewer;

end GLOBE_3D.Culler;
