
package body glow.Culler is

   procedure Viewer_is (Self : in out Culler'Class;   Now : p_Window)
   is
   begin
      self.Viewer := Now.all'access;
   end;



   function Viewer (Self : in     Culler'Class) return p_Window
   is
   begin
      return self.Viewer;
   end;

end glow.Culler;
