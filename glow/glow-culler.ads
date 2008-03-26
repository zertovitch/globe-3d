
package glow.Culler is


   task type Item (Window : access glow.Window.item'Class) is

      entry start;

   end Item;


   type View is access all Item'Class;


   procedure add (Self : in out Culler;   the_Visual : in ogl.Visual.view) is abstract;
   procedure rid (Self : in out Culler;   the_Visual : in ogl.Visual.view) is abstract;

   function  object_Count (Self : in Culler) return Natural is abstract;

   no_such_Object : exception;   -- raised when trying to 'rid' an object which has not been added to the Window.


   procedure evolve       (Self : in out Culler;   By : in     Real) is abstract;    -- tbd: rename 'freshen' ? ... use Duration for 'By' ?


   procedure Viewer_is (Self : in out Culler'Class;   Now : in globe_3d.p_Window);
   function  Viewer    (Self : in     Culler'Class) return globe_3d.p_Window;




private

   type Item (Window : access glow.Window.item'Class) is abstract tagged limited
      record
         Viewer : globe_3d.p_Window;
      end record;

end glow.Culler;
