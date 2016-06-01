-------------------------------------------------------------------------
--  GLOBE_3D.Culler
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package GLOBE_3D.Culler is

   type Culler   is abstract tagged limited private;
   type p_Culler is access all Culler'Class;

   procedure add (Self : in out Culler;   the_Visual : in GLOBE_3D.p_Visual) is abstract;
   procedure rid (Self : in out Culler;   the_Visual : in GLOBE_3D.p_Visual) is abstract;

   function  object_Count (Self : in Culler) return Natural is abstract;

   no_such_Object : exception;   -- raised when trying to 'rid' an object which has not been added to the Window.

   procedure evolve       (Self : in out Culler;   By : in     Real) is abstract;    -- tbd: rename 'freshen' ? ... use Duration for 'By' ?

   procedure Viewer_is (Self : in out Culler'Class;   Now : in GLOBE_3D.p_Window);
   function  Viewer    (Self : in     Culler'Class) return GLOBE_3D.p_Window;

private

   type Culler is abstract tagged limited
      record
         Viewer : GLOBE_3D.p_Window;
      end record;

end GLOBE_3D.Culler;
