-------------------------------------------------------------------------
--  GLOBE_3D.Collision_detection
--
--  Copyright (c) Gautier de Montmollin 1999..2008
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

-------------------------------------------------------------------------
--  Change log
--
--  21-May-2008: GM: slide mode working, by adding dist_before > 0.0
--  14-May-2008: GM: package created (re-used most of old Engine_3D)

package GLOBE_3D.Collision_Detection is

  --  Reaction to an object - and the world connected to it

  type Reaction_Method is (elastic, slide);

  type Ball_Type is record
    centre : Point_3D;
    radius : Real;
  end record;

  --  Collision between a ball, intending to do a step (vector), against
  --  an object o. If there is a collision, step is reduced in length and
  --  may change direction.
  --  Typically, the ball's centre is the camera position and step is the camera's
  --  move between two images. But the ball can be the abstraction of any moving
  --  object, actor,...

  procedure Reaction
    (o           : Object_3D'Class;
     ball        : Ball_Type;
     method      : Reaction_Method;
     step        : in out Vector_3D;  --  Whole step (in: desired, out: effective)
     reacted     :    out Real);      --  reaction in proportion to step; in [0,1]

  Unsupported : exception;  --  something not yet implemented

  Normal_length_is_not_one : exception;
  --  only occur when body's check_normals = True,
  --  and also when normals are wrong, of course...

end GLOBE_3D.Collision_Detection;
