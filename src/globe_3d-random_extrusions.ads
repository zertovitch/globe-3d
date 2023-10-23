------------------------------------------------------------------------------
--  File:            GLOBE_3D-Random_extrusions.ads
--  Description:     Algorithm to generate a Sci-Fi-style extruded surface
--  Date / Version:  14-May-2006
--                   Copyright (c) Gautier de Montmollin 2006
------------------------------------------------------------------------------

generic

  with procedure Geometric_Mapping (u : in Point_3D; x : out Point_3D);

  --  (u(1),u(2)) in [0;1] x [0;1]
  --
  --  Edge numbering:
  --  (0,1) 4--<--3 (1,1)
  --        |     |
  --  (0,0) 1-->--2 (1,0)
  --
  --  u(3): elevation above surface

package GLOBE_3D.Random_Extrusions is

  procedure Extrude_on_Rectangle
    (HT1, HT2, HT3, HT4 : in     Map_Idx_Pair;   --  Texture edges, horizontal surface
     VT1, VT2, VT3, VT4 : in     Map_Idx_Pair;   --  Texture edges, vertical surfaces
     grid_1, grid_2     : in     Positive;
     T_ID, V_ID         : in     Image_ID;       --  ID's of plane and vertical texture
     max_u3             : in     Real;
     iterations         : in     Natural;
     last_point         :    out Natural;
     mesh               :    out Point_3D_Array;
     last_face          :    out Natural;
     poly               :    out Face_Array;
     random_initiator   : in     Integer := 0);  --  default 0 -> time-dependent seed

end GLOBE_3D.Random_Extrusions;
