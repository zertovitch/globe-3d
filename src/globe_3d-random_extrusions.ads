------------------------------------------------------------------------------
--  File:            GLOBE_3D-Random_extrusions.ads
--  Description:     Algorithm to generate a Sci-Fi-style extruded surface
--  Date / Version:  14-May-2006
--                   Copyright (c) Gautier de Montmollin 2006
------------------------------------------------------------------------------

generic

  with procedure Geometric_mapping(u: in Point_3D; x: out Point_3D);

  -- (u(1),u(2)) in [0;1] x [0;1]
  --
  -- Edge numbering:
  -- (0,1) 4--<--3 (1,1)
  --       |     |
  -- (0,0) 1-->--2 (1,0)
  --
  -- u(3): elevation above surface

package GLOBE_3D.Random_extrusions is

  procedure Extrude_on_rectangle(
    T1,T2,T3,T4     :  in Map_idx_pair;  -- Texture edges, horizontal surface
    V1,V2,V3,V4     :  in Map_idx_pair;  -- Texture edges, vertical surfaces
    grid_1,grid_2   :  in Positive;
    T_ID, V_ID      :  in Image_ID;      -- ID's of plane and vertical texture
    max_u3          :  in Real;
    iterations      :  in Natural;
    last_point      : out Natural;
    mesh            : out Point_3D_array;
    last_face       : out Natural;
    poly            : out Face_array;
    random_initiator:  in Integer:= 0    -- default 0 -> time-dependent seed
  );

end GLOBE_3D.Random_extrusions;
