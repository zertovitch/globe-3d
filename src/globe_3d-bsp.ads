----------------------------------
-- Binary Space Partition (BSP) --
----------------------------------
--
-- Changes:
--
-- 31-Mar-2008: comment: node_id also meant for use with I/O
-- 14-Oct-2006: created


package GLOBE_3D.BSP is

  type BSP_node;
  type p_BSP_node is access BSP_node;

  type BSP_node is record
    front_child, back_child: p_BSP_node:= null;
    front_leaf, back_leaf: p_Object_3D:= null;
    -- outer normal to node's plane :
    normal: Vector_3D;
    -- signed distance between origin O and plane; positive if O in front :
    distance: Real;
    -- informative (debug) or as temporary for I/O :
    node_id: Natural:= 0;
  end record;

  procedure Locate( P: Point_3D; tree: p_BSP_node; area: out p_Object_3D );
  -- if P is in no area known to the BSP tree, area:= null

  procedure Delete( tree: in out p_BSP_node );

end GLOBE_3D.BSP;