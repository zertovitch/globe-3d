------------------------------------
--  Binary Space Partition (BSP)  --
------------------------------------
--
--  Changes:
--
--  31-Mar-2008: comment: node_id field also meant for use with I/O
--  14-Oct-2006: created

package GLOBE_3D.BSP is

  type BSP_Node;
  type p_BSP_Node is access BSP_Node;

  type BSP_Node is record
    front_child, back_child : p_BSP_Node := null;
    front_leaf, back_leaf : p_Object_3D := null;
    --  Outer normal to node's plane :
    normal : Vector_3D;
    --  Signed distance between origin O and plane; positive if O in front :
    distance : Real;
    --  Informative (debug) or as temporary for I/O :
    node_id : Natural := 0;
  end record;

  procedure Locate (P : Point_3D; tree : p_BSP_Node; area : out p_Object_3D);
  --  If P is in no area known to the BSP tree, area:= null

  procedure Delete (tree : in out p_BSP_Node);

end GLOBE_3D.BSP;
