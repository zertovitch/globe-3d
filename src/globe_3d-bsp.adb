with Ada.Unchecked_Deallocation;

with GL.Math, GLOBE_3D.Options;

package body GLOBE_3D.BSP is

  procedure Locate (P : Point_3D; tree : p_BSP_Node; area : out p_Object_3D) is

    --  Internal, for skipping useless parameter passing
    --
    procedure Locate_Point (node : p_BSP_Node) is
      use GL.Math;
    begin
      --  Info / debug: keep track of the path through the BSP tree.
      if Options.BSP_tracking then
        info_b_str1 := info_b_str1 & " ->" & Integer'Image (node.node_id);
        info_b_ntl1 := info_b_ntl1 + 1;
      end if;
      if P * node.normal + node.distance > 0.0 then  --   <--- Here is the location test.
        --  P is in front
        if node.front_child = null then
          area := node.front_leaf;
        else
          Locate_Point (node.front_child);
        end if;
      else
        --  P is in back
        if node.back_child = null then
          area := node.back_leaf;
        else
          Locate_Point (node.back_child);
        end if;
      end if;
    end Locate_Point;

  begin
    if Options.BSP_tracking then
      info_b_str1 := Null_Unbounded_String;
      info_b_ntl1 := 0;  --  depth counter
    end if;
    area := null;
    if tree /= null then
      Locate_Point (tree);
    end if;
    if Options.BSP_tracking then
      info_b_bool1 := area /= null;
    end if;
  end Locate;

  procedure Delete (tree : in out p_BSP_Node) is
    procedure Dispose is new Ada.Unchecked_Deallocation (BSP_Node, p_BSP_Node);
  begin
    if tree /= null then
      Delete (tree.front_child);
      Delete (tree.back_child);
      Dispose (tree);
      tree := null;
    end if;
  end Delete;

end GLOBE_3D.BSP;
