with GLOBE_3D, GLOBE_3D.BSP;

package delta4g1 is

procedure Create(
group   : in out GLOBE_3D.p_Object_3D_array;
BSP_tree: in out GLOBE_3D.BSP.p_BSP_node;
centre  : in     GLOBE_3D.Point_3D
);

end delta4g1;
