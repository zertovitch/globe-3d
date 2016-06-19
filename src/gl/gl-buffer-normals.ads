with GL.Buffer.General,
     GL.Geometry;

package GL.Buffer.Normals is
   new GL.Buffer.General (base_object   => GL.Buffer.array_Object,
                          index         => GL.Geometry.positive_vertex_Id,
                          element       => GL.Geometry.Normal,
                          element_array => GL.Geometry.normal_Array);
