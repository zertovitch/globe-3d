with GL.Buffer.General,
     GL.Geometry;

package GL.Buffer.Normals is
   new GL.Buffer.General (base_object        => GL.Buffer.array_Object,
                          index              => GL.Geometry.positive_Vertex_Id,
                          element            => GL.Geometry.Normal,
                          element_array      => GL.Geometry.Normal_array,
                          default_Terminator => (0.0, 0.0, 0.0));
