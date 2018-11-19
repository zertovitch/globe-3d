with GL.Buffer.General,
     GL.Geometry;

package GL.Buffer.Vertex is
   new GL.Buffer.General (base_object        => GL.Buffer.array_Object,
                          index              => GL.Geometry.positive_Vertex_Id,
                          element            => GL.Geometry.Vertex,
                          element_array      => GL.Geometry.Vertex_array,
                          default_Terminator => GL.Geometry.null_Vertex);
