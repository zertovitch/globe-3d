with GL.Buffer.General, GL.Geometry; --, GL.Textures;

package GL.Buffer.Vertex is

   new GL.Buffer.General (base_object   => GL.Buffer.array_Object,
                          index         => GL.Geometry.positive_vertex_Id,
                          element       => GL.Geometry.Vertex,
                          element_array => GL.Geometry.vertex_Array);
