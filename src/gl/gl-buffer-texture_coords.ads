with GL.Buffer.General, GL.Geometry, GL.Textures;

package GL.Buffer.Texture_coords is

   new GL.Buffer.General (base_object   => GL.Buffer.array_Object,
                          index         => GL.Geometry.positive_vertex_Id,
                          element       => GL.Textures.Coordinate_2D,
                          element_array => GL.Textures.Coordinate_2D_array);
