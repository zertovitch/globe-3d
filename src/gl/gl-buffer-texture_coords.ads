
with GL.Buffer.general;

package GL.Buffer.texture_coords is new gl.Buffer.general (base_object   => gl.Buffer.array_Object,
                                                           index         => gl.geometry.positive_vertex_Id,
                                                           element       => gl.textures.Coordinate_2D,
                                                           element_array => gl.textures.Coordinate_2D_array);
