
with GL.Buffer.general;

package GL.Buffer.normals is new gl.Buffer.general (base_object   => gl.Buffer.array_Object,
                                                    index         => gl.geometry.positive_vertex_Id,
                                                    element       => gl.geometry.Normal,
                                                    element_array => gl.geometry.normal_Array);
