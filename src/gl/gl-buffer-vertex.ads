
with gl.Buffer.general;


package gl.Buffer.vertex is new gl.Buffer.general (base_object   => gl.Buffer.array_Object,
                                                   index         => gl.geometry.positive_vertex_Id,
                                                   element       => gl.geometry.Vertex,
                                                   element_array => gl.geometry.vertex_Array);
