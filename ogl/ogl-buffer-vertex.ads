
with ogl.Buffer.general;


package ogl.Buffer.vertex is new ogl.Buffer.general (base_object   => gl.Buffer.array_Object,
                                                   index         => gl.geometry.positive_vertex_Id,
                                                   element       => gl.geometry.Vertex,
                                                   element_array => gl.geometry.vertex_Array);
