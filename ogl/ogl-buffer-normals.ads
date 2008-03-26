
with ogl.Buffer.general;


package ogl.Buffer.normals is new ogl.Buffer.general (base_object   => ogl.Buffer.array_Object,
                                                    index         => ogl.geometry.positive_vertex_Id,
                                                    element       => ogl.geometry.Normal,
                                                    element_array => ogl.geometry.normal_Array);
