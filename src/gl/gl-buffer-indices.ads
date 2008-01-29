
with gl.Buffer.general;


package gl.Buffer.indices is new gl.Buffer.general (base_object   => gl.Buffer.element_array_Object,
                                                    index         => gl.positive_uInt,
                                                    element       => gl.geometry.vertex_Id,
                                                    element_array => gl.geometry.vertex_Id_Array);
