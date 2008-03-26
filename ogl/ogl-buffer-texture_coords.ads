
with ogl.Buffer.general;


package ogl.Buffer.texture_coords is new ogl.Buffer.general (base_object   => ogl.Buffer.array_Object,
                                                           index         => ogl.geometry.positive_vertex_Id,
                                                           element       => ogl.textures.Coordinate_2D,
                                                           element_array => ogl.textures.Coordinate_2D_array);
