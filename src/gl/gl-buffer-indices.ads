with GL.Buffer.General, GL.Geometry;

package GL.Buffer.Indices is
   new GL.Buffer.general (base_object        => GL.Buffer.element_array_Object,
                          index              => GL.positive_uInt,
                          element            => GL.Geometry.vertex_Id,
                          element_array      => GL.Geometry.vertex_Id_array,
                          default_Terminator => GL.Geometry.vertex_Id'Last);
