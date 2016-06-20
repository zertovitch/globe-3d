set model3d=%~n1
o2g %model3d%
echo GLOBE_3D_Demo.exe -load=%model3d%>%model3d%.cmd
zipada -ed1 %model3d%.zip %model3d%.g3d %model3d%.obj %model3d%.mtl %model3d%.cmd *.bmp *.tga
%model3d%