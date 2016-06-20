set model3d=%~n1
o2g %model3d%
zipada -ed1 %model3d%.zip %model3d%.g3d *.bmp *.tga
GLOBE_3D_Demo.exe -load=%model3d%
