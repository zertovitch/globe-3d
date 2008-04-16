@echo off

echo ** Build and save objects, BSP, etc. from %1.proc

d3g -j -a %1.proc -c0

echo ** Make %1.zip file with the stuff (or add to an existing %1.zip)

zip %1.zip %1*.g3d %1.bsp

echo ** Add textures to %1.zip...

rem !!

echo ** Display!

GLOBE_3D_Demo.exe -load=%1
