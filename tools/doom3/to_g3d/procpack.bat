@echo off

call make_d3g

echo ** Build and save objects, BSP, etc. from %1.proc into %1.zip

if not exist %1.proc unzip %1.zip %1.proc

if     "%2"=="" d3g -j -a %1.proc -c0
if not "%2"=="" d3g -j -a %1.proc %2 %3 %4 %5 %6 %7 %8


echo ** Make / update %1.zip file with the stuff

copy %1.zip %1.old.zip

zip %1.zip %1*.g3d %1.bsp
del *.g3d
del *.bsp

echo ** Add textures to %1.zip...

md tmp
cd tmp
del *.bmp
del *.tga
cd..
call %1_textures_copy_fakes
cd tmp

call 7zip e -y -i@..\%1_textures_unzip_list.txt C:\Transferts\Doom3map\pak004.zip
unzip -o ..\%1.zip *.tga *.bmp
call ..\%1_textures_add_tex_suffix
zip ..\%1.zip *.bmp *.tga
cd..

echo ** Display!

echo GLOBE_3D_Demo.exe -load=%1 >%1.bat

%1
