@echo off

if "%1"=="" goto syntaxe

if not exist %1.proc unzip %1.zip %1.proc
rem ^ the .zip file may contain the .proc file, although useless for globe_3d
if not exist %1.proc unzip -j %1.pk4 *.proc
rem ^ look for the .proc file in the PK4 file
if not exist %1.proc echo Processed map file was "%1.proc" not found!
if not exist %1.proc goto syntaxe

call make_d3g -O

echo.
echo **************
echo ** D3G TOOL ** Build and save objects, BSP, etc. from %1.proc into %1.zip
echo **************
echo.

if     "%2"=="" d3g -j -a %1.proc -c0
if not "%2"=="" d3g -j -a %1.proc %2 %3 %4 %5 %6 %7 %8


echo.
echo ** Make / update %1.zip file with the stuff

copy %1.zip %1.old.zip

zip %1.zip %1*.g3d %1.bsp
del *.g3d
del *.bsp

echo **************
echo ** Add textures to %1.zip...
echo **************

md tmp
cd tmp
del *.bmp
del *.tga

echo   ** Put: *Original* DOOM 3 or QUAKE 4 textures (d3tex.zip has contents of pak004.pk4, but flat directory)
rem * Fast
7z e -y -i@..\%1_textures_unzip_list.txt ..\d3tex.zip
7z e -y -i@..\%1_textures_unzip_list.txt ..\q4tex.zip
rem * Slow, but with standard unzip
rem call ..\%1_textures_unzip1

echo   ** Put: *Modified* textures, e.g. stored as BMP's with palette
rem * Fast
7z e -y -i@..\%1_textures_unzip_list.txt ..\palettex.zip
rem * Slow, but with standard unzip
rem call ..\%1_textures_unzip2

echo   ** Put: Textures from the PK4 file
unzip -j -n ..\%1.PK4 *.tga

echo   ** Put: *Already stored* textures
unzip -o ..\%1.zip *.tga *.bmp

echo   ** Some diffuse images don't have the "_d"
call ..\%1_textures_add_tex_suffix1

echo   ** Put: Fake images for missing textures
call ..\%1_textures_copy_fakes

echo   ** Do some filtering
call ..\%1_textures_add_tex_suffix2

echo   ** Store all these pictures
zip ..\%1.zip *.bmp *.tga

cd..

echo.
echo ** Display and Play!

echo GLOBE_3D_Demo.exe -load=%1 >%1.bat

start %1

goto fin

:syntaxe
echo Syntax: procpack mylevel
echo.
echo Processes mylevel.proc and creates or completes mylevel.zip
echo Eventually: unpacks mylevel.proc first from mylevel.zip
echo If present, mylevel.pk4 will be used for finding mylevel.proc
echo if not found before; textures from mylevel.pk4 will be used too.
echo Other sources of textures:
echo   d3tex.zip          (original game textures)
echo   q4tex.zip          (original game textures)
echo   palettex.zip       (modified textures; names may overload original ones)
echo.
pause

:fin