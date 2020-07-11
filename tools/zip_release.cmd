rem   For a clean packaging...
rem  
rem   svn co http://svn.code.sf.net/p/globe3d/code GLOBE_3D
rem  
rem   Have a compiled globe_3d_demo_linux in the ../demo directory
rem   (this is for having both Linux and Windows executables packaged).
rem   Then run this.

del *.bak
cd ..
cd demo
if "%1"=="-b" gprbuild -PGLOBE_3D_Demos.gpr GLOBE_3D_Demo.adb Mini.adb -XOS_Kind=win32 -XBuild_Mode=small
cd ..
cd tools

REM call upx_the_demo

cd..
copy /B demo\freeglut.dll demo\culler\terrain_vbo
copy /B demo\freeglut.dll demo\sprite

cd tools
call clean
cd ..

set g3d_root=g3d
cd ..

zip -9 -r GLOBE_3D.zip %g3d_root%\*

cd %g3d_root%
cd tools
rem call lowcase_and_rezip.cmd
