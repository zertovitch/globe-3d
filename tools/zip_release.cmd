del *.bak

call upx_the_demo

cd..
copy /B demo\freeglut.dll demo\culler\terrain_vbo
copy /B demo\freeglut.dll demo\sprite

cd obj
cd gnatdebg
del *.ali
del *.o
del b~*.ad*
cd ..
cd gnatsmal
del *.ali
del *.o
cd ..
cd gnatfast
del *.ali
del *.o
cd ..
cd ..
cd ..

rem ren globe3d GLOBE_3D
rem We prefer with '_', the package & project name
rem Then, svn checkout with '_' and everything is fine ;-)
zip -9 -r GLOBE_3D.zip GLOBE_3D\*
rem ren globe_3d globe3d
deflopt GLOBE_3D.zip

pause
