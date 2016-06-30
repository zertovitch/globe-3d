rem For a clean packaging...
rem
rem svn co http://svn.code.sf.net/p/globe3d/code GLOBE_3D
rem
rem Have a compiled globe_3d_demo_linux in the ../demo directory.
rem Then run this.

del *.bak
cd ..
cd demo
call make_all
cd ..
cd tools

call upx_the_demo

cd..
copy /B demo\freeglut.dll demo\culler\terrain_vbo
copy /B demo\freeglut.dll demo\sprite

cd obj
cd gnat_debug
del *.ali
del *.o
del b~*.ad*
cd ..
cd gnat_small
del *.ali
del *.o
cd ..
cd gnat_fast
del *.ali
del *.o
cd ..
cd ..
cd ..

zip -9 -r GLOBE_3D.zip GLOBE_3D\*

deflopt GLOBE_3D.zip
rem DeflOpt also removes bogus Zip entries with directory names

call lowcase_and_rezip.cmd

