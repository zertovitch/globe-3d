call upx_the_demo

cd..
cd obj
cd gnatdebg
del *.ali
del *.o
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

zip -9 -r GLOBE_3D.zip GLOBE_3D\*
