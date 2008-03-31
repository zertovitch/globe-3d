rem for cleanacu: see gnatpaqs.zip on Gautier's Ada page
rem
rem go to g3d root:
cd..
rem
cd obj
cd gnatdebg
rem We don't need to keep the .ali, GPS/GNATMAKE can now put all .ali/.o files in a single directory
rem cleanacu
 del *.ali
 del *.o
del b~*
cd..
cd gnatfast
rem We don't need to keep the .ali, GPS/GNATMAKE can now put all .ali/.o files in a single directory
rem cleanacu
 del *.ali
 del *.o
del b~*
cd..
cd gnatsmal
rem We don't need to keep the .ali, GPS/GNATMAKE can now put all .ali/.o files in a single directory
rem cleanacu
 del *.ali
 del *.o
del b~*
cd..
cd..
cd demo
rem del *.exe
del *.#*
del b~*
del *.bak
del *.bk.?
cd..
cd src
del *.bak
del *.bk.?
cd models
del *.bak
del *.bk.?
cd..
cd gaming
del *.bak
del *.bk.?
cd..
cd unzip
del *.bak
del *.bk.?
cd..
cd..
cd bindings
del *.bak
del *.bk.?
cd win32
del *.bak
del *.bk.?
cd gcc_fmt
del *.bak
del *.bk.?
cd..
cd..
cd..
cd tools
del *.bak
del *.bk.?
del b~*
del *.ali
del *.o
cd vrml
call cleanw2a
cd..
cd doom3
call cleand3a
cd..
cd..
