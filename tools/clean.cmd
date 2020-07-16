@echo off

rem go to g3d root:
cd..

call :sub_clean_modes obj
call :sub_clean_modes lib

call :sub_clean demo
call :sub_clean src

cd src
call :sub_clean models
call :sub_clean gaming
call :sub_clean unzip
cd..

call :sub_clean bindings
cd bindings
call :sub_clean win32
cd win32
call :sub_clean gcc_fmt
cd..
cd..

call :sub_clean tools
cd tools
cd vrml
call cleanw2a
cd..
cd doom3
call clean_doom
cd..

goto :eof

:sub_clean_modes
 cd %1
    call :sub_clean gnat_debug
    call :sub_clean gnat_fast
    call :sub_clean gnat_small
 cd ..

:sub_clean
 cd %1
   call :sub_del *.ali
   call :sub_del *.db
   call :sub_del *.xml
   call :sub_del *.bexch
   call :sub_del *.lexch
   call :sub_del *.o
   call :sub_del *.stderr
   call :sub_del *.stdout
   call :sub_del b~*
   call :sub_del b__*
   call :sub_del *.#*
   call :sub_del *.bak
   call :sub_del *.bk.?
   call :sub_del libglobe3d.a
 cd ..
 goto :eof

:sub_del
if exist %1 del %1