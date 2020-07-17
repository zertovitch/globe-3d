@echo off

echo --  Cleanup of id's Doom3 / Quake 4 / ... tools  --

call :sub_del  *.a
call :sub_del  *.#*
call :sub_del  *.ali
call :sub_del  *.o
call :sub_del  *.bak

cd to_ada

call :sub_del  *.bak
call :sub_del  *.bk.?
call :sub_del  *.def
call :sub_del  *.#*
call :sub_del  b~*.*
call :sub_del  allada.txt
call :sub_del  *.log
call :sub_del  *.a
call :sub_del  *.ali
call :sub_del  *.o

cd..

cd to_g3d
call clean_doom.cmd
cd..


goto :eof

:sub_del
if exist %1 del %1
