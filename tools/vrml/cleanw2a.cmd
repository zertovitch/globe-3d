@echo off

echo --  Cleanup of VRML tools  --
call :sub_del *.c
call :sub_del *.bak
call :sub_del *.bk.?
call :sub_del *.def
call :sub_del *.#*
call :sub_del b~*.*
call :sub_del allada.txt
call :sub_del *.log
call :sub_del *.a
call :sub_del *.ali
call :sub_del *.o
call :sub_del test_w2a.out
call :sub_del translated_from_vrml.ad*

goto :eof

:sub_del
if exist %1 del %1