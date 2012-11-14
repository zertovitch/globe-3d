@echo off
echo -- Cleanup VRML tools --
del *.c
del *.bak
del *.bk.?
del *.def
del *.#*
del b~*.*
del allada.txt
del *.log
del *.a
rem cd acu
rem cleanacu: see gnatpaqs.zip (cleans ".acu" = {.ali & .o})
rem cleanacu
rem cd..
del *.ali
del *.o
del test_w2a.out
rem del wrl2ada.exe
del translated_from_vrml.ad*
