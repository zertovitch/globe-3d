@echo off
echo.
echo make_d3g: option -f recreates some of d3g's sources from the doom3.y file
echo.
if not "%1"=="-f" goto comp
rem
echo ** Compile AYACC (.y) file to Ada sources
ayacc.exe doom3.y off off on off>ayacc.log
type ayacc.log
rem
if exist yyparse.adb del yyparse.adb
ren doom3.a yyparse.adb
rem
:comp
echo.
rem
gnatmake -g -I.. -gnato d3g
rem
:fin