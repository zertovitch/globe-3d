@echo off
echo.
echo make_d3a: option -r recreates some of d3a's sources from the doom3.y file
echo.
if not "%1"=="-r" goto comp
rem
echo ** Compile AYACC (.y) file to Ada sources
ayacc.exe doom3.y off off on off>ayacc.log
type ayacc.log
rem
if exist yyparse.adb del yyparse.adb
ren doom3.a yyparse.adb
shift
rem
:comp
echo.
rem
gnatmake -Pd3a.gpr
rem
:fin