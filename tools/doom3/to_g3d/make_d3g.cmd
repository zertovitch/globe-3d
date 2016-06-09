@echo off
echo.
echo make_d3g: option -r recreates some of d3g's sources from the doom3.y file
echo                  -O builds with speed-oriented optimizations
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
set opt_comp294572=Debug
if "%1"=="-O" set opt_comp294572=Fast
if "%1"=="-O" shift
rem
gnatmake -Pd3g.gpr -XBuild_Mode=%opt_comp294572%
rem
set opt_comp294572=
:fin