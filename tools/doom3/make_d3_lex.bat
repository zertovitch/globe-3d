@echo off
echo.
echo make_d3_lex: option -f recreates sources from the doom3.l file
echo.
if not "%1"=="-f" goto fin
rem
echo ** Compile AFLEX (.l) file to Ada sources
aflex.exe -i doom3.l
echo.
gnatchop -w *.a
del *.a
:fin