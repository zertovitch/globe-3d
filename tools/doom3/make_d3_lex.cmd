@echo off
echo.
echo make_d3_lex: option -r recreates sources from the doom3.l file
echo.
if not "%1"=="-r" goto fin
rem
echo ** Compile AFLEX (.l) file to Ada sources
aflex.exe -i -E doom3.l
echo.
gnatchop -w *.a
del *.a
:fin