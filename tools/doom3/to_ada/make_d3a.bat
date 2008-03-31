@echo off
echo make_d3a: option -f recreates sources from the doom3.y, doom3.l files
if not "%1"=="-f" goto comp
rem
echo 1) Compile AYACC/AFLEX files to Ada sources
rem
echo .
echo 1.1) Running ayacc.exe
ayacc.exe doom3.y off off on off>ayacc.log
type ayacc.log
rem
if exist yargla.a del yargla.a
ren doom3.a yargla.a
rem
echo .
echo 1.2) Running aflex.exe
aflex.exe -i doom3.l
echo .
rem
echo 2) Glue everything together and let gnatchop find the right filenames
rem
copy *.a allada.txt
call gnatchop -w allada.txt
rem
:comp
echo 3) gnatmake d3a
rem
gnatmake -g -gnato d3a -largs -Wl,--stack=0x10000000
rem
echo 4) New d3a.exe is built (or should...) !
:fin