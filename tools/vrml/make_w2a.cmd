@echo off
echo make_w2a: option -f recreates sources from the vrml.y, vrml.l files
if not "%1"=="-f" goto comp
rem
echo 1) Compile AYACC/AFLEX files to Ada sources
rem
echo .
echo 1.1) Running ayacc.exe
ayacc.exe vrml.y off off on off>ayacc.log
type ayacc.log
rem
if exist yargla.a del yargla.a
ren vrml.a yargla.a
rem
echo .
echo 1.2) Running aflex.exe
aflex.exe -i vrml.l
echo .
rem
echo 2) Glue everything together and let gnatchop find the right filenames
rem
copy *.a allada.txt
call gnatchop -w allada.txt
rem
:comp
echo 3) build Wrl2Ada
rem
gprbuild -Pw2a.gpr
rem
echo 4) New wrl2ada.exe is built (or should...) !
:fin