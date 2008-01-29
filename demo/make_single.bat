@echo off

echo ----- GLOBE_3D: Make command for Win32.
echo.

if "%1" == "" echo Usage: make_1 (unit)
if "%1" == "" goto fin

echo ----- Procedure to be built: %1
echo.

rem ** Common gnatmake / compiler options
set compiler=-i -gnatecg3d_elim.pra
set src=-aI../src -aI../src/gl -aI../src/unzip -aI../src/models -aI../src/gaming -aI../bindings -aI../bindings/win32 -aI../bindings/win32/gcc_fmt -aI../ada05_proxy
set obj=-lopengl32 ../obj/libwin32/glee.o -lglu32 -lfreeglut -L../obj/libwin32

rem -Wl,--stack=20000000

goto small

rem -----------------------------------------------
:debug
echo ---- compile mode: DEBUG
gnatmake %compiler% -g -j2 -fstack-check -gnato -gnatwa -gnatVa -gnatecdebug.pra -fno-strict-aliasing %src% -aO../obj/gnatdebg %1 %2 %3 -bargs -E -largs %obj%
goto done

rem -----------------------------------------------
:fast
echo ---- compile mode: OPTIM: FAST
gnatmake %compiler% -O2 -j2 -gnatpn -funroll-loops -fpeel-loops -ftracer -funswitch-loops -fno-strict-aliasing %src% -aO../obj/gnatopti %1 %2 %3 -largs %obj% -s -mwindows

rem -----------------------------------------------
:small
echo ---- compile mode: OPTIM: SMALL
rem ** Set a good old version of GNAT (small .exe, Win 9x compatible)
rem ** Trick silent if alternative GNAT not installed...
path f:\ada\gnat315p\bin;%path%
gnatmake %compiler% -O2 -j2 -s -gnatp %src% -aO../obj/gnatsmal %1 %2 %3 -largs %obj% -s -mwindows

rem -----------------------------------------------
:done

rem GNAT 2005 -fno-strict-aliasing

echo.

if exist %1.exe ren %1.exe %1.exe
if exist b~%1.* del b~%1.*

:fin
echo ----- Done with %1
