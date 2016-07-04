@echo off

echo ----- GLOBE_3D: Make command for Win32.
echo.

if "%1" == "" echo Usage: make_one (unit)
if "%1" == "" pause
if "%1" == "" goto fin

echo ----- Procedure to be built: %1
echo.

set scenario_324943981475=fast

echo -----------------------------------------------
echo ---- Compile mode: %scenario_324943981475%
echo -----------------------------------------------

gprbuild -PGLOBE_3D_Demos.gpr %1.adb -d -XOS_Kind=win32 -XBuild_Mode=%scenario_324943981475%

set scenario_324943981475=

echo.

if exist %1.exe ren %1.exe %1.exe

:fin
echo ----- Done with %1
