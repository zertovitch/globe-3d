@echo off

echo Example of using the Doom3 .proc to .g3d converter, d3g
echo =======================================================

rem call make_d3g -O

if not exist delta4g1.proc goto bug

d3g delta4g1.proc -j -s(-174.0,+616.0,-2524.0)

rem Update local copy of level resource data
zip -f -9 g3demo_level_resources.zip

del *.g3d
del *.bsp
del *_texture*

rem start the big demo, load everything:
GLOBE_3D_Demo.exe -load

goto fin

:bug
echo The map file, delta4g1.proc, is missing.
echo A copy is stored in extras/model_src.zip
echo.
pause

:fin
