@echo off
echo.
echo make_y: option -f write doom3.y files from doom3.pry
echo.
if not "%1"=="-f" goto fin

echo ** Generating Doom3.y files
echo.

attrib -R /S *.y

gnatprep -b -u -Dver="ada" doom3.pry to_ada\doom3.y
gnatprep -b -u -Dver="g3d" doom3.pry to_g3d\doom3.y

echo ##>>to_ada\doom3.y
echo ##>>to_g3d\doom3.y

attrib +R /S *.y

:fin