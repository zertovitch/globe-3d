@echo off
echo.
echo make_y: option -r writes doom3.y parser files from doom3.pry
echo.
if not "%1"=="-r" goto fin

echo ** Generating Doom3.y files in their "to-G3D-files" and "to-Ada-sources" flavors
echo.

attrib -R /S *.y

gnatprep -b -u -Dver="ada" doom3.pry to_ada\doom3.y
gnatprep -b -u -Dver="g3d" doom3.pry to_g3d\doom3.y

echo ##>>to_ada\doom3.y
echo ##>>to_g3d\doom3.y

attrib +R /S *.y

:fin