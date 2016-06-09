rem Example of using the Doom3 to .g3d converter, d3g

rem call make_d3g

rem The map file, delta4g1.proc, is stored in extras/model_src.zip

d3g delta4g1.proc -j -a -s(-174.0,+616.0,-2524.0)

rem -c1 -s(-32,-25,-109)
rem -s(-174.0,+616.0,-2524.0)

rem Update local copy of level resource data

zip -f -9 G3Demo_Level_Resources.zip

rem start the big demo, load everything:
GLOBE_3D_Demo.exe -load
