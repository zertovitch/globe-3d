rem Example of using the Doom3 to .g3d converter

call make_d3g

d3g Delta4g1.proc -j -a -c0

rem -c1 -s(-32,-25,-109)
rem -s(-174.0,+616.0,-2524.0)

rem Update local copy of level resource data

zip -f -1 G3Demo_Level_Resources.zip

rem start the big demo, load everything:
g -load