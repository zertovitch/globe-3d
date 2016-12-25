gprbuild -PGLOBE_3D_Demos.gpr GLOBE_3D_Demo.adb Mini.adb -d -XOS_Kind=win32 -XBuild_Mode=fast

rem  Also build other demos and tools - from make_all.sh by John Howard, Echosoft.LLC@gmail.com

gprbuild -P culler/armada/armada.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P culler/terrain_vbo/terrain_vbo.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P multi_window/multi_window.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P sprite/sprite_demo.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P ../tools/globe_3d_tools.gpr -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P ../tools/doom3/to_ada/d3a.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P ../tools/doom3/to_g3d/d3g.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P ../tools/vrml/w2a.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
gprbuild -P ../tools/wavefront/o2g.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
cd ..\test
gnatmake -gnatpn -O2 dico_drill.adb
rem gprbuild -P vertex_buffer_object/general/simple.gpr -d -XOS_Kind=win32 -XBuild_Mode=fast
cd ..\demo