#!/bin/sh
# Author: John Howard <Echosoft.LLC@gmail.com>
# Version: 20161004
# Execute this Unix shell script (./make_all.sh) from the demo directory.
gprbuild -P globe_3d_demos.gpr                 -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P deprecated/culler/armada/armada.gpr           -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P deprecated/culler/terrain_vbo/terrain_vbo.gpr -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P deprecated/multi_window/multi_window.gpr      -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P deprecated/sprite/sprite_demo.gpr             -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P ../tools/globe_3d_tools.gpr           -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P ../tools/doom3/to_ada/d3a.gpr      -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P ../tools/doom3/to_g3d/d3g.gpr      -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P ../tools/vrml/w2a.gpr              -d -XG3D_OS=linux -XBuild_Mode=fast
gprbuild -P ../tools/wavefront/o2g.gpr         -d -XG3D_OS=linux -XBuild_Mode=fast
cd ../test/
gnatmake -gnatpn -O2 dico_drill.adb
gprbuild -P vertex_buffer_object/general/simple.gpr -d -XG3D_OS=linux -XBuild_Mode=fast
cd ../demo/