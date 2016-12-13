#!binsh
# Author John Howard Echosoft.LLC@gmail.com
# Version 20161004
# Execute this Unix shell script (.make_all.sh) from the demo directory.
gprbuild -P globe_3d_demos.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P cullerarmadaarmada.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P cullerterrain_vboterrain_vbo.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P multi_windowmulti_window.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P spritesprite_demo.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P ..toolsglobe_3d_tools.gpr -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P ..toolsdoom3to_adad3a.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P ..toolsdoom3to_g3dd3g.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P ..toolsvrmlw2a.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
gprbuild -P ..toolswavefronto2g.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
cd ..test
gnatmake dico_drill.adb
gprbuild -P vertex_buffer_objectgeneralsimple.gpr -d -XOS_Kind=linux -XBuild_Mode=fast
cd ..demo
