d3a %2 %3 %4 %1.proc >%1.ada 
gnatchop -w %1.ada
gnatmake %1 -gnatp -O2 -fomit-frame-pointer -I..\..\src -I..\..\bindings -I..\..\src\unzip -i -aO..\..\obj\gnatfast

