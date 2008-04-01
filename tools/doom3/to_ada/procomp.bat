d3a %2 %3 %4 %1.proc >%1.ada 
gnatchop -w %1.ada
gnatmake %1 -i -gnatp -O2 -I..\..\..\src -I..\..\..\bindings -I..\..\..\src\unzip -I..\..\..\src\gl -i -aO..\..\..\obj\gnatsmal

