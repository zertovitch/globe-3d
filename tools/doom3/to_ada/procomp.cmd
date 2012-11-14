rem Translation to Ada, then compilation, of a Doom 3 level (.proc file)

d3a %2 %3 %4 %1.proc >%1.ada 

gnatchop -w %1.ada

gnatmake %1 -gnatp -O2 -I..\..\..\src -I..\..\..\bindings -I..\..\..\bindings\win32 -I..\..\..\src\unzip -I..\..\..\src\gl -i -aO..\..\..\obj\gnatfast

