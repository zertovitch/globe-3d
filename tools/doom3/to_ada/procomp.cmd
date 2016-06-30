rem Translation to Ada, then compilation, of a Doom 3 level (.proc file)

d3a %2 %3 %4 %1.proc >%1.ada 

gnatchop -w %1.ada

gprbuild -Pd3a %1.adb

