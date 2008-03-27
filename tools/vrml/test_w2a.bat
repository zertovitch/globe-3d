if "%1"=="" goto default
wrl2ada %1 >test_w2a.out
goto fin

:default
wrl2ada Lissajous.wrl  >test_w2a.out
wrl2ada SkotKnot.wrl  >>test_w2a.out
goto fin

:fin
gnatchop -w test_w2a.out
gnatmake -i -g -I..\..\src -I..\bindings -I..\bindings\win32 -I..\bindings\win32\gcc_fmt -I..\..\src\unzip -aO..\..\obj\gnatdebg translated_from_vrml