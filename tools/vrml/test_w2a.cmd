gprbuild -Pw2a

if "%1"=="" goto default
wrl2ada %1 >test_w2a.out
gnatchop -w test_w2a.out
rem Test compilation
gprbuild -Pw2a -c %1
goto fin

:default
wrl2ada Lissajous.wrl  >test_w2a.out
wrl2ada SkotKnot.wrl  >>test_w2a.out
gnatchop -w test_w2a.out
rem Test compilation
gprbuild -Pw2a -c Lissajous SkotKnot
goto fin

:fin
