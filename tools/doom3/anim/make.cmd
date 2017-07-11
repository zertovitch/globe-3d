echo ** Compile the AFLEX (md5.l) file to Ada sources
aflex.exe -i -E md5.l
echo.
gnatchop -w *.a
del *.a

echo ** Compile the AYACC (md5.y) file to Ada sources
ayacc.exe md5.y off off on on >ayacc.log
type ayacc.log
echo. >>ayacc.log
type md5.verbose>>ayacc.log
del md5.verbose
rem
if exist yyparse.adb del yyparse.adb
ren md5.a yyparse.adb

gprbuild -P m2g.gpr