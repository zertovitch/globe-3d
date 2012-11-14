@echo off

rem G. de Montmollin, ver 31-Aug-2006 (1st: 4-Mar-2005)

if .%1. == .. goto syn
set main=%1
shift

set rtl=
if .%1. == .-a. set rtl=%1
if .%1. == .-a. shift

set include=%1 %2 %3 %4 %5 %6 %7 %8 %9

rem 1. creating a bind file:

gnatmake -c -gnatp %include% %rtl% %main%
gnatbind -A %main%.ali


rem 2. creating a set of tree files:

gnatmake -f -c %include% %rtl% -gnatc -gnatt %main%

rem: bug with RTL (3.15p), need tree for some more units:

if .%rtl%. == .-a. gnatmake -f -c %include% %rtl% -gnatc -gnatt g-except.ads
if .%rtl%. == .-a. gnatmake -f -c %include% %rtl% -gnatc -gnatt a-chahan.adb
if .%rtl%. == .-a. gnatmake -f -c %include% %rtl% -gnatc -gnatt g-casuti.adb
if .%rtl%. == .-a. gnatmake -f -c %include% %rtl% -gnatc -gnatt a-numaux.adb

rem 3. call gnatelim

gnatelim -v %rtl% %include% -m %main% >gnat.eli

goto fin

:syn
echo elim - creates gnat.eli with gnatelim; gnat.eli can be used as gnat.adc
echo.
echo Usage: elim main_name [-a] [-Iincdir1] [-Iincdir2] ...
echo.
echo -a     includes Run-Time Library
echo.

:fin
