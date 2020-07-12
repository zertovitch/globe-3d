cd..

rem  Remove Ada files written by GNAT.
call :sub_clean_modes obj
call :sub_clean_modes lib

zipada -ep2 -r2 g3d_src_%date%.zip '*.ad?' '*.y' '*.l' '*.bat' '*.ms' '*.ago' '*.cmd' '*.pr?' '*.gpr' '*.html' '*.txt' '*.bmp'
cd tools


goto :eof

:sub_clean_modes
 cd %1
    call :sub_clean gnat_debug
    call :sub_clean gnat_fast
    call :sub_clean gnat_small
 cd ..

:sub_clean
 cd %1
   call :sub_del b~*
   call :sub_del b__*
 cd ..
 goto :eof

:sub_del
if exist %1 del %1
