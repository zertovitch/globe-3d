rem This is for the upload of GLOBE_3D.

cd..
cd..

rename globe_3d.zip globe_3d.zip

rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------

set year=%date:~-4,4%

set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%

set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%

set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%

set min=%time:~3,2%

rem set nice_date=%year%-%month%-%day%_%hour%.%min%
set nice_date=%year%-%month%-%day%

rem --------------------------

echo The upload or version date of this archive's GLOBE_3D is: %nice_date% >_GLOBE_3D_Upload_version_%nice_date%.txt

zip -9 globe_3d.zip _GLOBE_3D_Upload_version_%nice_date%.txt

del _GLOBE_3D_Upload_version_%nice_date%.txt

rem goto skip_rezip

rezip -defl -comp globe_3d.zip
del globe_3d.old.zip
ren globe_3d.zip globe_3d.old.zip
ren globe_3d.repacked.zip globe_3d.zip

:skip_rezip

type no_svn\unpack.txt | zip -z globe_3d.zip

copy /B globe_3d.zip globe_3d_release_%nice_date%.zip

cd globe3d
cd globe_3d
cd tools

pause
