rem This is for the upload of GLOBE_3D.

cd..
cd..

rename globe_3d.zip globe_3d.zip

echo The upload or version date of this archive's GLOBE_3D is: %date% >_GLOBE_3D_Upload_version_%date%.txt

zip -9 globe_3d _GLOBE_3D_Upload_version_%date%.txt

del _GLOBE_3D_Upload_version_%date%.txt

rem goto skip_rezip

rezip -defl -comp globe_3d.zip
del globe_3d.old.zip
ren globe_3d.zip globe_3d.old.zip
ren globe_3d.repacked.zip globe_3d.zip
pause

:skip_rezip

type c:\ada\unpack.msg | zip -z globe_3d.zip

cd globe3d
cd globe_3d
cd tools

pause
