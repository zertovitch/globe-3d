rem This is for the upload of GLOBE_3D.

rename globe_3d.zip globe_3d.zip

echo The upload or version date of this archive's GLOBE_3D is: %date% >_GLOBE_3D_Upload_version_%date%.txt

zip -9 globe_3d _GLOBE_3D_Upload_version_%date%.txt

del _GLOBE_3D_Upload_version_%date%.txt

goto skip_rezip

rezip globe_3d.zip
deflopt globe_3d.zip

:skip_rezip

type c:\ada\unpack.msg | zip -z globe_3d.zip
pause
