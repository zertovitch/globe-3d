cd..

set src=-Idemo -Isrc -Isrc/unzip -Isrc/gl -Isrc/models -Isrc/gaming -Ibindings -Ibindings/win32 -Ibindings/win32/gcc_fmt
set obj=-lopengl32 -lglu32 -lglut32 -Lobj/libwin32

call tools\elim globe_3d_demo %src%

del g3d_elim.pra.bak
ren g3d_elim.pra g3d_elim.pra.bak
ren gnat.eli g3d_elim.pra

cd tools