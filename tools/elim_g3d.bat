cd..

set src=-aIsrc -aIsrc/unzip -aIsrc/models -aIsrc/gaming -aIbindings -aIbindings/win32 -aIbindings/win32/gcc_fmt
set obj=-lopengl32 -lglu32 -lglut32 -Lobj/libwin32

call elim globe_3d_demo %src%

del g3d_elim.pra.bak
ren g3d_elim.pra g3d_elim.pra.bak
ren gnat.eli g3d_elim.pra

cd tools