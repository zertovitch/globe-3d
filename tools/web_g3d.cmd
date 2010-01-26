cd ..
cd demo

call make_all

cd ..

perl tools/g3d_html.pl -Iobj/gnatfast -Ibindings -Ibindings/win32 -Isrc -Isrc/gaming -Isrc/gl -Isrc/models -Isrc/unzip -Idemo globe_3d_demo mini globe_3d.ads globe_3d.adb -f -d -og3d_html

cd tools
