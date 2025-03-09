cd ..
cd demo

call make_all

cd ..


set params=-f -og3d_html -b#fffcfb -ig3d_head.txt -jg3d_top.txt -kg3d_bottom.txt
set params=%params% -Iobj/gnat_debug -Ibindings -Ibindings/win32 -Isrc -Isrc/gaming -Isrc/gl -Isrc/models -Isrc/unzip -Idemo 

rem Here we invoke GNATHTML ( https://github.com/zertovitch/ali_parse )

gnathtml globe_3d_demo.adb mini_3d.adb globe_3d.ads globe_3d.adb %params%

start g3d_html/index.html

cd extras

pause
