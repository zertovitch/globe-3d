@echo off

echo --    Cleanup of id's Doom3 / Quake 4 / ... D3G tool  --

call :sub_del  *.bak
call :sub_del  *.bk.?
call :sub_del  *.def
call :sub_del  *.#*
call :sub_del  b~*.*
call :sub_del  allada.txt
call :sub_del  *.log
call :sub_del  *.a
call :sub_del  *.ali
call :sub_del  *.o
call :sub_del  *.bsp
call :sub_del  *.g3d
call :sub_del  *_textures_enum.ada
call :sub_del  *_textures_unzip1.cmd
call :sub_del  *_textures_unzip2.cmd
call :sub_del  *_textures_unzip_list.txt
call :sub_del  *_textures_add_tex_suffix1.cmd
call :sub_del  *_textures_add_tex_suffix2.cmd
call :sub_del  *_textures_copy_fakes.cmd

goto :eof

:sub_del
if exist %1 del %1
