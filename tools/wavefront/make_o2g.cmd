@echo off
echo.
echo make_o2g: option -O builds with speed-oriented optimizations
echo.

              set opt_comp294572=debug
if "%1"=="-O" set opt_comp294572=fast

gprbuild -Po2g.gpr -XBuild_Mode=%opt_comp294572%

set opt_comp294572=

