@echo off

rem   Create a DOT ( http://www.graphviz.org/ ) file for displaying
rem   the GLOBE_3D unit dependency structure.
rem   The tool is called DePlo, http://sites.google.com/site/depplot/

deplo config=g3d_dependency_graph.cfg >g3d_deps.dot

dot -o g3d_deps.emf -Temf g3d_deps.dot
dot -o g3d_deps.svg -Tsvg g3d_deps.dot
