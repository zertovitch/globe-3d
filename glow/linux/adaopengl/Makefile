include ../Make.conf

ADAOPENGL_ALIS = opengl-ext.ali opengl-glu.ali opengl-glut.ali opengl.ali
ifdef WITH_X11
ADAOPENGL_ALIS += opengl-glx-ext.ali opengl-glx.ali
ADA_CFLAGS += -I${ADA_BINDX_DIR}
endif
ifdef WITH_WIN32
ADAOPENGL_ALIS += opengl-wgl.ali
endif
ifdef WITH_GLFW
ADAOPENGL_ALIS += opengl-glfw.ali
endif

all: libadaopengl.a

.PHONY: clean install uninstall

install: libadaopengl.a
	@if [ ! -d ${PREFIX}/lib/ada/adainclude ] ; then \
		mkdir -p ${PREFIX}/lib/ada/adainclude; \
		mkdir -p ${PREFIX}/lib/ada/adalib; \
	fi
	@mkdir -p ${PREFIX}/lib/ada/adainclude/adaopengl
	@for i in `find . -name '*.ads'`; do \
		install -m 644 $${i} ${PREFIX}/lib/ada/adainclude/adaopengl; \
	done

	@mkdir -p ${PREFIX}/lib/ada/adalib/adaopengl
	@install -m 644 libadaopengl.a ${PREFIX}/lib/ada/adalib/adaopengl
	@for i in ${ADAOPENGL_ALIS}; do \
		install -m 644 $${i} ${PREFIX}/lib/ada/adalib/adaopengl; \
	done

uninstall:
	@-rm -rf ${PREFIX}/lib/ada/adainclude/adaopengl
	@-rm -rf ${PREFIX}/lib/ada/adalib/adaopengl

%.ali: %.ads
	$(ADA_CC) -c $(ADA_CFLAGS) -gnaty $<

libadaopengl.a: $(ADAOPENGL_ALIS)
	ar r $@ `echo ${ADAOPENGL_ALIS}|sed -e 's/\.ali/\.o/g'`
	ranlib $@

clean:
	@rm -f libadaopengl.a
	@rm -f *.o *.ali b~*

