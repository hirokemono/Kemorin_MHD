#! /bin/csh
#
#

HOMEDIR= $(HOME)
LOCALDIR= $(HOMEDIR)/local



WORKDIR= $(HOMEDIR)/src_kemo/work
MHDDIR =    $(HOMEDIR)/src_kemo

C_SRCDIR= /Users/matsui/src_kemo/MHD/C_src

KEMO_C_ICLUDES = \
-I$(MHDDIR)/MHD/C_src/CORE_C \
-I$(MHDDIR)/MHD/C_src/CONTROLS \
-I$(MHDDIR)/MHD/C_src/KEMO_GL \
-I$(MHDDIR)/MHD/C_src/GLSL \
-I$(MHDDIR)/MHD/C_src/KEMO_GLUT



CC=mpicc
OPTFLAGS = -O3 -Wall -g

#
#   Library settings
#
KEMO_INC = -I. $(KEMO_C_ICLUDES)
LIB_KEMO_C = -L$(WORKDIR) -lkemo_c $(PNG_LIBS) $(ZLIB_LIBS)

ZLIB_LIBS = `pkg-config --cflags --libs zlib`
#
PNG_LIBS = `pkg-config --cflags --libs libpng`
#
COCOA_FLAG= -D__APPLE__ 
OPTFLAGS_GL =  $(OPTFLAGS) $(COCOA_FLAG) $(KEMO_INC)
OPENGL_LIBS= -framework OpenGL  -framework Cocoa

#
#  -------------------------------------------------------------------------
#

TARGET_KEMOVIEW_GTK = kemoviewer_glfw_test

MOD_KEMOVIEW_GTK = \
kemo_mesh_viewer_glfw.o \
kemoviewer_glfw_test.o \
view_modifier_glfw.o

GLFW_INC= `pkg-config --cflags glfw3`
GLFW_LIBS=`pkg-config --cflags --libs glfw3`

#
#  -------------------------------------------------------------------------
#

.SUFFIXES: .o .c

.c.o:
	$(CC) -c $(OPTFLAGS_GL) $(GLFW_INC) $<


all: $(TARGET_KEMOVIEW_GTK)

$(TARGET_KEMOVIEW_GTK): $(MOD_KEMOVIEW_GTK)
	$(CC) $(OPTFLAGS_GL) -o $(TARGET_KEMOVIEW_GTK) $(MOD_KEMOVIEW_GTK) $(LIB_KEMO_C) $(GLFW_LIBS) $(OPENGL_LIBS)

kemoviewer_glfw_test.o: kemoviewer_glfw_test.c kemo_mesh_viewer_glfw.o view_modifier_glfw.o

kemo_mesh_viewer_glfw.o: kemo_mesh_viewer_glfw.c

view_modifier_glfw.o: view_modifier_glfw.c
	$(CC) -c $(OPTFLAGS_GL) $(GLFW_INC) $<

install:
	if [ ! -d $(LOCALDIR) ]; then \
		mkdir $(LOCALDIR); \
	fi
	if [ ! -d $(LOCALDIR)/lib ]; then \
		mkdir $(LOCALDIR)/lib ; \
	fi
	if [ ! -d $(LOCALDIR)/bin ]; then \
		mkdir $(LOCALDIR)/bin; \
	fi
	if [ ! -d $(LOCALDIR)/include ]; then \
		mkdir $(LOCALDIR)/include; \
	fi
	cp $(TARGET_KEMOVIEW_GTK)  $(LOCALDIR)/bin

clean:
	rm -f *.o ;
	rm -f *.mod ;
	rm -f *~ ;
	rm -f *.par ;
	rm -f *.diag
	rm -f $(TARGET_KEMOVIEW_GTK)
