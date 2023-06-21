SHELL = /bin/sh

SRCDIR = /Users/matsui/src_kemo
WORKDIR = $(SRCDIR)/work
CC=gcc
F90=mpif90

F90OPTFLAGS = -O3 -Wall -g -fopenmp  $(F_INCLUDE)
F90CPPFLAGS =  -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO -DGLFW3
F90DLLFLAGS = -fPIC $(F90OPTFLAGS)
OPTFLAGS = -O3 -Wall -g  -D__APPLE__
CXXFLAGS =  -D__APPLE__

KEMO_INCLUDE = -I. \
-I$(WORKDIR) \
-I$(SRCDIR)/MHD/C_src/CORE_C \
-I$(SRCDIR)/MHD/C_src/GTK \
-I$(SRCDIR)/MHD/C_src/CONTROLS

KEMO_LIBS = -L/Users/matsui/src_kemo/work \
-lkemo_MHD \
-lkemo_viz \
-lkemo_parallel \
-lkemo_solver \
-lkemo_core \
-lkemo_c

GTK3_CFLAGS =  -I/opt/homebrew/Cellar/gtk+3/3.24.37/include/gtk-3.0 -I/opt/homebrew/Cellar/glib/2.76.2/include/gio-unix-2.0 -I/opt/homebrew/Cellar/cairo/1.16.0_5/include/cairo -I/opt/homebrew/Cellar/libepoxy/1.5.10/include -I/opt/homebrew/Cellar/pango/1.50.14/include/pango-1.0 -I/opt/homebrew/Cellar/harfbuzz/7.1.0/include/harfbuzz -I/opt/homebrew/Cellar/pango/1.50.14/include/pango-1.0 -I/opt/homebrew/Cellar/fribidi/1.0.12/include/fribidi -I/opt/homebrew/Cellar/harfbuzz/7.1.0/include/harfbuzz -I/opt/homebrew/Cellar/graphite2/1.3.14/include -I/opt/homebrew/Cellar/atk/2.38.0/include/atk-1.0 -I/opt/homebrew/Cellar/cairo/1.16.0_5/include/cairo -I/opt/homebrew/Cellar/pixman/0.42.2/include/pixman-1 -I/opt/homebrew/Cellar/fontconfig/2.14.2/include -I/opt/homebrew/opt/freetype/include/freetype2 -I/opt/homebrew/Cellar/libxcb/1.15_1/include -I/opt/homebrew/Cellar/libxrender/0.9.11/include -I/opt/homebrew/Cellar/libxext/1.3.5/include -I/opt/homebrew/Cellar/libx11/1.8.4/include -I/opt/homebrew/Cellar/libxcb/1.15_1/include -I/opt/homebrew/Cellar/libxau/1.0.11/include -I/opt/homebrew/Cellar/libxdmcp/1.1.4/include -I/opt/homebrew/Cellar/gdk-pixbuf/2.42.10_1/include/gdk-pixbuf-2.0 -I/opt/homebrew/opt/libpng/include/libpng16 -I/opt/homebrew/Cellar/libtiff/4.5.0/include -I/opt/homebrew/Cellar/zstd/1.5.5/include -I/opt/homebrew/Cellar/jpeg-turbo/2.1.5.1/include -I/opt/homebrew/Cellar/glib/2.76.2/include -I/opt/homebrew/Cellar/glib/2.76.2/include/glib-2.0 -I/opt/homebrew/Cellar/glib/2.76.2/lib/glib-2.0/include -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/Cellar/pcre2/10.42/include -I/opt/homebrew/Cellar/xorgproto/2022.2/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/ffi -I/usr/X11/include

GTK3_LIBS =    -L/opt/homebrew/Cellar/gtk+3/3.24.37/lib -L/opt/homebrew/Cellar/pango/1.50.14/lib -L/opt/homebrew/Cellar/harfbuzz/7.1.0/lib -L/opt/homebrew/Cellar/atk/2.38.0/lib -L/opt/homebrew/Cellar/cairo/1.16.0_5/lib -L/opt/homebrew/Cellar/gdk-pixbuf/2.42.10_1/lib -L/opt/homebrew/Cellar/glib/2.76.2/lib -L/opt/homebrew/opt/gettext/lib -lgtk-3 -lgdk-3 -Wl,-framework,Cocoa -Wl,-framework,Carbon -Wl,-framework,CoreGraphics -lpangocairo-1.0 -lpango-1.0 -lharfbuzz -latk-1.0 -lcairo-gobject -lcairo -lgdk_pixbuf-2.0 -lgio-2.0 -lgobject-2.0 -lglib-2.0 -lintl -L/usr/X11/lib -lX11 -lm

all: control_GTK

control_GTK: file_dialog.o read_mhd_control_4_c.o
	$(F90) $(F90OPTFLAGS) $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	-o control_GTK file_dialog.o read_mhd_control_4_c.o $(KEMO_LIBS) $(GTK3_LIBS)


file_dialog.o: file_dialog.c
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/file_dialog.c

read_mhd_control_4_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/read_mhd_control_4_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/read_mhd_control_4_c.f90

clean:
	rm -f *.o *.mod *~