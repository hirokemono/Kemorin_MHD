#
# Makefile header for Kemorin Dynamo tests
#
#
SHELL = /bin/sh
#
#  all:         compile programs for simulations
#  utils_pg:    compile programs for 2d-drawing (PGPLOT is required)
#  viewer_gtk:  compile programs for gtk viewer  (GLUT & GTK are required)
#
# Compiler commands
#
CC=mpicc
F90=mpif90
CXX=
AS=mpicc
AR=ar
RANLIB=ranlib
F90_LOCAL=mpif90
MPIRUN=mpirun
#
# Compiler options
#
F90OPTFLAGS = -O3 -Wall -g -fopenmp  $(F_INCLUDE)
F90CPPFLAGS =  -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO -DGLFW3 -DFFMPEG
F90DLLFLAGS = -fPIC $(F90OPTFLAGS)
OPTFLAGS = -O3 -Wall -g  -D__APPLE__
CXXFLAGS =  -D__APPLE__
#
# Libralies and includes settting
#

F90LIBS =   $(PNG_LIBS) $(ZLIB_LIBS) $(FFTW3_LIBS) $(BLAS_LIBS) $(HDF5_LDFLAGS) $(HDF5_FLIBS)
F_INCLUDE = -I. -I/Users/matsui/src_kemo/work $(HDF5_FFLAGS)
C_INCLUDE = -I. -I/Users/matsui/src_kemo/work -I/Users/matsui/src_kemo/MHD/C_src/CORE_C -I/Users/matsui/src_kemo/MHD/C_src/CONTROLS $(PNG_CFLAGS)  $(ZLIB_CFLAGS)  $(FFTW3_CFLAGS) -I/Users/matsui/src_kemo/MHD/C_src/KEMO_GL -I/Users/matsui/src_kemo/MHD/C_src/GLSL -I/Users/matsui/src_kemo/MHD/C_src/GTK -I/Users/matsui/src_kemo/MHD/C_src/KEMO_GLUT

#
# zlib settings
#
ZLIB_CFLAGS = -I/opt/homebrew/include -I /opt/homebrew/Cellar/hdf5-mpi/1.14.6/include -I/opt/homebrew/opt/zlib/include
ZLIB_LIBS =   -L/opt/homebrew/opt/zlib/lib -lz 
#
# libpng settings
#
PNG_CFLAGS = -I/opt/homebrew/opt/libpng/include/libpng16
PNG_LIBS =    -L/opt/homebrew/opt/libpng/lib -lpng16
#
# BLAS settings
#
BLAS_LIBS =   -lblas
#
# FFTW3 settings
#
FFTW3_CFLAGS =   -I/opt/homebrew/Cellar/fftw/3.3.10_2/include
FFTW3_LIBS =     -L/opt/homebrew/Cellar/fftw/3.3.10_2/lib -lfftw3
FFTW3_OMP_LIBS = 
#
# HDF5 settings
#
HDF5_FFLAGS = -I/opt/homebrew/lib -L/opt/homebrew/lib -I/opt/homebrew/include 
HDF5_LDFLAGS = -L/opt/homebrew/lib 
HDF5_FLIBS = -L/opt/homebrew/lib -lhdf5_fortran -lhdf5
#
# PGPLOT Libralies
#
PGPLOT_LIBS = 
#
#
# OpenGL settings
#
OPENGL_INC =  
OPENGL_LIBS = -framework OpenGL  -framework CoreVideo  -framework IOKit -framework Cocoa $(PNG_LIBS) $(ZLIB_LIBS) -framework accelerate
OPTFLAGS_GL = $(OPTFLAGS)  -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO -DGLFW3 -DFFMPEG $(OPENGL_INC) $(PNG_CFLAGS)  
#
# GTK settings
#
GTK3_CFLAGS =  -I/opt/homebrew/Cellar/gtk+3/3.24.50/include/gtk-3.0 -I/opt/homebrew/Cellar/pango/1.57.0/include/pango-1.0 -I/opt/homebrew/Cellar/harfbuzz/11.5.0/include/harfbuzz -I/opt/homebrew/Cellar/graphite2/1.3.14/include -I/opt/homebrew/Cellar/cairo/1.18.4/include -I/opt/homebrew/Cellar/cairo/1.18.4/include/cairo -I/opt/homebrew/Cellar/fontconfig/2.17.1/include -I/opt/homebrew/opt/freetype/include/freetype2 -I/opt/homebrew/Cellar/libxext/1.3.6/include -I/opt/homebrew/Cellar/xorgproto/2024.1/include -I/opt/homebrew/Cellar/libxrender/0.9.12/include -I/opt/homebrew/Cellar/libx11/1.8.12/include -I/opt/homebrew/Cellar/libxcb/1.17.0/include -I/opt/homebrew/Cellar/libxau/1.0.12/include -I/opt/homebrew/Cellar/libxdmcp/1.1.5/include -I/opt/homebrew/Cellar/pixman/0.46.4/include/pixman-1 -I/opt/homebrew/include/gdk-pixbuf-2.0 -I/opt/homebrew/opt/libpng/include/libpng16 -I/opt/homebrew/opt/libtiff/include -I/opt/homebrew/opt/zstd/include -I/opt/homebrew/Cellar/xz/5.8.1/include -I/opt/homebrew/opt/jpeg-turbo/include -I/opt/homebrew/Cellar/at-spi2-core/2.56.4/include/atk-1.0 -I/opt/homebrew/Cellar/libepoxy/1.5.10/include -I/opt/homebrew/Cellar/fribidi/1.0.16/include/fribidi -I/opt/homebrew/Cellar/glib/2.86.0/include/gio-unix-2.0 -I/opt/homebrew/Cellar/glib/2.86.0/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX15.sdk/usr/include/ffi -I/opt/homebrew/Cellar/glib/2.86.0/include/glib-2.0 -I/opt/homebrew/Cellar/glib/2.86.0/lib/glib-2.0/include -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/Cellar/pcre2/10.46/include 
GTK3_LIBS =    -L/opt/homebrew/Cellar/gtk+3/3.24.50/lib -lgtk-3 -lgdk-3 -Wl,-framework,Cocoa -Wl,-framework,Carbon -Wl,-framework,CoreGraphics -L/opt/homebrew/Cellar/pango/1.57.0/lib -lpangocairo-1.0 -lpango-1.0 -L/opt/homebrew/Cellar/harfbuzz/11.5.0/lib -lharfbuzz -L/opt/homebrew/Cellar/cairo/1.18.4/lib -lcairo-gobject -lcairo -L/opt/homebrew/lib -lgdk_pixbuf-2.0 -L/opt/homebrew/Cellar/at-spi2-core/2.56.4/lib -latk-1.0 -L/opt/homebrew/Cellar/glib/2.86.0/lib -lgio-2.0 -lgobject-2.0 -lglib-2.0 -L/opt/homebrew/opt/gettext/lib -lintl -L/usr/X11/lib -lX11 -lm
#
# GLFW settings
#
GLFW_CFLAGS =  -I/opt/homebrew/Cellar/glfw/3.4/include
GLFW_LIBS =    -L/opt/homebrew/Cellar/glfw/3.4/lib -lglfw
#
# FFMPEG settings
#
FFMPEG_CFLAGS =  -I/opt/homebrew/Cellar/ffmpeg/8.0_1/include
FFMPEG_LIBS =    -L/opt/homebrew/Cellar/ffmpeg/8.0_1/lib -lavformat -lavcodec -lswscale -lavutil
#
# -----------------------------------------------------------
# Directory lists
# -----------------------------------------------------------
#
SRCDIR = /Users/matsui/src_kemo
#
MAKEDIR = /Users/matsui/src_kemo/work
BUILDDIR = /Users/matsui/src_kemo/bin
MHDDIR = /Users/matsui/src_kemo/MHD
INSTDIR = /Users/matsui/local
PYTHONDIR= /Users/matsui/src_kemo/MHD/pythons
EXPDIR= /Users/matsui/src_kemo/examples
TESTDIR= /Users/matsui/src_kemo/tests
#
#
