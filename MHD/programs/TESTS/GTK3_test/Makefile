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

GTK3_CFLAGS = $(shell pkg-config --cflags gtk+-3.0)
GTK3_LIBS =   $(shell pkg-config --libs gtk+-3.0)

MODULES = \
read_mhd_control_4_c.o \
ctl_data_platforms_to_c.o

all: control_GTK

control_GTK: file_dialog.o ctl_data_platforms_GTK.o $(MODULES)
	$(F90) $(F90OPTFLAGS) $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	-o control_GTK file_dialog.o ctl_data_platforms_GTK.o $(MODULES) $(KEMO_LIBS) $(GTK3_LIBS)


ctl_data_platforms_GTK.o: ctl_data_platforms_GTK.c ctl_data_platforms_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/ctl_data_platforms_GTK.c

file_dialog.o: file_dialog.c ctl_data_platforms_GTK.o
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/file_dialog.c

read_mhd_control_4_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/read_mhd_control_4_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/read_mhd_control_4_c.f90

ctl_data_platforms_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_data_platforms_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_data_platforms_to_c.f90

clean:
	rm -f *.o *.mod *~