SHELL = /bin/sh

SRCDIR = /Users/matsui/src_kemo
WORKDIR = $(SRCDIR)/work

TESTDIR = $(SRCDIR)/MHD/programs/TESTS/GTK4_test
GTK3_DIR = $(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK


CC=gcc
F90=gfortran

F90OPTFLAGS = -O3 -Wall -g -fopenmp  $(F_INCLUDE)
F90CPPFLAGS =  -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO -DGLFW3
F90DLLFLAGS = -fPIC $(F90OPTFLAGS)
OPTFLAGS = -O3 -Wall -g  -D__APPLE__
CXXFLAGS =  -D__APPLE__

KEMO_INCLUDE = -I. -I$(GTK3_DIR) \
-I$(WORKDIR) \
-I$(SRCDIR)/MHD/C_src/CORE_C \
-I$(SRCDIR)/MHD/C_src/GTK \
-I$(SRCDIR)/MHD/C_src/CONTROLS

KEMO_LIBS = \
-L/Users/matsui/src_kemo/MHD/pythons \
-lcalypso_to_pythons \
-L/Users/matsui/src_kemo/work \
-lkemo_core \
-lkemo_c

GTK4_CFLAGS = $(shell pkg-config --cflags gtk4)
GTK4_LIBS =   $(shell pkg-config --libs gtk4)

OBJECTS = \
GTK4_test.o \
\
c_control_data_pvrs.o \
c_ctl_data_4_sph_monitor.o \
c_ctl_data_sph_monitor_arrays.o \
c_ctl_data_SGS_model.o \
c_ctl_data_platforms.o


MODULES = \
read_mhd_control_4_c.o

all: control_GTK

control_GTK: $(OBJECTS) $(MODULES)
	$(F90) $(F90OPTFLAGS) $(KEMO_INCLUDE) $(GTK4_CFLAGS) \
	-o control_GTK $(OBJECTS) $(MODULES) $(KEMO_LIBS) $(GTK4_LIBS)

GTK4_test.o: GTK4_test.c $(GTK3_DIR)/c_control_data_pvrs.h $(GTK3_DIR)/c_ctl_data_4_sph_monitor.h $(GTK3_DIR)/c_ctl_data_sph_monitor_arrays.h $(GTK3_DIR)/c_ctl_data_SGS_model.h $(GTK3_DIR)/c_ctl_data_platforms.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK4_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK4_test/GTK4_test.c



read_mhd_control_4_c.o: $(SRCDIR)/MHD/programs/data_utilities/sph_spectr/read_mhd_control_4_c.f90 
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK4_CFLAGS) \
	$(SRCDIR)/MHD/programs/data_utilities/sph_spectr/read_mhd_control_4_c.f90 


c_ctl_data_platforms.o: $(GTK3_DIR)/c_ctl_data_platforms.c $(GTK3_DIR)/c_ctl_data_platforms.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(GTK3_DIR)/c_ctl_data_platforms.c

c_ctl_data_SGS_model.o: $(GTK3_DIR)/c_ctl_data_SGS_model.c $(GTK3_DIR)/c_ctl_data_SGS_model.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(GTK3_DIR)/c_ctl_data_SGS_model.c

c_ctl_data_sph_monitor_arrays.o: $(GTK3_DIR)/c_ctl_data_sph_monitor_arrays.c $(GTK3_DIR)/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(GTK3_DIR)/c_ctl_data_sph_monitor_arrays.c

c_ctl_data_4_sph_monitor.o: $(GTK3_DIR)/c_ctl_data_4_sph_monitor.c $(GTK3_DIR)/c_ctl_data_4_sph_monitor.h $(GTK3_DIR)/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(GTK3_DIR)/c_ctl_data_4_sph_monitor.c

c_control_data_pvrs.o: $(GTK3_DIR)/c_control_data_pvrs.c $(GTK3_DIR)/c_control_data_pvrs.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(GTK3_DIR)/c_control_data_pvrs.c




clean:
	rm -f *.o *.mod *~ control_GTK
