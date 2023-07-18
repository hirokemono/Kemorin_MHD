SHELL = /bin/sh

SRCDIR = /Users/matsui/src_kemo
WORKDIR = $(SRCDIR)/work
CC=gcc
F90=gfortran

F90OPTFLAGS = -O3 -Wall -g -fopenmp  $(F_INCLUDE)
F90CPPFLAGS =  -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO -DGLFW3
F90DLLFLAGS = -fPIC $(F90OPTFLAGS)
OPTFLAGS = -O3 -Wall -g  -D__APPLE__
CXXFLAGS =  -D__APPLE__

KEMO_INCLUDE = -I. -I./GTK \
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

GTK3_CFLAGS = $(shell pkg-config --cflags gtk+-3.0)
GTK3_LIBS =   $(shell pkg-config --libs gtk+-3.0)

OBJECTS = \
file_dialog.o \
control_block_panel_GTK.o \
ctl_data_platforms_GTK.o \
\
control_boxes_single_items_GTK.o \
control_panel_4_dimless_GTK.o \
control_panel_4_MHD_BCs_GTK.o \
control_panels_MHD_control_GTK.o \
control_panel_4_SGS_model_GTK.o \
control_panel_4_sph_monitor_GTK.o \
control_panel_cbox_real_GTK.o \
control_panel_cbox_cbox_GTK.o \
control_panel_cbox_GTK.o \
\
c_control_data_pvrs.o \
c_ctl_data_platforms.o \
c_ctl_data_MHD_BCs.o \
c_ctl_data_4_sph_monitor.o \
c_ctl_data_sph_monitor_arrays.o \
c_ctl_data_SGS_model.o


MODULES = \
read_mhd_control_4_c.o

all: control_GTK

control_GTK: $(OBJECTS) $(MODULES)
	$(F90) $(F90OPTFLAGS) $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	-o control_GTK $(OBJECTS) $(MODULES) $(KEMO_LIBS) $(GTK3_LIBS)


control_boxes_single_items_GTK.o: GTK/control_boxes_single_items_GTK.c GTK/control_boxes_single_items_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_boxes_single_items_GTK.c

control_block_panel_GTK.o: GTK/control_block_panel_GTK.c GTK/control_block_panel_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_block_panel_GTK.c

control_panel_4_dimless_GTK.o: GTK/control_panel_4_dimless_GTK.c GTK/control_panel_4_dimless_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_4_dimless_GTK.c

control_panel_cbox_GTK.o: GTK/control_panel_cbox_GTK.c GTK/control_panel_cbox_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_cbox_GTK.c

control_panel_cbox_cbox_GTK.o: GTK/control_panel_cbox_cbox_GTK.c GTK/control_panel_cbox_cbox_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_cbox_cbox_GTK.c

control_panel_cbox_real_GTK.o: GTK/control_panel_cbox_real_GTK.c GTK/control_panel_cbox_real_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_cbox_real_GTK.c

control_panel_4_MHD_BCs_GTK.o: GTK/control_panel_4_MHD_BCs_GTK.c GTK/control_panel_4_MHD_BCs_GTK.h GTK/c_ctl_data_MHD_BCs.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_4_MHD_BCs_GTK.c

control_panels_MHD_control_GTK.o: GTK/control_panels_MHD_control_GTK.c GTK/control_panels_MHD_control_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panels_MHD_control_GTK.c

control_panel_4_SGS_model_GTK.o: GTK/control_panel_4_SGS_model_GTK.c GTK/control_panel_4_SGS_model_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_4_SGS_model_GTK.c

c_ctl_data_SGS_model.o: GTK/c_ctl_data_SGS_model.c GTK/c_ctl_data_SGS_model.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_ctl_data_SGS_model.c

c_ctl_data_sph_monitor_arrays.o: GTK/c_ctl_data_sph_monitor_arrays.c GTK/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_ctl_data_sph_monitor_arrays.c

c_ctl_data_platforms.o: GTK/c_ctl_data_platforms.c GTK/c_ctl_data_platforms.h GTK/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_ctl_data_platforms.c

c_ctl_data_MHD_BCs.o: GTK/c_ctl_data_MHD_BCs.c GTK/c_ctl_data_MHD_BCs.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_ctl_data_MHD_BCs.c

c_ctl_data_4_sph_monitor.o: GTK/c_ctl_data_4_sph_monitor.c GTK/c_ctl_data_4_sph_monitor.h GTK/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_ctl_data_4_sph_monitor.c

c_control_data_pvrs.o: GTK/c_control_data_pvrs.c GTK/c_control_data_pvrs.h GTK/c_ctl_data_sph_monitor_arrays.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/c_control_data_pvrs.c

control_panel_4_sph_monitor_GTK.o: GTK/control_panel_4_sph_monitor_GTK.c GTK/control_panel_4_sph_monitor_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/GTK/control_panel_4_sph_monitor_GTK.c


ctl_data_platforms_GTK.o: ctl_data_platforms_GTK.c ctl_data_platforms_GTK.h GTK/control_boxes_single_items_GTK.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/ctl_data_platforms_GTK.c

file_dialog.o: file_dialog.c ctl_data_platforms_GTK.o GTK/control_panel_4_dimless_GTK.h GTK/control_panel_cbox_real_GTK.h GTK/control_panel_cbox_cbox_GTK.h GTK/control_panel_cbox_GTK.h GTK/control_panel_4_MHD_BCs_GTK.h GTK/control_panels_MHD_control_GTK.h GTK/control_panel_4_SGS_model_GTK.h GTK/control_block_panel_GTK.h GTK/c_ctl_data_platforms.h GTK/c_ctl_data_MHD_BCs.h GTK/c_ctl_data_4_sph_monitor.h GTK/control_panel_4_sph_monitor_GTK.h GTK/c_control_data_pvrs.h GTK/c_ctl_data_SGS_model.h
	$(CC) $(OPTFLAGS) -c  $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/file_dialog.c



read_mhd_control_4_c.o: $(SRCDIR)/MHD/programs/data_utilities/sph_spectr/read_mhd_control_4_c.f90 
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/data_utilities/sph_spectr/read_mhd_control_4_c.f90 

c_link_ctl_data_sph_shell.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_sph_shell.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_sph_shell.f90

c_link_ctl_data_divide_sph.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_divide_sph.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_divide_sph.f90

c_link_mhd_evo_scheme_ctl.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_mhd_evo_scheme_ctl.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_mhd_evo_scheme_ctl.f90

c_link_sph_monitor_control.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_sph_monitor_control.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_sph_monitor_control.f90

c_link_ctl_data_time_steps.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_time_steps.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_time_steps.f90

c_link_ctl_data_SGS_model.o: $(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_SGS_model.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/programs/TESTS/GTK3_test/c_link_ctl_data_SGS_model.f90

ctl_array_chara_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_chara_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_chara_to_c.f90

ctl_array_int_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_int_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_int_to_c.f90

ctl_array_real_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_real_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_array_real_to_c.f90

ctl_chara_int_array_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_chara_int_array_to_c.f90 ctl_array_chara_to_c.o
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_chara_int_array_to_c.f90

ctl_chara_real_array_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_chara_real_array_to_c.f90 ctl_array_chara_to_c.o
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_chara_real_array_to_c.f90

ctl_int_real_array_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_int_real_array_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_int_real_array_to_c.f90

ctl_int2_array_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_int2_array_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_int2_array_to_c.f90

ctl_data_platforms_to_c.o: $(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_data_platforms_to_c.f90
	$(F90) $(F90OPTFLAGS) -c $(KEMO_INCLUDE) $(GTK3_CFLAGS) \
	$(SRCDIR)/MHD/Fortran_src/Fortran2003/ctl_data_platforms_to_c.f90

clean:
	rm -f *.o *.mod *~ control_GTK
