#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#

SRCDIR = ~/src_kemo
MAKEDIR = $(SRCDIR)/work

include $(MAKEDIR)/Makefile.header

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = sph_initial_field

#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_LICV =            $(BUILDDIR)/sph_licv
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
#

all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< -L$(MAKEDIR) $(LIBS_SPH_MHD) $(F90LIBS)

test:
	make clean
	make $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 8 $(TARGET_SPH_INITIAL)


clean:
	rm -f *~ *.txt sph_ave_volume_s.dat picked_mode_l0_m0c.dat
	rm -f radial_point.dat time_total.dat
	rm -f rst_1/rst.*
	rm -fr $(TARGET_SPH_INITIAL) $(TARGET_SPH_INITIAL).dSYM

distclean: clean
