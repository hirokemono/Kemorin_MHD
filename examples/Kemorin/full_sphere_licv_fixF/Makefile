#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /Users/matsui/src_kemo
BUILDDIR= $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work

include $(MAKEDIR)/Makefile.header

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = sph_initial_field
#
TARGET_SPH_LICV =            $(BUILDDIR)/sph_licv
#
GMAKE = make
MPIRUN = mpirun
NP = 8


all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< -L$(MAKEDIR) $(LIBS_SPH_MHD) $(F90LIBS)

test: dataclean $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_LICV)


dataclean:
	rm -f *~ *.txt sph_ave_volume_s.dat picked_mode_l0_m0c.dat
	rm -f radial_point.dat time_total.dat
	rm -f rst_1/rst.*

clean: dataclean
	rm -fr $(TARGET_SPH_INITIAL) $(TARGET_SPH_INITIAL).dSYM

distclean: clean
