#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
# Makefile for initial field construction
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work
BUILDDIR= $(SRCDIR)/bin

include $(MAKEDIR)/Makefile.header

KEMO_LIBS = -L/Users/matsui/src_kemo/work \
           -lkemo_MHD \
           -lkemo_viz \
           -lkemo_parallel \
           -lkemo_core \
           -lkemo_solver \
           -lkemo_c \
           -lfftpack.5d \
           -lispack3_fft \
           -lispack_fft

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = sph_initial_field

#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd

TARGET_SPH_ENE_CHECK =       $(CURRENTDIR)/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(CURRENTDIR)/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(CURRENTDIR)/compare_psf

TARGETS= \
$(TARGET_SPH_ENE_CHECK) \
$(TARGET_SPH_PICK_MODE_CHECK) \
$(TARGET_COMPARE_PSF)
#
GMAKE= make
MPIRUN = mpirun
NP = 1


all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< $(KEMO_LIBS) $(F90LIBS)

targets: $(TARGET_SPH_INITIAL)
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		TARGET_SPH_PICK_MODE_CHECK='$(TARGET_SPH_PICK_MODE_CHECK)' \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		$(TARGETS)

test: targets
	echo $(CURRENTDIR)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)


clean:
	rm -f *.dat *.txt *~ 
	rm -f rst_1/rst.*
	rm -f monitor/*.dat monitor/*.dat.gz 
	rm -f field/out.*
	rm -fr $(TARGET_SPH_ENE_CHECK)*
	rm -fr $(TARGET_SPH_PICK_MODE_CHECK)*
	rm -fr $(TARGET_COMPARE_PSF)*
	rm -fr $(TARGET_SPH_INITIAL)*

distclean: clean
