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
BUILDDIR = $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work

include $(MAKEDIR)/Makefile.header

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = $(CURRENTDIR)/sph_initial_field

#
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd

TARGET_SPH_ENE_CHECK =       $(CURRENTDIR)/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(CURRENTDIR)/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(CURRENTDIR)/compare_psf

TARGETS= \
$(TARGET_SPH_ENE_CHECK) \
$(TARGET_SPH_PICK_MODE_CHECK) \
$(TARGET_COMPARE_PSF)
#
GMAKE = make
MPIRUN = mpirun
NP = 6

all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< -L$(MAKEDIR) $(LIBS_SPH_MHD) $(F90LIBS)

targets: $(TARGET_SPH_INITIAL)
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		TARGET_SPH_PICK_MODE_CHECK='$(TARGET_SPH_PICK_MODE_CHECK)' \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		$(TARGETS)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
	@echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_s  monitor/sph_pwr_volume_s  >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed volume mean square data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_m0 monitor/sph_pwr_volume_m0 >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed axisymmetric mean square data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l1_m0c  monitor/picked_mode_l1_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{0}^{0} data check'; \
		exit 1;\
	fi
	
	$(TARGET_COMPARE_PSF)
	@echo 'Static solid body rotation is finished'


dataclean:
	rm -f *~ *.txt
	rm -f radial_point.dat time_total.dat
	rm -f monitor/[a-q,s-z,A-Q,S-Z]*
	rm -f sections/[a-q,s-z,A-Q,S-Z]*
	rm -f rst_6/rst.*

clean: dataclean
	rm -fr $(TARGET_COMPARE_PSF)*
	rm -fr $(TARGET_SPH_ENE_CHECK)*
	rm -fr $(TARGET_SPH_PICK_MODE_CHECK)*
	rm -fr $(TARGET_SPH_INITIAL)*

distclean: clean
