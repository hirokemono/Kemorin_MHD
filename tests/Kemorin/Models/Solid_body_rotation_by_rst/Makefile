#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SRCDIR = /Users/matsui/src_kemo

include $(SRCDIR)/tests/Makefile.header

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = sph_initial_field

#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(SRCDIR)/bin/tests/compare_psf
#

all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< -L$(MAKEDIR) $(LIBS_SPH_MHD) $(F90LIBS)

test:
	make clean
	make $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 6 $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 6 $(TARGET_SPH_MHD)
	
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


clean:
	rm -f *~ *.txt
	rm -f radial_point.dat time_total.dat
	rm -f monitor/[a-q,s-z,A-Q,S-Z]*
	rm -f sections/[a-q,s-z,A-Q,S-Z]*
	rm -f rst_6/rst.*
	rm -fr $(TARGET_SPH_INITIAL) $(TARGET_SPH_INITIAL).dSYM

distclean: clean
