#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(SRCDIR)/bin/tests/compare_psf
#

all: test

test: 
	make clean
	
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
		echo 'Failed Y_{1}^{0} data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l6_m4c  monitor/picked_mode_l6_m4c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{6}^{4c} data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l12_m12c  monitor/picked_mode_l12_m12c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{12}^{12c} data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l16_m12c  monitor/picked_mode_l16_m12c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{16}^{12c} data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l24_m14s  monitor/picked_mode_l24_m14s >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{24}^{14s} data check'; \
		exit 1;\
	fi
	
	@echo 'Velocity filtering check in full sphere by static model is finished'

clean:
	rm -f *.dat *.txt rst_6/rst.* monitor/*.dat monitor/*.dat.gz sections/*.gz field/out.*

distclean: clean
