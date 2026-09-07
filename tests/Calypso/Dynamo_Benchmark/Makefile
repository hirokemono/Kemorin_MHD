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
#
TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =      $(BUILDDIR)/sph_snapshot

TARGET_SPH_ENE_CHECK = $(CURRENTDIR)/sph_ene_check
TARGET_COMPARE_PSF =   $(CURRENTDIR)/compare_psf

TARGETS= \
$(TARGET_SPH_ENE_CHECK) \
$(TARGET_COMPARE_PSF)
#
GMAKE = make
MPIRUN = mpirun
#
#  -------------------------------------------------------------------------
#

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		$(TARGETS)

test: targets
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case0 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case0; \
		$(GMAKE) GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case1 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case1; \
		$(GMAKE) GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case2 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case2; \
		$(GMAKE) GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)" TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case2_cont directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case2_cont; \
		$(GMAKE) GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)" TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in snapshot_test directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd snapshot_test; \
		$(GMAKE) GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)" TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	
	rm -f *.o *.mod *~

clean:
	cd Dynamobench_Case0; make clean
	cd Dynamobench_Case1; make clean
	cd Dynamobench_Case2; make clean
	cd Dynamobench_Case2_cont; make clean
	cd snapshot_test; make clean
	
	rm -f *.o *.mod *~
	rm -fr $(TARGET_COMPARE_PSF)*
	rm -fr $(TARGET_SPH_ENE_CHECK)*

distclean: clean
