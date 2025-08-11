#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
#
MPIRUN = mpirun
TARGET_SPH_MHD =       $(SRCDIR)/bin/sph_mhd
TARGET_SPH_ENE_CHECK = $(SRCDIR)/bin/tests/sph_ene_check
#
#  -------------------------------------------------------------------------
#

all: test

test:
	@echo "# Test in Dynamobench_Case0 directory..."
	@cd Dynamobench_Case0; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo "# Test in Dynamobench_Case1 directory..."
	@cd Dynamobench_Case1; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	
	rm -f *.o *.mod *~

clean:
	cd Dynamobench_Case0; make clean
	cd Dynamobench_Case1; make clean
	
	rm -f *.o *.mod *~

distclean: clean
