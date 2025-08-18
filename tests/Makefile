#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
SRCDIR = /Users/matsui/src_kemo
#
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
GMAKE=   make
MPIRUN= mpirun

TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =            $(BUILDDIR)/sph_snapshot
TARGET_PSF_2_VTK =           $(BUILDDIR)/utilities/section_to_VTK
TARGET_COMPARE_PSF =         $(BUILDDIR)/tests/compare_psf
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
#
TARGET_GAUSS_TEST =          $(BUILDDIR)/tests/gauss_points
TARGET_CMP_GAUSS_TEST =      $(BUILDDIR)/tests/compare_gauss_points
#
TARGET_FFTPACK_TEST =        $(BUILDDIR)/tests/test_FFTPACK
TARGET_FFW3_F_TEST =         $(BUILDDIR)/tests/test_FFTW3_f
TARGET_SGL_FFW3_TEST =       $(BUILDDIR)/tests/test_single_FFTW3
TARGET_CMPR_FFT_TEST =       $(BUILDDIR)/tests/compare_fft_test

TARGET_GEN_SPH_GRID =        $(BUILDDIR)/utilities/gen_sph_grids
TARGET_CHECK_SPH_GRID =      $(BUILDDIR)/tests/check_sph_grids
TARGET_TEST_SPH_GRID =       $(BUILDDIR)/tests/check_sph_comms

TARGET_NEWSPH=               $(BUILDDIR)/utilities/assemble_sph
TARGET_CMP_SPH_RST=          $(BUILDDIR)/tests/compare_sph_restart

#
#  -------------------------------------------------------------------------
#

all: test

test: 
	@echo "# Test in Calypso directory..."
	@cd Calypso; $(GMAKE) GMAKE="$(GMAKE)" BUILDDIR="$(BUILDDIR)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_GAUSS_TEST="$(TARGET_GAUSS_TEST)" \
		TARGET_CMP_GAUSS_TEST="$(TARGET_CMP_GAUSS_TEST)" \
		TARGET_FFTPACK_TEST="$(TARGET_FFTPACK_TEST)" \
		TARGET_FFW3_F_TEST="$(TARGET_FFW3_F_TEST)" \
		TARGET_SGL_FFW3_TEST="$(TARGET_SGL_FFW3_TEST)" \
		TARGET_CMPR_FFT_TEST="$(TARGET_CMPR_FFT_TEST)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test in Kemorin directory..."
	@cd Kemorin; $(GMAKE) GMAKE="$(GMAKE)" BUILDDIR="$(BUILDDIR)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_GAUSS_TEST="$(TARGET_GAUSS_TEST)" \
		TARGET_CMP_GAUSS_TEST="$(TARGET_CMP_GAUSS_TEST)" \
		TARGET_FFTPACK_TEST="$(TARGET_FFTPACK_TEST)" \
		TARGET_FFW3_F_TEST="$(TARGET_FFW3_F_TEST)" \
		TARGET_SGL_FFW3_TEST="$(TARGET_SGL_FFW3_TEST)" \
		TARGET_CMPR_FFT_TEST="$(TARGET_CMPR_FFT_TEST)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
		
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

sym_test: 
	for dir in sym_asym_field_test ; do \
	echo "# Test in $${dir} directory..."; \
		( cd $${dir}; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		BUILDDIR="$(BUILDDIR)" \
		TESTDIR="$(TESTDIR)" \
		MHDDIR="$(MHDDIR)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test)\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

clean:
	cd Calypso; make clean
	cd Kemorin; make clean
	
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
