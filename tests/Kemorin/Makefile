#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = ~/src_kemo
MAKEDIR = $(SRCDIR)/work
#
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
GMAKE=   make
MPIRUN= mpirun

TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =            $(BUILDDIR)/sph_snapshot
TARGET_SPH_LICV =            $(BUILDDIR)/sph_licv
TARGET_PSF_2_VTK =           $(BUILDDIR)/utilities/section_to_VTK
TARGET_COMPARE_PSF =         $(BUILDDIR)/tests/compare_psf
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_SPH_REF_CHECK =       $(BUILDDIR)/tests/sph_ref_field_check
#
TARGET_GAUSS_TEST =          $(BUILDDIR)/tests/gauss_points
TARGET_CMP_GAUSS_TEST =      $(BUILDDIR)/tests/compare_gauss_points
#
TARGET_FFTPACK_TEST =        $(BUILDDIR)/tests/test_FFTPACK
TARGET_FFW3_F_TEST =         $(BUILDDIR)/tests/test_FFTW3_f
TARGET_SGL_FFTW3_TEST =       $(BUILDDIR)/tests/test_single_FFTW3
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
	@echo '# -----------------------------------------------------------'
	@echo "# Tests in Mesh directory..."
	@echo '# -----------------------------------------------------------'
	@cd Mesh; \
		$(GMAKE) \
		BUILDDIR="$(BUILDDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		test
		
	@echo '# -----------------------------------------------------------'
	@echo "# Tests in Models directory..."
	@echo '# -----------------------------------------------------------'
	@cd Models; \
		$(GMAKE) \
		BUILDDIR="$(BUILDDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_LICV="$(TARGET_SPH_LICV)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test


clean:
	cd Mesh; make clean
	cd Models; make clean
	
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
