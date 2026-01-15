#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR= $(SRCDIR)/bin
#
GMAKE = make

MPIRUN= mpirun
TARGET_SPH_MHD =              $(BUILDDIR)/sph_mhd
TARGET_SPH_LICV =             $(BUILDDIR)/sph_licv
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(SRCDIR)/bin/tests/compare_psf
TARGET_SPH_REF_CHECK =       $(BUILDDIR)/tests/sph_ref_field_check
#
#  -------------------------------------------------------------------------
#

all: test

test: 
	make clean
	
	@echo "# Test in full_sphere_licv_z directory..."
	@cd full_sphere_licv_z; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_LICV="$(TARGET_SPH_LICV)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	
	@echo "# Test in full_sphere_rot_z directory..."
	@cd full_sphere_rot_z; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	
	@echo "# Test in static_latent_heat_by_ctl directory..."
	@cd static_latent_heat_by_ctl; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
		test
	
	@echo "# Test in full_sphere_v_filter_static directory..."
	@cd full_sphere_v_filter_static; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@rm -f *.dat *.txt

clean:
	cd full_sphere_licv_z; make clean
	cd full_sphere_rot_z; make clean
	cd static_latent_heat_by_ctl; make clean
	cd full_sphere_v_filter_static; make clean
	
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
