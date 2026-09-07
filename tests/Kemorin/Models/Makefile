#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work
BUILDDIR= $(SRCDIR)/bin
#
GMAKE = make
MPIRUN= mpirun

TARGET_SPH_MHD =              $(BUILDDIR)/sph_mhd
TARGET_SPH_LICV =             $(BUILDDIR)/sph_licv
#
#  -------------------------------------------------------------------------
#

all: test

test: 
	make clean
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in full_sphere_licv_z directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd full_sphere_licv_z; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_LICV="$(TARGET_SPH_LICV)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in full_sphere_rot_z directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd full_sphere_rot_z; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in static_latent_heat_by_ctl directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd static_latent_heat_by_ctl; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in Solid_body_rotation_by_rst directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Solid_body_rotation_by_rst; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in full_sphere_v_filter_static directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd full_sphere_v_filter_static; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)" \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		test
	
	@rm -f *.dat *.txt

clean:
	cd full_sphere_licv_z; make clean
	cd full_sphere_rot_z; make clean
	cd static_latent_heat_by_ctl; make clean
	cd full_sphere_v_filter_static; make clean
	cd Solid_body_rotation_by_rst; make clean
	
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
