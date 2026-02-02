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

TARGET_GEN_SPH_GRID =    $(BUILDDIR)/utilities/gen_sph_grids
TARGET_CHECK_SPH_GRID =  $(BUILDDIR)/tests/check_sph_grids
TARGET_TEST_SPH_GRID =   $(BUILDDIR)/tests/check_sph_comms
#
NP = 6
MPIRUN= mpirun
MPI_GEN =   $(MPIRUN) -np $(NP) $(TARGET_GEN_SPH_GRID)
MPI_CHECK = $(MPIRUN) -np $(NP) $(TARGET_CHECK_SPH_GRID)
MPI_TEST =  $(MPIRUN) -np $(NP) $(TARGET_TEST_SPH_GRID)
#
#
#  -------------------------------------------------------------------------
#

all: test

test: 
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo '#   Test spherical harmonics indexining'
	@echo '#       in full_sphere directory...'
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd full_sphere; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo '#   Test spherical harmonics indexining '
	@echo '#       in full_sphere_w_external directory...'
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd full_sphere_w_external; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@rm -f *.dat *.txt

clean:
	cd full_sphere; make clean
	cd full_sphere_w_external; make clean
	
	rm -f reference/in.*
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
