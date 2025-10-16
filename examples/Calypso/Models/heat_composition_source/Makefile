#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
# Makefile for initial field construction
#
SHELL           = /bin/sh
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
WORKDIR =  $(SRCDIR)/work
#
#  Set MPI fortran compiler
#
F90 =             mpif90
F_INCLUDES =     -I$(WORKDIR)
OPENMP_FCFLAGS = -fopenmp
F90OPTFLAGS = -O3 -Wall -g $(OPENMP_FCFLAGS) $(F_INCLUDES)

FFTW3_LIBS= -L/opt/homebrew/Cellar/fftw/3.3.10_2/lib -lfftw3
ZLIB_LIBS = -L/opt/homebrew/opt/zlib/lib -lz 
HDF5_FLIBS= -L/opt/homebrew/lib -lhdf5_fortran -lhdf5

KEMO_LIBS = -L/Users/matsui/src_kemo/work \
           -lkemo_MHD \
           -lkemo_viz \
           -lkemo_parallel \
           -lkemo_core \
           -lkemo_solver \
           -lkemo_c \
           -lfftpack.5d \
           -lispack3_fft \
           -lispack_fft

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
	$(F90) $(F90OPTFLAGS) -o $@ $< $(KEMO_LIBS) $(ZLIB_LIBS) $(FFTW3_LIBS) $(HDF5_FLIBS)

test: $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 1 $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 1 $(TARGET_SPH_MHD)


clean:
	rm -f *.dat *.txt *~ rst_6/rst.* monitor/*.dat monitor/*.dat.gz sections/*.gz field/out.*
	rm -fr $(TARGET_SPH_INITIAL) $(TARGET_SPH_INITIAL).dSYM

distclean: clean
