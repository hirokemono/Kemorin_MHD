#! /bin/csh
#
#
# -------  do not edit folloing lines
#
# MPI settings
#
MPICHDIR = /usr/local/mpich
MPILIBDIR = $(MPICHDIR)/lib
MPILIBS = -mpich -fmpich
MPIBINDIR = $(MPICHDIR)/bin
MPIINCDIR = $(MPICHDIR)/include
#
#

BUILDDIR= 
MHDDIR =

MAKENAME= $(MAKEDIR)/Makefile

#
# FC_NAME_MANGLE definition for Fortran-C connection
#     FC_NAME_LOWER_USCORE, FC_NAME_UPPER, or FC_NAME_LOWER
#
FC_NAME_MANGLE = FC_NAME_LOWER_USCORE
#
#   zlib and libpng settings
#
ZLIB_CFLAGS =
ZLIB_LIBS =
#
PNG_CFLAGS =
PNG_LIBS = 
#
COCOA_FLAG= -DWITH_COCOA 
OPENGL_INC = 
OPENGL_LIBS= 
#
PGPLOT_LIBS= 

SUBDIRS =         \
Fortran_src   \
C_src         \
External_libs \
programs



F_INCLUDE = -I.
C_INCLUDE = -I.

ifdef FC_NAME_MANGLE
  CPP_FLAGS = -DCONNECT_TO_C

  ifdef PNG_LIBS
    CPP_FLAGS += -DPNG_OUTPUT
    C_INCLUDE+= $$(PNG_CFLAGS) 
    F90LIBS+= $$(PNG_LIBS)
  endif
  ifdef ZLIB_LIBS
    CPP_FLAGS += -DZLIB_IO
    C_INCLUDE+= $$(ZLIB_CFLAGS) 
    F90LIBS+= $$(ZLIB_LIBS)
  endif

  ifdef FFTW3_LIBS
    CPP_FLAGS+= -DFFTW3
    C_INCLUDE+= $$(FFTW3_CFLAGS)
    F90LIBS+= $$(FFTW3_LIBS)
  endif
endif

ifdef BLAS_LIBS
  CPP_FLAGS += -DBLAS
  F90LIBS+= $$(BLAS_LIBS)
endif

ifdef HDF5_FLIBS
  CPP_FLAGS += -DHDF5_IO
  F_INCLUDE+= $$(HDF5_FFLAGS)
  F90LIBS+= $$(HDF5_LDFLAGS) $$(HDF5_FLIBS)
endif

ifdef XL_FORTRAN
  FORTRAN_C_CONNECT = -WF,"$(CPP_FLAGS)"
else
  FORTRAN_C_CONNECT = $(CPP_FLAGS)
endif

ifdef OPENGL_LIBS
  OPENGL_LIBRARIES = $(OPENGL_LIBS) $$(PNG_LIBS) $$(ZLIB_LIBS)
  OPTFLAGS_GL=  $$(OPTFLAGS) $(CPP_FLAGS) $$(OPENGL_INC) $$(PNG_CFLAGS)
endif

ifdef MPILIBS
  F90LIBS+= -L$(MPILIBDIR) $(MPILIBS)
endif

#
#  -------------------------------------------------------------------------
#

.SUFFIXES: .o .f90

.f90.o:
	$(F90) -o $@ @<



all: fluid

fluid:
	echo "# Compiling start"; \
		cd work;  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		fluid


makemake:
	if [ ! -d $(MAKEDIR) ]; then \
		mkdir $(MAKEDIR); \
	fi
	if [ ! -d $(BUILDDIR) ]; then \
		mkdir $(BUILDDIR); \
	fi
	@echo # create module list
	echo '#' > $(MAKENAME)
	echo '# Makefile for Kemono Dynamo simulation' >> $(MAKENAME)
	echo '#' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'SHELL = /bin/sh' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '#  all:         compile programs for simulations' >> $(MAKENAME)
	@echo '#  utils_pg:    compile programs for 2d-drawing (PGPLOT is required)' >> $(MAKENAME)
	@echo '#  viewer_glut: compile programs for glut viewer (GLUT is required)' >> $(MAKENAME)
	@echo '#  viewer_glui: compile programs for glui viewer (GLUT & GLUI are required)' >> $(MAKENAME)
	@echo '#  viewer_gtk:  compile programs for gtk viewer  (GLUT & GTK are required)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Compiler commands' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo CC=$(MPICC)            >> $(MAKENAME)
	@echo F90=$(MPIF90)          >> $(MAKENAME)
	@echo CXX=$(CXX)             >> $(MAKENAME)
	@echo AR=$(AR)               >> $(MAKENAME)
	@echo RANLIB=$(RANLIB) >> $(MAKENAME)
	@echo F90_LOCAL=$(F90_LOCAL) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Compiler options' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'F90OPTFLAGS = $(F90OPTFLAGS)  $$(F_INCLUDE)' >> $(MAKENAME)
	@echo 'F90CPPFLAGS = $(FORTRAN_C_CONNECT)'          >> $(MAKENAME)
	@echo 'OPTFLAGS = $(OPTFLAGS) $(FC_NAME_MANGLE) $(COCOA_FLAG)' >> $(MAKENAME)
	@echo 'CXXFLAGS = $(CXXFLAGS) $(COCOA_FLAG)'                   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'F90FLAGS = $$(F90OPTFLAGS) $$(F90CPPFLAGS)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Libralies and includes settting' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'F90LIBS =   $(F90LIBS)'   >> $(MAKENAME)
	@echo 'F_INCLUDE = $(F_INCLUDE)' >> $(MAKENAME)
	@echo 'C_INCLUDE = $(C_INCLUDE)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# zlib settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'ZLIB_CFLAGS = $(ZLIB_CFLAGS)' >> $(MAKENAME)
	@echo 'ZLIB_LIBS =   $(ZLIB_LIBS)'   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# libpng settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'PNG_CFLAGS = $(PNG_CFLAGS)'   >> $(MAKENAME)
	@echo 'PNG_LIBS =    $(PNG_LIBS)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# BLAS settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'BLAS_LIBS =   $(BLAS_LIBS)'   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# FFTW3 settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'FFTW3_CFLAGS = $(FFTW3_CFLAGS)'                  >> $(MAKENAME)
	@echo 'FFTW3_LIBS =   $(FFTW3_LIBS)'                    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# HDF5 settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'HDF5_FFLAGS = $(HDF5_FFLAGS)'    >> $(MAKENAME)
	@echo 'HDF5_LDFLAGS = $(HDF5_LDFLAGS)'  >> $(MAKENAME)
	@echo 'HDF5_FLIBS = $(HDF5_FLIBS)'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# PGPLOT Libralies' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'PGPLOT_LIBS = $(PGPLOT_LIBS)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# OpenGL settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'OPENGL_INC =  $(OPENGL_INC)'  >> $(MAKENAME)
	@echo 'OPENGL_LIBS = $(OPENGL_LIBRARIES)' >> $(MAKENAME)
	@echo 'OPTFLAGS_GL = $(OPTFLAGS_GL)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# GTK settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'GTK3_CFLAGS =  $(GTK3_CFLAGS)'         >> $(MAKENAME)
	@echo 'GTK3_LIBS =    $(GTK3_LIBS)'           >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Directory lists' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo SRCDIR =   $(SRCDIR)  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo MAKEDIR =  $(MAKEDIR)  >> $(MAKENAME)
	@echo BUILDDIR = $(BUILDDIR) >> $(MAKENAME)
	@echo MHDDIR =   $(MHDDIR)   >> $(MAKENAME)
	@echo INSTDIR =  $(INSTDIR)  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	>> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create directory list...'
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'          \
		MAKENAME='$(MAKENAME)'        \
		64BIT_INT='$(64BIT_INT)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# target lists ' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'PROGRAMS= parallels utilities' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'ifdef OPENGL_LIBS' >> $(MAKENAME)
	@echo '  PROGRAMS+= viewer_glut' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '  ifdef CXX' >> $(MAKENAME)
	@echo '    PROGRAMS+= viewer_glui' >> $(MAKENAME)
	@echo '  endif' >> $(MAKENAME)
	@echo '  ifdef GTK3_CFLAGS' >> $(MAKENAME)
	@echo '    PROGRAMS+= viewer_gtk' >> $(MAKENAME)
	@echo '  endif' >> $(MAKENAME)
	@echo 'endif' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'ifdef PGPLOT_LIBS' >> $(MAKENAME)
	@echo '  PROGRAMS+= utils_pg' >> $(MAKENAME)
	@echo 'endif' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create excecute file list...'
	@for dir in programs ; do \
		( \
		echo 'generate target list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# library lists' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create library file name list...'
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_name; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# objects list' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create object module list...'
	@for dir in $(SUBDIRS) ; do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		ZLIB_LIBS='$(ZLIB_LIBS)'     \
		PNG_LIBS='$(PNG_LIBS)'       \
		FFTW3_LIBS='$(FFTW3_LIBS)'   \
        FFTW3_C='$(FFTW3_C)'   \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		mod_list; \
		 ) \
	done
	@echo '' >> $(MAKENAME)
	@echo 'create excecution construct list...'
	@echo '#' >> $(MAKENAME)
	@echo 'all: $$(PROGRAMS)' >> $(MAKENAME)
	@echo 'tests: tests_with_f' >> $(MAKENAME)
	@echo 'tests_c: tests_with_c' >> $(MAKENAME)
	@for dir in programs ; do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		target_task \
		) \
	done
	@echo 'create library construct list...'
	@echo '#' >> $(MAKENAME)
	@echo '# Rules for libralies' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		OPENGL_LIBS='$(OPENGL_LIBS)' \
		GTK3_LIBS='$(GTK3_LIBS)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_tasks; \
		 ) \
	done
	@echo '' >> $(MAKENAME)
	@echo 'create objects construct list...'
	@echo '#' >> $(MAKENAME)
	@echo '# Rules for routines' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate compile commands for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		module; \
		) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# Rules for includes' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'copy C include files for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		includes; \
		) \
	done
	@echo '' >> $(MAKENAME)
	@echo 'install:' >> $(MAKENAME)
	@echo '	if [ ! -d $$(INSTDIR) ]; then \'       >> $(MAKENAME)
	@echo '		mkdir $$(INSTDIR); \'              >> $(MAKENAME)
	@echo '	fi'                                   >> $(MAKENAME)
	@echo '	if [ ! -d $$(INSTDIR)/lib ]; then \'   >> $(MAKENAME)
	@echo '		mkdir $$(INSTDIR)/lib ; \'         >> $(MAKENAME)
	@echo '	fi'                                     >> $(MAKENAME)
	@echo '	if [ ! -d $$(INSTDIR)/bin ]; then \'     >> $(MAKENAME)
	@echo '		mkdir $$(INSTDIR)/bin; \'            >> $(MAKENAME)
	@echo '	fi'                                     >> $(MAKENAME)
	@echo '	if [ ! -d $$(INSTDIR)/include ]; then \' >> $(MAKENAME)
	@echo '		mkdir $$(INSTDIR)/include; \'        >> $(MAKENAME)
	@echo '	fi'                                     >> $(MAKENAME)
	@echo '	cp $(BUILDDIR)/*  $$(INSTDIR)/bin' >> $(MAKENAME)
#	@echo '	cp *.a  $$(INSTDIR)/lib'           >> $(MAKENAME)
#	@echo '	cp kemoviewer.h   $$(INSTDIR)/include' >> $(MAKENAME)
#	@echo '	cp *.mod  $$(INSTDIR)/include'     >> $(MAKENAME)
	@echo ''       >> $(MAKENAME)
	@echo 'clean:' >> $(MAKENAME)
	@echo '	@rm -f m_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f m_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f m_*~ ;'      >> $(MAKENAME)
	@echo '	@rm -f m_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f m_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f s_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f s_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f s_*~ ;'      >> $(MAKENAME)
	@echo '	@rm -f s_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f s_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f t_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f t_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f t_*~ ;'      >> $(MAKENAME)
	@echo '	@rm -f t_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f t_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f cal*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*~ ;'     >> $(MAKENAME)
	@echo '	@rm -f cal*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f set*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f set*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f set*~ ;'     >> $(MAKENAME)
	@echo '	@rm -f set*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f set*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f sph*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f sph*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f sph*~ ;'     >> $(MAKENAME)
	@echo '	@rm -f sph*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f sph*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f f*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f f*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f f*~ ;'       >> $(MAKENAME)
	@echo '	@rm -f f*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f f*.diag ;'   >> $(MAKENAME)
	@echo '	@rm -f i*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f i*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f i*~ ;'       >> $(MAKENAME)
	@echo '	@rm -f i*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f c*.diag ;'   >> $(MAKENAME)
	@echo '	@rm -f c*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f c*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f c*~ ;'       >> $(MAKENAME)
	@echo '	@rm -f c*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f c*.diag ;'   >> $(MAKENAME)
	@echo '	rm -f *.o ;'       >> $(MAKENAME)
	@echo '	rm -f *.mod ;'     >> $(MAKENAME)
	@echo '	rm -f *~ ;'        >> $(MAKENAME)
	@echo '	rm -f *.par ;'     >> $(MAKENAME)
	@echo '	rm -f *.diag ;'    >> $(MAKENAME)
	@echo '	rm -f *.a *.L'     >> $(MAKENAME)
	@echo '	rm -f *.h'         >> $(MAKENAME)
	@echo '	rm -f GL'          >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

install:
	if [ ! -d $$(INSTDIR) ]; then \
		mkdir $$(INSTDIR); \
	fi
	if [ ! -d $$(INSTDIR)/lib ]; then \
		mkdir $$(INSTDIR)/lib ; \
	fi
	if [ ! -d $$(INSTDIR)/bin ]; then \
		mkdir $$(INSTDIR)/bin; \
	fi
	if [ ! -d $$(INSTDIR)/include ]; then \
		mkdir $$(INSTDIR)/include; \
	fi
	@for dir in geoutil solver MHD surface marge_fluid; do \
	 (cd $${dir}; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		INSTDIR=$$(INSTDIR) RANLIB=$(RANLIB) \
		install; \
		) \
	done

clean:
	rm -f *~

distclean:
	rm -rf $(MAKEDIR) $(BUILDDIR)

