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
MAKEDIR= 
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
ZLIB_LIB =
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
  FORTRAN_C_CONNECT = -DCONNECT_TO_C

  ifdef PNG_LIBS
    FORTRAN_C_CONNECT += -DPNG_OUTPUT
    C_INCLUDE+= $$(PNG_CFLAGS) 
    F90LIBS+= $$(PNG_LIBS)
  endif
  ifdef ZLIB_LIB
    FORTRAN_C_CONNECT += -DZLIB_IO
    C_INCLUDE+= $$(ZLIB_CFLAGS) 
    F90LIBS+= $$(ZLIB_LIB)
  endif

  ifdef FFTW3_LIBS
    FORTRAN_C_CONNECT+= -DFFTW3
    C_INCLUDE+= $$(FFTW3_CFLAGS)
    F90LIBS+= $$(FFTW3_LIBS)
  endif
endif

ifdef OPENGL_LIBS
  OPENGL_LIBRARIES = $(OPENGL_LIBS) $$(PNG_LIBS) $$(ZLIB_LIB)
  OPTFLAGS_GL=  $$(OPTFLAGS) $$(OPENGL_INC) $$(PNG_CFLAGS)
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
		make fluid


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
	@echo '#' >> $(MAKENAME)
	@echo '# Compiler options' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo F90OPTFLAGS = '$(F90OPTFLAGS)  $$(F_INCLUDE)' >> $(MAKENAME)
	@echo OPTFLAGS =    $(OPTFLAGS) $(FC_NAME_MANGLE) $(COCOA_FLAG) >> $(MAKENAME)
	@echo CXXFLAGS =    $(CXXFLAGS) $(COCOA_FLAG)                   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo F90FLAGS =     '$$(F90OPTFLAGS) $(FORTRAN_C_CONNECT)' >> $(MAKENAME)
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
	@echo 'ZLIB_CFLAGS = $(ZLIB_CFLAGS)'  >> $(MAKENAME)
	@echo 'ZLIB_LIB =    $(ZLIB_LIB)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# libpng settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'PNG_CFLAGS = $(PNG_CFLAGS)'  >> $(MAKENAME)
	@echo 'PNG_LIBS =    $(PNG_LIBS)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# FFTW3 settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'FFTW3_CFLAGS = $(FFTW3_CFLAGS)'                  >> $(MAKENAME)
	@echo 'FFTW3_LIBS =   $(FFTW3_LIBS)'                    >> $(MAKENAME)
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
	@echo 'GTK2_CFLAGS =  $(GTK2_CFLAGS)'         >> $(MAKENAME)
	@echo 'GTK2_LIBS =    $(GTK2_LIBS)'           >> $(MAKENAME)
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
		make dir_list \
		MAKEDIR='$(MAKEDIR)'          \
		MAKENAME='$(MAKENAME)'        \
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
	@echo '  ifdef GTK2_CFLAGS' >> $(MAKENAME)
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
		make target_list \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)'; \
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
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		make lib_name; \
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
		make mod_list \
		ZLIB_LIB='$(ZLIB_LIB)'       \
		PNG_LIBS='$(PNG_LIBS)'       \
		FFTW3_LIBS='$(FFTW3_LIBS)'   \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
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
		make \
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
		make lib_tasks  \
		OPENGL_LIBS='$(OPENGL_LIBS)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
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
		make module \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
		) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# Rules for includes' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'copy C include files for ' $$dir ; \
		cd $${dir};  \
		make includes \
		MAKENAME='$(MAKENAME)' \
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
	@echo '	cp *.a  $$(INSTDIR)/lib'           >> $(MAKENAME)
	@echo '	cp kemoviewer.h   $$(INSTDIR)/include' >> $(MAKENAME)
#	@echo '	cp *.mod  $$(INSTDIR)/include'     >> $(MAKENAME)
	@echo ''       >> $(MAKENAME)
	@echo 'clean:' >> $(MAKENAME)
	@echo '	@rm -f m_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f m_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f m_*.~ ;'     >> $(MAKENAME)
	@echo '	@rm -f m_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f m_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f s_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f s_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f s_*.~ ;'     >> $(MAKENAME)
	@echo '	@rm -f s_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f s_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f t_*.o ;'     >> $(MAKENAME)
	@echo '	@rm -f t_*.mod ;'   >> $(MAKENAME)
	@echo '	@rm -f t_*.~ ;'     >> $(MAKENAME)
	@echo '	@rm -f t_*.par ;'   >> $(MAKENAME)
	@echo '	@rm -f t_*.diag ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f cal*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*.~ ;'    >> $(MAKENAME)
	@echo '	@rm -f cal*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f cal*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f set*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f set*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f set*.~ ;'    >> $(MAKENAME)
	@echo '	@rm -f set*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f set*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f sph*.o ;'    >> $(MAKENAME)
	@echo '	@rm -f sph*.mod ;'  >> $(MAKENAME)
	@echo '	@rm -f sph*.~ ;'    >> $(MAKENAME)
	@echo '	@rm -f sph*.par ;'  >> $(MAKENAME)
	@echo '	@rm -f sph*.diag ;' >> $(MAKENAME)
	@echo '	@rm -f f*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f f*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f f*.~ ;'      >> $(MAKENAME)
	@echo '	@rm -f f*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f f*.diag ;'   >> $(MAKENAME)
	@echo '	@rm -f i*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f i*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f i*.~ ;'      >> $(MAKENAME)
	@echo '	@rm -f i*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f c*.diag ;'   >> $(MAKENAME)
	@echo '	@rm -f c*.o ;'      >> $(MAKENAME)
	@echo '	@rm -f c*.mod ;'    >> $(MAKENAME)
	@echo '	@rm -f c*.~ ;'      >> $(MAKENAME)
	@echo '	@rm -f c*.par ;'    >> $(MAKENAME)
	@echo '	@rm -f c*.diag ;'   >> $(MAKENAME)
	@echo '	rm -f *.o ;'       >> $(MAKENAME)
	@echo '	rm -f *.mod ;'     >> $(MAKENAME)
	@echo '	rm -f *.~ ;'       >> $(MAKENAME)
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
	 (cd $${dir}; make INSTDIR=$$(INSTDIR) RANLIB=$(RANLIB) install) \
	done

clean:
	rm -f *~
