#! /bin/csh
#
#
# -------  do not edit folloing lines
#

BUILDDIR= 
MHDDIR =

MAKENAME= $(MAKEDIR)/Makefile

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

SUBDIRS =         \
C_src         \
programs


C_INCLUDE = -I.

CPP_FLAGS =
C_INCLUDE += -I$(MHDDIR)/C_src/CORE_C
C_INCLUDE += -I$(MHDDIR)/C_src/CONTROLS

ifdef PNG_LIBS
  CPP_FLAGS += -DPNG_OUTPUT
  C_INCLUDE+= $$(PNG_CFLAGS) 
endif
ifdef ZLIB_LIBS
  CPP_FLAGS += -DZLIB_IO
  C_INCLUDE+= $$(ZLIB_CFLAGS) 
endif

ifdef OPENGL_LIBS
  C_INCLUDE += -I$(MHDDIR)/C_src/KEMO_GL
  C_INCLUDE += -I$(MHDDIR)/C_src/GLSL
  C_INCLUDE += -I$(MHDDIR)/C_src/GTK
  C_INCLUDE += -I$(MHDDIR)/C_src/KEMO_GLUT

  ifdef GLFW_LIBS
    CPP_FLAGS+= -DGLFW3
  endif

  OPENGL_LIBRARIES = $(OPENGL_LIBS) $$(PNG_LIBS) $$(ZLIB_LIBS)
  OPTFLAGS_GL=  $$(OPTFLAGS) $(CPP_FLAGS) $$(OPENGL_INC) $$(PNG_CFLAGS)  
endif

DEPFLAGS= -MM -w -DDEPENDENCY_CHECK

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

depends:
	if [ ! -d $(MAKEDIR) ]; then \
		mkdir $(MAKEDIR); \
	fi
	if [ ! -d $(BUILDDIR) ]; then \
		mkdir $(BUILDDIR); \
	fi
	@for dir in $(SUBDIRS) ; do \
		( \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MHDDIR='$(MHDDIR)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		F90_LOCAL='$(F90_LOCAL)'   \
		CC="$(CC)"                 \
		DEPFLAGS='$(DEPFLAGS)' \
		OPTFLAGS_GL='$(OPTFLAGS_GL)' \
		C_INCLUDE='$(C_INCLUDE)' \
		GTK3_CFLAGS='$(GTK3_CFLAGS)' \
		GLFW_CFLAGS='$(GLFW_CFLAGS)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends; \
		) \
	done


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
	@echo '#' >> $(MAKENAME)
	@echo '# Compiler commands' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo CC=$(CC)               >> $(MAKENAME)
	@echo CXX=$(CXX)             >> $(MAKENAME)
	@echo AR=$(AR)               >> $(MAKENAME)
	@echo RANLIB=$(RANLIB) >> $(MAKENAME)
	@echo F90_LOCAL=$(F90_LOCAL) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Compiler options' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'OPTFLAGS = $(OPTFLAGS) $(COCOA_FLAG)' >> $(MAKENAME)
	@echo 'CXXFLAGS = $(CXXFLAGS) $(COCOA_FLAG)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# Libralies and includes settting' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
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
	@echo '#' >> $(MAKENAME)
	@echo '# OpenGL settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'OPENGL_INC =  $(OPENGL_INC)'  >> $(MAKENAME)
	@echo 'OPENGL_LIBS = $(OPENGL_LIBRARIES)' >> $(MAKENAME)
	@echo 'OPTFLAGS_GL = $(OPTFLAGS_GL)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# GTK settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'GTK3_CFLAGS =  $(GTK3_CFLAGS) $(X_CFLAGS)' >> $(MAKENAME)
	@echo 'GTK3_LIBS =    $(GTK3_LIBS) $(X_LIBS)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# GLFW settings' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'GLFW_CFLAGS =  $(GLFW_CFLAGS)'         >> $(MAKENAME)
	@echo 'GLFW_LIBS =    $(GLFW_LIBS)'           >> $(MAKENAME)
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
		F90_LOCAL='$(F90_LOCAL)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# -----------------------------------------------------------' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '# target lists ' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'PROGRAMS= viewer_glfw' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'ifdef OPENGL_LIBS' >> $(MAKENAME)
	@echo '  ifdef GTK3_LIBS' >> $(MAKENAME)
	@echo '    ifdef GLFW_LIBS' >> $(MAKENAME)
	@echo '      PROGRAMS+= viewer_glfw' >> $(MAKENAME)
	@echo '    endif' >> $(MAKENAME)
	@echo '  endif' >> $(MAKENAME)
	@echo 'endif' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
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
		GLFW_LIBS='$(GLFW_LIBS)'   \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		 ) \
	done
	@echo '' >> $(MAKENAME)
	@echo 'create excecution construct list...'
	@echo '#' >> $(MAKENAME)
	@echo 'all: $$(PROGRAMS)' >> $(MAKENAME)
	@for dir in programs ; do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
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
		module; \
		) \
	done
	@echo '#' >> $(MAKENAME)
	@echo '# Rules for install' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
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
	@echo '	rm -f *.o ;'       >> $(MAKENAME)
	@echo '	rm -f *~ ;'        >> $(MAKENAME)
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

