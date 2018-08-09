#
#
#

C_SRCDIR = $$(MHDDIR)/C_src

LIB_KEMO_C =       -lkemo_c
LIB_KEMO_C_FILE = libkemo_c.a

OBJS_C= \
$$(OBJ_KEMO_C) \
$$(OBJ_KEMO_CTL_C)

LIBDIRS = \
CORE_C \
CONTROLS

SUBDIRS = \
CORE_C    \
CONTROLS  \
KEMO_GL   \
KEMO_GLUT

ifdef OPENGL_LIBS
  OBJS_C+= \
    $$(OBJ_KEMO_GL_C) \
    $$(OBJ_GL2PS_C)
  LIBDIRS+=   \
    KEMO_GL   \
    KEMO_GLUT
endif


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'C_SRCDIR =     $(C_SRCDIR)'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_KEMO_C =      $(LIB_KEMO_C)'       >> $(MAKENAME)
	@echo  'LIB_KEMO_C_FILE = $(LIB_KEMO_C_FILE)'  >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

libtarget:
	@echo  '$$(LIB_KEMO_C_FILE): $(OBJS_C)' >> $(MAKENAME)

lib_archve:
	@for dir in $(LIBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		ZLIB_LIBS='$(ZLIB_LIBS)'     \
		PNG_LIBS='$(PNG_LIBS)'       \
		FFTW3_C='$(FFTW3_C)'   \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
		) \
	done

includes:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate symbolic link commands for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		includes \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
