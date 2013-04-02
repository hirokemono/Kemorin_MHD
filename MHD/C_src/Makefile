#
#
#

C_SRCDIR = $$(MHDDIR)/C_src

LIB_KEMO_C =       -lkemo_c
LIB_KEMO_C_FILE = libkemo_c.a

OBJS_C= $$(OBJ_KEMO_C)
LIBDIRS = CORE_C

SUBDIRS = \
CORE_C    \
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
		make dir_list \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
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
		make \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		ZLIB_LIB='$(ZLIB_LIB)'       \
		PNG_LIBS='$(PNG_LIBS)'       \
		FFTW3_LIBS='$(FFTW3_LIBS)'   \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate compile commands for ' $$dir ; \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module \
		) \
	done

includes:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate symbolic link commands for ' $$dir ; \
		make \
		MAKENAME='$(MAKENAME)' \
		includes \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
