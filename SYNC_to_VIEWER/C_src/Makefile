#
#
#

C_SRCDIR = $$(MHDDIR)/C_src

LIB_KEMO_C =       -lcalypso_viz
LIB_KEMO_C_FILE = libcalypso_viz.a

OBJS_C= \
$$(OBJ_KEMO_C) \
$$(OBJ_CTL_C)

SUBDIRS =     \
CORE_C        \
CONTROLS

OBJS_C+= \
  $$(OBJ_C_GL)     \
  $$(OBJ_SHADERS)  \
  $$(OBJ_GTK_C)

SUBDIRS+=   \
  KEMO_GL   \
  GLSL      \
  GTK       \
  KEMO_GLUT

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
		F90_LOCAL='$(F90_LOCAL)'   \
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
	@for dir in $(SUBDIRS); do \
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
		GLFW_CFLAGS='$(GLFW_LIBS)'   \
		FFMPEG_CFLAGS="$(FFMPEG_CFLAGS)" \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'Copy sourse dependencies for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		F90_LOCAL='$(F90_LOCAL)'   \
		CC='$(CC)'                 \
		MHDDIR='$(MHDDIR)' \
		MAKENAME='$(MAKENAME)' \
		DEPFLAGS='$(DEPFLAGS)' \
		OPTFLAGS_GL='$(OPTFLAGS_GL)' \
		C_INCLUDE='$(C_INCLUDE)' \
		GTK3_CFLAGS='$(GTK3_CFLAGS)' \
		GLFW_CFLAGS='$(GLFW_CFLAGS)' \
		FFMPEG_CFLAGS="$(FFMPEG_CFLAGS)" \
		depends \
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
