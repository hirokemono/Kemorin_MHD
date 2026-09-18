#
#
#

C_SRCDIR = $$(MHDDIR)/C_src

LIB_KEMO_C =       -lkemo_c
LIB_KEMO_C_FILE = libkemo_c.a

LIB_KEMO_GTK3 =     -lkemo_gtk3
LIB_KEMO_GTK3_FILE = libkemo_gtk3.a

LIB_KEMO_GTK =     -lkemo_gtk4
LIB_KEMO_GTK_FILE = libkemo_gtk4.a

OBJS_C=
OBJS_GTK3=
OBJS_GTK=

SUBDIRS =     \
CORE_C        \
CONTROLS

GTK3_DIRS =
GTK_DIRS =

ifdef OPENGL_LIBS
  SUBDIRS+=   \
    KEMO_GL   \
    GLSL      \
    
  GTK3_DIRS+= \
    GTK       \
    KEMO_GLUT

  GTK_DIRS+= \
    GTK4
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
		F90_LOCAL='$(F90_LOCAL)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_KEMO_C =         $(LIB_KEMO_C)'          >> $(MAKENAME)
	@echo  'LIB_KEMO_C_FILE =    $(LIB_KEMO_C_FILE)'     >> $(MAKENAME)
	@echo  'LIB_KEMO_GTK3 =      $(LIB_KEMO_GTK3)'       >> $(MAKENAME)
	@echo  'LIB_KEMO_GTK3_FILE = $(LIB_KEMO_GTK3_FILE)'  >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve lib_archve_gtk3
	@echo '#' >> $(MAKENAME)


libtarget:
	@echo  '$$(LIB_KEMO_C_FILE): $$(OBJS_C)' >> $(MAKENAME)

lib_archve: libtarget
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget_gtk3:
	@echo  '$$(LIB_KEMO_GTK3_FILE): $$(OBJS_GTK3)' >> $(MAKENAME)

lib_archve_gtk3: libtarget_gtk3
	@for dir in $(GTK3_DIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget_gtk:
	@echo  '$$(LIB_KEMO_GTK_FILE): $$(OBJS_GTK)' >> $(MAKENAME)

lib_archve_gtk: libtarget_gtk
	@for dir in $(GTK_DIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done


mod_list:
	@echo  'OBJS_C = $(OBJS_C)' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		ZLIB_LIBS='$(ZLIB_LIBS)'         \
		PNG_LIBS='$(PNG_LIBS)'           \
		GLFW_CFLAGS='$(GLFW_LIBS)'       \
		FFMPEG_CFLAGS='$(FFMPEG_CFLAGS)' \
		OBJS_LIST='OBJS_C'           \
		MAKENAME='$(MAKENAME)'           \
		mod_list \
		) \
	done
	@echo '' >> $(MAKENAME)
	@echo 'OBJS_GTK3 = $(OBJS_GTK3)' >> $(MAKENAME)
	@for dir in $(GTK3_DIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		ZLIB_LIBS='$(ZLIB_LIBS)'         \
		PNG_LIBS='$(PNG_LIBS)'           \
		GLFW_CFLAGS='$(GLFW_LIBS)'       \
		FFMPEG_CFLAGS='$(FFMPEG_CFLAGS)' \
		OBJS_LIST='OBJS_GTK3'        \
		MAKENAME='$(MAKENAME)'           \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS) $(GTK3_DIRS); do \
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
		GLFW_CFLAGS="$(GLFW_CFLAGS)" \
		FFMPEG_CFLAGS="$(FFMPEG_CFLAGS)" \
		depends \
		) \
	done

clean:
	@for dir in $(SUBDIRS) $(GTK3_DIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
