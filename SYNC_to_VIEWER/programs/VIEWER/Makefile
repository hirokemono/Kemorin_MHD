#
#
#

KEMOVIEWER_DIR =    $$(PROG_DIR)/VIEWER

TARGET_KEMOVIEW_GLFW = CalypsoView_GLFW

SRC_C = $(shell ls *.c)

MOD_KEMOVIEW_GLFW = \
kemoviewer_glfw_gtk.o \
kemo_mesh_viewer_glfw_gtk.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'KEMOVIEWER_DIR = $(KEMOVIEWER_DIR)' >> $(MAKENAME)
	@echo 'KEMO_GTKDIR =  $$(C_SRCDIR)/GTK'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_KEMOVIEW_GLFW = $$(BUILDDIR)/$(TARGET_KEMOVIEW_GLFW)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo viewer_glfw: '$$(TARGET_KEMOVIEW_GLFW)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GLFW)': '$$(MOD_KEMOVIEW_GLFW)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GLFW)' \
	'$$(MOD_KEMOVIEW_GLFW) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(GLFW_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_KEMOVIEW_GLFW =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GLFW) >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS) $(C_INCLUDE) -I. $(MHDDIR)/programs/VIEWER/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)'@'$$\(MHDDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)//programs/VIEWER/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o *~ *.mod *.diag *.L
