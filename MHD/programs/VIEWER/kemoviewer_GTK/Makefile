#
#
#

TARGET_KEMOVIEW_GTK = kemoviewer_GTK

MOD_KEMOVIEW_GTK = \
kemo_mesh_viewer_gtk.o \
kemoview_fileselector_gtk.o \
kemoview_gtk_window_input.o \
kemoviewer_gtk.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'KEMOVIEW_GTKDIR =  $$(KEMOVIEWER_DIR)/kemoviewer_GTK'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_KEMOVIEW_GTK = $$(BUILDDIR)/$(TARGET_KEMOVIEW_GTK)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo viewer_gtk: '$$(TARGET_KEMOVIEW_GTK)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GTK)': '$$(MOD_KEMOVIEW_GTK)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK2_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GTK)' \
	'$$(MOD_KEMOVIEW_GTK) -L. $$(LIB_KEMO_C)' \
	'$$(GTK2_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_KEMOVIEW_GTK =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GTK) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemoviewer_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoviewer_gtk.c  \
	kemo_mesh_viewer_gtk.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK2_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoviewer_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemo_mesh_viewer_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_gtk.c  \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_window_input.o \
	kemoview_glut_routines.o \
	const_viewer_menu_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK2_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_fileselector_gtk.c  \
	kemoview_glut_routines.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK2_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_fileselector_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_window_input.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_gtk_window_input.c  \
	kemoview_glut_routines.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK2_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_gtk_window_input.c \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMOVIEW_GTKDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMOVIEW_GTKDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o *~ *.mod *.diag *.L
