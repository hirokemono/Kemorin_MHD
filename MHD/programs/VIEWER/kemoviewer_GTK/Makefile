#
#
#

TARGET_KEMOVIEW_GTK = kemoviewer_GTK

MOD_KEMOVIEW_GTK = \
tree_views_4_fixed_lists_GTK.o \
tree_view_real2_GTK.o \
tree_view_4_pvr_colormap.o \
\
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
	@echo 'KEMO_GTKDIR =  $$(C_SRCDIR)/GTK'  >> $(MAKENAME)
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
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GTK)' \
	'$$(MOD_KEMOVIEW_GTK) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
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
	kemoviewer.h \
	kemo_mesh_viewer_gtk.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoviewer_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemo_mesh_viewer_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_gtk.c  \
	kemoviewer.h \
	kemo_mesh_viewer_gtk.h \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_window_input.o \
	kemoview_glut_routines.o \
	const_viewer_menu_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_fileselector_gtk.c  \
	kemoviewer.h \
	kemoview_fileselector_gtk.h \
	kemoview_glut_routines.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_fileselector_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_window_input.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoview_gtk_window_input.c  \
	kemoviewer.h \
	kemoview_gtk_window_input.h \
	kemoview_glut_routines.o \
	tree_view_4_pvr_colormap.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
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
