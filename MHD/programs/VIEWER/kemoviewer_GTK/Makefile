#
#
#

TARGET_KEMOVIEW_GTK =  kemoviewer_GTK
TARGET_KEMOVIEW_GLFW = kemoviewer_GLFW

TARGET_KEMOVIEW_GTK_TEST =  gtk_test_4_view
TARGET_KEMOVIEW_GLFW_TEST =  glfw_test_4_view

MOD_KEMOVIEW_GTK = \
tree_views_4_fixed_lists_GTK.o \
tree_view_real2_GTK.o \
tree_view_4_pvr_colormap.o \
\
kemo_mesh_viewer_gtk.o \
kemoviewer_gtk.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

MOD_KEMOVIEW_GLFW = \
kemoviewer_glfw_gtk.o \
kemo_mesh_viewer_glfw_gtk.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

MOD_KEMOVIEW_GTK_TEST = \
viewer_GTK_test.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

MOD_KEMOVIEW_GLFW_TEST = \
viewer_GLFW_test.o \
\
'$$(OBJ_KEMOVIEW_GLUT)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_GTKDIR =  $$(C_SRCDIR)/GTK'  >> $(MAKENAME)
	@echo 'KEMOVIEW_GTKDIR =  $$(KEMOVIEWER_DIR)/kemoviewer_GTK'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_KEMOVIEW_GTK =  $$(BUILDDIR)/$(TARGET_KEMOVIEW_GTK)'  >> $(MAKENAME)
	@echo 'TARGET_KEMOVIEW_GLFW = $$(BUILDDIR)/$(TARGET_KEMOVIEW_GLFW)' >> $(MAKENAME)
	@echo 'TARGET_KEMOVIEW_GTK_TEST = $$(BUILDDIR)/$(TARGET_KEMOVIEW_GTK_TEST)' >> $(MAKENAME)
	@echo 'TARGET_KEMOVIEW_GLFW_TEST = $$(BUILDDIR)/$(TARGET_KEMOVIEW_GLFW_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo viewer_gtk:  '$$(TARGET_KEMOVIEW_GTK)'   >> $(MAKENAME)
	@echo viewer_glfw: '$$(TARGET_KEMOVIEW_GLFW)'  >> $(MAKENAME)
	@echo viewer_gtk_test:  '$$(TARGET_KEMOVIEW_GTK_TEST) $$(TARGET_KEMOVIEW_GLFW_TEST)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GTK)': '$$(MOD_KEMOVIEW_GTK)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GTK)' \
	'$$(MOD_KEMOVIEW_GTK) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(GLFW_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GLFW)': '$$(MOD_KEMOVIEW_GLFW)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GLFW)' \
	'$$(MOD_KEMOVIEW_GLFW) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(GLFW_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GTK_TEST)': '$$(MOD_KEMOVIEW_GTK_TEST)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GTK_TEST)' \
	'$$(MOD_KEMOVIEW_GTK_TEST) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(GLFW_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_KEMOVIEW_GLFW_TEST)': '$$(MOD_KEMOVIEW_GLFW_TEST)' \
	'$$(LIB_KEMO_C_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(CC)' '$$(OPTFLAGS_GL) $$(GTK3_CFLAGS) $$(GLFW_CFLAGS)' -o '$$(TARGET_KEMOVIEW_GLFW_TEST)' \
	'$$(MOD_KEMOVIEW_GLFW_TEST) -L. $$(LIB_KEMO_C)' \
	'$$(GTK3_LIBS) $$(GLFW_LIBS) $$(OPENGL_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_KEMOVIEW_GTK =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GTK) >> $(MAKENAME)
	@echo  MOD_KEMOVIEW_GLFW =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GLFW) >> $(MAKENAME)
	@echo  MOD_KEMOVIEW_GTK_TEST =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GTK_TEST) >> $(MAKENAME)
	@echo  MOD_KEMOVIEW_GLFW_TEST =  \\ >> $(MAKENAME)
	@echo  $(MOD_KEMOVIEW_GLFW_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemoviewer_glfw_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemoviewer_glfw_gtk.c  \
	kemoviewer.h \
	kemo_mesh_viewer_glfw_gtk.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemoviewer_glfw_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	kemo_mesh_viewer_glfw_gtk.o:  \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_glfw_gtk.c  \
	kemo_mesh_viewer_glfw_gtk.h \
	kemoviewer.h \
	kemoview_gtk_PSF_menu.o \
	kemoview_gtk_mesh_menu.o \
	kemoview_gtk_preference_menu.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_main_menu.o \
	kemoview_gtk_viewmatrix_menu.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_glfw_gtk.c \
	>> $(MAKENAME)
	
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
	kemoview_gtk_mesh_menu.o \
	kemoview_gtk_preference_menu.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/kemo_mesh_viewer_gtk.c \
	>> $(MAKENAME)
	
	@echo \
	viewer_GTK_test.o:  \
	'$$(KEMOVIEW_GTKDIR)'/viewer_GTK_test.c  \
	kemoviewer.h \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_mesh_menu.o \
	kemoview_gtk_preference_menu.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/viewer_GTK_test.c \
	>> $(MAKENAME)
	
	@echo \
	viewer_GLFW_test.o:  \
	'$$(KEMOVIEW_GTKDIR)'/viewer_GLFW_test.c  \
	kemoviewer.h \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS)' \
	'$$(KEMOVIEW_GTKDIR)'/viewer_GLFW_test.c \
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
