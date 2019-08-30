#
#
#

OBJ_KEMOVIEW_GLUT = \
kemoview_glut_routines.o \
const_viewer_menu_glut.o \
view_modifier_glut.o \
\
kemoview_fileselector_gtk.o \
kemoview_gtk_psf_selectors.o \
kemoview_gtk_PSF_surface_menu.o \
kemoview_gtk_PSF_isoline_menu.o \
kemoview_gtk_PSF_vector_menu.o \
kemoview_gtk_colormap_menu.o \
kemoview_gtk_mesh_menu.o \
kemoview_gtk_preference_menu.o \
\
tree_view_4_domain_viewer.o \
tree_view_4_ele_group_viewer.o \
tree_view_4_surf_group_viewer.o \
tree_view_4_nod_group_viewer.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLUTDIR = $$(C_SRCDIR)/KEMO_GLUT'   >> $(MAKENAME)

lib_name:

lib_archve:

mod_list:
	@echo  OBJ_KEMOVIEW_GLUT=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMOVIEW_GLUT)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	const_viewer_menu_glut.o: \
	'$$(KEMO_C_GLUTDIR)'/const_viewer_menu_glut.c \
	const_viewer_menu_glut.h \
	kemoviewer.h \
	view_modifier_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	view_modifier_glut.o: \
	'$$(KEMO_C_GLUTDIR)'/view_modifier_glut.c \
	view_modifier_glut.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_glut_routines.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_glut_routines.c \
	kemoview_glut_routines.h \
	kemoviewer.h \
	view_modifier_glut.o \
	view_modifier_glut.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_gtk.o:  \
	'$$(KEMO_C_GLUTDIR)'/kemoview_fileselector_gtk.c  \
	kemoview_fileselector_gtk.h \
	kemoviewer.h \
	kemoview_glut_routines.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_psf_selectors.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_psf_selectors.c \
	kemoview_gtk_psf_selectors.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_surface_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_surface_menu.c \
	kemoview_gtk_PSF_surface_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_pvr_colormap.o \
	kemoview_fileselector_gtk.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_isoline_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_isoline_menu.c \
	kemoview_gtk_PSF_isoline_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_pvr_colormap.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_vector_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_vector_menu.c \
	kemoview_gtk_PSF_vector_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_pvr_colormap.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_colormap_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_colormap_menu.c \
	kemoview_gtk_colormap_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_pvr_colormap.o \
	tree_view_4_light_position.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_psf_selectors.o \
	kemoview_gtk_PSF_surface_menu.o \
	kemoview_gtk_PSF_vector_menu.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_mesh_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_mesh_menu.c \
	kemoview_gtk_mesh_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_viewer_mesh.o \
	tree_view_4_domain_viewer.o \
	tree_view_4_nod_group_viewer.o \
	tree_view_4_ele_group_viewer.o \
	tree_view_4_surf_group_viewer.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_preference_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_preference_menu.c \
	kemoview_gtk_preference_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_view_4_pvr_colormap.o \
	tree_view_4_light_position.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_domain_viewer.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_domain_viewer.c \
	tree_view_4_domain_viewer.h \
	kemoviewer.h \
	tree_views_4_fixed_lists_GTK.o \
	t_control_chara_int3_IO.o \
	tree_view_chara_int3_GTK.o \
	tree_view_4_viewer_mesh.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_ele_group_viewer.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_ele_group_viewer.c \
	tree_view_4_ele_group_viewer.h \
	kemoviewer.h \
	tree_views_4_fixed_lists_GTK.o \
	t_control_chara_int3_IO.o \
	tree_view_chara_int3_GTK.o \
	tree_view_4_viewer_mesh.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_surf_group_viewer.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_surf_group_viewer.c \
	tree_view_4_surf_group_viewer.h \
	kemoviewer.h \
	tree_views_4_fixed_lists_GTK.o \
	t_control_chara_int3_IO.o \
	tree_view_chara_int3_GTK.o \
	tree_view_4_viewer_mesh.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_nod_group_viewer.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_nod_group_viewer.c \
	tree_view_4_nod_group_viewer.h \
	kemoviewer.h \
	tree_views_4_fixed_lists_GTK.o \
	t_control_chara_int_IO.o \
	tree_view_chara_int_GTK.o \
	tree_view_4_viewer_mesh.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_C_GLUTDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_C_GLUTDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done
