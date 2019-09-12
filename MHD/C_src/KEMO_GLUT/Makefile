#
#
#

OBJ_KEMOVIEW_GLUT = \
view_modifier_glfw.o \
\
kemoview_fileselector_gtk.o \
kemoview_gtk_PSF_surface_menu.o \
kemoview_gtk_PSF_isoline_menu.o \
kemoview_gtk_PSF_vector_menu.o \
kemoview_gtk_PSF_menu.o \
kemoview_gtk_mesh_menu.o \
kemoview_gtk_fline_selectors.o \
kemoview_gtk_fieldline_menu.o \
kemoview_gtk_axis_menu.o \
kemoview_gtk_rotation_menu.o \
kemoview_gtk_evolution_menu.o \
kemoview_gtk_viewmatrix_menu.o \
kemoview_gtk_main_menu.o \
kemoview_gtk_preference_menu.o \
\
tree_view_4_light_position.o \
tree_view_4_domain_viewer.o \
tree_view_4_ele_group_viewer.o \
tree_view_4_surf_group_viewer.o \
tree_view_4_nod_group_viewer.o \
tree_view_4_viewer_mesh.o

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
	view_modifier_glfw.o:  \
	'$$(KEMO_C_GLUTDIR)'/view_modifier_glfw.c  \
	view_modifier_glfw.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_fileselector_gtk.o:  \
	'$$(KEMO_C_GLUTDIR)'/kemoview_fileselector_gtk.c  \
	kemoview_fileselector_gtk.h \
	kemoviewer.h \
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
	tree_view_chara_int_GTK.o \
	kemoview_gtk_routines.o \
	kemoview_fileselector_gtk.o \
	view_modifier_glfw.o \
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
	tree_view_chara_int_GTK.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_vector_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_vector_menu.c \
	kemoview_gtk_PSF_vector_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	kemoview_gtk_routines.o \
	tree_view_4_pvr_colormap.o \
	tree_view_chara_int_GTK.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_PSF_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_PSF_menu.c \
	kemoview_gtk_PSF_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	m_kemoview_psf_menu.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_chara_int_GTK.o \
	tree_view_4_pvr_colormap.o \
	tree_view_4_light_position.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_PSF_surface_menu.o \
	kemoview_gtk_PSF_isoline_menu.o \
	kemoview_gtk_PSF_vector_menu.o \
	view_modifier_glfw.o \
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
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_fline_selectors.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_fline_selectors.c \
	kemoview_gtk_fline_selectors.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	m_kemoview_psf_menu.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_chara_int_GTK.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_fieldline_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_fieldline_menu.c \
	kemoview_gtk_fieldline_menu.h \
	kemoviewer.h \
	tree_views_4_fixed_lists_GTK.o \
	kemoview_gtk_fline_selectors.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_axis_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_axis_menu.c \
	kemoview_gtk_axis_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_views_4_fixed_lists_GTK.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_rotation_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_rotation_menu.c \
	kemoview_gtk_rotation_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_views_4_fixed_lists_GTK.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_evolution_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_evolution_menu.c \
	kemoview_gtk_evolution_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_views_4_fixed_lists_GTK.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_viewmatrix_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_viewmatrix_menu.c \
	kemoview_gtk_viewmatrix_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	kemoview_gtk_routines.o \
	kemoview_fileselector_gtk.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoview_gtk_main_menu.o: \
	'$$(KEMO_C_GLUTDIR)'/kemoview_gtk_main_menu.c \
	kemoview_gtk_main_menu.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_chara_int_GTK.o \
	kemoview_fileselector_gtk.o \
	kemoview_gtk_evolution_menu.o \
	kemoview_gtk_rotation_menu.o \
	kemoview_gtk_axis_menu.o \
	tree_view_4_light_position.o \
	kemoview_gtk_preference_menu.o \
	kemoview_gtk_PSF_menu.o \
	kemoview_gtk_fieldline_menu.o \
	kemoview_gtk_viewmatrix_menu.o \
	kemoview_gtk_mesh_menu.o \
	view_modifier_glfw.o \
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
	kemoview_gtk_colorsel.o \
	kemoview_gtk_routines.o \
	view_modifier_glfw.o \
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
	kemoview_gtk_routines.o \
	kemoview_gtk_colorsel.o \
	quicksort_c.o \
	view_modifier_glfw.o \
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
	kemoview_gtk_routines.o \
	kemoview_gtk_colorsel.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	view_modifier_glfw.o \
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
	kemoview_gtk_routines.o \
	kemoview_gtk_colorsel.o \
	create_tree_view_columns_GTK.o \
	quicksort_c.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_nod_group_viewer.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_nod_group_viewer.c \
	tree_view_4_nod_group_viewer.h \
	kemoviewer.h \
	t_control_chara_int_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_chara_int_GTK.o \
	tree_view_4_viewer_mesh.o \
	create_tree_view_columns_GTK.o \
	kemoview_gtk_routines.o \
	kemoview_gtk_colorsel.o \
	quicksort_c.o \
	view_modifier_glfw.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$(GTK3_CFLAGS) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_light_position.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_light_position.c \
	tree_view_4_light_position.h \
	t_control_real_IO.o \
	t_control_real3_IO.o \
	m_phong_light_table_c.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_real3_GTK.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_viewer_mesh.o: \
	'$$(KEMO_C_GLUTDIR)'/tree_view_4_viewer_mesh.c \
	kemoviewer.h \
	tree_view_4_viewer_mesh.h \
	t_control_chara_int_IO.o \
	t_control_chara_int3_IO.o \
	tree_view_chara_int_GTK.o \
	tree_view_chara_int3_GTK.o \
	tree_views_4_fixed_lists_GTK.o \
	create_tree_view_columns_GTK.o \
	m_surface_mesh_4_viewer_c.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
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
