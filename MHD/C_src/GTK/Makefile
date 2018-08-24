#
#
#

KEMO_GTK_DIR = $$(C_SRCDIR)/GTK

OBJ_KEMO_GTK_C = \
control_elements_IO_GTK.o \
\
tree_views_4_fixed_lists_GTK.o \
tree_view_real2_GTK.o \
tree_view_chara_GTK.o \
tree_view_chara_int_GTK.o \
tree_view_chara_real_GTK.o \
tree_view_chara2_real_GTK.o \
\
tree_view_4_pvr_colormap.o \
tree_view_4_field_GTK.o \
control_panel_4_field_GTK.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_GTK_DIR =  $(KEMO_GTK_DIR)'      >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_GTK_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_KEMO_GTK_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_GTK_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	control_elements_IO_GTK.o: \
	'$$(KEMO_GTK_DIR)'/control_elements_IO_GTK.c \
	control_elements_IO_GTK.h \
	t_control_int_IO.o \
	t_control_real_IO.o \
	t_control_chara_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_views_4_fixed_lists_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_views_4_fixed_lists_GTK.c \
	tree_views_4_fixed_lists_GTK.h \
	m_direction_labels_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_real2_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_real2_GTK.c \
	tree_view_real2_GTK.h \
	t_control_real2_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_chara_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_chara_GTK.c \
	tree_view_chara_GTK.h \
	t_control_chara_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_chara_int_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_chara_int_GTK.c \
	tree_view_chara_int_GTK.h \
	t_control_chara_int_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_chara_real_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_chara_real_GTK.c \
	tree_view_chara_real_GTK.h \
	t_control_chara_real_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_chara2_real_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_chara2_real_GTK.c \
	tree_view_chara2_real_GTK.h \
	t_control_chara2_real_IO.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_pvr_colormap.o:  \
	'$$(KEMO_GTK_DIR)'/tree_view_4_pvr_colormap.c  \
	tree_view_4_pvr_colormap.h \
	t_control_chara_IO.o \
	t_control_real_IO.o \
	t_control_real2_IO.o \
	t_ctl_data_pvr_colormap_c.o \
	tree_view_real2_GTK.o \
	tree_views_4_fixed_lists_GTK.o \
	quicksort_c.o \
	set_rgb_colors_c.o \
	set_rgba_table_c.o \
	m_color_table_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	tree_view_4_field_GTK.o: \
	'$$(KEMO_GTK_DIR)'/tree_view_4_field_GTK.c \
	tree_view_4_field_GTK.h \
	t_ctl_data_4_fields_c.o \
	tree_views_4_fixed_lists_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	@echo \
	
	@echo \
	control_panel_4_field_GTK.o: \
	'$$(KEMO_GTK_DIR)'/control_panel_4_field_GTK.c \
	control_panel_4_field_GTK.h \
	t_ctl_data_4_fields_c.o \
	tree_views_4_fixed_lists_GTK.o \
	tree_view_4_field_GTK.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(GTK3_CFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	@echo \


includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_GTK_DIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_GTK_DIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o $(TARGET) *.mod *~
