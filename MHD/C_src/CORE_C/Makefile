#
#
#

KEMO_C_DIR =     $$(C_SRCDIR)/CORE_C

SRC_KEMO_C = $(shell ls *.c)
INC_KEMO_C = $(addsuffix .h,$(basename $(SRC_KEMO_C)) )

OBJ_KEMO_C = \
 calypso_param_c.o \
 \
 delete_file_c.o \
 numbers_to_bin_c.o \
 jacobian_trilinear_c.o \
 ysglfontdata.o \
 ysglmaketextbitmap.o \
 get_ysglbitmap.o \
 write_image_2_bmp.o \
 read_image_2_bmp.o \
 \
 m_read_text_to_character.o \
 m_surface_mesh_4_viewer_c.o \
 m_grouping_mesh_4_viewer_c.o \
 m_psf_data_4_viewer_c.o \
 m_map_data_4_viewer_c.o \
 check_viewer_mesh_c.o \
 check_psf_data_viewer_c.o \
 skip_comment_c.o \
 read_viewer_mesh_c.o \
 read_grouping_mesh_c.o \
 read_psf_data_viewer_c.o \
 read_psf_bin_data_c.o \
 read_psf_vtk_data_c.o \
 set_psf_viewer.o \
 \
 read_viewer_mesh_gz_c.o \
 read_psf_data_gz_c.o \
 read_psf_bin_data_gz_c.o \
 read_psf_vtk_data_gz_c.o \
 \
 set_surface_mesh_data.o \
 set_normal_on_node_4_mesh.o \
 cal_surface_center_normal_c.o \
 take_normal_surf_mesh_c.o \
 \
 coordinate_converter_c.o \
 projection_patch_to_map.o \
 set_new_patch_4_map_c.o \
 set_map_data_4_viewer_c.o \
 take_normal_psf_c.o \
 \
 set_rgb_colors_c.o \
 invert_small_matrix_c.o \
 transfer_matvec.o \
 quicksort_c.o

ifdef ZLIB_LIBS
  OBJ_KEMO_C += \
 calypso_zlib_io_c.o
endif

ifdef PNG_LIBS
  OBJ_KEMO_C += \
   write_image_2_png.o \
   read_image_2_png.o
endif

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_DIR =  $(KEMO_C_DIR)'      >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_KEMO_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@echo 'Construct dependencies in C_src/CORE_C'
	@echo '#' > $(MAKENAME).tmp
	@for sources in $(SRC_KEMO_C) ; do \
		( \
		cd $(MAKEDIR); \
		$(CC) $(DEPFLAGS) $(OPTFLAGS_GL) $(C_INCLUDE) $(MHDDIR)/C_src/CORE_C/$$sources >> $(MAKENAME).tmp; \
		echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' >> $(MAKENAME).tmp \
		) \
	done
	@sed -e 's@'$(MHDDIR)/C_src'@'$$\(C_SRCDIR\)'@g' $(MAKENAME).tmp > $(MHDDIR)/C_src/CORE_C/Makefile.depends
	@rm $(MAKENAME).tmp

clean:
	rm -f *.o $(TARGET) *.mod *~
