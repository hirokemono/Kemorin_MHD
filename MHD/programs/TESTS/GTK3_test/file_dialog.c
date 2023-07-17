
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_array_chara3_items_c.h"
#include "t_ctl_array_chara_int3_items_c.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_data_4_fields_c.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_GTK.h"
#include "tree_view_4_field_GTK.h"
#include "tree_view_4_force_GTK.h"
#include "tree_view_boundary_condition_GTK.h"

#include "c_ctl_data_SGS_model.h"
#include "c_control_data_pvrs.h"
#include "c_ctl_data_platforms.h"
#include "c_ctl_data_MHD_BCs.h"

#include "ctl_data_platforms_GTK.h"
#include "control_panel_4_dimless_GTK.h"
#include "control_panels_MHD_control_GTK.h"
#include "control_block_panel_GTK.h"
#include "control_panel_cbox_GTK.h"
#include "control_panel_cbox_real_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_4_sph_monitor_GTK.h"
#include "control_panel_4_MHD_BCs_GTK.h"


extern void c_view_control_sph_SGS_MHD();


extern void * c_pvr_render_ctls_block_name(void *f_pvr_ctls);
extern int    c_pvr_render_ctls_num_pvr_ctl(void *f_pvr_ctls);
extern void * c_pvr_render_ctls_pvr_ctl(int idx, void *f_pvr_ctls);
extern void * c_append_viz_pvr_render_ctls(int idx, char *block_name, void *f_pvr_ctls);
extern void * c_delete_viz_pvr_render_ctls(int idx, void *f_pvr_ctls);

extern void * c_map_render_ctls_block_name(void *f_map_ctls);
extern int    c_map_render_ctls_num_map_ctl(void *f_map_ctls);
extern char * c_map_render_ctls_fname(int idx, void *f_map_ctls);
extern void * c_map_render_ctls_map_ctl(int idx, void *f_map_ctls);
extern void * c_append_viz_map_render_ctls(int idx, char *block_name, void *f_map_ctls);
extern void * c_delete_viz_map_render_ctls(int idx, void *f_map_ctls);



extern void * c_read_control_sph_SGS_MHD(char *file_name);
extern void * c_add_sgs_sph_mhd_ctl();
extern void * c_MHD_block_name(void *f_MHD_ctl);
extern void * c_MHD_iflag(void *f_MHD_ctl);
extern void * c_MHD_plt(void *f_MHD_ctl);
extern void * c_MHD_org_plt(void *f_MHD_ctl);
extern void * c_MHD_new_plt(void *f_MHD_ctl);
extern void * c_MHD_fname_psph(void *f_MHD_ctl);
extern void * c_MHD_psph_ctl(void *f_MHD_ctl);
extern void * c_MHD_model_ctl(void *f_MHD_ctl);
extern void * c_MHD_smctl_ctl(void *f_MHD_ctl);
extern void * c_MHD_smonitor_ctl(void *f_MHD_ctl);
extern void * c_MHD_nmtr_ctl(void *f_MHD_ctl);
extern void * c_MHD_sgs_ctl(void *f_MHD_ctl);
extern void * c_MHD_viz_ctls(void *f_MHD_ctl);
extern void * c_MHD_zm_ctls(void *f_MHD_ctl);

extern void * c_visualizations_block_name(void *f_viz_ctls);
extern void * c_visualizations_iflag(void *f_viz_ctls);
extern void * c_visualizations_psf_ctls(void *f_viz_ctls);
extern void * c_visualizations_iso_ctls(void *f_viz_ctls);
extern void * c_visualizations_map_ctls(void *f_viz_ctls);
extern void * c_visualizations_pvr_ctls(void *f_viz_ctls);
extern void * c_visualizations_fline_ctls(void *f_viz_ctls);
extern void * c_visualizations_lic_ctls(void *f_viz_ctls);
extern void * c_visualizations_repart_ctl(void *f_viz_ctls);
extern void * c_visualizations_fname_vrepart(void *f_viz_ctls);


extern void * c_dynamo_vizs_block_name(void *f_zm_ctls);
extern void * c_dynamo_vizs_iflag(void *f_zm_ctls);
extern void * c_dynamo_vizs_crust_filter_ctl(void *f_zm_ctls);
extern void * c_dynamo_vizs_zm_psf_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zRMS_psf_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zm_map_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zRMS_map_ctls(void *f_zm_ctls);

extern void * c_MHD_mdl_block_name(void *f_model_ctl);
extern void * c_MHD_mdl_iflag(void *f_model_ctl);
extern void * c_MHD_mdl_fld_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_evo_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_earea_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_nbc_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_sbc_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_dless_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_eqs_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_frc_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_g_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_cor_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_mcv_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_bscale_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_reft_ctl(void *f_model_ctl);
extern void * c_MHD_mdl_refc_ctl(void *f_model_ctl);


extern void * c_VIZ_PSF_ctl_block_name(void *f_psf_ctl);
extern void * c_VIZ_PSF_ctl_iflag(void *f_psf_ctl);
extern void * c_VIZ_PSF_fname_section_ctl(void *f_psf_ctl);
extern void * c_VIZ_PSF_psf_def_c(void *f_psf_ctl);
extern void * c_VIZ_PSF_fname_fld_on_psf(void *f_psf_ctl);
extern void * c_VIZ_PSF_fld_on_psf_c(void *f_psf_ctl);
extern void * c_VIZ_PSF_file_head_ctl(void *f_psf_ctl);
extern void * c_VIZ_PSF_output_type_ctl(void *f_psf_ctl);

extern void * c_VIZ_psf_define_ctl_block_name(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_ctl_iflag(void *f_psf_def_ctl);
extern void * c_VIZ_psf_def_sect_method_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_coefs_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_center_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_normal_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_axis_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_radius_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_grp_name_ctl(void *f_psf_def_ctl);
extern void * c_VIZ_psf_define_area_ctl(void *f_psf_def_ctl);

extern void * c_VIZ_fld_on_psf_ctl_block_name(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_ctl_iflag(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_field_out_ctl(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_out_value_ctl(void *f_psf_fld_ctl);
extern void * c_VIZ_fld_on_psf_out_type_ctl(void *f_psf_fld_ctl);


extern void * c_VIZ_ISO_ctl_block_name(void *f_iso_ctl);
extern void * c_VIZ_ISO_ctl_iflag(void *f_iso_ctl);
extern void * c_VIZ_ISO_iso_def_ctl(void *f_iso_ctl);
extern void * c_VIZ_ISO_fname_fld_on_iso(void *f_iso_ctl);
extern void * c_VIZ_ISO_fld_on_iso_c(void *f_iso_ctl);
extern void * c_VIZ_ISO_file_head_ctl(void *f_iso_ctl);
extern void * c_VIZ_ISO_output_type_ctl(void *f_iso_ctl);

extern void * c_VIZ_iso_define_ctl_block_name(void *f_iso_def_ctl);
extern void * c_VIZ_iso_define_ctl_iflag(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_data_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_comp_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_value_ctl(void *f_iso_def_ctl);
extern void * c_VIZ_isosurf_area_ctl(void *f_iso_def_ctl);

extern void * c_VIZ_MAP_ctl_block_name(void *f_map_ctl);
extern void * c_VIZ_MAP_ctl_iflag(void *f_map_ctl);
extern void * c_VIZ_MAP_map_define_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_fname_mat_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_viewmat_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_fname_cmap_cbar_c(void *f_map_ctl);
extern void * c_VIZ_MAP_cmap_cbar_c(void *f_map_ctl);
extern void * c_VIZ_MAP_map_image_prefix_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_image_fmt_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_field_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_map_comp_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_isoline_field_ctl(void *f_map_ctl);
extern void * c_VIZ_MAP_isoline_comp_ctl(void *f_map_ctl);

extern void * c_VIZ_LIC_ctl_block_name(void *f_lic_ctl);
extern void * c_VIZ_LIC_ctl_iflag(void *f_lic_ctl);
extern void * c_VIZ_LIC_LIC_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_pe_elapsed_dump_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_color_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_color_comp_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_opacity_field_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_opacity_comp_ctl(void *f_lic_ctl);
extern int    c_VIZ_LIC_num_masking_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_mask_ctl(int idx, void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_noise_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_noise_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_kernel_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_kernel_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_vr_sample_mode_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_step_size_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_normalize_type_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_normalize_value_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_fname_vol_repart_ctl(void *f_lic_ctl);
extern void * c_VIZ_LIC_repartition_ctl(void *f_lic_ctl);

extern void * c_VIZ_PVR_ctl_block_name(void *f_pvr_ctl);
extern void * c_VIZ_PVR_ctl_iflag(void *f_pvr_ctl);
extern void * c_VIZ_PVR_fname_mat_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_viewmat_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_fname_pvr_light_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_light_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_fname_cmap_cbar_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_cmap_cbar_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_movie_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_quilt_image_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_updated_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_file_head_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_file_head_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_file_fmt_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_file_fmt_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_monitoring_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_streo_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_anaglyph_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_quilt_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_render_area_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_field_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_component_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_sections_ctl(void *f_pvr_ctl);
extern void * c_VIZ_PVR_isosurfaces_ctl(void *f_pvr_ctl);

extern void * c_PVR_cmap_cbar_ctl_block_name(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_ctl_iflag(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_color_ctl(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_cbar_ctl(void *f_cmap_cbar_c);

extern void * c_PVR_colormap_ctl_block_name(void *f_color);
extern void * c_PVR_colormap_ctl_iflag(void *f_color);
extern void * c_PVR_cmap_lic_color_comp_ctl(void *f_color);
extern void * c_PVR_cmap_lic_opacity_fld_ctl(void *f_color);
extern void * c_PVR_cmap_lic_opacity_comp_ctl(void *f_color);
extern void * c_PVR_cmap_colormap_mode_ctl(void *f_color);
extern void * c_PVR_cmap_data_mapping_ctl(void *f_color);
extern void * c_PVR_cmap_opacity_style_ctl(void *f_color);
extern void * c_PVR_cmap_range_min_ctl(void *f_color);
extern void * c_PVR_cmap_range_max_ctl(void *f_color);
extern void * c_PVR_cmap_fix_opacity_ctl(void *f_color);
extern void * c_PVR_cmap_colortbl_ctl(void *f_color);
extern void * c_PVR_cmap_linear_opacity_ctl(void *f_color);
extern void * c_PVR_cmap_step_opacity_ctl(void *f_color);
extern void * c_PVR_cmap_background_color_ctl(void *f_color);

extern void * c_PVR_colorbar_ctl_block_name(void *f_cbar_ctl);
extern void * c_PVR_colorbar_ctl_iflag(void *f_cbar_ctl);
extern void * c_PVR_colorbar_switch_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_scale_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_position_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_zeromarker_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_font_size_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_ngrid_cbar_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_cbar_range_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_axis_switch_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_time_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_mapgrid_ctl(void *f_cbar_ctl);

extern void * c_PVR_light_ctl_block_name(void *f_light);
extern void * c_PVR_light_ctl_iflag(void *f_light);
extern void * c_PVR_light_ambient_coef_ctl(void *f_light);
extern void * c_PVR_light_diffuse_coef_ctl(void *f_light);
extern void * c_PVR_light_specular_coef_ctl(void *f_light);
extern void * c_PVR_light_position_ctl(void *f_light);

extern void * c_PVR_movie_ctl_block_name(void *f_movie);
extern void * c_PVR_movie_ctl_iflag(void *f_movie);
extern void * c_PVR_movie_movie_mode_ctl(void *f_movie);
extern void * c_PVR_movie_num_frames_ctl(void *f_movie);
extern void * c_PVR_movie_rotation_axis_ctl(void *f_movie);
extern void * c_PVR_movie_angle_range_ctl(void *f_movie);
extern void * c_PVR_movie_apature_range_ctl(void *f_movie);
extern void * c_PVR_movie_LIC_kernel_rge_ctl(void *f_movie);
extern void * c_PVR_movie_fname_view_st_ctl(void *f_movie);
extern void * c_PVR_movie_fname_view_end_ctl(void *f_movie);
extern void * c_PVR_movie_view_start_ctl(void *f_movie);
extern void * c_PVR_movie_view_end_ctl(void *f_movie);
extern void * c_PVR_movie_mul_mmats_c(void *f_movie);

extern void * c_VIZ_mul_mdlvw_ctl_block_name(void *f_mul_mmats_c);
extern int    c_VIZ_mul_mdlvw_num_mat_c(void *f_mul_mmats_c);
extern void * c_VIZ_mul_mdlvw_fname_ctl(int idx, void *f_mul_mmats_c);
extern void * c_VIZ_mul_mdlvw_matrices(int idx, void *f_mul_mmats_c);

extern void * c_PVR_quilt_img_ctl_block_name(void *f_quilt_c);
extern void * c_PVR_quilt_img_ctl_iflag(void *f_quilt_c);
extern void * c_PVR_quilt_num_column_row_ctl(void *f_quilt_c);
extern void * c_PVR_quilt_num_row_column_ctl(void *f_quilt_c);
extern void * c_PVR_quilt_mul_qmats_ctl(void *f_quilt_c);

extern int    c_VIZ_PVR_num_pvr_sect_ctl(void *f_pvr_scts_c);
extern void * c_VIZ_PVR_section_ctl(int idx, void *f_pvr_scts_c);
extern int    c_VIZ_PVR_num_pvr_iso_ctl(void *f_pvr_isos_c);
extern void * c_VIZ_PVR_isosurface_ctl(int idx, void *f_pvr_isos_c);

extern void * c_PVR_section_ctl_block_name(void *f_pvr_sect_ctl);
extern void * c_PVR_section_ctl_iflag(void *f_pvr_sect_ctl);
extern void * c_PVR_section_fname_sect_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_psf_def_c(void *f_pvr_sect_ctl);
extern void * c_PVR_section_opacity_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_zeroline_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_iso_color_mode(void *f_pvr_sect_ctl);
extern void * c_PVR_section_iso_number_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_iso_range_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_iso_width_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_grid_width_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_tan_cyl_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_tan_cyl_in_ctl(void *f_pvr_sect_ctl);
extern void * c_PVR_section_tan_cyl_out_ctl(void *f_pvr_sect_ctl);

extern void * c_PVR_isosurf_ctl_block_name(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_ctl_iflag(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_type_ctl(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_iso_value_ctl(void *f_pvr_iso_ctl);
extern void * c_PVR_isosurf_opacity_ctl(void *f_pvr_iso_ctl);


extern void * c_clust_filter_ctl_block_name(void *f_crust_filter_ctl);
extern void * c_clust_filter_ctl_iflag(void *f_crust_filter_ctl);
extern void * c_clust_filter_ltr_ctl(void *f_crust_filter_ctl);


extern void * c_VIZ_FLINE_ctl_block_name(void *f_fline_ctl);
extern void * c_VIZ_FLINE_ctl_iflag(void *f_fline_ctl);
extern void * c_VIZ_FLINE_file_head_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_output_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_field_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_color_field_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_color_comp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_area_grp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_starting_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_selection_type_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_line_direction_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_start_surf_grp_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_num_fieldline_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_max_line_step_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_seed_point_ctl(void *f_fline_ctl);
extern void * c_VIZ_FLINE_seed_surface_ctl(void *f_fline_ctl);


extern void * c_masking_fld_ctl_block_name(void *f_mask_ctl);
extern void * c_masking_fld_ctl_iflag(void *f_mask_ctl);
extern void * c_masking_fld_mask_type_ctl(void *f_mask_ctl);
extern void * c_masking_fld_field_name_ctl(void *f_mask_ctl);
extern void * c_masking_fld_component_ctl(void *f_mask_ctl);
extern void * c_masking_fld_mask_range_ctl(void *f_mask_ctl);

extern void * c_LIC_kernel_ctl_block_name(void *f_kernel_ctl);
extern void * c_LIC_kernel_ctl_iflag(void *f_kernel_ctl);
extern void * c_LIC_kernel_type_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_resolution_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_peak_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_kernel_sigma_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_trace_len_mod_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_half_length_ctl(void *f_kernel_ctl);
extern void * c_LIC_kernel_max_trace_cnt_ctl(void *f_kernel_ctl);

extern void * c_LIC_noise_ctl_block_name(void *f_noise_ctl);
extern void * c_LIC_noise_ctl_iflag(void *f_noise_ctl);
extern void * c_LIC_noise_type_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_file_name_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_file_format_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_resolution_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_stepping_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_cube_size_ctl(void *f_noise_ctl);
extern void * c_LIC_noise_deltax_ctl(void *f_noise_ctl);

extern void * c_modeview_ctl_block_name(void *f_mat);
extern void * c_modeview_ctl_iflag(void *f_mat);
extern void * c_modeview_mat_ctl_fname(void *f_mat);
extern void * c_modeview_ctl_pixel(void *f_mat);
extern void * c_modeview_ctl_proj(void *f_mat);
extern void * c_modeview_ctl_streo(void *f_mat);
extern void * c_modeview_modelview_mat_ctl(void *f_mat);
extern void * c_modeview_lookpoint_ctl(void *f_mat);
extern void * c_modeview_viewpoint_ctl(void *f_mat);
extern void * c_modeview_up_dir_ctl(void *f_mat);
extern void * c_modeview_view_rot_vec_ctl(void *f_mat);
extern void * c_modeview_rotation_deg_ctl(void *f_mat);
extern void * c_modeview_scale_factor_ctl(void *f_mat);
extern void * c_modeview_scale_vector_ctl(void *f_mat);
extern void * c_modeview_viewpt_in_view_ctl(void *f_mat);
extern void * c_modeview_projection_type_ctl(void *f_mat);


extern void * c_screen_pixel_ctl_block_name(void *f_pixel);
extern void * c_screen_pixel_ctl_iflag(void *f_pixel);
extern void * c_screen_num_xpixel_ctl(void *f_pixel);
extern void * c_screen_num_ypixel_ctl(void *f_pixel);

extern void * c_projection_ctl_block_name(void *f_proj);
extern void * c_projection_ctl_iflag(void *f_proj);
extern void * c_projection_perspect_agl_ctl(void *f_proj);
extern void * c_projection_xy_ratio_ctl(void *f_proj);
extern void * c_projection_near_ctl(void *f_proj);
extern void * c_projection_far_ctl(void *f_proj);
extern void * c_projection_horiz_range_ctl(void *f_proj);
extern void * c_projection_vert_range_ctl(void *f_proj);

extern void * c_streo_view_ctl_block_name(void *f_streo);
extern void * c_streo_view_ctl_iflag(void *f_streo);
extern void * c_streo_view_focalpoint_ctl(void *f_streo);
extern void * c_streo_view_eye_separate_ctl(void *f_streo);
extern void * c_streo_view_eye_sep_angle_ctl(void *f_streo);
extern void * c_streo_view_step_eye_sep_ctl(void *f_streo);

extern void * c_section_ctls_block_name(void *f_psf_ctls);
extern int    c_section_ctls_num_psf_ctl(void *f_psf_ctls);
extern char * c_section_ctls_fname(int idx, void *f_psf_ctls);
extern void * c_section_ctls_psf_ctl(int idx, void *f_psf_ctls);
extern void * c_append_viz_section_ctls(int idx, char *block_name, void *f_psf_ctls);
extern void * c_delete_viz_section_ctls(int idx, void *f_psf_ctls);

extern void * c_isosurf_ctls_block_name(void *f_iso_ctls);
extern int    c_isosurf_ctls_num_iso_ctl(void *f_iso_ctls);
extern char * c_isosurf_ctls_fname(int idx, void *f_iso_ctls);
extern void * c_isosurf_ctls_iso_ctl(int idx, void *f_iso_ctls);
extern void * c_append_viz_isosurf_ctls(int idx, char *block_name, void *f_iso_ctls);
extern void * c_delete_viz_isosurf_ctls(int idx, void *f_iso_ctls);

extern void * c_lic_render_ctls_block_name(void *f_lic_ctls);
extern int    c_lic_render_ctls_num_lic_ctl(void *f_lic_ctls);
extern char * c_lic_render_ctls_fname(int idx, void *f_lic_ctls);
extern void * c_lic_render_ctls_pvr_ctl(int idx, void *f_lic_ctls);
extern void * c_lic_render_ctls_lic_ctl(int idx, void *f_lic_ctls);
extern void * c_append_viz_lic_render_ctls(int idx, char *block_name, void *f_lic_ctls);
extern void * c_delete_viz_lic_render_ctls(int idx, void *f_lic_ctls);

extern void * c_fline_ctls_block_name(void *f_fline_ctls);
extern int    c_fline_ctls_num_fline_ctl(void *f_fline_ctls);
extern char * c_fline_ctls_fname(int idx, void *f_fline_ctls);
extern void * c_fline_ctls_fline_ctl(int idx, void *f_fline_ctls);
extern void * c_append_viz_fline_ctls(int idx, char *block_name, void *f_fline_ctls);
extern void * c_delete_viz_fline_ctls(int idx, void *f_fline_ctls);


extern void * c_MHD_evo_area_ctl_block_name(void *f_earea_ctl);
extern void * c_MHD_evo_area_ctl_iflag(void *f_earea_ctl);
extern void * c_MHD_evo_fluid_group_ctl(void *f_earea_ctl);
extern void * c_MHD_evo_conduct_group_ctl(void *f_earea_ctl);

extern void * c_MHD_gravity_ctl_block_name(void *f_g_ctl);
extern void * c_MHD_gravity_ctl_iflag(void *f_g_ctl);
extern void * c_MHD_FEM_gravity_model(void *f_g_ctl);
extern void * c_MHD_gravity_ctl_gravity(void *f_g_ctl);
extern void * c_MHD_gravity_ctl_vector(void *f_g_ctl);

extern void * c_MHD_coriolis_ctl_block_name(void *f_cor_ctl);
extern void * c_MHD_coriolis_ctl_iflag(void *f_cor_ctl);
extern void * c_MHD_FEM_coriolis_model(void *f_cor_ctl);
extern void * c_MHD_FEM_coriolis_implicit(void *f_cor_ctl);
extern void * c_MHD_system_rotation(void *f_cor_ctl);

extern void * c_MHD_mag_cv_ctl_block_name(void *f_mcv_ctl);
extern void * c_MHD_mag_cv_ctl_iflag(void *f_mcv_ctl);
extern void * c_MHD_mag_cv_filterd_ctl(void *f_mcv_ctl);
extern void * c_MHD_mag_cv_ctl_magneto_cv(void *f_mcv_ctl);
extern void * c_MHD_mag_cv_ctl_ext_magne(void *f_mcv_ctl);

extern void * c_MHD_B_scale_ctl_block_name(void *f_bscale_ctl);
extern void * c_MHD_B_scale_ctl_iflag(void *f_bscale_ctl);
extern void * c_MHD_B_scale_mag_to_kin_ctl(void *f_bscale_ctl);

extern void * c_takepiro_model_ctl_block_name(void *f_takepiro_ctl);
extern void * c_takepiro_model_ctl_iflag(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_sigma_ctl(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_width_ctl(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_rout_ctl(void *f_takepiro_ctl);

extern void * c_reftemp_point_ctl_block_name(void *f_refs_ctl);
extern void * c_reftemp_point_ctl_iflag(void *f_refs_ctl);
extern void * c_reftemp_point_value_ctl(void *f_refs_ctl);
extern void * c_reftemp_point_depth_ctl(void *f_refs_ctl);

extern void * c_temp_model_ctl_block_name(void *f_reft_ctl);
extern void * c_temp_model_ctl_iflag(void *f_reft_ctl);
extern void * c_temp_model_filter_advect_ctl(void *f_reft_ctl);
extern void * c_temp_model_reference_ctl(void *f_reft_ctl);
extern void * c_temp_model_stratified_ctl(void *f_reft_ctl);
extern void * c_temp_model_ref_file_ctl(void *f_reft_ctl);
extern void * c_temp_model_ICB_diffuse_ctl(void *f_reft_ctl);
extern void * c_temp_model_low_ctl(void *f_reft_ctl);
extern void * c_temp_model_high_ctl(void *f_reft_ctl);
extern void * c_temp_model_takepiro_ctl(void *f_reft_ctl);

extern void * c_node_monitor_ctl_block_name(void *f_nmtr_ctl);
extern void * c_node_monitor_ctl_iflag(void *f_nmtr_ctl);
extern void * c_node_monitor_xx_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_node_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_group_ctl(void *f_nmtr_ctl);

extern void * c_viz_repart_ctl_block_name(void *f_viz_repart_c);
extern void * c_viz_repart_ctl_iflag(void *f_viz_repart_c);
extern void * c_viz_repart_viz_plt_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fmesh_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fmesh_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_new_part_ctl(void *f_viz_repart_c);
extern void * c_viz_repart_Fsleeve_ctl(void *f_viz_repart_c);

extern void * c_FEM_sleeve_ctl_block_name(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_ctl_iflag(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_extension_mode_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_sleeve_level_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_sleeve_size_ctl(void *f_Fsleeve_ctl);
extern void * c_FEM_sleeve_ref_vector_ctl(void *f_Fsleeve_ctl);


extern void * c_new_repart_ctl_block_name(void *f_new_part_ctl);
extern void * c_new_repart_ctl_iflag(void *f_new_part_ctl);
extern void * c_new_repart_table_head_ctl(void *f_new_part_ctl);
extern void * c_new_repart_table_fmt_ctl(void *f_new_part_ctl);
extern void * c_new_repart_partition_ref_ctl(void *f_new_part_ctl);
extern void * c_new_repart_trace_cnt_head_ctl(void *f_new_part_ctl);
extern void * c_new_repart_trace_cnt_fmt_ctl(void *f_new_part_ctl);
extern void * c_new_repart_ndomain_sect_ctl(void *f_new_part_ctl);
extern void * c_new_repart_ratio_group_ctl(void *f_new_part_ctl);
extern void * c_new_repart_sleeve_level_ctl(void *f_new_part_ctl);
extern void * c_new_repart_weight_to_prev_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mask_switch_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mask_weight_ctl(void *f_new_part_ctl);
extern void * c_new_repart_pwr_of_vol_ctl(void *f_new_part_ctl);
extern void * c_new_repart_num_masking_ctl(void *f_new_part_ctl);
extern void * c_new_repart_mask_ctl(void *f_new_part_ctl);


extern void * c_MHD_evolution_ctl_block_name(void *f_evo_ctl);
extern void * c_MHD_evolution_ctl_iflag(void *f_evo_ctl);
extern void * c_MHD_t_evo_field_ctl(void *f_evo_ctl);



extern void * c_link_base_field_names_to_ctl(void *fld_names_c);
extern void * c_link_time_evo_list_to_ctl(void *fld_names_c);
extern void * c_link_force_list_to_ctl(void *fld_names_c);
extern void * c_link_sph_force_list_to_ctl(void *fld_names_c);
extern void * c_link_reftemp_list_to_ctl(void *fld_names_c);
extern void * c_link_sph_reftemp_list_to_ctl(void *fld_names_c);

extern void * c_link_xyz_dir_list_to_ctl(void *fld_names_c);

extern void * set_file_fmt_items_f(void *fmt_names_c);


struct f_MHD_t_evo_area_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_evo_fluid_group_ctl;
	struct chara_clist *f_evo_conduct_group_ctl;
};

struct f_MHD_gravity_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_FEM_gravity_model;
	struct chara_ctl_item *f_gravity;
	struct chara_real_clist *f_gravity_vector;
};

struct f_MHD_Coriolis_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_FEM_coriolis_model;
	struct chara_ctl_item *f_FEM_coriolis_implicit;
	struct chara_real_clist *f_system_rotation;
};

struct f_MHD_magneto_cv_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_filterd_induction_ctl;
	struct chara_ctl_item *f_magneto_cv;
	struct chara_real_clist *f_ext_magne;
};

struct f_MHD_magnetic_scale_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct real_ctl_item *f_mag_to_kin_energy_ctl;
};

struct f_MHD_reftemp_point_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct real_ctl_item *f_value;
	struct real_ctl_item *f_depth;
};

struct f_MHD_takepiro_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct real_ctl_item *f_stratified_sigma_ctl;
	struct real_ctl_item *f_stratified_width_ctl;
	struct real_ctl_item *f_stratified_outer_r_ctl;
};

struct f_MHD_temp_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_filterd_advect_ctl;
	struct chara_ctl_item *f_reference_ctl;
	struct chara_ctl_item *f_stratified_ctl;
	struct chara_ctl_item *f_ref_file_ctl;
	struct real_ctl_item  *f_ICB_diffuse_reduction_ctl;
	
	struct f_MHD_reftemp_point_control  *f_low_ctl;
	struct f_MHD_reftemp_point_control  *f_high_ctl;
	struct f_MHD_takepiro_model_control *f_takepiro_ctl;
};


struct f_MHD_time_evo_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_t_evo_field_ctl;
};


struct f_MHD_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_MHD_fields_control         *f_fld_ctl;
	struct f_MHD_time_evo_control       *f_evo_ctl;
	struct f_MHD_t_evo_area_control     *f_earea_ctl;
	struct f_MHD_node_bc_control        *f_nbc_ctl;
	struct f_MHD_surf_bc_control        *f_sbc_ctl;
	struct f_MHD_forces_control         *f_frc_ctl;
	struct f_MHD_dimless_control        *f_dless_ctl;
	struct f_MHD_equations_control      *f_eqs_ctl;
	struct f_MHD_gravity_control        *f_g_ctl;
	struct f_MHD_Coriolis_control       *f_cor_ctl;
	struct f_MHD_magneto_cv_control     *f_mcv_ctl;
	struct f_MHD_magnetic_scale_control *f_bscale_ctl;
	struct f_MHD_temp_model_control     *f_reft_ctl;
	struct f_MHD_temp_model_control     *f_refc_ctl;
	struct f_MHD_SGS_model_control      *f_sgs_ctl;
};


struct f_VIZ_PSF_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *psf_ctl_file_name;
    
    char *f_fname_section_ctl;
    void *f_psf_def_c;
    
    char *f_fname_fld_on_psf;
    void *f_fld_on_psf_c;
    
    struct chara_ctl_item *f_psf_file_head_ctl;
    struct chara_ctl_item *f_psf_output_type_ctl;
};

struct f_VIZ_ISO_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *iso_ctl_file_name;
    
    char *fname_fld_on_iso;
    void *f_fld_on_iso_c;
    
    void *f_iso_def_c;
    struct chara_ctl_item *f_iso_file_head_ctl;
    struct chara_ctl_item *f_iso_output_type_ctl;
};

struct f_VIZ_MAP_ctl{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *map_ctl_file_name;
    
    char *f_fname_mat_ctl;
    void *f_mat;
    
    char *f_fname_cmap_cbar_c;
    void *f_cmap_cbar_c;
    
    void *f_map_define_ctl;
    struct chara_ctl_item *f_map_image_prefix_ctl;
    struct chara_ctl_item *f_map_image_fmt_ctl;
    struct chara_ctl_item *f_map_field_ctl;
    struct chara_ctl_item *f_map_comp_ctl;
    struct chara_ctl_item *f_isoline_field_ctl;
    struct chara_ctl_item *f_isoline_comp_ctl;
};

struct f_VIZ_LIC_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
	char *f_fname_LIC_noise_ctl;
	void *f_noise_ctl;
	char *f_fname_LIC_kernel_ctl;
	void *f_kernel_ctl;
	char *f_fname_vol_repart_ctl;
	void *f_repart_ctl;
    
    int f_num_masking_ctl;
	struct void_clist *f_mask_ctl;
    
    struct chara_ctl_item *f_LIC_field_ctl;
	struct chara_ctl_item *f_subdomain_elapsed_dump_ctl;
	struct chara_ctl_item *f_color_field_ctl;
	struct chara_ctl_item *f_color_component_ctl;
	struct chara_ctl_item *f_opacity_field_ctl;
	struct chara_ctl_item *f_opacity_component_ctl;
    
	struct chara_ctl_item *f_vr_sample_mode_ctl;
	struct real_ctl_item *f_step_size_ctl;
	struct chara_ctl_item *f_normalization_type_ctl;
	struct real_ctl_item *f_normalization_value_ctl;
};

struct f_VIZ_FLINE_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    char *fline_ctl_file_name;
	
    struct chara_ctl_item *f_fline_file_head_ctl;
    struct chara_ctl_item *f_fline_output_type_ctl;
    struct chara_ctl_item *f_fline_field_ctl;
    struct chara_ctl_item *f_fline_color_field_ctl;
    struct chara_ctl_item *f_fline_color_comp_ctl;
    struct chara_clist    *f_fline_area_grp_ctl;
    struct chara_ctl_item *f_starting_type_ctl;
    struct chara_ctl_item *f_selection_type_ctl;
    struct chara_ctl_item *f_line_direction_ctl;
    struct chara_ctl_item *f_start_surf_grp_ctl;
    struct int_ctl_item   *f_num_fieldline_ctl;
    struct int_ctl_item   *f_max_line_stepping_ctl;
    struct real3_clist    *f_seed_point_ctl;
    struct int2_clist     *f_seed_surface_ctl;
};

struct f_VIZ_LIC_PVR_ctl{
    char *lic_ctl_file_name;
	struct f_VIZ_PVR_ctl *f_lic_pvr_ctl;
	struct f_VIZ_LIC_ctl *f_lic_lic_ctl;
};

struct f_MHD_viz_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	int f_num_psf_ctl;
	struct void_clist *f_psf_ctls;
	
	int f_num_iso_ctl;
	struct void_clist *f_iso_ctls;
	
	int f_num_map_ctl;
	struct void_clist *f_map_ctls;
	
	int f_num_pvr_ctl;
	struct void_clist *f_pvr_ctls;
	
	int f_num_lic_ctl;
	struct void_clist *f_lic_ctls;
	
	int f_num_fline_ctl;
	struct void_clist *f_fline_ctls;
	
	void * f_repart_ctl;
	void * f_fname_vol_repart_ctl;
};

struct f_MHD_crust_filter_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item *f_crust_truncation_ctl;
};

struct f_MHD_zm_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_MHD_crust_filter_ctl *f_crust_filter_ctl;
	struct void_clist * f_zm_psf_ctls;
	struct void_clist * f_zRMS_psf_ctls;
	struct void_clist * f_zm_map_ctls;
	struct void_clist * f_zRMS_map_ctls;
};

struct f_MHD_node_monitor_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct real3_clist * f_xx_4_monitor_ctl;
	struct int2_clist * f_node_4_monitor_ctl;
	struct chara_clist * f_group_4_monitor_ctl;
};


struct f_MHD_control{
	void * f_self;
	void * f_addition;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_platform_control *f_plt;
	struct f_platform_control * f_org_plt;
	struct f_platform_control * f_new_plt;
	char * f_fname_psph;
	struct f_MHD_sph_shell_control * f_psph_ctl;
	struct f_MHD_model_control *f_model_ctl;
	struct f_MHD_control_ctls * f_smctl_ctl;
	struct f_MHD_sph_monitor_ctls * f_smonitor_ctl;
	struct f_MHD_node_monitor_ctl * f_nmtr_ctl;
	
	struct f_MHD_viz_ctls * f_viz_ctls;
	struct f_MHD_zm_ctls * f_zm_ctls;
};

struct main_widgets{
	GtkWidget *main_Vbox;
	GtkWidget *open_Hbox;
	GtkWidget *ctl_MHD_Vbox;
    GtkWidget *ctl_MHD_inner_box;
	
//	GtkWidget *expand_vpwrs;
//	GtkWidget *expand_smntr;
	struct f_sph_monitor_widgets *f_lp_vws;
	
	struct block_array_widgets *vpvr_Wgts;
    
    struct chara_cbox_table_view * time_evo_vws;
    struct chara_cbox_table_view * force_vws;
    struct chara_cbox_table_view * ex_magne_vws;
    struct chara_clist *label_time_evo_list;
    struct chara_clist *label_force_list;
    struct chara_clist *label_reftemp_list;
    struct chara_clist *label_xyz_dir_list;
    struct chara_clist *label_file_format_list;
};

void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl,
						  struct main_widgets *mWidgets);

int iflag_read_iso = 0;

struct iso_ctl_GTK{
	struct iso_ctl_c *iso_c;
	
	struct field_ctl_c *iso_field_ctl;
	struct field_views *iso_fields_vws;
	
	struct field_ctl_c *color_field_ctl;
	struct field_views *color_fields_vws;
};


struct iso_ctl_GTK *iso_GTK0;


// struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;

void *MHD_ctl_C;

/*
static gboolean
boolean_to_text (GBinding *binding,
                 const GValue *source,
                 GValue *target,
                 gpointer dummy G_GNUC_UNUSED)
{
	if (g_value_get_boolean (source)){
		g_value_set_string (target, "On");
	}else{
		g_value_set_string (target, "Off");
	}
	
	return TRUE;
}
*/

struct f_MHD_time_evo_control * init_f_MHD_time_evo_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_time_evo_control *f_evo_ctl 
			= (struct f_MHD_time_evo_control *) malloc(sizeof(struct f_MHD_time_evo_control));
	if(f_evo_ctl == NULL){
		printf("malloc error for f_evo_ctl\n");
		exit(0);
	};
	
	f_evo_ctl->f_self =  c_load_self(f_parent);
	
	f_evo_ctl->f_iflag =        (int *) c_MHD_evolution_ctl_iflag(f_evo_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_evolution_ctl_block_name(f_evo_ctl->f_self);
	f_evo_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_evo_ctl->f_t_evo_field_ctl =  init_f_ctl_chara_array(c_MHD_t_evo_field_ctl, f_evo_ctl->f_self);
	return f_evo_ctl;
};


struct f_MHD_t_evo_area_control * init_f_MHD_t_evo_area_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_t_evo_area_control *f_earea_ctl 
			= (struct f_MHD_t_evo_area_control *) malloc(sizeof(struct f_MHD_t_evo_area_control));
	if(f_earea_ctl == NULL){
		printf("malloc error for f_earea_ctl\n");
		exit(0);
	};
	
	f_earea_ctl->f_self =  c_load_self(f_parent);
	
	f_earea_ctl->f_iflag = (int *) c_MHD_evo_area_ctl_iflag(f_earea_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_evo_area_ctl_block_name(f_earea_ctl->f_self);
	f_earea_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_earea_ctl->f_evo_fluid_group_ctl =    init_f_ctl_chara_array(c_MHD_evo_fluid_group_ctl,
																   f_earea_ctl->f_self);
	f_earea_ctl->f_evo_conduct_group_ctl =  init_f_ctl_chara_array(c_MHD_evo_conduct_group_ctl, 
																   f_earea_ctl->f_self);
	return f_earea_ctl;
};

struct f_MHD_gravity_control * init_f_MHD_gravity_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_gravity_control *f_g_ctl 
			= (struct f_MHD_gravity_control *) malloc(sizeof(struct f_MHD_gravity_control));
	if(f_g_ctl == NULL){
		printf("malloc error for f_g_ctl\n");
		exit(0);
	};
	
	f_g_ctl->f_self =  c_load_self(f_parent);
	
	f_g_ctl->f_iflag =        (int *) c_MHD_gravity_ctl_iflag(f_g_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_gravity_ctl_block_name(f_g_ctl->f_self);
	f_g_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_g_ctl->f_FEM_gravity_model = init_f_ctl_chara_item(c_MHD_FEM_gravity_model,
														 f_g_ctl->f_self);
	f_g_ctl->f_gravity =           init_f_ctl_chara_item(c_MHD_gravity_ctl_gravity,
														 f_g_ctl->f_self);
	f_g_ctl->f_gravity_vector =    init_f_ctl_cr_array(c_MHD_gravity_ctl_vector,
													   f_g_ctl->f_self);
	return f_g_ctl;
};

struct f_MHD_Coriolis_control * init_f_MHD_Coriolis_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_Coriolis_control *f_cor_ctl 
			= (struct f_MHD_Coriolis_control *) malloc(sizeof(struct f_MHD_Coriolis_control));
	if(f_cor_ctl == NULL){
		printf("malloc error for f_cor_ctl\n");
		exit(0);
	};
	
	f_cor_ctl->f_self =  c_load_self(f_parent);
	
	f_cor_ctl->f_iflag =        (int *) c_MHD_coriolis_ctl_iflag(f_cor_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_coriolis_ctl_block_name(f_cor_ctl->f_self);
	f_cor_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_cor_ctl->f_FEM_coriolis_model = init_f_ctl_chara_item(c_MHD_FEM_coriolis_model,
															f_cor_ctl->f_self);
	f_cor_ctl->f_FEM_coriolis_implicit = init_f_ctl_chara_item(c_MHD_FEM_coriolis_implicit,
															   f_cor_ctl->f_self);
	f_cor_ctl->f_system_rotation = init_f_ctl_cr_array(c_MHD_system_rotation,
													   f_cor_ctl->f_self);
	return f_cor_ctl;
};

struct f_MHD_magneto_cv_control * init_f_MHD_magneto_cv_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_magneto_cv_control *f_mcv_ctl 
			= (struct f_MHD_magneto_cv_control *) malloc(sizeof(struct f_MHD_magneto_cv_control));
	if(f_mcv_ctl == NULL){
		printf("malloc error for f_mcv_ctl\n");
		exit(0);
	};
	
	f_mcv_ctl->f_self =  c_load_self(f_parent);
	
	f_mcv_ctl->f_iflag =        (int *) c_MHD_mag_cv_ctl_iflag(f_mcv_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_mag_cv_ctl_block_name(f_mcv_ctl->f_self);
	f_mcv_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_mcv_ctl->f_filterd_induction_ctl = init_f_ctl_chara_item(c_MHD_mag_cv_filterd_ctl,
															   f_mcv_ctl->f_self);
	f_mcv_ctl->f_magneto_cv = init_f_ctl_chara_item(c_MHD_mag_cv_ctl_magneto_cv,
													f_mcv_ctl->f_self);
	f_mcv_ctl->f_ext_magne = init_f_ctl_cr_array(c_MHD_mag_cv_ctl_ext_magne,
												 f_mcv_ctl->f_self);
	return f_mcv_ctl;
};

struct f_MHD_magnetic_scale_control * init_f_MHD_magnetic_scale_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_magnetic_scale_control *f_bscale_ctl 
			= (struct f_MHD_magnetic_scale_control *) malloc(sizeof(struct f_MHD_magnetic_scale_control));
	if(f_bscale_ctl == NULL){
		printf("malloc error for f_bscale_ctl\n");
		exit(0);
	};
	
	f_bscale_ctl->f_self =  c_load_self(f_parent);
	
	f_bscale_ctl->f_iflag =        (int *) c_MHD_B_scale_ctl_iflag(f_bscale_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_B_scale_ctl_block_name(f_bscale_ctl->f_self);
	f_bscale_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_bscale_ctl->f_mag_to_kin_energy_ctl = init_f_ctl_real_item(c_MHD_B_scale_mag_to_kin_ctl,
																 f_bscale_ctl->f_self);
	return f_bscale_ctl;
};

struct f_MHD_reftemp_point_control * init_f_MHD_reftemp_point_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_reftemp_point_control *f_refs_ctl 
			= (struct f_MHD_reftemp_point_control *) malloc(sizeof(struct f_MHD_reftemp_point_control));
	if(f_refs_ctl == NULL){
		printf("malloc error for f_refs_ctl\n");
		exit(0);
	};
	
	f_refs_ctl->f_self =  c_load_self(f_parent);
	
	f_refs_ctl->f_iflag =        (int *) c_reftemp_point_ctl_iflag(f_refs_ctl->f_self);
	char *f_block_name =   (char *) c_reftemp_point_ctl_block_name(f_refs_ctl->f_self);
	f_refs_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_refs_ctl->f_value = init_f_ctl_real_item(c_reftemp_point_value_ctl,
											   f_refs_ctl->f_self);
	f_refs_ctl->f_depth = init_f_ctl_real_item(c_reftemp_point_depth_ctl,
											   f_refs_ctl->f_self);
	return f_refs_ctl;
};

struct f_MHD_takepiro_model_control * init_f_MHD_takepiro_model_control(void *(*c_load_self)(void *f_parent), 
                                                                        void *f_parent)
{
	struct f_MHD_takepiro_model_control *f_bscale_ctl 
			= (struct f_MHD_takepiro_model_control *) malloc(sizeof(struct f_MHD_takepiro_model_control));
	if(f_bscale_ctl == NULL){
		printf("malloc error for f_bscale_ctl\n");
		exit(0);
	};
	
	f_bscale_ctl->f_self =  c_load_self(f_parent);
	
	f_bscale_ctl->f_iflag =        (int *) c_takepiro_model_ctl_iflag(f_bscale_ctl->f_self);
	char *f_block_name =   (char *) c_takepiro_model_ctl_block_name(f_bscale_ctl->f_self);
	f_bscale_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_bscale_ctl->f_stratified_sigma_ctl = init_f_ctl_real_item(c_takepiro_stratified_sigma_ctl,
																f_bscale_ctl->f_self);
	f_bscale_ctl->f_stratified_width_ctl = init_f_ctl_real_item(c_takepiro_stratified_width_ctl,
																f_bscale_ctl->f_self);
	f_bscale_ctl->f_stratified_outer_r_ctl = init_f_ctl_real_item(c_takepiro_stratified_rout_ctl,
																  f_bscale_ctl->f_self);
	return f_bscale_ctl;
};

struct f_MHD_temp_model_control * init_f_MHD_temp_model_control(void *(*c_load_self)(void *f_parent), 
																void *f_parent)
{
	struct f_MHD_temp_model_control *f_reft_ctl 
			= (struct f_MHD_temp_model_control *) malloc(sizeof(struct f_MHD_temp_model_control));
	if(f_reft_ctl == NULL){
		printf("malloc error for f_reft_ctl\n");
		exit(0);
	};
	
	f_reft_ctl->f_self =  c_load_self(f_parent);
	
	f_reft_ctl->f_iflag =        (int *) c_temp_model_ctl_iflag(f_reft_ctl->f_self);
	char *f_block_name =   (char *) c_temp_model_ctl_block_name(f_reft_ctl->f_self);
	f_reft_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_reft_ctl->f_filterd_advect_ctl = init_f_ctl_chara_item(c_temp_model_filter_advect_ctl,
															  f_reft_ctl->f_self);
	f_reft_ctl->f_reference_ctl = init_f_ctl_chara_item(c_temp_model_reference_ctl,
															  f_reft_ctl->f_self);
	f_reft_ctl->f_stratified_ctl = init_f_ctl_chara_item(c_temp_model_stratified_ctl,
															  f_reft_ctl->f_self);
	f_reft_ctl->f_ref_file_ctl = init_f_ctl_chara_item(c_temp_model_ref_file_ctl,
															  f_reft_ctl->f_self);
	f_reft_ctl->f_ICB_diffuse_reduction_ctl = init_f_ctl_real_item(c_temp_model_ICB_diffuse_ctl,
																   f_reft_ctl->f_self);
	
	f_reft_ctl->f_low_ctl = init_f_MHD_reftemp_point_control(c_temp_model_low_ctl,
															 f_reft_ctl->f_self);
	f_reft_ctl->f_high_ctl = init_f_MHD_reftemp_point_control(c_temp_model_high_ctl,
															  f_reft_ctl->f_self);
	f_reft_ctl->f_takepiro_ctl = init_f_MHD_takepiro_model_control(c_temp_model_takepiro_ctl,
																   f_reft_ctl->f_self);
	return f_reft_ctl;
};


struct f_MHD_model_control * init_f_MHD_model_ctl(void *(*c_load_self)(void *f_parent), 
												  void *f_parent, void *f_addition)
{
	struct f_MHD_model_control *f_model_ctl 
			= (struct f_MHD_model_control *) malloc(sizeof(struct f_MHD_model_control));
	if(f_model_ctl == NULL){
		printf("malloc error for f_model_ctl\n");
		exit(0);
	};
	
	f_model_ctl->f_self =  c_load_self(f_parent);
	
	f_model_ctl->f_iflag =        (int *) c_MHD_mdl_iflag(f_model_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_mdl_block_name(f_model_ctl->f_self);
	f_model_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_model_ctl->f_fld_ctl =    init_f_MHD_fields_control(c_MHD_mdl_fld_ctl, f_model_ctl->f_self);
	f_model_ctl->f_evo_ctl =    init_f_MHD_time_evo_control(c_MHD_mdl_evo_ctl, f_model_ctl->f_self);
	f_model_ctl->f_earea_ctl =  init_f_MHD_t_evo_area_control(c_MHD_mdl_earea_ctl, f_model_ctl->f_self);
	f_model_ctl->f_nbc_ctl =    init_f_MHD_node_bc_control(c_MHD_mdl_nbc_ctl, f_model_ctl->f_self);
	f_model_ctl->f_sbc_ctl =    init_f_MHD_surf_bc_control(c_MHD_mdl_sbc_ctl, f_model_ctl->f_self);
	f_model_ctl->f_frc_ctl =    init_f_MHD_forces_ctl(c_MHD_mdl_frc_ctl, f_model_ctl->f_self);
	f_model_ctl->f_dless_ctl =  init_f_MHD_dimless_ctl(c_MHD_mdl_dless_ctl, f_model_ctl->f_self);
	f_model_ctl->f_eqs_ctl =    init_f_MHD_equations_ctl(c_MHD_mdl_eqs_ctl, f_model_ctl->f_self);
	f_model_ctl->f_g_ctl =      init_f_MHD_gravity_control(c_MHD_mdl_g_ctl, f_model_ctl->f_self);
	f_model_ctl->f_cor_ctl =    init_f_MHD_Coriolis_control(c_MHD_mdl_cor_ctl, f_model_ctl->f_self);
	f_model_ctl->f_mcv_ctl =    init_f_MHD_magneto_cv_control(c_MHD_mdl_mcv_ctl, f_model_ctl->f_self);
	f_model_ctl->f_bscale_ctl = init_f_MHD_magnetic_scale_control(c_MHD_mdl_bscale_ctl, f_model_ctl->f_self);
	f_model_ctl->f_reft_ctl =   init_f_MHD_temp_model_control(c_MHD_mdl_reft_ctl, f_model_ctl->f_self);
	f_model_ctl->f_refc_ctl =   init_f_MHD_temp_model_control(c_MHD_mdl_refc_ctl, f_model_ctl->f_self);
	f_model_ctl->f_sgs_ctl =    init_f_MHD_SGS_model_control(c_MHD_sgs_ctl, f_addition);
	return f_model_ctl;
}


struct f_VIZ_PSF_ctl * init_f_VIZ_PSF_ctl(int idx, void *f_parent)
{
	struct f_VIZ_PSF_ctl *f_psf_ctl 
			= (struct f_VIZ_PSF_ctl *) malloc(sizeof(struct f_VIZ_PSF_ctl));
	if(f_psf_ctl == NULL){
		printf("malloc error for f_VIZ_PSF_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_section_ctls_fname(idx, f_parent);
    f_psf_ctl->psf_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_psf_ctl->f_self =  c_section_ctls_psf_ctl(idx, f_parent);
	
	f_psf_ctl->f_iflag =   (int *) c_VIZ_PSF_ctl_iflag(f_psf_ctl->f_self);
	f_block_name =   (char *) c_VIZ_PSF_ctl_block_name(f_psf_ctl->f_self);
	f_psf_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    
	f_psf_ctl->f_fname_section_ctl = c_VIZ_PSF_fname_section_ctl(f_psf_ctl->f_self);
	f_psf_ctl->f_psf_def_c =         c_VIZ_PSF_psf_def_c(f_psf_ctl->f_self);
    
    f_psf_ctl->f_fname_fld_on_psf =  c_VIZ_PSF_fname_fld_on_psf(f_psf_ctl->f_self);
    f_psf_ctl->f_fld_on_psf_c =      c_VIZ_PSF_fld_on_psf_c(f_psf_ctl->f_self);
    
    f_psf_ctl->f_psf_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_PSF_file_head_ctl,
                                                             f_psf_ctl->f_self);
    f_psf_ctl->f_psf_output_type_ctl = init_f_ctl_chara_item(c_VIZ_PSF_output_type_ctl,
                                                             f_psf_ctl->f_self);
    return f_psf_ctl;
};


struct void_clist * init_f_VIZ_psf_ctls(void *f_parent, int *f_num_psf_ctl)
{
    char *f_block_name =   (char *) c_section_ctls_block_name(f_parent);
	struct void_clist *f_psf_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_psf_ctls->f_parent = f_parent;
	*f_num_psf_ctl = c_section_ctls_num_psf_ctl(f_psf_ctls->f_parent);
	
    struct f_VIZ_PSF_ctl *f_ctl_tmp;
    int i;
	for(i=0;i<*f_num_psf_ctl;i++){
        f_ctl_tmp = init_f_VIZ_PSF_ctl(i, f_psf_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_psf_ctls);
	}
	return f_psf_ctls;
}

struct f_VIZ_ISO_ctl * init_f_VIZ_ISO_ctl(int idx, void *f_parent)
{
	struct f_VIZ_ISO_ctl *f_iso_ctl 
			= (struct f_VIZ_ISO_ctl *) malloc(sizeof(struct f_VIZ_ISO_ctl));
	if(f_iso_ctl == NULL){
		printf("malloc error for f_VIZ_ISO_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_isosurf_ctls_fname(idx, f_parent);
    f_iso_ctl->iso_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_iso_ctl->f_self =  c_isosurf_ctls_iso_ctl(idx, f_parent);
	
	f_iso_ctl->f_iflag =   (int *) c_VIZ_ISO_ctl_iflag(f_iso_ctl->f_self);
	f_block_name =   (char *) c_VIZ_ISO_ctl_block_name(f_iso_ctl->f_self);
	f_iso_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_iso_ctl->fname_fld_on_iso = c_VIZ_ISO_fname_fld_on_iso(f_iso_ctl->f_self);
    f_iso_ctl->f_fld_on_iso_c =   c_VIZ_ISO_fld_on_iso_c(f_iso_ctl->f_self);
    f_iso_ctl->f_iso_def_c =      c_VIZ_ISO_iso_def_ctl(f_iso_ctl->f_self);
    
    f_iso_ctl->f_iso_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_ISO_file_head_ctl,
                                                             f_iso_ctl->f_self);
    f_iso_ctl->f_iso_output_type_ctl = init_f_ctl_chara_item(c_VIZ_ISO_output_type_ctl,
                                                             f_iso_ctl->f_self);
    return f_iso_ctl;
}


struct void_clist * init_f_VIZ_iso_ctls(void *f_parent, int *f_num_iso_ctl)
{
    char *f_block_name =   (char *) c_isosurf_ctls_block_name(f_parent);
	struct void_clist *f_iso_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_iso_ctls->f_parent = f_parent;
	*f_num_iso_ctl = c_isosurf_ctls_num_iso_ctl(f_iso_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_iso_ctl;i++){
        struct f_VIZ_ISO_ctl *f_ctl_tmp = init_f_VIZ_ISO_ctl(i, f_iso_ctls->f_parent);
        append_void_clist((void *) f_ctl_tmp, f_iso_ctls);
	}
	return f_iso_ctls;
}

struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl(int idx, void *f_parent)
{
	struct f_VIZ_MAP_ctl *f_map_ctl 
			= (struct f_VIZ_MAP_ctl *) malloc(sizeof(struct f_VIZ_MAP_ctl));
	if(f_map_ctl == NULL){
		printf("malloc error for f_VIZ_MAP_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_map_render_ctls_fname(idx, f_parent);
    f_map_ctl->map_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_map_ctl->f_self =  c_map_render_ctls_map_ctl(idx, f_parent);
	
	f_map_ctl->f_iflag =   (int *) c_VIZ_MAP_ctl_iflag(f_map_ctl->f_self);
	f_block_name =   (char *) c_VIZ_MAP_ctl_block_name(f_map_ctl->f_self);
	f_map_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_map_ctl->f_map_define_ctl = c_VIZ_MAP_map_define_ctl(f_map_ctl->f_self);
    f_map_ctl->f_fname_mat_ctl = c_VIZ_MAP_fname_mat_ctl(f_map_ctl->f_self);
    f_map_ctl->f_mat =           c_VIZ_MAP_viewmat_ctl(f_map_ctl->f_self);
    f_map_ctl->f_fname_cmap_cbar_c = c_VIZ_MAP_fname_cmap_cbar_c(f_map_ctl->f_self);
    f_map_ctl->f_cmap_cbar_c =       c_VIZ_MAP_cmap_cbar_c(f_map_ctl->f_self);
    
    f_map_ctl->f_map_image_prefix_ctl = init_f_ctl_chara_item(c_VIZ_MAP_map_image_prefix_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_image_fmt_ctl =    init_f_ctl_chara_item(c_VIZ_MAP_map_image_fmt_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_field_ctl =        init_f_ctl_chara_item(c_VIZ_MAP_map_field_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_map_comp_ctl =         init_f_ctl_chara_item(c_VIZ_MAP_map_comp_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_isoline_field_ctl =    init_f_ctl_chara_item(c_VIZ_MAP_isoline_field_ctl,
                                                              f_map_ctl->f_self);
    f_map_ctl->f_isoline_comp_ctl =     init_f_ctl_chara_item(c_VIZ_MAP_isoline_comp_ctl,
                                                              f_map_ctl->f_self);
    return f_map_ctl;
};

struct void_clist * init_f_VIZ_map_ctls(void *f_parent, int *f_num_map_ctl)
{
    char *f_block_name =   (char *) c_map_render_ctls_block_name(f_parent);
	struct void_clist *f_map_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_map_ctls->f_parent = f_parent;
	*f_num_map_ctl = c_map_render_ctls_num_map_ctl(f_map_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_map_ctl;i++){
        struct f_VIZ_MAP_ctl *f_ctl_tmp = init_f_VIZ_MAP_ctl(i, f_map_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_map_ctls);
	}
	return f_map_ctls;
}


struct f_VIZ_LIC_ctl * init_f_VIZ_LIC_ctl(int idx, void *f_parent)
{
	struct f_VIZ_LIC_ctl *f_lic_lic_ctl 
			= (struct f_VIZ_LIC_ctl *) malloc(sizeof(struct f_VIZ_LIC_ctl));
	if(f_lic_lic_ctl == NULL){
		printf("malloc error for f_VIZ_LIC_ctl\n");
		exit(0);
	};
	
	f_lic_lic_ctl->f_self =  c_lic_render_ctls_lic_ctl(idx, f_parent);
	
	f_lic_lic_ctl->f_iflag =   (int *) c_VIZ_LIC_ctl_iflag(f_lic_lic_ctl->f_self);
	char *f_block_name =   (char *) c_VIZ_LIC_ctl_block_name(f_lic_lic_ctl->f_self);
	f_lic_lic_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_block_name = (char *) c_VIZ_LIC_fname_noise_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_fname_LIC_noise_ctl = strngcopy_from_f(f_block_name);
    f_lic_lic_ctl->f_noise_ctl =       c_VIZ_LIC_noise_ctl(f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_kernel_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_fname_LIC_kernel_ctl = strngcopy_from_f(f_block_name);
    f_lic_lic_ctl->f_kernel_ctl =       c_VIZ_LIC_kernel_ctl(f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_vol_repart_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_fname_vol_repart_ctl = strngcopy_from_f(f_block_name);
    f_lic_lic_ctl->f_repart_ctl =           c_VIZ_LIC_repartition_ctl(f_lic_lic_ctl->f_self);
    
    f_lic_lic_ctl->f_num_masking_ctl = c_VIZ_LIC_num_masking_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_mask_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_lic_lic_ctl->f_mask_ctl->f_parent = f_lic_lic_ctl->f_self;
    int i;
    for(i=0;i<f_lic_lic_ctl->f_num_masking_ctl;i++){
        void *f_ctl_tmp = c_VIZ_LIC_mask_ctl(i, f_lic_lic_ctl->f_self);
		append_void_clist((void *) f_ctl_tmp, f_lic_lic_ctl->f_mask_ctl);
    };
    
    f_lic_lic_ctl->f_LIC_field_ctl = init_f_ctl_chara_item(c_VIZ_LIC_LIC_field_ctl,
                                                           f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_subdomain_elapsed_dump_ctl = init_f_ctl_chara_item(c_VIZ_LIC_pe_elapsed_dump_ctl,
                                                                        f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_color_field_ctl =       init_f_ctl_chara_item(c_VIZ_LIC_color_field_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_color_component_ctl =   init_f_ctl_chara_item(c_VIZ_LIC_color_comp_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_opacity_field_ctl =     init_f_ctl_chara_item(c_VIZ_LIC_opacity_field_ctl,
                                                                   f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_opacity_component_ctl = init_f_ctl_chara_item(c_VIZ_LIC_opacity_comp_ctl,
                                                                   f_lic_lic_ctl->f_self);
    
    f_lic_lic_ctl->f_vr_sample_mode_ctl = init_f_ctl_chara_item(c_VIZ_LIC_vr_sample_mode_ctl,
                                                                f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_step_size_ctl =      init_f_ctl_real_item(c_VIZ_LIC_step_size_ctl,
                                                               f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_normalization_type_ctl =   init_f_ctl_chara_item(c_VIZ_LIC_normalize_type_ctl,
                                                                      f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_normalization_value_ctl = init_f_ctl_real_item(c_VIZ_LIC_normalize_value_ctl,
                                                                    f_lic_lic_ctl->f_self);
    return f_lic_lic_ctl;
};

struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl(int idx, void *f_parent)
{
	struct f_VIZ_LIC_PVR_ctl *f_lic_ctl
			= (struct f_VIZ_LIC_PVR_ctl *) malloc(sizeof(struct f_VIZ_LIC_PVR_ctl));
	if(f_lic_ctl == NULL){
		printf("malloc error for f_VIZ_LIC_PVR_ctl\n");
		exit(0);
	};
    
    char *f_block_name = c_lic_render_ctls_fname(idx, f_parent);
    f_lic_ctl->lic_ctl_file_name =  strngcopy_from_f(f_block_name);
    f_lic_ctl->f_lic_pvr_ctl = init_f_VIZ_PVR_ctl(c_lic_render_ctls_pvr_ctl,
                                                  idx, f_parent);
    f_lic_ctl->f_lic_lic_ctl =  init_f_VIZ_LIC_ctl(idx, f_parent);
    return f_lic_ctl;
};

struct void_clist * init_f_VIZ_lic_ctls(void *f_parent, int *f_num_lic_ctl)
{
    char *f_block_name =   (char *) c_lic_render_ctls_block_name(f_parent);
	struct void_clist *f_lic_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_lic_ctls->f_parent = f_parent;
	*f_num_lic_ctl = c_lic_render_ctls_num_lic_ctl(f_lic_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_lic_ctl;i++){
        struct f_VIZ_LIC_PVR_ctl *f_ctl_tmp = init_f_VIZ_LIC_PVR_ctl(i, f_lic_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_lic_ctls);
	}
	return f_lic_ctls;
}

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl(int idx, void *f_parent)
{
	struct f_VIZ_FLINE_ctl *f_fline_ctl 
			= (struct f_VIZ_FLINE_ctl *) malloc(sizeof(struct f_VIZ_FLINE_ctl));
	if(f_fline_ctl == NULL){
		printf("malloc error for f_VIZ_FLINE_ctl\n");
		exit(0);
	};
	
    char *f_block_name = (char *) c_fline_ctls_fname(idx, f_parent);
    f_fline_ctl->fline_ctl_file_name =  strngcopy_from_f(f_block_name);
	f_fline_ctl->f_self =  c_fline_ctls_fline_ctl(idx, f_parent);
	
	f_fline_ctl->f_iflag =   (int *) c_VIZ_FLINE_ctl_iflag(f_fline_ctl->f_self);
	f_block_name =   (char *) c_VIZ_FLINE_ctl_block_name(f_fline_ctl->f_self);
	f_fline_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_fline_ctl->f_fline_file_head_ctl =   init_f_ctl_chara_item(c_VIZ_FLINE_file_head_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_output_type_ctl = init_f_ctl_chara_item(c_VIZ_FLINE_output_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_field_ctl =       init_f_ctl_chara_item(c_VIZ_FLINE_field_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_color_field_ctl = init_f_ctl_chara_item(c_VIZ_FLINE_color_field_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_color_comp_ctl =  init_f_ctl_chara_item(c_VIZ_FLINE_color_comp_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_fline_area_grp_ctl =    init_f_ctl_chara_array(c_VIZ_FLINE_area_grp_ctl,
																  f_fline_ctl->f_self);
    f_fline_ctl->f_starting_type_ctl =     init_f_ctl_chara_item(c_VIZ_FLINE_starting_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_selection_type_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_selection_type_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_line_direction_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_line_direction_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_start_surf_grp_ctl =    init_f_ctl_chara_item(c_VIZ_FLINE_start_surf_grp_ctl,
																 f_fline_ctl->f_self);
    f_fline_ctl->f_num_fieldline_ctl =     init_f_ctl_int_item(c_VIZ_FLINE_num_fieldline_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_max_line_stepping_ctl = init_f_ctl_int_item(c_VIZ_FLINE_max_line_step_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_seed_point_ctl =        init_f_ctl_r3_array(c_VIZ_FLINE_seed_point_ctl,
															   f_fline_ctl->f_self);
    f_fline_ctl->f_seed_surface_ctl =      init_f_ctl_i2_array(c_VIZ_FLINE_seed_surface_ctl,
															   f_fline_ctl->f_self);
	return f_fline_ctl;
}

struct void_clist * init_f_VIZ_fline_ctls(void *f_parent, int *f_num_fline_ctl)
{
    char *f_block_name =   (char *) c_fline_ctls_block_name(f_parent);
	struct void_clist *f_fline_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_fline_ctls->f_parent = f_parent;
	*f_num_fline_ctl = c_fline_ctls_num_fline_ctl(f_fline_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_fline_ctl;i++){
		struct f_VIZ_FLINE_ctl *f_ctl_tmp = init_f_VIZ_FLINE_ctl(i, f_fline_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_fline_ctls);
	}
	return f_fline_ctls;
}


struct void_clist * init_f_VIZ_pvr_ctls(void *f_parent, int *f_num_pvr_ctl)
{
    char *f_block_name =   (char *) c_pvr_render_ctls_block_name(f_parent);
	struct void_clist *f_pvr_ctls = init_void_clist(strngcopy_from_f(f_block_name));
	f_pvr_ctls->f_parent = f_parent;
	*f_num_pvr_ctl = c_pvr_render_ctls_num_pvr_ctl(f_pvr_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_pvr_ctl;i++){
		struct f_VIZ_PVR_ctl *f_ctl_tmp = init_f_VIZ_PVR_ctl(c_pvr_render_ctls_pvr_ctl,
                                                             i, f_pvr_ctls->f_parent);
		append_void_clist((void *) f_ctl_tmp, f_pvr_ctls);
	}
	return f_pvr_ctls;
}

struct f_MHD_viz_ctls * init_f_MHD_viz_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_viz_ctls *f_viz_ctls 
			= (struct f_MHD_viz_ctls *) malloc(sizeof(struct f_MHD_viz_ctls));
	if(f_viz_ctls == NULL){
		printf("malloc error for f_viz_ctls\n");
		exit(0);
	};
	
	f_viz_ctls->f_self =  c_load_self(f_parent);
	printf("f_viz_ctls->f_self %p\n", f_viz_ctls->f_self);
	
	f_viz_ctls->f_iflag =        (int *) c_visualizations_iflag(f_viz_ctls->f_self);
	char *f_block_name =   (char *) c_visualizations_block_name(f_viz_ctls->f_self);
	f_viz_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
    f_viz_ctls->f_psf_ctls =   init_f_VIZ_psf_ctls(c_visualizations_psf_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_psf_ctl);
    f_viz_ctls->f_iso_ctls =   init_f_VIZ_iso_ctls(c_visualizations_iso_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_iso_ctl);
    f_viz_ctls->f_map_ctls =   init_f_VIZ_map_ctls(c_visualizations_map_ctls(f_viz_ctls->f_self), 
                                                   &f_viz_ctls->f_num_map_ctl);
    f_viz_ctls->f_pvr_ctls =   init_f_VIZ_pvr_ctls(c_visualizations_pvr_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_pvr_ctl);
    f_viz_ctls->f_lic_ctls =   init_f_VIZ_lic_ctls(c_visualizations_lic_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_lic_ctl);
    f_viz_ctls->f_fline_ctls = init_f_VIZ_fline_ctls(c_visualizations_fline_ctls(f_viz_ctls->f_self),
                                                     &f_viz_ctls->f_num_fline_ctl);
	f_viz_ctls->f_repart_ctl =    c_visualizations_repart_ctl(f_viz_ctls->f_self);
	f_viz_ctls->f_fname_vol_repart_ctl = c_visualizations_fname_vrepart(f_viz_ctls->f_self);
	return f_viz_ctls;
}



struct f_MHD_crust_filter_ctl * init_f_MHD_crust_filter_ctl(void *(*c_load_self)(void *f_parent),
                                                            void *f_parent)
{
	struct f_MHD_crust_filter_ctl *f_crust_filter_ctl 
			= (struct f_MHD_crust_filter_ctl *) malloc(sizeof(struct f_MHD_crust_filter_ctl));
	if(f_crust_filter_ctl == NULL){
		printf("malloc error for f_MHD_crust_filter_ctl\n");
		exit(0);
	};
	
	f_crust_filter_ctl->f_self =  c_load_self(f_parent);
	
	f_crust_filter_ctl->f_iflag =   (int *) c_clust_filter_ctl_iflag(f_crust_filter_ctl->f_self);
	char *f_block_name =   (char *) c_clust_filter_ctl_block_name(f_crust_filter_ctl->f_self);
	f_crust_filter_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_crust_filter_ctl->f_crust_truncation_ctl = init_f_ctl_int_item(c_clust_filter_ltr_ctl,
                                                                     f_crust_filter_ctl->f_self);
    return f_crust_filter_ctl;
};


struct f_MHD_zm_ctls * init_f_MHD_zm_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_zm_ctls *f_zm_ctls 
			= (struct f_MHD_zm_ctls *) malloc(sizeof(struct f_MHD_zm_ctls));
	if(f_zm_ctls == NULL){
		printf("malloc error for f_zm_ctls\n");
		exit(0);
	};
	
	f_zm_ctls->f_self =  c_load_self(f_parent);
	
	f_zm_ctls->f_iflag =        (int *) c_dynamo_vizs_iflag(f_zm_ctls->f_self);
	char *f_block_name =   (char *) c_dynamo_vizs_block_name(f_zm_ctls->f_self);
	f_zm_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
    f_zm_ctls->f_crust_filter_ctl = init_f_MHD_crust_filter_ctl(c_dynamo_vizs_crust_filter_ctl,
                                                                f_zm_ctls->f_self);
    
    int n_single = 1;
    f_zm_ctls->f_zm_psf_ctls =   init_f_VIZ_psf_ctls(c_dynamo_vizs_zm_psf_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    f_zm_ctls->f_zRMS_psf_ctls = init_f_VIZ_psf_ctls(c_dynamo_vizs_zRMS_psf_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    
    f_zm_ctls->f_zm_map_ctls =   init_f_VIZ_map_ctls(c_dynamo_vizs_zm_map_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    f_zm_ctls->f_zRMS_map_ctls = init_f_VIZ_map_ctls(c_dynamo_vizs_zRMS_map_ctls(f_zm_ctls->f_self),
                                                     &n_single);
	return f_zm_ctls;
}

struct f_MHD_node_monitor_ctl * init_f_MHD_node_monitor_ctl(void *(*c_load_self)(void *f_parent),
															void *f_parent)
{
	struct f_MHD_node_monitor_ctl *f_nmtr_ctl 
			= (struct f_MHD_node_monitor_ctl *) malloc(sizeof(struct f_MHD_node_monitor_ctl));
	if(f_nmtr_ctl == NULL){
		printf("malloc error for f_nmtr_ctl\n");
		exit(0);
	};
	
	f_nmtr_ctl->f_self =  c_load_self(f_parent);
	
	f_nmtr_ctl->f_iflag =        (int *) c_node_monitor_ctl_iflag(f_nmtr_ctl->f_self);
	char *f_block_name =   (char *) c_node_monitor_ctl_block_name(f_nmtr_ctl->f_self);
	f_nmtr_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
    f_nmtr_ctl->f_xx_4_monitor_ctl =    init_f_ctl_r3_array(c_node_monitor_xx_ctl,
                                                            f_nmtr_ctl->f_self);
    f_nmtr_ctl->f_node_4_monitor_ctl =  init_f_ctl_i2_array(c_node_monitor_node_ctl,
                                                            f_nmtr_ctl->f_self);
    f_nmtr_ctl->f_group_4_monitor_ctl = init_f_ctl_chara_array(c_node_monitor_group_ctl,
                                                               f_nmtr_ctl->f_self);
	return f_nmtr_ctl;
}

static void set_f_MHD_control(struct f_MHD_control *f_MHD_ctl)
{
	f_MHD_ctl->f_iflag =        (int *) c_MHD_iflag(f_MHD_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_block_name(f_MHD_ctl->f_self);
	f_MHD_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_MHD_ctl->f_plt =          init_f_platform_control(c_MHD_plt, f_MHD_ctl->f_self);
	f_MHD_ctl->f_org_plt =      init_f_platform_control(c_MHD_org_plt, f_MHD_ctl->f_self);
	f_MHD_ctl->f_new_plt =      init_f_platform_control(c_MHD_new_plt, f_MHD_ctl->f_self);
	f_MHD_ctl->f_fname_psph =   (char *) c_MHD_fname_psph(f_MHD_ctl->f_self);
	f_MHD_ctl->f_psph_ctl =     init_f_MHD_sph_shell_ctl(c_MHD_psph_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_model_ctl =    init_f_MHD_model_ctl(c_MHD_model_ctl, f_MHD_ctl->f_self,
													 f_MHD_ctl->f_addition);
	f_MHD_ctl->f_smctl_ctl =    init_f_MHD_control_ctls(c_MHD_smctl_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_smonitor_ctl = init_f_MHD_sph_monitor_ctls(c_MHD_smonitor_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_nmtr_ctl =     init_f_MHD_node_monitor_ctl(c_MHD_nmtr_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_viz_ctls =     init_f_MHD_viz_ctls(c_MHD_viz_ctls, f_MHD_ctl->f_addition);
	f_MHD_ctl->f_zm_ctls =      init_f_MHD_zm_ctls(c_MHD_zm_ctls, f_MHD_ctl->f_addition);
	return;
}




static void cb_View(GtkButton *button, gpointer data)
{
	c_view_control_sph_SGS_MHD();
}
static void cb_Open(GtkButton *button, gpointer data)
{
	GtkWidget *dialog;
	GtkWidget *parent;
	GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN,
									 GTK_FILE_CHOOSER_ACTION_SAVE,
									 GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
									 GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *read_file_name;
  gchar *folder;
	
	
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	struct main_widgets *mWidgets = (struct main_widgets *) g_object_get_data(G_OBJECT(data), "mWidgets");
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) g_object_get_data(G_OBJECT(data), "MHD_ctl");
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[0],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);

	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name  */
		read_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog),
											read_file_name);
		g_print( "file name: %s\n", read_file_name);
		
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
		if (folder == NULL) {
			int length = strlen(read_file_name);
			char *stripped_filehead = (char *) calloc(length+1, sizeof(char));
			char *stripped_dir = (char *) calloc(length+1, sizeof(char));
			split_dir_and_file_name_c((char *) read_file_name, 
									  stripped_dir, stripped_filehead);
			printf("Folder %s\n", stripped_dir);
			chdir(stripped_dir);
		} else {
			g_print( "folder name: %s\n", folder);
			chdir(folder);
		}
		/* Get Folder name */
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
		f_MHD_ctl->f_self =     c_read_control_sph_SGS_MHD((char *) read_file_name);
		f_MHD_ctl->f_addition = c_add_sgs_sph_mhd_ctl();
		set_f_MHD_control(f_MHD_ctl);
		
		/* Show file name in entry */ 
		gtk_entry_set_text(entry, read_file_name);
		
		if((iso_GTK0 = (struct iso_ctl_GTK *) malloc(sizeof(struct iso_ctl_GTK))) == NULL) {
			printf("malloc error for iso_ctl_GTK \n");
			exit(0);
		}
		
		iso_GTK0->iso_c = init_iso_ctl_c();        
		read_iso_ctl_file_c(read_file_name, buf, iso_GTK0->iso_c);
		iflag_read_iso = 1;
		g_free(read_file_name);
		printf("iso_output_type_ctl original %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		set_primary_iso_format_flag_c(iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		printf("iso_output_type_ctl modified %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		
		MHD_control_expander(window, f_MHD_ctl, mWidgets);
		gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->ctl_MHD_Vbox, FALSE, TRUE, 0);
		gtk_widget_show_all(window);
	}else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	}else{
		g_print( "Another response was received.\n" );
	}
	gtk_widget_destroy(dialog);
}

static void cb_Save(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
	gchar *write_file_name;

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
	
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		
		write_iso_ctl_file_c(write_file_name, iso_GTK0->iso_c);
		dealloc_iso_ctl_c(iso_GTK0->iso_c);
		free(iso_GTK0);
		g_free(write_file_name);
		iflag_read_iso = 0;
		
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

void expander_MHD_ctl_callback(GObject *object, GParamSpec *param_spec, gpointer user_data){
	GtkExpander *expander;

	expander = GTK_EXPANDER (object);
	if (gtk_expander_get_expanded (expander)){
		printf("Expanded \n");
	}else{
		printf("Hided \n");
	}
	gtk_widget_show_all(window);
};


GtkWidget * iso_define_ctl_list_box(struct iso_define_ctl_c *iso_def_c){
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_2[2];
	
	iso_GTK0->iso_field_ctl = init_field_ctl_c();
	iso_GTK0->iso_fields_vws = init_field_views_GTK(iso_GTK0->iso_field_ctl);
	iso_GTK0->iso_fields_vws->used_tree_view 
			= create_field_tree_view(iso_GTK0->iso_fields_vws->all_fld_list,
									 iso_GTK0->iso_fields_vws->fld_ctl_gtk);
	iso_GTK0->iso_fields_vws->unused_field_tree_view
			= create_unused_field_tree_views(iso_GTK0->iso_fields_vws->all_fld_list);
	
	iso_GTK0->iso_fields_vws->field_group_tree_view
			= create_field_group_tree_view(iso_GTK0->iso_fields_vws->all_fld_list);
	iso_GTK0->iso_fields_vws->all_field_tree_view
			= create_all_field_tree_views(iso_GTK0->iso_fields_vws->all_fld_list);
	iso_GTK0->iso_fields_vws->selected_field_ctl =     iso_def_c->isosurf_data_ctl;
	iso_GTK0->iso_fields_vws->selected_component_ctl = iso_def_c->isosurf_comp_ctl;
	
	create_direction_tree_views(iso_GTK0->iso_fields_vws);
	
	printf("isosurf_data_ctl: %s\n", iso_def_c->isosurf_data_ctl->c_tbl);
	printf("isosurf_comp_ctl: %s\n", iso_def_c->isosurf_comp_ctl->c_tbl);
	add_all_field_combobox_vbox("Field_ctl:", "Comp_ctl:", 
								iso_GTK0->iso_fields_vws, vbox_1);
	
	vbox_2[0] = make_real_hbox(1, iso_def_c->label_iso_define_ctl->label[ 2],
							   iso_def_c->isosurf_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	
	GtkWidget *c_tree_view = NULL;
    GtkWidget *expand = add_c_list_box_w_addbottun(iso_def_c->iso_area_list,
                                                   c_tree_view);
	gtk_box_pack_start(GTK_BOX(vbox_1), expand, FALSE, FALSE, 0);
	return vbox_1;
};


struct f_MHD_tree_views{
	struct f_sph_shell_views *f_psph_vws;
	struct f_MHD_equations_views *f_eqs_vws;
    struct dimless_views * f_t_evo_vws;
    struct dimless_views * f_dimless_vws;

	struct f_MHD_BCs_tree_views *bc_nod_bc_vws;
 	struct f_MHD_BCs_tree_views *bc_surf_bc_vws;

	GtkWidget *f_fluid_area_tree_view;
	GtkWidget *f_conduct_area_tree_view;
	GtkWidget *f_force_default_view;
};
struct f_MHD_tree_views *f_MHD_vws;




static GtkWidget * draw_viz_each_pvr_ctl_vbox(char *label_name, struct f_VIZ_PVR_ctl *f_pvr_item, 
											  GtkWidget *window){
	GtkWidget *vbox_v_pwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_pvr_item->f_pvr_field_ctl);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_1,  FALSE, FALSE, 0);
    GtkWidget *expand_v_pwr = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_pvr_item->f_iflag,
															   f_pvr_item->pvr_ctl_file_name,
															   480, 480, window, vbox_v_pwr);
    return expand_v_pwr;
};

GtkWidget * MHD_temperature_model_expander(GtkWidget *window, struct f_MHD_temp_model_control *f_reft_ctl, 
                                    struct chara_clist *label_reftemp_list){
    GtkWidget *vbox_tl = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_tl1 = draw_real_item_entry_hbox(f_reft_ctl->f_low_ctl->f_value);
    GtkWidget *hbox_tl2 = draw_real_item_entry_hbox(f_reft_ctl->f_low_ctl->f_depth);
	gtk_box_pack_start(GTK_BOX(vbox_tl), hbox_tl1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tl), hbox_tl2, FALSE, FALSE, 0);
	GtkWidget *expand_tl = draw_control_block(f_reft_ctl->f_low_ctl->c_block_name, 
                                              f_reft_ctl->f_low_ctl->f_iflag,
                                              320, 400, window, vbox_tl);
    
    GtkWidget *vbox_th = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_th1 = draw_real_item_entry_hbox(f_reft_ctl->f_high_ctl->f_value);
    GtkWidget *hbox_th2 = draw_real_item_entry_hbox(f_reft_ctl->f_high_ctl->f_depth);
	gtk_box_pack_start(GTK_BOX(vbox_th), hbox_th1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_th), hbox_th2, FALSE, FALSE, 0);
	GtkWidget *expand_th = draw_control_block(f_reft_ctl->f_high_ctl->c_block_name, 
                                              f_reft_ctl->f_high_ctl->f_iflag,
                                              320, 400, window, vbox_th);
    
    GtkWidget *vbox_tp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *hbox_tp1 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_sigma_ctl);
    GtkWidget *hbox_tp2 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_width_ctl);
    GtkWidget *hbox_tp3 = draw_real_item_entry_hbox(f_reft_ctl->f_takepiro_ctl->f_stratified_outer_r_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tp), hbox_tp3, FALSE, FALSE, 0);
	GtkWidget *expand_tp = draw_control_block(f_reft_ctl->f_takepiro_ctl->c_block_name, 
                                              f_reft_ctl->f_takepiro_ctl->f_iflag,
                                              320, 400, window, vbox_tp);
    
    
    
    GtkWidget *hbox_t1 = draw_chara_switch_entry_hbox(f_reft_ctl->f_filterd_advect_ctl);
    GtkWidget *hbox_t2 = draw_chara_item_combobox_hbox(label_reftemp_list, f_reft_ctl->f_reference_ctl, window);
    GtkWidget *hbox_t3 = draw_chara_switch_entry_hbox(f_reft_ctl->f_stratified_ctl);
    GtkWidget *hbox_t4 = draw_chara_item_entry_hbox(f_reft_ctl->f_ref_file_ctl);
    GtkWidget *hbox_t5 = draw_real_item_entry_hbox(f_reft_ctl->f_ICB_diffuse_reduction_ctl);
    
	GtkWidget *vbox_tt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_tl, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_th, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), expand_tp, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_tt), hbox_t1, FALSE, FALSE, 0);
	GtkWidget *expand_t = draw_control_block(f_reft_ctl->c_block_name, f_reft_ctl->f_iflag,
                                              340, 400, window, vbox_tt);
    return expand_t;
};


void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
						  struct main_widgets *mWidgets){
	f_MHD_vws = (struct f_MHD_tree_views *) malloc(sizeof(struct f_MHD_tree_views));
	if(f_MHD_vws == NULL){
		printf("malloc error for f_MHD_tree_views\n");
		exit(0);
	};
	
    struct field_views *fields_vws = init_field_views_GTK(f_MHD_ctl->f_model_ctl->f_fld_ctl);
	fields_vws->used_tree_view = create_field_tree_view(fields_vws->all_fld_list,
														fields_vws->fld_ctl_gtk);
    fields_vws->unused_field_tree_view = create_unused_field_tree_views(fields_vws->all_fld_list);
    fields_vws->field_group_tree_view = create_field_group_tree_view(fields_vws->all_fld_list);
    fields_vws->all_field_tree_view = create_all_field_tree_views(fields_vws->all_fld_list);
    create_direction_tree_views(fields_vws);
	GtkWidget *vbox_MHD_fields = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_field_selection_box(fields_vws, window, vbox_MHD_fields);
    GtkWidget *expand_MHD_fields = draw_control_block(f_MHD_ctl->f_model_ctl->f_fld_ctl->c_block_name,
                                                      f_MHD_ctl->f_model_ctl->f_fld_ctl->f_iflag,
                                                      560, 500, window, vbox_MHD_fields);
    
    
    
    mWidgets->label_time_evo_list = init_f_ctl_chara_array(c_link_time_evo_list_to_ctl,
                                                           f_MHD_ctl->f_self);
    GtkWidget *vbox_m1 = c_list_combobox_expander(f_MHD_ctl->f_model_ctl->f_evo_ctl->f_t_evo_field_ctl,
                                                        mWidgets->label_time_evo_list,
                                                        mWidgets->time_evo_vws,
                                                        window);
    GtkWidget *expand_MHD_time_evo = draw_control_block(f_MHD_ctl->f_model_ctl->f_evo_ctl->c_block_name,
														f_MHD_ctl->f_model_ctl->f_evo_ctl->f_iflag,
                                                        560, 500, window, vbox_m1);
	
    
    GtkWidget *expand_MHD_fluid_area = add_c_list_box_w_addbottun(f_MHD_ctl->f_model_ctl->f_earea_ctl->f_evo_fluid_group_ctl, 
                                                                 f_MHD_vws->f_fluid_area_tree_view);
    GtkWidget *expand_MHD_conduct_area = add_c_list_box_w_addbottun(f_MHD_ctl->f_model_ctl->f_earea_ctl->f_evo_conduct_group_ctl, 
                                                                    f_MHD_vws->f_conduct_area_tree_view);
    
    GtkWidget *vbox_m2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m2), expand_MHD_fluid_area, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m2), expand_MHD_conduct_area, FALSE, FALSE, 0);
    
    GtkWidget *expand_MHD_tevo_area = draw_control_block(f_MHD_ctl->f_model_ctl->f_earea_ctl->c_block_name,
														f_MHD_ctl->f_model_ctl->f_earea_ctl->f_iflag,
                                                        560, 500, window, vbox_m2);
    
	
	GtkWidget *vbox_m3 = draw_node_bc_ctl_vbox(f_MHD_ctl->f_model_ctl->f_nbc_ctl,
											   f_MHD_vws->bc_nod_bc_vws, window);
    GtkWidget *expand_MHD_node_bc = draw_control_block(f_MHD_ctl->f_model_ctl->f_nbc_ctl->c_block_name,
                                                      f_MHD_ctl->f_model_ctl->f_nbc_ctl->f_iflag,
													   560, 500, window, vbox_m3);
	
	GtkWidget *vbox_m4 = draw_surf_bc_ctl_vbox(f_MHD_ctl->f_model_ctl->f_sbc_ctl,
											   f_MHD_vws->bc_surf_bc_vws, window);
    GtkWidget *expand_MHD_surf_bc = draw_control_block(f_MHD_ctl->f_model_ctl->f_sbc_ctl->c_block_name,
                                                      f_MHD_ctl->f_model_ctl->f_sbc_ctl->f_iflag,
                                                      560, 500, window, vbox_m4);
	
	
    mWidgets->label_force_list = init_f_ctl_chara_array(c_link_force_list_to_ctl,
                                                        f_MHD_ctl->f_self);
    GtkWidget *vbox_fce = c_list_combobox_expander(f_MHD_ctl->f_model_ctl->f_frc_ctl->f_force_names,
                                                  mWidgets->label_force_list,
                                                  mWidgets->force_vws,
                                                  window);
    GtkWidget *vbox_MHD_force = draw_control_block(f_MHD_ctl->f_model_ctl->f_frc_ctl->c_block_name,
                                                   f_MHD_ctl->f_model_ctl->f_frc_ctl->f_iflag,
                                                   560, 500, window, vbox_fce);
    
	GtkWidget *expand_MHD_dimless = add_dimless_selection_box(f_MHD_ctl->f_model_ctl->f_dless_ctl,
                                                              f_MHD_vws->f_dimless_vws, window);
	
	
	GtkWidget *vbox_eqs = draw_MHD_equations_vbox(f_MHD_ctl->f_model_ctl->f_eqs_ctl, 
												  f_MHD_vws->f_eqs_vws, window);
	GtkWidget *expand_MHD_eqs = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_iflag,
													 560, 400, window, vbox_eqs);
    
    mWidgets->label_reftemp_list = init_f_ctl_chara_array(c_link_reftemp_list_to_ctl,
                                                          f_MHD_ctl->f_self);
	GtkWidget *expand_t = MHD_temperature_model_expander(window, f_MHD_ctl->f_model_ctl->f_reft_ctl, 
                                                         mWidgets->label_reftemp_list);
	GtkWidget *expand_c = MHD_temperature_model_expander(window, f_MHD_ctl->f_model_ctl->f_refc_ctl, 
                                                         mWidgets->label_reftemp_list);
    
    
    mWidgets->label_xyz_dir_list = init_f_ctl_chara_array(c_link_xyz_dir_list_to_ctl,
                                                          f_MHD_ctl->f_self);
    GtkWidget *hbox_mc1 = draw_chara_switch_entry_hbox(f_MHD_ctl->f_model_ctl->f_mcv_ctl->f_filterd_induction_ctl);
    GtkWidget *hbox_mc2 = draw_chara_switch_entry_hbox(f_MHD_ctl->f_model_ctl->f_mcv_ctl->f_magneto_cv);
    GtkWidget *expand_mc = cr_list_combobox_expander(f_MHD_ctl->f_model_ctl->f_mcv_ctl->f_ext_magne, 
                                                     mWidgets->label_xyz_dir_list, mWidgets->ex_magne_vws, window);
    
    GtkWidget *vbox_mcv = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), hbox_mc1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), hbox_mc2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_mcv), expand_mc, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_mcv = draw_control_block(f_MHD_ctl->f_model_ctl->f_mcv_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_mcv_ctl->f_iflag,
													 560, 400, window, vbox_mcv);
    
 
    
    GtkWidget *vbox_m = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_fields, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_time_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_tevo_area, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_node_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_surf_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), vbox_MHD_force, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_dimless, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_eqs, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_t, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_mcv, FALSE, FALSE, 0);
	
	
    
    mWidgets->label_file_format_list = init_f_ctl_chara_array(set_file_fmt_items_f,
                                                              f_MHD_ctl->f_self);
	mWidgets->ctl_MHD_inner_box =   gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget * vbox_plt_c = draw_platform_control_vbox(f_MHD_ctl->f_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	GtkWidget * vbox_plt_o = draw_platform_control_vbox(f_MHD_ctl->f_org_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	GtkWidget * vbox_plt_n = draw_platform_control_vbox(f_MHD_ctl->f_new_plt,
                                                        mWidgets->label_file_format_list,
                                                        window);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_o, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), vbox_plt_n, FALSE, FALSE, 0);
	
	GtkWidget *expand_sph_shell = MHD_sph_shell_ctl_expander(window, f_MHD_ctl->f_psph_ctl,
															 f_MHD_ctl->f_fname_psph, 
															 f_MHD_vws->f_psph_vws);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_sph_shell, FALSE, FALSE, 0);
	
	GtkWidget *expand_MHD_model = draw_control_block(f_MHD_ctl->f_model_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_iflag,
													 560, 500, window, vbox_m);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_model, FALSE, FALSE, 0);
	
	
    GtkWidget *expand_MHD_control = draw_MHD_control_expand(window, f_MHD_ctl->f_smctl_ctl);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_control, FALSE, FALSE, 0);
	
	GtkWidget *expand_smntr = draw_MHD_sph_monitor_ctls_vbox(f_MHD_ctl->f_smonitor_ctl, 
															 mWidgets->f_lp_vws, window);
	gtk_container_add(GTK_CONTAINER(mWidgets->ctl_MHD_inner_box), expand_smntr);
	
	/*
	GtkWidget *vbox_node_monitor = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_node_monitor = draw_control_block(f_MHD_ctl->f_nmtr_ctl->c_block_name, 
													 f_MHD_ctl->f_nmtr_ctl->f_iflag,
													 560, 500, window, _node_monitor);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_node_monitor, FALSE, FALSE, 0);
	*/
	
	GtkWidget *vbox_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_psf = draw_control_block(f_MHD_ctl->f_viz_ctls->f_psf_ctls->clist_name, 
													 &f_MHD_ctl->f_viz_ctls->f_num_psf_ctl,
													 560, 500, window, vbox_psf);
	
	GtkWidget *vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_iso = draw_control_block(f_MHD_ctl->f_viz_ctls->f_iso_ctls->clist_name, 
													 &f_MHD_ctl->f_viz_ctls->f_num_iso_ctl,
													 560, 500, window, vbox_iso);
	
	GtkWidget *vbox_map = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_map = draw_control_block(f_MHD_ctl->f_viz_ctls->f_map_ctls->clist_name, 
													 &f_MHD_ctl->f_viz_ctls->f_num_map_ctl,
													 560, 500, window, vbox_map);
	
	
	GtkWidget *expand_MHD_pvr = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_pvr_ctls,
                                                          c_append_viz_pvr_render_ctls,
                                                          c_delete_viz_pvr_render_ctls,
                                                          (void *) init_f_VIZ_PVR_ctl,
                                                          dealloc_f_VIZ_PVR_ctl,
                                                          (void *) draw_viz_each_pvr_ctl_vbox,
                                                          mWidgets->vpvr_Wgts, window);
	
	GtkWidget *vbox_lic = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_lic = draw_control_block(f_MHD_ctl->f_viz_ctls->f_lic_ctls->clist_name, 
													 &f_MHD_ctl->f_viz_ctls->f_num_lic_ctl,
													 560, 500, window, vbox_lic);
	
	GtkWidget *vbox_fline = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_fline = draw_control_block(f_MHD_ctl->f_viz_ctls->f_fline_ctls->clist_name, 
													 &f_MHD_ctl->f_viz_ctls->f_num_fline_ctl,
													 560, 500, window, vbox_fline);
	
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_psf, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_iso, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_map, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_pvr, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_lic, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_fline, FALSE, FALSE, 0);
	
	
	
	GtkWidget *expand_MHD_viz = draw_control_block(f_MHD_ctl->f_viz_ctls->c_block_name, 
													 f_MHD_ctl->f_viz_ctls->f_iflag,
													 560, 500, window, vbox_viz);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_viz, FALSE, FALSE, 0);
	
	GtkWidget *vbox_zm = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_zm = draw_control_block(f_MHD_ctl->f_zm_ctls->c_block_name, 
													 f_MHD_ctl->f_zm_ctls->f_iflag,
													 560, 500, window, vbox_zm);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_zm, FALSE, FALSE, 0);
	
	
	
	mWidgets->ctl_MHD_Vbox = draw_control_block(f_MHD_ctl->c_block_name, f_MHD_ctl->f_iflag,
												560, 600, window, mWidgets->ctl_MHD_inner_box);
	return;
};


GtkWidget * MHD_control_bottuns_hbox(struct main_widgets *mWidgets){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
	if(f_MHD_ctl == NULL){
		printf("malloc error for f_MHD_ctl\n");
		exit(0);
	};
	
	GtkWidget *label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "MHD_ctl", (gpointer) f_MHD_ctl);
	g_object_set_data(G_OBJECT(entry), "mWidgets", (gpointer) mWidgets);
	
	/* Generate Bottuns */
	GtkWidget *button_O = gtk_button_new_with_label("Open");
	GtkWidget *button_V = gtk_button_new_with_label("View");
	GtkWidget *button_S = gtk_button_new_with_label("Save");
	GtkWidget *button_Q = gtk_button_new_with_label("Quit");
	
	
	
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_V), "clicked", G_CALLBACK(cb_View), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_V, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	return hbox;
}

int main(int argc, char** argv)
{
	gtk_init(&argc, &argv);
	
	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	struct main_widgets *mWidgets = (struct main_widgets *) malloc(sizeof(struct main_widgets));
		printf("mWidgets %p\n", mWidgets);
	if(mWidgets == NULL){
		printf("malloc error for mWidgets\n");
		exit(0);
	};
	
	mWidgets->main_Vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	mWidgets->open_Hbox = MHD_control_bottuns_hbox(mWidgets);
	gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->open_Hbox, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), mWidgets->main_Vbox);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

