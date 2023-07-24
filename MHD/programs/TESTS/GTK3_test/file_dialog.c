
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <sys/stat.h>

#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_ctl_array_chara3_items_c.h"
#include "t_ctl_array_chara_int3_items_c.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_array_chara2_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_data_4_fields_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_GTK.h"
#include "tree_view_4_field_GTK.h"
#include "tree_view_4_force_GTK.h"
#include "tree_view_4_colormap.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_boundary_condition_GTK.h"

#include "c_ctl_data_SGS_model.h"
#include "c_control_data_pvrs.h"
#include "c_ctl_data_platforms.h"
#include "c_ctl_data_MHD_BCs.h"
#include "c_ctl_data_MHD_model.h"
#include "c_ctl_data_PSF_ISOs.h"
#include "c_ctl_data_MAP.h"
#include "c_ctl_data_PVR_colormap.h"
#include "c_ctl_data_PVR_view_matrix.h"

#include "ctl_data_platforms_GTK.h"
#include "control_panel_4_dimless_GTK.h"
#include "control_panels_MHD_control_GTK.h"
#include "control_block_panel_GTK.h"
#include "control_panel_cbox_GTK.h"
#include "control_panel_cbox_real_GTK.h"
#include "control_panel_cbox_cbox_real_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_real3_GTK.h"
#include "control_panel_4_sph_monitor_GTK.h"
#include "control_panel_4_MHD_BCs_GTK.h"
#include "control_panels_MHD_model_GTK.h"
#include "control_panel_4_SGS_model_GTK.h"
#include "control_panel_fld_on_psf_GTK.h"
#include "control_panel_PSF_ISO_GTK.h"


extern void c_view_control_sph_SGS_MHD();


extern void * c_pvr_render_ctls_block_name(void *f_pvr_ctls);
extern int    c_pvr_render_ctls_num_pvr_ctl(void *f_pvr_ctls);
extern void * c_pvr_render_ctls_pvr_ctl(int idx, void *f_pvr_ctls);
extern void * c_append_viz_pvr_render_ctls(int idx, char *block_name, void *f_pvr_ctls);
extern void * c_delete_viz_pvr_render_ctls(int idx, void *f_pvr_ctls);



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


extern void * set_file_fmt_items_f(void *fmt_names_c);

struct f_LIC_noise_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	char * noise_ctl_file_name;
    
    struct chara_ctl_item *f_noise_type_ctl;
    struct chara_ctl_item *f_noise_file_name_ctl;
    struct chara_ctl_item *f_noise_file_format_ctl;
    struct int_ctl_item   *f_noise_resolution_ctl;
    struct int_ctl_item   *f_noise_stepping_ctl;
    struct real_ctl_item  *f_noise_cube_size_ctl;
    struct real_ctl_item  *f_noise_deltax_ctl;
};

struct f_LIC_kernel_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	char * kernel_ctl_file_name;
    
    struct chara_ctl_item *f_kernel_type_ctl;
    struct int_ctl_item   *f_kernel_resolution_ctl;
    struct real_ctl_item  *f_kernel_peak_ctl;
    struct real_ctl_item  *f_kernel_sigma_ctl;
    struct chara_ctl_item *f_trace_length_mode_ctl;
    struct real_ctl_item  *f_half_length_ctl;
    struct int_ctl_item   *f_max_trace_count_ctl;
};

struct f_LIC_masking_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
    struct chara_ctl_item *f_mask_type_ctl;
    struct chara_ctl_item *f_field_name_ctl;
    struct chara_ctl_item *f_component_ctl;
    struct real2_ctl_item *f_mask_range_ctl;
};

struct f_VIZ_LIC_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    
	struct f_LIC_noise_ctl *f_noise_ctl;
	struct f_LIC_kernel_ctl *f_kernel_ctl;
	char *f_fname_vol_repart_ctl;
	void *f_repart_ctl;
    
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
    
    void *void_panel;
};

struct f_VIZ_LIC_PVR_ctl{
    int f_iflag[1];
    char *lic_ctl_file_name;
	struct f_VIZ_PVR_ctl *f_lic_pvr_ctl;
	struct f_VIZ_LIC_ctl *f_lic_lic_ctl;
    
    void *void_panel;
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
	struct f_MHD_sph_shell_control * f_psph_ctl;
	struct f_MHD_model_control *f_model_ctl;
	struct f_MHD_control_ctls * f_smctl_ctl;
	struct f_MHD_sph_monitor_ctls * f_smonitor_ctl;
	struct f_MHD_node_monitor_ctl * f_nmtr_ctl;
	
	struct f_MHD_viz_ctls * f_viz_ctls;
	struct f_MHD_zm_ctls * f_zm_ctls;
};


struct MAP_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara_clist      *label_dir_list;
    struct colormap_view *color_vws;
    
    struct PSF_GTK_widgets *psf_def_vws;
    GtkWidget * map_area_view;
};

struct PVR_GTK_widgets{
    struct chara_int2_clist *label_field_list;
    struct chara_clist      *label_dir_list;
    struct colormap_view *color_vws;
    
    GtkWidget * lighting_tree_view;
    GtkWidget * psf_area_view;
};

struct LIC_GTK_widgets{
    GtkWidget * psf_area_view;
};

struct FLINE_GTK_widgets{
    GtkWidget * psf_area_view;
};

struct main_widgets{
	GtkWidget *main_Vbox;
	GtkWidget *open_Hbox;
	GtkWidget *ctl_MHD_Vbox;
    GtkWidget *ctl_MHD_inner_box;
	
	struct f_sph_shell_views *f_psph_vws;
    
    //	GtkWidget *expand_vpwrs;
//	GtkWidget *expand_smntr;
	struct f_sph_monitor_widgets *f_lp_vws;
	
	struct block_array_widgets *vpsf_Wgts;
	struct block_array_widgets *viso_Wgts;
	struct block_array_widgets *vmap_Wgts;
	struct block_array_widgets *vpvr_Wgts;
	struct block_array_widgets *vlic_Wgts;
    struct block_array_widgets *vfline_Wgts;
    
	struct block_array_widgets *zm_psf_Wgts;
	struct block_array_widgets *zrms_psf_Wgts;
	struct block_array_widgets *zm_map_Wgts;
	struct block_array_widgets *zrms_map_Wgts;
    
    struct chara_clist *label_file_format_list;

    struct MHD_model_widgets *model_wgts;
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
struct f_LIC_noise_ctl * init_f_LIC_noise_ctl(char *ctl_file_name,
                                               void *(*c_load_self)(void *f_parent),
                                               void *f_parent)
{
    struct f_LIC_noise_ctl *f_noise_ctl
            = (struct f_LIC_noise_ctl *) malloc(sizeof(struct f_LIC_noise_ctl));
    if(f_noise_ctl == NULL){
        printf("malloc error for f_LIC_noise_ctl\n");
        exit(0);
    };
    f_noise_ctl->f_self =  c_load_self(f_parent);
    
    f_noise_ctl->f_iflag =   (int *) c_LIC_noise_ctl_iflag(f_noise_ctl->f_self);
    char *f_block_name =   (char *) c_LIC_noise_ctl_block_name(f_noise_ctl->f_self);
    f_noise_ctl->c_block_name = strngcopy_from_f(f_block_name);
    f_noise_ctl->noise_ctl_file_name = ctl_file_name;
    
    f_noise_ctl->f_noise_type_ctl =        init_f_ctl_chara_item(c_LIC_noise_type_ctl,
                                                                 f_noise_ctl->f_self);
    f_noise_ctl->f_noise_file_name_ctl =   init_f_ctl_chara_item(c_LIC_noise_file_name_ctl,
                                                                f_noise_ctl->f_self);
    f_noise_ctl->f_noise_file_format_ctl = init_f_ctl_chara_item(c_LIC_noise_file_format_ctl,
                                                                 f_noise_ctl->f_self);
    f_noise_ctl->f_noise_resolution_ctl =  init_f_ctl_int_item(c_LIC_noise_resolution_ctl,
                                                               f_noise_ctl->f_self);
    f_noise_ctl->f_noise_stepping_ctl =    init_f_ctl_int_item(c_LIC_noise_stepping_ctl,
                                                               f_noise_ctl->f_self);
    f_noise_ctl->f_noise_cube_size_ctl =   init_f_ctl_real_item(c_LIC_noise_cube_size_ctl, 
                                                                f_noise_ctl->f_self);
    f_noise_ctl->f_noise_deltax_ctl =      init_f_ctl_real_item(c_LIC_noise_deltax_ctl, 
                                                                f_noise_ctl->f_self);
    return f_noise_ctl;
}


void dealloc_f_LIC_noise_ctl(struct f_LIC_noise_ctl *f_noise_ctl){
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_type_ctl);
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_file_name_ctl);
    dealloc_chara_ctl_item_c(f_noise_ctl->f_noise_file_format_ctl);
    dealloc_int_ctl_item_c(f_noise_ctl->f_noise_resolution_ctl);
    dealloc_int_ctl_item_c(f_noise_ctl->f_noise_stepping_ctl);
    dealloc_real_ctl_item_c(f_noise_ctl->f_noise_cube_size_ctl);
    dealloc_real_ctl_item_c(f_noise_ctl->f_noise_deltax_ctl);
    
    free(f_noise_ctl->noise_ctl_file_name);
    free(f_noise_ctl->c_block_name);
    f_noise_ctl->f_iflag = NULL;
    f_noise_ctl->f_self = NULL;
    free(f_noise_ctl);
    return;
}


struct f_LIC_kernel_ctl * init_f_LIC_kernel_ctl(char *ctl_file_name,
                                                void *(*c_load_self)(void *f_parent),
                                                void *f_parent)
{
    struct f_LIC_kernel_ctl *f_kernel_ctl
            = (struct f_LIC_kernel_ctl *) malloc(sizeof(struct f_LIC_kernel_ctl));
    if(f_kernel_ctl == NULL){
        printf("malloc error for f_LIC_kernel_ctl\n");
        exit(0);
    };
    f_kernel_ctl->f_self =  c_load_self(f_parent);
    
    f_kernel_ctl->f_iflag =   (int *) c_LIC_kernel_ctl_iflag(f_kernel_ctl->f_self);
    char *f_block_name =   (char *) c_LIC_kernel_ctl_block_name(f_kernel_ctl->f_self);
    f_kernel_ctl->c_block_name = strngcopy_from_f(f_block_name);
    f_kernel_ctl->kernel_ctl_file_name = ctl_file_name;
    
    f_kernel_ctl->f_kernel_type_ctl =       init_f_ctl_chara_item(c_LIC_kernel_type_ctl,
                                                                  f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_resolution_ctl = init_f_ctl_int_item(c_LIC_kernel_resolution_ctl,
                                                                f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_peak_ctl =       init_f_ctl_real_item(c_LIC_kernel_peak_ctl,
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_kernel_sigma_ctl =      init_f_ctl_real_item(c_LIC_kernel_kernel_sigma_ctl,
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_trace_length_mode_ctl = init_f_ctl_chara_item(c_LIC_kernel_trace_len_mod_ctl,
                                                                  f_kernel_ctl->f_self);
    f_kernel_ctl->f_half_length_ctl =       init_f_ctl_real_item(c_LIC_kernel_half_length_ctl, 
                                                                 f_kernel_ctl->f_self);
    f_kernel_ctl->f_max_trace_count_ctl =   init_f_ctl_int_item(c_LIC_kernel_max_trace_cnt_ctl, 
                                                                f_kernel_ctl->f_self);
    return f_kernel_ctl;
}

void dealloc_f_LIC_kernel_ctl(struct f_LIC_kernel_ctl *f_kernel_ctl){
    dealloc_chara_ctl_item_c(f_kernel_ctl->f_kernel_type_ctl);
    dealloc_int_ctl_item_c(f_kernel_ctl->f_kernel_resolution_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_kernel_peak_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_kernel_sigma_ctl);
    dealloc_chara_ctl_item_c(f_kernel_ctl->f_trace_length_mode_ctl);
    dealloc_real_ctl_item_c(f_kernel_ctl->f_half_length_ctl);
    dealloc_int_ctl_item_c(f_kernel_ctl->f_max_trace_count_ctl);
    
    free(f_kernel_ctl->kernel_ctl_file_name);
    free(f_kernel_ctl->c_block_name);
    f_kernel_ctl->f_iflag = NULL;
    f_kernel_ctl->f_self = NULL;
    free(f_kernel_ctl);
    return;
}

struct f_LIC_masking_ctl * init_f_LIC_masking_ctl(void *(*c_load_self)(int idx, void *f_parent),
                                                  int idx, void *f_parent)
{
    struct f_LIC_masking_ctl *f_mask_ctl
            = (struct f_LIC_masking_ctl *) malloc(sizeof(struct f_LIC_masking_ctl));
    if(f_mask_ctl == NULL){
        printf("malloc error for f_LIC_masking_ctl\n");
        exit(0);
    };
    f_mask_ctl->f_self =  c_load_self(idx, f_parent);
    
    f_mask_ctl->f_iflag =   (int *) c_masking_fld_ctl_iflag(f_mask_ctl->f_self);
    char *f_block_name =   (char *) c_masking_fld_ctl_block_name(f_mask_ctl->f_self);
    f_mask_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_mask_ctl->f_mask_type_ctl =  init_f_ctl_chara_item(c_masking_fld_mask_type_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_field_name_ctl = init_f_ctl_chara_item(c_masking_fld_field_name_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_component_ctl =  init_f_ctl_chara_item(c_masking_fld_component_ctl, f_mask_ctl->f_self);
    f_mask_ctl->f_mask_range_ctl = init_f_ctl_r2_item(c_masking_fld_mask_range_ctl, f_mask_ctl->f_self);
    return f_mask_ctl;
}

void dealloc_f_LIC_masking_ctl(struct f_LIC_masking_ctl *f_mask_ctl){
    dealloc_chara_ctl_item_c(f_mask_ctl->f_mask_type_ctl);
    dealloc_chara_ctl_item_c(f_mask_ctl->f_field_name_ctl);
    dealloc_chara_ctl_item_c(f_mask_ctl->f_component_ctl);
    dealloc_real2_ctl_item_c(f_mask_ctl->f_mask_range_ctl);
    
    free(f_mask_ctl->c_block_name);
    f_mask_ctl->f_iflag = NULL;
    f_mask_ctl->f_self = NULL;
    free(f_mask_ctl);
    return;
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
    f_lic_lic_ctl->f_noise_ctl = init_f_LIC_noise_ctl(strngcopy_from_f(f_block_name), 
                                                      c_VIZ_LIC_noise_ctl,
                                                      f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_kernel_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_kernel_ctl = init_f_LIC_kernel_ctl(strngcopy_from_f(f_block_name), 
                                                        c_VIZ_LIC_kernel_ctl, 
                                                        f_lic_lic_ctl->f_self);
    
    f_block_name = (char *) c_VIZ_LIC_fname_vol_repart_ctl(f_lic_lic_ctl->f_self);
    f_lic_lic_ctl->f_fname_vol_repart_ctl = strngcopy_from_f(f_block_name);
    f_lic_lic_ctl->f_repart_ctl =           c_VIZ_LIC_repartition_ctl(f_lic_lic_ctl->f_self);
    
    f_lic_lic_ctl->f_mask_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_lic_lic_ctl->f_mask_ctl->f_parent = f_lic_lic_ctl->f_self;
    int i;
    for(i=0;i<c_VIZ_LIC_num_masking_ctl(f_lic_lic_ctl->f_self);i++){
        void *f_ctl_tmp = init_f_LIC_masking_ctl(c_VIZ_LIC_mask_ctl, i, f_lic_lic_ctl->f_self);
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

struct f_VIZ_LIC_ctl * dealloc_f_VIZ_LIC_ctl(void *void_in)
{
    struct f_VIZ_LIC_ctl *f_lic_lic_ctl = (struct f_VIZ_LIC_ctl *) void_in;
    
    f_lic_lic_ctl->f_self = NULL;
	free(f_lic_lic_ctl->c_block_name);
    
    dealloc_f_LIC_noise_ctl(f_lic_lic_ctl->f_noise_ctl);
    dealloc_f_LIC_kernel_ctl(f_lic_lic_ctl->f_kernel_ctl);
    
    free(f_lic_lic_ctl->f_fname_vol_repart_ctl);
    f_lic_lic_ctl->f_repart_ctl =           c_VIZ_LIC_repartition_ctl(f_lic_lic_ctl->f_self);
    
    int i;
    for(i=0;i<count_void_clist(f_lic_lic_ctl->f_mask_ctl);i++){
        void *f_ctl_tmp = void_clist_at_index(i, f_lic_lic_ctl->f_mask_ctl);
		dealloc_f_LIC_masking_ctl((struct f_LIC_masking_ctl *) f_ctl_tmp);
    };
    dealloc_void_clist(f_lic_lic_ctl->f_mask_ctl);
    
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_LIC_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_subdomain_elapsed_dump_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_color_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_color_component_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_opacity_field_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_opacity_component_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_vr_sample_mode_ctl);
    dealloc_real_ctl_item_c(f_lic_lic_ctl->f_step_size_ctl);
    dealloc_chara_ctl_item_c(f_lic_lic_ctl->f_normalization_type_ctl);
    dealloc_real_ctl_item_c(f_lic_lic_ctl->f_normalization_value_ctl);
    
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
void dealloc_f_VIZ_LIC_PVR_ctl(struct f_VIZ_LIC_PVR_ctl *f_lic_ctl)
{
    free(f_lic_ctl->lic_ctl_file_name);
    f_lic_ctl->f_lic_pvr_ctl = dealloc_f_VIZ_PVR_ctl(f_lic_ctl->f_lic_pvr_ctl);
    f_lic_ctl->f_lic_lic_ctl =  dealloc_f_VIZ_LIC_ctl(f_lic_ctl->f_lic_lic_ctl);
    free(f_lic_ctl);
    return;
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

void dealloc_f_VIZ_FLINE_ctl(struct f_VIZ_FLINE_ctl *f_fline_ctl)
{
	f_fline_ctl->f_self = NULL;
    free(f_fline_ctl->fline_ctl_file_name);
	free(f_fline_ctl->c_block_name);
    
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_file_head_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_output_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_field_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_color_field_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_fline_color_comp_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_starting_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_selection_type_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_line_direction_ctl);
    dealloc_chara_ctl_item_c(f_fline_ctl->f_start_surf_grp_ctl);
    dealloc_int_ctl_item_c(f_fline_ctl->f_num_fieldline_ctl);
    dealloc_int_ctl_item_c(f_fline_ctl->f_max_line_stepping_ctl);
    dealloc_chara_clist(f_fline_ctl->f_fline_area_grp_ctl);
    dealloc_real3_clist(f_fline_ctl->f_seed_point_ctl);
    dealloc_int2_clist(f_fline_ctl->f_seed_surface_ctl);
    free(f_fline_ctl);
	return;
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
	f_block_name =              (char *) c_MHD_fname_psph(f_MHD_ctl->f_self);
	f_MHD_ctl->f_psph_ctl =     init_f_MHD_sph_shell_ctl(strngcopy_from_f(f_block_name),
                                                         c_MHD_psph_ctl, f_MHD_ctl->f_self);
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
    struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) g_object_get_data(G_OBJECT(data), "MHD_ctl");

    GtkWidget *dialog;
    GtkWidget *parent;
    GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
    gint response;
    gchar *write_file_name;
    gchar *folder;
    char path_name[LENGTHBUF];

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
    int i;
    
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		gtk_entry_set_text(entry, write_file_name);
		g_free(write_file_name);
		
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
        g_print( "Write folder name: %s\n", write_file_name);
        
        getcwd(path_name, LENGTHBUF);
        printf("before current dir : %s\n", path_name);
        chdir(folder);
        getcwd(path_name, LENGTHBUF);
        printf("Folder to save: %s\n", path_name);
        
        char *stripped_filehead = (char *) calloc(LENGTHBUF+1, sizeof(char));
        char *stripped_dir = (char *) calloc(LENGTHBUF+1, sizeof(char));
        struct stat st;
        
        split_dir_and_file_name_c(f_MHD_ctl->f_psph_ctl->fname_sph_shell, stripped_dir, stripped_filehead);
        printf("fname_sph_shell: %s %s, \n", stripped_dir, f_MHD_ctl->f_psph_ctl->fname_sph_shell);
        if(compare_string(strlen(f_MHD_ctl->f_psph_ctl->fname_sph_shell), 
                          stripped_dir, f_MHD_ctl->f_psph_ctl->fname_sph_shell) == 0){
            if(stat(stripped_dir, &st) != 0) {
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            }
        };
        
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_psf_ctls);i++){
            struct f_VIZ_PSF_ctl *ctl_tmp 
                    = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_psf_ctls);
            split_dir_and_file_name_c(ctl_tmp->psf_ctl_file_name, stripped_dir, stripped_filehead);
            printf("psf_ctl_file_name %d: %s %s, \n", i, stripped_dir, ctl_tmp->psf_ctl_file_name);
            if(compare_string(strlen(ctl_tmp->psf_ctl_file_name),
                              stripped_dir, ctl_tmp->psf_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_psf_def_c->psf_def_file_name),
                              stripped_dir, ctl_tmp->f_psf_def_c->psf_def_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_fld_on_psf_c->fname_fld_on_psf),
                              stripped_dir, ctl_tmp->f_fld_on_psf_c->fname_fld_on_psf) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_iso_ctls);i++){
            struct f_VIZ_ISO_ctl *ctl_tmp 
                    = (struct f_VIZ_ISO_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_iso_ctls);
            if(compare_string(strlen(ctl_tmp->iso_ctl_file_name),
                              stripped_dir, ctl_tmp->iso_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
            if(compare_string(strlen(ctl_tmp->f_fld_on_iso_c->fname_fld_on_psf),
                              stripped_dir, ctl_tmp->f_fld_on_iso_c->fname_fld_on_psf) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_map_ctls);i++){
            struct f_VIZ_MAP_ctl *ctl_tmp 
                    = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_map_ctls);
            if(compare_string(strlen(ctl_tmp->map_ctl_file_name),
                              stripped_dir, ctl_tmp->map_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_pvr_ctls);i++){
            struct f_VIZ_PVR_ctl *ctl_tmp 
                    = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
            if(compare_string(strlen(ctl_tmp->pvr_ctl_file_name),
                              stripped_dir, ctl_tmp->pvr_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_lic_ctls);i++){
            struct f_VIZ_LIC_PVR_ctl *ctl_tmp 
                    = (struct f_VIZ_LIC_PVR_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_lic_ctls);
            if(compare_string(strlen(ctl_tmp->lic_ctl_file_name),
                              stripped_dir, ctl_tmp->lic_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
        for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_fline_ctls);i++){
            struct f_VIZ_FLINE_ctl *ctl_tmp 
                    = (struct f_VIZ_FLINE_ctl *) void_clist_at_index(i,f_MHD_ctl->f_viz_ctls->f_fline_ctls);
            if(compare_string(strlen(ctl_tmp->fline_ctl_file_name),
                              stripped_dir, ctl_tmp->fline_ctl_file_name) == 0){
                printf("%s under %s does not exist. made flag %d\n", stripped_dir, path_name, 
                       mkdir(stripped_dir, 0777));
            };
        }
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

struct viewmat_GTK_widgets{
    struct chara_clist *label_xyz_dir_list;
    struct chara_clist *label_xyzw_dir_list;
    
    struct cbox_cbox_table_view *f_map_image_prefix_vws;
    struct chara_cbox_table_view *f_lookpoint_vws;
    struct chara_cbox_table_view *f_viewpoint_vws;
    struct chara_cbox_table_view *f_up_dir_vws;
    struct chara_cbox_table_view *f_scale_vector_vws;
    struct chara_cbox_table_view *f_viewpt_in_viewer_vws;
    struct chara_cbox_table_view *f_view_rot_vec_vws;
};

static GtkWidget * draw_viewmatrix_pixels_vbox(struct image_size_ctl_c *f_pixel, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_int_item_entry_hbox(f_pixel->f_num_xpixel_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_pixel->f_num_ypixel_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_pixel = draw_control_block(f_pixel->c_block_name, f_pixel->f_iflag,
                                                 window, vbox_v_pix);
    return expand_pixel;
};

static GtkWidget * draw_viewmatrix_projection_vbox(struct projection_mat_ctl_c *f_proj, 
                                                   GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_proj->f_perspective_angle_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_proj->f_perspective_xy_ratio_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_proj->f_perspective_near_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_proj->f_perspective_far_ctl);
    
    GtkWidget *hbox_5 = draw_real2_item_entry_hbox(f_proj->f_horizontal_range_ctl);
    GtkWidget *hbox_6 = draw_real2_item_entry_hbox(f_proj->f_vertical_range_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_6,  FALSE, FALSE, 0);
    
    GtkWidget *expand_proj = draw_control_block(f_proj->c_block_name, f_proj->f_iflag,
                                                window, vbox_v_pix);
    return expand_proj;
};

static GtkWidget * draw_viewmatrix_stereo_vbox(struct streo_view_ctl_c *f_streo, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_streo->f_focalpoint_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_streo->f_eye_separation_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_streo->f_eye_sep_angle_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_streo->f_step_eye_sep_angle_ctl);
    
    GtkWidget *vbox_s = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_4,  FALSE, FALSE, 0);
    
    GtkWidget *expand_streo = draw_control_block(f_streo->c_block_name, f_streo->f_iflag,
                                                 window, vbox_s);
    return expand_streo;
};

static GtkWidget * draw_viz_viewmatrix_vbox(struct modelview_ctl_c *f_mat_c, 
                                            GtkWidget *window){
    struct viewmat_GTK_widgets *viewmatrix_vws = (struct viewmat_GTK_widgets *) f_mat_c->void_panel;
    
    GtkWidget *expand_pixel = draw_viewmatrix_pixels_vbox(f_mat_c->f_pixel, window);
    GtkWidget *expand_proj =  draw_viewmatrix_projection_vbox(f_mat_c->f_proj, window);
    GtkWidget *expand_streo = draw_viewmatrix_stereo_vbox(f_mat_c->f_streo, window);
    
    GtkWidget *hbox_1 = c2r_list_combobox_expander(f_mat_c->f_modelview_mat_ctl,
                                                   viewmatrix_vws->label_xyzw_dir_list,
                                                   viewmatrix_vws->label_xyzw_dir_list, 
                                                   viewmatrix_vws->f_map_image_prefix_vws, window);
    GtkWidget *hbox_2 = cr_list_combobox_expander(f_mat_c->f_lookpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_lookpoint_vws, window);
    GtkWidget *hbox_3 = cr_list_combobox_expander(f_mat_c->f_viewpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpoint_vws, window);
    GtkWidget *hbox_4 = cr_list_combobox_expander(f_mat_c->f_up_dir_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_up_dir_vws, window);
    GtkWidget *hbox_5 = cr_list_combobox_expander(f_mat_c->f_view_rot_vec_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_view_rot_vec_vws, window);
    
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_mat_c->f_view_rotation_deg_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_mat_c->f_scale_factor_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_mat_c->f_projection_type_ctl);
    
    GtkWidget *hbox_9 = cr_list_combobox_expander(f_mat_c->f_scale_vector_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_scale_vector_vws, window);
    GtkWidget *hbox_10 = cr_list_combobox_expander(f_mat_c->f_viewpt_in_viewer_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpt_in_viewer_vws, window);
    
	GtkWidget *vbox_v_mat = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_pixel,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_proj,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_streo,  FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_map = draw_control_block_w_file_switch(f_mat_c->c_block_name,
															   f_mat_c->f_iflag,
															   f_mat_c->mat_ctl_file_name,
															   window, vbox_v_mat);
    return expand_v_map;
};

static GtkWidget * draw_map_define_vbox(struct f_MAP_section_ctl *f_map_define_ctl, 
                                        struct PSF_GTK_widgets *psf_def_vws, GtkWidget *window){
    GtkWidget *expand_s =  draw_psf_def_ctl_vbox(f_map_define_ctl->f_psf_def_c, 
                                                 psf_def_vws, window);
                                                                                                 
    GtkWidget *hbox_1 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_zeroline_switch_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_define_ctl->f_isoline_color_mode);
    GtkWidget *hbox_3 = draw_int_item_entry_hbox(f_map_define_ctl->f_isoline_number_ctl);
    GtkWidget *hbox_4 = draw_real2_item_entry_hbox(f_map_define_ctl->f_isoline_range_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_map_define_ctl->f_isoline_width_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_map_define_ctl->f_grid_width_ctl);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_tan_cyl_switch_ctl);
    GtkWidget *hbox_8 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_inner_ctl);
    GtkWidget *hbox_9 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_outer_ctl);
    
    GtkWidget *vbox_map_def = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), expand_s,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_9,  FALSE, FALSE, 0);
    
    GtkWidget *expand_map_def = draw_control_block(f_map_define_ctl->c_block_name, 
                                                   f_map_define_ctl->f_iflag,
                                                   window, vbox_map_def);
    return expand_map_def;
};

static GtkWidget * draw_viz_colormap_vbox(struct colormap_ctl_c *cmap_c, 
                                          struct colormap_view *color_vws, 
                                          struct chara_int2_clist *label_field_list, 
                                          struct chara_clist *label_dir_list, 
                                          GtkWidget *window){
    GtkWidget *hbox_2 = draw_field_combobox_hbox(label_field_list, 
                                                 cmap_c->f_lic_color_fld_ctl, window);
    GtkWidget *hbox_3 = draw_chara_item_combobox_hbox(label_dir_list,
                                                      cmap_c->f_lic_color_comp_ctl, window);
    GtkWidget *hbox_4 = draw_field_combobox_hbox(label_field_list, 
                                                 cmap_c->f_lic_opacity_fld_ctl, window);
    GtkWidget *hbox_5 = draw_chara_item_combobox_hbox(label_dir_list,
                                                      cmap_c->f_lic_opacity_comp_ctl, window);
    
    GtkWidget *hbox_10 = draw_real3_item_entry_hbox(cmap_c->f_background_color_ctl);
    
    GtkWidget *hbox_6 = draw_chara_item_entry_hbox(cmap_c->f_data_mapping_ctl);
    GtkWidget *hbox_7 = draw_chara_item_entry_hbox(cmap_c->f_opacity_style_ctl);
    GtkWidget *hbox_8 = draw_real_item_entry_hbox(cmap_c->f_fix_opacity_ctl);
    
    GtkWidget *hbox_9 =  draw_real_item_entry_hbox(cmap_c->f_range_min_ctl);
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(cmap_c->f_range_max_ctl);
    
    GtkWidget *vbox_cbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_1,  FALSE, FALSE, 0);
    
    add_pvr_colormap_list_box_2(color_vws, vbox_cbox);
    
    GtkWidget *expand_cmap = draw_control_block(cmap_c->c_block_name, cmap_c->f_iflag,
                                                window, vbox_cbox);
    return expand_cmap;
}
static GtkWidget * draw_viz_colorbar_vbox(struct pvr_colorbar_ctl_c *cbar_c,  GtkWidget *window){
    
    GtkWidget *hbox_1 = draw_chara_switch_entry_hbox(cbar_c->f_colorbar_switch_ctl);
    GtkWidget *hbox_2 = draw_chara_switch_entry_hbox(cbar_c->f_colorbar_scale_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(cbar_c->f_colorbar_position_ctl);
    GtkWidget *hbox_4 = draw_chara_switch_entry_hbox(cbar_c->f_zeromarker_flag_ctl);
    
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(cbar_c->f_font_size_ctl);
    GtkWidget *hbox_6 = draw_int_item_entry_hbox(cbar_c->f_ngrid_cbar_ctl);
    GtkWidget *hbox_7 = draw_real2_item_entry_hbox(cbar_c->f_cbar_range_ctl);
    
    GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(cbar_c->f_axis_switch_ctl);
    GtkWidget *hbox_9 = draw_chara_switch_entry_hbox(cbar_c->f_time_switch_ctl);
    GtkWidget *hbox_10 = draw_chara_switch_entry_hbox(cbar_c->f_mapgrid_switch_ctl);
    
    GtkWidget *vbox_cbar = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_10, FALSE, FALSE, 0);
    
    GtkWidget *expand_cbar = draw_control_block(cbar_c->c_block_name, cbar_c->f_iflag,
                                                window, vbox_cbar);
    return expand_cbar;
}


void append_viz_cmap_cbar_vbox(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                               struct colormap_view *color_vws, 
                               struct chara_int2_clist *label_field_list, 
                               struct chara_clist *label_dir_list, 
                               GtkWidget *window, GtkWidget *vbox_cc){
    GtkWidget *expand_cmap =  draw_viz_colormap_vbox(f_cmap_cbar_c->cmap_c, color_vws, 
                                                     label_field_list, label_dir_list, window);
    GtkWidget *expand_cbar =  draw_viz_colorbar_vbox(f_cmap_cbar_c->cbar_c, window);
    
    GtkWidget *label = gtk_label_new(f_cmap_cbar_c->c_block_name);
	GtkWidget *hbox_cc = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox_cc), label, FALSE, TRUE, 0);
    append_block_file_switch_hbox(f_cmap_cbar_c->cmap_ctl_file_name, hbox_cc);
    
    gtk_box_pack_start(GTK_BOX(vbox_cc), hbox_cc,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cc), expand_cmap,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cc), expand_cbar,  FALSE, FALSE, 0);
    return;
};

static GtkWidget * draw_viz_each_map_ctl_vbox(char *label_name, struct f_VIZ_MAP_ctl *f_map_item, 
											  GtkWidget *window){
    struct MAP_GTK_widgets *map_vws = (struct MAP_GTK_widgets *) f_map_item->void_panel;
	GtkWidget *vbox_v_map = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_map_item->f_map_image_prefix_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_item->f_map_image_fmt_ctl);
    GtkWidget *hbox_3 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_map_field_ctl, window);
    GtkWidget *hbox_4 = draw_chara_item_combobox_hbox(map_vws->label_dir_list,
                                                f_map_item->f_map_comp_ctl, window);
    GtkWidget *hbox_5 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_isoline_field_ctl, window);
    GtkWidget *hbox_6 = draw_chara_item_combobox_hbox(map_vws->label_dir_list, 
                                                      f_map_item->f_isoline_comp_ctl, window);
    
    GtkWidget *expand_mdef = draw_map_define_vbox(f_map_item->f_map_define_ctl, 
                                                  map_vws->psf_def_vws, window);
    GtkWidget *expand_vmat = draw_viz_viewmatrix_vbox(f_map_item->f_mat, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_mdef, FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_map_item->f_cmap_cbar_c, map_vws->color_vws, 
                              map_vws->label_field_list, map_vws->label_dir_list, 
                              window, vbox_v_map);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_vmat, FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_map = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_map_item->f_iflag,
															   f_map_item->map_ctl_file_name,
															   window, vbox_v_map);
    return expand_v_map;
};



static GtkWidget * draw_pvr_lighting_vbox(struct lighting_ctl_c *lighting_ctl_c,
                                          GtkWidget *lighting_tree_view, GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(lighting_ctl_c->f_ambient_coef_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(lighting_ctl_c->f_diffuse_coef_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(lighting_ctl_c->f_specular_coef_ctl);
    GtkWidget *expand_4 = r3_list_combobox_expander(lighting_ctl_c->f_light_position_ctl,
                                                    lighting_tree_view, window);
    
    GtkWidget *vbox_lgt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), expand_4,  FALSE, FALSE, 0);
    
    GtkWidget *expand_lgt = draw_control_block_w_file_switch(lighting_ctl_c->c_block_name,
                                                             lighting_ctl_c->f_iflag,
                                                             lighting_ctl_c->light_ctl_file_name,
                                                             window, vbox_lgt);
    return expand_lgt;
}


static GtkWidget * draw_viz_each_pvr_ctl_vbox(char *label_name, struct f_VIZ_PVR_ctl *f_pvr_item, 
											  GtkWidget *window){
    struct PVR_GTK_widgets *pvr_vws = (struct PVR_GTK_widgets *) f_pvr_item->void_panel;
    
    GtkWidget *vbox_v_pvr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_pvr_item->f_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_pvr_item->f_file_fmt_ctl);
    GtkWidget *hbox_4 = draw_field_combobox_hbox(pvr_vws->label_field_list, 
                                                 f_pvr_item->f_pvr_field_ctl, window);
    GtkWidget *hbox_5 = draw_chara_item_combobox_hbox(pvr_vws->label_dir_list,
                                                      f_pvr_item->f_pvr_comp_ctl, window);
    
    GtkWidget *expand_v_lgt = draw_pvr_lighting_vbox(f_pvr_item->f_light,
                                                     pvr_vws->lighting_tree_view, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_5,  FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_pvr_item->f_cmap_cbar_c, pvr_vws->color_vws, 
                              pvr_vws->label_field_list, pvr_vws->label_dir_list, 
                              window, vbox_v_pvr);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expand_v_lgt,  FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_pwr = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_pvr_item->f_iflag,
															   f_pvr_item->pvr_ctl_file_name,
															   window, vbox_v_pvr);
    return expand_v_pwr;
};

static GtkWidget * draw_viz_each_lic_lic_ctl_vbox(char *label_name, struct f_VIZ_LIC_ctl *f_lic_lic_ctl, 
											  GtkWidget *window){
	GtkWidget *vbox_v_lic = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_LIC_field_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_color_field_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_color_component_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_opacity_field_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_lic_lic_ctl->f_opacity_component_ctl);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), hbox_5,  FALSE, FALSE, 0);
    
    GtkWidget *expand_v_lic = draw_control_block(duplicate_underscore(label_name),
                                                 f_lic_lic_ctl->f_iflag,
                                                 window, vbox_v_lic);
    return expand_v_lic;
};

static GtkWidget * draw_viz_each_lic_ctl_vbox(char *label_name, struct f_VIZ_LIC_PVR_ctl *f_lic_item, 
											  GtkWidget *window){
    GtkWidget *expand_lic_pvr = draw_viz_each_pvr_ctl_vbox(f_lic_item->f_lic_pvr_ctl->c_block_name,
                                                           f_lic_item->f_lic_pvr_ctl, window);
    GtkWidget *expand_lic_lic = draw_viz_each_lic_lic_ctl_vbox(f_lic_item->f_lic_lic_ctl->c_block_name,
                                                               f_lic_item->f_lic_lic_ctl, window);
    
	GtkWidget *vbox_v_lic = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expand_lic_pvr,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_lic), expand_lic_lic,  FALSE, FALSE, 0);
    
    GtkWidget *expand_v_lic = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_lic_item->f_iflag,
															   f_lic_item->lic_ctl_file_name,
															   window, vbox_v_lic);
    return expand_v_lic;
};

static GtkWidget * draw_viz_each_fline_ctl_vbox(char *label_name, struct f_VIZ_FLINE_ctl *f_fline_item, 
											  GtkWidget *window){
	GtkWidget *vbox_v_fline = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_fline_item->f_fline_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_fline_item->f_fline_output_type_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_fline_item->f_fline_field_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_field_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_comp_ctl);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_5,  FALSE, FALSE, 0);
    
    GtkWidget *expand_v_fline = draw_control_block_w_file_switch(duplicate_underscore(label_name),
                                                                 f_fline_item->f_iflag,
                                                                 f_fline_item->fline_ctl_file_name,
                                                                 window, vbox_v_fline);
    return expand_v_fline;
};


struct viewmat_GTK_widgets * init_viewmat_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct viewmat_GTK_widgets *viewmatrix_vws 
			= (struct viewmat_GTK_widgets *) malloc(sizeof(struct viewmat_GTK_widgets));
	if(viewmatrix_vws == NULL){
		printf("malloc error for viewmatrix_vws\n");
		exit(0);
    };
    
    viewmatrix_vws->label_xyz_dir_list =  init_f_ctl_chara_array(c_link_xyz_dir_list_to_ctl,  NULL);
    viewmatrix_vws->label_xyzw_dir_list = init_f_ctl_chara_array(c_link_xyzw_dir_list_to_ctl, NULL);
    return viewmatrix_vws;
}
void dealloc_viewmat_GTK_widgets(struct viewmat_GTK_widgets *viewmatrix_vws){
    dealloc_chara_clist(viewmatrix_vws->label_xyzw_dir_list);
    dealloc_chara_clist(viewmatrix_vws->label_xyz_dir_list);
    free(viewmatrix_vws);
}

struct MAP_GTK_widgets * init_MAP_GTK_widgets(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct MAP_GTK_widgets *map_vws 
			= (struct MAP_GTK_widgets *) malloc(sizeof(struct MAP_GTK_widgets));
	if(map_vws == NULL){
		printf("malloc error for map_vws\n");
		exit(0);
    };
    
    map_vws->label_field_list = f_field_ctl;
    map_vws->label_dir_list = init_f_ctl_chara_array(c_link_scalar_dir_list_to_ctl, NULL);
    append_f_ctl_chara_array(c_link_vector_dir_list_to_ctl(NULL),
                             map_vws->label_dir_list);
    append_f_ctl_chara_array(c_link_stensor_dir_list_to_ctl(NULL), 
                             map_vws->label_dir_list);
    map_vws->color_vws = init_colormap_views_4_ctl(f_cmap_cbar_c->cmap_c);
    map_vws->psf_def_vws = init_PSF_GTK_widgets(f_field_ctl);
    return map_vws;
};
void dealloc_MAP_GTK_widgets(struct MAP_GTK_widgets *map_vws){
    dealloc_PSF_GTK_widgets(map_vws->psf_def_vws);
    dealloc_chara_int2_clist(map_vws->label_field_list);
    dealloc_chara_clist(map_vws->label_dir_list);
    dealloc_colormap_views_4_viewer(map_vws->color_vws);
    free(map_vws);
}

struct PVR_GTK_widgets * init_PVR_GTK_widgets(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct PVR_GTK_widgets *pvr_vws 
			= (struct PVR_GTK_widgets *) malloc(sizeof(struct PVR_GTK_widgets));
	if(pvr_vws == NULL){
		printf("malloc error for pvr_vws\n");
		exit(0);
    };
    pvr_vws->label_field_list = f_field_ctl;
    pvr_vws->label_dir_list = init_f_ctl_chara_array(c_link_scalar_dir_list_to_ctl, NULL);
    append_f_ctl_chara_array(c_link_vector_dir_list_to_ctl(NULL),
                             pvr_vws->label_dir_list);
    append_f_ctl_chara_array(c_link_stensor_dir_list_to_ctl(NULL), 
                             pvr_vws->label_dir_list);
    pvr_vws->color_vws = init_colormap_views_4_ctl(f_cmap_cbar_c->cmap_c);
    return pvr_vws;
};
void dealloc_PVR_GTK_widgets(struct PVR_GTK_widgets *pvr_vws){
    dealloc_colormap_views_4_viewer(pvr_vws->color_vws);
    dealloc_chara_int2_clist(pvr_vws->label_field_list);
    dealloc_chara_clist(pvr_vws->label_dir_list);
    free(pvr_vws);
}


struct LIC_GTK_widgets * init_LIC_GTK_widgets()
{
	struct LIC_GTK_widgets *lic_vws 
			= (struct LIC_GTK_widgets *) malloc(sizeof(struct LIC_GTK_widgets));
	if(lic_vws == NULL){
		printf("malloc error for lic_vws\n");
		exit(0);
    };
    return lic_vws;
};
void dealloc_LIC_GTK_widgets(struct LIC_GTK_widgets *lic_vws){
    free(lic_vws);
}

struct FLINE_GTK_widgets * init_FLINE_GTK_widgets()
{
	struct FLINE_GTK_widgets *fline_vws 
			= (struct FLINE_GTK_widgets *) malloc(sizeof(struct FLINE_GTK_widgets));
	if(fline_vws == NULL){
		printf("malloc error for fline_vws\n");
		exit(0);
    };
    return fline_vws;
};
void dealloc_FLINE_GTK_widgets(struct FLINE_GTK_widgets *fline_vws){
    free(fline_vws);
    return;
}


struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_MAP_ctl *f_map_ctl = init_f_VIZ_MAP_ctl(idx, f_parent);
    f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                          f_fld_ctl->f_field_ctl);
    f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_map_ctl;
}
void *dealloc_f_VIZ_MAP_ctl_GTK(void *void_in){
    struct f_VIZ_MAP_ctl *f_map_ctl = (struct f_VIZ_MAP_ctl *) void_in;
    dealloc_viewmat_GTK_widgets((struct viewmat_GTK_widgets *) f_map_ctl->f_mat->void_panel);
    dealloc_MAP_GTK_widgets((struct MAP_GTK_widgets *) f_map_ctl->void_panel);
    dealloc_f_VIZ_MAP_ctl(f_map_ctl);
    return NULL;
}

struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_PVR_ctl *f_pvr_ctl = init_f_VIZ_PVR_ctl(c_pvr_render_ctls_pvr_ctl, 
                                                         idx, f_parent);
    f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl->f_cmap_cbar_c, 
                                                          f_fld_ctl->f_field_ctl);
    return f_pvr_ctl;
}
void * dealloc_f_VIZ_PVR_ctl_GTK(void *void_in){
    struct f_VIZ_PVR_ctl *f_pvr_ctl = (struct f_VIZ_PVR_ctl *) void_in;
    dealloc_PVR_GTK_widgets((struct PVR_GTK_widgets *) f_pvr_ctl->void_panel);
    dealloc_f_VIZ_PVR_ctl(f_pvr_ctl);
    return NULL;
}

struct f_VIZ_LIC_PVR_ctl * init_f_VIZ_LIC_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_VIZ_LIC_PVR_ctl *f_lic_ctl = init_f_VIZ_LIC_PVR_ctl(idx, f_parent);
    f_lic_ctl->void_panel = (void *) init_LIC_GTK_widgets();
    return f_lic_ctl;
}
void * dealloc_f_VIZ_LIC_PVR_ctl_GTK(void *void_in){
    struct f_VIZ_LIC_PVR_ctl *f_lic_ctl = (struct f_VIZ_LIC_PVR_ctl *) void_in;
    dealloc_LIC_GTK_widgets((struct LIC_GTK_widgets *) f_lic_ctl->void_panel);
    dealloc_f_VIZ_LIC_PVR_ctl(f_lic_ctl);
    return NULL;
}

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_VIZ_FLINE_ctl *f_fline_ctl = init_f_VIZ_FLINE_ctl(idx, f_parent);
    f_fline_ctl->void_panel = (void *) init_FLINE_GTK_widgets();
    return f_fline_ctl;
}
void * dealloc_f_VIZ_FLINE_ctl_GTK(void *void_in){
    struct f_VIZ_FLINE_ctl *f_fline_ctl = (struct f_VIZ_FLINE_ctl *) void_in;
    dealloc_FLINE_GTK_widgets((struct FLINE_GTK_widgets *) f_fline_ctl->void_panel);
    dealloc_f_VIZ_FLINE_ctl(f_fline_ctl);
    return NULL;
}



GtkWidget *MHD_VIZs_ctl_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
                                 struct main_widgets *mWidgets){
    int i;
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_iso_ctls);i++){
        struct f_VIZ_ISO_ctl *f_iso_ctl
                = (struct f_VIZ_ISO_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_iso_ctls);
        f_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_viz_ctls->f_pvr_ctls);i++){
        struct f_VIZ_PVR_ctl *f_pvr_ctl
                = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i, f_MHD_ctl->f_viz_ctls->f_pvr_ctls);
        f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    
	GtkWidget *expand_MHD_psf = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->vpsf_Wgts, window);
	
	GtkWidget *expand_MHD_iso = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_iso_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_isosurf_ctls,
                                                          c_delete_viz_isosurf_ctls,
                                                          (void *) init_f_VIZ_ISO_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_ISO_ctl_GTK,
                                                          (void *) draw_viz_each_iso_ctl_vbox,
                                                          mWidgets->viso_Wgts, window);
	
	GtkWidget *expand_MHD_map = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->vmap_Wgts, window);
	
	GtkWidget *expand_MHD_pvr = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_pvr_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_pvr_render_ctls,
                                                          c_delete_viz_pvr_render_ctls,
                                                          (void *) init_f_VIZ_PVR_ctl_GTK,
                                                          dealloc_f_VIZ_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_pvr_ctl_vbox,
                                                          mWidgets->vpvr_Wgts, window);
	
	GtkWidget *expand_MHD_lic = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_lic_ctls, NULL,
                                                          c_append_viz_lic_render_ctls,
                                                          c_delete_viz_lic_render_ctls,
                                                          (void *) init_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_lic_ctl_vbox,
                                                          mWidgets->vlic_Wgts, window);
	
	GtkWidget *expand_MHD_fline = draw_array_block_ctl_vbox(f_MHD_ctl->f_viz_ctls->f_fline_ctls, NULL,
                                                          c_append_viz_fline_ctls,
                                                          c_delete_viz_fline_ctls,
                                                          (void *) init_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) draw_viz_each_fline_ctl_vbox,
                                                          mWidgets->vfline_Wgts, window);
	
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_psf, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_iso, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_map, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_pvr, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_lic, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_fline, FALSE, FALSE, 0);
	
	
	
	GtkWidget *expand_MHD_viz = draw_control_block(f_MHD_ctl->f_viz_ctls->c_block_name, 
													 f_MHD_ctl->f_viz_ctls->f_iflag,
                                                   window, vbox_viz);
    return expand_MHD_viz;
};

void MHD_control_expander(GtkWidget *window, struct f_MHD_control *f_MHD_ctl, 
						  struct main_widgets *mWidgets){
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
															 mWidgets->f_psph_vws);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_sph_shell, FALSE, FALSE, 0);
	
    GtkWidget *expand_MHD_model = MHD_model_ctl_expander(f_MHD_ctl->f_self, f_MHD_ctl->f_model_ctl,
                                                         mWidgets->model_wgts, window);
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
													 window, _node_monitor);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_node_monitor, FALSE, FALSE, 0);
	*/
	
    GtkWidget *expand_MHD_viz = MHD_VIZs_ctl_expander(window, f_MHD_ctl, mWidgets);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_viz, FALSE, FALSE, 0);
	
    GtkWidget *hbox_d1 = draw_int_item_entry_hbox(f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->f_crust_truncation_ctl);
	GtkWidget *expand_MHD_zm1 = draw_control_block(f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->c_block_name, 
                                                   f_MHD_ctl->f_zm_ctls->f_crust_filter_ctl->f_iflag,
                                                   window, hbox_d1);
    
    int i;
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zm_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zm_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_MHD_ctl->f_model_ctl->f_fld_ctl->f_field_ctl);
    };
    
	GtkWidget *expand_MHD_zm2 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zm_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->zm_psf_Wgts, window);
	GtkWidget *expand_MHD_zm3 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zRMS_psf_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          mWidgets->zrms_psf_Wgts, window);
    
	GtkWidget *expand_MHD_zm4 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zm_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->zm_map_Wgts, window);
	GtkWidget *expand_MHD_zm5 = draw_array_block_ctl_vbox(f_MHD_ctl->f_zm_ctls->f_zRMS_map_ctls,
                                                          (void *) f_MHD_ctl->f_model_ctl->f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          mWidgets->zrms_map_Wgts, window);
  
    GtkWidget *vbox_zm = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm5, FALSE, FALSE, 0);
    
	GtkWidget *expand_MHD_zm = draw_control_block(f_MHD_ctl->f_zm_ctls->c_block_name, 
													 f_MHD_ctl->f_zm_ctls->f_iflag,
													 window, vbox_zm);
	gtk_box_pack_start(GTK_BOX(mWidgets->ctl_MHD_inner_box), expand_MHD_zm, FALSE, FALSE, 0);
	
	
	
    mWidgets->ctl_MHD_Vbox = wrap_into_scroll_expansion_gtk(f_MHD_ctl->c_block_name, 560, 640,
                                                            window, mWidgets->ctl_MHD_inner_box);
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

