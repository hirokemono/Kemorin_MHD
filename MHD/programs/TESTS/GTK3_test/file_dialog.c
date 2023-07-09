
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_int2_IO.h"
#include "t_control_c_lists_w_file.h"
#include "t_ctl_array_chara3_items_c.h"
#include "t_ctl_array_chara_int3_items_c.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_data_4_fields_c.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_GTK.h"
#include "tree_view_4_field_GTK.h"
#include "tree_view_4_force_GTK.h"

#include "ctl_data_platforms_GTK.h"
#include "control_panel_4_dimless_GTK.h"
#include "control_panels_MHD_control_GTK.h"
#include "control_block_panel_GTK.h"
#include "control_panel_int_GTK.h"
#include "control_panel_int2_GTK.h"
#include "control_panel_4_sph_monitor_GTK.h"

            
extern void c_view_control_sph_SGS_MHD();

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

extern void * c_node_monitor_ctl_block_name(void *f_nmtr_ctl);
extern void * c_node_monitor_ctl_iflag(void *f_nmtr_ctl);
extern void * c_node_monitor_xx_monitor_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_node_mntr_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_group_mntr_ctl(void *f_nmtr_ctl);


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

extern void * c_SGS_model_ctl_block_name(void *f_sgs_ctl);
extern void * c_SGS_model_ctl_iflag(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_model_name_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_filter_name_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_DIFF_model_c_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_negative_clip_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_marging_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_perturbation_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_m_coef_type_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_hflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_cflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_mflux_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_maxwell_csim_type(void *f_sgs_ctl);
extern void * c_SGS_model_uxb_csim_type_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_coef_coord_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_buo_Csim_usage(void *f_sgs_ctl);
extern void * c_SGS_model_istep_dynamic_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_min_step_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_max_step_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_stabilize_weight(void *f_sgs_ctl);
extern void * c_SGS_model_del_shrink_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_del_extend_dynamic(void *f_sgs_ctl);
extern void * c_SGS_model_ngrp_radial_ave(void *f_sgs_ctl);
extern void * c_SGS_model_ngrp_med_ave_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_clipping_limit_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_hf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_cf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_mf_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_mxwl_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_uxb_factor_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_SGS_terms_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_commutate_fld_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_ffile_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_elayer_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_s3df_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_sph_filter_name(void *f_sgs_ctl);
extern int    c_SGS_model_num_sph_filter_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_sph_filter_ctl(int i, void *f_sgs_ctl);

extern void * c_SGS_3d_filter_ctl_block_name(void *f_s3df_ctl);
extern void * c_SGS_3d_filter_ctl_iflag(void *f_s3df_ctl);
extern void * c_SGS_3d_whole_filter_grp_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_fluid_filter_grp_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_momentum_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_heat_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_induction_filter_ctl(void *f_s3df_ctl);
extern void * c_SGS_3d_comp_filter_ctl(void *f_s3df_ctl);

extern void * c_SGS_filter_file_block_name(void *f_ffile_ctl);
extern void * c_SGS_filter_file_iflag(void *f_ffile_ctl);
extern void * c_SGS_filter_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_coef_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_elen_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_moms_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_filter_wide_head_ctl(void *f_ffile_ctl);
extern void * c_SGS_model_coef_ini_head(void *f_ffile_ctl);
extern void * c_SGS_commute_coef_ini_head(void *f_ffile_ctl);
extern void * c_SGS_filter_elen_format(void *f_ffile_ctl);
extern void * c_SGS_filter_3d_format(void *f_ffile_ctl);
extern void * c_SGS_filter_wide_format(void *f_ffile_ctl);
extern void * c_SGS_model_coef_rst_format(void *f_ffile_ctl);
extern void * c_SGS_commute_coef_rst_format(void *f_ffile_ctl);

extern void * c_SGS_layering_ctl_block_name(void *f_elayer_ctl);
extern void * c_SGS_layering_ctl_iflag(void *f_elayer_ctl);
extern void * c_SGS_layering_grp_type_ctl(void *f_elayer_ctl);
extern void * c_SGS_layer_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_igrp_stack_layer_ctl(void *f_elayer_ctl);
extern void * c_SGS_num_layering_grp_ctl(void *f_elayer_ctl);
extern void * c_SGS_num_fluid_layer_grp_ctl(void *f_elayer_ctl);
extern void * c_SGS_start_layer_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_start_fluid_grp_name_ctl(void *f_elayer_ctl);
extern void * c_SGS_ngrp_on_sphere_ctl(void *f_elayer_ctl);

extern void * c_SGS_sph_filter_ctl_block_name(void *f_sph_filter_ctl);
extern void * c_SGS_sph_filter_ctl_iflag(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_sph_filter_type_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_r_filter_type_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_maximum_moments_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_sph_filter_width_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_r_filter_width_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_first_reference_ctl(void *f_sph_filter_ctl);
extern void * c_SPH_SGS_second_reference_ctl(void *f_sph_filter_ctl);


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

extern void * c_map_render_ctls_block_name(void *f_map_ctls);
extern int    c_map_render_ctls_num_map_ctl(void *f_map_ctls);
extern char * c_map_render_ctls_fname(int idx, void *f_map_ctls);
extern void * c_map_render_ctls_map_ctl(int idx, void *f_map_ctls);
extern void * c_append_viz_map_render_ctls(int idx, char *block_name, void *f_map_ctls);
extern void * c_delete_viz_map_render_ctls(int idx, void *f_map_ctls);

extern void * c_pvr_render_ctls_block_name(void *f_pvr_ctls);
extern int    c_pvr_render_ctls_num_pvr_ctl(void *f_pvr_ctls);
extern char * c_pvr_render_ctls_fname(int idx, void *f_pvr_ctls);
extern void * c_pvr_render_ctls_pvr_ctl(int idx, void *f_pvr_ctls);
extern void * c_append_viz_pvr_render_ctls(int idx, char *block_name, void *f_pvr_ctls);
extern void * c_delete_viz_pvr_render_ctls(int idx, void *f_pvr_ctls);

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

extern void * c_MHD_node_bc_ctl_block_name(void *f_nbc_ctl);
extern void * c_MHD_node_bc_ctl_iflag(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_T_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_U_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_P_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_C_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_B_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_MP_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_A_ctl(void *f_nbc_ctl);
extern void * c_MHD_node_bc_node_bc_J_ctl(void *f_nbc_ctl);


extern void * c_MHD_surf_bc_ctl_block_name(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_ctl_iflag(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_HF_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_ST_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_PN_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_BN_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_JN_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_AN_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_MPN_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_CF_ctl(void *f_sbc_ctl);
extern void * c_MHD_surf_bc_surf_bc_INF_ctl(void *f_sbc_ctl);


extern void * c_MHD_evolution_ctl_block_name(void *f_evo_ctl);
extern void * c_MHD_evolution_ctl_iflag(void *f_evo_ctl);
extern void * c_MHD_t_evo_field_ctl(void *f_evo_ctl);

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


struct f_MHD_SGS_SPH_filter_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_sph_filter_type_ctl;
	struct f_ctl_chara_item *f_radial_filter_type_ctl;
	struct f_ctl_int_item *f_maximum_moments_ctl;
	struct f_ctl_real_item *f_sphere_filter_width_ctl;
	struct f_ctl_real_item *f_radial_filter_width_ctl;
	struct f_ctl_int_item *f_first_reference_ctl;
	struct f_ctl_int_item *f_second_reference_ctl;
};

struct f_MHD_SGS_3d_filter_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_whole_filter_grp_ctl;
	struct chara_clist *f_fluid_filter_grp_ctl;
	struct f_ctl_chara_item *f_momentum_filter_ctl;
	struct f_ctl_chara_item *f_heat_filter_ctl;
	struct f_ctl_chara_item *f_induction_filter_ctl;
	struct f_ctl_chara_item *f_compostion_filter_ctl;
};

struct f_MHD_SGS_layer_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_layering_grp_type_ctl;
	struct chara_clist *f_layer_grp_name_ctl;
	struct int_clist   *f_igrp_stack_layer_ctl;
	struct f_ctl_int_item   *f_num_layering_grp_ctl;
	struct f_ctl_int_item   *f_num_fl_layer_grp_ctl;
	struct f_ctl_chara_item *f_start_layering_grp_name_ctl;
	struct f_ctl_chara_item *f_start_fl_layer_grp_name_ctl;
	struct f_ctl_int_item   *f_ngrp_SGS_on_sphere_ctl;
};

struct f_MHD_SGS_filter_file_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_filter_head_ctl;
	struct f_ctl_chara_item *f_filter_coef_head_ctl;
	struct f_ctl_chara_item *f_filter_elen_head_ctl;
	struct f_ctl_chara_item *f_filter_moms_head_ctl;
	struct f_ctl_chara_item *f_filter_wide_head_ctl;
	struct f_ctl_chara_item *f_model_coef_ini_head_ctl;
	struct f_ctl_chara_item *f_commute_coef_ini_head_ctl;
	struct f_ctl_chara_item *f_filter_elen_format;
	struct f_ctl_chara_item *f_filter_3d_format;
	struct f_ctl_chara_item *f_filter_wide_format;
	struct f_ctl_chara_item *f_model_coef_rst_format;
	struct f_ctl_chara_item *f_commute_coef_rst_format;
};

struct f_MHD_SGS_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_SGS_model_name_ctl;
	struct f_ctl_chara_item *f_SGS_filter_name_ctl;
	struct f_ctl_chara_item *f_DIFF_model_coef_ctl;
	struct f_ctl_chara_item *f_SGS_negative_clip_ctl;
	struct f_ctl_chara_item *f_SGS_marging_ctl;
	struct f_ctl_chara_item *f_SGS_perturbation_ctl;
	struct f_ctl_chara_item *f_SGS_model_coef_type_ctl;
	struct f_ctl_chara_item *f_heat_flux_csim_type_ctl;
	struct f_ctl_chara_item *f_comp_flux_csim_type_ctl;
	struct f_ctl_chara_item *f_mom_flux_csim_type_ctl;
	struct f_ctl_chara_item *f_maxwell_csim_type_ctl;
	struct f_ctl_chara_item *f_uxb_csim_type_ctl;
	struct f_ctl_chara_item *f_SGS_model_coef_coord_ctl;
	struct f_ctl_chara_item *f_SGS_buo_Csim_usage_ctl;
	
	struct f_ctl_int_item *f_istep_dynamic_ctl;
	struct f_ctl_int_item *f_min_step_dynamic_ctl;
	struct f_ctl_int_item *f_max_step_dynamic_ctl;
	
	struct f_ctl_real_item *f_stabilize_weight_ctl;
	struct f_ctl_real_item *f_delta_to_shrink_dynamic_ctl;
	struct f_ctl_real_item *f_delta_to_extend_dynamic_ctl;
	
	struct f_ctl_int_item *f_ngrp_radial_ave_ctl;
	struct f_ctl_int_item *f_ngrp_med_ave_ctl;
	
	struct f_ctl_real_item *f_clipping_limit_ctl;
	struct f_ctl_real_item *f_SGS_hf_factor_ctl;
	struct f_ctl_real_item *f_SGS_cf_factor_ctl;
	struct f_ctl_real_item *f_SGS_mf_factor_ctl;
	struct f_ctl_real_item *f_SGS_mxwl_factor_ctl;
	struct f_ctl_real_item *f_SGS_uxb_factor_ctl;
	
	struct chara_clist *f_SGS_terms_ctl;
	struct chara_clist *f_commutate_fld_ctl;
	
	struct f_MHD_SGS_filter_file_control *f_ffile_ctl;
	struct f_MHD_SGS_layer_control       *f_elayer_ctl;
	struct f_MHD_SGS_3d_filter_control    *f_s3df_ctl;
	
	char *f_sph_filter_name;
	int   f_num_sph_filter_ctl;
	struct void_clist *f_sph_filter_ctl;
};

struct f_MHD_node_bc_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara2_real_clist *f_node_bc_T_ctl;
	struct chara2_real_clist *f_node_bc_U_ctl;
	struct chara2_real_clist *f_node_bc_P_ctl;
	struct chara2_real_clist *f_node_bc_C_ctl;
	struct chara2_real_clist *f_node_bc_B_ctl;
	struct chara2_real_clist *f_node_bc_MP_ctl;
	struct chara2_real_clist *f_node_bc_A_ctl;
	struct chara2_real_clist *f_node_bc_J_ctl;
};

struct f_MHD_surf_bc_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara2_real_clist *f_surf_bc_HF_ctl;
	struct chara2_real_clist *f_surf_bc_ST_ctl;
	struct chara2_real_clist *f_surf_bc_PN_ctl;
	struct chara2_real_clist *f_surf_bc_BN_ctl;
	struct chara2_real_clist *f_surf_bc_JN_ctl;
	struct chara2_real_clist *f_surf_bc_AN_ctl;
	struct chara2_real_clist *f_surf_bc_MPN_ctl;
	struct chara2_real_clist *f_surf_bc_CF_ctl;
	struct chara2_real_clist *f_surf_bc_INF_ctl;
};

struct f_MHD_time_evo_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_t_evo_field_ctl;
};

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
	
	struct f_ctl_chara_item *f_FEM_gravity_model;
	struct f_ctl_chara_item *f_gravity;
	struct chara_real_clist *f_gravity_vector;
};

struct f_MHD_Coriolis_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_FEM_coriolis_model;
	struct f_ctl_chara_item *f_FEM_coriolis_implicit;
	struct chara_real_clist *f_system_rotation;
};

struct f_MHD_magneto_cv_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_filterd_induction_ctl;
	struct f_ctl_chara_item *f_magneto_cv;
	struct chara_real_clist *f_ext_magne;
};

struct f_MHD_magnetic_scale_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_real_item *f_mag_to_kin_energy_ctl;
};

struct f_MHD_reftemp_point_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_real_item *f_value;
	struct f_ctl_real_item *f_depth;
};

struct f_MHD_takepiro_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_real_item *f_stratified_sigma_ctl;
	struct f_ctl_real_item *f_stratified_width_ctl;
	struct f_ctl_real_item *f_stratified_outer_r_ctl;
};

struct f_MHD_temp_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_filterd_advect_ctl;
	struct f_ctl_chara_item *f_reference_ctl;
	struct f_ctl_chara_item *f_stratified_ctl;
	struct f_ctl_chara_item *f_ref_file_ctl;
	struct f_ctl_real_item  *f_ICB_diffuse_reduction_ctl;
	
	struct f_MHD_reftemp_point_control  *f_low_ctl;
	struct f_MHD_reftemp_point_control  *f_high_ctl;
	struct f_MHD_takepiro_model_control *f_takepiro_ctl;
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

struct f_VIZ_LIC_ctls{
	void *f_lic_pvr_ctl;
	void *f_lic_lic_ctl;

};

struct f_MHD_viz_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	int f_num_psf_ctl;
	struct void_file_clist *f_psf_ctls;
	
	int f_num_iso_ctl;
	struct void_file_clist *f_iso_ctls;
	
	int f_num_map_ctl;
	struct void_file_clist *f_map_ctls;
	
	int f_num_pvr_ctl;
	struct void_file_clist *f_pvr_ctls;
	
	int f_num_lic_ctl;
	struct void_file_clist *f_lic_ctls;
	
	int f_num_fline_ctl;
	struct void_file_clist *f_fline_ctls;
	
	void * f_repart_ctl;
	void * f_fname_vol_repart_ctl;
};

struct f_MHD_zm_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_crust_filter_ctl;
	void * f_zm_psf_ctls;
	void * f_zRMS_psf_ctls;
	void * f_zm_map_ctls;
	void * f_zRMS_map_ctls;
};

struct f_MHD_node_monitor_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_xx_4_monitor_ctl;
	void * f_node_4_monitor_ctl;
	void * f_group_4_monitor_ctl;
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
};


void draw_ISO_control_list(GtkWidget *window, GtkWidget *vbox0, struct iso_ctl_c *iso_c);
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

struct f_MHD_SGS_SPH_filter_control * init_f_MHD_SGS_SPH_filter_control(int idx_in, void *f_parent)
{
	struct f_MHD_SGS_SPH_filter_control *f_sph_filter_ctl 
			= (struct f_MHD_SGS_SPH_filter_control *) malloc(sizeof(struct f_MHD_SGS_SPH_filter_control));
	if(f_sph_filter_ctl == NULL){
		printf("malloc error for f_sph_filter_ctl\n");
		exit(0);
	};
	
	f_sph_filter_ctl->f_self = c_SGS_model_sph_filter_ctl(idx_in, f_parent);
	
	f_sph_filter_ctl->f_iflag =  (int *)  c_SGS_sph_filter_ctl_iflag(f_sph_filter_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_sph_filter_ctl_block_name(f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sph_filter_ctl->f_sph_filter_type_ctl =    init_f_ctl_chara_item(c_SPH_SGS_sph_filter_type_ctl,
																		 f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_radial_filter_type_ctl = init_f_ctl_chara_item(c_SPH_SGS_r_filter_type_ctl,
																		 f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_maximum_moments_ctl =    init_f_ctl_int_item(c_SPH_SGS_maximum_moments_ctl,
																	  f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_sphere_filter_width_ctl =  init_f_ctl_real_item(c_SPH_SGS_sph_filter_width_ctl,
																		f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_radial_filter_width_ctl =  init_f_ctl_real_item(c_SPH_SGS_r_filter_width_ctl,
																		f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_first_reference_ctl =   init_f_ctl_int_item(c_SPH_SGS_first_reference_ctl,
																	f_sph_filter_ctl->f_self);
	f_sph_filter_ctl->f_second_reference_ctl =  init_f_ctl_int_item(c_SPH_SGS_second_reference_ctl,
																	f_sph_filter_ctl->f_self);
	return f_sph_filter_ctl;
};


struct f_MHD_SGS_3d_filter_control * init_f_MHD_SGS_3d_filter_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_3d_filter_control *f_s3df_ctl 
			= (struct f_MHD_SGS_3d_filter_control *) malloc(sizeof(struct f_MHD_SGS_3d_filter_control));
	if(f_s3df_ctl == NULL){
		printf("malloc error for f_s3df_ctl\n");
		exit(0);
	};
	
	f_s3df_ctl->f_self =  c_load_self(f_parent);
	
	f_s3df_ctl->f_iflag =  (int *)  c_SGS_3d_filter_ctl_iflag(f_s3df_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_3d_filter_ctl_block_name(f_s3df_ctl->f_self);
	f_s3df_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_s3df_ctl->f_whole_filter_grp_ctl =  init_f_ctl_chara_array(c_SGS_3d_whole_filter_grp_ctl,
																 f_s3df_ctl->f_self);
	f_s3df_ctl->f_fluid_filter_grp_ctl =  init_f_ctl_chara_array(c_SGS_3d_fluid_filter_grp_ctl,
																 f_s3df_ctl->f_self);
	f_s3df_ctl->f_momentum_filter_ctl =   init_f_ctl_chara_item(c_SGS_3d_momentum_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_heat_filter_ctl =       init_f_ctl_chara_item(c_SGS_3d_heat_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_induction_filter_ctl =  init_f_ctl_chara_item(c_SGS_3d_induction_filter_ctl,
																f_s3df_ctl->f_self);
	f_s3df_ctl->f_compostion_filter_ctl = init_f_ctl_chara_item(c_SGS_3d_comp_filter_ctl,
																f_s3df_ctl->f_self);
	return f_s3df_ctl;
}

struct f_MHD_SGS_layer_control * init_f_MHD_SGS_layer_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_layer_control *f_elayer_ctl 
			= (struct f_MHD_SGS_layer_control *) malloc(sizeof(struct f_MHD_SGS_layer_control));
	if(f_elayer_ctl == NULL){
		printf("malloc error for f_elayer_ctl\n");
		exit(0);
	};
	
	f_elayer_ctl->f_self =  c_load_self(f_parent);
	
	f_elayer_ctl->f_iflag =  (int *)  c_SGS_layering_ctl_iflag(f_elayer_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_layering_ctl_block_name(f_elayer_ctl->f_self);
	f_elayer_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_elayer_ctl->f_layering_grp_type_ctl =  init_f_ctl_chara_item(c_SGS_layering_grp_type_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_layer_grp_name_ctl =     init_f_ctl_chara_array(c_SGS_layer_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_igrp_stack_layer_ctl =  init_f_ctl_int_array(c_SGS_igrp_stack_layer_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_num_layering_grp_ctl =  init_f_ctl_int_item(c_SGS_num_layering_grp_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_num_fl_layer_grp_ctl =  init_f_ctl_int_item(c_SGS_num_fluid_layer_grp_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_start_layering_grp_name_ctl =  init_f_ctl_chara_item(c_SGS_start_layer_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_start_fl_layer_grp_name_ctl =  init_f_ctl_chara_item(c_SGS_start_fluid_grp_name_ctl,
														   f_elayer_ctl->f_self);
	f_elayer_ctl->f_ngrp_SGS_on_sphere_ctl =  init_f_ctl_int_item(c_SGS_ngrp_on_sphere_ctl,
														   f_elayer_ctl->f_self);
	return f_elayer_ctl;
}

struct f_MHD_SGS_filter_file_control * init_f_MHD_SGS_filter_file_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_filter_file_control *f_ffile_ctl 
			= (struct f_MHD_SGS_filter_file_control *) malloc(sizeof(struct f_MHD_SGS_filter_file_control));
	if(f_ffile_ctl == NULL){
		printf("malloc error for f_ffile_ctl\n");
		exit(0);
	};
	
	f_ffile_ctl->f_self =  c_load_self(f_parent);
	
	f_ffile_ctl->f_iflag =  (int *)  c_SGS_filter_file_iflag(f_ffile_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_filter_file_block_name(f_ffile_ctl->f_self);
	f_ffile_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_ffile_ctl->f_filter_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_coef_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_coef_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_elen_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_elen_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_moms_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_moms_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_wide_head_ctl =  init_f_ctl_chara_item(c_SGS_filter_wide_head_ctl,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_model_coef_ini_head_ctl =  init_f_ctl_chara_item(c_SGS_model_coef_ini_head,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_commute_coef_ini_head_ctl =  init_f_ctl_chara_item(c_SGS_commute_coef_ini_head,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_elen_format =  init_f_ctl_chara_item(c_SGS_filter_elen_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_3d_format =  init_f_ctl_chara_item(c_SGS_filter_3d_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_filter_wide_format =  init_f_ctl_chara_item(c_SGS_filter_wide_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_model_coef_rst_format =  init_f_ctl_chara_item(c_SGS_model_coef_rst_format,
																 f_ffile_ctl->f_self);
	f_ffile_ctl->f_commute_coef_rst_format =  init_f_ctl_chara_item(c_SGS_commute_coef_rst_format,
																 f_ffile_ctl->f_self);
	return f_ffile_ctl;
}

struct f_MHD_SGS_model_control * init_f_MHD_SGS_model_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_SGS_model_control *f_sgs_ctl 
			= (struct f_MHD_SGS_model_control *) malloc(sizeof(struct f_MHD_SGS_model_control));
	if(f_sgs_ctl == NULL){
		printf("malloc error for f_sgs_ctl\n");
		exit(0);
	};
	
	f_sgs_ctl->f_self =  c_load_self(f_parent);
	
	f_sgs_ctl->f_iflag =        (int *) c_SGS_model_ctl_iflag(f_sgs_ctl->f_self);
	char *f_block_name =   (char *) c_SGS_model_ctl_block_name(f_sgs_ctl->f_self);
	f_sgs_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sgs_ctl->f_SGS_model_name_ctl =    init_f_ctl_chara_item(c_SGS_model_SGS_model_name_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_filter_name_ctl =   init_f_ctl_chara_item(c_SGS_model_filter_name_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_DIFF_model_coef_ctl =   init_f_ctl_chara_item(c_SGS_model_DIFF_model_c_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_negative_clip_ctl = init_f_ctl_chara_item(c_SGS_model_negative_clip_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_marging_ctl =       init_f_ctl_chara_item(c_SGS_model_SGS_marging_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_perturbation_ctl =  init_f_ctl_chara_item(c_SGS_model_perturbation_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_type_ctl =  init_f_ctl_chara_item(c_SGS_model_m_coef_type_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_heat_flux_csim_type_ctl =  init_f_ctl_chara_item(c_SGS_model_hflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_comp_flux_csim_type_ctl =  init_f_ctl_chara_item(c_SGS_model_cflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_mom_flux_csim_type_ctl =   init_f_ctl_chara_item(c_SGS_model_mflux_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_maxwell_csim_type_ctl =    init_f_ctl_chara_item(c_SGS_model_maxwell_csim_type, f_sgs_ctl->f_self);
	f_sgs_ctl->f_uxb_csim_type_ctl =        init_f_ctl_chara_item(c_SGS_model_uxb_csim_type_ctl,
																  f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_coord_ctl = init_f_ctl_chara_item(c_SGS_model_coef_coord_ctl, 
																  f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_buo_Csim_usage_ctl =   init_f_ctl_chara_item(c_SGS_model_SGS_buo_Csim_usage, 
																  f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_istep_dynamic_ctl =    init_f_ctl_int_item(c_SGS_model_istep_dynamic_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_min_step_dynamic_ctl = init_f_ctl_int_item(c_SGS_model_min_step_dynamic, f_sgs_ctl->f_self);
	f_sgs_ctl->f_max_step_dynamic_ctl = init_f_ctl_int_item(c_SGS_model_max_step_dynamic, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_stabilize_weight_ctl =        init_f_ctl_real_item(c_SGS_model_stabilize_weight, 
																	f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_shrink_dynamic_ctl = init_f_ctl_real_item(c_SGS_model_del_shrink_dynamic, 
																	f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_extend_dynamic_ctl = init_f_ctl_real_item(c_SGS_model_del_extend_dynamic, 
																	f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_ngrp_radial_ave_ctl = init_f_ctl_int_item(c_SGS_model_ngrp_radial_ave, f_sgs_ctl->f_self);
	f_sgs_ctl->f_ngrp_med_ave_ctl =    init_f_ctl_int_item(c_SGS_model_ngrp_med_ave_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_clipping_limit_ctl =  init_f_ctl_real_item(c_SGS_model_clipping_limit_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_hf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_hf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_cf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_cf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mf_factor_ctl =   init_f_ctl_real_item(c_SGS_model_SGS_mf_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mxwl_factor_ctl = init_f_ctl_real_item(c_SGS_model_mxwl_factor_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_uxb_factor_ctl =  init_f_ctl_real_item(c_SGS_model_SGS_uxb_factor_ctl, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_SGS_terms_ctl =       init_f_ctl_chara_array(c_SGS_model_SGS_terms_ctl, f_sgs_ctl->f_self);
	f_sgs_ctl->f_commutate_fld_ctl =   init_f_ctl_chara_array(c_SGS_model_commutate_fld_ctl, f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_ffile_ctl = init_f_MHD_SGS_filter_file_control(c_SGS_model_ffile_ctl, 
																f_sgs_ctl->f_self);
	f_sgs_ctl->f_elayer_ctl = init_f_MHD_SGS_layer_control(c_SGS_model_elayer_ctl, 
														   f_sgs_ctl->f_self);
	f_sgs_ctl->f_s3df_ctl = init_f_MHD_SGS_3d_filter_control(c_SGS_model_s3df_ctl,
															 f_sgs_ctl->f_self);
	
	
	f_block_name =   (char *) c_SGS_model_sph_filter_name(f_sgs_ctl->f_self);
 	f_sgs_ctl->f_sph_filter_ctl = init_void_clist(strngcopy_from_f(f_block_name));
	f_sgs_ctl->f_sph_filter_ctl->f_parent =  f_sgs_ctl->f_self;
	f_sgs_ctl->f_num_sph_filter_ctl = c_SGS_model_num_sph_filter_ctl(f_sgs_ctl->f_self);
	int i;
	for(i=0;i<f_sgs_ctl->f_num_sph_filter_ctl;i++){
		struct f_MHD_SGS_SPH_filter_control *void_in 
				= init_f_MHD_SGS_SPH_filter_control(i, f_sgs_ctl->f_self);
		append_void_clist((void *) void_in, f_sgs_ctl->f_sph_filter_ctl);
	}
	return f_sgs_ctl;
};

struct f_MHD_node_bc_control * init_f_MHD_node_bc_control(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_node_bc_control *f_nbc_ctl 
			= (struct f_MHD_node_bc_control *) malloc(sizeof(struct f_MHD_node_bc_control));
	if(f_nbc_ctl == NULL){
		printf("malloc error for f_nbc_ctl\n");
		exit(0);
	};
	
	f_nbc_ctl->f_self =  c_load_self(f_parent);
	
	f_nbc_ctl->f_iflag =        (int *) c_MHD_node_bc_ctl_iflag(f_nbc_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_node_bc_ctl_block_name(f_nbc_ctl->f_self);
	f_nbc_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_nbc_ctl->f_node_bc_T_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_T_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_U_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_U_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_P_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_P_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_C_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_C_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_B_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_B_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_MP_ctl = init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_MP_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_A_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_A_ctl, f_nbc_ctl->f_self);
	f_nbc_ctl->f_node_bc_J_ctl =  init_f_ctl_c2r_array(c_MHD_node_bc_node_bc_J_ctl, f_nbc_ctl->f_self);
	return f_nbc_ctl;
};


struct f_MHD_surf_bc_control * init_f_MHD_surf_bc_control(void *(*c_load_self)(void *f_parent), 
														  void *f_parent)
{
	struct f_MHD_surf_bc_control *f_sbc_ctl 
			= (struct f_MHD_surf_bc_control *) malloc(sizeof(struct f_MHD_surf_bc_control));
	if(f_sbc_ctl == NULL){
		printf("malloc error for f_sbc_ctl\n");
		exit(0);
	};
	
	f_sbc_ctl->f_self =  c_load_self(f_parent);
	
	f_sbc_ctl->f_iflag =        (int *) c_MHD_surf_bc_ctl_iflag(f_sbc_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_surf_bc_ctl_block_name(f_sbc_ctl->f_self);
	f_sbc_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_sbc_ctl->f_surf_bc_HF_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_HF_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_ST_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_ST_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_PN_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_PN_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_BN_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_BN_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_JN_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_JN_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_AN_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_AN_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_MPN_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_MPN_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_CF_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_CF_ctl, f_sbc_ctl->f_self);
	f_sbc_ctl->f_surf_bc_INF_ctl =  init_f_ctl_c2r_array(c_MHD_surf_bc_surf_bc_INF_ctl, f_sbc_ctl->f_self);
	return f_sbc_ctl;
}

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
	
	f_earea_ctl->f_iflag =        (int *) c_MHD_evolution_ctl_iflag(f_earea_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_evolution_ctl_block_name(f_earea_ctl->f_self);
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

struct void_file_clist * init_f_VIZ_psf_ctls(void *f_viz_ctls_self, int *f_num_psf_ctl)
{
    char *f_block_name =   (char *) c_section_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_psf_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_psf_ctls->f_parent =  c_visualizations_psf_ctls(f_viz_ctls_self);
	*f_num_psf_ctl = c_section_ctls_num_psf_ctl(f_psf_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_psf_ctl;i++){
        f_block_name = c_section_ctls_fname(i, f_psf_ctls->f_parent);
        void *void_in =  c_section_ctls_psf_ctl(i, f_psf_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) void_in, f_psf_ctls);
	}
	return f_psf_ctls;
}

struct void_file_clist * init_f_VIZ_iso_ctls(void *f_viz_ctls_self, int *f_num_iso_ctl)
{
    char *f_block_name =   (char *) c_isosurf_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_iso_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_iso_ctls->f_parent =  c_visualizations_iso_ctls(f_viz_ctls_self);
	*f_num_iso_ctl = c_isosurf_ctls_num_iso_ctl(f_iso_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_iso_ctl;i++){
        f_block_name = c_isosurf_ctls_fname(i, f_iso_ctls->f_parent);
        void *void_in =  c_isosurf_ctls_iso_ctl(i, f_iso_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) void_in, f_iso_ctls);
	}
	return f_iso_ctls;
}

struct void_file_clist * init_f_VIZ_map_ctls(void *f_viz_ctls_self, int *f_num_map_ctl)
{
    char *f_block_name =   (char *) c_map_render_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_map_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_map_ctls->f_parent =  c_visualizations_map_ctls(f_viz_ctls_self);
	*f_num_map_ctl = c_map_render_ctls_num_map_ctl(f_map_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_map_ctl;i++){
        f_block_name = c_map_render_ctls_fname(i, f_map_ctls->f_parent);
        void *void_in =  c_map_render_ctls_map_ctl(i, f_map_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) void_in, f_map_ctls);
	}
	return f_map_ctls;
}

struct void_file_clist * init_f_VIZ_pvr_ctls(void *f_viz_ctls_self, int *f_num_pvr_ctl)
{
    char *f_block_name =   (char *) c_pvr_render_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_pvr_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_pvr_ctls->f_parent =  c_visualizations_pvr_ctls(f_viz_ctls_self);
	*f_num_pvr_ctl = c_pvr_render_ctls_num_pvr_ctl(f_pvr_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_pvr_ctl;i++){
        f_block_name = c_pvr_render_ctls_fname(i, f_pvr_ctls->f_parent);
        void *void_in =  c_pvr_render_ctls_pvr_ctl(i, f_pvr_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) void_in, f_pvr_ctls);
	}
	return f_pvr_ctls;
}

struct void_file_clist * init_f_VIZ_lic_ctls(void *f_viz_ctls_self, int *f_num_lic_ctl)
{
    char *f_block_name =   (char *) c_lic_render_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_lic_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_lic_ctls->f_parent =  c_visualizations_lic_ctls(f_viz_ctls_self);
	*f_num_lic_ctl = c_lic_render_ctls_num_lic_ctl(f_lic_ctls->f_parent);
	
	
	int i;
	for(i=0;i<*f_num_lic_ctl;i++){
		struct f_VIZ_LIC_ctls *f_lic_ctl_tmp
				= (struct f_VIZ_LIC_ctls *) malloc(sizeof(struct f_VIZ_LIC_ctls));
		if(f_lic_ctl_tmp == NULL){
			printf("malloc error for f_viz_ctls\n");
			exit(0);
		};
        f_block_name = c_lic_render_ctls_fname(i, f_lic_ctls->f_parent);
        f_lic_ctl_tmp->f_lic_pvr_ctl =  c_lic_render_ctls_pvr_ctl(i, f_lic_ctls->f_parent);
        f_lic_ctl_tmp->f_lic_lic_ctl =  c_lic_render_ctls_lic_ctl(i, f_lic_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) f_lic_ctl_tmp, f_lic_ctls);
	}
	return f_lic_ctls;
}

struct void_file_clist * init_f_VIZ_fline_ctls(void *f_viz_ctls_self, int *f_num_fline_ctl)
{
    char *f_block_name =   (char *) c_fline_ctls_block_name(f_viz_ctls_self);
	struct void_file_clist *f_pvr_ctls = init_void_file_clist(strngcopy_from_f(f_block_name));
	f_pvr_ctls->f_parent =  c_visualizations_fline_ctls(f_viz_ctls_self);
	*f_num_fline_ctl = c_fline_ctls_num_fline_ctl(f_pvr_ctls->f_parent);
	
	int i;
	for(i=0;i<*f_num_fline_ctl;i++){
        f_block_name = c_fline_ctls_fname(i, f_pvr_ctls->f_parent);
        void *void_in =  c_fline_ctls_fline_ctl(i, f_pvr_ctls->f_parent);
		append_void_file_clist(strngcopy_from_f(f_block_name), (void *) void_in, f_pvr_ctls);
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
	
	f_viz_ctls->f_psf_ctls =   init_f_VIZ_psf_ctls(f_viz_ctls->f_self,   &f_viz_ctls->f_num_psf_ctl);
	f_viz_ctls->f_iso_ctls =   init_f_VIZ_iso_ctls(f_viz_ctls->f_self,   &f_viz_ctls->f_num_iso_ctl);
	f_viz_ctls->f_map_ctls =   init_f_VIZ_map_ctls(f_viz_ctls->f_self,   &f_viz_ctls->f_num_map_ctl);
	f_viz_ctls->f_pvr_ctls =   init_f_VIZ_pvr_ctls(f_viz_ctls->f_self,   &f_viz_ctls->f_num_pvr_ctl);
	f_viz_ctls->f_lic_ctls =   init_f_VIZ_lic_ctls(f_viz_ctls->f_self,   &f_viz_ctls->f_num_lic_ctl);
	f_viz_ctls->f_fline_ctls = init_f_VIZ_fline_ctls(f_viz_ctls->f_self, &f_viz_ctls->f_num_fline_ctl);
	f_viz_ctls->f_repart_ctl =    c_visualizations_repart_ctl(f_viz_ctls->f_self);
	f_viz_ctls->f_fname_vol_repart_ctl = c_visualizations_fname_vrepart(f_viz_ctls->f_self);
	return f_viz_ctls;
}

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
	
	f_zm_ctls->f_crust_filter_ctl =    c_dynamo_vizs_crust_filter_ctl(f_zm_ctls->f_self);
	f_zm_ctls->f_zm_psf_ctls =    c_dynamo_vizs_zm_psf_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zRMS_psf_ctls =    c_dynamo_vizs_zRMS_psf_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zm_map_ctls =    c_dynamo_vizs_zm_map_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zRMS_map_ctls =    c_dynamo_vizs_zRMS_map_ctls(f_zm_ctls->f_self);
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
	
	f_nmtr_ctl->f_xx_4_monitor_ctl =    c_node_monitor_xx_monitor_ctl(f_nmtr_ctl->f_self);
	f_nmtr_ctl->f_node_4_monitor_ctl =  c_node_monitor_node_mntr_ctl(f_nmtr_ctl->f_self);
	f_nmtr_ctl->f_group_4_monitor_ctl = c_node_monitor_group_mntr_ctl(f_nmtr_ctl->f_self);
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
		
//		draw_ISO_control_list(window, mWidgets->main_Vbox, iso_GTK0->iso_c);
		MHD_control_expander(window, f_MHD_ctl, mWidgets);
		gtk_box_pack_start(GTK_BOX(mWidgets->main_Vbox), mWidgets->ctl_MHD_Vbox, FALSE, FALSE, 0);
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


GtkWidget * iso_field_ctl_list_box(struct iso_field_ctl_c *iso_fld_c){
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_2[2];
	
	char *c_label;
	
	iso_GTK0->color_field_ctl = init_field_ctl_c();
	iso_GTK0->color_fields_vws = init_field_views_GTK(iso_GTK0->color_field_ctl);
	iso_GTK0->color_fields_vws->used_tree_view 
			= create_field_tree_view(iso_GTK0->color_fields_vws->all_fld_list,
									 iso_GTK0->color_fields_vws->fld_ctl_gtk);
	iso_GTK0->color_fields_vws->unused_field_tree_view
			= create_unused_field_tree_views(iso_GTK0->color_fields_vws->all_fld_list);
	
	iso_GTK0->color_fields_vws->field_group_tree_view
			= create_field_group_tree_view(iso_GTK0->color_fields_vws->all_fld_list);
	iso_GTK0->color_fields_vws->all_field_tree_view
			= create_all_field_tree_views(iso_GTK0->color_fields_vws->all_fld_list);
	create_direction_tree_views(iso_GTK0->color_fields_vws);
	
	
	GtkWidget *color_flags_tree_view
			= create_control_flags_tree_view(iso_fld_c->flag_iso_color);
	
	add_control_combobox_vbox_old(iso_fld_c->output_type_ctl->c_tbl,
							  iso_fld_c->flag_iso_color, color_flags_tree_view, vbox_1);
	printf("%le\n", iso_fld_c->output_value_ctl->r_data);
	c_label = duplicate_underscore(iso_fld_c->label_fld_on_iso_ctl->label[ 2]);
	vbox_2[0] = make_real_hbox(1, c_label, iso_fld_c->output_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_field_selection_box(iso_GTK0->color_fields_vws, window, vbox_1);
	
	return vbox_1;
};
struct f_MHD_tree_views{
	struct f_sph_shell_views *f_psph_vws;
	struct f_MHD_equations_views *f_eqs_vws;
	
	GtkWidget *f_force_tree_view;
	GtkWidget *f_force_default_view;
};
struct f_MHD_tree_views *f_MHD_vws;





void draw_ISO_control_list(GtkWidget *window, GtkWidget *vbox0, struct iso_ctl_c *iso_c){
	GtkWidget *vbox_1;
	GtkWidget *vbox_2[iso_c->label_iso_ctl_w_dpl->num_labels];
	
	char *c_label;
	
	GtkWidget *file_fmt_flags_tree_view
			= create_control_flags_tree_view(iso_c->flag_iso_format);
	
	/* Generate expander */
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	vbox_2[0] = make_text_hbox
    		(0, duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 0]),
             iso_c->iso_file_head_ctl);

	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_control_combobox_vbox_old(iso_c->iso_output_type_ctl->c_tbl,
							  iso_c->flag_iso_format, file_fmt_flags_tree_view, vbox_1);
	
	vbox_2[2] = iso_define_ctl_list_box(iso_c->iso_def_c);
	vbox_2[3] = iso_field_ctl_list_box(iso_c->iso_fld_c);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 2]), 
			 400, 200, window, vbox_2[2]);
	
    gtk_box_pack_start(GTK_BOX(vbox_1), expander, FALSE, FALSE, 0);
	GtkWidget *expander1 = wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 3]), 
			 400, 500, window, vbox_2[3]);
    gtk_box_pack_start(GTK_BOX(vbox_1), expander1, FALSE, FALSE, 0);
	c_label = isosurface_control_head();
	GtkWidget *expander2 = wrap_into_expanded_frame_gtk(duplicate_underscore(c_label),
								 560, 600, window, vbox_1);
	gtk_box_pack_start(GTK_BOX(vbox0), expander2, FALSE, FALSE, 0);
	return;
}

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

	GtkWidget *vbox_m1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_time_evo = draw_control_block(f_MHD_ctl->f_model_ctl->f_evo_ctl->c_block_name,
														f_MHD_ctl->f_model_ctl->f_evo_ctl->f_iflag,
														560, 500, window, vbox_m1);
	
	
	GtkWidget *vbox_m2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_tevo_area = draw_control_block(f_MHD_ctl->f_model_ctl->f_earea_ctl->c_block_name,
														 f_MHD_ctl->f_model_ctl->f_earea_ctl->f_iflag,
														 560, 500, window, vbox_m2);
	
	
	GtkWidget *vbox_m3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_node_bc = draw_control_block(f_MHD_ctl->f_model_ctl->f_nbc_ctl->c_block_name,
                                                      f_MHD_ctl->f_model_ctl->f_nbc_ctl->f_iflag,
													   560, 500, window, vbox_m3);
	
	
	GtkWidget *vbox_m4 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_surf_bc = draw_control_block(f_MHD_ctl->f_model_ctl->f_sbc_ctl->c_block_name,
                                                      f_MHD_ctl->f_model_ctl->f_sbc_ctl->f_iflag,
                                                      560, 500, window, vbox_m4);
	
	
	GtkWidget *vbox_MHD_force = add_c_list_box_w_addbottun(f_MHD_ctl->f_model_ctl->f_frc_ctl->f_force_names, 
														   f_MHD_vws->f_force_tree_view);
	GtkWidget *expand_MHD_dimless = add_dimless_selection_box(f_MHD_ctl->f_model_ctl->f_dless_ctl, window);
	
	
	GtkWidget *vbox_eqs = draw_MHD_equations_vbox(f_MHD_ctl->f_model_ctl->f_eqs_ctl, 
												  f_MHD_vws->f_eqs_vws, window);
	GtkWidget *expand_MHD_eqs = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_iflag,
													 560, 400, window, vbox_eqs);
	
	GtkWidget *vbox_m = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_fields, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_time_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_tevo_area, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_node_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_surf_bc, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), vbox_MHD_force, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_dimless, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_eqs, FALSE, FALSE, 0);
	
	
	mWidgets->ctl_MHD_inner_box =   gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget * vbox_plt_c = draw_platform_control_vbox(f_MHD_ctl->f_plt, window);
	GtkWidget * vbox_plt_o = draw_platform_control_vbox(f_MHD_ctl->f_org_plt, window);
	GtkWidget * vbox_plt_n = draw_platform_control_vbox(f_MHD_ctl->f_new_plt, window);
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
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
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

