
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

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
extern void * c_SGS_model_num_sph_filter_ctl(void *f_sgs_ctl);
extern void * c_SGS_model_sph_filter_ctl(int i, void *f_sgs_ctl);

extern void * c_sph_monitor_ctl_block_name(void *f_smonitor_ctl);
extern void * c_sph_monitor_ctl_iflag(void *f_smonitor_ctl);

extern void * c_sph_monitor_ctl_v_pwr_name(void *f_smonitor_ctl);
extern int    c_sph_monitor_num_vspec_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_vspec_ctl(int idx, void *f_smonitor_ctl);
extern void * c_sph_monitor_lp_ctl(void *f_smonitor_ctl);

extern void * c_sph_monitor_g_pwr(void *f_smonitor_ctl);
extern void * c_sph_monitor_pspec_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_circ_ctls(void *f_smonitor_ctl);
extern void * c_sph_monitor_dbench_ctl(void *f_smonitor_ctl);
extern void * c_sph_monitor_fdip_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_vol_ave_prefix(void *f_smonitor_ctl);
extern void * c_sph_mntr_vol_pspec_prefix(void *f_smonitor_ctl);
extern void * c_sph_mntr_v_pwr_spectr_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_degree_v_spectra_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_order_v_spectra_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_diff_v_lm_spectr_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_axis_v_power_switch(void *f_smonitor_ctl);
extern void * c_sph_mntr_h_Nusselt_file_pfx(void *f_smonitor_ctl);
extern void * c_sph_mntr_c_Nusselt_file_pfx(void *f_smonitor_ctl);
extern void * c_sph_mntr_h_Nusselt_file_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_c_Nusselt_file_fmt(void *f_smonitor_ctl);
extern void * c_sph_mntr_lscale_file_pfix_ctl(void *f_smonitor_ctl);
extern void * c_sph_mntr_lscale_file_fmt_ctl(void *f_smonitor_ctl);

extern void * c_sph_v_spectr_ctl_block_name(void *f_v_pwr_item);
extern void * c_sph_v_spectr_ctl_iflag(void *f_v_pwr_item);
extern void * c_sph_volume_spec_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_ave_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_spec_format_ctl(void *f_v_pwr_item);
extern void * c_sph_degree_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_order_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_diff_v_lm_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_axis_v_power_switch(void *f_v_pwr_item);
extern void * c_sph_v_spec_inner_radius_ctl(void *f_v_pwr_item);
extern void * c_sph_v_spec_outer_radius_ctl(void *f_v_pwr_item);


struct f_MHD_SGS_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void *f_SGS_model_name_ctl;
	void *f_SGS_filter_name_ctl;
	void *f_DIFF_model_coef_ctl;
	void *f_SGS_negative_clip_ctl;
	void *f_SGS_marging_ctl;
	void *f_SGS_perturbation_ctl;
	void *f_SGS_model_coef_type_ctl;
	void *f_heat_flux_csim_type_ctl;
	void *f_comp_flux_csim_type_ctl;
	void *f_mom_flux_csim_type_ctl;
	void *f_maxwell_csim_type_ctl;
	void *f_uxb_csim_type_ctl;
	void *f_SGS_model_coef_coord_ctl;
	void *f_SGS_buo_Csim_usage_ctl;
	void *f_istep_dynamic_ctl;
	void *f_min_step_dynamic_ctl;
	void *f_max_step_dynamic_ctl;
	void *f_stabilize_weight_ctl;
	void *f_delta_to_shrink_dynamic_ctl;
	void *f_delta_to_extend_dynamic_ctl;
	void *f_ngrp_radial_ave_ctl;
	void *f_ngrp_med_ave_ctl;
	void *f_clipping_limit_ctl;
	void *f_SGS_hf_factor_ctl;
	void *f_SGS_cf_factor_ctl;
	void *f_SGS_mf_factor_ctl;
	void *f_SGS_mxwl_factor_ctl;
	void *f_SGS_uxb_factor_ctl;
	void *f_SGS_terms_ctl;
	void *f_commutate_fld_ctl;
	void *f_ffile_ctl;
	void *f_elayer_ctl;
	void *f_s3df_ctl;
	int  *f_num_sph_filter_ctl;
	void **f_sph_filter_ctl;
};

struct f_MHD_model_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_fld_ctl;
	void * f_evo_ctl;
	void * f_earea_ctl;
	void * f_nbc_ctl;
	void * f_sbc_ctl;
	struct f_MHD_forces_control *f_frc_ctl;
	struct f_MHD_dimless_control * f_dless_ctl;
	struct f_MHD_equations_control * f_eqs_ctl;
	void * f_g_ctl;
	void * f_cor_ctl;
	void * f_mcv_ctl;
	void * f_bscale_ctl;
	void * f_reft_ctl;
	void * f_refc_ctl;
	struct f_MHD_SGS_model_control * f_sgs_ctl;
};

struct f_sph_vol_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_ctl_chara_item *f_volume_spec_file_ctl;
	struct f_ctl_chara_item *f_volume_ave_file_ctl;
	struct f_ctl_chara_item *f_volume_spec_format_ctl;
	struct f_ctl_chara_item *f_degree_v_spectra_switch;
	struct f_ctl_chara_item *f_order_v_spectra_switch;
	struct f_ctl_chara_item *f_diff_v_lm_spectra_switch;
	struct f_ctl_chara_item *f_axis_v_power_switch;
	struct f_ctl_real_item *f_inner_radius_ctl;
	struct f_ctl_real_item *f_outer_radius_ctl;
};

struct f_MHD_sph_monitor_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	char * c_v_pwr_name;
	int f_num_vspec_ctl;
	struct void_clist *f_v_pwr;
	void * f_lp_ctl;
	void * f_g_pwr;
	void * f_pspec_ctl;
	void * f_circ_ctls;
	void * f_dbench_ctl;
	void * f_fdip_ctl;
	
	struct f_ctl_chara_item * f_volume_average_prefix;
	struct f_ctl_chara_item * f_volume_pwr_spectr_prefix;
	struct f_ctl_chara_item * f_volume_pwr_spectr_format;
	struct f_ctl_chara_item * f_degree_v_spectra_switch;
	struct f_ctl_chara_item * f_order_v_spectra_switch;
	struct f_ctl_chara_item * f_diff_v_lm_spectra_switch;
	struct f_ctl_chara_item * f_axis_v_power_switch;
	struct f_ctl_chara_item * f_heat_Nusselt_file_prefix;
	struct f_ctl_chara_item * f_comp_Nusselt_file_prefix;
	struct f_ctl_chara_item * f_heat_Nusselt_file_format;
	struct f_ctl_chara_item * f_comp_Nusselt_file_format;
	struct f_ctl_chara_item * f_typ_scale_file_prefix_ctl;
	struct f_ctl_chara_item * f_typ_scale_file_format_ctl;
};


struct f_MHD_viz_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	void * f_psf_ctls;
	void * f_iso_ctls;
	void * f_map_ctls;
	void * f_pvr_ctls;
	void * f_fline_ctls;
	void * f_lic_ctls;
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

void draw_MHD_control_list(GtkWidget *window, GtkWidget *vbox0, struct f_MHD_control *f_MHD_ctl, struct iso_ctl_c *iso_c);

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
GtkWidget *vbox_0;

GtkWidget *entry_3, *entry_4, *entry_5;

double rtest = 2.5;
int ntest = 66;
char *ctest = "ahahahaha";
struct chara_ctl_item item_test = {55, "tako_tako"};
struct chara_ctl_item *ptem_test;

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
	
	f_sgs_ctl->f_SGS_model_name_ctl = c_SGS_model_SGS_model_name_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_filter_name_ctl = c_SGS_model_filter_name_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_DIFF_model_coef_ctl = c_SGS_model_DIFF_model_c_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_negative_clip_ctl = c_SGS_model_negative_clip_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_marging_ctl = c_SGS_model_SGS_marging_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_perturbation_ctl = c_SGS_model_perturbation_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_type_ctl = c_SGS_model_m_coef_type_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_heat_flux_csim_type_ctl = c_SGS_model_hflux_csim_type(f_sgs_ctl->f_self);
	f_sgs_ctl->f_comp_flux_csim_type_ctl = c_SGS_model_cflux_csim_type(f_sgs_ctl->f_self);
	f_sgs_ctl->f_mom_flux_csim_type_ctl = c_SGS_model_mflux_csim_type(f_sgs_ctl->f_self);
	f_sgs_ctl->f_maxwell_csim_type_ctl = c_SGS_model_maxwell_csim_type(f_sgs_ctl->f_self);
	f_sgs_ctl->f_uxb_csim_type_ctl = c_SGS_model_uxb_csim_type_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_model_coef_coord_ctl = c_SGS_model_coef_coord_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_buo_Csim_usage_ctl = c_SGS_model_SGS_buo_Csim_usage(f_sgs_ctl->f_self);
	f_sgs_ctl->f_istep_dynamic_ctl = c_SGS_model_istep_dynamic_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_min_step_dynamic_ctl = c_SGS_model_min_step_dynamic(f_sgs_ctl->f_self);
	f_sgs_ctl->f_max_step_dynamic_ctl = c_SGS_model_max_step_dynamic(f_sgs_ctl->f_self);
	f_sgs_ctl->f_stabilize_weight_ctl = c_SGS_model_stabilize_weight(f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_shrink_dynamic_ctl = c_SGS_model_del_shrink_dynamic(f_sgs_ctl->f_self);
	f_sgs_ctl->f_delta_to_extend_dynamic_ctl = c_SGS_model_del_extend_dynamic(f_sgs_ctl->f_self);
	f_sgs_ctl->f_ngrp_radial_ave_ctl = c_SGS_model_ngrp_radial_ave(f_sgs_ctl->f_self);
	f_sgs_ctl->f_ngrp_med_ave_ctl = c_SGS_model_ngrp_med_ave_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_clipping_limit_ctl = c_SGS_model_clipping_limit_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_hf_factor_ctl = c_SGS_model_SGS_hf_factor_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_cf_factor_ctl = c_SGS_model_SGS_cf_factor_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mf_factor_ctl = c_SGS_model_SGS_mf_factor_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_mxwl_factor_ctl = c_SGS_model_mxwl_factor_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_uxb_factor_ctl = c_SGS_model_SGS_uxb_factor_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_SGS_terms_ctl = c_SGS_model_SGS_terms_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_commutate_fld_ctl = c_SGS_model_commutate_fld_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_ffile_ctl = c_SGS_model_ffile_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_elayer_ctl = c_SGS_model_elayer_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_s3df_ctl = c_SGS_model_s3df_ctl(f_sgs_ctl->f_self);
	f_sgs_ctl->f_num_sph_filter_ctl = (int *) c_SGS_model_num_sph_filter_ctl(f_sgs_ctl->f_self);
	
	f_sgs_ctl->f_sph_filter_ctl = (void **) malloc(f_sgs_ctl->f_num_sph_filter_ctl[0] * sizeof(void *));
	if(f_sgs_ctl->f_sph_filter_ctl == NULL){
		printf("malloc error for f_sgs_ctl->f_sph_filter_ctl\n");
		exit(0);
	};
	printf("f_sgs_ctl->f_num_sph_filter_ctl[0] %d\n", f_sgs_ctl->f_num_sph_filter_ctl[0]);
	int i;
	for(i=0;i<f_sgs_ctl->f_num_sph_filter_ctl[0];i++){
		f_sgs_ctl->f_sph_filter_ctl[i] = (void *) malloc(sizeof(void));
		if(f_sgs_ctl->f_sph_filter_ctl[i] == NULL){
			printf("malloc error for %d -th f_sgs_ctl->f_sph_filter_ctl\n", i);
			exit(0);
		};
		f_sgs_ctl->f_sph_filter_ctl[i] = c_SGS_model_sph_filter_ctl((i+1), f_sgs_ctl->f_self);
	}
	return f_sgs_ctl;
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
	
	f_model_ctl->f_fld_ctl =    c_MHD_mdl_fld_ctl(f_model_ctl->f_self);
	f_model_ctl->f_evo_ctl =    c_MHD_mdl_evo_ctl(f_model_ctl->f_self);
	f_model_ctl->f_earea_ctl =  c_MHD_mdl_earea_ctl(f_model_ctl->f_self);
	f_model_ctl->f_nbc_ctl =    c_MHD_mdl_nbc_ctl(f_model_ctl->f_self);
	f_model_ctl->f_sbc_ctl =    c_MHD_mdl_sbc_ctl(f_model_ctl->f_self);
	f_model_ctl->f_frc_ctl =    init_f_MHD_forces_ctl(c_MHD_mdl_frc_ctl, f_model_ctl->f_self);
	f_model_ctl->f_dless_ctl =  init_f_MHD_dimless_ctl(c_MHD_mdl_dless_ctl, f_model_ctl->f_self);
	f_model_ctl->f_eqs_ctl =    init_f_MHD_equations_ctl(c_MHD_mdl_eqs_ctl, f_model_ctl->f_self);
	f_model_ctl->f_g_ctl =      c_MHD_mdl_g_ctl(f_model_ctl->f_self);
	f_model_ctl->f_cor_ctl =    c_MHD_mdl_cor_ctl(f_model_ctl->f_self);
	f_model_ctl->f_mcv_ctl =    c_MHD_mdl_mcv_ctl(f_model_ctl->f_self);
	f_model_ctl->f_bscale_ctl = c_MHD_mdl_bscale_ctl(f_model_ctl->f_self);
	f_model_ctl->f_reft_ctl =   c_MHD_mdl_reft_ctl(f_model_ctl->f_self);
	f_model_ctl->f_refc_ctl =   c_MHD_mdl_refc_ctl(f_model_ctl->f_self);
	f_model_ctl->f_sgs_ctl =    init_f_MHD_SGS_model_control(c_MHD_sgs_ctl, f_addition);
	return f_model_ctl;
}

struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(void *(*c_load_self)(int idx, void *f_parent),
														  int idx, void *f_parent)
{
	struct f_sph_vol_spectr_ctls *f_v_pwr_item 
			= (struct f_sph_vol_spectr_ctls *) malloc(sizeof(struct f_sph_vol_spectr_ctls));
	if(f_v_pwr_item == NULL){
		printf("malloc error for f_v_pwr_item\n");
		exit(0);
	};
	
	f_v_pwr_item->f_self =  c_load_self(idx, f_parent);
	
	f_v_pwr_item->f_iflag = (int *) c_sph_v_spectr_ctl_iflag(f_v_pwr_item->f_self);
	char *f_block_name =   (char *) c_sph_v_spectr_ctl_block_name(f_v_pwr_item->f_self);
	f_v_pwr_item->c_block_name = strngcopy_from_f(f_block_name);
	
	f_v_pwr_item->f_volume_spec_file_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_file_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_ave_file_ctl =  init_f_ctl_chara_item(c_sph_volume_ave_file_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_spec_format_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_format_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_degree_v_spectra_switch =  init_f_ctl_chara_item(c_sph_degree_v_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_order_v_spectra_switch =  init_f_ctl_chara_item(c_sph_order_v_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_diff_v_lm_spectra_switch =  init_f_ctl_chara_item(c_sph_diff_v_lm_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_axis_v_power_switch =  init_f_ctl_chara_item(c_sph_axis_v_power_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_inner_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_inner_radius_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_outer_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_outer_radius_ctl, f_v_pwr_item->f_self);
	return f_v_pwr_item;
}

struct f_MHD_sph_monitor_ctls * init_f_MHD_sph_monitor_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_sph_monitor_ctls *f_smonitor_ctl 
			= (struct f_MHD_sph_monitor_ctls *) malloc(sizeof(struct f_MHD_sph_monitor_ctls));
	if(f_smonitor_ctl == NULL){
		printf("malloc error for f_smonitor_ctl\n");
		exit(0);
	};
	
	f_smonitor_ctl->f_self =  c_load_self(f_parent);
	printf("f_self %p\n", f_smonitor_ctl->f_self);
	
	f_smonitor_ctl->f_iflag = (int *) c_sph_monitor_ctl_iflag(f_smonitor_ctl->f_self);
	char *f_block_name =   (char *) c_sph_monitor_ctl_block_name(f_smonitor_ctl->f_self);
	f_smonitor_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
    f_block_name =   (char *) c_sph_monitor_ctl_v_pwr_name(f_smonitor_ctl->f_self);
    f_smonitor_ctl->c_v_pwr_name = strngcopy_from_f(f_block_name);
	f_smonitor_ctl->f_num_vspec_ctl = c_sph_monitor_num_vspec_ctl(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_v_pwr = init_void_clist();
	
	printf("f_smonitor_ctl->f_num_vspec_ctl %d\n", f_smonitor_ctl->f_num_vspec_ctl);
	int i;
	for(i=0;i<f_smonitor_ctl->f_num_vspec_ctl;i++){
		append_void_clist((void *) init_f_sph_vol_spectr_ctls(c_sph_monitor_vspec_ctl, i, f_smonitor_ctl->f_self), 
						  f_smonitor_ctl->f_v_pwr);
	}
	f_smonitor_ctl->f_lp_ctl =     c_sph_monitor_lp_ctl(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_g_pwr =      c_sph_monitor_g_pwr(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_pspec_ctl =  c_sph_monitor_pspec_ctl(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_circ_ctls =  c_sph_monitor_circ_ctls(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_dbench_ctl = c_sph_monitor_dbench_ctl(f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_fdip_ctl =   c_sph_monitor_fdip_ctl(f_smonitor_ctl->f_self);
	
	f_smonitor_ctl->f_volume_average_prefix =     init_f_ctl_chara_item(c_sph_mntr_vol_ave_prefix, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_volume_pwr_spectr_prefix =  init_f_ctl_chara_item(c_sph_mntr_vol_pspec_prefix, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_volume_pwr_spectr_format =  init_f_ctl_chara_item(c_sph_mntr_v_pwr_spectr_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_degree_v_spectra_switch =   init_f_ctl_chara_item(c_sph_mntr_degree_v_spectra_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_order_v_spectra_switch =    init_f_ctl_chara_item(c_sph_mntr_order_v_spectra_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_diff_v_lm_spectra_switch =  init_f_ctl_chara_item(c_sph_mntr_diff_v_lm_spectr_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_axis_v_power_switch =       init_f_ctl_chara_item(c_sph_mntr_axis_v_power_switch, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_heat_Nusselt_file_prefix =  init_f_ctl_chara_item(c_sph_mntr_h_Nusselt_file_pfx, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_comp_Nusselt_file_prefix =  init_f_ctl_chara_item(c_sph_mntr_c_Nusselt_file_pfx, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_heat_Nusselt_file_format =  init_f_ctl_chara_item(c_sph_mntr_h_Nusselt_file_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_comp_Nusselt_file_format =  init_f_ctl_chara_item(c_sph_mntr_c_Nusselt_file_fmt, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_typ_scale_file_prefix_ctl = init_f_ctl_chara_item(c_sph_mntr_lscale_file_pfix_ctl, f_smonitor_ctl->f_self);
	f_smonitor_ctl->f_typ_scale_file_format_ctl = init_f_ctl_chara_item(c_sph_mntr_lscale_file_fmt_ctl, f_smonitor_ctl->f_self);
	return f_smonitor_ctl;
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
	printf("f_self %p\n", f_viz_ctls->f_self);
	
	f_viz_ctls->f_iflag =        (int *) c_visualizations_iflag(f_viz_ctls->f_self);
	char *f_block_name =   (char *) c_visualizations_block_name(f_viz_ctls->f_self);
	f_viz_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
	f_viz_ctls->f_psf_ctls =    c_visualizations_psf_ctls(f_viz_ctls->f_self);
	f_viz_ctls->f_iso_ctls =    c_visualizations_iso_ctls(f_viz_ctls->f_self);
	f_viz_ctls->f_map_ctls =    c_visualizations_map_ctls(f_viz_ctls->f_self);
	f_viz_ctls->f_pvr_ctls =    c_visualizations_pvr_ctls(f_viz_ctls->f_self);
	f_viz_ctls->f_fline_ctls =    c_visualizations_fline_ctls(f_viz_ctls->f_self);
	f_viz_ctls->f_lic_ctls =    c_visualizations_lic_ctls(f_viz_ctls->f_self);
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
		
		
		draw_MHD_control_list(window, vbox_0, f_MHD_ctl, iso_GTK0->iso_c);
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
	
	add_control_combobox_vbox(iso_fld_c->output_type_ctl->c_tbl, iso_fld_c->output_type_ctl->c_tbl,
							  iso_fld_c->flag_iso_color, 
							  color_flags_tree_view, vbox_1);
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

void store_each_vspec_ctl_to_F(struct f_sph_vol_spectr_ctls *f_v_pwr_item)
{
	c_store_chara_item_charavalue(f_v_pwr_item->f_volume_ave_file_ctl->f_self, 
								  f_v_pwr_item->f_volume_ave_file_ctl->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_volume_spec_file_ctl->f_self, 
								  f_v_pwr_item->f_volume_spec_file_ctl->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_volume_spec_format_ctl->f_self, 
								  f_v_pwr_item->f_volume_spec_format_ctl->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_degree_v_spectra_switch->f_self, 
								  f_v_pwr_item->f_degree_v_spectra_switch->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_order_v_spectra_switch->f_self, 
								  f_v_pwr_item->f_order_v_spectra_switch->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_diff_v_lm_spectra_switch->f_self, 
								  f_v_pwr_item->f_diff_v_lm_spectra_switch->c_charavalue);
	c_store_chara_item_charavalue(f_v_pwr_item->f_axis_v_power_switch->f_self, 
								  f_v_pwr_item->f_axis_v_power_switch->c_charavalue);
	c_store_real_item_realvalue(f_v_pwr_item->f_inner_radius_ctl->f_self, 
								f_v_pwr_item->f_inner_radius_ctl->c_realvalue);
	c_store_real_item_realvalue(f_v_pwr_item->f_outer_radius_ctl->f_self, 
								f_v_pwr_item->f_outer_radius_ctl->c_realvalue);
	return;
}

static GtkWidget * draw_sph_each_vspec_ctl_vbox(struct f_sph_vol_spectr_ctls *f_v_pwr_item, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_ave_file_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_file_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_format_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_v_pwr_item->f_inner_radius_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_v_pwr_item->f_outer_radius_ctl);
    GtkWidget *hbox_6 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_degree_v_spectra_switch);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_order_v_spectra_switch);
    GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_diff_v_lm_spectra_switch);
    GtkWidget *hbox_9 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_axis_v_power_switch);
	
	GtkWidget *vbox_v_pwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_9,  FALSE, FALSE, 0);
	GtkWidget *expand_v_pwr = wrap_into_expanded_frame_gtk(duplicate_underscore(f_v_pwr_item->c_block_name),
														   480, 240, window, vbox_v_pwr);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_v_pwr, FALSE, FALSE, 0);
    return vbox_out;
};

static GtkWidget * draw_sph_vol_spectr_ctl_vbox(struct void_clist *f_v_pwr, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	int i;
	for(i=0;i<count_void_clist(f_v_pwr);i++){
		void *v_pwr = void_clist_at_index(i, (void *) f_v_pwr);
		GtkWidget *vbox_z = draw_sph_each_vspec_ctl_vbox((struct f_sph_vol_spectr_ctls *) v_pwr, window);
		gtk_box_pack_start(GTK_BOX(vbox_out), vbox_z,  FALSE, FALSE, 0);
	}
   return vbox_out;
};


static GtkWidget * draw_MHD_sph_monitor_ctls_vbox(struct f_MHD_sph_monitor_ctls *f_smonitor_ctl, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	int i;
	
	int itmp[1];
	itmp[0] = f_smonitor_ctl->f_num_vspec_ctl;
	GtkWidget *vbox_smontr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_p11 = draw_sph_vol_spectr_ctl_vbox(f_smonitor_ctl->f_v_pwr, window);
    GtkWidget *expand_vpwrs = draw_control_block(f_smonitor_ctl->c_v_pwr_name, itmp,
												 480, 440, window, vbox_p11);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), expand_vpwrs, FALSE, FALSE, 0);
	
	/*
    GtkWidget *vbox_p11 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_lp_ctl->c_block_name,
                                                     f_smonitor_ctl->f_lp_ctl->f_iflag,
                                                     560, 500, window, vbox_p11);
    
    GtkWidget *vbox_p1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_g_pwr->c_block_name,
                                                     f_smonitor_ctl->f_g_pwr->f_iflag,
                                                     560, 500, window, vbox_p1);
    
    GtkWidget *vbox_p2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_pspec_ctl->c_block_name,
                                                     f_smonitor_ctl->f_pspec_ctl->f_iflag,
                                                     560, 500, window, vbox_p2);
    
    GtkWidget *vbox_p3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_circ_ctls->c_block_name,
                                                     f_smonitor_ctl->f_circ_ctls->f_iflag,
                                                     560, 500, window, vbox_p3);
    
    GtkWidget *vbox_p4 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_dbench_ctl->c_block_name,
                                                     f_smonitor_ctl->f_dbench_ctl->f_iflag,
                                                     560, 500, window, vbox_p4);
    
    GtkWidget *vbox_p5 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkWidget *expand_MHD_zm = draw_control_block(f_smonitor_ctl->f_fdip_ctl->c_block_name,
                                                     f_smonitor_ctl->f_fdip_ctl->f_iflag,
                                                     560, 500, window, vbox_p5);
    */

    
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_average_prefix);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_pwr_spectr_prefix);
	GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_volume_pwr_spectr_format);
	
    GtkWidget *hbox_4 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_degree_v_spectra_switch);
    GtkWidget *hbox_5 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_order_v_spectra_switch);
    GtkWidget *hbox_6 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_diff_v_lm_spectra_switch);
	GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_smonitor_ctl->f_axis_v_power_switch);
	
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_heat_Nusselt_file_prefix);
	GtkWidget *hbox_9 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_heat_Nusselt_file_format);
	
    GtkWidget *hbox_10 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_comp_Nusselt_file_prefix);
	GtkWidget *hbox_11 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_comp_Nusselt_file_format);
	
    GtkWidget *hbox_12 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_typ_scale_file_prefix_ctl);
    GtkWidget *hbox_13 = draw_chara_item_entry_hbox(f_smonitor_ctl->f_typ_scale_file_format_ctl);
	
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), hbox_13, FALSE, FALSE, 0);
    /*
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p4, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_smontr), vbox_p5, FALSE, FALSE, 0);
*/
    GtkWidget *expand_smntr = draw_control_block(f_smonitor_ctl->c_block_name, f_smonitor_ctl->f_iflag,
                                               480, 240, window, vbox_smontr);
    gtk_box_pack_start(GTK_BOX(vbox_out), expand_smntr, FALSE, FALSE, 0);
    return vbox_out;
};


void draw_MHD_control_list(GtkWidget *window, GtkWidget *vbox0, struct f_MHD_control *f_MHD_ctl, struct iso_ctl_c *iso_c){
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
	add_control_combobox_vbox(iso_c->iso_output_type_ctl->c_tbl, iso_c->iso_output_type_ctl->c_tbl,
							  iso_c->flag_iso_format, 
							  file_fmt_flags_tree_view, vbox_1);
	
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
    
	
	int iflag_ptr[1];
	iflag_ptr[0] = 0;
	
	
	
	f_MHD_vws = (struct f_MHD_tree_views *) malloc(sizeof(struct f_MHD_tree_views));
	if(f_MHD_vws == NULL){
		printf("malloc error for f_MHD_tree_views\n");
		exit(0);
	};
	
	GtkWidget *vbox_MHD_force = add_c_list_box_w_addbottun(f_MHD_ctl->f_model_ctl->f_frc_ctl->f_force_names, 
														   f_MHD_vws->f_force_tree_view);
	GtkWidget *expand_MHD_dimless = add_dimless_selection_box(f_MHD_ctl->f_model_ctl->f_dless_ctl, window);
	
	
	GtkWidget *vbox_eqs = draw_MHD_equations_vbox(f_MHD_ctl->f_model_ctl->f_eqs_ctl, 
												  f_MHD_vws->f_eqs_vws, window);
	GtkWidget *expand_MHD_eqs = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_iflag,
													 560, 400, window, vbox_eqs);
	
	GtkWidget *vbox_m = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m), vbox_MHD_force, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_dimless, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_eqs, FALSE, FALSE, 0);
	
	
	GtkWidget *vbox_plt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget * vbox_plt_c =     draw_platform_control_vbox(f_MHD_ctl->f_plt, window);
	GtkWidget * vbox_plt_o = draw_platform_control_vbox(f_MHD_ctl->f_org_plt, window);
	GtkWidget * vbox_plt_n = draw_platform_control_vbox(f_MHD_ctl->f_new_plt, window);
	gtk_box_pack_start(GTK_BOX(vbox_plt), vbox_plt_c, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_plt), vbox_plt_o, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_plt), vbox_plt_n, FALSE, FALSE, 0);
	
	GtkWidget *expand_sph_shell = MHD_sph_shell_ctl_expander(window, f_MHD_ctl->f_psph_ctl,
															 f_MHD_ctl->f_fname_psph, 
															 f_MHD_vws->f_psph_vws);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_sph_shell, FALSE, FALSE, 0);
	
	GtkWidget *expand_MHD_model = draw_control_block(f_MHD_ctl->f_model_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_iflag,
													 560, 500, window, vbox_m);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_model, FALSE, FALSE, 0);
	
	
    GtkWidget *expand_MHD_control = draw_MHD_control_expand(window, f_MHD_ctl->f_smctl_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_control, FALSE, FALSE, 0);
	
	GtkWidget *vbox_smonitor = draw_MHD_sph_monitor_ctls_vbox(f_MHD_ctl->f_smonitor_ctl, window);
	gtk_box_pack_start(GTK_BOX(vbox_plt), vbox_smonitor, FALSE, FALSE, 0);
	
	/*
	GtkWidget *vbox_node_monitor = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_node_monitor = draw_control_block(f_MHD_ctl->f_nmtr_ctl->c_block_name, 
													 f_MHD_ctl->f_nmtr_ctl->f_iflag,
													 560, 500, window, _node_monitor);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_node_monitor, FALSE, FALSE, 0);
	*/
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_viz = draw_control_block(f_MHD_ctl->f_viz_ctls->c_block_name, 
													 f_MHD_ctl->f_viz_ctls->f_iflag,
													 560, 500, window, vbox_viz);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_viz, FALSE, FALSE, 0);
	
	GtkWidget *vbox_zm = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_zm = draw_control_block(f_MHD_ctl->f_zm_ctls->c_block_name, 
													 f_MHD_ctl->f_zm_ctls->f_iflag,
													 560, 500, window, vbox_zm);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_zm, FALSE, FALSE, 0);
	
	
	
	GtkWidget *expand_MHD = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 600, window, vbox_plt);
	gtk_box_pack_start(GTK_BOX(vbox0), expand_MHD, FALSE, FALSE, 0);

	GtkWidget *expander2 = wrap_into_expanded_frame_gtk(duplicate_underscore(c_label),
								 560, 600, window, vbox_1);
	gtk_box_pack_start(GTK_BOX(vbox0), expander2, FALSE, FALSE, 0);
	return;
};


void draw_MHD_control_bottuns(GtkWidget *vbox0){
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
	if(f_MHD_ctl == NULL){
		printf("malloc error for f_MHD_ctl\n");
		exit(0);
	};
	
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)window);
	g_object_set_data(G_OBJECT(entry), "MHD_ctl", (gpointer)f_MHD_ctl);
	
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
	
	gtk_box_pack_start(GTK_BOX(vbox0), hbox, FALSE, FALSE, 0);
	return;
}



int main(int argc, char** argv)
{
	gtk_init(&argc, &argv);
	
	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	
//	GtkWidget *scroll_window = gtk_scrolled_window_new(NULL, NULL);
//	gtk_box_pack_start(GTK_BOX(vbox_0), scroll_window, TRUE, TRUE, 0);
	
			ptem_test = init_chara_ctl_item_c();
			ptem_test->iflag = 111;
			ptem_test->c_tbl = "gggg";
	
	vbox_0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	draw_MHD_control_bottuns(vbox_0);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

