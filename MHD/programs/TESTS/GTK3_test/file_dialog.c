
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include "control_elements_IO_c.h"
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

extern void * c_MHD_forces_block_name(void *f_frc_ctl);
extern void * c_MHD_forces_iflag(void *f_frc_ctl);
extern void * c_MHD_forces_array(void *f_frc_ctl);

extern void * c_MHD_eqs_block_name(void *f_eqs_ctl);
extern void * c_MHD_eqs_iflag(void *f_eqs_ctl);
extern void * c_MHD_eqs_mom_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_induct_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_heat_ctl(void *f_eqs_ctl);
extern void * c_MHD_eqs_comp_ctl(void *f_eqs_ctl);

extern void * c_MHD_momentum_eq_block_name(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_iflag(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_viscous(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_inertia(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_grad_p(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_t_buoyancy(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_c_buoyancy(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_coriolis(void *f_mom_eq_ctl);
extern void * c_MHD_momentum_eq_lorentz(void *f_mom_eq_ctl);

extern void * c_MHD_induction_block_name(void *f_induct_ctl);
extern void * c_MHD_induction_iflag(void *f_induct_ctl);
extern void * c_MHD_induction_evo(void *f_induct_ctl);
extern void * c_MHD_induction_diffuse(void *f_induct_ctl);
extern void * c_MHD_induction_potential(void *f_induct_ctl);
extern void * c_MHD_induction_uxb(void *f_induct_ctl);

extern void * c_MHD_heat_block_name(void *f_heat_ctl);
extern void * c_MHD_heat_iflag(void *f_heat_ctl);
extern void * c_MHD_heat_advect(void *f_heat_ctl);
extern void * c_MHD_heat_diffuse(void *f_heat_ctl);
extern void * c_MHD_heat_source(void *f_heat_ctl);


struct f_MHD_forces_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_ctl_chara_array * f_force_names;
	struct c_array_views * f_force_vws;
};

struct f_MHD_mom_eq_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_ctl_chara_array * f_coef_4_viscous;
	struct c_array_views * vws_coef_4_viscous;
	struct f_ctl_chara_array * f_coef_4_intertia;
	struct c_array_views * vws_coef_4_intertia;
	struct f_ctl_chara_array * f_coef_4_grad_p;
	struct c_array_views * vws_coef_4_grad_p;
	struct f_ctl_chara_array * f_coef_4_termal_buo;
	struct c_array_views * vws_coef_4_termal_buo;
	struct f_ctl_chara_array * f_coef_4_comp_buo;
	struct c_array_views * vws_coef_4_comp_buo;
	struct f_ctl_chara_array * f_coef_4_Coriolis;
	struct c_array_views * vws_coef_4_Coriolis;
	struct f_ctl_chara_array * f_coef_4_Lorentz;
	struct c_array_views * vws_coef_4_Lorentz;
};

struct f_MHD_induct_eq_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_ctl_chara_array * f_coef_4_magne_evo;
	struct c_array_views * vws_coef_4_magne_evo;
	struct f_ctl_chara_array * f_coef_4_mag_diffuse;
	struct c_array_views * vws_coef_4_mag_diffuse;
	struct f_ctl_chara_array * f_coef_4_mag_potential;
	struct c_array_views * vws_coef_4_mag_potential;
	struct f_ctl_chara_array * f_coef_4_induction;
	struct c_array_views * vws_coef_4_induction;
};

struct f_MHD_heat_eq_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_ctl_chara_array * f_coef_4_adv_flux;
	struct c_array_views * vws_coef_4_adv_flux;
	struct f_ctl_chara_array * f_coef_4_diffuse;
	struct c_array_views * vws_coef_4_diffuse;
	struct f_ctl_chara_array * f_coef_4_source;
	struct c_array_views * vws_coef_4_source;
};

struct f_MHD_equations_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_MHD_mom_eq_control *    f_mom_ctl;
	struct f_MHD_induct_eq_control * f_induct_ctl;
	struct f_MHD_heat_eq_control *   f_heat_ctl;
	struct f_MHD_heat_eq_control *   f_comp_ctl;
};

struct f_MHD_model_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
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
};


struct f_MHD_viz_ctls{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
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
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	void * f_crust_filter_ctl;
	void * f_zm_psf_ctls;
	void * f_zRMS_psf_ctls;
	void * f_zm_map_ctls;
	void * f_zRMS_map_ctls;
};

struct f_MHD_control{
	void * f_self;
	void * f_addition;
	
	char * f_block_name;
	int * f_iflag;
	
	int f_namelength[1];
	char * c_block_name;
	
	struct f_platform_control *f_plt;
	void * f_org_plt;
	void * f_new_plt;
	void * f_fname_psph;
	void * f_psph_ctl;
	struct f_MHD_model_control *f_model_ctl;
	void * f_smctl_ctl;
	void * f_smonitor_ctl;
	void * f_nmtr_ctl;
	
	void * f_sgs_ctl;
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

struct f_MHD_forces_control * init_f_MHD_forces_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent)
{
	struct f_MHD_forces_control *f_frc_ctl
			= (struct f_MHD_forces_control *) malloc(sizeof(struct f_MHD_forces_control));
	if(f_frc_ctl == NULL){
		printf("malloc error for f_frc_ctl\n");
		exit(0);
	};
	
	f_frc_ctl->f_self =  c_load_self(f_parent);
	
	f_frc_ctl->f_block_name =   (char *) c_MHD_forces_block_name(f_frc_ctl->f_self);
	f_frc_ctl->f_iflag =        (int *)  c_MHD_forces_iflag(f_frc_ctl->f_self);
	
	c_chara_item_clength(f_frc_ctl->f_block_name, f_frc_ctl->f_namelength);
	f_frc_ctl->c_block_name = alloc_string((long) f_frc_ctl->f_namelength[0]);
	strngcopy_w_length(f_frc_ctl->c_block_name, f_frc_ctl->f_namelength[0], 
					   f_frc_ctl->f_block_name);
	
	f_frc_ctl->f_force_names = init_f_ctl_chara_array(c_MHD_forces_array, f_frc_ctl->f_self);
	f_frc_ctl->f_force_vws =   init_c_array_views(f_frc_ctl->f_force_names);
	
	return f_frc_ctl;
};

struct f_MHD_mom_eq_control * init_f_MHD_mom_eq_ctl(void *(*c_load_self)(void *f_parent),
													void *f_parent)
{
	struct f_MHD_mom_eq_control *f_mom_eq_ctl
			= (struct f_MHD_mom_eq_control *) malloc(sizeof(struct f_MHD_mom_eq_control));
	if(f_mom_eq_ctl == NULL){
		printf("malloc error for f_mom_eq_ctl\n");
		exit(0);
	};
	
	f_mom_eq_ctl->f_self =  c_load_self(f_parent);
	
	f_mom_eq_ctl->f_block_name =   (char *) c_MHD_momentum_eq_block_name(f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->f_iflag =        (int *)  c_MHD_momentum_eq_iflag(f_mom_eq_ctl->f_self);
	
	c_chara_item_clength(f_mom_eq_ctl->f_block_name, f_mom_eq_ctl->f_namelength);
	f_mom_eq_ctl->c_block_name = alloc_string((long) f_mom_eq_ctl->f_namelength[0]);
	strngcopy_w_length(f_mom_eq_ctl->c_block_name, f_mom_eq_ctl->f_namelength[0], 
					   f_mom_eq_ctl->f_block_name);
	
	f_mom_eq_ctl->f_coef_4_viscous = init_f_ctl_chara_array(c_MHD_momentum_eq_viscous, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_viscous =   init_c_array_views(f_mom_eq_ctl->f_coef_4_viscous);
	f_mom_eq_ctl->f_coef_4_intertia = init_f_ctl_chara_array(c_MHD_momentum_eq_inertia, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_intertia =   init_c_array_views(f_mom_eq_ctl->f_coef_4_intertia);
	f_mom_eq_ctl->f_coef_4_grad_p = init_f_ctl_chara_array(c_MHD_momentum_eq_grad_p, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_grad_p =   init_c_array_views(f_mom_eq_ctl->f_coef_4_grad_p);
	f_mom_eq_ctl->f_coef_4_termal_buo = init_f_ctl_chara_array(c_MHD_momentum_eq_t_buoyancy, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_termal_buo =   init_c_array_views(f_mom_eq_ctl->f_coef_4_termal_buo);
	f_mom_eq_ctl->f_coef_4_comp_buo = init_f_ctl_chara_array(c_MHD_momentum_eq_c_buoyancy, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_comp_buo =   init_c_array_views(f_mom_eq_ctl->f_coef_4_comp_buo);
	f_mom_eq_ctl->f_coef_4_Coriolis = init_f_ctl_chara_array(c_MHD_momentum_eq_coriolis, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_Coriolis =   init_c_array_views(f_mom_eq_ctl->f_coef_4_Coriolis);
	f_mom_eq_ctl->f_coef_4_Lorentz = init_f_ctl_chara_array(c_MHD_momentum_eq_lorentz, f_mom_eq_ctl->f_self);
	f_mom_eq_ctl->vws_coef_4_Lorentz =   init_c_array_views(f_mom_eq_ctl->f_coef_4_Lorentz);
	
	return f_mom_eq_ctl;
};

struct f_MHD_induct_eq_control * init_f_MHD_induction_eq_ctl(void *(*c_load_self)(void *f_parent),
															 void *f_parent)
{
	struct f_MHD_induct_eq_control *f_induct_ctl
			= (struct f_MHD_induct_eq_control *) malloc(sizeof(struct f_MHD_induct_eq_control));
	if(f_induct_ctl == NULL){
		printf("malloc error for f_induct_ctl\n");
		exit(0);
	};
	
	f_induct_ctl->f_self =  c_load_self(f_parent);
	
	f_induct_ctl->f_block_name =   (char *) c_MHD_induction_block_name(f_induct_ctl->f_self);
	f_induct_ctl->f_iflag =        (int *)  c_MHD_induction_iflag(f_induct_ctl->f_self);
	
	c_chara_item_clength(f_induct_ctl->f_block_name, f_induct_ctl->f_namelength);
	f_induct_ctl->c_block_name = alloc_string((long) f_induct_ctl->f_namelength[0]);
	strngcopy_w_length(f_induct_ctl->c_block_name, f_induct_ctl->f_namelength[0], 
					   f_induct_ctl->f_block_name);
	
	f_induct_ctl->f_coef_4_magne_evo = init_f_ctl_chara_array(c_MHD_induction_evo, f_induct_ctl->f_self);
	f_induct_ctl->vws_coef_4_magne_evo =   init_c_array_views(f_induct_ctl->f_coef_4_magne_evo);
	f_induct_ctl->f_coef_4_mag_diffuse = init_f_ctl_chara_array(c_MHD_induction_diffuse, f_induct_ctl->f_self);
	f_induct_ctl->vws_coef_4_mag_diffuse =   init_c_array_views(f_induct_ctl->f_coef_4_mag_diffuse);
	f_induct_ctl->f_coef_4_mag_potential = init_f_ctl_chara_array(c_MHD_induction_potential, f_induct_ctl->f_self);
	f_induct_ctl->vws_coef_4_mag_potential =   init_c_array_views(f_induct_ctl->f_coef_4_mag_potential);
	f_induct_ctl->f_coef_4_induction = init_f_ctl_chara_array(c_MHD_induction_uxb, f_induct_ctl->f_self);
	f_induct_ctl->vws_coef_4_induction =   init_c_array_views(f_induct_ctl->f_coef_4_induction);
	
	return f_induct_ctl;
};

struct f_MHD_heat_eq_control * init_f_MHD_heat_eq_ctl(void *(*c_load_self)(void *f_parent),
															 void *f_parent)
{
	struct f_MHD_heat_eq_control *f_heat_ctl
			= (struct f_MHD_heat_eq_control *) malloc(sizeof(struct f_MHD_heat_eq_control));
	if(f_heat_ctl == NULL){
		printf("malloc error for f_heat_ctl\n");
		exit(0);
	};
	
	f_heat_ctl->f_self =  c_load_self(f_parent);
	
	f_heat_ctl->f_block_name =   (char *) c_MHD_heat_block_name(f_heat_ctl->f_self);
	f_heat_ctl->f_iflag =        (int *)  c_MHD_heat_iflag(f_heat_ctl->f_self);
	
	c_chara_item_clength(f_heat_ctl->f_block_name, f_heat_ctl->f_namelength);
	f_heat_ctl->c_block_name = alloc_string((long) f_heat_ctl->f_namelength[0]);
	strngcopy_w_length(f_heat_ctl->c_block_name, f_heat_ctl->f_namelength[0], 
					   f_heat_ctl->f_block_name);
	
	f_heat_ctl->f_coef_4_adv_flux = init_f_ctl_chara_array(c_MHD_heat_advect, f_heat_ctl->f_self);
	f_heat_ctl->vws_coef_4_adv_flux =   init_c_array_views(f_heat_ctl->f_coef_4_adv_flux);
	f_heat_ctl->f_coef_4_diffuse = init_f_ctl_chara_array(c_MHD_heat_diffuse, f_heat_ctl->f_self);
	f_heat_ctl->vws_coef_4_diffuse =   init_c_array_views(f_heat_ctl->f_coef_4_diffuse);
	f_heat_ctl->f_coef_4_source = init_f_ctl_chara_array(c_MHD_heat_source, f_heat_ctl->f_self);
	f_heat_ctl->vws_coef_4_source =   init_c_array_views(f_heat_ctl->f_coef_4_source);
	
	return f_heat_ctl;
};

struct f_MHD_equations_control * init_f_MHD_equations_ctl(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_equations_control *f_eqs_ctl 
			= (struct f_MHD_equations_control *) malloc(sizeof(struct f_MHD_equations_control));
	if(f_eqs_ctl == NULL){
		printf("malloc error for f_eqs_ctl\n");
		exit(0);
	};
	
	f_eqs_ctl->f_self =  c_load_self(f_parent);
	
	f_eqs_ctl->f_block_name =   (char *) c_MHD_eqs_block_name(f_eqs_ctl->f_self);
	f_eqs_ctl->f_iflag =        (int *) c_MHD_eqs_iflag(f_eqs_ctl->f_self);
	
	c_chara_item_clength(f_eqs_ctl->f_block_name, f_eqs_ctl->f_namelength);
	f_eqs_ctl->c_block_name = alloc_string((long) f_eqs_ctl->f_namelength[0]);
	strngcopy_w_length(f_eqs_ctl->c_block_name, f_eqs_ctl->f_namelength[0], 
					   f_eqs_ctl->f_block_name);
	
	f_eqs_ctl->f_mom_ctl =    init_f_MHD_mom_eq_ctl(c_MHD_eqs_mom_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_induct_ctl = init_f_MHD_induction_eq_ctl(c_MHD_eqs_induct_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_heat_ctl =   init_f_MHD_heat_eq_ctl(c_MHD_eqs_heat_ctl, f_eqs_ctl->f_self);
	f_eqs_ctl->f_comp_ctl =   init_f_MHD_heat_eq_ctl(c_MHD_eqs_comp_ctl, f_eqs_ctl->f_self);
	return f_eqs_ctl;
}


struct f_MHD_model_control * init_f_MHD_model_ctl(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_model_control *f_model_ctl 
			= (struct f_MHD_model_control *) malloc(sizeof(struct f_MHD_model_control));
	if(f_model_ctl == NULL){
		printf("malloc error for f_model_ctl\n");
		exit(0);
	};
	
	f_model_ctl->f_self =  c_load_self(f_parent);
	
	f_model_ctl->f_block_name =   (char *) c_MHD_mdl_block_name(f_model_ctl->f_self);
	f_model_ctl->f_iflag =        (int *) c_MHD_mdl_iflag(f_model_ctl->f_self);
	
	c_chara_item_clength(f_model_ctl->f_block_name, f_model_ctl->f_namelength);
	f_model_ctl->c_block_name = alloc_string((long) f_model_ctl->f_namelength[0]);
	strngcopy_w_length(f_model_ctl->c_block_name, f_model_ctl->f_namelength[0], 
					   f_model_ctl->f_block_name);
	
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
	return f_model_ctl;
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
	
	f_viz_ctls->f_block_name =   (char *) c_visualizations_block_name(f_viz_ctls->f_self);
	f_viz_ctls->f_iflag =        (int *) c_visualizations_iflag(f_viz_ctls->f_self);
	c_chara_item_clength(f_viz_ctls->f_block_name, f_viz_ctls->f_namelength);
	f_viz_ctls->c_block_name = alloc_string((long) f_viz_ctls->f_namelength[0]);
	strngcopy_w_length(f_viz_ctls->c_block_name, f_viz_ctls->f_namelength[0], 
					   f_viz_ctls->f_block_name);
	
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
	
	f_zm_ctls->f_block_name =   (char *) c_dynamo_vizs_block_name(f_zm_ctls->f_self);
	f_zm_ctls->f_iflag =        (int *) c_dynamo_vizs_iflag(f_zm_ctls->f_self);
	
	c_chara_item_clength(f_zm_ctls->f_block_name, f_zm_ctls->f_namelength);
	f_zm_ctls->c_block_name = alloc_string((long) f_zm_ctls->f_namelength[0]);
	strngcopy_w_length(f_zm_ctls->c_block_name, f_zm_ctls->f_namelength[0], 
					   f_zm_ctls->f_block_name);
	
	f_zm_ctls->f_crust_filter_ctl =    c_dynamo_vizs_crust_filter_ctl(f_zm_ctls->f_self);
	f_zm_ctls->f_zm_psf_ctls =    c_dynamo_vizs_zm_psf_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zRMS_psf_ctls =    c_dynamo_vizs_zRMS_psf_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zm_map_ctls =    c_dynamo_vizs_zm_map_ctls(f_zm_ctls->f_self);
	f_zm_ctls->f_zRMS_map_ctls =    c_dynamo_vizs_zRMS_map_ctls(f_zm_ctls->f_self);
	return f_zm_ctls;
}


static void set_f_MHD_control(struct f_MHD_control *f_MHD_ctl)
{
		f_MHD_ctl->f_block_name =   (char *) c_MHD_block_name(f_MHD_ctl->f_self);
		f_MHD_ctl->f_iflag =        (int *) c_MHD_iflag(f_MHD_ctl->f_self);
		
		c_chara_item_clength(f_MHD_ctl->f_block_name, f_MHD_ctl->f_namelength);
		f_MHD_ctl->c_block_name = alloc_string((long) f_MHD_ctl->f_namelength[0]);
		strngcopy_w_length(f_MHD_ctl->c_block_name, f_MHD_ctl->f_namelength[0], 
						   f_MHD_ctl->f_block_name);
		
		f_MHD_ctl->f_plt = init_f_platform_control(c_MHD_plt, f_MHD_ctl->f_self);
		
		f_MHD_ctl->f_org_plt =      c_MHD_org_plt(f_MHD_ctl->f_self);
		f_MHD_ctl->f_new_plt =      c_MHD_new_plt(f_MHD_ctl->f_self);
		f_MHD_ctl->f_fname_psph =   c_MHD_fname_psph(f_MHD_ctl->f_self);
		f_MHD_ctl->f_psph_ctl =     c_MHD_psph_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_model_ctl =    init_f_MHD_model_ctl(c_MHD_model_ctl, f_MHD_ctl->f_self);
		f_MHD_ctl->f_smctl_ctl =    c_MHD_smctl_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_smonitor_ctl = c_MHD_smonitor_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_nmtr_ctl =     c_MHD_nmtr_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_sgs_ctl =      c_MHD_sgs_ctl(f_MHD_ctl->f_addition);
		f_MHD_ctl->f_viz_ctls =     init_f_MHD_viz_ctls(c_MHD_viz_ctls, f_MHD_ctl->f_addition);
		f_MHD_ctl->f_zm_ctls =      init_f_MHD_zm_ctls(c_MHD_zm_ctls, f_MHD_ctl->f_addition);
	printf("f_zm_ctls %s %d\n", f_MHD_ctl->f_zm_ctls->c_block_name, f_MHD_ctl->f_zm_ctls->f_iflag);
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
		if ((folder == NULL)) {
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
		int iflag = read_iso_ctl_file_c(read_file_name, buf, iso_GTK0->iso_c);
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

static void cb_edited_egrp(GtkCellRendererText *cell, gchar *path_str, 
                           gchar *new_text, gpointer user_data){
    printf("path_str %s\n", path_str);
    printf("new_text %s\n", new_text);
}

static void cb_add(GtkButton *button, gpointer user_data)
{
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *iso_area_list
			= (struct chara_clist *) g_object_get_data(G_OBJECT(user_data), "chara_list");
	add_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), iso_area_list);
}
static void cb_delete(GtkButton *button, gpointer user_data)
{
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *iso_area_list
			= (struct chara_clist *) g_object_get_data(G_OBJECT(user_data), "chara_list");
	delete_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), iso_area_list);
}

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
	
	printf("iso_area_list %d\n", count_chara_clist(iso_def_c->iso_area_list));
	int index = 0;
	GtkWidget *c_tree_view = gtk_tree_view_new();
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();

    create_text_tree_view(c_tree_view, renderer_text, iso_def_c->iso_area_list);
    index = append_c_list_from_ctl(index, &iso_def_c->iso_area_list->c_item_head, c_tree_view);
    g_signal_connect(G_OBJECT(renderer_text), "edited", 
                     G_CALLBACK(cb_edited_egrp), (gpointer) iso_def_c);
    
    
	printf("index %d\n", index);
	GtkWidget *button_A = gtk_button_new_with_label("Add");
	GtkWidget *button_D = gtk_button_new_with_label("Delete");
	g_object_set_data(G_OBJECT(c_tree_view), "chara_list",
					  (gpointer) iso_def_c->iso_area_list);
	g_signal_connect(G_OBJECT(button_A), "clicked", G_CALLBACK(cb_add),
					 (gpointer) c_tree_view);
	g_signal_connect(G_OBJECT(button_D), "clicked", G_CALLBACK(cb_delete),
					 (gpointer) c_tree_view);
	add_chara_list_box_w_addbottun(c_tree_view, 
								   button_A, button_D, vbox_1);
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
	
	add_control_combobox_vbox(iso_fld_c->output_type_ctl, iso_fld_c->output_type_ctl,
							  iso_fld_c->flag_iso_color, 
							  color_flags_tree_view, vbox_1);
	printf("%le\n", iso_fld_c->output_value_ctl->r_data);
	c_label = duplicate_underscore(iso_fld_c->label_fld_on_iso_ctl->label[ 2]);
	vbox_2[0] = make_real_hbox(1, c_label, iso_fld_c->output_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_field_selection_box(iso_GTK0->color_fields_vws, window, vbox_1);
	
	return vbox_1;
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
	
	GtkWidget * vbox_plt = draw_platform_control_vbox(f_MHD_ctl->f_plt, window);
	GtkWidget *vbox_test = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *expand_MHD_force = draw_control_block(f_MHD_ctl->f_model_ctl->f_frc_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_frc_ctl->f_iflag,
													 560, 240, window, vbox_test);
	
	
	GtkWidget *expand_MHD_dimless = add_dimless_selection_box(f_MHD_ctl->f_model_ctl->f_dless_ctl, window);
	
	
	GtkWidget *expand_MHD_mom = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_mom_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_mom_ctl->f_iflag,
													 560, 400, window, vbox_test);
	GtkWidget *expand_MHD_magne = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_induct_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_induct_ctl->f_iflag,
													 560, 400, window, vbox_test);
	GtkWidget *expand_MHD_heat = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_heat_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_heat_ctl->f_iflag,
													 560, 400, window, vbox_test);
	GtkWidget *expand_MHD_light = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_comp_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_comp_ctl->f_iflag,
													 560, 400, window, vbox_test);
	
	GtkWidget *vbox_eqs = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_eqs), expand_MHD_mom, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_eqs), expand_MHD_magne, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_eqs), expand_MHD_heat, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_eqs), expand_MHD_light, FALSE, FALSE, 0);
	
	GtkWidget *expand_MHD_eqs = draw_control_block(f_MHD_ctl->f_model_ctl->f_eqs_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_eqs_ctl->f_iflag,
													 560, 400, window, vbox_eqs);
	
	GtkWidget *vbox_m = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_force, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_dimless, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_m), expand_MHD_eqs, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_model = draw_control_block(f_MHD_ctl->f_model_ctl->c_block_name, 
													 f_MHD_ctl->f_model_ctl->f_iflag,
													 560, 500, window, vbox_m);
	
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_model, FALSE, FALSE, 0);
	
	/*
	GtkWidget *expand_MHD_org_plt = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_new_plt = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_fname_psph = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_psph_ctl = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_smctl_ctl = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_smonitor_ctl = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_nmtr_ctl = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	GtkWidget *expand_MHD_sgs_ctl = draw_control_block(f_MHD_ctl->c_block_name, 
											   f_MHD_ctl->f_iflag,
											   560, 200, window, vbox_test);
	 */

	GtkWidget *expand_MHD_viz_ctls = draw_control_block(f_MHD_ctl->f_viz_ctls->c_block_name, 
											   f_MHD_ctl->f_viz_ctls->f_iflag,
											   560, 200, window, vbox_test);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_viz_ctls, FALSE, FALSE, 0);
	GtkWidget *expand_MHD_zm_ctls = draw_control_block(f_MHD_ctl->f_zm_ctls->c_block_name, 
											   f_MHD_ctl->f_zm_ctls->f_iflag,
											   560, 200, window, vbox_test);
	gtk_box_pack_start(GTK_BOX(vbox_plt), expand_MHD_zm_ctls, FALSE, FALSE, 0);
	
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
