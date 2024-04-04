/*
//  c_ctl_data_MHD_model.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_MHD_model.h"

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

extern void * c_reftemp_point_ctl_block_name(void *f_refs_ctl);
extern void * c_reftemp_point_ctl_iflag(void *f_refs_ctl);
extern void * c_reftemp_point_value_ctl(void *f_refs_ctl);
extern void * c_reftemp_point_depth_ctl(void *f_refs_ctl);

extern void * c_takepiro_model_ctl_block_name(void *f_takepiro_ctl);
extern void * c_takepiro_model_ctl_iflag(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_sigma_ctl(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_width_ctl(void *f_takepiro_ctl);
extern void * c_takepiro_stratified_rout_ctl(void *f_takepiro_ctl);

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
extern void * c_MHD_sgs_ctl(void *f_model_ctl);


static struct f_MHD_time_evo_control * init_f_MHD_time_evo_control(void *(*c_load_self)(void *f_parent), 
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

static struct f_MHD_t_evo_area_control * init_f_MHD_t_evo_area_control(void *(*c_load_self)(void *f_parent), 
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



static struct f_MHD_gravity_control * init_f_MHD_gravity_control(void *(*c_load_self)(void *f_parent), 
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

static struct f_MHD_Coriolis_control * init_f_MHD_Coriolis_control(void *(*c_load_self)(void *f_parent), 
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

static struct f_MHD_magneto_cv_control * init_f_MHD_magneto_cv_control(void *(*c_load_self)(void *f_parent), 
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

static struct f_MHD_magnetic_scale_control * init_f_MHD_magnetic_scale_control(void *(*c_load_self)(void *f_parent), 
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
	
	f_bscale_ctl->f_mag_to_kin_energy_ctl = init_f_ctl_cr_array(c_MHD_B_scale_mag_to_kin_ctl,
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

static struct f_MHD_temp_model_control * init_f_MHD_temp_model_control(void *(*c_load_self)(void *f_parent), 
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
