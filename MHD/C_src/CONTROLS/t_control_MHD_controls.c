/*
//  t_control_MHD_controls.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_control_MHD_controls.h"

extern void * c_time_steps_ctl_block_name(void *f_tctl);
extern void * c_time_steps_ctl_iflag(void *f_tctl);
extern void * c_time_steps_i_step_init_ctl(void *f_tctl);
extern void * c_time_steps_i_step_number(void *f_tctl);
extern void * c_time_steps_elapsed_time_ctl(void *f_tctl);
extern void * c_time_steps_i_step_check_ctl(void *f_tctl);
extern void * c_time_steps_i_step_rst_ctl(void *f_tctl);

extern void * c_time_steps_i_step_pvr_ctl(void *f_tctl);
extern void * c_time_steps_i_step_psf_ctl(void *f_tctl);
extern void * c_time_steps_i_step_map_ctl(void *f_tctl);
extern void * c_time_steps_i_step_iso_ctl(void *f_tctl);
extern void * c_time_steps_i_step_lic_ctl(void *f_tctl);
extern void * c_time_steps_i_step_fline_ctl(void *f_tctl);

extern void * c_time_steps_i_step_ucd_ctl(void *f_tctl);
extern void * c_time_steps_i_step_monitor(void *f_tctl);
extern void * c_time_steps_i_step_sgs_coef(void *f_tctl);
extern void * c_time_steps_i_step_boundary(void *f_tctl);

extern void * c_time_steps_dt_ctl(void *f_tctl);
extern void * c_time_steps_time_init_ctl(void *f_tctl);

extern void * c_time_steps_i_diff_steps_ctl(void *f_tctl);
extern void * c_time_steps_flexible_step(void *f_tctl);

extern void * c_time_steps_ratio_to_cfl(void *f_tctl);
extern void * c_time_steps_start_rst_step(void *f_tctl);
extern void * c_time_steps_end_rst_step(void *f_tctl);
extern void * c_time_steps_min_delta_t_ctl(void *f_tctl);
extern void * c_time_steps_max_delta_t_ctl(void *f_tctl);
extern void * c_time_steps_max_eps_shrink(void *f_tctl);
extern void * c_time_steps_min_eps_expand(void *f_tctl);
extern void * c_time_steps_delta_t_check(void *f_tctl);

extern void * c_time_steps_delta_t_rst_ctl(void *f_tctl);
extern void * c_time_steps_delta_t_psf_ctl(void *f_tctl);
extern void * c_time_steps_delta_t_iso_ctl(void *f_tctl);
extern void * c_time_steps_delta_t_map_ctl(void *f_tctl);
extern void * c_time_steps_delta_t_pvr_ctl(void *f_tctl);
extern void * c_time_steps_delta_t_fline(void *f_tctl);
extern void * c_time_steps_delta_t_lic_ctl(void *f_tctl);

extern void * c_time_steps_delta_t_field(void *f_tctl);
extern void * c_time_steps_delta_t_monitor(void *f_tctl);
extern void * c_time_steps_dt_sgs_coef_ctl(void *f_tctl);
extern void * c_time_steps_dt_boundary_ctl(void *f_tctl);

extern void * c_MHD_restart_ctl_block_name(void *f_tctl);
extern void * c_MHD_restart_ctl_iflag(void *f_tctl);
extern void * c_MHD_restart_flag_ctl(void *f_tctl);


extern void * c_mhd_evo_scheme_ctl_block_name(void *f_mevo_ctl);
extern void * c_mhd_evo_scheme_ctl_iflag(void *f_mevo_ctl);
extern void * c_evo_scheme_coef_implicit_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_coef_imp_v_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_coef_imp_t_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_coef_imp_b_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_coef_imp_c_ctl(void *f_mevo_ctl);

extern void * c_evo_scheme_iflag_supg_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_iflag_supg_v_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_iflag_supg_t_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_iflag_supg_b_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_iflag_supg_c_ctl(void *f_mevo_ctl);

extern void * c_evo_scheme_num_multi_pass(void *f_mevo_ctl);
extern void * c_evo_scheme_maxiter_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_eps_4_velo_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_eps_4_magne_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_eps_crank_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_eps_B_crank_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_scheme_ctl(void *f_mevo_ctl);
extern void * c_evo_scheme_diffuse_correct(void *f_mevo_ctl);
extern void * c_evo_scheme_method_4_CN(void *f_mevo_ctl);
extern void * c_evo_scheme_precond_4_CN(void *f_mevo_ctl);

extern void * c_evo_scheme_Leg_trans_type(void *f_mevo_ctl);
extern void * c_evo_scheme_FFT_library(void *f_mevo_ctl);
extern void * c_evo_scheme_import_mode(void *f_mevo_ctl);
extern void * c_evo_scheme_leg_vector_len(void *f_mevo_ctl);

extern void * c_smctl_ctl_block_name(void *f_smctl_ctl);
extern void * c_smctl_ctl_iflag(void *f_smctl_ctl);
extern void * c_smctl_ctl_tctl(void *f_smctl_ctl);
extern void * c_smctl_mrst_ctl(void *f_smctl_ctl);
extern void * c_smctl_mevo_ctl(void *f_smctl_ctl);



static struct f_time_step_control_ctls * init_f_time_step_control_ctls(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_time_step_control_ctls *f_tctl 
			= (struct f_time_step_control_ctls *) malloc(sizeof(struct f_time_step_control_ctls));
	if(f_tctl == NULL){
		printf("malloc error for f_tctl\n");
		exit(0);
	};
	
	f_tctl->f_self =  c_load_self(f_parent);
	
	f_tctl->f_iflag =        (int *) c_time_steps_ctl_iflag(f_tctl->f_self);
	char *f_block_name =   (char *)  c_time_steps_ctl_block_name(f_tctl->f_self);
	f_tctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_tctl->f_i_step_init_ctl =   init_f_ctl_int_item(c_time_steps_i_step_init_ctl, f_tctl->f_self);
	f_tctl->f_i_step_number_ctl = init_f_ctl_int_item(c_time_steps_i_step_number, f_tctl->f_self);
	f_tctl->f_elapsed_time_ctl =  init_f_ctl_real_item(c_time_steps_elapsed_time_ctl, f_tctl->f_self);
	
	f_tctl->f_i_step_check_ctl = init_f_ctl_int_item(c_time_steps_i_step_check_ctl, f_tctl->f_self);
	f_tctl->f_i_step_rst_ctl =   init_f_ctl_int_item(c_time_steps_i_step_rst_ctl, f_tctl->f_self);
	
	f_tctl->f_i_step_pvr_ctl =   init_f_ctl_int_item(c_time_steps_i_step_pvr_ctl, f_tctl->f_self);
	f_tctl->f_i_step_psf_ctl =   init_f_ctl_int_item(c_time_steps_i_step_psf_ctl, f_tctl->f_self);
	f_tctl->f_i_step_map_ctl =   init_f_ctl_int_item(c_time_steps_i_step_map_ctl, f_tctl->f_self);
	f_tctl->f_i_step_iso_ctl =   init_f_ctl_int_item(c_time_steps_i_step_iso_ctl, f_tctl->f_self);
	f_tctl->f_i_step_lic_ctl =   init_f_ctl_int_item(c_time_steps_i_step_lic_ctl, f_tctl->f_self);
	f_tctl->f_i_step_fline_ctl = init_f_ctl_int_item(c_time_steps_i_step_fline_ctl, f_tctl->f_self);
	
	f_tctl->f_i_step_ucd_ctl =       init_f_ctl_int_item(c_time_steps_i_step_ucd_ctl, f_tctl->f_self);
	f_tctl->f_i_step_monitor_ctl =   init_f_ctl_int_item(c_time_steps_i_step_monitor, f_tctl->f_self);
	f_tctl->f_i_step_sgs_coefs_ctl = init_f_ctl_int_item(c_time_steps_i_step_sgs_coef, f_tctl->f_self);
	f_tctl->f_i_step_boundary_ctl =  init_f_ctl_int_item(c_time_steps_i_step_boundary, f_tctl->f_self);
	
	f_tctl->f_dt_ctl =        init_f_ctl_real_item(c_time_steps_dt_ctl, f_tctl->f_self);
	f_tctl->f_time_init_ctl = init_f_ctl_real_item(c_time_steps_time_init_ctl, f_tctl->f_self);
	
	f_tctl->f_i_diff_steps_ctl =  init_f_ctl_int_item(c_time_steps_i_diff_steps_ctl, f_tctl->f_self);
	f_tctl->f_flexible_step_ctl = init_f_ctl_chara_item(c_time_steps_flexible_step, f_tctl->f_self);
	
	f_tctl->f_ratio_to_cfl_ctl =      init_f_ctl_real_item(c_time_steps_ratio_to_cfl, f_tctl->f_self);
	f_tctl->f_start_rst_step_ctl =    init_f_ctl_int_item(c_time_steps_start_rst_step, f_tctl->f_self);
	f_tctl->f_end_rst_step_ctl =      init_f_ctl_int_item(c_time_steps_end_rst_step, f_tctl->f_self);
	f_tctl->f_min_delta_t_ctl =       init_f_ctl_real_item(c_time_steps_min_delta_t_ctl, f_tctl->f_self);
	f_tctl->f_max_delta_t_ctl =       init_f_ctl_real_item(c_time_steps_max_delta_t_ctl, f_tctl->f_self);
	f_tctl->f_max_eps_to_shrink_ctl = init_f_ctl_real_item(c_time_steps_max_eps_shrink, f_tctl->f_self);
	f_tctl->f_min_eps_to_expand_ctl = init_f_ctl_real_item(c_time_steps_min_eps_expand, f_tctl->f_self);
	
	f_tctl->f_delta_t_check_ctl = init_f_ctl_real_item(c_time_steps_delta_t_check, f_tctl->f_self);
	f_tctl->f_delta_t_rst_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_rst_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_psf_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_psf_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_iso_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_iso_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_map_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_map_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_pvr_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_pvr_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_lic_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_lic_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_fline_ctl = init_f_ctl_real_item(c_time_steps_delta_t_fline, f_tctl->f_self);
	
	f_tctl->f_delta_t_field_ctl =     init_f_ctl_real_item(c_time_steps_delta_t_field, f_tctl->f_self);
	f_tctl->f_delta_t_monitor_ctl =   init_f_ctl_real_item(c_time_steps_delta_t_monitor, f_tctl->f_self);
	f_tctl->f_delta_t_sgs_coefs_ctl = init_f_ctl_real_item(c_time_steps_dt_sgs_coef_ctl, f_tctl->f_self);
	f_tctl->f_delta_t_boundary_ctl =  init_f_ctl_real_item(c_time_steps_dt_boundary_ctl, f_tctl->f_self);
	return f_tctl;
};


static struct f_MHD_restart_controls * init_f_MHD_restart_controls(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_restart_controls *f_mrst_ctl 
			= (struct f_MHD_restart_controls *) malloc(sizeof(struct f_MHD_restart_controls));
	if(f_mrst_ctl == NULL){
		printf("malloc error for f_mrst_ctl\n");
		exit(0);
	};
	
	f_mrst_ctl->f_self =  c_load_self(f_parent);
	
	f_mrst_ctl->f_iflag =        (int *) c_MHD_restart_ctl_iflag(f_mrst_ctl->f_self);
	char *f_block_name =   (char *)  c_MHD_restart_ctl_block_name(f_mrst_ctl->f_self);
	f_mrst_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_mrst_ctl->f_restart_flag_ctl =   init_f_ctl_chara_item(c_MHD_restart_flag_ctl, f_mrst_ctl->f_self);
	return f_mrst_ctl;
};

static struct f_MHD_evo_scheme_controls * init_f_MHD_evo_scheme_controls(void *(*c_load_self)(void *f_parent), 
															  void *f_parent)
{
	struct f_MHD_evo_scheme_controls *f_mevo_ctl 
			= (struct f_MHD_evo_scheme_controls *) malloc(sizeof(struct f_MHD_evo_scheme_controls));
	if(f_mevo_ctl == NULL){
		printf("malloc error for f_mevo_ctl\n");
		exit(0);
	};
	
	f_mevo_ctl->f_self =  c_load_self(f_parent);
	
	f_mevo_ctl->f_iflag =        (int *) c_mhd_evo_scheme_ctl_iflag(f_mevo_ctl->f_self);
	char *f_block_name =   (char *)  c_mhd_evo_scheme_ctl_block_name(f_mevo_ctl->f_self);
	f_mevo_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_mevo_ctl->f_scheme_ctl =         init_f_ctl_chara_item(c_evo_scheme_scheme_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_coef_implicit_ctl =  init_f_ctl_real_item(c_evo_scheme_coef_implicit_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_coef_imp_v_ctl =     init_f_ctl_real_item(c_evo_scheme_coef_imp_v_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_coef_imp_t_ctl =     init_f_ctl_real_item(c_evo_scheme_coef_imp_t_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_coef_imp_b_ctl =     init_f_ctl_real_item(c_evo_scheme_coef_imp_b_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_coef_imp_c_ctl =     init_f_ctl_real_item(c_evo_scheme_coef_imp_c_ctl, f_mevo_ctl->f_self);
	
	f_mevo_ctl->f_Legendre_trans_type = init_f_ctl_chara_item(c_evo_scheme_Leg_trans_type, f_mevo_ctl->f_self);
	f_mevo_ctl->f_FFT_library =         init_f_ctl_chara_item(c_evo_scheme_FFT_library, f_mevo_ctl->f_self);
	f_mevo_ctl->f_import_mode =         init_f_ctl_chara_item(c_evo_scheme_import_mode, f_mevo_ctl->f_self);
	f_mevo_ctl->f_leg_vector_len =      init_f_ctl_int_item(c_evo_scheme_leg_vector_len, f_mevo_ctl->f_self);
	
	f_mevo_ctl->f_iflag_supg_ctl =     init_f_ctl_chara_item(c_evo_scheme_iflag_supg_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_iflag_supg_v_ctl =   init_f_ctl_chara_item(c_evo_scheme_iflag_supg_v_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_iflag_supg_t_ctl =   init_f_ctl_chara_item(c_evo_scheme_iflag_supg_t_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_iflag_supg_b_ctl =   init_f_ctl_chara_item(c_evo_scheme_iflag_supg_b_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_iflag_supg_c_ctl =   init_f_ctl_chara_item(c_evo_scheme_iflag_supg_c_ctl, f_mevo_ctl->f_self);
	
	f_mevo_ctl->f_method_4_CN =        init_f_ctl_chara_item(c_evo_scheme_method_4_CN, f_mevo_ctl->f_self);
	f_mevo_ctl->f_precond_4_CN =       init_f_ctl_chara_item(c_evo_scheme_precond_4_CN, f_mevo_ctl->f_self);
	f_mevo_ctl->f_num_multi_pass_ctl = init_f_ctl_int_item(c_evo_scheme_num_multi_pass, f_mevo_ctl->f_self);
	f_mevo_ctl->f_maxiter_ctl =        init_f_ctl_int_item(c_evo_scheme_maxiter_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_eps_4_velo_ctl =     init_f_ctl_real_item(c_evo_scheme_eps_4_velo_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_eps_4_magne_ctl =    init_f_ctl_real_item(c_evo_scheme_eps_4_magne_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_eps_crank_ctl =      init_f_ctl_real_item(c_evo_scheme_eps_crank_ctl, f_mevo_ctl->f_self);
	f_mevo_ctl->f_eps_B_crank_ctl =    init_f_ctl_real_item(c_evo_scheme_eps_B_crank_ctl, f_mevo_ctl->f_self);
	
	f_mevo_ctl->f_diffuse_correct =    init_f_ctl_chara_item(c_evo_scheme_diffuse_correct, f_mevo_ctl->f_self);
	return f_mevo_ctl;
};



struct f_MHD_control_ctls * init_f_MHD_control_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_control_ctls *f_smctl_ctl 
			= (struct f_MHD_control_ctls *) malloc(sizeof(struct f_MHD_control_ctls));
	if(f_smctl_ctl == NULL){
		printf("malloc error for f_smctl_ctl\n");
		exit(0);
	};
	
	f_smctl_ctl->f_self =  c_load_self(f_parent);
	printf("f_self %p\n", f_smctl_ctl->f_self);
	
	f_smctl_ctl->f_iflag =        (int *) c_smctl_ctl_iflag(f_smctl_ctl->f_self);
	char *f_block_name =   (char *) c_smctl_ctl_block_name(f_smctl_ctl->f_self);
	f_smctl_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_smctl_ctl->f_tctl =     init_f_time_step_control_ctls(c_smctl_ctl_tctl, f_smctl_ctl->f_self);
	f_smctl_ctl->f_mrst_ctl = init_f_MHD_restart_controls(c_smctl_mrst_ctl, f_smctl_ctl->f_self);
	f_smctl_ctl->f_mevo_ctl = init_f_MHD_evo_scheme_controls(c_smctl_mevo_ctl, f_smctl_ctl->f_self);
	return f_smctl_ctl;
}
