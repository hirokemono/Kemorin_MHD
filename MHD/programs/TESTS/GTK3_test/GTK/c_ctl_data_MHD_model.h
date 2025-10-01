/*
//  c_ctl_data_MHD_model.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_MHD_MODEL_H_
#define C_CTL_DATA_MHD_MODEL_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_data_4_fields_c.h"
#include "c_ctl_data_MHD_BCs.h"
#include "c_ctl_data_platforms.h"
#include "c_ctl_data_SGS_model.h"


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
	
	struct chara_real_clist *f_mag_to_kin_energy_ctl;
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


struct f_MHD_t_evo_area_control{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_clist *f_evo_fluid_group_ctl;
	struct chara_clist *f_evo_conduct_group_ctl;
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


/* prototypes */

struct f_MHD_model_control * init_f_MHD_model_ctl(void *(*c_load_self)(void *f_parent), 
                                                  void *f_parent, void *f_addition);

#endif /* C_CTL_DATA_MHD_MODEL_H_ */
