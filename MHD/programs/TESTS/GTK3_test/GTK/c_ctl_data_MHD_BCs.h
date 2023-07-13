/*
//  c_ctl_data_MHD_BCs.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_MHD_BCS_H_
#define C_CTL_DATA_MHD_BCS_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_array_single_items_c.h"


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


/* prototypes */

extern void *set_label_sph_thermal_bc_f(void *f_dummy);
extern void *set_label_sph_momentum_bc_f(void *f_dummy);
extern void *set_label_sph_induction_bc_f(void *f_dummy);

extern void *set_label_thermal_bc_f(void *f_dummy);
extern void *set_label_momentum_bc_f(void *f_dummy);
extern void *set_label_induction_bc_f(void *f_dummy);

extern void *set_label_potential_bc_f(void *f_dummy);
extern void *set_label_vector_p_bc_f(void *f_dummy);
extern void *set_label_current_bc_f(void *f_dummy);

extern void *surf_bc_label_thermal_bc_f();
extern void *surf_bc_label_momentum_bc_f();
extern void *surf_bc_label_induction_bc_f();
extern void *surf_bc_label_infinity_bc_f();

extern void *surf_bc_label_potential_bc_f();
extern void *surf_bc_label_vector_p_bc_f();
extern void *surf_bc_label_current_bc_f();

struct f_MHD_node_bc_control * init_f_MHD_node_bc_control(void *(*c_load_self)(void *f_parent), 
                                                          void *f_parent);
struct f_MHD_surf_bc_control * init_f_MHD_surf_bc_control(void *(*c_load_self)(void *f_parent), 
                                                          void *f_parent);

#endif /* C_CTL_DATA_MHD_BCS_H_ */
