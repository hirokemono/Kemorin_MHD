/*
//  c_ctl_data_MHD_BCs.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_MHD_BCs.h"

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
