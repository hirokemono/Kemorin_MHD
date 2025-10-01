/*
//  t_ctl_data_mhd_evo_scheme_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/28.
*/

#ifndef t_ctl_data_mhd_evo_scheme_c_h__
#define t_ctl_data_mhd_evo_scheme_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

struct mhd_restart_control_c{
    int iflag_use;
    int maxlen;
    
    struct chara_ctl_item *restart_flag_c;
};

struct mhd_evo_scheme_control_c{
    int iflag_use;
    int maxlen;
    
    struct chara_ctl_item *coef_imp_v_c;
    struct chara_ctl_item *coef_imp_t_c;
    struct chara_ctl_item *coef_imp_b_c;
    struct chara_ctl_item *coef_imp_c_c;
	
    struct chara_ctl_item *iflag_supg_c;
    struct chara_ctl_item *iflag_supg_v_c;
    struct chara_ctl_item *iflag_supg_t_c;
    struct chara_ctl_item *iflag_supg_b_c;
    struct chara_ctl_item *iflag_supg_c_c;
	
    struct int_ctl_item *num_multi_pass_c;
    struct int_ctl_item *maxiter_c;
	
	struct real_ctl_item *eps_4_velo_c;
	struct real_ctl_item *eps_4_magne_c;
	
	struct real_ctl_item *eps_crank_c;
	struct real_ctl_item *eps_B_crank_c;
	
	struct chara_ctl_item *scheme_c;
	struct chara_ctl_item *diffuse_correct_c;
	
	struct chara_ctl_item *method_4_CN_c;
	struct chara_ctl_item *precond_4_CN_c;
	
	struct chara_ctl_item *Legendre_trans_type_c;
	struct chara_ctl_item *FFT_library_c;
	struct chara_ctl_item *import_mode_c;
	struct chara_ctl_item *SR_routine_c;
	
    struct int_ctl_item *leg_vector_len_c;
};

/* prototype */
void get_label_mhd_restart_control(int index, char *label);
void get_label_mhd_evo_scheme_control(int index, char *label);

struct mhd_restart_control_c * init_mhd_restart_control_c();
void dealloc_mhd_restart_control_c(struct mhd_restart_control_c *mrst_ctl);
void read_mhd_restart_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mhd_restart_control_c *mrst_ctl);
int write_mhd_restart_control_c(FILE *fp, int level, const char *label, 
                                struct mhd_restart_control_c *mrst_ctl);

struct mhd_evo_scheme_control_c * init_mhd_evo_scheme_control_c();
void dealloc_mhd_evo_scheme_control_c(struct mhd_evo_scheme_control_c *mevo_ctl);
void read_mhd_evo_scheme_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mhd_evo_scheme_control_c *mevo_ctl);
int write_mhd_evo_scheme_control_c(FILE *fp, int level, const char *label, 
                                   struct mhd_evo_scheme_control_c *mevo_ctl);


#endif /* t_ctl_data_mhd_evo_scheme_c_h__ */
