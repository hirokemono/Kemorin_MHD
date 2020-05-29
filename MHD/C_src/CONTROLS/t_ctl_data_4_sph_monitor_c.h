/*
//  t_ctl_data_4_sph_monitor_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/19.
*/

#ifndef t_ctl_data_4_sph_monitor_c_h__
#define t_ctl_data_4_sph_monitor_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_int2_IO.h"
#include "t_ctl_data_4_volume_spectr_list.h"

struct pick_spectr_control_c{
    int iflag_use;
    int maxlen;
    
    struct chara_ctl_item *picked_mode_head_c;
	
    struct int_clist  *idx_pick_layer_list;
    struct int2_clist *idx_pick_sph_list;
    struct int_clist  *idx_pick_sph_l_list;
    struct int_clist  *idx_pick_sph_m_list;
};

struct layerd_spectr_control_c{
    int iflag_use;
    int maxlen;
    
	struct chara_ctl_item *layered_pwr_spectr_prefix_c;
	
    struct chara_ctl_item *degree_spectr_switch_c;
    struct chara_ctl_item *order_spectr_switch_c;
    struct chara_ctl_item *diff_lm_spectr_switch_c;
	struct chara_ctl_item *axis_spectr_switch_c;
	
    struct int_clist *idx_spec_layer_list;
};

struct gauss_spectr_control_c{
    int iflag_use;
    int maxlen;
    
	struct chara_ctl_item *gauss_coefs_prefix_c;
    struct real_ctl_item *gauss_coefs_radius_c;
	
    struct int2_clist *idx_gauss_list;
    struct int_clist  *idx_gauss_l_list;
    struct int_clist  *idx_gauss_m_list;
};

struct mid_equator_control_c{
    int iflag_use;
    int maxlen;
    
    struct chara_ctl_item *pick_circle_coord_c;
    struct int_ctl_item *nphi_mid_eq_c;
	
	struct real_ctl_item *pick_s_c;
    struct real_ctl_item *pick_z_c;
};

struct sph_monitor_control_c{
    int iflag_use;
    int maxlen;
	
	
    struct chara_ctl_item *volume_average_prefix_c;
    struct chara_ctl_item *volume_pwr_spectr_prefix_c;
    struct chara_ctl_item *Nusselt_file_prefix_c;
	
	struct pick_spectr_control_c *pspec_ctl_c;
	
	struct gauss_spectr_control_c *g_pwr;
	struct layerd_spectr_control_c *lp_ctl;
	struct mid_equator_control_c *meq_ctl;
	
	struct volume_spectr_ctl_list v_pwr_list;
};

/* prototypes */
void get_label_sph_monitor_ctl(int index, char *label);
void get_label_pick_spectr_ctl(int index, char *label);
void get_label_gauss_spectr_ctl(int index, char *label);
void get_label_layerd_spectr_ctl(int index, char *label);
void get_label_mid_equator_ctl(int index, char *label);

struct pick_spectr_control_c * init_pick_spectr_control_c();
void dealloc_pick_spectr_control_c(struct pick_spectr_control_c *pspec_ctl_c);
void read_pick_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pick_spectr_control_c *pspec_ctl_c);
int write_pick_spectr_control_c(FILE *fp, int level, const char *label, 
                                struct pick_spectr_control_c *pspec_ctl_c);

struct gauss_spectr_control_c * init_gauss_spectr_control_c();
void dealloc_gauss_spectr_control_c(struct gauss_spectr_control_c *g_pwr);
void read_gauss_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct gauss_spectr_control_c *g_pwr);
int write_gauss_spectr_control_c(FILE *fp, int level,
			const char *label, struct gauss_spectr_control_c *g_pwr);

struct layerd_spectr_control_c * init_layerd_spectr_control_c();
void dealloc_layerd_spectr_control_c(struct layerd_spectr_control_c *lp_ctl);
void read_layerd_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct layerd_spectr_control_c *lp_ctl);
int write_layerd_spectr_control_c(FILE *fp, int level,
			const char *label, struct layerd_spectr_control_c *lp_ctl);

struct mid_equator_control_c * init_mid_equator_control_c();
void dealloc_mid_equator_control_c(struct mid_equator_control_c *meq_ctl);
void read_mid_equator_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mid_equator_control_c *meq_ctl);
int write_mid_equator_control_c(FILE *fp, int level,
			const char *label, struct mid_equator_control_c *meq_ctl);


struct sph_monitor_control_c * init_sph_monitor_ctl_c();
void dealloc_sph_monitor_ctl_c(struct sph_monitor_control_c *monitor_ctl);
void read_sph_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct sph_monitor_control_c *monitor_ctl);
int write_sph_monitor_ctl_c(FILE *fp, int level, const char *label,
			struct sph_monitor_control_c *monitor_ctl);


#endif /* t_ctl_data_4_sph_monitor_c_h */
