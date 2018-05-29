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
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"


/* prototype */

struct pick_spectr_control_c{
    int maxlen;
    
    struct chara_ctl_item *picked_mode_head_c;
	
    struct int_ctl_array *idx_pick_layer_c;
    struct i2_ctl_array *idx_pick_sph_c;
    struct int_ctl_array *idx_pick_sph_l_c;
    struct int_ctl_array *idx_pick_sph_m_c;
};

struct volume_spectr_control_c{
    int maxlen;
    
    struct chara_ctl_item *volume_spec_file_c;
    struct chara_ctl_item *volume_ave_file_c;
	
    struct real_ctl_item *inner_radius_c;
    struct real_ctl_item *outer_radius_c;
};

struct layerd_spectr_control_c{
    int maxlen;
    
	struct chara_ctl_item *layered_pwr_spectr_prefix_c;
	
    struct chara_ctl_item *degree_spectr_switch_c;
    struct chara_ctl_item *order_spectr_switch_c;
    struct chara_ctl_item *diff_lm_spectr_switch_c;
	struct chara_ctl_item *axis_spectr_switch_c;
	
    struct int_ctl_array *idx_spec_layer_c;
};

struct gauss_spectr_control_c{
    int maxlen;
    
	struct chara_ctl_item *gauss_coefs_prefix_c;
    struct real_ctl_item *gauss_coefs_radius_c;
	
    struct i2_ctl_array  *idx_gauss_c;
    struct int_ctl_array *idx_gauss_l_c;
    struct int_ctl_array *idx_gauss_m_c;
};

struct mid_equator_control_c{
    int maxlen;
    
    struct chara_ctl_item *pick_circle_coord_c;
    struct int_ctl_item *nphi_mid_eq_c;
	
	struct real_ctl_item *pick_s_c;
    struct real_ctl_item *pick_z_c;
};

struct sph_monitor_control_c{
    int maxlen;
	
	
    struct chara_ctl_item *volume_average_prefix_c;
    struct chara_ctl_item *volume_pwr_spectr_prefix_c;
    struct chara_ctl_item *Nusselt_file_prefix_c;
	
	int iflag_pspec_ctl;
	struct pick_spectr_control_c *pspec_ctl_c;
	
	int num_vspec_c;
	struct volume_spectr_control_c **v_pwr_c;
	
	int iflag_g_pwr;
	struct gauss_spectr_control_c *g_pwr;
	int iflag_lp_ctl;
	struct layerd_spectr_control_c *lp_ctl;
	int iflag_meq_ctl;
	struct mid_equator_control_c *meq_ctl;
};

/* prototypes */

void alloc_pick_spectr_control_c(struct pick_spectr_control_c *pspec_ctl_c);
void dealloc_pick_spectr_control_c(struct pick_spectr_control_c *pspec_ctl_c);
int read_pick_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pick_spectr_control_c *pspec_ctl_c);
int write_pick_spectr_control_c(FILE *fp, int level, int iflag,
			const char *label, struct pick_spectr_control_c *pspec_ctl_c);

void alloc_gauss_spectr_control_c(struct gauss_spectr_control_c *g_pwr);
void dealloc_gauss_spectr_control_c(struct gauss_spectr_control_c *g_pwr);
int read_gauss_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct gauss_spectr_control_c *g_pwr);
int write_gauss_spectr_control_c(FILE *fp, int level, int iflag,
			const char *label, struct gauss_spectr_control_c *g_pwr);

void alloc_layerd_spectr_control_c(struct layerd_spectr_control_c *lp_ctl);
void dealloc_layerd_spectr_control_c(struct layerd_spectr_control_c *lp_ctl);
int read_layerd_spectr_control_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct layerd_spectr_control_c *lp_ctl);
int write_layerd_spectr_control_c(FILE *fp, int level, int iflag,
			const char *label, struct layerd_spectr_control_c *lp_ctl);

void alloc_volume_spectr_control_c(struct sph_monitor_control_c *monitor_ctl);
void dealloc_volume_spectr_control_c(struct sph_monitor_control_c *monitor_ctl);
int read_volume_spectr_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct sph_monitor_control_c *monitor_ctl);
int write_volume_spectr_control_c(FILE *fp, int level, const char *label, 
			struct sph_monitor_control_c *monitor_ctl);

void alloc_mid_equator_control_c(struct mid_equator_control_c *meq_ctl);
void dealloc_mid_equator_control_c(struct mid_equator_control_c *meq_ctl);
int read_mid_equator_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mid_equator_control_c *meq_ctl);
int write_mid_equator_control_c(FILE *fp, int level, int iflag,
			const char *label, struct mid_equator_control_c *meq_ctl);


void alloc_sph_monitor_ctl_c(struct sph_monitor_control_c *monitor_ctl);
void dealloc_sph_monitor_ctl_c(struct sph_monitor_control_c *monitor_ctl);
int read_sph_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF],
			struct sph_monitor_control_c *monitor_ctl);
int write_sph_monitor_ctl_c(FILE *fp, int level, int *iflag,
			struct sph_monitor_control_c *monitor_ctl);


#endif /* t_ctl_data_4_sph_monitor_c_h */
