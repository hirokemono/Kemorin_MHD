/*
//  t_ctl_data_4_sphere_model_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_4_sphere_model_c_h_
#define t_ctl_data_4_sphere_model_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_int2_IO.h"
#include "t_control_int_real_IO.h"
#include "t_control_chara_int_IO.h"

struct sphere_domain_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *inner_decomp_c;
	struct int_ctl_item *num_radial_domain_c;
	struct int_ctl_item *num_horiz_domain_c;
	
	struct chara_int_clist *ndomain_sph_grid_list;
	struct chara_int_clist *ndomain_legendre_list;
	struct chara_int_clist *ndomain_spectr_list;
};

struct sphere_data_ctl_c{
	int maxlen;
	
	struct int_ctl_item *ltr_c;
	struct int_ctl_item *phi_symmetry_c;
	
	struct chara_ctl_item *sph_grid_type_c;
	struct chara_ctl_item *sph_coef_type_c;
	
	struct int_ctl_item *ngrid_elevation_c;
	struct int_ctl_item *ngrid_azimuth_c;
	
	struct int_real_ctl_list radius_list;
	struct chara_int_clist *radial_grp_list;
	
	struct chara_ctl_item *radial_grid_type_c;
	struct int_ctl_item *num_fluid_grid_c;
	
	struct real_ctl_item *Min_radius_c;
	struct real_ctl_item *ICB_radius_c;
	struct real_ctl_item *CMB_radius_c;
	struct real_ctl_item *Max_radius_c;
	struct real_ctl_item *fluid_core_size_c;
	struct real_ctl_item *ICB_to_CMB_ratio_c;
	
	struct int_ctl_item *num_radial_layer_c;
	struct int_ctl_item *num_med_layer_c;
	
	struct int2_ctl_list radial_layer_list;
	struct int2_ctl_list med_layer_list;
};


/* prototypes */
void get_label_sphere_domain_ctl(int index, char *label);
void get_label_sphere_data_ctl(int index, char *label);

void alloc_sphere_domain_ctl_c(struct sphere_domain_ctl_c *sdctl_c);
void dealloc_sphere_domain_ctl_c(struct sphere_domain_ctl_c *sdctl_c);
int read_sphere_domain_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_domain_ctl_c *sdctl_c);
int write_sphere_domain_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_domain_ctl_c *sdctl_c);

void alloc_sphere_data_ctl_c(struct sphere_data_ctl_c *spctl_c);
void dealloc_sphere_data_ctl_c(struct sphere_data_ctl_c *spctl_c);
int read_sphere_data_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct sphere_data_ctl_c *spctl_c);
int write_sphere_data_ctl_c(FILE *fp, int level,
			const char *label, struct sphere_data_ctl_c *spctl_c);


#endif /* t_ctl_data_4_sphere_model_c_h_ */
