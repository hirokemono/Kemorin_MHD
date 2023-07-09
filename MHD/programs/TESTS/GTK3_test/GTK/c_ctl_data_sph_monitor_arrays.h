//
//  c_ctl_data_sph_monitor_arrays.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
//

#ifndef C_CTL_DATA_SPH_MONITOR_ARRAYS_H_
#define C_CTL_DATA_SPH_MONITOR_ARRAYS_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_ctl_array_single_items_c.h"


struct f_sph_vol_spectr_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_volume_spec_file_ctl;
	struct chara_ctl_item *f_volume_ave_file_ctl;
	struct chara_ctl_item *f_volume_spec_format_ctl;
	struct chara_ctl_item *f_degree_v_spectra_switch;
	struct chara_ctl_item *f_order_v_spectra_switch;
	struct chara_ctl_item *f_diff_v_lm_spectra_switch;
	struct chara_ctl_item *f_axis_v_power_switch;
	struct f_ctl_real_item *f_inner_radius_ctl;
	struct f_ctl_real_item *f_outer_radius_ctl;
};

struct f_sph_field_on_circle_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct chara_ctl_item *f_circle_field_file_ctl;
	struct chara_ctl_item *f_circle_spectr_file_ctl;
	struct chara_ctl_item *f_circle_file_format_ctl;
	struct chara_ctl_item *f_pick_circle_coord_ctl;\
	
	struct f_ctl_int_item   *f_nphi_mid_eq_ctl;
	struct f_ctl_real_item  *f_pick_s_ctl;
	struct f_ctl_real_item  *f_pick_z_ctl;
};

/* prototypes */

extern int    c_sph_monitor_num_vspec_ctl(void *f_smonitor_ctl);
extern void * c_append_sph_mntr_vspec_ctl(int idx, char *block_name,
                                          void *f_smonitor_ctl);
extern void * c_delete_sph_mntr_vspec_ctl(int idx, void *f_smonitor_ctl);
extern void * c_sph_monitor_vspec_ctl(int idx, void *f_smonitor_ctl);

extern int    c_data_on_circles_num(void *f_circ_ctls);
extern void * c_data_on_circles_meq_ctl(int idx, void *f_circ_ctls);
extern void * c_append_circles_meq_ctl(int idx, char *c_name, void *f_circ_ctls);
extern void * c_delete_circles_meq_ctl(int idx, void *f_circ_ctls);


struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(int idx, void *f_parent);
void * dealloc_f_sph_vol_spectr_ctls(void *f_item);

struct f_sph_field_on_circle_ctls * init_f_sph_field_on_circle_ctls(int idx, void *f_parent);
void * dealloc_f_sph_field_on_circle_ctls(void *f_item);

#endif /* C_CTL_DATA_SPH_MONITOR_ARRAYS_H_ */
