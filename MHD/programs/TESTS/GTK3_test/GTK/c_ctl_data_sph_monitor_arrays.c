/*
//  c_ctl_data_sph_monitor_arrays.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_sph_monitor_arrays.h"


extern void * c_sph_v_spectr_ctl_block_name(void *f_v_pwr_item);
extern void * c_sph_v_spectr_ctl_iflag(void *f_v_pwr_item);
extern void * c_sph_volume_spec_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_ave_file_ctl(void *f_v_pwr_item);
extern void * c_sph_volume_spec_format_ctl(void *f_v_pwr_item);
extern void * c_sph_degree_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_order_v_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_diff_v_lm_spectra_switch(void *f_v_pwr_item);
extern void * c_sph_axis_v_power_switch(void *f_v_pwr_item);
extern void * c_sph_v_spec_inner_radius_ctl(void *f_v_pwr_item);
extern void * c_sph_v_spec_outer_radius_ctl(void *f_v_pwr_item);

extern void * c_data_on_circle_block_name(void *f_circ_ctls);
extern void * c_data_on_circle_iflag(void *f_circ_ctls);
extern void * c_data_on_circle_field_file(void *f_circ_ctls);
extern void * c_data_on_circle_spectr_file(void *f_circ_ctls);
extern void * c_data_on_circle_file_fmt_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_coord_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_nphi_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_pick_s_ctl(void *f_circ_ctls);
extern void * c_data_on_circle_pick_z_ctl(void *f_circ_ctls);

struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(int idx, void *void_in, void *f_parent)
{
	struct f_sph_vol_spectr_ctls *f_v_pwr_item 
			= (struct f_sph_vol_spectr_ctls *) malloc(sizeof(struct f_sph_vol_spectr_ctls));
	if(f_v_pwr_item == NULL){
		printf("malloc error for f_v_pwr_item\n");
		exit(0);
	};
	f_v_pwr_item->f_self =  c_sph_monitor_vspec_ctl(idx, f_parent);
	
	f_v_pwr_item->f_iflag = (int *) c_sph_v_spectr_ctl_iflag(f_v_pwr_item->f_self);
	char *f_block_name =   (char *) c_sph_v_spectr_ctl_block_name(f_v_pwr_item->f_self);
	f_v_pwr_item->c_block_name = strngcopy_from_f(f_block_name);
	
	f_v_pwr_item->f_volume_spec_file_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_file_ctl, 
																  f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_ave_file_ctl =  init_f_ctl_chara_item(c_sph_volume_ave_file_ctl, 
																 f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_spec_format_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_format_ctl,
																	f_v_pwr_item->f_self);
	f_v_pwr_item->f_degree_v_spectra_switch =  init_f_ctl_chara_item(c_sph_degree_v_spectra_switch, 
																	 f_v_pwr_item->f_self);
	f_v_pwr_item->f_order_v_spectra_switch =  init_f_ctl_chara_item(c_sph_order_v_spectra_switch, 
																	f_v_pwr_item->f_self);
	f_v_pwr_item->f_diff_v_lm_spectra_switch =  init_f_ctl_chara_item(c_sph_diff_v_lm_spectra_switch, 
																	  f_v_pwr_item->f_self);
	f_v_pwr_item->f_axis_v_power_switch =  init_f_ctl_chara_item(c_sph_axis_v_power_switch, 
																 f_v_pwr_item->f_self);
	f_v_pwr_item->f_inner_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_inner_radius_ctl,
															 f_v_pwr_item->f_self);
	f_v_pwr_item->f_outer_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_outer_radius_ctl, 
															 f_v_pwr_item->f_self);
	return f_v_pwr_item;
}

struct f_sph_vol_spectr_ctls * dealloc_f_sph_vol_spectr_ctls(void *f_item){
	struct f_sph_vol_spectr_ctls *f_v_pwr_item = (struct f_sph_vol_spectr_ctls *) f_item;
	f_v_pwr_item->f_self = NULL;
	f_v_pwr_item->f_iflag = NULL;
	free(f_v_pwr_item->c_block_name);
	
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_volume_spec_file_ctl);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_volume_ave_file_ctl);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_volume_spec_format_ctl);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_degree_v_spectra_switch);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_order_v_spectra_switch);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_diff_v_lm_spectra_switch);
	dealloc_chara_ctl_item_c(f_v_pwr_item->f_axis_v_power_switch);
	dealloc_real_ctl_item_c(f_v_pwr_item->f_inner_radius_ctl);
	dealloc_real_ctl_item_c(f_v_pwr_item->f_outer_radius_ctl);
	free(f_v_pwr_item);
	return f_v_pwr_item;
}


struct f_sph_field_on_circle_ctls * init_f_sph_field_on_circle_ctls(int idx, void *void_in, void *f_parent)
{
	struct f_sph_field_on_circle_ctls *f_circ_ctls 
			= (struct f_sph_field_on_circle_ctls *) malloc(sizeof(struct f_sph_field_on_circle_ctls));
	if(f_circ_ctls == NULL){
		printf("malloc error for f_circ_ctls\n");
		exit(0);
	};
	f_circ_ctls->f_self =  c_data_on_circles_meq_ctl(idx, f_parent);
	
	f_circ_ctls->f_iflag = (int *) c_data_on_circle_iflag(f_circ_ctls->f_self);
	char *f_block_name =   (char *) c_data_on_circle_block_name(f_circ_ctls->f_self);
	f_circ_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
	f_circ_ctls->f_circle_field_file_ctl =  init_f_ctl_chara_item(c_data_on_circle_field_file,
																  f_circ_ctls->f_self);
	f_circ_ctls->f_circle_spectr_file_ctl =  init_f_ctl_chara_item(c_data_on_circle_spectr_file, 
																   f_circ_ctls->f_self);
	f_circ_ctls->f_circle_file_format_ctl =  init_f_ctl_chara_item(c_data_on_circle_file_fmt_ctl, 
																   f_circ_ctls->f_self);
	f_circ_ctls->f_pick_circle_coord_ctl =  init_f_ctl_chara_item(c_data_on_circle_coord_ctl, 
																  f_circ_ctls->f_self);
	
	f_circ_ctls->f_nphi_mid_eq_ctl = init_f_ctl_int_item(c_data_on_circle_nphi_ctl, 
														 f_circ_ctls->f_self);
	f_circ_ctls->f_pick_s_ctl =  init_f_ctl_real_item(c_data_on_circle_pick_s_ctl, 
													  f_circ_ctls->f_self);
	f_circ_ctls->f_pick_z_ctl =  init_f_ctl_real_item(c_data_on_circle_pick_z_ctl, 
													  f_circ_ctls->f_self);
	return f_circ_ctls;
}

struct f_sph_field_on_circle_ctls * dealloc_f_sph_field_on_circle_ctls(void *f_item){
	struct f_sph_field_on_circle_ctls *f_circ_ctls = (struct f_sph_field_on_circle_ctls *) f_item;
	f_circ_ctls->f_self = NULL;
	f_circ_ctls->f_iflag = NULL;
	free(f_circ_ctls->c_block_name);
	
	dealloc_chara_ctl_item_c(f_circ_ctls->f_circle_field_file_ctl);
	dealloc_chara_ctl_item_c(f_circ_ctls->f_circle_spectr_file_ctl);
	dealloc_chara_ctl_item_c(f_circ_ctls->f_circle_file_format_ctl);
	dealloc_chara_ctl_item_c(f_circ_ctls->f_pick_circle_coord_ctl);
	dealloc_int_ctl_item_c(f_circ_ctls->f_nphi_mid_eq_ctl);
	dealloc_real_ctl_item_c(f_circ_ctls->f_pick_s_ctl);
	dealloc_real_ctl_item_c(f_circ_ctls->f_pick_z_ctl);
	free(f_circ_ctls);
	return f_circ_ctls;
}
