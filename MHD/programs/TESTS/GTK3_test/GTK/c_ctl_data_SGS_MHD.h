/*
//  c_ctl_data_SGS_MHD.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_SGS_MHD_H_
#define C_CTL_DATA_SGS_MHD_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_int2_IO.h"
#include "t_control_real3_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_control_MHD_controls.h"
#include "c_ctl_VIZ_repartition.h"
#include "c_ctl_data_MHD_model.h"
#include "c_ctl_data_4_sph_monitor.h"
#include "c_ctl_data_PSF_ISOs.h"
#include "c_ctl_data_FLINE.h"
#include "c_ctl_data_MAP.h"
#include "c_ctl_data_LIC.h"
#include "c_control_data_pvrs.h"


struct f_MHD_viz_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	int f_num_psf_ctl;
	struct void_clist *f_psf_ctls;
	
	int f_num_iso_ctl;
	struct void_clist *f_iso_ctls;
	
	int f_num_map_ctl;
	struct void_clist *f_map_ctls;
	
	int f_num_pvr_ctl;
	struct void_clist *f_pvr_ctls;
	
	int f_num_lic_ctl;
	struct void_clist *f_lic_ctls;
	
	int f_num_fline_ctl;
	struct void_clist *f_fline_ctls;
	
	struct f_VIZ_repartition_ctl *f_repart_ctl;
};

struct f_MHD_crust_filter_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct int_ctl_item *f_crust_truncation_ctl;
};

struct f_MHD_zm_ctls{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_MHD_crust_filter_ctl *f_crust_filter_ctl;
	struct void_clist * f_zm_psf_ctls;
	struct void_clist * f_zRMS_psf_ctls;
	struct void_clist * f_zm_map_ctls;
	struct void_clist * f_zRMS_map_ctls;
};

struct f_MHD_node_monitor_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
	struct real3_clist * f_xx_4_monitor_ctl;
	struct int2_clist * f_node_4_monitor_ctl;
	struct chara_clist * f_group_4_monitor_ctl;
};


struct f_MHD_control{
	void * f_self;
	void * f_addition;
	int * f_iflag;
	
	char * c_block_name;
	
	struct f_platform_control *f_plt;
	struct f_platform_control * f_org_plt;
	struct f_platform_control * f_new_plt;
	struct f_MHD_sph_shell_control * f_psph_ctl;
	struct f_MHD_model_control *f_model_ctl;
	struct f_MHD_control_ctls * f_smctl_ctl;
	struct f_MHD_sph_monitor_ctls * f_smonitor_ctl;
	struct f_MHD_node_monitor_ctl * f_nmtr_ctl;
	
	struct f_MHD_viz_ctls * f_viz_ctls;
	struct f_MHD_zm_ctls * f_zm_ctls;
};


/* prototypes */
extern void c_view_control_sph_SGS_MHD();
extern void * c_read_control_sph_SGS_MHD(char *file_name);

extern void * c_add_sgs_sph_mhd_ctl();

struct f_MHD_viz_ctls * init_f_MHD_viz_ctls(void *(*c_load_self)(void *f_parent), void *f_parent);
void dealloc_f_MHD_viz_ctls(struct f_MHD_viz_ctls *f_viz_ctls);

struct f_MHD_crust_filter_ctl * init_f_MHD_crust_filter_ctl(void *(*c_load_self)(void *f_parent),
                                                            void *f_parent);
struct f_MHD_zm_ctls * init_f_MHD_zm_ctls(void *(*c_load_self)(void *f_parent), void *f_parent);
struct f_MHD_node_monitor_ctl * init_f_MHD_node_monitor_ctl(void *(*c_load_self)(void *f_parent),
                                                            void *f_parent);

void set_f_MHD_control(struct f_MHD_control *f_MHD_ctl);

#endif /* C_CTL_DATA_SGS_MHD_H_ */
