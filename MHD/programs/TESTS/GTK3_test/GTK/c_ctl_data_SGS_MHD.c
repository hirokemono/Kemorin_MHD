/*
//  c_ctl_data_SGS_MHD.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_SGS_MHD.h"


extern void * c_MHD_block_name(void *f_MHD_ctl);
extern void * c_MHD_iflag(void *f_MHD_ctl);
extern void * c_MHD_plt(void *f_MHD_ctl);
extern void * c_MHD_org_plt(void *f_MHD_ctl);
extern void * c_MHD_new_plt(void *f_MHD_ctl);
extern void * c_MHD_fname_psph(void *f_MHD_ctl);
extern void * c_MHD_psph_ctl(void *f_MHD_ctl);
extern void * c_MHD_model_ctl(void *f_MHD_ctl);
extern void * c_MHD_smctl_ctl(void *f_MHD_ctl);
extern void * c_MHD_smonitor_ctl(void *f_MHD_ctl);
extern void * c_MHD_nmtr_ctl(void *f_MHD_ctl);
extern void * c_MHD_viz_ctls(void *f_MHD_ctl);
extern void * c_MHD_zm_ctls(void *f_MHD_ctl);

extern void * c_visualizations_block_name(void *f_viz_ctls);
extern void * c_visualizations_iflag(void *f_viz_ctls);
extern void * c_visualizations_psf_ctls(void *f_viz_ctls);
extern void * c_visualizations_iso_ctls(void *f_viz_ctls);
extern void * c_visualizations_map_ctls(void *f_viz_ctls);
extern void * c_visualizations_pvr_ctls(void *f_viz_ctls);
extern void * c_visualizations_fline_ctls(void *f_viz_ctls);
extern void * c_visualizations_lic_ctls(void *f_viz_ctls);
extern void * c_visualizations_repart_ctl(void *f_viz_ctls);
extern void * c_visualizations_fname_vrepart(void *f_viz_ctls);


extern void * c_node_monitor_ctl_block_name(void *f_nmtr_ctl);
extern void * c_node_monitor_ctl_iflag(void *f_nmtr_ctl);
extern void * c_node_monitor_xx_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_node_ctl(void *f_nmtr_ctl);
extern void * c_node_monitor_group_ctl(void *f_nmtr_ctl);

extern void * c_dynamo_vizs_block_name(void *f_zm_ctls);
extern void * c_dynamo_vizs_iflag(void *f_zm_ctls);
extern void * c_dynamo_vizs_crust_filter_ctl(void *f_zm_ctls);
extern void * c_dynamo_vizs_zm_psf_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zRMS_psf_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zm_map_ctls(void *f_zm_ctls);
extern void * c_dynamo_vizs_zRMS_map_ctls(void *f_zm_ctls);

extern void * c_clust_filter_ctl_block_name(void *f_crust_filter_ctl);
extern void * c_clust_filter_ctl_iflag(void *f_crust_filter_ctl);
extern void * c_clust_filter_ltr_ctl(void *f_crust_filter_ctl);



struct f_MHD_viz_ctls * init_f_MHD_viz_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_viz_ctls *f_viz_ctls 
			= (struct f_MHD_viz_ctls *) malloc(sizeof(struct f_MHD_viz_ctls));
	if(f_viz_ctls == NULL){
		printf("malloc error for f_viz_ctls\n");
		exit(0);
	};
	
	f_viz_ctls->f_self =  c_load_self(f_parent);
	printf("f_viz_ctls->f_self %p\n", f_viz_ctls->f_self);
	
	f_viz_ctls->f_iflag =        (int *) c_visualizations_iflag(f_viz_ctls->f_self);
	char *f_block_name =   (char *) c_visualizations_block_name(f_viz_ctls->f_self);
	f_viz_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
    f_viz_ctls->f_psf_ctls =   init_f_VIZ_psf_ctls(c_visualizations_psf_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_psf_ctl);
    f_viz_ctls->f_iso_ctls =   init_f_VIZ_iso_ctls(c_visualizations_iso_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_iso_ctl);
    f_viz_ctls->f_map_ctls =   init_f_VIZ_map_ctls(c_visualizations_map_ctls(f_viz_ctls->f_self), 
                                                   &f_viz_ctls->f_num_map_ctl);
    f_viz_ctls->f_pvr_ctls =   init_f_VIZ_pvr_ctls(c_visualizations_pvr_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_pvr_ctl);
    f_viz_ctls->f_lic_ctls =   init_f_VIZ_lic_ctls(c_visualizations_lic_ctls(f_viz_ctls->f_self),
                                                   &f_viz_ctls->f_num_lic_ctl);
    f_viz_ctls->f_fline_ctls = init_f_VIZ_fline_ctls(c_visualizations_fline_ctls(f_viz_ctls->f_self),
                                                     &f_viz_ctls->f_num_fline_ctl);
    
	f_block_name = (char *) c_visualizations_fname_vrepart(f_viz_ctls->f_self);
    f_viz_ctls->f_repart_ctl = init_f_VIZ_repartition_ctl(strngcopy_from_f(f_block_name),
                                                          c_visualizations_repart_ctl,
                                                          f_viz_ctls->f_self);
	return f_viz_ctls;
}

void dealloc_f_MHD_viz_ctls(struct f_MHD_viz_ctls *f_viz_ctls){
    return;
}

struct f_MHD_crust_filter_ctl * init_f_MHD_crust_filter_ctl(void *(*c_load_self)(void *f_parent),
                                                            void *f_parent)
{
	struct f_MHD_crust_filter_ctl *f_crust_filter_ctl 
			= (struct f_MHD_crust_filter_ctl *) malloc(sizeof(struct f_MHD_crust_filter_ctl));
	if(f_crust_filter_ctl == NULL){
		printf("malloc error for f_MHD_crust_filter_ctl\n");
		exit(0);
	};
	
	f_crust_filter_ctl->f_self =  c_load_self(f_parent);
	
	f_crust_filter_ctl->f_iflag =   (int *) c_clust_filter_ctl_iflag(f_crust_filter_ctl->f_self);
	char *f_block_name =   (char *) c_clust_filter_ctl_block_name(f_crust_filter_ctl->f_self);
	f_crust_filter_ctl->c_block_name = strngcopy_from_f(f_block_name);
    
    f_crust_filter_ctl->f_crust_truncation_ctl = init_f_ctl_int_item(c_clust_filter_ltr_ctl,
                                                                     f_crust_filter_ctl->f_self);
    return f_crust_filter_ctl;
};


struct f_MHD_zm_ctls * init_f_MHD_zm_ctls(void *(*c_load_self)(void *f_parent), void *f_parent)
{
	struct f_MHD_zm_ctls *f_zm_ctls 
			= (struct f_MHD_zm_ctls *) malloc(sizeof(struct f_MHD_zm_ctls));
	if(f_zm_ctls == NULL){
		printf("malloc error for f_zm_ctls\n");
		exit(0);
	};
	
	f_zm_ctls->f_self =  c_load_self(f_parent);
	
	f_zm_ctls->f_iflag =        (int *) c_dynamo_vizs_iflag(f_zm_ctls->f_self);
	char *f_block_name =   (char *) c_dynamo_vizs_block_name(f_zm_ctls->f_self);
	f_zm_ctls->c_block_name = strngcopy_from_f(f_block_name);
	
    f_zm_ctls->f_crust_filter_ctl = init_f_MHD_crust_filter_ctl(c_dynamo_vizs_crust_filter_ctl,
                                                                f_zm_ctls->f_self);
    
    int n_single = 1;
    f_zm_ctls->f_zm_psf_ctls =   init_f_VIZ_psf_ctls(c_dynamo_vizs_zm_psf_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    f_zm_ctls->f_zRMS_psf_ctls = init_f_VIZ_psf_ctls(c_dynamo_vizs_zRMS_psf_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    
    f_zm_ctls->f_zm_map_ctls =   init_f_VIZ_map_ctls(c_dynamo_vizs_zm_map_ctls(f_zm_ctls->f_self),
                                                     &n_single);
    f_zm_ctls->f_zRMS_map_ctls = init_f_VIZ_map_ctls(c_dynamo_vizs_zRMS_map_ctls(f_zm_ctls->f_self),
                                                     &n_single);
	return f_zm_ctls;
}

struct f_MHD_node_monitor_ctl * init_f_MHD_node_monitor_ctl(void *(*c_load_self)(void *f_parent),
															void *f_parent)
{
	struct f_MHD_node_monitor_ctl *f_nmtr_ctl 
			= (struct f_MHD_node_monitor_ctl *) malloc(sizeof(struct f_MHD_node_monitor_ctl));
	if(f_nmtr_ctl == NULL){
		printf("malloc error for f_nmtr_ctl\n");
		exit(0);
	};
	
	f_nmtr_ctl->f_self =  c_load_self(f_parent);
	
	f_nmtr_ctl->f_iflag =        (int *) c_node_monitor_ctl_iflag(f_nmtr_ctl->f_self);
	char *f_block_name =   (char *) c_node_monitor_ctl_block_name(f_nmtr_ctl->f_self);
	f_nmtr_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
    f_nmtr_ctl->f_xx_4_monitor_ctl =    init_f_ctl_r3_array(c_node_monitor_xx_ctl,
                                                            f_nmtr_ctl->f_self);
    f_nmtr_ctl->f_node_4_monitor_ctl =  init_f_ctl_i2_array(c_node_monitor_node_ctl,
                                                            f_nmtr_ctl->f_self);
    f_nmtr_ctl->f_group_4_monitor_ctl = init_f_ctl_chara_array(c_node_monitor_group_ctl,
                                                               f_nmtr_ctl->f_self);
	return f_nmtr_ctl;
}

void set_f_MHD_control(struct f_MHD_control *f_MHD_ctl)
{
	f_MHD_ctl->f_iflag =        (int *) c_MHD_iflag(f_MHD_ctl->f_self);
	char *f_block_name =   (char *) c_MHD_block_name(f_MHD_ctl->f_self);
	f_MHD_ctl->c_block_name = strngcopy_from_f(f_block_name);
	
	f_MHD_ctl->f_plt =          init_f_platform_control(c_MHD_plt, f_MHD_ctl->f_self);
	f_MHD_ctl->f_org_plt =      init_f_platform_control(c_MHD_org_plt, f_MHD_ctl->f_self);
	f_MHD_ctl->f_new_plt =      init_f_platform_control(c_MHD_new_plt, f_MHD_ctl->f_self);
	f_block_name =              (char *) c_MHD_fname_psph(f_MHD_ctl->f_self);
	f_MHD_ctl->f_psph_ctl =     init_f_MHD_sph_shell_ctl(strngcopy_from_f(f_block_name),
                                                         c_MHD_psph_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_model_ctl =    init_f_MHD_model_ctl(c_MHD_model_ctl, f_MHD_ctl->f_self,
													 f_MHD_ctl->f_addition);
	f_MHD_ctl->f_smctl_ctl =    init_f_MHD_control_ctls(c_MHD_smctl_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_smonitor_ctl = init_f_MHD_sph_monitor_ctls(c_MHD_smonitor_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_nmtr_ctl =     init_f_MHD_node_monitor_ctl(c_MHD_nmtr_ctl, f_MHD_ctl->f_self);
	f_MHD_ctl->f_viz_ctls =     init_f_MHD_viz_ctls(c_MHD_viz_ctls, f_MHD_ctl->f_addition);
	f_MHD_ctl->f_zm_ctls =      init_f_MHD_zm_ctls(c_MHD_zm_ctls, f_MHD_ctl->f_addition);
	return;
}

