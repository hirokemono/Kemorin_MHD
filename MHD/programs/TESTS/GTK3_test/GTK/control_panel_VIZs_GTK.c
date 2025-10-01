/*
//  control_panel_VIZs_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_VIZs_GTK.h"

extern void * set_file_fmt_items_f(void *fmt_names_c);

struct VIZs_widgets * init_MHD_VIZs_GTK(struct f_MHD_viz_ctls *f_viz_ctls, 
                                        struct f_MHD_fields_control *f_fld_ctl){
	struct VIZs_widgets *vizs_Wgts = (struct VIZs_widgets *) malloc(sizeof(struct VIZs_widgets));
	if(vizs_Wgts == NULL){
		printf("malloc error for vizs_Wgts\n");
		exit(0);
	};
    
    int i;
    for(i=0;i<count_void_clist(f_viz_ctls->f_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_viz_ctls->f_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_viz_ctls->f_iso_ctls);i++){
        struct f_VIZ_ISO_ctl *f_iso_ctl
                = (struct f_VIZ_ISO_ctl *) void_clist_at_index(i, f_viz_ctls->f_iso_ctls);
        f_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_viz_ctls->f_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_viz_ctls->f_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_viz_ctls->f_pvr_ctls);i++){
        struct f_VIZ_PVR_ctl *f_pvr_ctl
                = (struct f_VIZ_PVR_ctl *) void_clist_at_index(i, f_viz_ctls->f_pvr_ctls);
        f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl, 
                                                              f_fld_ctl->f_field_ctl);
    };
    
    
    for(i=0;i<count_void_clist(f_viz_ctls->f_lic_ctls);i++){
        struct f_VIZ_LIC_PVR_ctl *f_lic_ctl
                = (struct f_VIZ_LIC_PVR_ctl *) void_clist_at_index(i, f_viz_ctls->f_lic_ctls);
        struct LIC_GTK_widgets *lic_vws = init_LIC_GTK_widgets(f_lic_ctl->f_lic_pvr_ctl, 
                                                               f_fld_ctl->f_field_ctl);
        f_lic_ctl->f_lic_pvr_ctl->void_panel = (void *) lic_vws->lic_pvr_vws;
        f_lic_ctl->void_panel = (void *) lic_vws;
    };
    for(i=0;i<count_void_clist(f_viz_ctls->f_fline_ctls);i++){
        struct f_VIZ_FLINE_ctl *f_fline_ctl
                = (struct f_VIZ_FLINE_ctl *) void_clist_at_index(i, f_viz_ctls->f_fline_ctls);
        f_fline_ctl->void_panel = (void *) init_FLINE_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    
    vizs_Wgts->f_repart_vws
			= (struct VIZ_repartition_widgets *) malloc(sizeof(struct VIZ_repartition_widgets));
	if(vizs_Wgts->f_repart_vws == NULL){
		printf("malloc error for VIZ_repartition_widgets\n");
		exit(0);
    };
    vizs_Wgts->f_repart_vws->label_field_list = f_fld_ctl->f_field_ctl;
    vizs_Wgts->label_file_format_list = init_f_ctl_chara_array(set_file_fmt_items_f,
                                                               f_viz_ctls->f_self);
    vizs_Wgts->f_repart_vws->label_file_format_list = init_f_ctl_chara_array(set_file_fmt_items_f,
                                                               f_viz_ctls->f_self);
    return vizs_Wgts;
};

struct dynamo_VIZs_widgets *init_dynamo_VIZs_GTK(struct f_MHD_zm_ctls *f_zm_ctls, 
                                                 struct f_MHD_fields_control *f_fld_ctl){
    struct dynamo_VIZs_widgets *dviz_Wgts
            = (struct dynamo_VIZs_widgets *) malloc(sizeof(struct dynamo_VIZs_widgets));
	if(dviz_Wgts == NULL){
		printf("malloc error for dviz_Wgts\n");
		exit(0);
	};
    
    int i;
    for(i=0;i<count_void_clist(f_zm_ctls->f_zm_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_zm_ctls->f_zm_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_zm_ctls->f_zRMS_psf_ctls);i++){
        struct f_VIZ_PSF_ctl *f_psf_ctl
                = (struct f_VIZ_PSF_ctl *) void_clist_at_index(i, f_zm_ctls->f_zRMS_psf_ctls);
        f_psf_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_zm_ctls->f_zm_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_zm_ctls->f_zm_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    for(i=0;i<count_void_clist(f_zm_ctls->f_zRMS_map_ctls);i++){
        struct f_VIZ_MAP_ctl *f_map_ctl
                = (struct f_VIZ_MAP_ctl *) void_clist_at_index(i, f_zm_ctls->f_zRMS_map_ctls);
        f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                              f_fld_ctl->f_field_ctl);
        f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    };
    
    return dviz_Wgts;
}


GtkWidget *MHD_VIZs_ctl_expander(GtkWidget *window, struct f_MHD_viz_ctls *f_viz_ctls, 
                                 struct f_MHD_fields_control *f_fld_ctl,
                                 struct VIZs_widgets *vizs_Wgts){
	GtkWidget *expand_MHD_psf = draw_array_block_ctl_vbox(f_viz_ctls->f_psf_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          vizs_Wgts->vpsf_Wgts, window);
	
	GtkWidget *expand_MHD_iso = draw_array_block_ctl_vbox(f_viz_ctls->f_iso_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_isosurf_ctls,
                                                          c_delete_viz_isosurf_ctls,
                                                          (void *) init_f_VIZ_ISO_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_ISO_ctl_GTK,
                                                          (void *) draw_viz_each_iso_ctl_vbox,
                                                          vizs_Wgts->viso_Wgts, window);
	
	GtkWidget *expand_MHD_map = draw_array_block_ctl_vbox(f_viz_ctls->f_map_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          vizs_Wgts->vmap_Wgts, window);
	
	GtkWidget *expand_MHD_pvr = draw_array_block_ctl_vbox(f_viz_ctls->f_pvr_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_pvr_render_ctls,
                                                          c_delete_viz_pvr_render_ctls,
                                                          (void *) init_f_VIZ_PVR_ctl_GTK,
                                                          dealloc_f_VIZ_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_pvr_ctl_vbox,
                                                          vizs_Wgts->vpvr_Wgts, window);
	
	GtkWidget *expand_MHD_lic = draw_array_block_ctl_vbox(f_viz_ctls->f_lic_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_lic_render_ctls,
                                                          c_delete_viz_lic_render_ctls,
                                                          (void *) init_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_LIC_PVR_ctl_GTK,
                                                          (void *) draw_viz_each_lic_ctl_vbox,
                                                          vizs_Wgts->vlic_Wgts, window);
	
	GtkWidget *expand_MHD_fline = draw_array_block_ctl_vbox(f_viz_ctls->f_fline_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_fline_ctls,
                                                          c_delete_viz_fline_ctls,
                                                          (void *) init_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_FLINE_ctl_GTK,
                                                          (void *) draw_viz_each_fline_ctl_vbox,
                                                          vizs_Wgts->vfline_Wgts, window);
	
    GtkWidget *expander_rep =  draw_VIZ_repartition_ctl_vbox(f_viz_ctls->f_repart_ctl, 
                                                             vizs_Wgts->f_repart_vws, window);
	
	GtkWidget *vbox_viz = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expander_rep, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_psf, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_iso, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_map, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_pvr, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_lic, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_viz), expand_MHD_fline, FALSE, FALSE, 0);
	
	GtkWidget *expand_MHD_viz = draw_control_block(f_viz_ctls->c_block_name, 
                                                   f_viz_ctls->f_iflag,
                                                   window, vbox_viz);
    return expand_MHD_viz;
};

GtkWidget *MHD_dynamo_VIZs_expander(GtkWidget *window, struct f_MHD_zm_ctls *f_zm_ctls, 
                                    struct f_MHD_fields_control *f_fld_ctl,
                                    struct dynamo_VIZs_widgets *dviz_Wgts){
    GtkWidget *hbox_d1 = draw_int_item_entry_hbox(f_zm_ctls->f_crust_filter_ctl->f_crust_truncation_ctl);
	GtkWidget *expand_MHD_zm1 = draw_control_block(f_zm_ctls->f_crust_filter_ctl->c_block_name, 
                                                   f_zm_ctls->f_crust_filter_ctl->f_iflag,
                                                   window, hbox_d1);
	GtkWidget *expand_MHD_zm2 = draw_array_block_ctl_vbox(f_zm_ctls->f_zm_psf_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          dviz_Wgts->zm_psf_Wgts, window);
	GtkWidget *expand_MHD_zm3 = draw_array_block_ctl_vbox(f_zm_ctls->f_zRMS_psf_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_section_ctls,
                                                          c_delete_viz_section_ctls,
                                                          (void *) init_f_VIZ_PSF_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_PSF_ctl_GTK,
                                                          (void *) draw_viz_each_psf_ctl_vbox,
                                                          dviz_Wgts->zrms_psf_Wgts, window);
    
	GtkWidget *expand_MHD_zm4 = draw_array_block_ctl_vbox(f_zm_ctls->f_zm_map_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          dviz_Wgts->zm_map_Wgts, window);
	GtkWidget *expand_MHD_zm5 = draw_array_block_ctl_vbox(f_zm_ctls->f_zRMS_map_ctls,
                                                          (void *) f_fld_ctl,
                                                          c_append_viz_map_render_ctls,
                                                          c_delete_viz_map_render_ctls,
                                                          (void *) init_f_VIZ_MAP_ctl_GTK,
                                                          (void *) dealloc_f_VIZ_MAP_ctl_GTK,
                                                          (void *) draw_viz_each_map_ctl_vbox,
                                                          dviz_Wgts->zrms_map_Wgts, window);
  
    GtkWidget *vbox_zm = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_zm), expand_MHD_zm5, FALSE, FALSE, 0);
    
	GtkWidget *expand_MHD_zm = draw_control_block(f_zm_ctls->c_block_name, 
													 f_zm_ctls->f_iflag,
													 window, vbox_zm);
	return expand_MHD_zm;
};

