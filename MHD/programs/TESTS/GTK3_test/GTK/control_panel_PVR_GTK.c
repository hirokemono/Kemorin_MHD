/*
//  control_panel_PVR_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_PVR_GTK.h"


struct PVR_GTK_widgets * init_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct PVR_GTK_widgets *pvr_vws 
			= (struct PVR_GTK_widgets *) malloc(sizeof(struct PVR_GTK_widgets));
	if(pvr_vws == NULL){
		printf("malloc error for pvr_vws\n");
		exit(0);
    };
	pvr_vws->pvr_areaWgts = (struct pvr_area_widgets *) malloc(sizeof(struct pvr_area_widgets));
	if(pvr_vws->pvr_areaWgts == NULL){
		printf("malloc error for pvr_area_widgets\n");
		exit(0);
    };
    
    f_pvr_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_field_ctl);
    int i;
    struct f_PVR_section_ctl *f_pvr_sect_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_scts_c);i++){
        f_pvr_sect_ctl = (struct f_PVR_section_ctl *) void_clist_at_index(i, f_pvr_ctl->f_pvr_scts_c);
        f_pvr_sect_ctl->void_panel = (void *) init_PSF_GTK_widgets(f_field_ctl);
    }
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_isos_c);i++){
        f_pvr_iso_ctl = (struct f_PVR_isosurface_ctl *) void_clist_at_index(i, f_pvr_ctl->f_pvr_isos_c);
        f_pvr_iso_ctl->void_panel = (void *) init_ISO_GTK_widgets(f_field_ctl);
    }
    
    struct modelview_ctl_c *f_mat_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_quilt_c->f_mul_qmats_c);i++){
        f_mat_ctl = (struct modelview_ctl_c *) void_clist_at_index(i, f_pvr_ctl->f_quilt_c->f_mul_qmats_c);
        f_mat_ctl->void_panel = (void *) init_viewmat_GTK_widgets(f_field_ctl);
    }
    for(i=0;i<count_void_clist(f_pvr_ctl->f_movie->f_mul_mmats_c);i++){
        f_mat_ctl = (struct modelview_ctl_c *) void_clist_at_index(i, f_pvr_ctl->f_movie->f_mul_mmats_c);
        f_mat_ctl->void_panel = (void *) init_viewmat_GTK_widgets(f_field_ctl);
    }
    f_pvr_ctl->f_movie->f_view_start_ctl->void_panel = (void *) init_viewmat_GTK_widgets(f_field_ctl);
    f_pvr_ctl->f_movie->f_view_end_ctl->void_panel = (void *) init_viewmat_GTK_widgets(f_field_ctl);
    
    pvr_vws->label_field_list = f_field_ctl;
    pvr_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             pvr_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             pvr_vws->label_dir_list);
    pvr_vws->color_vws = init_colormap_views_4_ctl(f_pvr_ctl->f_cmap_cbar_c->cmap_c);
    return pvr_vws;
};

void dealloc_PVR_GTK_widgets(struct f_VIZ_PVR_ctl *f_pvr_ctl,
                             struct PVR_GTK_widgets *pvr_vws){
    dealloc_colormap_views_4_viewer(pvr_vws->color_vws);
    dealloc_chara_int2_clist(pvr_vws->label_field_list);
    dealloc_chara2_int_clist(pvr_vws->label_dir_list);
    
    int i;
    struct PSF_GTK_widgets *psf_def_vws;
    struct f_PVR_section_ctl *f_pvr_sect_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_scts_c);i++){
        f_pvr_sect_ctl = (struct f_PVR_section_ctl *) void_clist_at_index(i,f_pvr_ctl->f_pvr_scts_c);
        dealloc_f_VIZ_PSF_ctl_GTK(f_pvr_sect_ctl->void_panel);
    }
    
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_pvr_isos_c);i++){
        f_pvr_iso_ctl = (struct f_PVR_isosurface_ctl *) void_clist_at_index(i, f_pvr_ctl->f_pvr_isos_c);
        dealloc_f_VIZ_ISO_ctl_GTK(f_pvr_iso_ctl->void_panel);
    }
    
    struct modelview_ctl_c *f_mat_ctl;
    for(i=0;i<count_void_clist(f_pvr_ctl->f_quilt_c->f_mul_qmats_c);i++){
        f_mat_ctl = (struct modelview_ctl_c *) void_clist_at_index(i, f_pvr_ctl->f_quilt_c->f_mul_qmats_c);
        dealloc_viewmat_GTK_widgets(f_mat_ctl->void_panel);
    }
    for(i=0;i<count_void_clist(f_pvr_ctl->f_movie->f_mul_mmats_c);i++){
        f_mat_ctl = (struct modelview_ctl_c *) void_clist_at_index(i, f_pvr_ctl->f_movie->f_mul_mmats_c);
        dealloc_viewmat_GTK_widgets(f_mat_ctl->void_panel);
    }
    dealloc_viewmat_GTK_widgets(f_pvr_ctl->f_movie->f_view_start_ctl->void_panel);
    dealloc_viewmat_GTK_widgets(f_pvr_ctl->f_movie->f_view_end_ctl->void_panel);
    
    dealloc_viewmat_GTK_widgets((struct viewmat_GTK_widgets *) f_pvr_ctl->f_mat->void_panel);
    
    free(pvr_vws->pvr_areaWgts);
    free(pvr_vws);
}

struct f_PVR_section_ctl * init_f_PVR_sections_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_PVR_section_ctl *f_pvr_psf_c = init_f_PVR_section_ctl(idx, f_parent);
    f_pvr_psf_c->void_panel = (void *) init_PSF_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_pvr_psf_c;
}
static void dealloc_f_PVR_section_GTK(void *void_ctl){
    struct f_PVR_section_ctl *f_pvr_psf_c = (struct f_PVR_section_ctl *) void_ctl;
    dealloc_PSF_GTK_widgets((struct PSF_GTK_widgets *) f_pvr_psf_c->void_panel);
    dealloc_f_PVR_section_ctl(f_pvr_psf_c);
    return;
};

static void dealloc_f_PVR_isosurface_GTK(void *void_ctl){
    struct f_PVR_isosurface_ctl *f_pvr_iso_ctl = ((struct f_PVR_isosurface_ctl *) void_ctl);
    dealloc_f_PVR_isosurface_ctl(f_pvr_iso_ctl);
    return;
};


struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_PVR_ctl *f_pvr_ctl = init_f_VIZ_PVR_ctl(c_pvr_render_ctls_pvr_ctl, 
                                                         idx, f_parent);
    f_pvr_ctl->void_panel = (void *) init_PVR_GTK_widgets(f_pvr_ctl, f_fld_ctl->f_field_ctl);
    
    return f_pvr_ctl;
}
void * dealloc_f_VIZ_PVR_ctl_GTK(void *void_in){
    struct f_VIZ_PVR_ctl *f_pvr_ctl = (struct f_VIZ_PVR_ctl *) void_in;
    dealloc_PVR_GTK_widgets(f_pvr_ctl, (struct PVR_GTK_widgets *) f_pvr_ctl->void_panel);
    dealloc_f_VIZ_PVR_ctl(f_pvr_ctl);
    return NULL;
}


static GtkWidget * draw_pvr_section_vbox(char *label_name, struct f_PVR_section_ctl *f_pvr_psf_c,
                                         GtkWidget *window){
    struct PSF_GTK_widgets *psf_def_vws = (struct PSF_GTK_widgets *) f_pvr_psf_c->void_panel;
    
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_pvr_psf_c->f_opacity_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_pvr_psf_c->f_zeroline_switch_ctl);
    GtkWidget *expand_s =  draw_psf_def_ctl_vbox(f_pvr_psf_c->f_psf_def_c, 
                                                 psf_def_vws, window);
    
    GtkWidget *vbox_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_psf), expand_s,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_psf = draw_control_block(label_name, f_pvr_psf_c->f_iflag,
                                               window, vbox_psf);
    return expand_psf;
}

static GtkWidget * draw_pvr_isosurface_vbox(char *label_name, struct f_PVR_isosurface_ctl *f_pvr_iso_ctl,
                                            GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_pvr_iso_ctl->f_iso_value_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_pvr_iso_ctl->f_opacity_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_pvr_iso_ctl->f_isosurf_type_ctl);
    
    GtkWidget *vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_iso = draw_control_block(label_name, f_pvr_iso_ctl->f_iflag,
                                               window, vbox_iso);
    return expand_iso;
}

static GtkWidget * draw_pvr_plot_area_vbox(struct pvr_plot_area_ctl_c *f_area,
                                           struct pvr_area_widgets *pvr_areaWgts, GtkWidget *window){
    GtkWidget *expand_s1 = add_c_list_box_w_addbottun(f_area->f_pvr_area_ctl,
                                                      pvr_areaWgts->f_pvr_area_tree);
    GtkWidget *expand_s2 = c2r_list_text2_expander(f_area->f_surf_enhanse_ctl,
                                                   pvr_areaWgts->f_surf_enhanse_tree, window);
    
    GtkWidget *vbox_area = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_area), expand_s1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_area), expand_s2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_area = draw_control_block(f_area->c_block_name, f_area->f_iflag,
                                                 window, vbox_area);
    return expand_area;
};

static GtkWidget * draw_PVR_quilt_ctl_vbox(struct f_PVR_quilt_image_ctl *f_quilt_c, 
                                           struct PVR_GTK_widgets *pvr_vws, GtkWidget *window){
    GtkWidget *hbox_1 = draw_int2_item_entry_hbox(f_quilt_c->f_num_column_row_ctl);
    GtkWidget *hbox_2 = draw_int2_item_entry_hbox(f_quilt_c->f_num_row_column_ctl);
    
    GtkWidget *expander_vmat =  draw_array_block_ctl_vbox(f_quilt_c->f_mul_qmats_c,
                                                          pvr_vws->label_field_list,
                                                          c_append_mul_mdlvw_mat_ctl,
                                                          c_delete_mul_mdlvw_mat_ctl,
                                                          (void *) init_modelview_ctl_GTK,
                                                          (void *) dealloc_modelview_ctl_GTK,
                                                          (void *) draw_viz_viewmatrix_vbox,
                                                          pvr_vws->pvr_qmat_Wgts, window);
    
    GtkWidget *vbox_qlt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_qlt), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_qlt), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_qlt), expander_vmat,  FALSE, FALSE, 0);
    
    GtkWidget *expand_qlt = draw_control_block(f_quilt_c->c_block_name,
                                               f_quilt_c->f_iflag, window, vbox_qlt);
    return expand_qlt;
};

static GtkWidget * draw_PVR_movie_ctl_vbox(struct pvr_movie_ctl_c *f_movie, 
                                           struct PVR_GTK_widgets *pvr_vws, GtkWidget *window){
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_movie->f_movie_mode_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_movie->f_num_frames_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_movie->f_rotation_axis_ctl);
    GtkWidget *hbox_4 = draw_real2_item_entry_hbox(f_movie->f_angle_range_ctl);
    GtkWidget *hbox_5 = draw_real2_item_entry_hbox(f_movie->f_apature_range_ctl);
    GtkWidget *hbox_6 = draw_real2_item_entry_hbox(f_movie->f_LIC_kernel_peak_range_ctl);
    
    GtkWidget *expander_smat = draw_viz_viewmatrix_vbox(f_movie->f_view_start_ctl->c_block_name, 
                                                        f_movie->f_view_start_ctl, window);
    GtkWidget *expander_emat = draw_viz_viewmatrix_vbox(f_movie->f_view_end_ctl->c_block_name, 
                                                        f_movie->f_view_end_ctl, window);
    GtkWidget *expander_vmat =  draw_array_block_ctl_vbox(f_movie->f_mul_mmats_c,
                                                          pvr_vws->label_field_list,
                                                          c_append_mul_mdlvw_mat_ctl,
                                                          c_delete_mul_mdlvw_mat_ctl,
                                                          (void *) init_modelview_ctl_GTK,
                                                          (void *) dealloc_modelview_ctl_GTK,
                                                          (void *) draw_viz_viewmatrix_vbox,
                                                          pvr_vws->pvr_mmat_Wgts, window);
    
    GtkWidget *vbox_mve = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), expander_smat,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), expander_emat,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_mve), expander_vmat,  FALSE, FALSE, 0);
    
    GtkWidget *expand_mve = draw_control_block(f_movie->c_block_name,
                                               f_movie->f_iflag, window, vbox_mve);
    return expand_mve;
};



GtkWidget * draw_viz_each_pvr_ctl_vbox(char *label_name, struct f_VIZ_PVR_ctl *f_pvr_item, 
                                        GtkWidget *window){
    struct PVR_GTK_widgets *pvr_vws = (struct PVR_GTK_widgets *) f_pvr_item->void_panel;
    GtkWidget *vbox_v_pvr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_pvr_item->f_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_pvr_item->f_file_fmt_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_pvr_item->f_monitoring_ctl);
    GtkWidget *hbox_4 = draw_chara_switch_entry_hbox(f_pvr_item->f_streo_ctl);
    GtkWidget *hbox_5 = draw_chara_switch_entry_hbox(f_pvr_item->f_anaglyph_ctl);
    GtkWidget *hbox_6 = draw_chara_switch_entry_hbox(f_pvr_item->f_quilt_ctl);
    
    GtkWidget *hbox_7 = draw_field_combobox_hbox(pvr_vws->label_field_list,
                                                f_pvr_item->f_pvr_field_ctl, window);
    GtkWidget *hbox_8 = draw_component_combobox_hbox(pvr_vws->label_dir_list,
                                                     f_pvr_item->f_pvr_comp_ctl, window);
                                                     
    GtkWidget *hbox_9 = draw_pvr_plot_area_vbox(f_pvr_item->f_render_area_c,
                                                pvr_vws->pvr_areaWgts, window);
    
    GtkWidget *expand_v_lgt = draw_pvr_lighting_vbox(f_pvr_item->f_light,
                                                     pvr_vws->lighting_tree_view, window);
    
    GtkWidget *expander_vmat = draw_viz_viewmatrix_vbox(f_pvr_item->f_mat->c_block_name, 
                                                        f_pvr_item->f_mat, window);
    GtkWidget *expander_psf =  draw_array_block_ctl_vbox(f_pvr_item->f_pvr_scts_c,
                                                        pvr_vws->label_field_list,
                                                        c_append_PVR_sections_ctls,
                                                        c_delete_PVR_sections_ctls,
                                                        (void *) init_f_PVR_sections_GTK,
                                                        (void *) dealloc_f_PVR_section_GTK,
                                                        (void *) draw_pvr_section_vbox,
                                                        pvr_vws->pvr_psfs_Wgts, window);
    GtkWidget *expander_iso = draw_array_block_ctl_vbox(f_pvr_item->f_pvr_isos_c, NULL,
                                                        c_append_PVR_isosurface_ctls,
                                                        c_delete_PVR_isosurface_ctls,
                                                        (void *) init_f_PVR_isosurface_ctl,
                                                        (void *) dealloc_f_PVR_isosurface_GTK,
                                                        (void *) draw_pvr_isosurface_vbox,
                                                        pvr_vws->pvr_isos_Wgts, window);
    GtkWidget *expander_qlt = draw_PVR_movie_ctl_vbox(f_pvr_item->f_movie,
                                                      pvr_vws, window);
    GtkWidget *expander_mve = draw_PVR_quilt_ctl_vbox(f_pvr_item->f_quilt_c,
                                                      pvr_vws, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), hbox_9,  FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_pvr_item->f_cmap_cbar_c, pvr_vws->color_vws, 
                              pvr_vws->label_field_list, pvr_vws->label_dir_list, 
                              window, vbox_v_pvr);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expand_v_lgt,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_vmat,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_psf,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_iso,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_qlt,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pvr), expander_mve,  FALSE, FALSE, 0);

    
    GtkWidget *expand_v_pwr = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_pvr_item->f_iflag,
															   f_pvr_item->pvr_ctl_file_name,
															   window, vbox_v_pvr);
    return expand_v_pwr;
};
