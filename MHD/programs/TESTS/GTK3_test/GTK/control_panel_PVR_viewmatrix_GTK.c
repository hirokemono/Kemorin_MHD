/*
//  control_panel_PVR_viewmatrix_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_PVR_viewmatrix_GTK.h"

struct viewmat_GTK_widgets * init_viewmat_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
    struct viewmat_GTK_widgets *viewmatrix_vws
            = (struct viewmat_GTK_widgets *) malloc(sizeof(struct viewmat_GTK_widgets));
    if(viewmatrix_vws == NULL){
        printf("malloc error for viewmatrix_vws\n");
        exit(0);
    };
    
    viewmatrix_vws->label_xyz_dir_list =  init_field_label_array(c_link_xyz_dir_list_to_ctl());
    viewmatrix_vws->label_xyzw_dir_list = init_field_label_array(c_link_xyzw_dir_list_to_ctl());
    return viewmatrix_vws;
}

void dealloc_viewmat_GTK_widgets(struct viewmat_GTK_widgets *viewmatrix_vws){
    dealloc_chara2_int_clist(viewmatrix_vws->label_xyzw_dir_list);
    dealloc_chara2_int_clist(viewmatrix_vws->label_xyz_dir_list);
    free(viewmatrix_vws);
}

struct modelview_ctl_c * init_modelview_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct modelview_ctl_c *mat_c = init_modelview_ctl_c();
    mat_c->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    
    return mat_c;
}
void * dealloc_modelview_ctl_GTK(void *void_in){
    struct modelview_ctl_c *mat_c = (struct modelview_ctl_c *) void_in;
    dealloc_viewmat_GTK_widgets((struct viewmat_GTK_widgets *) mat_c->void_panel);
    dealloc_modelview_ctl_c(mat_c);
    return NULL;
}



static GtkWidget * draw_viewmatrix_pixels_vbox(struct image_size_ctl_c *f_pixel, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_int_item_entry_hbox(f_pixel->f_num_xpixel_ctl);
    GtkWidget *hbox_2 = draw_int_item_entry_hbox(f_pixel->f_num_ypixel_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    
    GtkWidget *expand_pixel = draw_control_block(f_pixel->c_block_name, f_pixel->f_iflag,
                                                 window, vbox_v_pix);
    return expand_pixel;
};

static GtkWidget * draw_viewmatrix_projection_vbox(struct projection_mat_ctl_c *f_proj, 
                                                   GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_proj->f_perspective_angle_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_proj->f_perspective_xy_ratio_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_proj->f_perspective_near_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_proj->f_perspective_far_ctl);
    
    GtkWidget *hbox_5 = draw_real2_item_entry_hbox(f_proj->f_horizontal_range_ctl);
    GtkWidget *hbox_6 = draw_real2_item_entry_hbox(f_proj->f_vertical_range_ctl);
    
    GtkWidget *vbox_v_pix = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pix), hbox_6,  FALSE, FALSE, 0);
    
    GtkWidget *expand_proj = draw_control_block(f_proj->c_block_name, f_proj->f_iflag,
                                                window, vbox_v_pix);
    return expand_proj;
};

static GtkWidget * draw_viewmatrix_stereo_vbox(struct streo_view_ctl_c *f_streo, 
                                               GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(f_streo->f_focalpoint_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(f_streo->f_eye_separation_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(f_streo->f_eye_sep_angle_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_streo->f_step_eye_sep_angle_ctl);
    
    GtkWidget *vbox_s = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_s), hbox_4,  FALSE, FALSE, 0);
    
    GtkWidget *expand_streo = draw_control_block(f_streo->c_block_name, f_streo->f_iflag,
                                                 window, vbox_s);
    return expand_streo;
};

GtkWidget * draw_viz_viewmatrix_vbox(char *label_name, 
                                     struct modelview_ctl_c *f_mat_c, 
                                     GtkWidget *window){
    struct viewmat_GTK_widgets *viewmatrix_vws = (struct viewmat_GTK_widgets *) f_mat_c->void_panel;
    
    GtkWidget *expand_pixel = draw_viewmatrix_pixels_vbox(f_mat_c->f_pixel, window);
    GtkWidget *expand_proj =  draw_viewmatrix_projection_vbox(f_mat_c->f_proj, window);
    GtkWidget *expand_streo = draw_viewmatrix_stereo_vbox(f_mat_c->f_streo, window);
    
    GtkWidget *hbox_1 = c2r_list_combobox_expander(f_mat_c->f_modelview_mat_ctl,
                                                   viewmatrix_vws->label_xyzw_dir_list,
                                                   viewmatrix_vws->label_xyzw_dir_list, 
                                                   viewmatrix_vws->f_map_image_prefix_vws, window);
    GtkWidget *hbox_2 = cr_list_combobox_expander(f_mat_c->f_lookpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_lookpoint_vws, window);
    GtkWidget *hbox_3 = cr_list_combobox_expander(f_mat_c->f_viewpoint_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpoint_vws, window);
    GtkWidget *hbox_4 = cr_list_combobox_expander(f_mat_c->f_up_dir_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_up_dir_vws, window);
    GtkWidget *hbox_5 = cr_list_combobox_expander(f_mat_c->f_view_rot_vec_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_view_rot_vec_vws, window);
    
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_mat_c->f_view_rotation_deg_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_mat_c->f_scale_factor_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_mat_c->f_projection_type_ctl);
    
    GtkWidget *hbox_9 = cr_list_combobox_expander(f_mat_c->f_scale_vector_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_scale_vector_vws, window);
    GtkWidget *hbox_10 = cr_list_combobox_expander(f_mat_c->f_viewpt_in_viewer_ctl,
                                                  viewmatrix_vws->label_xyz_dir_list,
                                                  viewmatrix_vws->f_viewpt_in_viewer_vws, window);
    
	GtkWidget *vbox_v_mat = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_pixel,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_proj,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_mat), expand_streo,  FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_mat = draw_control_block_w_file_switch(label_name,
															   f_mat_c->f_iflag,
															   f_mat_c->mat_ctl_file_name,
															   window, vbox_v_mat);
    return expand_v_mat;
};
