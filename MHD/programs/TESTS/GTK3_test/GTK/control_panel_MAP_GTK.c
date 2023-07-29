/*
//  control_panel_MAP_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_MAP_GTK.h"

struct MAP_GTK_widgets * init_MAP_GTK_widgets(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                                              struct chara_int2_clist *f_field_ctl)
{
	struct MAP_GTK_widgets *map_vws 
			= (struct MAP_GTK_widgets *) malloc(sizeof(struct MAP_GTK_widgets));
	if(map_vws == NULL){
		printf("malloc error for map_vws\n");
		exit(0);
    };
    
    map_vws->label_field_list = f_field_ctl;
    map_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             map_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             map_vws->label_dir_list);
    map_vws->color_vws = init_colormap_views_4_ctl(f_cmap_cbar_c->cmap_c);
    map_vws->psf_def_vws = init_PSF_GTK_widgets(f_field_ctl);
    return map_vws;
};
void dealloc_MAP_GTK_widgets(struct MAP_GTK_widgets *map_vws){
    dealloc_PSF_GTK_widgets(map_vws->psf_def_vws);
    dealloc_chara_int2_clist(map_vws->label_field_list);
    dealloc_chara2_int_clist(map_vws->label_dir_list);
    dealloc_colormap_views_4_viewer(map_vws->color_vws);
    free(map_vws);
}

struct f_VIZ_MAP_ctl * init_f_VIZ_MAP_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_MAP_ctl *f_map_ctl = init_f_VIZ_MAP_ctl(idx, f_parent);
    f_map_ctl->void_panel = (void *) init_MAP_GTK_widgets(f_map_ctl->f_cmap_cbar_c, 
                                                          f_fld_ctl->f_field_ctl);
    f_map_ctl->f_mat->void_panel = (void *) init_viewmat_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_map_ctl;
}
void *dealloc_f_VIZ_MAP_ctl_GTK(void *void_in){
    struct f_VIZ_MAP_ctl *f_map_ctl = (struct f_VIZ_MAP_ctl *) void_in;
    dealloc_viewmat_GTK_widgets((struct viewmat_GTK_widgets *) f_map_ctl->f_mat->void_panel);
    dealloc_MAP_GTK_widgets((struct MAP_GTK_widgets *) f_map_ctl->void_panel);
    dealloc_f_VIZ_MAP_ctl(f_map_ctl);
    return NULL;
}



static GtkWidget * draw_map_define_vbox(struct f_MAP_section_ctl *f_map_define_ctl, 
                                        struct PSF_GTK_widgets *psf_def_vws, GtkWidget *window){
    GtkWidget *expand_s =  draw_psf_def_ctl_vbox(f_map_define_ctl->f_psf_def_c, 
                                                 psf_def_vws, window);
                                                                                                 
    GtkWidget *hbox_1 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_zeroline_switch_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_define_ctl->f_isoline_color_mode);
    GtkWidget *hbox_3 = draw_int_item_entry_hbox(f_map_define_ctl->f_isoline_number_ctl);
    GtkWidget *hbox_4 = draw_real2_item_entry_hbox(f_map_define_ctl->f_isoline_range_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_map_define_ctl->f_isoline_width_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_map_define_ctl->f_grid_width_ctl);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_map_define_ctl->f_tan_cyl_switch_ctl);
    GtkWidget *hbox_8 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_inner_ctl);
    GtkWidget *hbox_9 = draw_real_item_entry_hbox(f_map_define_ctl->f_tangent_cylinder_outer_ctl);
    
    GtkWidget *vbox_map_def = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), expand_s,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_map_def), hbox_9,  FALSE, FALSE, 0);
    
    GtkWidget *expand_map_def = draw_control_block(f_map_define_ctl->c_block_name, 
                                                   f_map_define_ctl->f_iflag,
                                                   window, vbox_map_def);
    return expand_map_def;
};

GtkWidget * draw_viz_each_map_ctl_vbox(char *label_name, struct f_VIZ_MAP_ctl *f_map_item,
                                       GtkWidget *window){
    struct MAP_GTK_widgets *map_vws = (struct MAP_GTK_widgets *) f_map_item->void_panel;
	GtkWidget *vbox_v_map = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_map_item->f_map_image_prefix_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_map_item->f_map_image_fmt_ctl);
    GtkWidget *hbox_3 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_map_field_ctl, window);
    GtkWidget *hbox_4 = draw_component_combobox_hbox(map_vws->label_dir_list,
                                                     f_map_item->f_map_comp_ctl, window);
    GtkWidget *hbox_5 = draw_field_combobox_hbox(map_vws->label_field_list, 
                                                 f_map_item->f_isoline_field_ctl, window);
    GtkWidget *hbox_6 = draw_component_combobox_hbox(map_vws->label_dir_list,
                                                    f_map_item->f_isoline_comp_ctl, window);
    
    GtkWidget *expand_mdef = draw_map_define_vbox(f_map_item->f_map_define_ctl, 
                                                  map_vws->psf_def_vws, window);
    GtkWidget *expand_vmat = draw_viz_viewmatrix_vbox(f_map_item->f_mat->c_block_name, 
                                                      f_map_item->f_mat, window);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_mdef, FALSE, FALSE, 0);
    
    append_viz_cmap_cbar_vbox(f_map_item->f_cmap_cbar_c, map_vws->color_vws, 
                              map_vws->label_field_list, map_vws->label_dir_list, 
                              window, vbox_v_map);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_map), expand_vmat, FALSE, FALSE, 0);
    
    
    GtkWidget *expand_v_map = draw_control_block_w_file_switch(duplicate_underscore(label_name),
															   f_map_item->f_iflag,
															   f_map_item->map_ctl_file_name,
															   window, vbox_v_map);
    return expand_v_map;
};

