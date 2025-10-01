/*
//  control_panel_PVR_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_PVR_colormap.h"


static GtkWidget * draw_viz_colormap_vbox(struct colormap_ctl_c *cmap_c, 
                                          struct colormap_view *color_vws, 
                                          struct chara_int2_clist *label_field_list, 
                                          struct chara2_int_clist *label_dir_list,
                                          GtkWidget *window){
    GtkWidget *hbox_2 = draw_field_combobox_hbox(label_field_list, 
                                                 cmap_c->f_lic_color_fld_ctl, window);
    GtkWidget *hbox_3 = draw_component_combobox_hbox(label_dir_list,
                                                     cmap_c->f_lic_color_comp_ctl, window);
    GtkWidget *hbox_4 = draw_field_combobox_hbox(label_field_list, 
                                                 cmap_c->f_lic_opacity_fld_ctl, window);
    GtkWidget *hbox_5 = draw_component_combobox_hbox(label_dir_list,
                                                     cmap_c->f_lic_opacity_comp_ctl, window);
    
    GtkWidget *hbox_10 = draw_real3_item_entry_hbox(cmap_c->f_background_color_ctl);
    
    GtkWidget *hbox_6 = draw_chara_item_entry_hbox(cmap_c->f_data_mapping_ctl);
    GtkWidget *hbox_7 = draw_chara_item_entry_hbox(cmap_c->f_opacity_style_ctl);
    GtkWidget *hbox_8 = draw_real_item_entry_hbox(cmap_c->f_fix_opacity_ctl);
    
    GtkWidget *hbox_9 =  draw_real_item_entry_hbox(cmap_c->f_range_min_ctl);
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(cmap_c->f_range_max_ctl);
    
    GtkWidget * frame_cmap = add_pvr_colormap_list_box_2(color_vws, vbox_cbox);

    GtkWidget *vbox_cbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_10,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbox), frame_cmap, TRUE, TRUE, 0);

    GtkWidget *expand_cmap = draw_control_block(cmap_c->c_block_name, cmap_c->f_iflag,
                                                window, vbox_cbox);
    return expand_cmap;
}
static GtkWidget * draw_viz_colorbar_vbox(struct pvr_colorbar_ctl_c *cbar_c,  GtkWidget *window){
    
    GtkWidget *hbox_1 = draw_chara_switch_entry_hbox(cbar_c->f_colorbar_switch_ctl);
    GtkWidget *hbox_2 = draw_chara_switch_entry_hbox(cbar_c->f_colorbar_scale_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(cbar_c->f_colorbar_position_ctl);
    GtkWidget *hbox_4 = draw_chara_switch_entry_hbox(cbar_c->f_zeromarker_flag_ctl);
    
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(cbar_c->f_font_size_ctl);
    GtkWidget *hbox_6 = draw_int_item_entry_hbox(cbar_c->f_ngrid_cbar_ctl);
    GtkWidget *hbox_7 = draw_real2_item_entry_hbox(cbar_c->f_cbar_range_ctl);
    
    GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(cbar_c->f_axis_switch_ctl);
    GtkWidget *hbox_9 = draw_chara_switch_entry_hbox(cbar_c->f_time_switch_ctl);
    GtkWidget *hbox_10 = draw_chara_switch_entry_hbox(cbar_c->f_mapgrid_switch_ctl);
    
    GtkWidget *vbox_cbar = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cbar), hbox_10, FALSE, FALSE, 0);
    
    GtkWidget *expand_cbar = draw_control_block(cbar_c->c_block_name, cbar_c->f_iflag,
                                                window, vbox_cbar);
    return expand_cbar;
}


void append_viz_cmap_cbar_vbox(struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c,
                               struct colormap_view *color_vws, 
                               struct chara_int2_clist *label_field_list, 
                               struct chara2_int_clist *label_dir_list,
                               GtkWidget *window, GtkWidget *vbox_cc){
    GtkWidget *expand_cmap =  draw_viz_colormap_vbox(f_cmap_cbar_c->cmap_c, color_vws, 
                                                     label_field_list, label_dir_list, window);
    GtkWidget *expand_cbar =  draw_viz_colorbar_vbox(f_cmap_cbar_c->cbar_c, window);
    
    GtkWidget *label = gtk_label_new(f_cmap_cbar_c->c_block_name);
	GtkWidget *hbox_cc = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox_cc), label, FALSE, TRUE, 0);
    append_block_file_switch_hbox(f_cmap_cbar_c->cmap_ctl_file_name, hbox_cc);
    
    gtk_box_pack_start(GTK_BOX(vbox_cc), hbox_cc,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cc), expand_cmap,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_cc), expand_cbar,  FALSE, FALSE, 0);
    return;
};

GtkWidget * draw_pvr_lighting_vbox(struct lighting_ctl_c *light_ctl_c,
                                   GtkWidget *lighting_tree_view, GtkWidget *window){
    GtkWidget *hbox_1 = draw_real_item_entry_hbox(light_ctl_c->f_ambient_coef_ctl);
    GtkWidget *hbox_2 = draw_real_item_entry_hbox(light_ctl_c->f_diffuse_coef_ctl);
    GtkWidget *hbox_3 = draw_real_item_entry_hbox(light_ctl_c->f_specular_coef_ctl);
    GtkWidget *expand_4 = r3_list_combobox_expander(light_ctl_c->f_light_position_ctl,
                                                    lighting_tree_view, window);
    GtkWidget *expand_5 = r3_list_combobox_expander(light_ctl_c->f_light_sph_posi_ctl,
                                                    lighting_tree_view, window);

    GtkWidget *vbox_lgt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), expand_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_lgt), expand_5,  FALSE, FALSE, 0);

    GtkWidget *expand_lgt = draw_control_block_w_file_switch(light_ctl_c->c_block_name,
                                                             light_ctl_c->f_iflag,
                                                             light_ctl_c->light_ctl_file_name,
                                                             window, vbox_lgt);
    return expand_lgt;
}

