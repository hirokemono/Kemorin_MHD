/*
//  control_panel_FLINE_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_FLINE_GTK.h"

extern void * c_link_scalar_dir_list_to_ctl(void);
extern void * c_link_vector_dir_list_to_ctl(void);
extern void * c_link_stensor_dir_list_to_ctl(void);
extern void * c_link_atensor_dir_list_to_ctl(void);


struct FLINE_GTK_widgets * init_FLINE_GTK_widgets(struct chara_int2_clist *f_field_ctl)
{
	struct FLINE_GTK_widgets *fline_vws 
			= (struct FLINE_GTK_widgets *) malloc(sizeof(struct FLINE_GTK_widgets));
	if(fline_vws == NULL){
		printf("malloc error for fline_vws\n");
		exit(0);
    };
    fline_vws->label_field_list = f_field_ctl;
    fline_vws->label_dir_list = init_field_label_array(c_link_scalar_dir_list_to_ctl());
    append_field_label_array(c_link_vector_dir_list_to_ctl(),
                             fline_vws->label_dir_list);
    append_field_label_array(c_link_stensor_dir_list_to_ctl(),
                             fline_vws->label_dir_list);
    return fline_vws;
};

void dealloc_FLINE_GTK_widgets(struct FLINE_GTK_widgets *fline_vws){
    dealloc_chara_int2_clist(fline_vws->label_field_list);
    dealloc_chara2_int_clist(fline_vws->label_dir_list);
    free(fline_vws);
    return;
}

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl_GTK(int idx, void *f_parent, void *void_in_gtk){
    struct f_MHD_fields_control *f_fld_ctl = (struct f_MHD_fields_control *) void_in_gtk;
    struct f_VIZ_FLINE_ctl *f_fline_ctl = init_f_VIZ_FLINE_ctl(idx, f_parent);
    f_fline_ctl->void_panel = (void *) init_FLINE_GTK_widgets(f_fld_ctl->f_field_ctl);
    return f_fline_ctl;
}
void * dealloc_f_VIZ_FLINE_ctl_GTK(void *void_in){
    struct f_VIZ_FLINE_ctl *f_fline_ctl = (struct f_VIZ_FLINE_ctl *) void_in;
    dealloc_FLINE_GTK_widgets((struct FLINE_GTK_widgets *) f_fline_ctl->void_panel);
    dealloc_f_VIZ_FLINE_ctl(f_fline_ctl);
    return NULL;
}



GtkWidget * draw_viz_each_fline_ctl_vbox(char *label_name, struct f_VIZ_FLINE_ctl *f_fline_item, 
                                         GtkWidget *window){
    struct FLINE_GTK_widgets *fline_vws = (struct FLINE_GTK_widgets *) f_fline_item->void_panel;
    
    GtkWidget *vbox_v_fline = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_fline_item->f_fline_file_head_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_fline_item->f_fline_output_type_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_fline_item->f_fline_field_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_field_ctl);
    GtkWidget *hbox_5 = draw_chara_item_entry_hbox(f_fline_item->f_fline_color_comp_ctl);
    
    GtkWidget *hbox_6 = add_c_list_box_w_addbottun(f_fline_item->f_fline_area_grp_ctl, 
                                                   fline_vws->fline_area_grp_view);
    
    GtkWidget *hbox_7 = draw_chara_item_entry_hbox(f_fline_item->f_starting_type_ctl);
    GtkWidget *hbox_8 = draw_chara_item_entry_hbox(f_fline_item->f_selection_type_ctl);
    GtkWidget *hbox_9 = draw_chara_item_entry_hbox(f_fline_item->f_line_direction_ctl);
    
    GtkWidget *hbox_10 = draw_chara_item_entry_hbox(f_fline_item->f_start_surf_grp_ctl);
    
    GtkWidget *hbox_11 = draw_int_item_entry_hbox(f_fline_item->f_num_fieldline_ctl);
    GtkWidget *hbox_12 = draw_int_item_entry_hbox(f_fline_item->f_max_line_stepping_ctl);
    GtkWidget *hbox_15 = draw_int_item_entry_hbox(f_fline_item->f_max_trace_length_ctl);
    GtkWidget *hbox_13 = r3_list_combobox_expander(f_fline_item->f_seed_point_ctl,
                                                   fline_vws->f_seed_point_vws, window);
    
    GtkWidget *hbox_14 = add_i2_list_box_w_addbottun(f_fline_item->f_seed_surface_ctl,
                                                     fline_vws->f_seed_surface_vws);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_5,  FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_9,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_10, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_11, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_12, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_13, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_fline), hbox_14, FALSE, FALSE, 0);
    
    GtkWidget *expand_v_fline = draw_control_block_w_file_switch(duplicate_underscore(label_name),
                                                                 f_fline_item->f_iflag,
                                                                 f_fline_item->fline_ctl_file_name,
                                                                 window, vbox_v_fline);
    return expand_v_fline;
};

