/*
//  sph_data_on_circles_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "sph_data_on_circles_block_panel_GTK.h"

struct f_sph_field_on_circle_ctls * init_f_sph_field_on_circle_ctls(int idx, void *f_parent)
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
	
	f_circ_ctls->f_circle_field_file_ctl =  init_f_ctl_chara_item(c_data_on_circle_field_file, f_circ_ctls->f_self);
	f_circ_ctls->f_circle_spectr_file_ctl =  init_f_ctl_chara_item(c_data_on_circle_spectr_file, f_circ_ctls->f_self);
	f_circ_ctls->f_circle_file_format_ctl =  init_f_ctl_chara_item(c_data_on_circle_file_fmt_ctl, f_circ_ctls->f_self);
	f_circ_ctls->f_pick_circle_coord_ctl =  init_f_ctl_chara_item(c_data_on_circle_coord_ctl, f_circ_ctls->f_self);
	
	f_circ_ctls->f_nphi_mid_eq_ctl = init_f_ctl_int_item(c_data_on_circle_nphi_ctl, f_circ_ctls->f_self);
	f_circ_ctls->f_pick_s_ctl =  init_f_ctl_real_item(c_data_on_circle_pick_s_ctl, f_circ_ctls->f_self);
	f_circ_ctls->f_pick_z_ctl =  init_f_ctl_real_item(c_data_on_circle_pick_z_ctl, f_circ_ctls->f_self);
	return f_circ_ctls;
}

void * dealloc_f_sph_field_on_circle_ctls(void *f_item){
	struct f_sph_field_on_circle_ctls *f_circ_ctls = (struct f_sph_field_on_circle_ctls *) f_item;
	f_circ_ctls->f_self = NULL;
	f_circ_ctls->f_iflag = NULL;
	free(f_circ_ctls->c_block_name);
	
	dealloc_f_ctl_chara_item(f_circ_ctls->f_circle_field_file_ctl);
	dealloc_f_ctl_chara_item(f_circ_ctls->f_circle_spectr_file_ctl);
	dealloc_f_ctl_chara_item(f_circ_ctls->f_circle_file_format_ctl);
	dealloc_f_ctl_chara_item(f_circ_ctls->f_pick_circle_coord_ctl);
	dealloc_f_ctl_int_item(f_circ_ctls->f_nphi_mid_eq_ctl);
	dealloc_f_ctl_real_item(f_circ_ctls->f_pick_s_ctl);
	dealloc_f_ctl_real_item(f_circ_ctls->f_pick_z_ctl);
	free(f_circ_ctls);
	return NULL;
}

GtkWidget * add_fld_on_circle_ctl_vbox(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	GtkCellRenderer *renderer_text =  gtk_cell_renderer_text_new();
	create_block_tree_view(GTK_TREE_VIEW(v_tree_view), renderer_text);
	v_clist_gtk->index_bc = append_void_list_from_ctl(v_clist_gtk->index_bc, &v_clist_gtk->c_item_head,
													  GTK_TREE_VIEW(v_tree_view));

	GtkWidget *expander = block_list_box_expander(v_clist_gtk->clist_name, GTK_TREE_VIEW(v_tree_view),
												  button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(vbox), expander, FALSE, TRUE, 0);
	return vbox;
};

GtkWidget * draw_sph_each_fld_on_circle_ctl_vbox(struct f_sph_field_on_circle_ctls *f_circ_ctls, GtkWidget *window){
	GtkWidget *vbox_dcirc = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_field_file_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_spectr_file_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_circ_ctls->f_circle_file_format_ctl);
    GtkWidget *hbox_4 = draw_chara_item_entry_hbox(f_circ_ctls->f_pick_circle_coord_ctl);
    GtkWidget *hbox_5 = draw_int_item_entry_hbox(f_circ_ctls->f_nphi_mid_eq_ctl);
    GtkWidget *hbox_6 = draw_real_item_entry_hbox(f_circ_ctls->f_pick_s_ctl);
    GtkWidget *hbox_7 = draw_real_item_entry_hbox(f_circ_ctls->f_pick_z_ctl);
	
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_dcirc), hbox_7,  FALSE, FALSE, 0);
    return vbox_dcirc;
};

