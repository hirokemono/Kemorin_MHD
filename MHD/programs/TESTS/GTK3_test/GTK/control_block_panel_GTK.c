/*
//  control_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_block_panel_GTK.h"

struct f_sph_vol_spectr_ctls * init_f_sph_vol_spectr_ctls(int idx, void *f_parent)
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
	
	f_v_pwr_item->f_volume_spec_file_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_file_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_ave_file_ctl =  init_f_ctl_chara_item(c_sph_volume_ave_file_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_volume_spec_format_ctl =  init_f_ctl_chara_item(c_sph_volume_spec_format_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_degree_v_spectra_switch =  init_f_ctl_chara_item(c_sph_degree_v_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_order_v_spectra_switch =  init_f_ctl_chara_item(c_sph_order_v_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_diff_v_lm_spectra_switch =  init_f_ctl_chara_item(c_sph_diff_v_lm_spectra_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_axis_v_power_switch =  init_f_ctl_chara_item(c_sph_axis_v_power_switch, f_v_pwr_item->f_self);
	f_v_pwr_item->f_inner_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_inner_radius_ctl, f_v_pwr_item->f_self);
	f_v_pwr_item->f_outer_radius_ctl =  init_f_ctl_real_item(c_sph_v_spec_outer_radius_ctl, f_v_pwr_item->f_self);
	return f_v_pwr_item;
}

void * dealloc_f_sph_vol_spectr_ctls(void *f_item){
	struct f_sph_vol_spectr_ctls *f_v_pwr_item = (struct f_sph_vol_spectr_ctls *) f_item;
	f_v_pwr_item->f_self = NULL;
	f_v_pwr_item->f_iflag = NULL;
	free(f_v_pwr_item->c_block_name);
	
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_volume_spec_file_ctl);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_volume_ave_file_ctl);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_volume_spec_format_ctl);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_degree_v_spectra_switch);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_order_v_spectra_switch);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_diff_v_lm_spectra_switch);
	dealloc_f_ctl_chara_item(f_v_pwr_item->f_axis_v_power_switch);
	dealloc_f_ctl_real_item(f_v_pwr_item->f_inner_radius_ctl);
	dealloc_f_ctl_real_item(f_v_pwr_item->f_outer_radius_ctl);
	free(f_v_pwr_item);
	return NULL;
}

GtkWidget * add_block_list_box_w_addbottun(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out){
	printf("f_parent in add_block_list_box_w_addbottun %p\n", v_clist_gtk->f_parent);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	v_tree_view = gtk_tree_view_new();
	
	GtkCellRenderer *renderer_text =  gtk_cell_renderer_text_new();
	create_block_tree_view(GTK_TREE_VIEW(v_tree_view), renderer_text);
	v_clist_gtk->index_bc = append_void_list_from_ctl(v_clist_gtk->index_bc, &v_clist_gtk->c_item_head, 
													  GTK_TREE_VIEW(v_tree_view));
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox0), button_add, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), button_delete, FALSE, TRUE, 0);
	
	GtkWidget *expander = block_list_box_expander(v_clist_gtk->clist_name, GTK_TREE_VIEW(v_tree_view), 
												  button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), expander, FALSE, TRUE, 0);
	return vbox;
};

GtkWidget * draw_sph_each_vspec_ctl_vbox(struct f_sph_vol_spectr_ctls *f_v_pwr_item, GtkWidget *window){
	GtkWidget *vbox_v_pwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    GtkWidget *hbox_1 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_ave_file_ctl);
    GtkWidget *hbox_2 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_file_ctl);
    GtkWidget *hbox_3 = draw_chara_item_entry_hbox(f_v_pwr_item->f_volume_spec_format_ctl);
    GtkWidget *hbox_4 = draw_real_item_entry_hbox(f_v_pwr_item->f_inner_radius_ctl);
    GtkWidget *hbox_5 = draw_real_item_entry_hbox(f_v_pwr_item->f_outer_radius_ctl);
    GtkWidget *hbox_6 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_degree_v_spectra_switch);
    GtkWidget *hbox_7 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_order_v_spectra_switch);
    GtkWidget *hbox_8 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_diff_v_lm_spectra_switch);
    GtkWidget *hbox_9 = draw_chara_switch_entry_hbox(f_v_pwr_item->f_axis_v_power_switch);
	
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_1,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_2,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_3,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_4,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_5,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_6,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_7,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_8,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_v_pwr), hbox_9,  FALSE, FALSE, 0);
    return vbox_v_pwr;
};

