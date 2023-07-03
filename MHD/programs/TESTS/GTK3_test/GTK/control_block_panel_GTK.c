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

static void add_block_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	struct void_clist *v_clist_gtk = (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	GtkWidget *vbox_out = (GtkWidget *) g_object_get_data(G_OBJECT(button), "vbox_out");
	
	printf("New number pre %d %d\n", c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent),
		   count_void_clist(v_clist_gtk));
	v_clist_gtk->index_bc = add_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view),
													c_append_sph_mntr_vspec_ctl, 
													(void *) init_f_sph_vol_spectr_ctls, 
													dealloc_f_sph_vol_spectr_ctls, 
													v_clist_gtk);
	printf("New number %d %d\n", c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent),
		   count_void_clist(v_clist_gtk));
	gtk_main_iteration();
	
};

static void delete_block_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	struct void_clist *v_clist_gtk = (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	GtkWidget *vbox_out = (GtkWidget *) g_object_get_data(G_OBJECT(button), "vbox_out");
	delete_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view), 
							   c_delete_sph_mntr_vspec_ctl, 
							   (void *) init_f_sph_vol_spectr_ctls, 
							   dealloc_f_sph_vol_spectr_ctls, 
							   v_clist_gtk);
	printf("New number %d %d\n", c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent),
		   count_void_clist(v_clist_gtk));
	gtk_widget_queue_draw(vbox_out);
};

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

static GtkWidget * draw_sph_each_vspec_ctl_vbox(struct f_sph_vol_spectr_ctls *f_v_pwr_item, GtkWidget *window){
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


GtkWidget * draw_sph_vol_spectr_ctl_vbox(struct void_clist *f_v_pwr, GtkWidget *window){
    GtkWidget *vbox_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	int i;
	GtkWidget *v_pwr_tree_view = NULL;
	
	
	
    GtkWidget *button_add =    gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add),    "v_clist_gtk",       (gpointer) v_clist_gtk);
	g_object_set_data(G_OBJECT(button_add),    "vbox_out",          (gpointer) vbox_out);
	g_object_set_data(G_OBJECT(button_delete), "v_clist_gtk",       (gpointer) v_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "vbox_out",          (gpointer) vbox_out);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_block_list_items_cb), (gpointer) v_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_block_list_items_cb), (gpointer) v_tree_view);
	
	
	
	GtkWidget *vbox_tbl = add_block_list_box_w_addbottun(f_v_pwr, v_pwr_tree_view, 
														 button_add, button_delete, vbox_out);
	gtk_box_pack_start(GTK_BOX(vbox_out), vbox_tbl,  FALSE, FALSE, 0);
	for(i=0;i<count_void_clist(f_v_pwr);i++){
		void *ctmp =  void_clist_label_at_index(i, (void *) f_v_pwr);
		void *v_pwr = void_clist_at_index(i, (void *) f_v_pwr);
		GtkWidget *vbox_z = draw_sph_each_vspec_ctl_vbox((struct f_sph_vol_spectr_ctls *) v_pwr, window);
		GtkWidget *expand_v_pwr = wrap_into_expanded_frame_gtk(duplicate_underscore(ctmp),
															   480, 480, window, vbox_z);
		gtk_box_pack_start(GTK_BOX(vbox_out), expand_v_pwr,  FALSE, FALSE, 0);
	}
   return vbox_out;
};
