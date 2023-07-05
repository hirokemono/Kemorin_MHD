/*
//  sph_data_on_circles_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "sph_data_on_circles_block_panel_GTK.h"


static GtkWidget * draw_sph_each_fld_on_circle_ctl_vbox(struct f_sph_field_on_circle_ctls *f_circ_ctls, GtkWidget *window){
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

static void draw_sph_d_circle_ctls_vbox(struct void_clist *f_circ_ctls, 
										struct sph_d_circle_widgets *dcirc_Widgets,
										GtkWidget *window){
    dcirc_Widgets->vbox_dcirc_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	int i;
	
	dcirc_Widgets->expand_dcirc_list = init_void_clist("V_spec_expander_list");
	for(i=0;i<count_void_clist(f_circ_ctls);i++){
		void *ctmp =  void_clist_label_at_index(i, (void *) f_circ_ctls);
		struct f_sph_field_on_circle_ctls *d_circ = (struct f_sph_field_on_circle_ctls *) void_clist_at_index(i, (void *) f_circ_ctls);
		GtkWidget *vbox_z = draw_sph_each_fld_on_circle_ctl_vbox(d_circ, window);
		GtkWidget *expand_v_pwr = draw_control_block(duplicate_underscore(ctmp), d_circ->f_iflag,
													 480, 480, window, vbox_z);
		append_void_clist((void *) expand_v_pwr, dcirc_Widgets->expand_dcirc_list);
		gtk_box_pack_start(GTK_BOX(dcirc_Widgets->vbox_dcirc_items), expand_v_pwr,  FALSE, FALSE, 0);
	}
   return;
};


static void add_d_circle_ctl_block_cb(GtkButton *button, gpointer user_data){
	GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(button), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	struct sph_d_circle_widgets *dcirc_Widgets
			= (struct sph_d_circle_widgets *) g_object_get_data(G_OBJECT(button), "dcirc_Widgets");
	
	printf("New number pre %d %d\n", c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent),
		   count_void_clist(v_clist_gtk));
	v_clist_gtk->index_bc = add_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view),
													c_append_circles_meq_ctl, 
													(void *) init_f_sph_field_on_circle_ctls, 
													dealloc_f_sph_field_on_circle_ctls, 
													v_clist_gtk);
	printf("New counts: %d %d \n",
		   c_data_on_circles_num(v_clist_gtk->f_parent) ,
		   count_void_clist(v_clist_gtk));
	
	
	gtk_widget_destroy(dcirc_Widgets->vbox_dcirc_items);
    draw_sph_d_circle_ctls_vbox(v_clist_gtk, dcirc_Widgets, window);
	gtk_container_remove(GTK_CONTAINER(dcirc_Widgets->vbox_dcirc), dcirc_Widgets->vbox_dcirc_items);
	gtk_container_add(GTK_CONTAINER(dcirc_Widgets->vbox_dcirc), dcirc_Widgets->vbox_dcirc_items);
	gtk_widget_show_all(window);
};

static void delete_d_circle_ctl_block_cb(GtkButton *button, gpointer user_data){
    GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(button), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	struct sph_d_circle_widgets *dcirc_Widgets
			= (struct sph_d_circle_widgets *) g_object_get_data(G_OBJECT(button), "dcirc_Widgets");
	
	printf("delete_void_list_items_GTK v_tree_view %p \n", v_tree_view);
	delete_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view), 
							   c_delete_circles_meq_ctl, 
							   (void *) init_f_sph_field_on_circle_ctls, 
							   dealloc_f_sph_field_on_circle_ctls, 
							   v_clist_gtk);
	printf("New counts: %d %d \n", 
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) , 
		   count_void_clist(v_clist_gtk));
	
	gtk_widget_destroy(dcirc_Widgets->vbox_dcirc_items);
    draw_sph_d_circle_ctls_vbox(v_clist_gtk, dcirc_Widgets, window);
	gtk_container_remove(GTK_CONTAINER(dcirc_Widgets->vbox_dcirc), dcirc_Widgets->vbox_dcirc_items);
	gtk_container_add(GTK_CONTAINER(dcirc_Widgets->vbox_dcirc), dcirc_Widgets->vbox_dcirc_items);
	gtk_widget_show_all(window);
};


GtkWidget * draw_sph_d_circle_ctl_vbox(struct void_clist *f_circ_ctls,
									   struct sph_d_circle_widgets *dcirc_Widgets,
									   GtkWidget *window){
    dcirc_Widgets->vbox_dcirc = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *button_add =    gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add),    "window",        (gpointer) window);
	g_object_set_data(G_OBJECT(button_add),    "v_clist_gtk",   (gpointer) f_circ_ctls);
	g_object_set_data(G_OBJECT(button_add),    "dcirc_Widgets", (gpointer) dcirc_Widgets);
	g_object_set_data(G_OBJECT(button_delete), "window",        (gpointer) window);
	g_object_set_data(G_OBJECT(button_delete), "v_clist_gtk",   (gpointer) f_circ_ctls);
	g_object_set_data(G_OBJECT(button_delete), "dcirc_Widgets", (gpointer) dcirc_Widgets);
	
	dcirc_Widgets->d_circ_tree_view = gtk_tree_view_new();
	GtkWidget *vbox_tbl = add_block_list_box_w_addbottun(f_circ_ctls, dcirc_Widgets->d_circ_tree_view, 
													 button_add, button_delete, dcirc_Widgets->vbox_dcirc);
	
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(add_d_circle_ctl_block_cb), (gpointer) dcirc_Widgets->d_circ_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
					 G_CALLBACK(delete_d_circle_ctl_block_cb), (gpointer) dcirc_Widgets->d_circ_tree_view);
	gtk_box_pack_start(GTK_BOX(dcirc_Widgets->vbox_dcirc), vbox_tbl,  FALSE, FALSE, 0);
	
	draw_sph_d_circle_ctls_vbox(f_circ_ctls, dcirc_Widgets, window);
	gtk_container_add(GTK_CONTAINER(dcirc_Widgets->vbox_dcirc), dcirc_Widgets->vbox_dcirc_items);
	
	int itmp = 1;
	GtkWidget *expand_dcirc = draw_control_block(f_circ_ctls->clist_name, &itmp,
												480, 440, window, dcirc_Widgets->vbox_dcirc);
	return expand_dcirc;
};

