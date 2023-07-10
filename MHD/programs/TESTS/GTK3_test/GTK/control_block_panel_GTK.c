/*
//  control_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_block_panel_GTK.h"


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


static void draw_sph_vspec_controls_vbox(struct void_clist *f_v_pwr, 
										 struct sph_vspectr_widgets *vpwr_Widgets,
								  GtkWidget *window){
    vpwr_Widgets->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	int i;
	
	for(i=0;i<count_void_clist(f_v_pwr);i++){
		void *ctmp =  void_clist_label_at_index(i, (void *) f_v_pwr);
		struct f_sph_vol_spectr_ctls *v_pwr 
				= (struct f_sph_vol_spectr_ctls *) void_clist_at_index(i, (void *) f_v_pwr);
		GtkWidget *vbox_z = draw_sph_each_vspec_ctl_vbox(v_pwr, window);
		GtkWidget *expand_v_pwr = draw_control_block(duplicate_underscore(ctmp), v_pwr->f_iflag,
													 480, 480, window, vbox_z);
		gtk_box_pack_start(GTK_BOX(vpwr_Widgets->vbox_vpwr_items), expand_v_pwr,
						   FALSE, FALSE, 0);
	}
   return;
};

static void add_vol_spectr_ctl_block_cb(GtkButton *button, gpointer user_data){
	GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(button), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	struct sph_vspectr_widgets *vpwr_Widgets 
			= (struct sph_vspectr_widgets *) g_object_get_data(G_OBJECT(button), "vpwr_Widgets");
	
	printf("New number pre %d %d\n", c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent),
		   count_void_clist(v_clist_gtk));
	v_clist_gtk->index_bc = add_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view),
													c_append_sph_mntr_vspec_ctl, 
													(void *) init_f_sph_vol_spectr_ctls, 
													dealloc_f_sph_vol_spectr_ctls, 
													v_clist_gtk);
	/*
	printf("New counts: %d %d \n",
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) ,
		   count_void_clist(v_clist_gtk));
	*/
	
	gtk_widget_destroy(vpwr_Widgets->vbox_vpwr_items);
    draw_sph_vspec_controls_vbox(v_clist_gtk, vpwr_Widgets, window);
	gtk_container_remove(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	gtk_container_add(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	gtk_widget_show_all(window);
};

static void delete_vol_spectr_ctl_block_cb(GtkButton *button, gpointer user_data){
    GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(button), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(button), "v_clist_gtk");
	struct sph_vspectr_widgets *vpwr_Widgets
			= (struct sph_vspectr_widgets *) g_object_get_data(G_OBJECT(button), "vpwr_Widgets");
	
	printf("delete_void_list_items_GTK v_tree_view %p \n", v_tree_view);
	delete_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view), 
							   c_delete_sph_mntr_vspec_ctl, 
							   (void *) init_f_sph_vol_spectr_ctls, 
							   dealloc_f_sph_vol_spectr_ctls, 
							   v_clist_gtk);
	/*
	printf("New counts: %d %d \n", 
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) , 
		   count_void_clist(v_clist_gtk));
	*/
	gtk_widget_destroy(vpwr_Widgets->vbox_vpwr_items);
    draw_sph_vspec_controls_vbox(v_clist_gtk, vpwr_Widgets, window);
	gtk_container_remove(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	gtk_container_add(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	gtk_widget_show_all(window);
};


GtkWidget * draw_sph_vol_spectr_ctl_vbox(struct void_clist *f_v_pwr, 
										 struct sph_vspectr_widgets *vpwr_Widgets,
										 GtkWidget *window){
	vpwr_Widgets = (struct sph_vspectr_widgets *) malloc(sizeof(struct sph_vspectr_widgets));
	if(vpwr_Widgets == NULL){
		printf("malloc error for vpwr_Widgets\n");
		exit(0);
	};
	
    vpwr_Widgets->vbox_vpwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *button_add =    gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add),    "window",       (gpointer) window);
	g_object_set_data(G_OBJECT(button_add),    "v_clist_gtk",  (gpointer) f_v_pwr);
	g_object_set_data(G_OBJECT(button_add),    "vpwr_Widgets", (gpointer) vpwr_Widgets);
	g_object_set_data(G_OBJECT(button_delete), "window",       (gpointer) window);
	g_object_set_data(G_OBJECT(button_delete), "v_clist_gtk",  (gpointer) f_v_pwr);
	g_object_set_data(G_OBJECT(button_delete), "vpwr_Widgets", (gpointer) vpwr_Widgets);
	
	vpwr_Widgets->v_pwr_tree_view = gtk_tree_view_new();
	GtkWidget *vbox_tbl = add_block_list_box_w_addbottun(f_v_pwr, vpwr_Widgets->v_pwr_tree_view, 
														 button_add, button_delete, 
														 vpwr_Widgets->vbox_vpwr);

    g_signal_connect(G_OBJECT(button_add), "clicked",
					 G_CALLBACK(add_vol_spectr_ctl_block_cb), 
					 (gpointer) vpwr_Widgets->v_pwr_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
					 G_CALLBACK(delete_vol_spectr_ctl_block_cb), 
					 (gpointer) vpwr_Widgets->v_pwr_tree_view);
	gtk_box_pack_start(GTK_BOX(vpwr_Widgets->vbox_vpwr), vbox_tbl,  FALSE, FALSE, 0);
	
	draw_sph_vspec_controls_vbox(f_v_pwr, vpwr_Widgets, window);
	gtk_container_add(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	
	int itmp = 1;
	GtkWidget *expand_vpwrs = draw_control_block(f_v_pwr->clist_name, &itmp,
												480, 440, window, vpwr_Widgets->vbox_vpwr);
	return expand_vpwrs;
};
