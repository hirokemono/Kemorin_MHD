/*
//  control_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_block_panel_GTK.h"


static GtkWidget * draw_sph_each_vspec_ctl_expand(char *label_name, void *block_item, GtkWidget *window){
	struct f_sph_vol_spectr_ctls *f_v_pwr_item = (struct f_sph_vol_spectr_ctls *) block_item;
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
	GtkWidget *expand_v_pwr = draw_control_block(duplicate_underscore(label_name),
												 f_v_pwr_item->f_iflag,
												 480, 480, window, vbox_v_pwr);
    return expand_v_pwr;
};


static void draw_sph_vspec_controls_vbox(void *(*const_each_block_expander)(char *label_name, 
																			void *block_item,
																			GtkWidget *window),
										 struct void_clist *f_array_block, 
										 GtkWidget *vbox_array_block,
										 GtkWidget *window){
	int i;
	for(i=0;i<count_void_clist(f_array_block);i++){
		void *label_name = void_clist_label_at_index(i, (void *) f_array_block);
		void *block_item = void_clist_at_index(i, (void *) f_array_block);
		GtkWidget *expand_array_block = const_each_block_expander(label_name, block_item, window);
		gtk_box_pack_start(GTK_BOX(vbox_array_block), expand_array_block,
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
	void * (*c_append_item)(int *idx, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(button), "c_append_item");
	void * (*init_block_item)(int *idx, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(button), "init_block_item");
	void * (*dealloc_block_item)(void *f_item)
			= (void *) g_object_get_data(G_OBJECT(button), "dealloc_block_item");
	void * (*const_each_block_expander)(char *label_name, void *block_item, GtkWidget *window)
			= (void *) g_object_get_data(G_OBJECT(button), "const_each_block_exp");
	
	v_clist_gtk->index_bc = add_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view),
													c_append_item, 
													init_block_item, 
													dealloc_block_item, 
													v_clist_gtk);
	/*
	printf("New counts: %d %d \n",
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) ,
		   count_void_clist(v_clist_gtk));
	*/
	
	gtk_widget_destroy(vpwr_Widgets->vbox_vpwr_items);
    vpwr_Widgets->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_vspec_controls_vbox(const_each_block_expander, v_clist_gtk, 
								 vpwr_Widgets->vbox_vpwr_items, window);
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
	void * (*c_delete_item)(int *idx, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(button), "c_delete_item");
	void * (*init_block_item)(int *idx, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(button), "init_block_item");
	void * (*dealloc_block_item)(void *f_item)
			= (void *) g_object_get_data(G_OBJECT(button), "dealloc_block_item");
	void * (*const_each_block_expander)(char *label_name, void *block_item, GtkWidget *window)
			= (void *) g_object_get_data(G_OBJECT(button), "const_each_block_exp");
	
	printf("delete_void_list_items_GTK v_tree_view %p \n", v_tree_view);
	delete_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view), 
							   c_delete_item, 
							   init_block_item, 
							   dealloc_block_item, 
							   v_clist_gtk);
	/*
	printf("New counts: %d %d \n", 
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) , 
		   count_void_clist(v_clist_gtk));
	*/
	
	gtk_widget_destroy(vpwr_Widgets->vbox_vpwr_items);
    vpwr_Widgets->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_vspec_controls_vbox(const_each_block_expander, v_clist_gtk, 
								 vpwr_Widgets->vbox_vpwr_items, window);
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
	g_object_set_data(G_OBJECT(button_add), "c_append_item",
					  (gpointer) c_append_sph_mntr_vspec_ctl);
	g_object_set_data(G_OBJECT(button_add), "init_block_item",
					  (gpointer) init_f_sph_vol_spectr_ctls);
	g_object_set_data(G_OBJECT(button_add), "dealloc_block_item",
					  (gpointer) dealloc_f_sph_vol_spectr_ctls);
	g_object_set_data(G_OBJECT(button_add), "const_each_block_exp",
					  (gpointer) draw_sph_each_vspec_ctl_expand);
	
	g_object_set_data(G_OBJECT(button_delete), "window",       (gpointer) window);
	g_object_set_data(G_OBJECT(button_delete), "v_clist_gtk",  (gpointer) f_v_pwr);
	g_object_set_data(G_OBJECT(button_delete), "vpwr_Widgets", (gpointer) vpwr_Widgets);
	g_object_set_data(G_OBJECT(button_delete), "c_delete_item",
					  (gpointer) c_delete_sph_mntr_vspec_ctl);
	g_object_set_data(G_OBJECT(button_delete), "init_block_item",
					  (gpointer) init_f_sph_vol_spectr_ctls);
	g_object_set_data(G_OBJECT(button_delete), "dealloc_block_item",
					  (gpointer) dealloc_f_sph_vol_spectr_ctls);
	g_object_set_data(G_OBJECT(button_delete), "const_each_block_exp",
					  (gpointer) draw_sph_each_vspec_ctl_expand);
	
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
	
    vpwr_Widgets->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_vspec_controls_vbox(draw_sph_each_vspec_ctl_expand, f_v_pwr, 
								 vpwr_Widgets->vbox_vpwr_items, window);
	gtk_container_add(GTK_CONTAINER(vpwr_Widgets->vbox_vpwr), vpwr_Widgets->vbox_vpwr_items);
	
	int itmp = 1;
	GtkWidget *expand_vpwrs = draw_control_block(f_v_pwr->clist_name, &itmp,
												480, 440, window, vpwr_Widgets->vbox_vpwr);
	return expand_vpwrs;
};
