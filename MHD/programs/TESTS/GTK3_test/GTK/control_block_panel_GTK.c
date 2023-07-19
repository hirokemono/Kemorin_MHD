/*
//  control_block_panel_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_block_panel_GTK.h"

static void draw_sph_array_block_ctls_vbox(void *(*const_each_block_expander)(char *label_name, 
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

static void add_array_block_ctl_cb(GtkButton *button, gpointer user_data){
	GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(user_data), "v_clist_gtk");
    void *void_in_gtk = (void *) g_object_get_data(G_OBJECT(user_data), "void_in_gtk");
	struct block_array_widgets *array_block_Wgts
			= (struct block_array_widgets *) g_object_get_data(G_OBJECT(user_data), "array_block_Wgts");
	void * (*append_ctl_block_F)(int idx, char *block_name, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(user_data), "append_ctl_block_F");
	void * (*init_block_item)(int idx, void *f_parent, void *void_in_gtk)
			= (void *) g_object_get_data(G_OBJECT(user_data), "init_block_item");
	void * (*dealloc_block_item)(void *f_item)
			= (void *) g_object_get_data(G_OBJECT(user_data), "dealloc_block_item");
	void * (*const_each_block_expander)(char *label_name, void *block_item, GtkWidget *window)
			= (void *) g_object_get_data(G_OBJECT(user_data), "const_each_block_exp");
	
	v_clist_gtk->index_bc = add_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view),
													append_ctl_block_F, 
													init_block_item, 
													dealloc_block_item, 
                                                    void_in_gtk, v_clist_gtk);
	/*
	printf("New counts: %d %d \n",
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) ,
		   count_void_clist(v_clist_gtk));
	*/
	
	gtk_widget_destroy(array_block_Wgts->vbox_vpwr_items);
    array_block_Wgts->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_array_block_ctls_vbox(const_each_block_expander, v_clist_gtk, 
								   array_block_Wgts->vbox_vpwr_items, window);
	gtk_container_remove(GTK_CONTAINER(array_block_Wgts->vbox_vpwr), array_block_Wgts->vbox_vpwr_items);
	gtk_container_add(GTK_CONTAINER(array_block_Wgts->vbox_vpwr), array_block_Wgts->vbox_vpwr_items);
	gtk_widget_show_all(window);
};

static void delete_array_block_ctl_cb(GtkButton *button, gpointer user_data){
    GtkWidget *v_tree_view = GTK_WIDGET(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "window"));
	struct void_clist *v_clist_gtk 
			= (struct void_clist *) g_object_get_data(G_OBJECT(user_data), "v_clist_gtk");
    void *void_in_gtk = (void *) g_object_get_data(G_OBJECT(user_data), "void_in_gtk");
	struct block_array_widgets *array_block_Wgts
			= (struct block_array_widgets *) g_object_get_data(G_OBJECT(user_data), "array_block_Wgts");
	void * (*delete_ctl_block_F)(int idx, void *f_parent)
			= (void *) g_object_get_data(G_OBJECT(user_data), "delete_ctl_block_F");
	void * (*init_block_item)(int idx, void *f_parent, void *void_in_gtk)
			= (void *) g_object_get_data(G_OBJECT(user_data), "init_block_item");
	void * (*dealloc_block_item)(void *f_item)
			= (void *) g_object_get_data(G_OBJECT(user_data), "dealloc_block_item");
	void * (*const_each_block_expander)(char *label_name, void *block_item, GtkWidget *window)
			= (void *) g_object_get_data(G_OBJECT(user_data), "const_each_block_exp");
	
	printf("delete_void_list_items_GTK v_tree_view %p \n", v_tree_view);
	delete_void_list_items_GTK(GTK_TREE_VIEW(v_tree_view), 
							   delete_ctl_block_F, 
							   init_block_item, 
							   dealloc_block_item, 
							   void_in_gtk, v_clist_gtk);
	/*
	printf("New counts: %d %d \n", 
		   c_sph_monitor_num_vspec_ctl(v_clist_gtk->f_parent) , 
		   count_void_clist(v_clist_gtk));
	*/
	
	gtk_widget_destroy(array_block_Wgts->vbox_vpwr_items);
    array_block_Wgts->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_array_block_ctls_vbox(const_each_block_expander, v_clist_gtk, 
								   array_block_Wgts->vbox_vpwr_items, window);
	gtk_container_remove(GTK_CONTAINER(array_block_Wgts->vbox_vpwr), array_block_Wgts->vbox_vpwr_items);
	gtk_container_add(GTK_CONTAINER(array_block_Wgts->vbox_vpwr), array_block_Wgts->vbox_vpwr_items);
	gtk_widget_show_all(window);
};


GtkWidget * draw_array_block_ctl_vbox(struct void_clist *v_clist, void *void_in_gtk,
                                      void *(*append_ctl_block_F)(int idx, char *block_name, void *f_parent),
                                      void *(*delete_ctl_block_F)(int idx, void *f_parent),
                                      void *(*init_block_item)(int idx, void *f_parent, void *void_in_gtk),
                                      void *(*dealloc_block_item)(void *f_item),
                                      void *(*const_each_block_expander)(char *label_name,
                                                                         void *block_item,
                                                                         GtkWidget *window),
                                      struct block_array_widgets *array_block_Wgts,
                                      GtkWidget *window){
	array_block_Wgts = (struct block_array_widgets *) malloc(sizeof(struct block_array_widgets));
	if(array_block_Wgts == NULL){
		printf("malloc error for array_block_Wgts\n");
		exit(0);
	};
	
    array_block_Wgts->vbox_vpwr = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	GtkWidget *button_add =    gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	array_block_Wgts->v_pwr_tree_view = gtk_tree_view_new();
	GtkWidget *vbox_tbl = add_block_list_box_w_addbottun(v_clist, array_block_Wgts->v_pwr_tree_view,
														 button_add, button_delete, 
														 array_block_Wgts->vbox_vpwr);
	
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "window",       (gpointer) window);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "v_clist_gtk",  (gpointer) v_clist);
    g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "void_in_gtk",  (gpointer) void_in_gtk);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "array_block_Wgts",
					  (gpointer) array_block_Wgts);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "append_ctl_block_F", 
					  (gpointer) append_ctl_block_F);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "delete_ctl_block_F", 
					  (gpointer) delete_ctl_block_F);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "init_block_item", 
					  (gpointer) init_block_item);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "dealloc_block_item", 
					  (gpointer) dealloc_block_item);
	g_object_set_data(G_OBJECT(array_block_Wgts->v_pwr_tree_view), "const_each_block_exp",
					  (gpointer) const_each_block_expander);
	
    g_signal_connect(G_OBJECT(button_add), "clicked",
					 G_CALLBACK(add_array_block_ctl_cb), 
					 (gpointer) array_block_Wgts->v_pwr_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
					 G_CALLBACK(delete_array_block_ctl_cb), 
					 (gpointer) array_block_Wgts->v_pwr_tree_view);
	gtk_box_pack_start(GTK_BOX(array_block_Wgts->vbox_vpwr), vbox_tbl,  FALSE, FALSE, 0);
	
    array_block_Wgts->vbox_vpwr_items = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	draw_sph_array_block_ctls_vbox(const_each_block_expander, v_clist,
								   array_block_Wgts->vbox_vpwr_items, window);
	gtk_container_add(GTK_CONTAINER(array_block_Wgts->vbox_vpwr), array_block_Wgts->vbox_vpwr_items);
	
	int itmp = 1;
	GtkWidget *expand_vpwrs = draw_control_block(v_clist->clist_name, &itmp,
												 window, array_block_Wgts->vbox_vpwr);
	return expand_vpwrs;
};
