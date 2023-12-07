/*
//  tree_view_4_light_position.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_light_position.h"


struct lightparams_view * init_light_views_4_ctl(struct real3_clist *light_list){
	struct lightparams_view *light_vws = (struct lightparams_view *) malloc(sizeof(struct lightparams_view));
	light_vws->light_rtp_vws = (struct r3_clist_view *) malloc(sizeof(struct r3_clist_view));
	
    init_r3_clist_views(light_list, light_vws->light_rtp_vws);
    return light_vws;
}

struct lightparams_view * init_light_views_4_viewer(struct phong_lights *lights){
	int i, num;
	float r,t, p;

	struct lightparams_view *light_vws = (struct lightparams_view *) malloc(sizeof(struct lightparams_view));
	light_vws->light_rtp_vws = (struct r3_clist_view *) malloc(sizeof(struct r3_clist_view));
	light_vws->light_rtp_vws->r3_clist_gtk = init_real3_clist();
	
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->clist_name, "Light position");
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r1_name, "radius");
    sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r2_name, "elevation");
	sprintf(light_vws->light_rtp_vws->r3_clist_gtk->r3_name, "azimuth");
	
	light_vws->lights_gtk = lights;
	
	num = send_num_light_position(light_vws->lights_gtk);
	for(i=0;i<num;i++){
		send_each_light_rtp(light_vws->lights_gtk, i, &r, &t, &p);
		append_real3_clist((double) r, (double) t, (double) p, light_vws->light_rtp_vws->r3_clist_gtk);
	}
	return light_vws;
}

void dealloc_light_views_4_viewer(struct lightparams_view *light_vws){
	free(light_vws->lights_gtk);
	free(light_vws->light_rtp_vws);
	free(light_vws);
    return;
}

static void sync_phong_light_position_from_list(struct kemoviewer_type *kemo_sgl,
                                                struct lightparams_view *light_vws){
	double r, t, p;
	int i;
	int num = count_real3_clist(light_vws->light_rtp_vws->r3_clist_gtk);
	dealloc_phong_light_list(light_vws->lights_gtk);
	alloc_phong_light_list(light_vws->lights_gtk, num);
	for(i=0;i<num;i++){
		set_from_real3_clist_at_index(i, light_vws->light_rtp_vws->r3_clist_gtk,
					&r, &t, &p);
		kemoview_set_each_light_position(i, (float) r, (float) t, (float) p,
                                         kemo_sgl);
	};
	return;
};


void light_radius_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                            gchar *new_text, gpointer user_data){
	struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(cell), "kemoview");
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view));
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
	GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
	GtkTreeIter iter;
    double old_value1, old_value2, old_value3, new_value;
	
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
	/*
	double prev_value1 = 0.0;
	double next_value1 = 1.0e30;
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	if(gtk_tree_model_iter_previous(child_model, &iter)){
		gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &prev_value1, -1);
	};
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	if(gtk_tree_model_iter_next(child_model, &iter)){
		gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &next_value1, -1);
	};
	
	if(new_value < prev_value1){
		new_value = prev_value1;
	} else if(new_value > next_value1){
		new_value = next_value1;
	};
	*/
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
				new_value, old_value2, old_value3,
				light_vws->light_rtp_vws->r3_clist_gtk);
	
	sync_phong_light_position_from_list(kemo_sgl, light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void light_theta_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(cell), "kemoview");

    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view));
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
	GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
	GtkTreeIter iter;
    double old_value1, old_value2, old_value3, new_value;
	
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
	
	if(new_value < -90.0){
		new_value = -90.0;
	} else if(new_value > 90.0){
		new_value = 90.0;
	};
	
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
				old_value1, new_value, old_value3,
				light_vws->light_rtp_vws->r3_clist_gtk);
	
	sync_phong_light_position_from_list(kemo_sgl, light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void light_phi_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
	struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(cell), "kemoview");

    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view));
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
	GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
	GtkTreeIter iter;
    double old_value1, old_value2, old_value3, new_value;
	
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
	
	if(new_value < -180.0){
		new_value = -180.0;
	} else if(new_value > 360.0){
		new_value = 360.0;
	};
	
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_MATH, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
				old_value1, old_value2, new_value,
				light_vws->light_rtp_vws->r3_clist_gtk);
	
	sync_phong_light_position_from_list(kemo_sgl, light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

void add_lightposition_list_items_cb(GtkButton *button, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(button), "kemoview");

	light_vws->light_rtp_vws->index_bc
			= add_r3_list_items(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
								light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(kemo_sgl, light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};
void delete_lightposition_list_items_cb(GtkButton *button, gpointer user_data){
    struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(button), "kemoview");

	delete_r3_list_items(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk);
	sync_phong_light_position_from_list(kemo_sgl, light_vws);
	gtk_widget_queue_draw(light_vws->scrolled_window);
};

static void cursor_chenge_CB(GtkTreeView *tree_view, gpointer user_data){
	double org_value[3];
	struct lightparams_view *light_vws = (struct lightparams_view *) user_data;
	GList *reference_list = set_selected_r3_list_items(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				org_value);
	printf("Changed %lf, %lf, %lf\n",org_value[0], org_value[1], org_value[2]);
}

static GtkWidget *init_lightposition_list_box(struct kemoviewer_type *kemo_sgl,
                                              struct lightparams_view *light_vws){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkCellRenderer *renderer_spin3;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	light_vws->light_rtp_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	renderer_spin3 = gtk_cell_renderer_text_new();
	
	create_real3_tree_view(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view), 
                           light_vws->light_rtp_vws->r3_clist_gtk, 
                           renderer_spin1, renderer_spin2, renderer_spin3);
    /*
	g_signal_connect(G_OBJECT(light_vws->light_rtp_vws->tree_view), "cursor-changed", 
					 G_CALLBACK(cursor_chenge_CB), (gpointer) light_vws);
    */
	
    g_object_set_data(G_OBJECT(renderer_spin1), "kemoview",  (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(renderer_spin2), "kemoview",  (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(renderer_spin3), "kemoview",  (gpointer) kemo_sgl);

    g_signal_connect(G_OBJECT(renderer_spin1), "edited",
                     G_CALLBACK(light_radius_edited_cb), (gpointer) light_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited",
                     G_CALLBACK(light_theta_edited_cb), (gpointer) light_vws);
	g_signal_connect(G_OBJECT(renderer_spin3), "edited",
                     G_CALLBACK(light_phi_edited_cb), (gpointer) light_vws);
	
	light_vws->light_rtp_vws->index_bc
			= append_r3_list_from_ctl(light_vws->light_rtp_vws->index_bc,
				light_vws->light_rtp_vws->r3_clist_gtk, 
				GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view));
	
	button_add = gtk_button_new_with_label("Add");
    button_delete = gtk_button_new_with_label("Remove");
	
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_real3_list_box(GTK_TREE_VIEW(light_vws->light_rtp_vws->tree_view),
				light_vws->light_rtp_vws->r3_clist_gtk,
				button_add, button_delete, vbox);
	
    g_object_set_data(G_OBJECT(button_add),    "kemoview",  (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(button_delete), "kemoview",  (gpointer) kemo_sgl);

    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(add_lightposition_list_items_cb), (gpointer) light_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_lightposition_list_items_cb), (gpointer) light_vws);
    
    return vbox;
};

GtkWidget * init_light_list_frame(struct kemoviewer_type *kemo_sgl,
                                  struct lightparams_view *light_vws){
    light_vws->light_vbox = init_lightposition_list_box(kemo_sgl, light_vws);
	
	light_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(light_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(light_vws->scrolled_window, 160, 200);
    gtk_widget_set_app_paintable(light_vws->scrolled_window, TRUE);
    gtk_widget_add_events (light_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
    gtk_container_add(GTK_CONTAINER(light_vws->scrolled_window), light_vws->light_vbox);
	
    
    GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), light_vws->scrolled_window);
    
    return Frame_1;
};


