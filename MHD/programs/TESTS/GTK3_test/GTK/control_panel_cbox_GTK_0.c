/*
//  control_panel_cbox_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_cbox_GTK.h"

extern int lengthchara_f(void);

extern void * c_chara_item_clength(void *f_ctl, int *length);

struct chara_cbox_table_view * init_chara_cbox_table_view(struct chara_clist *ctl_clist,
                                                          struct chara_clist *item_clist){
	struct chara_cbox_table_view *chara_tbl_vws  
			= (struct chara_cbox_table_view *) malloc(sizeof(struct chara_cbox_table_view));
	if(chara_tbl_vws == NULL){
		printf("malloc error for chara_cbox_table_view\n");
		exit(0);
	};
	
    chara_tbl_vws->ctl_clist_gtk = ctl_clist;
    chara_tbl_vws->clist_tree_view = gtk_tree_view_new();
    chara_tbl_vws->items_tree_view = create_fixed_label_tree(item_clist);
    return chara_tbl_vws;
}

static void copy_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	int i;
	for(i=0;i<count_chara_clist(c_clist);i++){
        struct chara_ctl_item *tmp_item = chara_clist_at_index(i, c_clist);
		c_store_chara_array(c_clist->f_self, i, tmp_item->c_tbl);
	}
    return;
}

static void update_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	if(c_clist->f_self == NULL) return;
	copy_f_ctl_c_array_by_r_list(c_clist);
    return;
}
static void chara_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(cell), "c_clist_gtk");
    c_tree_name_edited(path_str, new_text, GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	update_f_ctl_c_array_by_r_list(c_clist_gtk);
};

void create_cbox_tree_view(GtkTreeView *items_tree_view, GtkTreeView *c_tree_view,
                           GtkCellRenderer *renderer_cbox)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	
    GtkTreeModel *model;
    GtkListStore *child_model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    

	/* Construct empty list storage */
    GtkTreeModel *cbox_child_model = gtk_tree_view_get_model(GTK_TREE_VIEW(items_tree_view));
    child_model = gtk_list_store_new(2, G_TYPE_INT, G_TYPE_STRING);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);

    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(c_tree_view), model);
    
    /* First raw */
	column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(c_tree_view, column);
    gtk_tree_view_column_set_title(column, "Text entry");
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set(G_OBJECT(renderer_cbox), 
                 "text-column", FALSE, 
                 "editable", TRUE, 
                 "model", cbox_child_model,
                 "has-entry", TRUE, 
                 "width", (gint)150, NULL);
    gtk_tree_view_column_pack_start(column, renderer_cbox, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    gtk_tree_view_column_set_attributes(column, renderer_cbox, "text", COLUMN_FIELD_NAME, NULL);
	/*
    g_signal_connect(G_OBJECT(column), "clicked",
					 G_CALLBACK(column_clicked), (gpointer) c_tree_view);
	*/
	
    /* 選択モード */
    selection = gtk_tree_view_get_selection(c_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(c_tree_view, COLUMN_FIELD_NAME);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_NAME, GTK_SORT_ASCENDING);
}

static void init_c_tree_view2(struct chara_clist *c_clist_gtk,
                              GtkWidget *items_tree_view, GtkWidget *c_tree_view){
    GtkCellRenderer *renderer_cbox = gtk_cell_renderer_combo_new();
    g_object_set_data(G_OBJECT(renderer_cbox), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	g_signal_connect(G_OBJECT(renderer_cbox), "edited", 
					 G_CALLBACK(chara_tree_value1_edited_cb), (gpointer) c_tree_view);
	
	create_cbox_tree_view(GTK_TREE_VIEW(items_tree_view), GTK_TREE_VIEW(c_tree_view), renderer_cbox);
	
	c_clist_gtk->index_bc = append_c_list_from_ctl_w_index(c_clist_gtk->index_bc, &c_clist_gtk->c_item_head,
												 GTK_TREE_VIEW(c_tree_view));
};

void cb_check_c_array_toggle2(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(widget), "c_clist_gtk");
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_chara_array(count_chara_clist(c_clist_gtk), c_clist_gtk);
		copy_f_ctl_c_array_by_r_list(c_clist_gtk);
	}else{
		reflesh_f_ctl_chara_array(0, c_clist_gtk);
	};
	return;
}

GtkWidget *hbox_with_c_array_checkbox2(struct chara_clist *c_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	if(count_chara_clist(c_clist_gtk) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_c_array_toggle2), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}

static void reflesh_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	if(c_clist->f_self == NULL) return;
	int num_array = count_chara_clist(c_clist);
	reflesh_f_ctl_chara_array(num_array, c_clist);
	copy_f_ctl_c_array_by_r_list(c_clist);
    return;
}
static void add_c_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(button), "c_clist_gtk");
	c_clist_gtk->index_bc = add_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	reflesh_f_ctl_c_array_by_r_list(c_clist_gtk);
};
static void delete_c_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(button), "c_clist_gtk");
	delete_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	reflesh_f_ctl_c_array_by_r_list(c_clist_gtk);
};

GtkWidget * add_c_list_combobox(struct chara_clist *c_clist_gtk,
                                struct chara_clist *input_list, 
                                struct chara_cbox_table_view *time_evo_vws){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    
    time_evo_vws = init_chara_cbox_table_view(c_clist_gtk, input_list);
	time_evo_vws->clist_tree_view = gtk_tree_view_new();
	init_c_tree_view2(c_clist_gtk, time_evo_vws->items_tree_view, time_evo_vws->clist_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "c_clist_gtk", (gpointer) c_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	GtkWidget * hbox1 = hbox_with_c_array_checkbox2(c_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	
	GtkWidget *expander = chara_list_box_expander(c_clist_gtk->clist_name, GTK_TREE_VIEW(time_evo_vws->clist_tree_view), 
												  button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_c_list_items_cb), (gpointer) time_evo_vws->clist_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_c_list_items_cb), (gpointer) time_evo_vws->clist_tree_view);
	return vbox;
};




GtkWidget * c_list_combobox_expander(struct chara_clist *c_clist_gtk, 
                                     struct chara_clist *input_list, 
                                     GtkWidget *clist_tree_view,
                                     GtkWidget *items_tree_view,
                                     GtkWidget *window){
    clist_tree_view = gtk_tree_view_new();
    items_tree_view = create_fixed_label_tree(items_tree_view);
    
//    init_boundary_condition_tree_view2(bc_vws);
    
    GtkWidget *vbox_m3t = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
//    add_bc_temp_selection_box2(bc_vws, vbox_m3t);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(c_clist_gtk->clist_name),
                                                        320, 160, window, vbox_m3t);
    return expand_bc;
}

