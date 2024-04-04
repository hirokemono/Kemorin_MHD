/*
//  control_panel_text_text_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_text_text_real_GTK.h"

static void c_combobox1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                                 gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct chara2_real_clist *ctl_clist = (struct chara2_real_clist *) g_object_get_data(G_OBJECT(cell), "chara2_real_clist");
    
    c2r_tree_1st_text_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_chara2_real_array(ctl_clist);
}

static void c_combobox2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                                 gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct chara2_real_clist *ctl_clist = (struct chara2_real_clist *) g_object_get_data(G_OBJECT(cell), "chara2_real_clist");
    
    c2r_tree_2nd_text_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_chara2_real_array(ctl_clist);
}

static void c_spin_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                             gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct chara2_real_clist *ctl_clist = (struct chara2_real_clist *) g_object_get_data(G_OBJECT(cell), "chara2_real_clist");
    
    c2r_tree_value_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_chara2_real_array(ctl_clist);
}

static void cb_add_c_item(GtkButton *button, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct chara2_real_clist *ctl_clist = (struct chara2_real_clist *) g_object_get_data(G_OBJECT(button), "chara2_real_clist");
    
    ctl_clist->index_bc = add_c2r_list_by_bottun_GTK(ctl_clist->index_bc,
                                                     GTK_TREE_VIEW(clist_tree_view),
                                                     ctl_clist);
    reflesh_f_ctl_c2r_array(count_chara2_real_clist(ctl_clist), ctl_clist);
    load_clist_to_chara2_real_array(ctl_clist);
    return;
}

static void cb_delete_c_item_by_list(GtkButton *button, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct chara2_real_clist *ctl_clist = (struct chara2_real_clist *) g_object_get_data(G_OBJECT(button), "chara2_real_clist");
    
    delete_c2r_list_items_GTK(GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    reflesh_f_ctl_c2r_array(count_chara2_real_clist(ctl_clist), ctl_clist);
    load_clist_to_chara2_real_array(ctl_clist);
}

void create_text_text_real_tree_view(GtkTreeView *cr_tree_view, GtkCellRenderer *renderer_text1,
                                     GtkCellRenderer *renderer_text2, GtkCellRenderer *renderer_spin)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkListStore *child_model;

	/* Construct empty list storage */
    child_model = gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(cr_tree_view, model);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(cr_tree_view, column);
    gtk_tree_view_column_set_title(column, "Type");
    g_object_set(G_OBJECT(renderer_text1), "editable", TRUE, NULL);
    g_object_set(renderer_text1, "width", (gint)120, NULL);
    gtk_tree_view_column_pack_start(column, renderer_text1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_text1, "text", COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_fixed_width(column, (gint) 120);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) cr_tree_view);
	*/
    /* Third row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(cr_tree_view, column);
    gtk_tree_view_column_set_title(column, "Type");
    g_object_set(G_OBJECT(renderer_text2), "editable", TRUE, NULL);
    g_object_set(renderer_text2, "width", (gint)120, NULL);
    gtk_tree_view_column_pack_start(column, renderer_text2, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_text2, "text", COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_fixed_width(column, (gint) 120);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) cr_tree_view);
	*/
    /* Forth row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(cr_tree_view, column);
    gtk_tree_view_column_set_title(column, "value");
    GtkAdjustment *adjust = gtk_adjustment_new(2.5, -1.0e30, 1.0e30, 0.1,
                                               100, 21474836);
    g_object_set(G_OBJECT(renderer_spin), 
                 "adjustment", adjust,
                 "climb-rate", 0.5,
                 "digits", 3, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin, "text", COLUMN_FORTH, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FORTH));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) cr_tree_view);
    */
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(cr_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(cr_tree_view, COLUMN_FIELD_NAME);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_NAME, GTK_SORT_ASCENDING);
}

static void init_c2r_combobox_tree_view(struct chara2_real_clist *ctl_clist, GtkWidget *clist_tree_view){
    GtkCellRenderer *renderer_spin =  gtk_cell_renderer_spin_new();
    
    GtkCellRenderer *renderer_text1 = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_text2 = gtk_cell_renderer_text_new();
    create_text_text_real_tree_view(GTK_TREE_VIEW(clist_tree_view),
                                    renderer_text1, renderer_text2, renderer_spin);
    
	g_object_set_data(G_OBJECT(renderer_text1), "chara2_real_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_text2), "chara2_real_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_spin), "chara2_real_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_spin), "chara2_real_clist", (gpointer) ctl_clist);
    
    g_signal_connect(G_OBJECT(renderer_text1), "edited",
                     G_CALLBACK(c_combobox1_edited_cb), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(renderer_text2), "edited",
                     G_CALLBACK(c_combobox2_edited_cb), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(renderer_spin), "edited",
                     G_CALLBACK(c_spin_edited_cb), (gpointer) clist_tree_view);

    ctl_clist->index_bc = append_c2r_list_from_ctl(ctl_clist->index_bc, &ctl_clist->c2r_item_head,
                                                   GTK_TREE_VIEW(clist_tree_view));
}

static void add_c2r_list_box_w_addbottun2(GtkTreeView *cr_tree_view,
                                   GtkWidget *button_add, GtkWidget *button_delete, 
                                   GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkWidget *scrolled_window;
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), button_delete, FALSE, FALSE, 0);
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 240, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(cr_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    add_sorting_signal_w_label(cr_tree_view, hbox);
};

static void add_c2r_list_selection_box(struct chara2_real_clist *ctl_clist,
                                       GtkWidget *clist_tree_view, GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
    
    add_c2r_list_box_w_addbottun2(GTK_TREE_VIEW(clist_tree_view),
                                   button_add, button_delete, vbox);
    /* Add callbacks */
    
	g_object_set_data(G_OBJECT(button_add),    "chara2_real_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(button_delete), "chara2_real_clist", (gpointer) ctl_clist);
    
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(cb_add_c_item), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked",
                     G_CALLBACK(cb_delete_c_item_by_list), (gpointer) clist_tree_view);
};

GtkWidget * c2r_list_text2_expander(struct chara2_real_clist *ctl_clist,
                                    GtkWidget *clist_tree_view, GtkWidget *window){
    clist_tree_view = gtk_tree_view_new();
    init_c2r_combobox_tree_view(ctl_clist, clist_tree_view);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_c2r_list_selection_box(ctl_clist, clist_tree_view, vbox);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(ctl_clist->clist_name),
                                                        window, vbox);
    return expand_bc;
}

