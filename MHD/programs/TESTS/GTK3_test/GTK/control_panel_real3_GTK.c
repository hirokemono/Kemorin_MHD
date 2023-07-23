/*
//  control_panel_real3_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_real3_GTK.h"


void load_clist_to_real3_array(struct real3_clist *ctl_clst){
    struct real3_ctl_item *tmp_clist;
    int i;
    for(i=0;i<count_real3_clist(ctl_clst);i++){
        tmp_clist = real3_clist_at_index(i,ctl_clst);
        c_store_real3_array(ctl_clst->f_self, i,
                            real3_clist_at_index(i,ctl_clst)->r_data[0],
                            real3_clist_at_index(i,ctl_clst)->r_data[1],
                            real3_clist_at_index(i,ctl_clst)->r_data[2]);
    }
    return;
}


static void c_spin1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                             gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct real3_clist *ctl_clist = (struct real3_clist *) g_object_get_data(G_OBJECT(cell), "real3_clist");
    
    r3_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_real3_array(ctl_clist);
}

static void c_spin2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                             gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct real3_clist *ctl_clist = (struct real3_clist *) g_object_get_data(G_OBJECT(cell), "real3_clist");
    
    r3_tree_value2_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_real3_array(ctl_clist);
}
static void c_spin3_edited_cb(GtkCellRendererText *cell, gchar *path_str,
                             gchar *new_text, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct real3_clist *ctl_clist = (struct real3_clist *) g_object_get_data(G_OBJECT(cell), "real3_clist");
    
    r3_tree_value3_edited(path_str, new_text, GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    load_clist_to_real3_array(ctl_clist);
}
static void cb_add_c_item(GtkButton *button, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct real3_clist *ctl_clist = (struct real3_clist *) g_object_get_data(G_OBJECT(button), "real3_clist");
    
    ctl_clist->index_bc = add_r3_list_items(GTK_TREE_VIEW(clist_tree_view),
                                                ctl_clist);
    reflesh_f_ctl_r3_array(count_real3_clist(ctl_clist), ctl_clist);
    load_clist_to_real3_array(ctl_clist);
    return;
}

static void cb_delete_c_items(GtkButton *button, gpointer user_data)
{
    GtkWidget *clist_tree_view = GTK_WIDGET(user_data);
	struct real3_clist *ctl_clist = (struct real3_clist *) g_object_get_data(G_OBJECT(button), "real3_clist");
    
    delete_r3_list_items(GTK_TREE_VIEW(clist_tree_view), ctl_clist);
    reflesh_f_ctl_r3_array(count_real3_clist(ctl_clist), ctl_clist);
    load_clist_to_real3_array(ctl_clist);
}

void init_r3_spins_tree_view(struct real3_clist *ctl_clist, GtkWidget *clist_tree_view){
    GtkCellRenderer *renderer_spin1 =  gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin2 =  gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin3 =  gtk_cell_renderer_text_new();
    
    create_real3_tree_view(GTK_TREE_VIEW(clist_tree_view), ctl_clist,
                           renderer_spin1, renderer_spin2, renderer_spin3);
    
	g_object_set_data(G_OBJECT(renderer_spin1), "real3_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_spin2), "real3_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_spin3), "real3_clist", (gpointer) ctl_clist);
    
    g_signal_connect(G_OBJECT(renderer_spin1), "edited",
                     G_CALLBACK(c_spin1_edited_cb), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited",
                     G_CALLBACK(c_spin2_edited_cb), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(renderer_spin3), "edited",
                     G_CALLBACK(c_spin3_edited_cb), (gpointer) clist_tree_view);

    ctl_clist->index_bc = append_r3_list_from_ctl(ctl_clist->index_bc, &ctl_clist->r3_item_head,
                                                  GTK_TREE_VIEW(clist_tree_view));
}

void add_r3_list_box_w_addbottun(GtkTreeView *r3_tree_view,
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
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(r3_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    add_sorting_signal_w_label(r3_tree_view, hbox);
};

static void add_r3_list_selection_box(struct real3_clist *ctl_clist,
                                       GtkWidget *clist_tree_view, GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
    
    add_r3_list_box_w_addbottun(GTK_TREE_VIEW(clist_tree_view),
                                button_add, button_delete, vbox);
    /* Add callbacks */
    
	g_object_set_data(G_OBJECT(button_add),    "real3_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(button_delete), "real3_clist", (gpointer) ctl_clist);
    
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(cb_add_c_item), (gpointer) clist_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked",
                     G_CALLBACK(cb_delete_c_items), (gpointer) clist_tree_view);
};

GtkWidget * r3_list_combobox_expander(struct real3_clist *ctl_clist,
                                      GtkWidget *clist_tree_view, GtkWidget *window){
    clist_tree_view = gtk_tree_view_new();
    init_r3_spins_tree_view(ctl_clist, clist_tree_view);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_r3_list_selection_box(ctl_clist, clist_tree_view, vbox);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(ctl_clist->clist_name),
                                                        window, vbox);
    return expand_bc;
}

