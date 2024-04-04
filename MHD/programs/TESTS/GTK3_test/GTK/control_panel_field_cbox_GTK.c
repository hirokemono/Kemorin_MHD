/*
//  control_panel_field_cbox_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "control_panel_field_cbox_GTK.h"

GtkWidget * create_field_label_tree(struct chara_int2_clist *field_list)
{
    GtkWidget *ctl_flags_tree_view = gtk_tree_view_new();
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(1, G_TYPE_STRING);
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    /* Append items */
    int i;
    GtkTreeIter iter;
    struct chara_int2_ctl_item *citem_tmp;
    for(i=0;i<count_chara_int2_clist(field_list);i++){
        citem_tmp = find_chara_int2_ctl_item_by_index(i, field_list);
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, citem_tmp->c_tbl, -1);
    };
    
    gtk_tree_view_set_model(GTK_TREE_VIEW(ctl_flags_tree_view), model);
    return ctl_flags_tree_view;
};

GtkWidget *create_compoonent_label_tree(struct chara2_int_clist *dir_list)
{
    GtkWidget *ctl_flags_tree_view = gtk_tree_view_new();
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(1, G_TYPE_STRING);
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    /* Append items */
    int i;
    GtkTreeIter iter;
    struct chara2_int_ctl_item *citem_tmp;
    for(i=0;i<count_chara2_int_clist(dir_list);i++){
        citem_tmp = chara2_int_clist_at_index(i, dir_list);
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, citem_tmp->c1_tbl, -1);
    };
    
    gtk_tree_view_set_model(GTK_TREE_VIEW(ctl_flags_tree_view), model);
    return ctl_flags_tree_view;
};


static void set_field_name(char *c_charavalue, struct chara_int2_clist *field_list,
                           GtkWidget *combobox){
    struct chara_int2_ctl_item *citem_tmp;
	int i;
	for(i=0;i<count_chara_int2_clist(field_list);i++){
        citem_tmp = find_chara_int2_ctl_item_by_index(i, field_list);
		if(cmp_no_case_c(c_charavalue, citem_tmp->c_tbl)){
            gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), i);
        };
	};
	return;
}

static void set_component_name(char *c_charavalue, struct chara2_int_clist *dir_list,
                               GtkWidget *combobox){
    struct chara2_int_ctl_item *citem_tmp;
    int i;
    for(i=0;i<count_chara2_int_clist(dir_list);i++){
        citem_tmp = chara2_int_clist_at_index(i, dir_list);
        if(cmp_no_case_c(c_charavalue, citem_tmp->c1_tbl)){
            gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), i);
        };
    };
    return;
}

static void cb_set_selected_field(GtkComboBox *combobox, gpointer user_data)
{
    struct chara_ctl_item *f_citem = g_object_get_data(G_OBJECT(combobox), "chara_ctl_item");

	GtkTreeModel *model_ctl = gtk_combo_box_get_model(combobox);
	GtkTreeIter iter;
	
	gchar *row_string;
	
	gint idx = gtk_combo_box_get_active(combobox);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_ctl, &iter, path);
    gtk_tree_model_get(model_ctl, &iter, COLUMN_FIELD_INDEX, &row_string, -1);
   
    update_chara_ctl_item_c(row_string, f_citem);
    c_store_chara_item_charavalue(f_citem->f_self, f_citem->c_tbl);
	/*
    printf("Selected %s\n", row_string);
	printf("c_charavalue %s\n", c_charavalue);
	*/
	return;
}

static void add_field_combobox_vbox(struct chara_ctl_item *f_citem,
                                    struct chara_int2_clist *field_list, GtkWidget *vbox_out)
{
	GtkWidget *hbox =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
    GtkWidget *ctl_flags_tree_view = create_field_label_tree(field_list);
	GtkTreeModel *model_ctl
			= gtk_tree_view_get_model(GTK_TREE_VIEW(ctl_flags_tree_view));
    
	GtkWidget *combobox = gtk_combo_box_new_with_model(model_ctl);
	GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(combobox), "chara_ctl_item", f_citem);
	g_signal_connect(G_OBJECT(combobox), "changed",
				G_CALLBACK(cb_set_selected_field), NULL);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), column_group, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), column_group,
								   "text", COLUMN_FIELD_INDEX, NULL);
	set_field_name(f_citem->c_tbl, field_list, combobox);
	
	gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
	return;
}

static void add_component_combobox_vbox(struct chara_ctl_item *f_citem,
                                        struct chara2_int_clist *dir_list,
                                        GtkWidget *vbox_out)
{
    GtkWidget *hbox =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    
    GtkWidget *ctl_flags_tree_view = create_compoonent_label_tree(dir_list);
    GtkTreeModel *model_ctl
            = gtk_tree_view_get_model(GTK_TREE_VIEW(ctl_flags_tree_view));
    
    GtkWidget *combobox = gtk_combo_box_new_with_model(model_ctl);
    GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
    
    g_object_set_data(G_OBJECT(combobox), "chara_ctl_item", f_citem);
    g_signal_connect(G_OBJECT(combobox), "changed",
                     G_CALLBACK(cb_set_selected_field), NULL);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), column_group, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), column_group,
                                   "text", COLUMN_FIELD_INDEX, NULL);
    set_component_name(f_citem->c_tbl, dir_list, combobox);
    
    gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
    return;
}

GtkWidget *draw_field_combobox_hbox(struct chara_int2_clist *field_list,
                                    struct chara_ctl_item *f_citem, GtkWidget *window){
    GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag, f_citem->c_block_name);
    
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    add_field_combobox_vbox(f_citem, field_list, vbox);
    gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
    return hbox;
}

GtkWidget *draw_component_combobox_hbox(struct chara2_int_clist *dir_list,
                                        struct chara_ctl_item *f_citem, GtkWidget *window){
    GtkWidget *hbox = hbox_with_block_checkbox(f_citem->f_iflag, f_citem->c_block_name);
    
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    add_component_combobox_vbox(f_citem, dir_list, vbox);
    gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
    return hbox;
}
