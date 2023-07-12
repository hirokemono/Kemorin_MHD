/*
//  control_combobox_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "control_combobox_GTK.h"

extern void load_chara_from_c(char *c_ctl);

static void set_current_name(char *c_charavalue,
							 struct control_labels_f *flag_list, 
							 GtkWidget *combobox){
	int i;
	int index_field = -1;
	for(i=0;i<flag_list->num_labels;i++){
		printf("%d %s %s \n", i, c_charavalue, flag_list->label[i]);
		if(cmp_no_case_c(c_charavalue, flag_list->label[i])){index_field = i;};
	};
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox), index_field);
	return;
}

static void cb_set_selected_name_old(GtkComboBox *combobox, gpointer user_data)
{
	struct control_labels_f *flag_list = (struct control_labels_f *) user_data;
    char *c_charavalue
            = g_object_get_data(G_OBJECT(combobox), "cbox_comp_c");

	GtkTreeModel *model_ctl = gtk_combo_box_get_model(combobox);
	GtkTreeIter iter;
	
	gchar *row_string;
	int index_field;
	
	gint idx = gtk_combo_box_get_active(combobox);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_ctl, &iter, path);  
    gtk_tree_model_get(model_ctl, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_ctl, &iter, COLUMN_FIELD_NAME, &row_string, -1);
   
	strcpy(c_charavalue, flag_list->label[index_field]);
	/*
    printf("Selected %d %s\n", index_field, row_string);
	printf("flag_list %s\n", flag_list->label[index_field]);
	printf("c_charavalue %s\n", c_charavalue);
	*/
	return;
}


GtkWidget * create_fixed_label_tree(struct chara_clist *c1_clist)
{
    GtkWidget *ctl_flags_tree_view = gtk_tree_view_new();
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(1, G_TYPE_STRING);
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    /* Append items */
    int i;
    GtkTreeIter iter;
    for(i=0;i<count_chara_clist(c1_clist);i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, chara_clist_at_index(i,c1_clist)->c_tbl,
                           -1);
    };
    
    gtk_tree_view_set_model(GTK_TREE_VIEW(ctl_flags_tree_view), model);
    return ctl_flags_tree_view;
};


GtkWidget * create_control_flags_tree_view(struct control_labels_f *flag_list)
{
	int index = 0;
	GtkWidget *ctl_flags_tree_view = create_fixed_label_w_index_tree();
	index = append_c_list_from_array(index, flag_list->num_labels, flag_list->label, 
									 GTK_TREE_VIEW(ctl_flags_tree_view));
	return ctl_flags_tree_view;
};


void add_control_combobox_vbox_old(char *c_charavalue,
                               struct control_labels_f *flag_list,
							   GtkWidget *ctl_flags_tree_view, GtkWidget *vbox_out)
{
	GtkWidget *hbox =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkTreeModel *model_ctl
			= gtk_tree_view_get_model(GTK_TREE_VIEW(ctl_flags_tree_view));
	GtkWidget *combobox = gtk_combo_box_new_with_model(model_ctl);
	GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
	
    g_object_set_data(G_OBJECT(combobox), "cbox_comp_c", c_charavalue);
	g_signal_connect(G_OBJECT(combobox), "changed",
				G_CALLBACK(cb_set_selected_name_old), (gpointer) flag_list);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), column_group, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), column_group,
								   "text", COLUMN_FIELD_NAME, NULL);
	set_current_name(c_charavalue, flag_list, combobox);
	
	gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
	return;
}


static void cb_set_selected_name(GtkComboBox *combobox, gpointer user_data)
{
	struct control_labels_f *flag_list = (struct control_labels_f *) user_data;
    struct chara_ctl_item *f_citem = g_object_get_data(G_OBJECT(combobox), "chara_ctl_item");

	GtkTreeModel *model_ctl = gtk_combo_box_get_model(combobox);
	GtkTreeIter iter;
	
	gchar *row_string;
	int index_field;
	
	gint idx = gtk_combo_box_get_active(combobox);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_ctl, &iter, path);
    gtk_tree_model_get(model_ctl, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_ctl, &iter, COLUMN_FIELD_NAME, &row_string, -1);
   
	strcpy(f_citem->c_tbl, flag_list->label[index_field]);
    c_store_chara_item_charavalue(f_citem->f_self, f_citem->c_tbl);
	/*
    printf("Selected %d %s\n", index_field, row_string);
	printf("flag_list %s\n", flag_list->label[index_field]);
	printf("c_charavalue %s\n", c_charavalue);
	*/
	return;
}


void add_control_combobox_vbox(struct chara_ctl_item *f_citem,
                               struct control_labels_f *flag_list,
							   GtkWidget *ctl_flags_tree_view, GtkWidget *vbox_out)
{
	GtkWidget *hbox =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkTreeModel *model_ctl
			= gtk_tree_view_get_model(GTK_TREE_VIEW(ctl_flags_tree_view));
	GtkWidget *combobox = gtk_combo_box_new_with_model(model_ctl);
	GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(combobox), "chara_ctl_item", f_citem);
	g_signal_connect(G_OBJECT(combobox), "changed",
				G_CALLBACK(cb_set_selected_name), (gpointer) flag_list);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), column_group, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), column_group,
								   "text", COLUMN_FIELD_NAME, NULL);
	set_current_name(f_citem->c_tbl, flag_list, combobox);
	
	gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
	return;
}
