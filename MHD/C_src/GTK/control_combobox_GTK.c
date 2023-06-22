/*
//  control_combobox_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "control_combobox_GTK.h"

extern void load_chara_from_c(char *f_plt);

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

static void cb_set_selected_name(GtkComboBox *combobox, gpointer user_data)
{
	struct control_labels_f *flag_list = (struct control_labels_f *) user_data;
	char *f_charavalue
			= g_object_get_data(G_OBJECT(combobox), "cbox_comp_f");
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
    strcpy(f_charavalue, flag_list->label[index_field]);
    load_chara_from_c(f_charavalue);
	/*
    printf("Selected %d %s\n", index_field, row_string);
	printf("flag_list %s\n", flag_list->label[index_field]);
	printf("c_charavalue %s\n", c_charavalue);
	*/
	return;
}

GtkWidget * create_control_flags_tree_view(struct control_labels_f *flag_list)
{
	int index = 0;
	GtkWidget *ctl_flags_tree_view = create_fixed_label_w_index_tree();
	index = append_c_list_from_array(index, flag_list->num_labels, flag_list->label, 
									 GTK_TREE_VIEW(ctl_flags_tree_view));
	return ctl_flags_tree_view;
};


void add_control_combobox_vbox(char *f_charavalue, char *c_charavalue,
                               struct control_labels_f *flag_list,
							   GtkWidget *ctl_flags_tree_view, GtkWidget *vbox_out)
{
	GtkWidget *hbox =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkTreeModel *model_ctl
			= gtk_tree_view_get_model(GTK_TREE_VIEW(ctl_flags_tree_view));
	GtkWidget *combobox = gtk_combo_box_new_with_model(model_ctl);
	GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(combobox), "cbox_comp_f", f_charavalue);
    g_object_set_data(G_OBJECT(combobox), "cbox_comp_c", c_charavalue);
	g_signal_connect(G_OBJECT(combobox), "changed",
				G_CALLBACK(cb_set_selected_name), (gpointer) flag_list);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), column_group, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), column_group,
								   "text", COLUMN_FIELD_NAME, NULL);
	set_current_name(c_charavalue, flag_list, combobox);
	
	gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
	return;
}
