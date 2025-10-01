/*
//  control_panel_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "control_panel_4_field_GTK.h"


static void reflesh_field_ctl_f(struct chara_int2_clist *f_field_ctl){
    c_dealloc_chara3_array(f_field_ctl->f_self);
    c_alloc_chara3_array(count_chara_int2_clist(f_field_ctl),
                         f_field_ctl->f_self);
    update_field_ctl_f(f_field_ctl);
    return;
}


static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
    gchar *field_name;
    gchar *field_math;
    int index_field;
	int iflag_viz, iflag_monitor, num_comp, iflag_quad;
	int i;
	
	struct field_views *fields_vws = (struct field_views *) user_data;
	/* Get path of selected raw */
	/* The path is for tree_model_sort */
	GtkTreeModel *model_for_used 
			= gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->used_tree_view));
	GtkTreeModel *child_model_for_used 
			= gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_for_used));
	
	GtkTreeSelection *selection
			= gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->used_tree_view));
	GList *list = gtk_tree_selection_get_selected_rows(selection, NULL);
	
	
	int num_group = fields_vws->all_fld_list->fld_list->ntot_field_groups;
	GtkTreeModel **model_for_unused;
	GtkTreeModel **child_model_for_unused;
	if ((model_for_unused
		 = (GtkTreeModel **) malloc(num_group*sizeof(GtkTreeModel *))) == NULL) {
		printf("malloc error for model_for_unused\n");
		exit(0);
	}
	if ((child_model_for_unused
		 = (GtkTreeModel **) malloc(num_group*sizeof(GtkTreeModel *))) == NULL) {
		printf("malloc error for model_for_unused\n");
		exit(0);
	}
	
	for(i=0;i<num_group;i++){
		model_for_unused[i] = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->unused_field_tree_view[i]));
		child_model_for_unused[i] = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_for_unused[i]));
	};

	/* Make reference from path */
	/* After deleting data, obtained path would not be valied */
	GList *reference_list = NULL;
	GList *cur;
	GtkTreePath *child_path;
	GtkTreeRowReference *child_reference;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		/* Convert tree model sort path into tree model path */
		child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_for_used), 
					(GtkTreePath *)cur->data);

		child_reference = gtk_tree_row_reference_new(child_model_for_used, child_path);
		reference_list = g_list_append(reference_list, child_reference);

		gtk_tree_path_free(child_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	/* Temporaly block changed signal from GtkTreeSelection */
	block_changed_signal(G_OBJECT(child_model_for_used));
	for(i=0;i<num_group;i++){
		block_changed_signal(G_OBJECT(child_model_for_unused[i]));
	};
	/* Back the reference to path and delete */

    struct chara2_int_ctl_item *tmp_item;
	for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;
		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_for_used, &iter, tree_path);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_FORTH, &iflag_viz, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_FIFTH, &iflag_monitor, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_NUM_COMP, &num_comp, -1);
        gtk_tree_model_get(child_model_for_used, &iter, COLUMN_QUADRATURE, &iflag_quad, -1);
        
        tmp_item = chara2_int_clist_at_index(index_field,
                                             fields_vws->all_fld_list->fld_list->field_label);
        printf("To be moved: %d, %s: %s\n", index_field, field_name, tmp_item->c1_tbl);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model_for_used), &iter);
		
		/* Add */
		for(i=0;i<num_group;i++){
			if(index_field >= fields_vws->all_fld_list->fld_list->istack_fields[i] &&
			   index_field < fields_vws->all_fld_list->fld_list->istack_fields[i+1]){
				append_field_model_data(index_field, fields_vws->all_fld_list, 
										GTK_LIST_STORE(child_model_for_unused[i]));
			};
		};
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		printf("Delete field list %d \n", index_field);
		delete_field_wqflag_in_ctl(index_field, fields_vws->all_fld_list, fields_vws->fld_ctl_gtk);
	}
	g_list_free(reference_list);
	
	
	/* Release bloking of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_for_used));
	for(i=0;i<num_group;i++){
		unblock_changed_signal(G_OBJECT(child_model_for_unused[i]));
	};
	free(child_model_for_unused);
	free(model_for_unused);
    /*
    check_field_ctl_list(fields_vws->fld_ctl_gtk);
     */
    reflesh_field_ctl_f(fields_vws->fld_ctl_gtk->f_field_ctl);
	return;
}

static void add_field_to_use(GtkButton *button, gpointer user_data)
{
	GtkWidget *current_unused_field_tree_view 
			= GTK_WIDGET(g_object_get_data(G_OBJECT(button), "current_tree"));
	GtkWidget *used_tree_view
			= GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "used_tree"));
	struct all_field_ctl_c *all_fld_list 
			= (struct all_field_ctl_c *) g_object_get_data(G_OBJECT(user_data), "all_fields");
	struct f_MHD_fields_control *fld_ctl_gtk
			= (struct f_MHD_fields_control *) g_object_get_data(G_OBJECT(user_data), "fields_gtk");
	
    gchar *field_name;
    gchar *field_math;
    int index_field;
    int iflag_viz, iflag_monitor, num_comp, iflag_quad;

	/* Get path of selected raw */
	/* The path is for tree_model_sort */
	GtkTreeModel *current_model
			= gtk_tree_view_get_model(GTK_TREE_VIEW(current_unused_field_tree_view));
	GtkTreeModel *current_child_model
			= gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(current_model));
	GtkTreeSelection *selection =
			gtk_tree_view_get_selection(GTK_TREE_VIEW(current_unused_field_tree_view));
	GList *list = gtk_tree_selection_get_selected_rows(selection, NULL);
	
	GtkTreeModel *model_for_used
			= gtk_tree_view_get_model(GTK_TREE_VIEW(used_tree_view));
	GtkTreeModel *child_model_for_used
			= gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_for_used));
	
	/* Make reference from path */
	/* After deleting data, obtained path would not be valied */
	GList *reference_list = NULL;
	GtkTreePath *child_path;
	GtkTreeRowReference *child_reference;
	GList *cur;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		/* Convert tree model sort path into tree model path */
		child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(current_model), 
					(GtkTreePath *)cur->data);

		child_reference = gtk_tree_row_reference_new(current_child_model, child_path);
		reference_list = g_list_append(reference_list, child_reference);

		gtk_tree_path_free(child_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	/* Temporaly block changed signal from GtkTreeSelection */
	block_changed_signal(G_OBJECT(current_child_model));
	block_changed_signal(G_OBJECT(child_model_for_used));

	/* Back the reference to path and delete */
    struct chara2_int_ctl_item *tmp_item;
	for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;
		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(current_child_model, &iter, tree_path);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_FORTH, &iflag_viz, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_FIFTH, &iflag_monitor, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_NUM_COMP, &num_comp, -1);
        gtk_tree_model_get(current_child_model, &iter, COLUMN_QUADRATURE, &iflag_quad, -1);
        
        tmp_item = chara2_int_clist_at_index(index_field,
                                             all_fld_list->fld_list->field_label);
        printf("To be moved: %d, %s: %s\n", index_field, field_name, tmp_item->c1_tbl);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(current_child_model), &iter);
		
		/* Add */
		append_field_model_data(index_field, all_fld_list, 
								GTK_LIST_STORE(child_model_for_used));
		
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		printf("Add field list \n");
		add_field_wqflag_to_ctl(index_field, all_fld_list, fld_ctl_gtk);
		
	}
	g_list_free(reference_list);

	/* Release bloking of changed signal */
	unblock_changed_signal(G_OBJECT(current_child_model));
	unblock_changed_signal(G_OBJECT(child_model_for_used));
    /*
    check_field_ctl_list(fld_ctl_gtk);
	 */
    reflesh_field_ctl_f(fld_ctl_gtk->f_field_ctl);
	return;
}

static void cb_set_component_name(GtkComboBox *combobox_comp, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_comp);  
	GtkTreeIter iter;
	
	gchar *row_string;
	
	gint idx = gtk_combo_box_get_active(combobox_comp);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_comp, &iter, path);  
	gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_INDEX,
					   &fields_vws->selected_component_id, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
	printf("Selected component %d, %s\n", 
		   fields_vws->selected_component_id, row_string);
	return;
}

static void cb_set_field_name(GtkComboBox *combobox_field, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	GtkTreeModel *model_field = gtk_combo_box_get_model(combobox_field);
	GtkTreeIter iter;
	
    gchar *row_string;
	int index_field;
	int num_comp;
    int iflag_quad;
	
	GtkWidget *combobox_comp;
	GtkTreeModel *model_comp;
	
	gint idx = gtk_combo_box_get_active(combobox_field);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_field, &iter, path);  
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_NUM_COMP, &num_comp, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_QUADRATURE, &iflag_quad, -1);
   
    printf("Selected field %d %d %d %s\n", index_field, num_comp, iflag_quad, row_string);
	
	if(num_comp == 6){
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->sym_tensor_label_view));
	}else if(num_comp == 3){
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	}else{
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->scalar_label_view));
	};
	
	combobox_comp = g_object_get_data(G_OBJECT(combobox_field), "cbox_comp");
	gtk_combo_box_set_model(GTK_COMBO_BOX(combobox_comp), model_comp);
	
	return;
}

static void cb_set_group_name(GtkComboBox *combobox_group, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	GtkWidget *combobox_field = g_object_get_data(G_OBJECT(combobox_group), "cbox_field");
	
	GtkTreeModel *model_group = gtk_combo_box_get_model(combobox_group);
	GtkTreeIter iter;
	
	gchar *row_string;
	int index_field;
	
	gint idx = gtk_combo_box_get_active(combobox_group);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_group, &iter, path);  
    gtk_tree_model_get(model_group, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_group, &iter, COLUMN_FIELD_NAME, &row_string, -1);
   
    printf("Selected field %d %s\n", index_field, row_string);
	GtkTreeModel *model_field
			= gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->all_field_tree_view[index_field]));
	
	gtk_combo_box_set_model(GTK_COMBO_BOX(combobox_field), model_field);
	gtk_combo_box_set_active (GTK_COMBO_BOX(combobox_field), 0);
	cb_set_field_name(GTK_COMBO_BOX(combobox_field), user_data);
	return;
}

static void add_unused_field_box(int igrp, struct field_views *fields_vws, 
                                 GtkWidget *window, GtkWidget *vbox){
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *button_1;
	GtkWidget *scrolled_window_1;
	GtkWidget *entry;
	
	/* Construct unused field panel */
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "current_tree",
					  (gpointer) fields_vws->unused_field_tree_view[igrp]);
	g_object_set_data(G_OBJECT(entry), "used_tree",
					  (gpointer) fields_vws->used_tree_view);
	g_object_set_data(G_OBJECT(entry), "all_fields",
					  (gpointer) fields_vws->all_fld_list);
	g_object_set_data(G_OBJECT(entry), "fields_gtk",
					  (gpointer) fields_vws->fld_ctl_gtk);
	
	scrolled_window_1 = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window_1),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
//	gtk_widget_set_size_request(scrolled_window_1, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window_1), fields_vws->unused_field_tree_view[igrp]);
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window_1, TRUE, TRUE, 0);
	
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	/* Add data bottun */
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	button_1 = gtk_button_new_with_label("Add");
	g_object_set_data(G_OBJECT(button_1), "current_tree", 
					  (gpointer) fields_vws->unused_field_tree_view[igrp]);
	g_signal_connect(G_OBJECT(button_1), "clicked", 
				G_CALLBACK(add_field_to_use), (gpointer) entry);
    gtk_box_pack_start(GTK_BOX(hbox_1), button_1, FALSE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), Frame_1, TRUE, TRUE, 0);
	
    struct chara_ctl_item *tmp_grp
        = chara_clist_at_index(igrp, fields_vws->all_fld_list->fld_list->fld_grp_list);
	char *tmpchara = duplicate_underscore(tmp_grp->c_tbl);
	GtkWidget *expander = wrap_into_scroll_expansion_gtk(tmpchara, 250, 200, window, hbox_1);
    gtk_box_pack_start(GTK_BOX(vbox), expander, FALSE, FALSE, 0);
};

void add_unused_field_boxes(struct field_views *fields_vws, 
                            GtkWidget *window, GtkWidget *vbox){
	int igrp;
	GtkWidget *vbox_1;
	
	/* Construct unused field panel */
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	for(igrp=0;igrp<fields_vws->all_fld_list->fld_list->ntot_field_groups;igrp++){
		add_unused_field_box(igrp, fields_vws, window, vbox_1);
	};
	
	GtkWidget *expander = wrap_into_scroll_expansion_gtk("Field to add", 260, 200, window, vbox_1);
    gtk_box_pack_start(GTK_BOX(vbox), expander, FALSE, FALSE, 0);
};

void add_field_selection_box(struct field_views *fields_vws, 
                             GtkWidget *window, GtkWidget *vbox_out){
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *scrolled_window;

    /* Delete data bottun */
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	button = gtk_button_new_with_label("Remove");
	g_signal_connect(G_OBJECT(button), "clicked", 
					 G_CALLBACK(remove_field_to_use), (gpointer) fields_vws);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window), fields_vws->used_tree_view);
	gtk_box_pack_start(GTK_BOX(vbox_out), scrolled_window, TRUE, TRUE, 0);
	
	/* Set signals for sorting
	add_sorting_signal_w_label(GTK_TREE_VIEW(fields_vws->used_tree_view), hbox);
	*/
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
	
	/* Construct unused field panel */
	add_unused_field_boxes(fields_vws, window, vbox_out);
};

void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox_out)
{
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkTreeModel *model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->used_tree_view));
	GtkWidget *combobox_field = gtk_combo_box_new_with_model(model_field);
	GtkCellRenderer *column_field = gtk_cell_renderer_text_new();
	
	GtkTreeModel *model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	GtkWidget *combobox_comp = gtk_combo_box_new_with_model(model_comp);
	GtkCellRenderer *column_comp = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(combobox_field), "cbox_comp", combobox_comp);
	g_object_set_data(G_OBJECT(combobox_field), "column_comp", combobox_comp);
	
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(cb_set_field_name), (gpointer) fields_vws);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
				"text", COLUMN_FIELD_NAME, NULL);
	
	g_signal_connect(G_OBJECT(combobox_comp), "changed",
				G_CALLBACK(cb_set_component_name), fields_vws);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), column_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), column_comp,
				"text", COLUMN_FIELD_NAME, NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), combobox_field, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), combobox_comp, FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_out), hbox, FALSE, FALSE, 0);
}

void add_all_field_combobox_vbox(char *field_ctl_label, char *comp_ctl_label, 
								 struct field_views *fields_vws, GtkWidget *vbox_out)
{
	GtkWidget *hbox_fld =  gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	GtkWidget *label_field = gtk_label_new(duplicate_underscore(field_ctl_label));
	GtkTreeModel *model_group = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->field_group_tree_view));
	GtkWidget *combobox_group = gtk_combo_box_new_with_model(model_group);
	GtkCellRenderer *column_group = gtk_cell_renderer_text_new();
	
	GtkTreeModel *model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->all_field_tree_view[0]));
	GtkWidget *combobox_field = gtk_combo_box_new_with_model(model_field);
	GtkCellRenderer *column_field = gtk_cell_renderer_text_new();
	
	GtkWidget *label_comp =  gtk_label_new(duplicate_underscore(comp_ctl_label));
	GtkTreeModel *model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	GtkWidget *combobox_comp = gtk_combo_box_new_with_model(model_comp);
	GtkCellRenderer *column_comp = gtk_cell_renderer_text_new();
	
	int i_field = find_field_address(fields_vws->selected_field_ctl->c_tbl,
									 fields_vws->all_fld_list->fld_list);
	int i_grp = find_field_group(i_field, fields_vws->all_fld_list->fld_list);
	int i_comp = find_comp_address(fields_vws->selected_component_ctl->c_tbl, i_field, 
								   fields_vws->all_fld_list->fld_list,
								   fields_vws->comp_flags);
	
	printf("%d %d selected_field_ctl: %s\n", i_grp, i_field, 
		   fields_vws->selected_field_ctl->c_tbl);
    printf("%d selected_component_ctl: %s\n", i_comp, fields_vws->selected_component_ctl->c_tbl);
    
	g_object_set_data(G_OBJECT(combobox_group), "cbox_field", combobox_field);
	g_object_set_data(G_OBJECT(combobox_field), "cbox_comp", combobox_comp);
	g_object_set_data(G_OBJECT(combobox_field), "column_comp", combobox_comp);
	
	g_signal_connect(G_OBJECT(combobox_group), "changed", 
				G_CALLBACK(cb_set_group_name), (gpointer) fields_vws);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_group), column_group, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_group), column_group,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_group), i_grp);
	
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(cb_set_field_name), (gpointer) fields_vws);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), i_field);
	
	g_signal_connect(G_OBJECT(combobox_comp), "changed",
				G_CALLBACK(cb_set_component_name), fields_vws);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), column_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), column_comp,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_comp), i_comp);
	
	
	gtk_box_pack_start(GTK_BOX(hbox_fld), label_field, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_fld), combobox_group, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_fld), combobox_field, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(hbox_comp), label_comp, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_comp), combobox_comp, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox_fld, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_out), hbox_comp, FALSE, FALSE, 0);
}
