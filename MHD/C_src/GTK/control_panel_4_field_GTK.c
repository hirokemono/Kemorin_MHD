/*
//  control_panel_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "control_panel_4_field_GTK.h"


static void transfer_to_used_tree(struct all_field_ctl_c *all_fld_list,
			struct field_ctl_c *fld_ctl, GtkWidget *tree_view_to_del, GtkWidget *tree_view_to_add)
{
	GtkTreeModel *model_to_del;
	GtkTreeModel *child_model_to_del;
	GtkTreeModel *model_to_add;
	GtkTreeModel *child_model_to_add;
	GtkTreeSelection *selection;
	GList *list;
	GList *reference_list;
	GList *cur;
    
    gchar *field_name;
    gchar *field_math;
    int index_field;
    int iflag_viz, iflag_monitor, num_comp, iflag_quad;

	/* Get path of selected raw */
	/* The path is for tree_model_sort */
	model_to_del = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view_to_del));
	child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
	
	model_to_add = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view_to_add));
	child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view_to_del));
	list = gtk_tree_selection_get_selected_rows(selection, NULL);

	/* Make reference from path */
	/* After deliting data, obtained path would not be valied */
	reference_list = NULL;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *child_path;
		GtkTreeRowReference *child_reference;
		/* Convert tree model sort path into tree model path */
		child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_del), 
					(GtkTreePath *)cur->data);

		child_reference = gtk_tree_row_reference_new(child_model_to_del, child_path);
		reference_list = g_list_append(reference_list, child_reference);

		gtk_tree_path_free(child_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	/* Temporaly block changed signal from GtkTreeSelection */
	block_changed_signal(G_OBJECT(child_model_to_del));
	block_changed_signal(G_OBJECT(child_model_to_add));

	/* Back the reference to path and delete */
	for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;
		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FORTH, &iflag_viz, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIFTH, &iflag_monitor, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_NUM_COMP, &num_comp, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_QUADRATURE, &iflag_quad, -1);
        
        printf("To be moved: %d, %s: %s\n", index_field, field_name,
               all_fld_list->fld_list->field_name[index_field]);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
		
		/* Add */
		append_field_model_data(index_field, all_fld_list, 
								GTK_LIST_STORE(child_model_to_add));
		
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		printf("Add field list \n");
		add_field_wqflag_to_ctl(index_field, all_fld_list, fld_ctl);
		
	}
	g_list_free(reference_list);

	/* Release bloking of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_to_del));
	unblock_changed_signal(G_OBJECT(child_model_to_add));
}

static void transfer_to_unused_tree(struct all_field_ctl_c *all_fld_list,
			struct field_ctl_c *fld_ctl, GtkWidget *tree_view_to_del, GtkWidget **unused_field_tree_view)
{
	GtkTreeModel *model_for_used;
	GtkTreeModel *child_model_for_used;
	GtkTreeModel **model_for_unused;
	GtkTreeModel **child_model_for_unused;
	GtkTreeSelection *selection;
	GList *list;
	GList *reference_list;
	GList *cur;
    
    gchar *field_name;
    gchar *field_math;
    int index_field;
	int iflag_viz, iflag_monitor, num_comp, iflag_quad;
	int i;

	/* Get path of selected raw */
	/* The path is for tree_model_sort */
	model_for_used = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view_to_del));
	child_model_for_used = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_for_used));
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view_to_del));
	list = gtk_tree_selection_get_selected_rows(selection, NULL);
	
	
	int num_group = all_fld_list->fld_list->ntot_field_groups;
	if ((model_for_unused = (GtkTreeModel **) malloc(num_group*sizeof(GtkTreeModel *))) == NULL) {
		printf("malloc error for model_for_unused\n");
		exit(0);
	}
	if ((child_model_for_unused = (GtkTreeModel **) malloc(num_group*sizeof(GtkTreeModel *))) == NULL) {
		printf("malloc error for model_for_unused\n");
		exit(0);
	}
	
	for(i=0;i<num_group;i++){
		model_for_unused[i] = gtk_tree_view_get_model(GTK_TREE_VIEW(unused_field_tree_view[i]));
		child_model_for_unused[i] = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_for_unused[i]));
	};

	/* Make reference from path */
	/* データの削除を行なうと取得済みのパスが(大抵の場合)無効になる */
	reference_list = NULL;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *child_path;
		GtkTreeRowReference *child_reference;
		/* ツリーモデルソートのパスをツリーモデルのパスに変換する */
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
        
        printf("To be moved: %d, %s: %s\n", index_field, field_name,
               all_fld_list->fld_list->field_name[index_field]);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model_for_used), &iter);
		
		/* Add */
		for(i=0;i<num_group;i++){
			if(index_field >= all_fld_list->fld_list->istack_fields[i] &&
			   index_field < all_fld_list->fld_list->istack_fields[i+1]){
				append_field_model_data(index_field, all_fld_list, 
										GTK_LIST_STORE(child_model_for_unused[i]));
			};
		};
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		printf("Delete field list \n");
		delete_field_wqflag_in_ctl(index_field, all_fld_list, fld_ctl);
	}
	g_list_free(reference_list);
	
	
	/* changedシグナルのブロックを解除する */
	unblock_changed_signal(G_OBJECT(child_model_for_used));
	for(i=0;i<num_group;i++){
		unblock_changed_signal(G_OBJECT(child_model_for_unused[i]));
	};
	free(child_model_for_unused);
	free(model_for_unused);
	return;
}

static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	
	transfer_to_unused_tree(fields_vws->fld_gtk_data->all_fld_list, 
						fields_vws->fld_gtk_data->fld_ctl_gtk,
						fields_vws->used_tree_view, 
						fields_vws->unused_field_tree_view);
    /*
    check_field_ctl_list(fields_vws->fld_gtk_data->fld_ctl_gtk);
     */
}

static void add_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	GtkWidget *current_unused_field_tree_view = g_object_get_data(G_OBJECT(button), "current_tree");
	
	transfer_to_used_tree(fields_vws->fld_gtk_data->all_fld_list,
						fields_vws->fld_gtk_data->fld_ctl_gtk,
						current_unused_field_tree_view,
						fields_vws->used_tree_view);
    /*
    check_field_ctl_list(fields_vws->fld_gtk_data->fld_ctl_gtk);
     */
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

static void cb_set_component_name(GtkComboBox *combobox_comp, gpointer user_data)
{
	GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_comp);  
	GtkTreeIter iter;
	
	gchar *row_string;
	int index_comp;
	
	gint idx = gtk_combo_box_get_active(combobox_comp);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_comp, &iter, path);  
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_INDEX, &index_comp, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    printf("Selected component %d, %s\n", index_comp, row_string);
	
	return;
}

static void add_unused_field_box(int igrp, struct field_views *fields_vws, GtkWidget *vbox)
{
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *button_1;
	GtkWidget *scrolled_window_1;
	
	/* Construct unused field panel */
	
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
				G_CALLBACK(add_field_to_use), (gpointer) fields_vws);
    gtk_box_pack_start(GTK_BOX(hbox_1), button_1, FALSE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), Frame_1, TRUE, TRUE, 0);
	
	char *tmpchara = duplicate_underscore(fields_vws->fld_gtk_data->all_fld_list->fld_list->field_group_name[igrp]);
	wrap_into_expanded_frame_gtk(tmpchara, 250, 200, hbox_1, vbox);
};

void add_unused_field_boxes(struct field_views *fields_vws, GtkWidget *vbox)
{
	int igrp;
	GtkWidget *vbox_1;
	
	/* Construct unused field panel */
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	for(igrp=0;igrp<fields_vws->fld_gtk_data->all_fld_list->fld_list->ntot_field_groups;igrp++){
		add_unused_field_box(igrp, fields_vws, vbox_1);
	};
	
	wrap_into_expanded_frame_gtk("Field to add", 260, 200, vbox_1, vbox);
};

void add_field_selection_box(struct field_views *fields_vws, GtkWidget *vbox)
{
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *scrolled_window;
	
	char *c_label = (char *)calloc(KCHARA_C, sizeof(char));
	get_label_MHD_control_head(c_label);
	
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
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
	
	/* Set signals for sorting
	add_sorting_signal_w_label(GTK_TREE_VIEW(fields_vws->used_tree_view), hbox);
	*/
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	
	/* Construct unused field panel */
	add_unused_field_boxes(fields_vws, vbox);
};

void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox)
{
	GtkTreeModel *model_field;
	GtkTreeModel *model_comp;
	GtkWidget *hbox;
	GtkWidget *combobox_field;
    GtkCellRenderer *column_field;
	GtkWidget *combobox_comp;
    GtkCellRenderer *column_comp;
	
	model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->used_tree_view));
	model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	combobox_field = gtk_combo_box_new_with_model(model_field);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(cb_set_field_name), (gpointer) fields_vws);
	column_field = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_box_pack_start(GTK_BOX(hbox), combobox_field, FALSE, FALSE, 0);
	
	
	combobox_comp = gtk_combo_box_new_with_model(model_comp);
	g_signal_connect(G_OBJECT(combobox_comp), "changed", 
				G_CALLBACK(cb_set_component_name), NULL);
	
	g_object_set_data(G_OBJECT(combobox_field), "cbox_comp", combobox_comp);
	g_object_set_data(G_OBJECT(combobox_field), "column_comp", combobox_comp);
	gtk_box_pack_start(GTK_BOX(hbox), combobox_comp, FALSE, FALSE, 0);
	
	column_comp = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), column_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), column_comp,
				"text", COLUMN_FIELD_NAME, NULL);
	
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
}
