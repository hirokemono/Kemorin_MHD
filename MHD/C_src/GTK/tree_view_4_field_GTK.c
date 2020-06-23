/*
//  tree_view_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "tree_view_4_field_GTK.h"

struct field_views * init_field_views_GTK(struct field_ctl_c *fld_ctl_ref){
    struct field_views *fields_vws;
	if((fields_vws = (struct field_views *) malloc(sizeof(struct field_views))) == NULL){
		printf("malloc error for field_views\n");
		exit(0);
	};
	
	fields_vws->fld_ctl_gtk = fld_ctl_ref;
	fields_vws->all_fld_list = init_all_field_ctl_c();
    fields_vws->comp_flags = init_component_flags_f();
    
	load_field_w_qflag_from_ctl(fields_vws->fld_ctl_gtk, fields_vws->all_fld_list);
	return fields_vws;
}

void dealloc_field_views_GTK(struct field_views *fields_vws){
    dealloc_all_field_ctl_c(fields_vws->all_fld_list);
	dealloc_component_flags_f(fields_vws->comp_flags);
	free(fields_vws);
	return;
}


/* Append new data at the end of list */
void append_field_model_data(int index_field, struct all_field_ctl_c *all_fld_list, 
			GtkListStore *child_model)
{
    GtkTreeIter iter;
    
    gtk_list_store_append(child_model, &iter);
    gtk_list_store_set(child_model, &iter,
                       COLUMN_FIELD_INDEX, index_field,
                       COLUMN_FIELD_NAME, all_fld_list->fld_list->field_name[index_field],
                       COLUMN_FIELD_MATH, all_fld_list->fld_list->field_math[index_field],
                       COLUMN_FORTH, (gboolean) all_fld_list->iflag_viz[index_field],
                       COLUMN_FIFTH, (gboolean) all_fld_list->iflag_monitor[index_field],
                       COLUMN_NUM_COMP, all_fld_list->fld_list->num_comp[index_field],
                       COLUMN_QUADRATURE, all_fld_list->iflag_quad[index_field],
                       -1);
}

static void toggle_viz_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	struct all_field_ctl_c *all_fld_list 
			= (struct all_field_ctl_c *) g_object_get_data(G_OBJECT(user_data), "all_fields");
	struct field_ctl_c *fld_ctl_gtk
			= (struct field_ctl_c *) g_object_get_data(G_OBJECT(user_data), "fields_gtk");
	
	GtkWidget *used_tree_view = GTK_WIDGET(user_data);
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(GTK_TREE_VIEW(used_tree_view)));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &index_for_toggle, -1);
    
   
    printf("toggle_viz_switch %d, %s: %s\n", index_field, row_string,
            all_fld_list->fld_list->field_name[index_field]);
    
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FORTH, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    all_fld_list->iflag_viz[index_field] = index_for_toggle;
	update_field_flag_wqflag_in_ctl(index_field, all_fld_list, fld_ctl_gtk);
}

static void toggle_monitor_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	struct all_field_ctl_c *all_fld_list 
			= (struct all_field_ctl_c *) g_object_get_data(G_OBJECT(user_data), "all_fields");
	struct field_ctl_c *fld_ctl_gtk
			= (struct field_ctl_c *) g_object_get_data(G_OBJECT(user_data), "fields_gtk");
	
	GtkWidget *used_tree_view = GTK_WIDGET(user_data);
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(used_tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
	GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;
    
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH, &index_for_toggle, -1);
    
    printf("toggle_monitor_switch %d, %s: %s\n", index_field, row_string,
           all_fld_list->fld_list->field_name[index_field]);
	
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIFTH, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    all_fld_list->iflag_monitor[index_field] = index_for_toggle;
	update_field_flag_wqflag_in_ctl(index_field, all_fld_list, fld_ctl_gtk);
}


static void create_field_tree_columns(GtkWidget *used_tree_view)
{
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
    GtkCellRenderer *textRenderer3;
    GtkCellRenderer *toggleRenderer1;
    GtkCellRenderer *toggleRenderer2;
	
	GtkTreeViewColumn *column_1st;
    GtkTreeViewColumn *column_2nd;
    GtkTreeViewColumn *column_3rd;
    GtkTreeViewColumn *column_4th;
    GtkTreeViewColumn *column_5th;
	
	/* First raw */
	column_1st = create_each_field_column(used_tree_view, "Index", COLUMN_FIELD_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_FIELD_INDEX);
    
    /* Second row */
	column_2nd = create_each_field_column(used_tree_view, "Field name", COLUMN_FIELD_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_FIELD_NAME);
   
    /* Third row */
	column_3rd = create_each_field_column(used_tree_view, "Component", COLUMN_NUM_COMP);
	textRenderer3 = create_each_text_renderer(column_3rd, 60, COLUMN_NUM_COMP);
    
    /* Forth row */
	column_4th = create_each_field_column(used_tree_view, "Field output", COLUMN_FORTH);
	toggleRenderer1 = create_each_toggle_renderer(column_4th, 60, COLUMN_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_viz_switch), (gpointer) used_tree_view);
    
    /* Fifth row */
	column_5th = create_each_field_column(used_tree_view, "Monitor output", COLUMN_FIFTH);
	toggleRenderer2 = create_each_toggle_renderer(column_5th, 60, COLUMN_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_monitor_switch), (gpointer) used_tree_view);
};

GtkWidget * create_field_tree_view(struct all_field_ctl_c *all_fld_list, struct field_ctl_c *fld_ctl_gtk)
{
	GtkWidget *used_tree_view = gtk_tree_view_new();
	
	int i;
	GtkTreeModel *model;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	
	/* Construct empty list storage */
	GtkListStore *child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
												   G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
	g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
	
	/* Construct model for sorting and set to tree view */
	
	model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
	gtk_tree_view_set_model(GTK_TREE_VIEW(used_tree_view), model);
	
	g_object_set_data(G_OBJECT(used_tree_view), "all_fields", (gpointer) all_fld_list);
	g_object_set_data(G_OBJECT(used_tree_view), "fields_gtk", (gpointer) fld_ctl_gtk);
	create_field_tree_columns(used_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(used_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(used_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<all_fld_list->fld_list->ntot_fields;i++){
		if(all_fld_list->iflag_use[i] > 0) {
			append_field_model_data(i, all_fld_list, child_model);
		};
    }
	return used_tree_view;
}

static GtkWidget * create_unused_field_tree_view(int ist, int ied, struct all_field_ctl_c *all_fld_list)
{
	GtkWidget *unused_field_tree_view = gtk_tree_view_new();
	
	/*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
	
	GtkTreeViewColumn *column_1st;
	GtkTreeViewColumn *column_2nd;
	
	GtkListStore *child_model;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(unused_field_tree_view), model);
    
    /* First raw */
	column_1st = create_each_field_column(unused_field_tree_view, "Index name", COLUMN_FIELD_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_FIELD_INDEX);
	
	/* Second row */
	column_2nd = create_each_field_column(unused_field_tree_view, "Field name", COLUMN_FIELD_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_FIELD_NAME);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(unused_field_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(unused_field_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=ist;i<ied;i++){
		if(all_fld_list->iflag_use[i] == 0) {
			append_field_model_data(i, all_fld_list, child_model);
		};
    }
	return unused_field_tree_view;
}

static GtkWidget * create_all_field_tree_view(int ist, int ied, struct all_field_ctl_c *all_fld_list)
{
	GtkWidget *all_field_tree_view = gtk_tree_view_new();
	
	/*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
	
	GtkTreeViewColumn *column_1st;
	GtkTreeViewColumn *column_2nd;
	
	GtkListStore *child_model;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(all_field_tree_view), model);
    
    /* First raw */
	column_1st = create_each_field_column(all_field_tree_view, "Index name", COLUMN_FIELD_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_FIELD_INDEX);
	
	/* Second row */
	column_2nd = create_each_field_column(all_field_tree_view, "Field name", COLUMN_FIELD_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_FIELD_NAME);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(all_field_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(all_field_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
	
	for(i=ist;i<ied;i++){append_field_model_data(i, all_fld_list, child_model);
	}
	return all_field_tree_view;
}

GtkWidget ** create_unused_field_tree_views(struct all_field_ctl_c *all_fld_list)
{
	int i, ist, ied;
	int num_group = all_fld_list->fld_list->ntot_field_groups;
	GtkWidget **unused_field_tree_view;
	if ((unused_field_tree_view = (GtkWidget **) malloc(num_group*sizeof(GtkWidget *))) == NULL) {
		printf("malloc error for unused_field_tree_view\n");
		exit(0);
	}
	for(i=0;i<num_group;i++){
		ist = all_fld_list->fld_list->istack_fields[i];
		ied = all_fld_list->fld_list->istack_fields[i+1];
		unused_field_tree_view[i] = create_unused_field_tree_view(ist, ied, all_fld_list);
	}
	
	return unused_field_tree_view;
};

GtkWidget ** create_all_field_tree_views(struct all_field_ctl_c *all_fld_list)
{
	int i, ist, ied;
	int num_group = all_fld_list->fld_list->ntot_field_groups;
	GtkWidget **all_field_tree_view;
	if ((all_field_tree_view = (GtkWidget **) malloc(num_group*sizeof(GtkWidget *))) == NULL) {
		printf("malloc error for all_field_tree_view\n");
		exit(0);
	}
	for(i=0;i<num_group;i++){
		ist = all_fld_list->fld_list->istack_fields[i];
		ied = all_fld_list->fld_list->istack_fields[i+1];
		all_field_tree_view[i] = create_all_field_tree_view(ist, ied, all_fld_list);
	}
	
	return all_field_tree_view;
};

GtkWidget * create_field_group_tree_view(struct all_field_ctl_c *all_fld_list)
{
	int index = 0;
	GtkWidget *field_group_tree_view = create_fixed_label_w_index_tree();
	index = append_c_list_from_array(index, all_fld_list->fld_list->ntot_field_groups, 
									 all_fld_list->fld_list->field_group_name, 
									 GTK_TREE_VIEW(field_group_tree_view));
	return field_group_tree_view;
};


void create_direction_tree_views(struct field_views *fields_vws)
{
	fields_vws->scalar_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->comp_flags->scalar_components_flag,
                                  fields_vws->scalar_label_view);
	
	fields_vws->vector_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->comp_flags->vector_components_flag, 
                                  fields_vws->vector_label_view);
	
	fields_vws->sym_tensor_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->comp_flags->sym_tensor_components_flag, 
                                  fields_vws->sym_tensor_label_view);
	
/*    
	fields_vws->xyz_dir_label_view = create_fixed_label_w_math_tree();
	append_xyz_componnet_label(fields_vws->xyz_dir_label_view);
	fields_vws->surface_eq_view = create_fixed_label_w_math_tree();
	append_surface_equation_label(fields_vws->surface_eq_view);
*/
}


int find_field_address(const char *field_in, struct field_names_f *fld_list){
	int i;
	for(i=0;i<fld_list->ntot_fields;i++){
		if(cmp_no_case_c(field_in, fld_list->field_name[i]) > 0) {return i;};
	};
	return -1;
}

int find_field_group(const int i_field, struct field_names_f *fld_list){
	int i;
	for(i=0;i<fld_list->ntot_field_groups;i++){
		if(i_field >= fld_list->istack_fields[i]
		   && i_field < fld_list->istack_fields[i+1]) return i;
	};
	return -1;
}

static int find_each_comp_address(char *comp_in, struct flag_with_math_f *components_flag){
	int i;
	for(i=0;i<components_flag->num_flags;i++){
			if(cmp_no_case_c(comp_in, components_flag->component_name[i]) > 0) {return i;};
	};
	return -1;
}

int find_comp_address(char *comp_in, int i_field, struct field_names_f *fld_list,
					  struct component_flags_f *comp_flags){
	int i_comp;
	if(fld_list->num_comp[i_field] == 6){
		i_comp = find_each_comp_address(comp_in, comp_flags->sym_tensor_components_flag);
	}else if(fld_list->num_comp[i_field] == 3){
		i_comp = find_each_comp_address(comp_in, comp_flags->vector_components_flag);
	}else{
		i_comp = find_each_comp_address(comp_in, comp_flags->scalar_components_flag);
	};
	return i_comp;
}

