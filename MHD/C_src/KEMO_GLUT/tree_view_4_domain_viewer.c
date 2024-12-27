/*
//  tree_view_4_domain_viewer.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_domain_viewer.h"



static void toggle_draw_domain_patch_CB(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int index1_for_toggle;
	int index_grp = toggle_draw_patch_switch(path_str, grp_vws, &index1_for_toggle);
    kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFSOLID_TOGGLE, 
                                index_grp, index1_for_toggle,
                                kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void toggle_draw_domain_grid_CB(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int index2_for_toggle;
	int index_grp = toggle_draw_grid_switch(path_str, grp_vws, &index2_for_toggle);
    kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFGRID_TOGGLE, 
                                index_grp, index2_for_toggle,
                                kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void toggle_draw_domain_node_CB(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int index3_for_toggle;
	int index_grp = toggle_draw_node_switch(path_str, grp_vws, &index3_for_toggle);
    kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFNOD_TOGGLE,
                                index_grp, index3_for_toggle,
                                kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}


static void draw_all_domain_patch_CB(GtkButton *button, gpointer user_data)
{
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int num = set_all_draw_flags(IONE, COLUMN_MESH_THIRD, grp_vws);
    int i;
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                    i, IONE, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}

static void draw_all_domain_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FORTH, grp_vws);
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFGRID_TOGGLE, 
                                    i, IONE, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}

static void draw_all_domain_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int num = set_all_draw_flags(IONE, COLUMN_MESH_FIFTH, grp_vws);
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFNOD_TOGGLE, 
                                    i, IONE, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}

static void hide_all_domain_patch_CB(GtkButton *button, gpointer user_data)
{
	int i;
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_THIRD, grp_vws);
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFSOLID_TOGGLE, 
                                    i, IZERO, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}

static void hide_all_domain_grids_CB(GtkButton *button, gpointer user_data)
{
	int i;
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int num = set_all_draw_flags(IZERO, COLUMN_MESH_FORTH, grp_vws);
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFGRID_TOGGLE, 
                                    i, IZERO, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}

static void hide_all_domain_nodes_CB(GtkButton *button, gpointer user_data)
{
	int i;
    struct ci3_clist_view *grp_vws
            = (struct ci3_clist_view *) g_object_get_data(G_OBJECT(user_data),     "domain_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int num = set_all_draw_flags(IZERO, COLUMN_MESH_FIFTH, grp_vws);
	for(i=0;i<num;i++){
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFNOD_TOGGLE, 
                                    i, IZERO, kemo_gl->kemoview_data);
	};
    draw_full_gl(kemo_gl);
}


static void domain_patch_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	if (index_mode == SINGLE_COLOR){
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	} else {
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	};
	
    draw_full_gl(kemo_gl);
	return;
};

static void domain_grid_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	if (index_mode == SINGLE_COLOR){
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFGRID_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	} else {
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFGRID_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	};
	
    draw_full_gl(kemo_gl);
	return;
};

static void domain_node_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	if (index_mode == SINGLE_COLOR){
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFNOD_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	} else {
		kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFNOD_TOGGLE,
                                     index_mode, kemo_gl->kemoview_data);
	};
	
    draw_full_gl(kemo_gl);
	return;
};

static void set_domain_opacity_CB(GtkWidget *entry, gpointer user_data)
{
	float colorcode4[4];
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFSOLID_TOGGLE, colorcode4);
	colorcode4[3] = (float) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4, kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
	return;
}
static void set_single_domain_patch_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent_win = GTK_WINDOW(user_data);
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFSOLID_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent_win, colorcode4);
	if(iflag_set > 0) {
        kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                     colorcode4, kemo_gl->kemoview_data);
    };
	
    draw_full_gl(kemo_gl);
	return;
};

static void set_single_domain_grids_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent_win = GTK_WINDOW(user_data);
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFGRID_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent_win, colorcode4);
	if(iflag_set > 0) {
        kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFGRID_TOGGLE,
                                     colorcode4, kemo_gl->kemoview_data);
    };
	
    draw_full_gl(kemo_gl);
	return;
};
static void set_single_domain_nodes_color_CB(GtkButton *button, gpointer user_data)
{
	float colorcode4[4];
	GtkWindow *parent_win = GTK_WINDOW(user_data);
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFNOD_TOGGLE, colorcode4);
	int iflag_set = kemoview_gtk_colorsel_CB(parent_win, colorcode4);
	if(iflag_set > 0) {
        kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFNOD_TOGGLE,
                                     colorcode4, kemo_gl->kemoview_data);
    };
	
    draw_full_gl(kemo_gl);
	return;
};


static void create_domain_group_columns(struct kemoviewer_gl_type *kemo_gl,
                                        struct ci3_clist_view *domain_vws,
                                        GtkWidget *dummy_entry)
{
    /* First raw
    GtkTreeViewColumn *column_1st = create_each_column_no_sort(domain_vws->tree_view,
                                                               "Index", COLUMN_MESH_INDEX);
    GtkCellRenderer *textRenderer1 = create_each_text_renderer(column_1st, 40, COLUMN_MESH_INDEX);
*/
    /* Second row
    GtkTreeViewColumn *column_2nd = create_each_column_no_sort(domain_vws->tree_view,
                                                               "Domain", COLUMN_MESH_NAME);
    GtkCellRenderer *textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_MESH_NAME);
*/
    /* Third row */
    GtkTreeViewColumn *column_3rd = create_each_column_no_sort(domain_vws->tree_view,
                                                               "Patch", COLUMN_MESH_THIRD);
    GtkCellRenderer *toggleRenderer1 = create_each_toggle_renderer(column_3rd, 30, COLUMN_MESH_THIRD);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled",
				G_CALLBACK(toggle_draw_domain_patch_CB), (gpointer) dummy_entry);
    
    /* Forth row */
    GtkTreeViewColumn *column_4th = create_each_column_no_sort(domain_vws->tree_view,
                                                               "Grid", COLUMN_MESH_FORTH);
    GtkCellRenderer *toggleRenderer2 = create_each_toggle_renderer(column_4th, 30, COLUMN_MESH_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_draw_domain_grid_CB), (gpointer) dummy_entry);
	
    /* Fifth row */
    GtkTreeViewColumn *column_5th = create_each_column_no_sort(domain_vws->tree_view,
                                                               "Node", COLUMN_MESH_FIFTH);
    GtkCellRenderer *toggleRenderer3 = create_each_toggle_renderer(column_5th, 30, COLUMN_MESH_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer3), "toggled",
                     G_CALLBACK(toggle_draw_domain_node_CB), (gpointer) dummy_entry);
};

static void create_domain_group_view(struct kemoviewer_gl_type *kemo_gl,
                                     struct ci3_clist_view *domain_vws,
                                     GtkWidget *dummy_entry)
{
    int i;
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
 
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(5, G_TYPE_INT, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    domain_vws->tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(domain_vws->tree_view), model);
	
	create_domain_group_columns(kemo_gl, domain_vws, dummy_entry);
    
    /* Mode selection */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(domain_vws->tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(domain_vws->tree_view), COLUMN_MESH_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_MESH_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<count_chara_int3_clist(domain_vws->ci3_clist_gtk);i++){
		append_grp_model_data(i, domain_vws, child_model);
    }
    
}

void set_domain_draw_box(struct kemoviewer_gl_type *kemo_gl,
                         struct group_gtk_menu *domain_group_gmenu){
	int iflag_color;
	float color4[4];
	iflag_color = kemoview_get_mesh_color_flag(kemo_gl->kemoview_data,
                                               DOMAIN_FLAG, SURFSOLID_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_patch_color), 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_patch_color), 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_patch_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_patch_color), 0);
	};
	
	iflag_color = kemoview_get_mesh_color_flag(kemo_gl->kemoview_data,
                                               DOMAIN_FLAG, SURFGRID_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_grid_color), 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_grid_color), 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_grid_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_grid_color), 0);
	};
	
	iflag_color = kemoview_get_mesh_color_flag(kemo_gl->kemoview_data,
                                               DOMAIN_FLAG, SURFNOD_TOGGLE);
	if(iflag_color == GROUP_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_node_color), 3);
	} else 	if(iflag_color == DOMAIN_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_node_color), 2);
	} else 	if(iflag_color == SINGLE_COLOR){
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_node_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_node_color), 0);
	};
	
	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFSOLID_TOGGLE, color4);
	set_color_to_GTK(color4, &domain_group_gmenu->gcolor);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(domain_group_gmenu->spin_opacity), (double) color4[3]);
	gtk_color_chooser_set_rgba(GTK_COLOR_CHOOSER(domain_group_gmenu->button_patch_color),
							   &domain_group_gmenu->gcolor);

	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFGRID_TOGGLE, color4);
	set_color_to_GTK(color4, &domain_group_gmenu->gcolor);
	gtk_color_chooser_set_rgba(GTK_COLOR_CHOOSER(domain_group_gmenu->button_grid_color),
							   &domain_group_gmenu->gcolor);
	
	kemoview_get_mesh_color_code(kemo_gl->kemoview_data, DOMAIN_FLAG,
                                 SURFNOD_TOGGLE, color4);
	set_color_to_GTK(color4, &domain_group_gmenu->gcolor);
	gtk_color_chooser_set_rgba(GTK_COLOR_CHOOSER(domain_group_gmenu->button_node_color),
							   &domain_group_gmenu->gcolor);
	return;
};

void init_domain_draw_expander(struct kemoviewer_gl_type *kemo_gl,
                               GtkWidget *window,
                               struct group_gtk_menu *domain_group_gmenu){
    GtkWidget *dummy_entry = gtk_entry_new();
    g_object_set_data(G_OBJECT(dummy_entry),
                      "domain_view", (gpointer) domain_group_gmenu->group_vws);
    g_object_set_data(G_OBJECT(dummy_entry),
                      "kemoview_gl", (gpointer) kemo_gl);
    create_domain_group_view(kemo_gl, domain_group_gmenu->group_vws, dummy_entry);

    
    /* Delete data bottun */
    domain_group_gmenu->scrolled_table = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(domain_group_gmenu->scrolled_table),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(domain_group_gmenu->scrolled_table, 400, 300);
    gtk_container_add(GTK_CONTAINER(domain_group_gmenu->scrolled_table),
                      domain_group_gmenu->group_vws->tree_view);
    
    domain_group_gmenu->button_draw_patch = gtk_button_new_with_label("Draw patch");
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_draw_patch), "clicked",
                     G_CALLBACK(draw_all_domain_patch_CB), (gpointer) dummy_entry);
    
    domain_group_gmenu->button_draw_grid = gtk_button_new_with_label("Draw grids");
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_draw_grid), "clicked",
                     G_CALLBACK(draw_all_domain_grids_CB), (gpointer) dummy_entry);
    
    domain_group_gmenu->button_draw_node = gtk_button_new_with_label("Draw nodes");
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_draw_node), "clicked",
                     G_CALLBACK(draw_all_domain_nodes_CB), (gpointer) dummy_entry);
    
    
    domain_group_gmenu->button_hide_patch = gtk_button_new_with_label("Hide patch");
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_hide_patch), "clicked",
                     G_CALLBACK(hide_all_domain_patch_CB), (gpointer) dummy_entry);
    
    domain_group_gmenu->button_hide_grid = gtk_button_new_with_label("Hide grids");
    g_object_set_data(G_OBJECT(domain_group_gmenu->button_hide_grid), "kemoview_gl", (gpointer) kemo_gl);
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_hide_grid), "clicked",
                     G_CALLBACK(hide_all_domain_grids_CB), (gpointer) dummy_entry);
    
    domain_group_gmenu->button_hide_node = gtk_button_new_with_label("Hide nodes");
    g_object_set_data(G_OBJECT(domain_group_gmenu->button_hide_node), "kemoview_gl", (gpointer) kemo_gl);
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_hide_node), "clicked",
                     G_CALLBACK(hide_all_domain_nodes_CB), (gpointer) dummy_entry);
    
    
    GtkWidget *label_tree_patch_color = create_fixed_label_w_index_tree();
    GtkTreeModel *model_patch_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_patch_color));
    GtkTreeModel *child_model_patch_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_patch_color));
    int index = 0;
    index = append_ci_item_to_tree(index, "White",           WHITE_SURFACE, child_model_patch_color);
    index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_patch_color);
    index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_patch_color);
    index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_patch_color);
    
    GtkCellRenderer *renderer_patch_color = gtk_cell_renderer_text_new();
    domain_group_gmenu->combobox_patch_color = gtk_combo_box_new_with_model(child_model_patch_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_patch_color), 2);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_patch_color),
                               renderer_patch_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_patch_color),
                                   renderer_patch_color, "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(domain_group_gmenu->combobox_patch_color), "changed",
                     G_CALLBACK(domain_patch_colormode_CB), (gpointer) kemo_gl);
    
    
    GtkWidget *label_tree_grid_color = create_fixed_label_w_index_tree();
    GtkTreeModel *model_grid_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_grid_color));
    GtkTreeModel *child_model_grid_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_grid_color));
    index = 0;
    index = append_ci_item_to_tree(index, "Black",           BLACK_LINE, child_model_grid_color);
    index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_grid_color);
    index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_grid_color);
    index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_grid_color);
    
    GtkCellRenderer *renderer_grid_color = gtk_cell_renderer_text_new();
    domain_group_gmenu->combobox_grid_color = gtk_combo_box_new_with_model(child_model_grid_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_grid_color), 2);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_grid_color),
                               renderer_grid_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_grid_color),
                                   renderer_grid_color, "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(domain_group_gmenu->combobox_grid_color), "changed",
                     G_CALLBACK(domain_grid_colormode_CB), (gpointer) kemo_gl);
    
    
    GtkWidget *label_tree_node_color = create_fixed_label_w_index_tree();
    GtkTreeModel *model_node_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_node_color));
    GtkTreeModel *child_model_node_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_node_color));
    index = 0;
    index = append_ci_item_to_tree(index, "White",           WHITE_SURFACE, child_model_node_color);
    index = append_ci_item_to_tree(index, "Single color",    SINGLE_COLOR, child_model_node_color);
    index = append_ci_item_to_tree(index, "Color by domain", DOMAIN_COLOR, child_model_node_color);
    index = append_ci_item_to_tree(index, "Color by group",  GROUP_COLOR, child_model_node_color);
    
    GtkCellRenderer *renderer_node_color = gtk_cell_renderer_text_new();
    domain_group_gmenu->combobox_node_color = gtk_combo_box_new_with_model(child_model_node_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(domain_group_gmenu->combobox_node_color), 2);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_node_color),
                               renderer_node_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(domain_group_gmenu->combobox_node_color),
                                   renderer_node_color, "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(domain_group_gmenu->combobox_node_color), "changed",
                     G_CALLBACK(domain_node_colormode_CB), (gpointer) kemo_gl);
    
    g_object_set_data(G_OBJECT(window), "kemoview_gl",  (gpointer) kemo_gl);

    float color4[4] = {0.0, 0.0, 0.0, 1.0};
    set_color_to_GTK(color4, &domain_group_gmenu->gcolor);
    GtkAdjustment *adj_opacity = gtk_adjustment_new(color4[3], 0.0, 1.0, 0.01, 0.01, 0.0);
    domain_group_gmenu->spin_opacity = gtk_spin_button_new(GTK_ADJUSTMENT(adj_opacity), 0, 2);
    g_signal_connect(domain_group_gmenu->spin_opacity, "value-changed",
                     G_CALLBACK(set_domain_opacity_CB), (gpointer) window);
    
    domain_group_gmenu->button_patch_color = gtk_color_button_new_with_rgba(&domain_group_gmenu->gcolor);
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_patch_color), "clicked",
                     G_CALLBACK(set_single_domain_patch_color_CB), (gpointer) window);
    
    domain_group_gmenu->button_grid_color = gtk_color_button_new_with_rgba(&domain_group_gmenu->gcolor);
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_grid_color), "clicked",
                     G_CALLBACK(set_single_domain_grids_color_CB), (gpointer) window);
    
    domain_group_gmenu->button_node_color = gtk_color_button_new_with_rgba(&domain_group_gmenu->gcolor);
    g_signal_connect(G_OBJECT(domain_group_gmenu->button_node_color), "clicked",
                     G_CALLBACK(set_single_domain_nodes_color_CB), (gpointer) window);
    return;
}

    
    
GtkWidget * pack_domain_menu_box(struct group_gtk_menu *domain_group_gmenu){
    GtkWidget *box_domain;
    
    GtkWidget *vbox_table = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(vbox_table), domain_group_gmenu->scrolled_table, TRUE, TRUE, 0);
    add_sorting_signal_w_label(GTK_TREE_VIEW(domain_group_gmenu->group_vws->tree_view), vbox_table);
    
	GtkWidget *Frame = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame), vbox_table);
	
	GtkWidget *hbox_table = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_table), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_table), Frame, TRUE, TRUE, 0);
	
	GtkWidget *hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), domain_group_gmenu->button_draw_patch, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), domain_group_gmenu->button_draw_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), domain_group_gmenu->button_draw_node, TRUE, FALSE, 0);
	
	GtkWidget *hbox_hide = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_hide), gtk_label_new("Hide all: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), domain_group_gmenu->button_hide_patch, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), domain_group_gmenu->button_hide_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_hide), domain_group_gmenu->button_hide_node, TRUE, FALSE, 0);
	
	GtkWidget *hbox_one_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), gtk_label_new("Opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), domain_group_gmenu->spin_opacity, TRUE, TRUE, 0);
	
    
	GtkWidget *hbox_patch_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), gtk_label_new("Patch color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), domain_group_gmenu->combobox_patch_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_patch_color), domain_group_gmenu->button_patch_color, TRUE, FALSE, 0);

    GtkWidget *hbox_grid_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), gtk_label_new("Grid color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), domain_group_gmenu->combobox_grid_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_grid_color), domain_group_gmenu->button_grid_color, TRUE, FALSE, 0);
	
    GtkWidget *hbox_node_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), gtk_label_new("Node color: "), TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), domain_group_gmenu->combobox_node_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_color), domain_group_gmenu->button_node_color, TRUE, FALSE, 0);
	
    box_domain = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_table, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_draw, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_hide, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_one_opacity, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_patch_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_grid_color, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_domain), hbox_node_color, TRUE, FALSE, 0);
    return box_domain;
};

