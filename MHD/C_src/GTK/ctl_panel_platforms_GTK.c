/*
//  ctl_panel_platforms_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#include "ctl_panel_platforms_GTK.h"

#define ASCII_MODE           0
#define BINARY_MODE          1
#define GZIP_MODE            2
#define BIN_GZ_MODE          3
#define MERGED_MODE          4
#define MERGED_BIN_MODE      5
#define MERGED_GZ_MODE       6
#define MERGED_BIN_GZ_MODE   7

const char *label_ascii =          "ascii";
const char *label_binary =         "binary";
const char *label_gzip =           "gzip";
const char *label_bin_gz  =        "bin_gz";
const char *label_merged_ascii =   "merged";
const char *label_merged_binary =  "merged_bin";
const char *label_merged_gzip =    "merged_gz";
const char *label_merged_bin_gz  = "merged_bin_gz";

const char file_fmt_labels[8][KCHARA_C] = {
    "ascii", 
    "binary",
    "gzip",
    "bin_gz",
    "merged",
    "merged_bin",
    "merged_gz",
    "merged_bin_gz"
};


static void debug_sw_cb(GObject    *switch_3,
                        GParamSpec *pspec,
                        gpointer    data){
    struct chara_ctl_item *debug_flag_c = (struct chara_ctl_item *) data;

    if(gtk_switch_get_state(switch_3) == TRUE){
        sprintf(debug_flag_c->c_tbl, "ON");
        gtk_switch_set_state(switch_3, TRUE);
    } else {
        sprintf(debug_flag_c->c_tbl, "OFF");
        gtk_switch_set_state(switch_3, FALSE);
    };
};

static void set_ndomain_cb(GtkEntry *spinner, gpointer data){
    struct int_ctl_item *ctl_item = (struct chara_ctl_item *) data;
    
    if(data != NULL) {
        ctl_item->iflag = 1;
        ctl_item->i_data = gtk_spin_button_get_value_as_int(spinner);
        /*        printf("New value: %f\n", ctl_item->r_data); */
    };
    return;
};

static void file_prefix_cb(GtkEntry *entry, gpointer data)
{
    struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
    
    if(ctl_item->c_tbl != NULL) {
        ctl_item->iflag = 1;
        ctl_item->c_tbl = gtk_entry_get_text(entry);
    };
    return;
}

static int find_file_fmt_index(struct chara_ctl_item *file_fmt_c){
	int i;
	
	for(i=0;i<8;i++){
		if(cmp_no_case_c(file_fmt_c->c_tbl, &file_fmt_labels[i][0]) > 0) return i;
	}
	return -1;
};


static void set_file_fmt_cb(GtkComboBox *combobox_cmap, gpointer data)
{
    struct chara_ctl_item *file_fmt = (struct colormap_view *) data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    sprintf(file_fmt->c_tbl, "%s", row_string);
    return;
}

void add_platoform_box(struct platform_data_control_c *files_c, GtkWidget *vbox){
	int i;
	char *c_label;
    GtkWidget *label_tree[7];
    GtkTreeModel *model[7];
	GtkTreeModel *child_model[7];
	
    GtkWidget *switch_0, *switch_19;
    GtkWidget *spinner_1, *spinner_2;
    GtkAdjustment *adjust_1, *adjust_2;
    GtkWidget *tbox_3, *tbox_4, *tbox_5, *tbox_6, *tbox_7;
    GtkWidget *tbox_8, *tbox_9, *tbox_10, *tbox_11;
    GtkWidget *combobox_12, *combobox_13, *combobox_14, *combobox_15;
	GtkWidget *combobox_16, *combobox_17, *combobox_18;
	
	GtkWidget *hbox_1[NLBL_PLATFORM_CTL];
	GtkWidget *vbox_platform;
	GtkWidget *scrolled_window;
	
    int index = 0;
    
	for(i=0;i<7;i++){
		index = 0;
		label_tree[i] = gtk_tree_view_new();
		create_fixed_label_w_index_tree(label_tree[i]);
		model[i] = gtk_tree_view_get_model (label_tree[i]);  
		child_model[i] = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model[i]));
		index = append_ci_item_to_tree(index, &file_fmt_labels[ASCII_MODE][0], ASCII_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[BINARY_MODE][0], BINARY_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[GZIP_MODE][0], GZIP_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[BIN_GZ_MODE][0], BIN_GZ_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_MODE][0], MERGED_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_MODE][0], MERGED_BIN_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_GZ_MODE][0], MERGED_GZ_MODE, child_model[i]);
		index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_GZ_MODE][0], MERGED_BIN_GZ_MODE, child_model[i]);
	};
	
	adjust_1 = gtk_adjustment_new(files_c->ndomain_c->i_data, 1, 2147483648, 1,
				100, 21474836);
	spinner_1 = gtk_spin_button_new(adjust_1, 1, 0);
	g_signal_connect(G_OBJECT(spinner_1), "value-changed", G_CALLBACK(set_ndomain_cb), 
				(gpointer) files_c->ndomain_c);
	
	adjust_2 = gtk_adjustment_new(files_c->num_smp_c->i_data, 1, 2147483648, 1,
				100, 21474836);
	spinner_2 = gtk_spin_button_new(adjust_2, 1, 0);
	g_signal_connect(G_OBJECT(spinner_2), "value-changed", G_CALLBACK(set_ndomain_cb), 
				(gpointer) files_c->num_smp_c);
	
	switch_0 = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(switch_0), TRUE);
	if(cmp_no_case_c(files_c->debug_flag_c->c_tbl, "ON") > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_0), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_0), FALSE);
	}
	g_signal_connect (G_OBJECT (switch_0), "notify::active", G_CALLBACK(debug_sw_cb), 
				(gpointer) files_c->debug_flag_c);
	
	tbox_3 = gtk_entry_new();
	gtk_entry_set_text(tbox_3, files_c->sph_file_prefix_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_3), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->sph_file_prefix_c);
	
	tbox_4 = gtk_entry_new();
	gtk_entry_set_text(tbox_4, files_c->mesh_file_prefix_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_4), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->mesh_file_prefix_c);
	
	tbox_5 = gtk_entry_new();
	gtk_entry_set_text(tbox_5, files_c->field_file_prefix_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_5), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->field_file_prefix_c);
	
	tbox_6 = gtk_entry_new();
	gtk_entry_set_text(tbox_6, files_c->restart_file_prefix_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_6), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->restart_file_prefix_c);
	
	tbox_7 = gtk_entry_new();
	gtk_entry_set_text(tbox_7, files_c->spectr_field_file_prefix_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_7), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->spectr_field_file_prefix_c);
	
	tbox_8 = gtk_entry_new();
	gtk_entry_set_text(tbox_8, files_c->coriolis_int_file_name_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_8), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->coriolis_int_file_name_c);
	
	tbox_9 = gtk_entry_new();
	gtk_entry_set_text(tbox_9, files_c->bc_data_file_name_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_9), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->bc_data_file_name_c);
	
	tbox_10 = gtk_entry_new();
	gtk_entry_set_text(tbox_10, files_c->interpolate_sph_to_fem_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_10), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->interpolate_sph_to_fem_c);
	
	tbox_11 = gtk_entry_new();
	gtk_entry_set_text(tbox_11, files_c->interpolate_fem_to_sph_c->c_tbl);
	g_signal_connect(G_OBJECT(tbox_11), "activate", G_CALLBACK(file_prefix_cb), 
				(gpointer) files_c->interpolate_fem_to_sph_c);
	
	combobox_12 = gtk_combo_box_new_with_model(child_model[0]);
	child_model[0] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_12), child_model[0], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_12), child_model[0],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_12, find_file_fmt_index(files_c->sph_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_12), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->sph_file_fmt_c);
	
	combobox_13 = gtk_combo_box_new_with_model(child_model[1]);
	child_model[1] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_13), child_model[1], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_13), child_model[1],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_13, find_file_fmt_index(files_c->mesh_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_13), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->mesh_file_fmt_c);
	
	combobox_14 = gtk_combo_box_new_with_model(child_model[2]);
	child_model[2] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_14), child_model[2], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_14), child_model[2],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_14, find_file_fmt_index(files_c->restart_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_14), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->restart_file_fmt_c);
	
	combobox_15 = gtk_combo_box_new_with_model(child_model[3]);
	child_model[3] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_15), child_model[3], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_15), child_model[3],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_15, find_file_fmt_index(files_c->field_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_15), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->field_file_fmt_c);
	
	combobox_16 = gtk_combo_box_new_with_model(child_model[4]);
	child_model[4] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_16), child_model[4], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_16), child_model[4],
							"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_16, find_file_fmt_index(files_c->itp_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_16), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->itp_file_fmt_c);
	
	combobox_17 = gtk_combo_box_new_with_model(child_model[5]);
	child_model[5] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_17), child_model[5], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_17), child_model[5],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_17, find_file_fmt_index(files_c->spectr_field_fmt_c));
	g_signal_connect(G_OBJECT(combobox_17), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->spectr_field_fmt_c);
	
	combobox_18 = gtk_combo_box_new_with_model(child_model[6]);
	child_model[6] = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_18), child_model[6], TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_18), child_model[6],
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox_18, find_file_fmt_index(files_c->coriolis_file_fmt_c));
	g_signal_connect(G_OBJECT(combobox_18), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) files_c->coriolis_file_fmt_c);
	
	switch_19 = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(switch_19), TRUE);
	if(cmp_no_case_c(files_c->del_org_data_ctl_c->c_tbl, "ON") > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_19), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_19), FALSE);
	}
	g_signal_connect (G_OBJECT (switch_19), "notify::active", G_CALLBACK(debug_sw_cb), 
				(gpointer) files_c->del_org_data_ctl_c);
	
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	for(i=0;i<NLBL_PLATFORM_CTL;i++){
		hbox_1[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		gtk_box_set_homogeneous(hbox_1[i], FALSE);
		get_label_platform_ctl(i, c_label);
		gtk_box_pack_start(GTK_BOX(hbox_1[i]), gtk_label_new(c_label), FALSE, FALSE, 0);
	};
	free(c_label);
	
	gtk_box_pack_start(GTK_BOX(hbox_1[0]), switch_0, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[1]), spinner_1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[2]), spinner_2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[3]), tbox_3, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[4]), tbox_4, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[5]), tbox_5, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[6]), tbox_6, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[7]), tbox_7, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[8]), tbox_8, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[9]), tbox_9, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[10]), tbox_10, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[11]), tbox_11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[12]), combobox_12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[13]), combobox_13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[14]), combobox_14, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[15]), combobox_15, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[16]), combobox_16, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[17]), combobox_17, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[18]), combobox_18, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1[19]), switch_19, FALSE, FALSE, 0);
	
	vbox_platform = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_set_size_request(scrolled_window, 300, 400);
	gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
	
	
	for(i=0;i<NLBL_PLATFORM_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_platform), hbox_1[i], FALSE, FALSE, 0);
	};
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_platform);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, FALSE, FALSE, 0);
}
