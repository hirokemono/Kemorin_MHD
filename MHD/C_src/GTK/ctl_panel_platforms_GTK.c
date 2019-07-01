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

GtkWidget * make_file_format_hbox(const char *label, struct chara_ctl_item *ctl_item){
    GtkTreeModel *model;
	GtkTreeModel *child_model;
	
    GtkWidget *combobox;
	
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *label_tree = gtk_tree_view_new();
	int index = 0;
	
	create_fixed_label_w_index_tree(label_tree);
	model = gtk_tree_view_get_model(label_tree);  
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	index = append_ci_item_to_tree(index, &file_fmt_labels[ASCII_MODE][0], ASCII_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[BINARY_MODE][0], BINARY_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[GZIP_MODE][0], GZIP_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[BIN_GZ_MODE][0], BIN_GZ_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_MODE][0], MERGED_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_MODE][0], MERGED_BIN_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_GZ_MODE][0], MERGED_GZ_MODE, child_model);
	index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_GZ_MODE][0], MERGED_BIN_GZ_MODE, child_model);
	
	gtk_box_set_homogeneous(hbox, FALSE);
	
	get_label_platform_ctl(12, label);
	combobox = gtk_combo_box_new_with_model(child_model);
	child_model = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), child_model, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), child_model,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combobox, find_file_fmt_index(ctl_item));
	g_signal_connect(G_OBJECT(combobox), "changed", G_CALLBACK(set_file_fmt_cb),
				(gpointer) ctl_item);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(label), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), combobox, FALSE, FALSE, 0);
	
	return hbox;
};

GtkWidget * make_platoform_hbox(const char *label_hd, struct platform_data_control_c *files_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_PLATFORM_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_platform_ctl(0, c_label);
	hbox_3[0] = make_chara_ctl_switch_hbox(c_label, files_c->debug_flag_c);
	
	get_label_platform_ctl(1, c_label);
	hbox_3[1] = make_integer_hbox(c_label, files_c->ndomain_c);
	
	get_label_platform_ctl(2, c_label);
	hbox_3[2] = make_integer_hbox(c_label, files_c->num_smp_c);
	
	get_label_platform_ctl(3, c_label);
	hbox_3[3] = make_text_hbox(c_label, files_c->sph_file_prefix_c);
	
	get_label_platform_ctl(4, c_label);
	hbox_3[4] = make_text_hbox(c_label, files_c->mesh_file_prefix_c);
	
	get_label_platform_ctl(5, c_label);
	hbox_3[5] = make_text_hbox(c_label, files_c->field_file_prefix_c);
	
	get_label_platform_ctl(6, c_label);
	hbox_3[6] = make_text_hbox(c_label, files_c->restart_file_prefix_c);
	
	get_label_platform_ctl(7, c_label);
	hbox_3[7] = make_text_hbox(c_label, files_c->spectr_field_file_prefix_c);
	
	get_label_platform_ctl(8, c_label);
	hbox_3[8] = make_text_hbox(c_label, files_c->coriolis_int_file_name_c);
	
	get_label_platform_ctl(9, c_label);
	hbox_3[9] = make_text_hbox(c_label, files_c->bc_data_file_name_c);
	
	get_label_platform_ctl(10, c_label);
	hbox_3[10] = make_text_hbox(c_label, files_c->interpolate_sph_to_fem_c);
	
	get_label_platform_ctl(11, c_label);
	hbox_3[11] = make_text_hbox(c_label, files_c->interpolate_fem_to_sph_c);
	
	get_label_platform_ctl(12, c_label);
	hbox_3[12] = make_file_format_hbox(c_label, files_c->sph_file_fmt_c);
	
	get_label_platform_ctl(13, c_label);
	hbox_3[13] = make_file_format_hbox(c_label, files_c->mesh_file_fmt_c);
	
	get_label_platform_ctl(14, c_label);
	hbox_3[14] = make_file_format_hbox(c_label, files_c->restart_file_fmt_c);
	
	get_label_platform_ctl(15, c_label);
	hbox_3[15] = make_file_format_hbox(c_label, files_c->field_file_fmt_c);
	
	get_label_platform_ctl(16, c_label);
	hbox_3[16] = make_file_format_hbox(c_label, files_c->itp_file_fmt_c);
	
	get_label_platform_ctl(17, c_label);
	hbox_3[17] = make_file_format_hbox(c_label, files_c->spectr_field_fmt_c);
	
	get_label_platform_ctl(18, c_label);
	hbox_3[18] = make_file_format_hbox(c_label, files_c->coriolis_file_fmt_c);
	
	get_label_platform_ctl(19, c_label);
	hbox_3[19] = make_chara_ctl_switch_hbox(c_label, files_c->del_org_data_ctl_c);
	
	free(c_label);
	
	for(i=0;i<NLBL_PLATFORM_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	
	hbox = make_expand_ctl_hbox(label_hd, &files_c->iflag_use, 400, vbox_1);
	return hbox;
}
