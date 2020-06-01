/*
//  ctl_panel_para_sph_shell_GTK.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2019/06/30.
*/

#include "ctl_panel_para_sph_shell_GTK.h"

#define RADIAL_MODE      0
#define HORIZONTAL_MODE  1

const char *label_radial =     "radial";
const char *label_horizontal = "horizontal";
const char *label_meridional = "meridional";
const char *label_zonal =      "zonal";
const char *label_modes =      "modes";

const char inner_docomp_labels[2][KCHARA_C] = {
    "radial", 
    "horizontal"
};

static int find_inner_decomp_index(struct chara_ctl_item *inner_decomp_c){
	int i;
	
	for(i=0;i<2;i++){
		if(cmp_no_case_c(inner_decomp_c->c_tbl, &inner_docomp_labels[i][0]) > 0) return i;
	}
	return -1;
};

static void set_inner_decomp_cb(GtkComboBox *combobox_cmap, gpointer data)
{
    struct chara_ctl_item *inner_decomp_c = (struct chara_ctl_item *) data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    
    gint idx;
    gchar *row_string;
    int index_field;
	
	if(inner_decomp_c->iflag == 0) gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_cmap), -1);
	idx = gtk_combo_box_get_active(combobox_cmap);
	if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    sprintf(inner_decomp_c->c_tbl, "%s", row_string);
    return;
}

GtkWidget * make_inner_decomp_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	int index = 0;
	int iflag;
	
    GtkWidget *label_tree = create_fixed_label_w_index_tree();
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    
	index = append_ci_item_to_tree(index, &inner_docomp_labels[RADIAL_MODE][0], RADIAL_MODE, child_model);
	index = append_ci_item_to_tree(index, &inner_docomp_labels[HORIZONTAL_MODE][0], HORIZONTAL_MODE, child_model);
	
	tbox_flag->entry = gtk_combo_box_new_with_model(child_model);
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(tbox_flag->entry), renderer, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(tbox_flag->entry), renderer,
				"text", COLUMN_FIELD_NAME, NULL);
	
	iflag = find_inner_decomp_index(ctl_item);
	if(iflag == HORIZONTAL_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(tbox_flag->entry), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(tbox_flag->entry), 0);		
	};
	g_signal_connect(G_OBJECT(tbox_flag->entry), "changed", G_CALLBACK(set_inner_decomp_cb),
				(gpointer) ctl_item);
	
	hbox = make_entry_with_switch_hbox(iflag_fix_on, label, &ctl_item->iflag, tbox_flag);
	return hbox;
};

GtkWidget * make_FEM_mesh_ctl_hbox(const char *label_hd, struct FEM_mesh_control_c *Fmesh){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_FEM_MESH_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_FEM_mesh_ctl(0, c_label);
	hbox_3[0] = make_chara_ctl_switch_hbox(0, c_label, Fmesh->memory_conservation_c);
	
	get_label_FEM_mesh_ctl(1, c_label);
	hbox_3[1] = make_chara_ctl_switch_hbox(0, c_label, Fmesh->FEM_mesh_output_switch_c);
	
	get_label_FEM_mesh_ctl(2, c_label);
	hbox_3[2] = make_chara_ctl_switch_hbox(0, c_label, Fmesh->FEM_surface_output_switch_c);
	
	get_label_FEM_mesh_ctl(3, c_label);
	hbox_3[3] = make_chara_ctl_switch_hbox(0, c_label, Fmesh->FEM_viewer_output_switch_c);
	
	get_label_FEM_mesh_ctl(4, c_label);
	hbox_3[4] = make_integer_hbox(0, c_label, Fmesh->FEM_sleeve_level_c);
	
	get_label_FEM_mesh_ctl(5, c_label);
	hbox_3[5] = make_chara_ctl_switch_hbox(0, c_label, Fmesh->FEM_element_overlap_c);
	
	free(c_label);
	
	for(i=0;i<NLBL_FEM_MESH_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	hbox = make_expand_ctl_hbox(label_hd, &Fmesh->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget *make_ndomain_hbox(int iflag_decomp, const char *label_hd, 
			struct chara_int_clist *ndomain_list){
	int i, num;
	struct chara_int_ctl_item *tmp_list;
	GtkAdjustment *adjust[2];
	GtkWidget *spin[2];
	GtkWidget *hbox_3[2];
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	
	if(iflag_decomp == 5){
		num = 1;
	} else { 
		num = 2;
	};
	
	if(count_chara_int_clist(ndomain_list) != num){
		dealloc_chara_int_clist(ndomain_list);
        ndomain_list = init_ndomain_list_c();
		if(iflag_decomp == 3){
			append_chara_int_clist(label_radial, 0, ndomain_list);
			append_chara_int_clist(label_meridional, 0, ndomain_list);
		} else if(iflag_decomp == 4){
			append_chara_int_clist(label_radial, 0, ndomain_list);
			append_chara_int_clist(label_zonal, 0, ndomain_list);
		} else {
			append_chara_int_clist(label_modes, 0, ndomain_list);
		};
	};
	
	for(i=0;i<num;i++){
		tmp_list = chara_int_clist_at_index(i, ndomain_list);
		hbox_3[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
		adjust[i] = gtk_adjustment_new(tmp_list->i_data, 0, 2147483648, 1, 100, 21474836);
		spin[i] = gtk_spin_button_new(adjust[i], 1, 0);
		gtk_box_pack_start(GTK_BOX(hbox_3[i]), gtk_label_new(tmp_list->c_tbl), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_3[i]), spin[i], TRUE, TRUE, 0);
		
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	GtkWidget *hbox = make_expand_ctl_hbox(label_hd, &ndomain_list->iflag_use, 100, vbox_1);
	
	return hbox;
}

GtkWidget * make_sph_domains_ctl_hbox(const char *label_hd, struct sphere_domain_ctl_c *sdctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPHERE_DOMAIN_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	get_label_sphere_domain_ctl(0, c_label);
	hbox_3[0] = make_inner_decomp_hbox(0, c_label, sdctl_c->inner_decomp_c);
	
	get_label_sphere_domain_ctl(1, c_label);
	hbox_3[1] = make_integer_hbox(1, c_label, sdctl_c->num_radial_domain_c);
	
	get_label_sphere_domain_ctl(2, c_label);
	hbox_3[2] = make_integer_hbox(1, c_label, sdctl_c->num_horiz_domain_c);
	
	get_label_sphere_domain_ctl(3, c_label);
	hbox_3[3] = make_ndomain_hbox(3, c_label, sdctl_c->ndomain_sph_grid_list);
	
	get_label_sphere_domain_ctl(4, c_label);
	hbox_3[4] = make_ndomain_hbox(4, c_label, sdctl_c->ndomain_legendre_list);
	
	get_label_sphere_domain_ctl(5, c_label);
	hbox_3[5] = make_ndomain_hbox(5, c_label, sdctl_c->ndomain_spectr_list);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DOMAIN_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
		
	hbox = make_expand_ctl_hbox(label_hd, &sdctl_c->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget * make_sph_shell_ctl_hbox(const char *label_hd, struct sphere_data_ctl_c *spctl_c){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPHERE_DATA_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
    get_label_sphere_data_ctl(0, c_label);
    hbox_3[0] = make_text_hbox(0, c_label, spctl_c->sph_coef_type_c);
    
    get_label_sphere_data_ctl(1, c_label);
    hbox_3[1] = make_text_hbox(0, c_label, spctl_c->sph_grid_type_c);
    
	get_label_sphere_data_ctl(2, c_label);
	hbox_3[2] = make_integer_hbox(1, c_label, spctl_c->ltr_c);
	
	get_label_sphere_data_ctl(3, c_label);
	hbox_3[3] = make_integer_hbox(0, c_label, spctl_c->phi_symmetry_c);
	
	get_label_sphere_data_ctl(4, c_label);
	hbox_3[4] = make_integer_hbox(1, c_label, spctl_c->ngrid_elevation_c);
	
	get_label_sphere_data_ctl(5, c_label);
	hbox_3[5] = make_integer_hbox(1, c_label, spctl_c->ngrid_azimuth_c);
	
	get_label_sphere_data_ctl(6, c_label);
	hbox_3[6] = make_text_hbox(1, c_label, spctl_c->radial_grid_type_c);
	
	get_label_sphere_data_ctl(7, c_label);
	hbox_3[7] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(8, c_label);
	hbox_3[8] = make_integer_hbox(1, c_label, spctl_c->num_fluid_grid_c);
	
	get_label_sphere_data_ctl(9, c_label);
	hbox_3[9] = make_real_hbox(0, c_label, spctl_c->fluid_core_size_c);
	
	get_label_sphere_data_ctl(10, c_label);
	hbox_3[10] = make_real_hbox(0, c_label, spctl_c->ICB_to_CMB_ratio_c);
	
	get_label_sphere_data_ctl(11, c_label);
	hbox_3[11] = make_real_hbox(0, c_label, spctl_c->Min_radius_c);
	
	get_label_sphere_data_ctl(12, c_label);
	hbox_3[12] = make_real_hbox(0, c_label, spctl_c->ICB_radius_c);
	
	get_label_sphere_data_ctl(13, c_label);
	hbox_3[13] = make_real_hbox(0, c_label, spctl_c->CMB_radius_c);
	
	get_label_sphere_data_ctl(14, c_label);
	hbox_3[14] = make_real_hbox(0, c_label, spctl_c->Max_radius_c);
	
	get_label_sphere_data_ctl(15, c_label);
	hbox_3[15] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(16, c_label);
	hbox_3[16] = make_integer_hbox(0, c_label, spctl_c->num_radial_layer_c);
	
	get_label_sphere_data_ctl(17, c_label);
	hbox_3[17] = make_integer_hbox(0, c_label, spctl_c->num_med_layer_c);
	
	get_label_sphere_data_ctl(18, c_label);
	hbox_3[18] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	get_label_sphere_data_ctl(19, c_label);
	hbox_3[19] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	free(c_label);
	
	for(i=0;i<NLBL_SPHERE_DATA_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], FALSE, FALSE, 0);
	};
	hbox = make_expand_ctl_hbox(label_hd, &spctl_c->iflag_use, 400, vbox_1);
	return hbox;
}

GtkWidget *make_parallel_shell_hbox(const char *label_hd, char *shell_ctl_file_name, 
			struct parallel_sph_shell_control_c *shell_ctl, GtkWidget *save_bottun){
	int i;
	char *c_label;
	
	GtkWidget *hbox_3[NLBL_SPH_SHELL_CTL];
	
	GtkWidget *hbox;
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	get_label_sph_shell_ctl(0, c_label);
	hbox_3[0] = make_FEM_mesh_ctl_hbox(c_label, shell_ctl->Fmesh_ctl);
	
	get_label_sph_shell_ctl(1, c_label);
	hbox_3[1] = make_sph_domains_ctl_hbox(c_label, shell_ctl->sdctl_c);
	
	get_label_sph_shell_ctl(2, c_label);
	hbox_3[2] = make_sph_shell_ctl_hbox(c_label, shell_ctl->spctl_c);
	free(c_label);
	
	for(i=0;i<NLBL_SPH_SHELL_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], TRUE, TRUE, 0);
	};
	
    hbox = make_expand_ctl_file_hbox(label_hd, &shell_ctl->iflag_use_file, 
                                     shell_ctl_file_name, 400, vbox_1, save_bottun);
	return hbox;
}
