
#include <gtk/gtk.h>
#include "t_SGS_MHD_control_c.h"
#include "tree_views_4_fixed_lists_GTK.h"

int iflag_read_mhd = 0;
struct SGS_MHD_control_c *mhd_ctl;

void set_control_box(GtkWidget *vbox0);

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


GtkWidget *window;
GtkWidget *expander_Top;
GtkWidget *vbox_0;

static void cb_New(GtkButton *button, gpointer data)
{
	printf("takotakotako \n");
	gtk_widget_destroy (expander_Top);
	gtk_widget_show_all(window);
}

static void cb_Open(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *read_file_name;
  gchar *folder;
	
	
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget*/
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[0],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);

	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		read_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		/* Get Foulder name */
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
		g_print( "file name: %s\n", read_file_name);
		g_print( "foulder name: %s\n", folder);
		g_free(folder);
    /* Show file name in entry */
		gtk_entry_set_text(entry, read_file_name);
		
		iflag_read_mhd = 1;
		
		read_SGS_MHD_control_file_c(read_file_name, buf, mhd_ctl);
		
		g_free(read_file_name);
        
		set_control_box(vbox_0);
		gtk_widget_show_all(window);
	}else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	}else{
		g_print( "Another response was received.\n" );
	}
	gtk_widget_destroy(dialog);
	
}

static void cb_Save(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *write_file_name;

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget*/
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
	
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		
		write_SGS_MHD_control_file_c(write_file_name, mhd_ctl);
		dealloc_SGS_MHD_control_c(mhd_ctl);
		g_free(write_file_name);
		iflag_read_mhd = 0;
		
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

void expander_MHD_ctl_callback(GObject *object, GParamSpec *param_spec, gpointer user_data){
	GtkExpander *expander;

	expander = GTK_EXPANDER (object);
	if (gtk_expander_get_expanded (expander)){
		printf("Expanded \n");
	}else{
		printf("Hided \n");
	}
	gtk_widget_show_all(window);
};


void set_file_box(GtkWidget *vbox0){
	GtkWidget *button_N, *button_O, *button_S, *button_Q;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *entry;
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry */
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)window);
	
	/* Generate Bottuns */
	button_N = gtk_button_new_with_label("New");
	button_O = gtk_button_new_with_label("Open");
	button_S = gtk_button_new_with_label("Save");
	button_Q = gtk_button_new_with_label("Quit");
	
	g_signal_connect(G_OBJECT(button_N), "clicked", G_CALLBACK(cb_New), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_N, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox0), hbox, FALSE, FALSE, 0);
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

int find_file_fmt_index(struct chara_ctl_item *file_fmt_c){
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

void set_control_box(GtkWidget *vbox0){
    GtkWidget *scrolled_window;
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *hbox_2[NLBL_SGS_MHD_CTL], *vbox_2[NLBL_SGS_MHD_CTL], *Frame_2[NLBL_SGS_MHD_CTL];
	GtkWidget *vbox_platform;
    GtkWidget *switch_0, *spinner_1, *spinner_2, *switch_19;
    GtkAdjustment *adjust_1, *adjust_2;
    GtkWidget *tbox_3, *tbox_4, *tbox_5, *tbox_6, *tbox_7;
    GtkWidget *tbox_8, *tbox_9, *tbox_10, *tbox_11;
    GtkWidget *combobox_12, *combobox_13, *combobox_14, *combobox_15;
    GtkWidget *combobox_16, *combobox_17, *combobox_18;
    
	GtkWidget *hbox_3[NLBL_PLATFORM_CTL];
	GtkWidget *expander_MHD_ctl[NLBL_SGS_MHD_CTL];
	
    GtkWidget *label_tree[7];
    GtkTreeModel *model[7];
    GtkTreeModel *child_model[7];
    int index = 0;
    
	int i, ii;
	char *c_label;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	/* Generate expander */
	get_label_MHD_control_head(c_label);
	expander_Top = gtk_expander_new_with_mnemonic(c_label);
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		get_label_SGS_MHD_ctl(i, c_label);
		expander_MHD_ctl[i] =  gtk_expander_new(c_label);
		gtk_box_pack_start(GTK_BOX(vbox_1), expander_MHD_ctl[i], TRUE, TRUE, 0);
		
		vbox_2[i] = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
		
		if(i == 0){
			vbox_platform = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
			for(ii=0;ii<7;ii++){
				index = 0;
				label_tree[ii] = create_fixed_label_w_index_tree();
				model[ii] = gtk_tree_view_get_model (label_tree[ii]);  
				child_model[ii] = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model[ii]));
				index = append_ci_item_to_tree(index, &file_fmt_labels[ASCII_MODE][0], ASCII_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[BINARY_MODE][0], BINARY_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[GZIP_MODE][0], GZIP_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[BIN_GZ_MODE][0], BIN_GZ_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_MODE][0], MERGED_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_MODE][0], MERGED_BIN_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_GZ_MODE][0], MERGED_GZ_MODE, child_model[ii]);
				index = append_ci_item_to_tree(index, &file_fmt_labels[MERGED_BIN_GZ_MODE][0], MERGED_BIN_GZ_MODE, child_model[ii]);
            };

            for(ii=0;ii<NLBL_PLATFORM_CTL;ii++){
                hbox_3[ii] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
                gtk_box_set_homogeneous(hbox_3[ii], FALSE);
                get_label_platform_ctl(ii, c_label);
                gtk_box_pack_start(GTK_BOX(hbox_3[ii]), gtk_label_new(c_label), FALSE, FALSE, 0);
            }
            
			if(iflag_read_mhd > 0){
                switch_0 = gtk_switch_new();
                gtk_switch_set_active(GTK_SWITCH(switch_0), TRUE);
                if(cmp_no_case_c(mhd_ctl->files->debug_flag_c->c_tbl, "ON") > 0){
                    gtk_switch_set_state(GTK_SWITCH(switch_0), TRUE);
                } else {
                    gtk_switch_set_state(GTK_SWITCH(switch_0), FALSE);
                }
                g_signal_connect (G_OBJECT (switch_0), "notify::active", G_CALLBACK(debug_sw_cb), 
                                  (gpointer) mhd_ctl->files->debug_flag_c);
                
                adjust_1 = gtk_adjustment_new(mhd_ctl->files->ndomain_c->i_data, 1, 2147483648, 1,
                                            100, 21474836);
                spinner_1 = gtk_spin_button_new(adjust_1, 1, 0);
                g_signal_connect(G_OBJECT(spinner_1), "value-changed", G_CALLBACK(set_ndomain_cb), 
                                 (gpointer) mhd_ctl->files->ndomain_c);
                
                adjust_2 = gtk_adjustment_new(mhd_ctl->files->num_smp_c->i_data, 1, 2147483648, 1,
                                              100, 21474836);
                spinner_2 = gtk_spin_button_new(adjust_2, 1, 0);
                g_signal_connect(G_OBJECT(spinner_2), "value-changed", G_CALLBACK(set_ndomain_cb), 
                                 (gpointer) mhd_ctl->files->num_smp_c);
                
                tbox_3 = gtk_entry_new();
                gtk_entry_set_text(tbox_3, mhd_ctl->files->sph_file_prefix_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_3), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->sph_file_prefix_c);
                
                tbox_4 = gtk_entry_new();
                gtk_entry_set_text(tbox_4, mhd_ctl->files->mesh_file_prefix_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_4), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->mesh_file_prefix_c);
                
                tbox_5 = gtk_entry_new();
                gtk_entry_set_text(tbox_5, mhd_ctl->files->field_file_prefix_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_5), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->field_file_prefix_c);
                
                tbox_6 = gtk_entry_new();
                gtk_entry_set_text(tbox_6, mhd_ctl->files->restart_file_prefix_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_6), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->restart_file_prefix_c);
                
                tbox_7 = gtk_entry_new();
                gtk_entry_set_text(tbox_7, mhd_ctl->files->spectr_field_file_prefix_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_7), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->spectr_field_file_prefix_c);
                
                tbox_8 = gtk_entry_new();
                gtk_entry_set_text(tbox_8, mhd_ctl->files->coriolis_int_file_name_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_8), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->coriolis_int_file_name_c);
                
                tbox_9 = gtk_entry_new();
                gtk_entry_set_text(tbox_9, mhd_ctl->files->bc_data_file_name_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_9), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->bc_data_file_name_c);
                
                tbox_10 = gtk_entry_new();
                gtk_entry_set_text(tbox_10, mhd_ctl->files->interpolate_sph_to_fem_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_10), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->interpolate_sph_to_fem_c);
                
                tbox_11 = gtk_entry_new();
                gtk_entry_set_text(tbox_11, mhd_ctl->files->interpolate_fem_to_sph_c->c_tbl);
                g_signal_connect(G_OBJECT(tbox_11), "activate", G_CALLBACK(file_prefix_cb), 
                                 (gpointer) mhd_ctl->files->interpolate_fem_to_sph_c);
                
                combobox_12 = gtk_combo_box_new_with_model(child_model[0]);
                child_model[0] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_12), child_model[0], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_12), child_model[0],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_12, find_file_fmt_index(mhd_ctl->files->sph_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_12), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->sph_file_fmt_c);
                
                combobox_13 = gtk_combo_box_new_with_model(child_model[1]);
                child_model[1] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_13), child_model[1], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_13), child_model[1],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_13, find_file_fmt_index(mhd_ctl->files->mesh_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_13), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->mesh_file_fmt_c);
                
                combobox_14 = gtk_combo_box_new_with_model(child_model[2]);
                child_model[2] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_14), child_model[2], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_14), child_model[2],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_14, find_file_fmt_index(mhd_ctl->files->restart_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_14), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->restart_file_fmt_c);
                
                combobox_15 = gtk_combo_box_new_with_model(child_model[3]);
                child_model[3] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_15), child_model[3], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_15), child_model[3],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_15, find_file_fmt_index(mhd_ctl->files->field_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_15), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->field_file_fmt_c);
                
                combobox_16 = gtk_combo_box_new_with_model(child_model[4]);
                child_model[4] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_16), child_model[4], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_16), child_model[4],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_16, find_file_fmt_index(mhd_ctl->files->itp_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_16), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->itp_file_fmt_c);
                
                combobox_17 = gtk_combo_box_new_with_model(child_model[5]);
                child_model[5] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_17), child_model[5], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_17), child_model[5],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_17, find_file_fmt_index(mhd_ctl->files->spectr_field_fmt_c));
                g_signal_connect(G_OBJECT(combobox_17), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->spectr_field_fmt_c);
                
                combobox_18 = gtk_combo_box_new_with_model(child_model[6]);
                child_model[6] = gtk_cell_renderer_text_new();
                gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_18), child_model[6], TRUE);
                gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_18), child_model[6],
							"text", COLUMN_FIELD_NAME, NULL);
				gtk_combo_box_set_active(combobox_18, find_file_fmt_index(mhd_ctl->files->coriolis_file_fmt_c));
                g_signal_connect(G_OBJECT(combobox_18), "changed", G_CALLBACK(set_file_fmt_cb),
                                 (gpointer) mhd_ctl->files->coriolis_file_fmt_c);
                
                switch_19 = gtk_switch_new();
                gtk_switch_set_active(GTK_SWITCH(switch_19), TRUE);
                if(cmp_no_case_c(mhd_ctl->files->del_org_data_ctl_c->c_tbl, "ON") > 0){
                    gtk_switch_set_state(GTK_SWITCH(switch_19), TRUE);
                } else {
                    gtk_switch_set_state(GTK_SWITCH(switch_19), FALSE);
                }
                g_signal_connect (G_OBJECT (switch_19), "notify::active", G_CALLBACK(debug_sw_cb), 
                                  (gpointer) mhd_ctl->files->del_org_data_ctl_c);
                
                gtk_box_pack_start(GTK_BOX(hbox_3[0]), switch_0, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[1]), spinner_1, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[2]), spinner_2, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[3]), tbox_3, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[4]), tbox_4, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[5]), tbox_5, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[6]), tbox_6, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[7]), tbox_7, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[8]), tbox_8, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[9]), tbox_9, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[10]), tbox_10, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[11]), tbox_11, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[12]), combobox_12, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[13]), combobox_13, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[14]), combobox_14, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[15]), combobox_15, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[16]), combobox_16, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[17]), combobox_17, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[18]), combobox_18, FALSE, FALSE, 0);
                gtk_box_pack_start(GTK_BOX(hbox_3[19]), switch_19, FALSE, FALSE, 0);
			};
            for(ii=0;ii<NLBL_PLATFORM_CTL;ii++){
                gtk_box_pack_start(GTK_BOX(vbox_platform), hbox_3[ii], FALSE, FALSE, 0);
			}
			scrolled_window = gtk_scrolled_window_new(NULL, NULL);
			gtk_widget_set_size_request(scrolled_window, 300, 400);
			gtk_scrolled_window_set_max_content_height(scrolled_window, 400);
			gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 10);
			gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
			
			gtk_scrolled_window_add_with_viewport (
						GTK_SCROLLED_WINDOW(scrolled_window), vbox_platform);
			gtk_box_pack_start(GTK_BOX(vbox_2[0]), scrolled_window, FALSE, FALSE, 0);
		};
		
		Frame_2[i] = gtk_frame_new("");
		gtk_frame_set_shadow_type(GTK_FRAME(Frame_2[i]), GTK_SHADOW_IN);
		gtk_container_add(GTK_CONTAINER(Frame_2[i]), vbox_2[i]);
		
		hbox_2[i] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		gtk_box_pack_start(GTK_BOX(hbox_2[i]), gtk_label_new("  "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_2[i]), Frame_2[i], TRUE, TRUE, 0);
		gtk_container_add(GTK_CONTAINER(expander_MHD_ctl[i]), hbox_2[i]);
	};
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), Frame_1, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(expander_Top), hbox_1);
    
	gtk_box_pack_start(GTK_BOX(vbox0), expander_Top, TRUE, TRUE, 0);
};

int main(int argc, char** argv)
{
	GtkWidget *scroll_window;
	
    mhd_ctl = alloc_SGS_MHD_control_c();
	
	gtk_init(&argc, &argv);

	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	vbox_0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	/*
	scroll_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_box_pack_start(GTK_BOX(vbox_0), scroll_window, TRUE, TRUE, 0);
	*/
	/*
	gtk_box_pack_start(GTK_BOX(vbox_0), gtk_label_new(" BoxBoxBox "), TRUE, TRUE, 0);
     */
	set_file_box(vbox_0);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}
