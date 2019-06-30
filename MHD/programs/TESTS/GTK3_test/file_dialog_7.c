
#include <gtk/gtk.h>
#include "t_SGS_MHD_control_c.h"
#include "control_panel_4_platforms_GTK.h"

#define NONE_MODE   0
#define FILE_MODE  -1
#define TYPE_MODE   1

const char *label_none =    "None";
/*const char *label_begin = "Begin"; */
/* const char *label_file =    "File"; */

const char input_mode_labels[3][KCHARA_C] = {
    "None",
    "File", 
    "Begin",
};

int iflag_read_mhd = 0;
struct SGS_MHD_control_c *mhd_ctl;

void set_control_box(GtkWidget *vbox0);

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
		
		mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
		alloc_SGS_MHD_control_c(mhd_ctl);
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

static void cb_Save_sph(GtkButton *button, gpointer data)
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
		
		write_spherical_shell_file_c(write_file_name, mhd_ctl->shell_ctl);
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

static void expander_switch_cb(GObject    *switch_3,
                        GParamSpec *pspec,
                        gpointer    data){
    int *iflag = (int *) data;
	
	if(gtk_switch_get_state(switch_3) == TRUE){
		gtk_switch_set_state(switch_3, TRUE);
        *iflag = 1;
	} else {
		gtk_switch_set_state(switch_3, FALSE);
        *iflag = 0;
	};
};

static void expander_action_cb(GObject    *switch_3,
                        gpointer    data){
    struct GtkWidget *expender = (GtkWidget *) switch_3;
    int *iflag = (int *) data;
	
	if(*iflag == 0){
		gtk_expander_set_expanded(GTK_EXPANDER(expender), TRUE);
    };
};

static void set_block_mode_cb(GtkComboBox *combobox_cmap, gpointer data)
{
    int *iflag_block = (int *) data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_mode, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, iflag_block, -1);
    return;
}

static void file_name_cb(GtkEntry *entry, gpointer data)
{
    char *file_name = (char *) data;
	file_name = gtk_entry_get_text(entry);
    return;
}


void set_control_box(GtkWidget *vbox0){
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *hbox_2[NLBL_SGS_MHD_CTL], *vbox_2[NLBL_SGS_MHD_CTL], *Frame_2[NLBL_SGS_MHD_CTL];
	GtkWidget *hbox_3, *switch_3[NLBL_SGS_MHD_CTL];
	GtkWidget *expander_MHD_ctl[NLBL_SGS_MHD_CTL];
	GtkWidget *entry_3;
	GtkWidget *button_S;
	
	int i, ii;
	char *c_label;
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
    GtkWidget *label_tree;
    GtkTreeModel *model;
	GtkTreeModel *child_model;
    int index = 0;
	
	index = 0;
	label_tree = gtk_tree_view_new();
	create_fixed_label_w_index_tree(label_tree);
	model = gtk_tree_view_get_model (label_tree);  
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	index = append_ci_item_to_tree(index, &input_mode_labels[0][0], NONE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[1][0], FILE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[2][0], TYPE_MODE, child_model);
	
	/* Generate expander */
	get_label_MHD_control_head(c_label);
    expander_Top = gtk_expander_new_with_mnemonic(c_label);
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	
	switch_3[0] = gtk_switch_new();
	expander_MHD_ctl[0] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[0]), TRUE);
	if(mhd_ctl->iflag_data_files_def > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[0]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[0]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[0]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_data_files_def);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[0]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_data_files_def);
	
	switch_3[1] = gtk_switch_new();
	expander_MHD_ctl[1] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[1]), TRUE);
	if(mhd_ctl->iflag_org_files_def > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[1]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[1]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[1]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_org_files_def);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[1]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_org_files_def);
	
	switch_3[2] = gtk_switch_new();
	expander_MHD_ctl[2] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[2]), TRUE);
	if(mhd_ctl->iflag_new_files_def > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[2]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[2]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[2]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_new_files_def);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[2]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_new_files_def);
	
    expander_MHD_ctl[3] = gtk_expander_new("");
	switch_3[3] = gtk_combo_box_new_with_model(child_model);
	child_model = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(switch_3[3]), child_model, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(switch_3[3]), child_model,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(switch_3[3], mhd_ctl->iflag_spherical_shell_ctl);
	g_signal_connect(G_OBJECT(switch_3[3]), "changed", G_CALLBACK(set_block_mode_cb),
				(gpointer) &mhd_ctl->iflag_spherical_shell_ctl);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[3]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_spherical_shell_ctl);
	
	switch_3[4] = gtk_switch_new();
	expander_MHD_ctl[4] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[4]), TRUE);
	if(mhd_ctl->iflag_model > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[4]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[4]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[4]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_model);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[4]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_model);
	
	switch_3[5] = gtk_switch_new();
	expander_MHD_ctl[5] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[5]), TRUE);
	if(mhd_ctl->iflag_control > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[5]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[5]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[5]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_control);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[5]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_control);
	
	switch_3[6] = gtk_switch_new();
	expander_MHD_ctl[6] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[6]), TRUE);
	if(mhd_ctl->iflag_sph_monitor_ctl > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[6]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[6]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[6]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_sph_monitor_ctl);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[6]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_sph_monitor_ctl);
	
	switch_3[7] = gtk_switch_new();
	expander_MHD_ctl[7] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[7]), TRUE);
	if(mhd_ctl->iflag_node_monitor_ctl > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[7]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[7]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[7]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_node_monitor_ctl);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[7]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_node_monitor_ctl);
	
	switch_3[8] = gtk_switch_new();
	expander_MHD_ctl[8] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[8]), TRUE);
	if(mhd_ctl->iflag_visual_control > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[8]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[8]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[8]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_visual_control);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[8]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_visual_control);
	
	switch_3[9] = gtk_switch_new();
	expander_MHD_ctl[9] = gtk_expander_new("");
	gtk_switch_set_active(GTK_SWITCH(switch_3[9]), TRUE);
	if(mhd_ctl->iflag_zonal_mean_control > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_3[9]), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_3[9]), FALSE);
	};
	g_signal_connect (G_OBJECT(switch_3[9]), "notify::active", G_CALLBACK(expander_switch_cb), 
				(gpointer) &mhd_ctl->iflag_zonal_mean_control);
	g_signal_connect(G_OBJECT(expander_MHD_ctl[9]), "activate", G_CALLBACK(expander_action_cb), 
				(gpointer) &mhd_ctl->iflag_zonal_mean_control);
	
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		vbox_2[i] = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
		
		hbox_3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
		
		get_label_SGS_MHD_ctl(i, c_label);
		
		gtk_box_pack_start(GTK_BOX(hbox_3), gtk_label_new(c_label), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_3), switch_3[i], FALSE, FALSE, 0);
		
		if(i==3){
			entry_3 = gtk_entry_new();
			gtk_entry_set_text(entry_3, mhd_ctl->shell_ctl_file_name);
			g_signal_connect(G_OBJECT(entry_3), "activate", G_CALLBACK(file_name_cb), 
				(gpointer) mhd_ctl->shell_ctl_file_name);
			
			button_S = gtk_button_new_with_label("Save");
			g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save_sph), 
						(gpointer) entry_3);
			
			gtk_box_pack_start(GTK_BOX(hbox_3), gtk_label_new("File:"), FALSE, FALSE, 0);
			gtk_box_pack_start(GTK_BOX(hbox_3), entry_3, TRUE, TRUE, 0);
			gtk_box_pack_start(GTK_BOX(hbox_3), button_S, FALSE, FALSE, 0);
		};
		
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3, TRUE, TRUE, 0);
		gtk_box_pack_start(GTK_BOX(vbox_1), expander_MHD_ctl[i], TRUE, TRUE, 0);
		
		
        if(iflag_read_mhd > 0){
            if(i == 0){add_platoform_box(mhd_ctl->files, vbox_2[i]);};
            if(i == 1){add_platoform_box(mhd_ctl->org_files, vbox_2[i]);};
            if(i == 2){add_platoform_box(mhd_ctl->new_files, vbox_2[i]);};
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
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	alloc_SGS_MHD_control_c(mhd_ctl);
	
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
