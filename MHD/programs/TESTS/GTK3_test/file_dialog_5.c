
#include <gtk/gtk.h>
#include "t_SGS_MHD_control_c.h"
#include "control_elements_IO_c.h"

void draw_MHD_control_list(GtkWidget *vbox0, struct SGS_MHD_control_c *MHD_c, struct chara_ctl_item *ptem_t);


int iflag_read_mhd = 0;
struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;
GtkWidget *vbox_0;

GtkWidget *entry_3, *entry_4, *entry_5;

double rtest = 2.5;
int ntest = 66;
char *ctest = "ahahahaha";
struct chara_ctl_item item_test = {55, "tako_tako"};
struct chara_ctl_item *ptem_test;

static gboolean
boolean_to_text (GBinding *binding,
                 const GValue *source,
                 GValue *target,
                 gpointer dummy G_GNUC_UNUSED)
{
	if (g_value_get_boolean (source)){
		g_value_set_string (target, "On");
	}else{
		g_value_set_string (target, "Off");
	}
	
	return TRUE;
}

static void cb_switch_ctl_item(GtkEntry *toggle, gpointer data)
{
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
    gboolean status = gtk_toggle_button_get_active(toggle);
    
	if(ctl_item->c_tbl != NULL) {
        ctl_item->iflag = 1;
        set_boolean_by_chara_ctl_item((int) status, ctl_item);
		gtk_button_set_label(toggle, ctl_item->c_tbl);
        printf("New value: %s\n", ctl_item->c_tbl);
    };
    return;
}

static GtkWidget *
make_switch_hbox (const char *label, struct chara_ctl_item *ctl_item , gboolean is_on, gboolean is_sensitive){
	GtkWidget *hbox;
	GtkWidget *toggle, *current;
    int iflag=0;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);

	toggle = gtk_toggle_button_new ();
    iflag = find_boolean_from_chara_ctl_item(ctl_item);
    gtk_toggle_button_set_active(toggle, (gboolean) iflag);
    gtk_button_set_label(toggle, ctl_item->c_tbl);
    g_signal_connect(G_OBJECT(toggle), "toggled", G_CALLBACK(cb_switch_ctl_item), 
                     (gpointer) ctl_item);

    gtk_box_pack_start (GTK_BOX (hbox), toggle, FALSE, FALSE, 0);
	gtk_widget_set_sensitive (toggle, is_sensitive);
	gtk_widget_show (toggle);
  return hbox;
}


static void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(ctl_item->c_tbl != NULL) {
		ctl_item->iflag = 1;
		ctl_item->c_tbl = gtk_entry_get_text(entry);
	};
	return;
}

static GtkWidget *
make_text_hbox (const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *tbox, *current;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	tbox = gtk_entry_new();
	g_signal_connect(G_OBJECT(tbox), "activate", G_CALLBACK(cb_chara_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), tbox, TRUE, TRUE, 0);
	
  return hbox;
}


static void cb_int_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct int_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->i_data = gtk_spin_button_get_value_as_int(spinner);
/*		printf("New value: %d\n", ctl_item->i_data); */
	};
	return;
}

static GtkWidget *
make_integer_hbox (const char *label, struct int_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *spinner, *current;
	GtkAdjustment *adjust;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	adjust = gtk_adjustment_new(ctl_item->i_data, 0, 2147483648, 1,
                    100, 21474836);
	spinner = gtk_spin_button_new(adjust, 1, 0);
	g_signal_connect(G_OBJECT(spinner), "value-changed", G_CALLBACK(cb_int_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
	
  return hbox;
}

static void cb_real_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct real_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->r_data = gtk_spin_button_get_value_as_real(spinner);
/*		printf("New value: %f\n", ctl_item->r_data); */
	};
	return;
}

static GtkWidget *
make_real_hbox (const char *label, struct real_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *spinner, *current;
	GtkAdjustment *adjust;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	adjust = gtk_adjustment_new(ctl_item->r_data, -1.0e30, 1.0e30, 0.1,
                    100, 21474836);
	spinner = gtk_spin_button_new(adjust, 0.1, 8);
	g_signal_connect(G_OBJECT(spinner), "value-changed", G_CALLBACK(cb_real_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
	
  return hbox;
}

static void cb_SelectFile(GtkButton *button, gpointer data)
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
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		
		g_free(write_file_name);
		iflag_read_mhd = 0;
		
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

static GtkWidget *
make_filename_hbox (const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *tbox, *current;
    GtkWidget *button_S;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	tbox = gtk_entry_new();
	g_signal_connect(G_OBJECT(tbox), "activate", G_CALLBACK(cb_chara_ctl_item), 
				(gpointer) ctl_item);
	gtk_box_pack_start(GTK_BOX(hbox), tbox, TRUE, TRUE, 0);
	
	button_S = gtk_button_new_with_label("Select");
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_SelectFile), (gpointer)tbox);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	
	return hbox;
}

static void cb_New(GtkButton *button, gpointer data)
{
	draw_MHD_control_list(vbox_0, mhd_ctl, ptem_test);
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
		/*
		mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
		alloc_SGS_MHD_control_c(mhd_ctl);
		iflag_read_mhd = 1;
		*/
		read_SGS_MHD_control_file_c(read_file_name, buf, mhd_ctl);
		
		gtk_entry_set_text(GTK_ENTRY(entry_3), (const gchar *) mhd_ctl->files->sph_file_prefix_c->c_tbl);
		
		g_free(read_file_name);
		
		draw_MHD_control_list(vbox_0, mhd_ctl, ptem_test);
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

static void cb3_entry(GtkEntry *entry, gpointer data)
{
	printf("ntest: %s: %s\n", (char *) data, ctest);
//	struct chara_ctl_item *c_item = data;
//	printf("file %d : %s \n", c_item->iflag, c_item->c_tbl);
	if(mhd_ctl->files->sph_file_prefix_c->c_tbl != NULL) {
//		mhd_ctl->files->sph_file_prefix_c->c_tbl = gtk_entry_get_text(entry);
		printf("file: %s \n", mhd_ctl->files->sph_file_prefix_c->c_tbl);
	};
}

static void cb4_entry(GtkEntry *entry, gpointer data)
{
	printf("ntest: %d: %d\n", *(int *) data, ntest);
//	struct chara_ctl_item *c_item = data;
//	printf("file %d : %s \n", c_item->iflag, c_item->c_tbl);
//	if(mhd_ctl->files->sph_file_prefix_c->c_tbl != NULL) {
//		mhd_ctl->files->sph_file_prefix_c->c_tbl = gtk_entry_get_text(entry);
//		printf("file: %s \n", mhd_ctl->files->sph_file_prefix_c->c_tbl);
//	};
}

void draw_MHD_control_list(GtkWidget *vbox0, struct SGS_MHD_control_c *MHD_c, struct chara_ctl_item *ptem_t){
	GtkWidget *expander_Top;
	GtkWidget *expander_MHD_ctl[NLBL_SGS_MHD_CTL];
	
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *hbox_2[NLBL_SGS_MHD_CTL], *vbox_2[NLBL_SGS_MHD_CTL], *Frame_2[NLBL_SGS_MHD_CTL];
	GtkWidget *hbox_3;
	
	int i;
	char *c_label;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	
	/* Generate expander */
	get_label_MHD_control_head(c_label);
	expander_Top = gtk_expander_new_with_mnemonic(c_label);
	/*g_signal_connect(expander_Top, "notify::expanded", G_CALLBACK(expander_MHD_ctl_callback), NULL);*/
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		get_label_SGS_MHD_ctl(i, c_label);
		expander_MHD_ctl[i] =  gtk_expander_new(c_label);
		gtk_box_pack_start(GTK_BOX(vbox_1), expander_MHD_ctl[i], TRUE, TRUE, 0);
		
		vbox_2[i] = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
		
		if(i == 0){
			hbox_3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
			get_label_platform_ctl(0, c_label);
			gtk_box_pack_start(GTK_BOX(hbox_3), gtk_label_new(c_label), FALSE, FALSE, 0);
			
			entry_3 = gtk_entry_new();
			printf("ctest in main: %s \n", ctest);
			g_signal_connect(G_OBJECT(entry_3), "activate", G_CALLBACK(cb3_entry), 
						(gpointer) ctest);
//						(void *) mhd_ctl->files->sph_file_prefix_c);
			gtk_box_pack_start(GTK_BOX(hbox_3), entry_3, TRUE, TRUE, 0);
			
			entry_4 = gtk_entry_new();
			printf("ntest in main: %d \n", ntest);
			g_signal_connect(G_OBJECT(entry_4), "activate", G_CALLBACK(cb4_entry), 
						(gpointer) &ntest);
//						(void *) mhd_ctl->files->sph_file_prefix_c);
			gtk_box_pack_start(GTK_BOX(hbox_3), entry_4, TRUE, TRUE, 0);
			gtk_box_pack_start(GTK_BOX(vbox_2[0]), hbox_3, FALSE, FALSE, 0);
			
			
			get_label_platform_ctl(0, c_label);
			hbox_3 = make_switch_hbox(c_label, MHD_c->files->debug_flag_c, FALSE, TRUE);
			gtk_box_pack_start(GTK_BOX(vbox_2[0]), hbox_3, FALSE, FALSE, 0);
			
			get_label_platform_ctl(1, c_label);
			hbox_3 = make_integer_hbox(c_label, MHD_c->files->ndomain_c);
			gtk_box_pack_start(GTK_BOX(vbox_2[0]), hbox_3, FALSE, FALSE, 0);
			
			printf("sph_file_prefix_c in main: %d: %s\n", MHD_c->files->sph_file_prefix_c->iflag, 
						MHD_c->files->sph_file_prefix_c->c_tbl);
			get_label_platform_ctl(4, c_label);
			hbox_3 = make_text_hbox(c_label, MHD_c->files->sph_file_prefix_c);
			gtk_box_pack_start(GTK_BOX(vbox_2[0]), hbox_3, FALSE, FALSE, 0);
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

void draw_MHD_control_bottuns(GtkWidget *vbox0){
	GtkWidget *button_N, *button_O, *button_S, *button_Q;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *entry;
	
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	alloc_SGS_MHD_control_c(mhd_ctl);

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
	
}


int main(int argc, char** argv)
{
//	GtkWidget *scroll_window;
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	alloc_SGS_MHD_control_c(mhd_ctl);
	iflag_read_mhd = 1;
	
	gtk_init(&argc, &argv);

	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	vbox_0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	/*
	scroll_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_box_pack_start(GTK_BOX(vbox_0), scroll_window, TRUE, TRUE, 0);
	*/
	
			ptem_test = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
			alloc_ctl_chara_item(ptem_test);
			ptem_test->iflag = 111;
			ptem_test->c_tbl = "gggg";
	
	draw_MHD_control_bottuns(vbox_0);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}
