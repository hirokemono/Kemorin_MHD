
#include <gtk/gtk.h>
#include "t_control_chara_IO.h"
#include "t_SGS_MHD_control_c.h"

int iflag_read_mhd = 0;
struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;

GtkWidget *entry_3, *entry_4, *entry_5;

double rtest = 2.5;
int ntest = 66;
char *ctest = "ahahahaha";
struct chara_ctl_item item_test = {55, "tako_tako"};
struct chara_ctl_item *ptem_test;

static void cb_New(GtkButton *button, gpointer data)
{
	
	/*
     mhd_ctl = alloc_SGS_MHD_control_c();
	*/
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
         mhd_ctl = alloc_SGS_MHD_control_c();
		iflag_read_mhd = 1;
		*/
		read_SGS_MHD_control_file_c(read_file_name, buf, mhd_ctl);
		
		gtk_entry_set_text(GTK_ENTRY(entry_3), (const gchar *) mhd_ctl->files->sph_file_prefix_c->c_tbl);
		
		g_free(read_file_name);
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

static void cb5_entry(GtkEntry *entry, gpointer data)
{
	struct chara_ctl_item *item_recv = (struct chara_ctl_item *) data;
	
	printf("ntest: %d: %s\n", item_recv->iflag, item_recv->c_tbl);
//	struct chara_ctl_item *c_item = data;
//	printf("file %d : %s \n", c_item->iflag, c_item->c_tbl);
	if(mhd_ctl->files->sph_file_prefix_c->c_tbl != NULL) {
//		mhd_ctl->files->sph_file_prefix_c->c_tbl = gtk_entry_get_text(entry);
		printf("file %d: %s \n", mhd_ctl->files->sph_file_prefix_c->iflag, 
					mhd_ctl->files->sph_file_prefix_c->c_tbl);
	};
}

void draw_MHD_control_list(GtkWidget *vbox_0, struct SGS_MHD_control_c *MHD_c, struct chara_ctl_item *ptem_t){
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
			
			
			printf("item_test in main: %d: %s\n", item_test.iflag, item_test.c_tbl);
			printf("ctem_test in main: %d: %s\n", ptem_t->iflag, ptem_t->c_tbl);
			printf("sph_file_prefix_c in main: %d: %s\n", MHD_c->files->sph_file_prefix_c->iflag, 
						MHD_c->files->sph_file_prefix_c->c_tbl);
			entry_5 = gtk_entry_new();
			g_signal_connect(G_OBJECT(entry_5), "activate", G_CALLBACK(cb5_entry), 
//						(gpointer) ptem_t);
						(gpointer) MHD_c->files->sph_file_prefix_c);
			gtk_box_pack_start(GTK_BOX(hbox_3), entry_5, TRUE, TRUE, 0);
			
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
	gtk_box_pack_start(GTK_BOX(vbox_0), expander_Top, TRUE, TRUE, 0);
		
};

void draw_MHD_control_bottuns(GtkWidget *vbox_0){
	GtkWidget *button_N, *button_O, *button_S, *button_Q;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *entry;
	
	
    mhd_ctl = alloc_SGS_MHD_control_c();

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
	
	gtk_box_pack_start(GTK_BOX(vbox_0), hbox, FALSE, FALSE, 0);
	
}


int main(int argc, char** argv)
{
	GtkWidget *vbox_0;
//	GtkWidget *scroll_window;
	
    mhd_ctl = alloc_SGS_MHD_control_c();
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
	
	draw_MHD_control_list(vbox_0, mhd_ctl, ptem_test);
	draw_MHD_control_bottuns(vbox_0);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}
