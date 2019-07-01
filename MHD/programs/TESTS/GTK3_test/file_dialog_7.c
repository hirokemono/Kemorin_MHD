
#include <gtk/gtk.h>
#include "t_SGS_MHD_control_c.h"
#include "control_elements_IO_GTK.h"
#include "ctl_panel_platforms_GTK.h"
#include "ctl_panel_para_sph_shell_GTK.h"
#include "ctl_panel_SPH_MHD_model_GTK.h"


int iflag_read_mhd = 0;
struct SGS_MHD_control_c *mhd_ctl;

GtkWidget *make_control_hbox();

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
	GtkWidget *hbox_0;
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
        
		hbox_0 = make_control_hbox(vbox_0);
		gtk_box_pack_start(GTK_BOX(vbox_0), hbox_0, TRUE, TRUE, 0);
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
	GtkWidget *parent = GTK_WIDGET(data);
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *write_file_name;


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
	GtkWidget *parent = GTK_WIDGET(data);
    
    /* Four selections for GtkFileChooserAction */
    GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
        GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
    gint response;
    gchar *write_file_name;
    
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
        
        write_spherical_shell_file_c(write_file_name, write_file_name);
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
	
	g_signal_connect(G_OBJECT(button_N), "clicked", G_CALLBACK(cb_New), (gpointer) entry);
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer) entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer) window);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_N, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox0), hbox, FALSE, FALSE, 0);
};

GtkWidget *make_control_hbox(){
	GtkWidget *hbox;
	GtkWidget *vbox_1;
	GtkWidget *hbox_3[NLBL_SGS_MHD_CTL];
    GtkWidget *sph_save_bottun = gtk_button_new_with_label("Save");
	
	int i, ii;
	char *c_label;
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	/* Generate expander */
	get_label_MHD_control_head(c_label);
    expander_Top = gtk_expander_new_with_mnemonic(c_label);
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	get_label_SGS_MHD_ctl(0, c_label);
	hbox_3[0] = make_platoform_hbox(c_label, mhd_ctl->files);
	
	get_label_SGS_MHD_ctl(1, c_label);
	hbox_3[1] = make_platoform_hbox(c_label, mhd_ctl->org_files);
	
	get_label_SGS_MHD_ctl(2, c_label);
	hbox_3[2] = make_platoform_hbox(c_label, mhd_ctl->new_files);
	
    g_signal_connect(G_OBJECT(sph_save_bottun), "clicked", G_CALLBACK(cb_Save_sph), 
                     (gpointer) window);
	get_label_SGS_MHD_ctl(3, c_label);
	hbox_3[3] = make_parallel_shell_hbox(c_label, mhd_ctl->shell_ctl_file_name,
				mhd_ctl->shell_ctl, sph_save_bottun);
	
	get_label_SGS_MHD_ctl(4, c_label);
	hbox_3[4] = make_mhd_model_ctl_hbox(c_label, mhd_ctl->model_ctl);
	
	get_label_SGS_MHD_ctl(5, c_label);
    hbox_3[5] = make_mhd_control_ctl_hbox(c_label, mhd_ctl->control_ctl);
	
	get_label_SGS_MHD_ctl(6, c_label);
    hbox_3[6] = make_empty_ctl_hbox(c_label, &mhd_ctl->iflag_sph_monitor_ctl);
	
	get_label_SGS_MHD_ctl(7, c_label);
	hbox_3[7] = make_empty_ctl_hbox(c_label, &mhd_ctl->iflag_node_monitor_ctl);
	
	get_label_SGS_MHD_ctl(8, c_label);
	hbox_3[8] = make_empty_ctl_hbox(c_label, &mhd_ctl->iflag_visual_control);
	
	get_label_SGS_MHD_ctl(9, c_label);
	hbox_3[9] = make_empty_ctl_hbox(c_label, &mhd_ctl->iflag_zonal_mean_control);
	
	for (i=0;i<NLBL_SGS_MHD_CTL;i++){
		gtk_box_pack_start(GTK_BOX(vbox_1), hbox_3[i], TRUE, TRUE, 0);
	};
	
	get_label_MHD_control_head(c_label);
    hbox = make_expand_ctl_hbox(c_label, &iflag_read_mhd, 700, vbox_1);
    return hbox;
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
	
	set_file_box(vbox_0);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	
	
	gtk_widget_show_all(window);
	gtk_main();
	
	return 0;
}
