
/* viewer_GTK_test.c */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <gtk/gtk.h>


#include "kemoviewer.h"
#include "kemoview_gtk_PSF_menu.h"
#include "kemoview_gtk_mesh_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_main_menu.h"
#include "kemoview_gtk_viewmatrix_menu.h"

struct kemoviewer_type *single_kemoview;
GtkWidget *gtk_win;
int iflag_gtk_focus = 0;
struct main_buttons *mbot;

/* Callback functions for GTK */

static void gtkWindowclose_CB(GtkButton *button, gpointer user_data){
	gtk_widget_destroy(gtk_win);
	iflag_gtk_focus = 0;
}

static void gtkFocus_in_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
	printf ("Focus-in GTK window \n");
	iflag_gtk_focus = 1;
	return;
}

static void gtkFocus_out_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
	printf ("Focus-out GTK window \n");
	iflag_gtk_focus = 0;
	return;
}


void gtk_test_window(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *vbox;
	
	GtkWidget *quitButton;
	
	mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
	mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));
	mbot->color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	mbot->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
	mbot->lightparams_vws =  init_light_views_4_viewer(kemoviewer_data->kemo_buffers->kemo_lights);
	
	gtk_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(gtk_win), "Mesh viewer");
	gtk_widget_set_size_request(gtk_win, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(gtk_win), 5);
	g_signal_connect(G_OBJECT(gtk_win), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
	
	quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtkWindowclose_CB), NULL);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(gtk_win), vbox);
	
	mbot->menuHbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), quitButton, FALSE, FALSE, 0);
	make_gtk_main_menu_box(mbot, gtk_win, single_kemoview);
	gtk_box_pack_start(GTK_BOX(vbox), mbot->menuHbox, FALSE, FALSE, 0);

	gtk_widget_show(quitButton);
	gtk_widget_show(vbox);
	gtk_widget_show_all(mbot->menuHbox);
	gtk_widget_show(gtk_win);
	return;
}


int draw_gtk_menu_kemo(void) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_retinamode = 0;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
    kemo_gl = kemoview_allocate_gl_pointers();

	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init(&narg_glut, &arg_glut);
   	
	iflag_gtk_focus = 1;
	
	gtk_test_window(single_kemoview);
	gtk_main();
	
	//	free(mbot->lightparams_vws);
	free(mbot->mesh_vws);
	free(mbot->color_vws);
	free(mbot->view_menu);
	free(mbot);
	return 0;
};

int main(int argc, char *argv[]){
    draw_gtk_menu_kemo();
	return 0;
};


