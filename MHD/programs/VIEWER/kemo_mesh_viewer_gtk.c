
/* kemo_mesh_viewer_gtk.c*/

#include "kemo_mesh_viewer_gtk.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;

int iflag_gtk_focus = 0;

GtkWidget *gtk_win;
GtkWidget *kemoview_win;
GtkWidget *kemoview_area;
GdkGLContext *gl_context;

struct main_buttons *mbot;

/* Callback functions for GTK */

static void gtkWindowclose_CB(GtkButton *button, gpointer user_data){
	gtk_widget_destroy(gtk_win);
	gtk_widget_destroy(kemoview_win);
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

GdkGLContext * tako_context(GtkGLArea *area, gpointer user_data){
	int mj, mn;
	GdkGLContext *context = gtk_gl_area_get_context(area);
	gdk_gl_context_set_required_version(context, 3, 2);
	gdk_gl_context_get_required_version(context, &mj, &mn);
	printf("tako_context %d %d\n", mj, mn);
	gtk_gl_area_get_required_version(kemoview_area, &mj, &mn);
	printf("mj, mn init:  %d, %d \n", mj, mn);
	return context;
}

static gboolean realiseCB(GtkGLArea *area, GdkGLContext *context)
{
	int mj, mn;
	
	gtk_gl_area_get_required_version(area, &mj, &mn);
	printf("mj, mn:  %d, %d \n", mj, mn);
	
	printf("Tako\n");
	gtk_gl_area_make_current(GTK_GL_AREA(area));
  if (gtk_gl_area_get_error (GTK_GL_AREA(area)) != NULL)
  {
    printf("failed to initialiize buffers\n");
    return FALSE;
	}

	fprintf(stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	return TRUE;
	
	/* Make the window's context current */
	gtk_widget_set_size_request(area, NPIX_X, NPIX_Y);
	
	gtk_gl_area_set_has_alpha(GTK_GL_AREA(area), TRUE);
	gtk_gl_area_set_has_depth_buffer(GTK_GL_AREA(area), TRUE);
	gtk_gl_area_set_has_stencil_buffer(GTK_GL_AREA(area), TRUE);
	gtk_gl_area_set_auto_render(GTK_GL_AREA(area), TRUE);
	
	/* ! set the perspective and lighting */
	kemoviewer_reset_to_init_angle();
    kemoview_init_gl_background_color();
	kemoview_init_lighting();
    kemoview_gl_init_lighting();
	kemoview_init_phong_light_list();
	
	return TRUE;
};

static gboolean renderCB(GtkGLArea *area, GdkGLContext *context)
{
	return TRUE;
};

/* Callbacks for GLFW

void windowSizeCB(GLFWwindow *window, int width, int height) {
	/*	printf("windowSizeCB %d %d\n", width, height);
	kemoview_update_projection_by_viewer_size(width, height);
    kemoview_set_message_opacity(1.0);
	glViewport(IZERO, IZERO, (GLint) width, (GLint) height);
	
	update_windowsize_menu(mbot->view_menu, gtk_win);
}
*/
/* Main GTK window */

void kemoview_main_window(struct kemoviewer_type *kemoviewer_data, GtkWidget *vbox){
	
	GtkWidget *quitButton;
	GtkWidget *takoButton;
	
	mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
	mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));
	mbot->color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	mbot->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
	mbot->lightparams_vws = init_light_views_4_viewer(kemoviewer_data->kemo_shaders->lights);
	
	gtk_window_set_title(GTK_WINDOW(gtk_win), "Kemoviewer menu");
	gtk_widget_set_size_request(gtk_win, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(gtk_win), 5);
	g_signal_connect(G_OBJECT(gtk_win), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
	
	quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtkWindowclose_CB), NULL);
	
	mbot->menuHbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), quitButton, FALSE, FALSE, 0);
	make_gtk_main_menu_box(mbot, gtk_win);
	gtk_box_pack_start(GTK_BOX(vbox), mbot->menuHbox, FALSE, FALSE, 0);
	
	
	
	gtk_widget_show_all(mbot->menuHbox);
	gtk_widget_show(quitButton);
	return;
}

/* Main routine for C */

int draw_mesh_kemo(void) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_retinamode = 0;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
	
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init(&narg_glut, &arg_glut);
	
	gtk_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	kemoview_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(kemoview_win), "Kemoviewer");

	kemoview_area = open_kemoviwer_gl_panel(NPIX_X, NPIX_Y);
	gtk_widget_set_vexpand(kemoview_area, TRUE);
	gtk_widget_set_hexpand(kemoview_area, TRUE);

	gtk_gl_area_set_required_version(kemoview_area, 3, 3);
	
//	g_signal_connect(G_OBJECT(kemoview_area), "create-context", G_CALLBACK(tako_context), NULL);
	g_signal_connect(G_OBJECT(kemoview_area), "realize", G_CALLBACK(realiseCB), NULL);
	g_signal_connect(G_OBJECT(kemoview_area), "render", G_CALLBACK(renderCB), NULL);
	gtk_callbacks_init();
	
	/* Set callback for drug and Drop into window */
	
	/* Set callback for window size changing */
//	glfwSetWindowSizeCallback(glfw_win, windowSizeCB);
	
	
	iflag_gtk_focus = 1;
//	glClear(GL_COLOR_BUFFER_BIT);
	draw_full();
	
	GtkWidget *vbox;
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	kemoview_main_window(single_kemoview, vbox);
	
    /*! Create viewer window*/
    kemoview_set_windowsize(NPIX_X, NPIX_Y, NPIX_X, NPIX_Y);

    gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER(gtk_win), vbox);
	gtk_container_add(GTK_CONTAINER(kemoview_win), kemoview_area);
	gtk_widget_show(gtk_win);
	gtk_widget_show_all(kemoview_win);
	
	gtk_main();
	
	dealloc_light_views_4_viewer(mbot->lightparams_vws);
	//	free(mbot->lightparams_vws);
	free(mbot->mesh_vws);
	free(mbot->color_vws);
	free(mbot->view_menu);
	free(mbot);
	return 0;
};
