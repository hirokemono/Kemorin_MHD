
/* kemo_mesh_viewer_glfw_gtk.c*/

#include "kemo_mesh_viewer_glfw_gtk.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;
int iflag_glfw_end = 0;
int iflag_gtk_focus = 0;

GtkWidget *gtk_win;

struct main_buttons *mbot;

static void mainloop_4_glfw(){
	int iflag;
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win)){
		iflag = glfwWindowShouldClose(glfw_win);
		
		glfwPollEvents();
		
		/* Collect GTK events */
		if(iflag_glfw_end == 1) return;
		update_viewmatrix_menu(mbot->view_menu, gtk_win);
		while (gtk_events_pending()) gtk_main_iteration();
	};
	return;
}

/* Callback functions for GTK */

static void gtkWindowclose_CB(GtkButton *button, gpointer user_data){
	gtk_widget_destroy(gtk_win);
	glfwSetWindowShouldClose(glfw_win, GLFW_TRUE);
	iflag_glfw_focus = 0;
	iflag_gtk_focus = 0;
}

static void gtkFocus_in_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
/*	printf ("Focus-in GTK window \n"); */
	iflag_gtk_focus = 1;
	iflag_glfw_focus = 0;
	return;
}

static void gtkFocus_out_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
/*	printf ("Focus-out GTK window \n"); */
	iflag_gtk_focus = 0;
	if(glfwGetWindowAttrib(glfw_win, GLFW_FOCUSED) == GLFW_TRUE) iflag_glfw_focus = 1;
	return;
}

/* Callbacks for GLFW */ 

void glfwWindowFocus_CB(GLFWwindow *window, int focused) {
	if(focused){
/*		printf("GLFW window focused\n"); */
		iflag_glfw_focus = 1;
	} else {
/*		printf("GLFW window lost focuse\n"); */
		iflag_glfw_focus = 0;
	}
}

void glfwWindowclose_CB(GLFWwindow *window) {
	gtk_widget_destroy(gtk_win);
	glfwSetWindowShouldClose(window, GLFW_TRUE);
	iflag_glfw_focus = 0;
	iflag_gtk_focus = 0;
	iflag_glfw_end = 1;
	return;
}

void dropFileToGlfw_CB(GLFWwindow *window, int num, const char **paths) {
	struct kv_string *filename;
	for (int i = 0; i < num; i++) {
		printf("%s\n", paths[i]);
		filename = kemoview_init_kvstring_by_string(paths[i]);
		open_kemoviewer_file_glfw(filename, mbot, gtk_win);
	}
}

void windowSizeCB(GLFWwindow *window, int width, int height) {
    int nx_buf, ny_buf;
	glfwGetFramebufferSize(glfw_win, &nx_buf, &ny_buf);
	
	kemoview_update_projection_by_viewer_size(nx_buf, ny_buf, width, height);
	kemoview_set_windowsize_message(1);
	glViewport(IZERO, IZERO, (GLint) nx_buf, (GLint) ny_buf);
	
	update_windowsize_menu(mbot->view_menu, gtk_win);
	kemoview_set_windowsize_message(0);
/*    printf("retinemode %d\n", kemoview_get_retinamode()); */
}

void frameBufferSizeCB(GLFWwindow *window, int nx_buf, int ny_buf){
	int npix_x, npix_y;
	glfwGetWindowSize(window, &npix_x, &npix_y);
/*	printf("frameBufferSizeCB %d %d\n", nx_buf, ny_buf); */
	
	kemoview_update_projection_by_viewer_size(nx_buf, ny_buf, npix_x, npix_y);
	kemoview_set_windowsize_message(1);
	glViewport(IZERO, IZERO, (GLint) nx_buf, (GLint) ny_buf);
	
	update_windowsize_menu(mbot->view_menu, gtk_win);
	kemoview_set_windowsize_message(0);
}

/* Main GTK window */

void kemoview_main_window(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *quitButton;
	GtkWidget *vbox_main;
	
	mbot = init_main_buttons(kemoviewer_data);
	
	gtk_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(gtk_win), "CalypsoView menu");
	gtk_widget_set_size_request(gtk_win, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(gtk_win), 5);
	g_signal_connect(G_OBJECT(gtk_win), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
	
	quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtkWindowclose_CB), NULL);
	
	
	
	mbot->menuHbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    mbot->vbox_menu = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), quitButton, FALSE, FALSE, 0);
	
	make_gtk_main_menu_box(mbot, gtk_win);
	
	vbox_main = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_main), mbot->menuHbox, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(gtk_win), vbox_main);
	
	gtk_widget_show(quitButton);
	gtk_widget_show(vbox_main);
	gtk_widget_show_all(mbot->menuHbox);
	gtk_widget_show(gtk_win);
	return;
}

/* Main routine for C */

int draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_retinamode = 1;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
	kemoview_set_view_integer(ISET_SHUTTER, iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_view_integer(ISET_ANAGYLYPH, ANAGLYPH_OFF);
	} else {
		kemoview_set_view_integer(ISET_ANAGYLYPH, ANAGLYPH_ON);
	};
	
	/*! glfw Initialization*/
	if(!glfwInit()) return -1;

	glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

#ifdef __APPLE__
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // for MacOS
	if(iflag_retinamode == 1){
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_TRUE);
	} else{
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_FALSE);
	};
#endif
	
	glfwWindowHint(GLFW_RED_BITS, 8);
	glfwWindowHint(GLFW_GREEN_BITS, 8);
	glfwWindowHint(GLFW_BLUE_BITS, 8);
	glfwWindowHint(GLFW_ALPHA_BITS, 8);
	glfwWindowHint(GLFW_DEPTH_BITS, 24);
	glfwWindowHint(GLFW_STENCIL_BITS, 8);
	glfwWindowHint(GLFW_STENCIL_BITS, 8);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		glfwWindowHint(GLFW_STEREO, GLFW_FALSE);
	} else{
		glfwWindowHint(GLFW_STEREO, GLFW_FALSE);
	};
	
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init(&narg_glut, &arg_glut);
    
	/* Create a windowed mode window and its OpenGL context */
    int nx_buf, ny_buf;
	glfw_win = open_kemoviwer_glfw_window(NPIX_X, NPIX_Y);
	glfwGetFramebufferSize(glfw_win, &nx_buf, &ny_buf);
    kemoview_set_windowsize(nx_buf, ny_buf, NPIX_X, NPIX_Y);
	
	fprintf(
			stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	/*! set callback for GLfw*/
	kemoviewer_reset_to_init_angle();
	glfw_callbacks_init();
	
	/* Set Cllback for drug and Drop into window */
	glfwSetDropCallback(glfw_win, dropFileToGlfw_CB);
	
	/* Set callback for window size changing */
	glfwSetWindowSizeCallback(glfw_win, windowSizeCB);
	/* Set callback for window framebuffer size changing
	glfwSetFramebufferSizeCallback(glfw_win, frameBufferSizeCB);
	*/
	/* set callback for window focus */
	glfwSetWindowFocusCallback(glfw_win, glfwWindowFocus_CB);
	/* set callback for window focus */
	glfwSetWindowCloseCallback(glfw_win, glfwWindowclose_CB);
	
	/* ! set the perspective and lighting */
	kemoview_init_background_color();
	kemoview_init_lighting();
	kemoview_init_phong_light_list();
	
	iflag_gtk_focus = 1;
	glClear(GL_COLOR_BUFFER_BIT);
	draw_full();
	glfwPollEvents();
	glfwPostEmptyEvent();
	
	kemoview_main_window(single_kemoview);
	mainloop_4_glfw();
	glfwTerminate();
	
	dealloc_main_buttons(mbot);
	return 0;
};

