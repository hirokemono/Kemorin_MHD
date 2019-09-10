
/* kemo_mesh_viewer_glfw_gtk.c*/

#include "kemo_mesh_viewer_glfw_gtk.h"

#define NPIX_X  800
#define NPIX_Y  640

struct kemoviewer_type *single_kemoview;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;
int iflag_gtk_focus = 0;

GtkWidget *gtk_win;
struct main_buttons *mbot;

static void mainloop_4_glfw(){
	int iflag;
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win)){
		iflag = glfwWindowShouldClose(glfw_win);
		/* Render here */
		glClear(GL_COLOR_BUFFER_BIT);
		display(glfw_win);
		
		/* Poll for and process events */
		glfwPollEvents();
		
		/* Collect GTK ivents */
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
	printf ("Focus-in GTK window \n");
	iflag_gtk_focus = 1;
	iflag_glfw_focus = 0;
	return;
}

static void gtkFocus_out_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
	printf ("Focus-out GTK window \n");
	iflag_gtk_focus = 0;
	if(glfwGetWindowAttrib(glfw_win, GLFW_FOCUSED) == GLFW_TRUE) iflag_glfw_focus = 1;
	return;
}

/* Callbacks for GLFW */ 

void glfwWindowFocus_CB(GLFWwindow *window, int focused) {
	if(focused){
		printf("GLFW window focused\n");
		iflag_glfw_focus = 1;
	} else {
		printf("GLFW window lost focuse\n");
		iflag_glfw_focus = 0;
	}
}

void glfwWindowclose_CB(GLFWwindow *window) {
	gtk_widget_destroy(gtk_win);
	glfwSetWindowShouldClose(window, GLFW_TRUE);
	iflag_glfw_focus = 0;
	return;
}

/* Main GTK window */

void kemoview_main_window(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *vbox;
	
	GtkWidget *quitButton;
	
	mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
	mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));
	mbot->color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	
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
	
	mbot->vbox_menu = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), quitButton, FALSE, FALSE, 0);
	make_gtk_main_menu_box(kemoviewer_data, mbot, gtk_win);
	gtk_box_pack_start(GTK_BOX(vbox), mbot->vbox_menu, FALSE, FALSE, 0);

	gtk_widget_show(quitButton);
	gtk_widget_show(vbox);
	gtk_widget_show(gtk_win);
	return;
}

/* Main routine for C */

int draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
	int iflag_retinamode = 0;
    GLboolean bStereo;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	/*! Create viewer window*/
	kemoview_set_retinamode(iflag_retinamode);
	kemoview_set_windowsize(NPIX_X, NPIX_Y);

	/*! glfw Initialization*/
	if(!glfwInit()) return -1;

	glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // for MacOS
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	glfwWindowHint(GLFW_RED_BITS, 8);
	glfwWindowHint(GLFW_GREEN_BITS, 8);
	glfwWindowHint(GLFW_BLUE_BITS, 8);
	glfwWindowHint(GLFW_ALPHA_BITS, 8);
	glfwWindowHint(GLFW_DEPTH_BITS, 24);
	glfwWindowHint(GLFW_STENCIL_BITS, 8);
	glfwWindowHint(GLFW_STENCIL_BITS, 8);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
	
	glfwWindowHint(GLFW_SAMPLES, 4);
	if(iflag_streo_shutter == SHUTTER_ON){
		glfwWindowHint(GLFW_STEREO, GLFW_FALSE);
	} else{
		glfwWindowHint(GLFW_STEREO, GLFW_FALSE);
	};
	if(iflag_retinamode == 1){
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_TRUE);
	} else{
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_FALSE);
	};
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init(&narg_glut, &arg_glut);
    
	/* Create a windowed mode window and its OpenGL context */
	glfw_win = open_kemoviwer_window(NPIX_X, NPIX_Y);
	
	fprintf(
			stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	/*! set callback for GLfw*/
	kemoviewer_reset_to_init_angle();
	glfw_view_modifier_init(glfw_win);
	/* set callback for window focus */
	glfwSetWindowFocusCallback(glfw_win, glfwWindowFocus_CB);
	/* set callback for window focus */
	glfwSetWindowCloseCallback(glfw_win, glfwWindowclose_CB);
	
	/* ! set the perspective and lighting */
	kemoview_init_background_color();
	kemoview_init_lighting(iflag_core_profile);
	kemoview_init_phong_light_list();
	
	iflag_gtk_focus = 1;
	glClear(GL_COLOR_BUFFER_BIT);
	display(glfw_win);
	glfwPollEvents();
	glfwPostEmptyEvent();
	
	kemoview_main_window(single_kemoview);
	mainloop_4_glfw();
	glfwTerminate();
	
	free(mbot->color_vws);
	free(mbot->view_menu);
	free(mbot);
	return 0;
};

