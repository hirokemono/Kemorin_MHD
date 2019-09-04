
/* kemo_mesh_viewer_glfw_gtk.c*/

#include "kemo_mesh_viewer_glfw_gtk.h"

#define NPIX_X  800
#define NPIX_Y  640

struct kemoviewer_type *single_kemoview;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;

GtkWidget *window_main;

/* Callback functions for GTK */

static void gtkWindowclose_CB(GtkButton *button, gpointer user_data){
	glfwTerminate();
	iflag_glfw_focus = 0;
	gtk_widget_destroy(window_main);
	gtk_main_quit();
}

static void focus_in_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
	printf ("Focus-in GTK window \n");
//	gtk_main();
	return;
}

static void focus_out_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
	printf ("Focus-out GTK window \n");
	iflag_glfw_focus = 1;
	
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win))
	{
		if(iflag_glfw_focus == 0) break;
		//*		printf ("glfw loop \n"); */
		/* Render here */
		glClear(GL_COLOR_BUFFER_BIT);
		
		display(glfw_win);
		
		/* Poll for and process events */
		glfwPollEvents();
		
	}
	
	return;
}

/* Callbacks for GLFW */ 

void glfwWindowFocus_CB(GLFWwindow *window, int focused) {
	if(focused){
		printf("GLFW window focused\n");
		iflag_glfw_focus = 1;
//		gtk_main_quit();
	} else {
		printf("GLFW window lost focuse\n");
		iflag_glfw_focus = 0;
//		gtk_main();
	}
}

void glfwWindowclose_CB(GLFWwindow *window) {
	glfwTerminate();
	iflag_glfw_focus = 0;
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	return;
}

/* Main GTK window */

void kemoview_main_window(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *vbox;
	
	GtkWidget *quitButton;
	
	quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtkWindowclose_CB), NULL);
	
	window_main = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_main), "Mesh viewer");
	gtk_widget_set_size_request(window_main, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_main), 5);
	g_signal_connect(G_OBJECT(window_main), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
	g_signal_connect(G_OBJECT(window_main), "focus-in-event", G_CALLBACK(focus_in_CB), NULL);
	g_signal_connect(G_OBJECT(window_main), "focus-out-event", G_CALLBACK(focus_out_CB), NULL);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), quitButton, FALSE, FALSE, 0);
	add_gtk_main_menu(kemoviewer_data, window_main, vbox);

	gtk_widget_show_all(window_main);
	gtk_main();
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
	gtk_init (&narg_glut, &arg_glut);
    
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
	
	kemoview_main_window(single_kemoview);
	
	return 0;
};

