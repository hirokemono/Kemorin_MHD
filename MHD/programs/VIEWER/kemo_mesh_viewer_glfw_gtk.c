
/* kemo_mesh_viewer_glfw_gtk.c*/

#include <math.h>
#include "kemo_mesh_viewer_glfw_gtk.h"
#include "set_texture_4_psf.h"
#include "move_draw_objects_gl.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;
struct kemoviewer_gl_type *kemoview_gl;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;
int iflag_glfw_end = 0;
int iflag_gtk_focus = 0;

int iflag_msg_fade;
float message_opacity;
double msg_timer_start;
double delta_t;

int iflag_fast_prev;
int iflag_fast_draw;
double fast_draw_start;


GtkWidget *gtk_win;

struct main_buttons *mbot;

static void mainloop_4_glfw(){
    int icou = 0;
    int jcou = 0;
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win)){
		glfwWindowShouldClose(glfw_win);
		
		if(glfwGetWindowAttrib(glfw_win, GLFW_FOCUSED) != 0){
            glfwPollEvents();
            icou++;
        }
		
        if(iflag_msg_fade == 1){
	        delta_t = glfwGetTime() - msg_timer_start;
    	    if(delta_t < 4.5){
	    	    message_opacity = log10(10.0 - 2.0*delta_t);
                kemoview_set_message_opacity(message_opacity,
                                             single_kemoview);
            	draw_full(single_kemoview);
            }else{ 
                iflag_msg_fade = 0;
            };
        };
        
        if(iflag_fast_draw == 1){
            if(iflag_fast_prev == 0){
                fast_draw_start = glfwGetTime();
                iflag_fast_prev = 1;
            };

            delta_t = glfwGetTime() - fast_draw_start;
            if(delta_t > 1.5){
                draw_full(single_kemoview);
                iflag_fast_prev = 0;
            	iflag_fast_draw = 0;
            };
        };

/*
            printf("gtk callback %d %d %d %d \n", icou, jcou,
            glfwGetWindowAttrib(glfw_win, GLFW_FOCUSED),
            (int) gtk_window_is_active(gtk_win));
 */

        /* Collect GTK events */
        if(mbot == NULL) return;
		if(iflag_glfw_end == 1) return;
        set_viewmatrix_value(single_kemoview, mbot->view_menu, gtk_win);

		if(glfwGetWindowAttrib(glfw_win, GLFW_FOCUSED) == 0){
            while (g_main_context_pending(NULL)) g_main_context_iteration(NULL, TRUE);
            jcou++;
        };
//        if(icou%10000==0 || jcou%10000==0) printf("icou %d %d\n", icou, jcou);
	};
	return;
}

/* Callback functions for GTK */

static void gtkWindowclose_CB(GtkButton *button, gpointer user_data){
	gtk_widget_destroy(gtk_win);
	glfwSetWindowShouldClose(glfw_win, GLFW_TRUE);
//	iflag_glfw_focus = 0;
//	iflag_gtk_focus = 0;
}

static void gtkFocus_in_CB (GtkWidget *window, GtkDirectionType direction, gpointer user_data){
/*	printf ("Focus-in GTK window \n"); */
//	iflag_gtk_focus = 1;
//	iflag_glfw_focus = 0;
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
//		iflag_glfw_focus = 1;
	} else {
/*		printf("GLFW window lost focuse\n"); */
//		iflag_glfw_focus = 0;
	}
}

void glfwWindowclose_CB(GLFWwindow *window) {
	gtk_widget_destroy(gtk_win);
	glfwSetWindowShouldClose(window, GLFW_TRUE);
//	iflag_glfw_focus = 0;
//	iflag_gtk_focus = 0;
	iflag_glfw_end = 1;
	return;
}

void dropFileToGlfw_CB(GLFWwindow *window, int num, const char **paths) {
	struct kv_string *filename;
	for (int i = 0; i < num; i++) {
		filename = kemoview_init_kvstring_by_string(paths[i]);
		open_kemoviewer_file_glfw(single_kemoview, kemoview_gl,
                                  filename, mbot, gtk_win);
	}
}

void windowSizeCB(GLFWwindow *window, int width, int height) {
    int nx_buf, ny_buf;
	glfwGetFramebufferSize(glfw_win, &nx_buf, &ny_buf);
	
    message_opacity = 1.0;
	kemoview_update_projection_by_viewer_size(nx_buf, ny_buf,
                                              width, height,
                                              single_kemoview);
	kemoview_set_message_opacity(message_opacity, single_kemoview);
	glViewport(IZERO, IZERO, (GLint) nx_buf, (GLint) ny_buf);
    iflag_msg_fade = 1;
    msg_timer_start = glfwGetTime();
	
    if(mbot == NULL) return;
    update_windowsize_menu(single_kemoview, mbot->view_menu, gtk_win);
/*    printf("retinemode %d\n", kemoview_get_retinamode(single_kemoview)); */
}

void frameBufferSizeCB(GLFWwindow *window, int nx_buf, int ny_buf){
	int npix_x, npix_y;
	glfwGetWindowSize(window, &npix_x, &npix_y);
/*	printf("frameBufferSizeCB %d %d\n", nx_buf, ny_buf); */
	
    message_opacity = 1.0;
	kemoview_update_projection_by_viewer_size(nx_buf, ny_buf,
                                              npix_x, npix_y,
                                              single_kemoview);
    kemoview_set_message_opacity(message_opacity, single_kemoview);
	glViewport(IZERO, IZERO, (GLint) nx_buf, (GLint) ny_buf);
    iflag_msg_fade = 1;
    msg_timer_start = glfwGetTime();
	
    if(mbot == NULL) return;
	update_windowsize_menu(single_kemoview, mbot->view_menu, gtk_win);
}


/* Main GTK window */
static void open_file_CB(GtkButton *button, gpointer user_data){
    struct kv_string *filename;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_sgl_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

    int iflag_set = kemoview_gtk_read_file_select(button, user_data);
    if(iflag_set == IZERO) return;
    GtkEntry *entry = GTK_ENTRY(user_data);
    GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
    filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    
    open_kemoviewer_file_glfw(kemo_sgl, kemo_sgl_gl, filename,
                              mbot, window_main);
    return;
};

/*
static void gtkhidetest_CB(GtkButton *button, gpointer user_data){
    struct main_buttons *mbot = (struct main_buttons *)user_data;
    gchar * text = gtk_button_get_label(button);
    char test1[1];
    test1[0] = text[1];
    if(test1[0] == 110){
        gtk_button_set_label(button, "Off");
        gtk_widget_set_sensitive(mbot->expander_view, FALSE);
        gtk_widget_set_sensitive(mbot->expander_pref, FALSE);
//        sel_mesh_menu_box(mbot, FALSE);
    }else if(test1[0] == 102){
        gtk_button_set_label(button, "On");
        gtk_widget_set_sensitive(mbot->expander_view, TRUE);
        gtk_widget_set_sensitive(mbot->expander_pref, TRUE);
//        sel_mesh_menu_box(mbot, TRUE);
    };
    return;
}
*/
void kemoview_main_window(struct kemoviewer_type *kemoviewer_data){
	mbot = init_main_buttons(kemoviewer_data);
	
    gtk_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
    gtk_window_set_title(GTK_WINDOW(gtk_win), "CalypsoView menu");
    gtk_widget_set_size_request(gtk_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(gtk_win), 5);
    g_signal_connect(G_OBJECT(gtk_win), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
//    g_signal_connect(G_OBJECT(gtk_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
//    g_signal_connect(G_OBJECT(gtk_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *menuGrid = make_gtk_menu_button(mbot, single_kemoview);
    
    /*
    GtkWidget *testButton = gtk_button_new_with_label("On");
    g_signal_connect(G_OBJECT(testButton), "clicked",
                     G_CALLBACK(gtkhidetest_CB), (gpointer) mbot);
    */
    mbot->menuHbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    mbot->vbox_menu = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);


    GtkWidget *entry_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer)   gtk_win);
    g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer)  mbot);
    g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) single_kemoview);
    g_object_set_data(G_OBJECT(entry_file), "kemoview_gl", (gpointer) kemoview_gl);

    GtkWidget *open_Button = gtk_button_new_with_label("Open...");
    g_signal_connect(G_OBJECT(open_Button), "clicked",
                     G_CALLBACK(open_file_CB), (gpointer)entry_file);
    GtkWidget *hbox_open = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_open), menuGrid, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), gtk_label_new("File: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), entry_file, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_open), open_Button, FALSE, FALSE, 0);
    
    GtkWidget *entry_image_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(entry_image_file), "parent", (gpointer) gtk_win);
    
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_open, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), topbox, FALSE, FALSE, 0);
	
    iflag_fast_prev = 0;
    
    GtkWidget *quitButton = gtk_button_new_with_label("Quit");
    g_signal_connect(G_OBJECT(quitButton), "clicked",
                     G_CALLBACK(gtkWindowclose_CB), NULL);
    make_gtk_main_menu_box(mbot, quitButton, gtk_win, 
                           single_kemoview, kemoview_gl);
    
    GtkWidget *takobox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(takobox), mbot->vbox_menu, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->menuHbox), takobox, FALSE, FALSE, 0);
    
    mbot->psf_gmenu->iflag_psfBox =    0;
    mbot->fline_menu->iflag_flineBox = 0;
    mbot->mesh_vws->iflag_meshBox =    0;
    mbot->id_current[0] = 1;
    
    activate_evolution_menu(single_kemoview, mbot->itemTEvo);

    GtkWidget *vbox_main = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_main), mbot->menuHbox, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(gtk_win), vbox_main);
    
	gtk_widget_show(quitButton);
    gtk_widget_show(copyButton);
	gtk_widget_show(vbox_main);
	gtk_widget_show_all(mbot->menuHbox);
	gtk_widget_show(gtk_win);
	return;
}

/* Main routine for C */

int draw_mesh_kemo(void) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_retinamode = 1;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
    kemoview_init_cube_buf(single_kemoview);
    kemoview_gl = kemoview_allocate_gl_pointers();
	
	/*! glfw Initialization*/
	if(!glfwInit()) return -1;

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
    glfwWindowHint(GLFW_SAMPLES, 4);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);
	glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
    glfwWindowHint(GLFW_STEREO, GLFW_FALSE);
	    
	/* Create a windowed mode window and its OpenGL context */
    int nx_buf, ny_buf;
	glfw_win = open_kemoviwer_glfw_window(NPIX_X, NPIX_Y);
	glfwGetFramebufferSize(glfw_win, &nx_buf, &ny_buf);
    kemoview_set_windowsize(nx_buf, ny_buf, NPIX_X, NPIX_Y,
                            single_kemoview);
	
	fprintf(
			stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	/*! set callback for GLfw*/
	kemoviewer_reset_to_init_angle(single_kemoview);
	glfw_callbacks_init(single_kemoview, kemoview_gl);
	
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
    kemoview_init_background_color(single_kemoview);
	kemoview_init_lighting(single_kemoview);
    kemoview_gl_background_color(single_kemoview);
    kemoview_gl_init_lighting(kemoview_gl);
	kemoview_init_phong_light_list(single_kemoview);
	
//	iflag_gtk_focus = 1;
	glClear(GL_COLOR_BUFFER_BIT);
	draw_full(single_kemoview);
	glfwPollEvents();
	glfwPostEmptyEvent();
	
    /*! GTK Initialization*/
    /* gtk_set_locale(); */
    gtk_init(&narg_glut, &arg_glut);
	kemoview_main_window(single_kemoview);
	mainloop_4_glfw();
	glfwTerminate();
	
	dealloc_main_buttons(mbot);
	return 0;
};

