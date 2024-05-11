
/* kemo_mesh_viewer_glfw_gtk.c*/

#include <math.h>
#include "kemo_mesh_viewer_glfw_gtk.h"
#include "set_texture_4_psf.h"
#include "move_draw_objects_gl.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;
struct kemoviewer_gl_type *kemo_sgl_gl;

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
            while (gtk_events_pending()) gtk_main_iteration();
            jcou++;
        };
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
		filename = kemoview_init_kvstring_by_string(paths[i]);
		open_kemoviewer_file_glfw(single_kemoview, kemo_sgl_gl,
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

static void prefWindowclose_CB (GtkWidget *new_win, gpointer user_data)
{
    GtkWidget *menu_item = GTK_WIDGET(user_data);
    gtk_widget_set_sensitive(menu_item, TRUE);
};

static void pref_menu_CB(GtkWidget *menu_item, gpointer user_data)
{
    struct preference_gtk_menu *pref_gmenu
            = (struct preference_gtk_menu *) g_object_get_data(G_OBJECT(menu_item), "pref_menu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(menu_item), "kemoview");

    GtkWidget *pref_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(pref_win), "Preferences");
    gtk_widget_set_size_request(pref_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(pref_win), 5);
    g_signal_connect(G_OBJECT(pref_win), "destroy", G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
    g_signal_connect(G_OBJECT(pref_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
    g_signal_connect(G_OBJECT(pref_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_pref = init_preference_frame(kemo_sgl, mbot->pref_gmenu, pref_win);
    gtk_container_add(GTK_CONTAINER(pref_win), frame_pref);
    gtk_widget_show_all(pref_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

static void evo_menu_CB(GtkWidget *menu_item, gpointer user_data)
{
    struct evolution_gtk_menu *evo_gmenu
            = (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(menu_item), "tevo_menu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(menu_item), "kemoview");

    GtkWidget *evo_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(evo_win), "Time evolution");
    gtk_widget_set_size_request(evo_win, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(evo_win), 5);
    g_signal_connect(G_OBJECT(evo_win), "destroy", G_CALLBACK(prefWindowclose_CB), G_OBJECT(menu_item));
    g_signal_connect(G_OBJECT(evo_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
    g_signal_connect(G_OBJECT(evo_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
    
    GtkWidget *frame_evo = init_evoluaiton_menu_frame(kemo_sgl, evo_gmenu, evo_win);
    gtk_container_add(GTK_CONTAINER(evo_win), frame_evo);
    gtk_widget_show_all(evo_win);
    gtk_widget_set_sensitive(menu_item, FALSE);
}

static void tako_callback (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
    g_print ("You clicked \"Tako\"\n");
}

/* Main GTK window */
static void gtkCopyToClipboard_CB(GtkButton *button, gpointer user_data){
    struct gl_texure_image *render_image = alloc_kemoview_gl_texure();
    if(kemoview_get_view_type_flag(single_kemoview) == VIEW_STEREO){
        draw_anaglyph_to_rgb_gl(single_kemoview, kemo_sgl_gl, render_image);
    }else{
        draw_objects_to_rgb_gl(single_kemoview, kemo_sgl_gl, render_image);
    }
    
    struct gl_texure_image *fliped_img = alloc_kemoview_gl_texure();
    alloc_draw_psf_texture(render_image->nipxel_xy[0],
                           render_image->nipxel_xy[1],
                           fliped_img);
    flip_gl_bitmap(render_image->nipxel_xy[0], render_image->nipxel_xy[1],
                   render_image->texure_rgba, fliped_img->texure_rgba);
    GdkPixbuf* pixbuf = gdk_pixbuf_new_from_data((const guchar *) fliped_img->texure_rgba,
                                                 GDK_COLORSPACE_RGB, FALSE, 8,
                                                 fliped_img->nipxel_xy[0], fliped_img->nipxel_xy[1],
                                                 (3*fliped_img->nipxel_xy[0]),
                                                 NULL, NULL);

    GtkClipboard *clipboard = (GtkClipboard *) user_data;
    gtk_clipboard_set_image(clipboard, pixbuf);
    dealloc_kemoview_gl_texure(render_image);
    dealloc_kemoview_gl_texure(fliped_img);
    return;
}
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
	GtkWidget *quitButton, *copyButton;
    GtkClipboard *clipboard;
    
    
	mbot = init_main_buttons(kemoviewer_data);
	
    clipboard = gtk_clipboard_get(GDK_SELECTION_PRIMARY);                                                            
    gtk_clipboard_clear(clipboard);                                                                                  
    gtk_clipboard_set_text(clipboard, "", 0);                                                                        

    clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);                                                          
    gtk_clipboard_clear(clipboard);                                                                                
    gtk_clipboard_set_text(clipboard, "", 0);
    
    gtk_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(gtk_win), "CalypsoView menu");
	gtk_widget_set_size_request(gtk_win, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(gtk_win), 5);
	g_signal_connect(G_OBJECT(gtk_win), "destroy", G_CALLBACK(gtkWindowclose_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-in-event", G_CALLBACK(gtkFocus_in_CB), NULL);
	g_signal_connect(G_OBJECT(gtk_win), "focus-out-event", G_CALLBACK(gtkFocus_out_CB), NULL);
	

    GtkWidget *submenu_widget = gtk_menu_new();
    GtkWidget *itemTako = gtk_menu_item_new_with_mnemonic ("Tako");
    gtk_menu_shell_append(GTK_MENU (submenu_widget), itemTako);
    g_signal_connect(itemTako, "activate", G_CALLBACK (tako_callback), NULL);


    GtkWidget *menu_widget = gtk_menu_new();

    GtkWidget *itemPref = gtk_menu_item_new_with_mnemonic("Preferences...");
    g_object_set_data(G_OBJECT(itemPref), "pref_menu", (gpointer) mbot->pref_gmenu);
    g_object_set_data(G_OBJECT(itemPref), "kemoview", (gpointer) kemoviewer_data);
    g_signal_connect(itemPref, "activate", G_CALLBACK (pref_menu_CB), NULL);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), itemPref);

    mbot->updatable->itemTEvo = gtk_menu_item_new_with_mnemonic("Evolution...");
    g_object_set_data(G_OBJECT(mbot->updatable->itemTEvo), "tevo_menu", (gpointer) mbot->updatable->evo_gmenu);
    g_object_set_data(G_OBJECT(mbot->updatable->itemTEvo), "kemoview", (gpointer) kemoviewer_data);
    g_signal_connect(mbot->updatable->itemTEvo, "activate", G_CALLBACK (evo_menu_CB), NULL);
    gtk_menu_shell_append(GTK_MENU_SHELL(menu_widget), mbot->updatable->itemTEvo);

    GtkWidget *imprMi =  gtk_menu_item_new_with_label("Takotako");
    gtk_menu_item_set_submenu(GTK_MENU_ITEM(imprMi), submenu_widget);
    gtk_menu_shell_append(GTK_MENU (menu_widget), imprMi);

    GtkWidget *menuButton = gtk_menu_button_new();
    gtk_menu_button_set_popup (GTK_MENU_BUTTON (menuButton), menu_widget);
    gtk_widget_show_all (menu_widget);

    GtkWidget *menuGrid = gtk_grid_new ();
    gtk_grid_attach (GTK_GRID (menuGrid), menuButton, 1, 1, 1, 1);

    
    quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtkWindowclose_CB), NULL);
    copyButton = gtk_button_new_with_label("Copy");
    g_signal_connect(G_OBJECT(copyButton), "clicked", G_CALLBACK(gtkCopyToClipboard_CB), (gpointer) clipboard);
	/*
    GtkWidget *testButton = gtk_button_new_with_label("On");
    g_signal_connect(G_OBJECT(testButton), "clicked",
                     G_CALLBACK(gtkhidetest_CB), (gpointer) mbot);
    */
    mbot->menuHbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    mbot->vbox_menu = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);

    GtkWidget *topbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(topbox), menuGrid, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(topbox), copyButton, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(topbox), quitButton, TRUE, TRUE, 0);

    
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), topbox, FALSE, FALSE, 0);
	
    iflag_fast_prev = 0;
    GtkWidget *takobox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	make_gtk_main_menu_box(mbot, takobox, gtk_win, single_kemoview, kemo_sgl_gl);

    gtk_box_pack_start(GTK_BOX(takobox), mbot->vbox_menu, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->menuHbox), takobox, FALSE, FALSE, 0);
    
    mbot->updatable->psf_gmenu->iflag_psfBox =    0;
    mbot->updatable->fline_menu->iflag_flineBox = 0;
    mbot->updatable->mesh_vws->iflag_meshBox =    0;
    mbot->id_current[0] = 1;
    
    activate_evolution_menu(single_kemoview, mbot->updatable->itemTEvo);

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
    kemo_sgl_gl = kemoview_allocate_gl_pointers();
	
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
	glfw_callbacks_init(single_kemoview, kemo_sgl_gl);
	
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
    kemoview_gl_init_lighting(kemo_sgl_gl);
	kemoview_init_phong_light_list(single_kemoview);
	
	iflag_gtk_focus = 1;
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

