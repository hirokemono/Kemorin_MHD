
/* kemo_mesh_viewer_gtk.c*/

#include "kemo_mesh_viewer_gtk.h"

#define NPIX_X  800
#define NPIX_Y  640

struct glut_menu_address  glut_menu_id_struct;
struct glut_menu_address *glut_menu_id;

static int winid, menu_win;

static void make_1st_level_menu();

struct kemoviewer_type *single_kemoview;

GtkWidget *window_main;


/* Main GTK window */

void kemoview_main_window(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *vbox;
	
	GtkWidget *quitButton;
	
	quitButton = gtk_button_new_with_label("Quit");
	g_signal_connect(G_OBJECT(quitButton), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	window_main = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_main), "Mesh viewer");
	gtk_widget_set_size_request(window_main, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_main), 5);
	g_signal_connect(G_OBJECT(window_main), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), quitButton, FALSE, FALSE, 0);
	add_gtk_main_menu(kemoviewer_data, window_main, vbox);
	
	gtk_widget_show_all(window_main);
	gtk_main();
	return;
}

/* subroutine for reading mesh */


static void link_glut_menu_address(){
	glut_menu_id = &glut_menu_id_struct;
	return;
}


/* draw object using GLUT */

static void draw_mesh_w_menu(){
	make_1st_level_menu();
	draw_mesh_keep_menu();
	return;
};

/* ---------  Action for selected menu -----------   */ 

static void main_menu_handler(int sel){
	if (sel == QUIT_SELECTED)   { 
		exit(EXIT_SUCCESS);
	}else if(sel == SET_COAST_RADIUS){
		kemoview_main_window(single_kemoview);
	};
    return;
};

static void dummy_handler(int sel){
	int itmp;
	itmp = sel;
	return;
}

/* Create 1st level menu() */

static void make_1st_level_menu(){
	GLint menu_id;
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	glutSetWindow(menu_win);
	
	glut_menu_id->submenu_id = menu_init();
	
	menu_id = glutCreateMenu(main_menu_handler);
	
	glutAddMenuEntry("Main menu",SET_COAST_RADIUS);
	glutAddMenuEntry("Quit",QUIT_SELECTED);
	glutAttachMenu(GLUT_LEFT_BUTTON);
	return;
};

/* Main routine for C */

void draw_mesh_kemo(void) {
	int narg_glut = 0;
	char **arg_glut;
    GLboolean bStereo;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
    kemo_gl = kemoview_allocate_gl_pointers();

	link_glut_menu_address();
	glutInit(&narg_glut, arg_glut);
	
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init (&narg_glut, &arg_glut);
    
	/*! Initializations with GLUT*/
    glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
                        |GLUT_MULTISAMPLE|GLUT_3_2_CORE_PROFILE);
	/*! Create viewer window*/
    kemoview_set_retinamode(IZERO);
	kemoview_set_windowsize(NPIX_X, NPIX_Y, NPIX_X, NPIX_Y);
	glutInitWindowSize(NPIX_X, NPIX_Y);
	winid = create_kemoview_window();
	
	  fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	  );
	
	
	/*  initialize view_modifier, receiving the id for it's submenu  */
	kemoviewer_reset_to_init_angle();
	view_modifier_init();
	
	/* ! set the perspective and lighting */
    kemoview_init_background_color();
	kemoview_init_lighting();
    kemoview_gl_background_color();
    kemoview_gl_init_lighting(kemo_gl);
	kemoview_init_phong_light_list();
	
	menu_win = create_kemoview_menu();
	
	glutSetWindow(menu_win);
	kemoview_draw_menu_setup(kemo_gl);
	
	glutSetWindow(winid);
	draw_mesh_w_menu();
	
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};

