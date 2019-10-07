
/* kemo_mesh_viewer_gtk.c*/

#include "kemo_mesh_viewer_gtk.h"

#define NPIX_X  800
#define NPIX_Y  640

static int winid, menu_win;
int submenu_id;

struct kemoviewer_type *single_kemoview;

/* Main routine for C */

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	glutInit(&narg_glut, arg_glut);
	
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init (&narg_glut, &arg_glut);
    
	/*! Initializations with GLUT*/
	if(iflag_streo_shutter == SHUTTER_ON){
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_STEREO|GLUT_3_2_CORE_PROFILE);
		} else {
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_3_2_CORE_PROFILE);
	};
	/*! Create viewer window*/
    kemoview_set_retinamode(IZERO);
	kemoview_set_windowsize(NPIX_X, NPIX_Y);
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
	kemoview_init_lighting(iflag_core_profile);
	kemoview_init_phong_light_list();
	
	menu_win = create_kemoview_menu();
	
	glutSetWindow(menu_win);
	kemoview_draw_menu_setup();
	
	glutSetWindow(winid);
	draw_mesh_keep_menu();
	
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};

