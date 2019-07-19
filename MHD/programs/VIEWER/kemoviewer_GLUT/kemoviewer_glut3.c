
/* kemoviewer_glut.c */

#include "kemo_mesh_viewer_glut.h"

#define NPIX_X  800
#define NPIX_Y  640

static int winid, menu_win;
struct kemoviewer_type *single_kemoview;

static void display_menu3(){
	kemoview_draw_glut_menubottun3();
	glutSwapBuffers();
	
	return;
};

void draw_mesh_kemo3(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
	
	/* Initialize arrays for viewer */
	
	kemoview_allocate_single_viwewer_struct(single_kemoview);
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	link_glut_menu_address();
	
	/*! Initializations with GLUT*/
	glutInit(&narg_glut, arg_glut);
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
	winid = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(winid);
	
	/*glutEntryFunc(enter_leave);*/
	  fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	  );
	
	/*! Set the display callback  */
	
	glutDisplayFunc(display);
	glutReshapeFunc(modifywindow);
	
	/*
	if (!glutExtensionSupported("GL_ARB_texture_non_power_of_two")) 
		{printf("GL_ARB_texture_non_power_of_two is not Supported\n");};
	
	/*  initialize view_modifier, receiving the id for it's submenu */
	kemoviewer_reset_to_init_angle();
	view_modifier_init();
	
	/* ! set the perspective and lighting */
	kemoview_init_background_color();
	kemoview_init_lighting(iflag_core_profile);
	
	
	/*! Create menu window */
	menu_win = glutCreateSubWindow(winid,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	/*glutEntryFunc(enter_leave);*/
	
	glutDisplayFunc(display_menu3);
	
	draw_mesh_w_menu();
	
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};


int main(int argc, char *argv[]){
	int iflag_streo_shutter = SHUTTER_OFF;
	int i;
	
	/*	printf("Number of arguments %d\n", argc);*/
	for (i = 0; i < argc; i++) {
/*		printf("%dth arguments: %s\n", i, argv[i]);*/
		if(strcmp(argv[i],"-help") == 0){
			printf("-stereo_shutter: Use streo monitor with shutter\n");
			return 0;
		}
	}
		
	for (i = 0; i < argc; i++) {
		if(strcmp(argv[i],"-stereo_shutter") == 0){
			printf("shutter ON\n");
			iflag_streo_shutter = SHUTTER_ON;
		}
	}
	
	draw_mesh_kemo3(iflag_streo_shutter, IZERO);
	return 0;
};


