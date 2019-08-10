
/* kemoviewer_glut.c */

#include "kemoviewer.h"

#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_console_input.h"
#include "kemoview_glut_routines.h"


#define WINDOW_TITLE_PREFIX "Kemoviewer_GL3"
#define NPIX_X  800
#define NPIX_Y  640

static int winid, menu_win;
struct kemoviewer_type *single_kemoview;

struct glut_menu_address  glut_menu_id_struct;
struct glut_menu_address *glut_menu_id;

unsigned FrameCount = 0;

int CurrentWidth = NPIX_X;
int CurrentHeight = NPIX_Y;
int WindowHandle = 0;

void link_glut_menu_address(){
	glut_menu_id = &glut_menu_id_struct;
	return;
}


void display_menu3(){
	glutSetWindow(menu_win);
	kemoview_draw_menu_gl3();
	glutSwapBuffers();
	glutSetWindow(winid);
};

void RenderFunction(void)
{
	GLfloat model[16], proj[16];
	int i;
	
	glutSetWindow(winid);
	++FrameCount;
	
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	kemoview_draw_quad_gl3();
	
	glutSwapBuffers();
}

void IdleFunction(void)
{
  glutPostRedisplay();
}

void TimerFunction(int Value)
{
  if (0 != Value) {
    char* TempString = (char*)
      malloc(512 + strlen(WINDOW_TITLE_PREFIX));

    sprintf(
	    TempString,
	    "%s: %d Frames Per Second @ %d x %d",
	    WINDOW_TITLE_PREFIX,
	    FrameCount * 4,
	    CurrentWidth,
      CurrentHeight
	    );

    glutSetWindowTitle(TempString);
    free(TempString);
  }
  
  FrameCount = 0;
  glutTimerFunc(250, TimerFunction, 1);
}

void init_kemoview_GLUT3(int iflag_streo_shutter) {
	
}

void draw_mesh_kemo3(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	
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
	
	/*! Create viewer window */
	kemoview_set_retinamode(IZERO);
	kemoview_set_windowsize(NPIX_X, NPIX_Y);
	glutInitWindowSize(NPIX_X, NPIX_Y);
	
	winid = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(winid);
	
	/*! Set the display callback  */
	glutDisplayFunc(RenderFunction);
	glutReshapeFunc(modifywindow);
	glutIdleFunc(IdleFunction);
	glutTimerFunc(0, TimerFunction, 0);
	
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
	kemoview_init_lighting(1);
	
	/*! Create menu window*/
	menu_win = glutCreateSubWindow(winid,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	glutDisplayFunc(display_menu3);
	glutSetWindow(winid);
	
	
	
	
	glutSetWindow(winid);
	kemoview_draw_quad_setup();
	
	glutSetWindow(menu_win);
	kemoview_draw_menu_setup();
	
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glutMainLoop();
	
	kemo_Cleanup();
}

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
