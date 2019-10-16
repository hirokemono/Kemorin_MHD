
/* viewer_GLFW_test.c */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <gtk/gtk.h>


#include "kemoviewer.h"
#include "view_modifier_glfw.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;

struct main_buttons *mbot;

static void mainloop_4_glfw(){
	int iflag;
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win)){
		iflag = glfwWindowShouldClose(glfw_win);
		
		draw_full();
		glfwPollEvents();
	};
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
	glfwSetWindowShouldClose(window, GLFW_TRUE);
	iflag_glfw_focus = 0;
	return;
}

void dropFileToGlfw_CB(GLFWwindow *window, int num, const char **paths) {
	struct kv_string *filename;
	printf("dropFileToGlfw_CB %d\n", num);
	for (int i = 0; i < num; i++) {
		printf("%s\n", paths[i]);
		filename = kemoview_init_kvstring_by_string(paths[i]);
	}
}
void windowSizeCB(GLFWwindow *window, int width, int height) {
/*	printf("windowSizeCB %d %d\n", width, height); */
	kemoview_update_projection_by_viewer_size(width, height);
	glViewport(IZERO, IZERO, (GLint) width, (GLint) height);
}
void frameBufferSizeCB(GLFWwindow *window, int nx_buf, int ny_buf){
	printf("frameBufferSizeCB %d %d\n", nx_buf, ny_buf);
}

/* Main GTK window */

/* Main routine for C */

int draw_glfw_test(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
	int iflag_retinamode = 0;
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
	/*
	if(iflag_retinamode == 1){
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_TRUE);
	} else{
		glfwWindowHint(GLFW_COCOA_RETINA_FRAMEBUFFER, GLFW_FALSE);
	};
	*/
	
	/* Create a windowed mode window and its OpenGL context */
	glfw_win = open_kemoviwer_glfw_window(NPIX_X, NPIX_Y);
	int nx_buf, ny_buf;
	glfwGetFramebufferSize(glfw_win, &nx_buf, &ny_buf);
	
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
	kemoview_init_lighting(iflag_core_profile);
	kemoview_init_phong_light_list();
	
	glClear(GL_COLOR_BUFFER_BIT);
	draw_full();
	glfwPollEvents();
	glfwPostEmptyEvent();
	
	mainloop_4_glfw();
	glfwTerminate();
	return 0;
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
	
	draw_glfw_test(iflag_streo_shutter, IZERO);
	return 0;
};


