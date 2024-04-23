
/* kemo_mesh_viewer_glfw.c*/

#include <stdio.h>
#include <math.h>
#include "kemo_mesh_viewer_glfw.h"
#include "move_draw_objects_gl.h"

#define NPIX_X  960
#define NPIX_Y  800

struct kemoviewer_type *single_kemoview;
struct kemoviewer_gl_type *kemo_sgl_gl;

GLFWwindow *glfw_win;
int iflag_glfw_focus = 0;
int iflag_glfw_end = 0;

int iflag_msg_fade;
float message_opacity;
double msg_timer_start;
double delta_t;

int iflag_fast_prev;
int iflag_fast_draw;
double fast_draw_start;

static void mainloop_4_glfw(){
    int icou = 0;
	int jcou = 0;
	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(glfw_win)){
		glfwWindowShouldClose(glfw_win);
        glfwPollEvents();
        icou++;
		
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
		if(iflag_glfw_end == 1) return;
		draw_fast(single_kemoview);
	};
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
	glfwSetWindowShouldClose(window, GLFW_TRUE);
	iflag_glfw_focus = 0;
	iflag_glfw_end = 1;
	return;
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
}


int draw_mesh_kemo(void) {
	int iflag_retinamode = 1;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
    kemo_sgl_gl = kemoview_allocate_gl_pointers();
	
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
	
	/* Set Cllback for drug and Drop into window
	glfwSetDropCallback(glfw_win, dropFileToGlfw_CB);
	*/
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
	
	glClear(GL_COLOR_BUFFER_BIT);
	draw_full(single_kemoview);
	glfwPollEvents();
	glfwPostEmptyEvent();
	
	mainloop_4_glfw();
	glfwTerminate();
	return 0;
};

