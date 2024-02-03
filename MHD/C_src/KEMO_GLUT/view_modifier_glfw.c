
/* view_modifier_glfw.c */


#include "view_modifier_glfw.h"

/* initial settings */

GLFWwindow *glfw_window;
struct kemoviewer_type *    kemoview_GLFW;
struct kemoviewer_gl_type * kemoGL_GLFW;
int iflag_quickdraw = 0;

static int left_button_func =   ROTATE;
static int middle_button_func = PAN;
static int right_button_func =  ZOOM;
static int arrow_key_func =     PAN;

static int      moving_left, moving_middle, moving_right;
static double  begin_left[2], begin_middle[2], begin_right[2];

static double  begin[2];
static double  gTrackBallRotation[4];


void set_GLFW_viewtype_mode(int selected){
    if(selected == RESET) selected = VIEW_3D;

    if(selected == VIEW_3D
                || selected == VIEW_STEREO){
        left_button_func = ROTATE;
    }
    else if(selected == VIEW_MAP
                || selected == VIEW_XY
                || selected == VIEW_XZ
                || selected == VIEW_YZ) {
        left_button_func = PAN;
    };
    return;
}


void mouseButtonCB(GLFWwindow *window, int button, int action, int mods) {
	double xpos;
	double ypos;
	glfwGetCursorPos(window, &xpos, &ypos);
/*	printf("mouseButtonCB %d %d %d %lf, %lf\n", button, action, mods, xpos, ypos);*/

	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
		moving_left = 1;
		begin_left[0] = xpos;
		begin_left[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
		moving_left = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_PRESS) {
		moving_middle = 1;
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_RELEASE) {
		moving_middle = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_PRESS) {
		moving_right = 1;
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_RELEASE) {
		moving_right = 0;
	};
	
	if(action == GLFW_RELEASE){
		draw_full(kemoview_GLFW);
	};
	return;
};

void mousePosCB(GLFWwindow *window, double xpos, double ypos) {
	/*! This gets called when the mouse moves */
	double factor;
	int button_function = left_button_func;
	
	/*printf("mousePosCB %.1lf %.1lf\n", xpos, ypos); */
	if (moving_left == 0 && moving_middle == 0 && moving_right == 0) return;
	
	/*! Determine and apply the button function */
	
	if (moving_left == 1) {
        button_function = left_button_func;
        begin[0] = begin_left[0];
        begin[1] = begin_left[1];
	}
	else if (moving_middle == 1) {
        button_function = middle_button_func;
        begin[0] = begin_middle[0];
        begin[1] = begin_middle[1];
	}
	else if (moving_right == 1) {
        button_function = right_button_func;
        begin[0] = begin_right[0];
        begin[1] = begin_right[1];
	};
	
	if (button_function == ZOOM){
		factor = -0.5*(ypos-begin[1]);
		kemoview_zooming(factor, kemoview_GLFW);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, xpos, ypos, kemoview_GLFW);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, xpos, ypos, kemoview_GLFW);
	}
	else if (button_function == ROTATE) {
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]), kemoview_GLFW);
		kemoview_rollToTrackball( xpos, (-ypos), kemoview_GLFW);
		kemoview_drugging_addToRotationTrackball(kemoview_GLFW);
	}
	else if (button_function == SCALE){
		double current_scale = kemoview_get_view_parameter(kemoview_GLFW, ISET_SCALE, 0);
        
		if (ypos < begin[1]) {
			factor = ONE + TWO_MILI*(begin[1]-ypos);
		}
		else if (ypos > begin[1]) {
			factor = ONE/(ONE  + TWO_MILI*(ypos-begin[1]));
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemoview_GLFW);
	};
    /* ! update private variables and redisplay */
	
	if (moving_left) {
		begin_left[0] = xpos;
		begin_left[1] = ypos;
    }
	else if(moving_middle) {
		begin_middle[0] = xpos;
		begin_middle[1] = ypos;
	}
	else if(moving_right) {
		begin_right[0] = xpos;
		begin_right[1] = ypos;
	};
	return;
}

void set_GLFWindowSize(int width, int height,
                       struct kemoviewer_type *kemo_sgl){
	glfwSetWindowSize(glfw_window, width, height);
	kemoview_update_projection_by_viewer_size(width, height,
                                              width, height,
                                              kemo_sgl);
	glViewport(IZERO, IZERO, (GLint) width, (GLint) height);
};

void mouseScrollCB(GLFWwindow *window, double x, double y) {
/*	printf("mouseScrollCB %.1lf %.1lf\n", x, y);*/
    double newScale = x + y;
    kemoview_zooming(newScale, kemoview_GLFW);
}
void charFunCB(GLFWwindow* window, unsigned int charInfo) {
	printf("charFunCB %d\n", charInfo);
}


/*! This routine handles the arrow key operations */
static void keyFuncCB(GLFWwindow* window, int key, int scancode, int action, int mods){
    double x_dbl, y_dbl;
	double factor;
	
/*	printf("keyFuncCB %d %d %d %d\n", key, scancode, action, mods);	*/
	
	x_dbl = ZERO;
	y_dbl = ZERO;
	if (arrow_key_func == ZOOM){
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			factor = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			factor = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_zooming(factor, kemoview_GLFW);
	}
	
	else if (arrow_key_func == WALKTO){
		begin[0] = ZERO;
		begin[1] = ZERO;
		x_dbl = ZERO;
		y_dbl = ZERO;
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			y_dbl = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_mousedolly(begin, x_dbl, y_dbl, kemoview_GLFW);
	}
	
	else if (arrow_key_func == PAN){
		begin[0] = ZERO;
		begin[1] = ZERO;
		if (key == GLFW_KEY_LEFT && action == GLFW_PRESS){
			x_dbl = -ONE;
			y_dbl = ZERO;
		}
		else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS){
			x_dbl = ONE;
			y_dbl = ZERO;
		}
		else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = ZERO;
			y_dbl = ONE;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			x_dbl = ZERO;
			y_dbl = -ONE;
		};
		kemoview_mousepan(begin, x_dbl, y_dbl, kemoview_GLFW);
	}
	
	else if (arrow_key_func == ROTATE){
		if (key == GLFW_KEY_LEFT && action == GLFW_PRESS){
			x_dbl = begin[0] - TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS){
			x_dbl = begin[0] + TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] + TEN;
		}
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] - TEN;
		};
		kemoview_startTrackball( begin[0], (-begin[1]), kemoview_GLFW);
		kemoview_rollToTrackball( x_dbl, (-y_dbl), kemoview_GLFW);
		kemoview_drugging_addToRotationTrackball(kemoview_GLFW);
	}
	
	else if (arrow_key_func == SCALE){
		double current_scale = kemoview_get_view_parameter(kemoview_GLFW, ISET_SCALE, 0);
        
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS)
		factor = ONE/(ONE + TWO_CENT);
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			factor = ONE + TWO_CENT;
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_view_parameter(ISET_SCALE, 0, current_scale, kemoview_GLFW);
 	};
	
	glfwSwapBuffers(window);
	return;
}

GLFWwindow * open_kemoviwer_glfw_window(int npixel_x, int npixel_y){
	glfw_window = glfwCreateWindow(npixel_x, npixel_y, "CalypsoView", NULL, NULL);
	if (!glfw_window)
	{
		glfwTerminate();
		exit(1);
	}

	/* Make the window's context current */
	glfwSetWindowSize(glfw_window, npixel_x, npixel_y);
	glfwMakeContextCurrent(glfw_window);
	return glfw_window;
};


void glfw_callbacks_init(struct kemoviewer_type *kemo_sgl,
                         struct kemoviewer_gl_type *kemo_gl){
    kemoview_GLFW = kemo_sgl;
    kemoGL_GLFW = kemo_gl;
    
    
	/* set callback for mouse button */
	glfwSetMouseButtonCallback(glfw_window, mouseButtonCB);
	/* set callback for cursor position */
	glfwSetCursorPosCallback(glfw_window, mousePosCB);
	/* set callback for cursor position */
	glfwSetScrollCallback(glfw_window, mouseScrollCB);
	
	/* Set callback for keyboard input */
	glfwSetKeyCallback(glfw_window, keyFuncCB);
	glfwSetCharCallback(glfw_window, charFunCB);
	
	return;
}

void select_anaglyph(struct kemoviewer_type *kemo_sgl){
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        kemoview_modify_anaglyph(kemo_sgl, kemoGL_GLFW);
        glfwSwapBuffers(glfw_window);
    }else{
        kemoview_mono_viewmatrix(kemo_sgl);
        glDrawBuffer(GL_BACK);
        kemoview_modify_view(kemo_sgl, kemoGL_GLFW);
        glfwSwapBuffers(glfw_window);
    }
    return;
}

void draw_full(struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    select_anaglyph(kemo_sgl);
	return;
};

void draw_fast(struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW, kemo_sgl);
    select_anaglyph(kemo_sgl);
    return;
};
void draw_simple(struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view(kemo_sgl, kemoGL_GLFW);
    glfwSwapBuffers(glfw_window);
    return;
};

void draw_quilt(int istep_qult, struct kemoviewer_type *kemo_sgl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, QUILT_DRAW, kemo_sgl);
    kemoview_step_viewmatrix(istep_qult, kemo_sgl);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view(kemo_sgl, kemoGL_GLFW);
    glfwSwapBuffers(glfw_window);
    return;
};

static void write_rotate_quilt_views(struct kemoviewer_type *kemo_sgl,
                                     int iflag_img, struct kv_string *image_prefix,
                                     int i_axis, int inc_deg) {
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	
    int i, i_quilt, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;
	
	kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_sgl);
	glfwFocusWindow(glfw_window);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, int_degree, kemo_sgl);
		
		for(i_quilt=0;i_quilt<(nimg_column * nimg_raw);i_quilt++){
			draw_quilt(i_quilt, kemo_sgl);
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
			kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
		};
		kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
											 (nimg_column * npix_x),
											 (nimg_raw * npix_y), quilt_image);
	};
    free(quilt_image);
    free(image);
	return;
}

static void write_rotate_views(struct kemoviewer_type *kemo_sgl,
                               int iflag_img, struct kv_string *image_prefix,
                               int i_axis, int inc_deg) {
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	
    int i, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;
	
	kemoview_set_view_integer(ISET_ROTATE_AXIS, i_axis, kemo_sgl);
	glfwFocusWindow(glfw_window);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, int_degree, kemo_sgl);
        kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW, kemo_sgl);
        select_anaglyph(kemo_sgl);
		kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
		kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
											 npix_x, npix_y, image);
	};
    free(image);
	return;
}

static void write_evolution_quilt_views(struct kemoviewer_type *kemo_sgl,
                                        int iflag_img, struct kv_string *image_prefix,
                                        int ist_udt, int ied_udt, int inc_udt){
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
	int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
	unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int i, i_quilt;
	
	glfwFocusWindow(glfw_window);
	glfwSwapBuffers(glfw_window);
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_sgl);
			
			for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
				draw_quilt(i_quilt, kemo_sgl);
				kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
				kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
			};
			kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
												 (nimg_column * npix_x),
												 (nimg_raw * npix_y), quilt_image);
		}
	}
    free(quilt_image);
    free(image);
	return;
};

static void write_evolution_views(struct kemoviewer_type *kemo_sgl,
                                  int iflag_img, struct kv_string *image_prefix,
                                  int ist_udt, int ied_udt, int inc_udt){
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);
	int i;

	glfwFocusWindow(glfw_window);
	glfwSwapBuffers(glfw_window);
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_sgl);
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			draw_full(kemo_sgl);
			glfwSwapBuffers(glfw_window);
			
			kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
			kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
												 npix_x, npix_y, image);
		}
	}
    free(image);
	return;
};


void sel_write_rotate_views(struct kemoviewer_type *kemo_sgl,
                            int iflag_img, struct kv_string *image_prefix,
                            int i_axis, int inc_deg) {
	if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) != 0){
		write_rotate_quilt_views(kemo_sgl,iflag_img, image_prefix,
                                 i_axis, inc_deg);
	}else{
		write_rotate_views(kemo_sgl, iflag_img, image_prefix, i_axis, inc_deg);
	}
    draw_fast(kemo_sgl);
	return;
}

void sel_write_evolution_views(struct kemoviewer_type *kemo_sgl,
                               int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt){
	if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) != 0){
		write_evolution_quilt_views(kemo_sgl, iflag_img, image_prefix,
									ist_udt, ied_udt, inc_udt);
	}else{
		write_evolution_views(kemo_sgl, iflag_img, image_prefix,
							  ist_udt, ied_udt, inc_udt);
	};
    draw_full(kemo_sgl);
	return;
};


