
/* view_modifier_glfw.c */


#include "view_modifier_glfw.h"

/* initial settings */

GLFWwindow *glfw_window;
int iflag_quickdraw = 0;

static int left_button_func =   ROTATE;
static int middle_button_func = PAN;
static int right_button_func =  ZOOM;
static int arrow_key_func =     PAN;

static int      moving_left, moving_middle, moving_right;
static GLdouble  begin_left[2], begin_middle[2], begin_right[2];

static GLdouble  begin[2];
static GLdouble  gTrackBallRotation[4];

void mouseButtonCB(GLFWwindow *window, int button, int action, int mods) {
	double xpos;
	double ypos;
	glfwGetCursorPos(window, &xpos, &ypos);
	printf("mouseButtonCB %d %d %d %lf, %lf\n", button, action, mods, xpos, ypos);

	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
		moving_left = 1;
		begin_left[0] = (GLdouble) xpos;
		begin_left[1] = (GLdouble) ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
		moving_left = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_PRESS) {
		moving_middle = 1;
		begin_middle[0] = (GLdouble) xpos;
		begin_middle[1] = (GLdouble) ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_MIDDLE && action == GLFW_RELEASE) {
		moving_middle = 0;
	};
	
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_PRESS) {
		moving_right = 1;
		begin_right[0] = (GLdouble) xpos;
		begin_right[1] = (GLdouble) ypos;
	};
	if(button == GLFW_MOUSE_BUTTON_RIGHT && action == GLFW_RELEASE) {
		moving_right = 0;
	};
	return;
};

void mousePosCB(GLFWwindow *window, double xpos, double ypos) {
	/*! This gets called when the mouse moves */
	
	GLdouble factor;
	int button_function;
	
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
		kemoview_zooming((GLdouble) factor);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, (GLdouble) xpos, (GLdouble) ypos);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, (GLdouble) xpos, (GLdouble) ypos);
	}
	else if (button_function == ROTATE) {
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]));
		kemoview_rollToTrackball( xpos, (-ypos));
		kemoview_drugging_addToRotationTrackball();
	}
	else if (button_function == SCALE){
		GLdouble current_scale;
        current_scale = kemoview_get_scale_factor();
        
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
		kemoview_set_scale_factor(current_scale);
	};
    /* ! update private variables and redisplay */
	
	if (moving_left) {
		begin_left[0] = (GLdouble) xpos;
		begin_left[1] = (GLdouble) ypos;
    }
	else if(moving_middle) {
		begin_middle[0] = (GLdouble) xpos;
		begin_middle[1] = (GLdouble) ypos;
	}
	else if(moving_right) {
		begin_right[0] = (GLdouble) xpos;
		begin_right[1] = (GLdouble) ypos;
	};
	
	if (moving_left == 1 || moving_middle == 1 || moving_right == 1) {
//		glfwSwapBuffers(window);
	};
	return;
}


void windowSizeCB(GLFWwindow *window, int width, int height) {
	printf("windowSizeCB %d %d\n", width, height);
    kemoview_update_projection_by_viewer_size(width, height);
}
void mouseScrollCB(GLFWwindow *window, double x, double y) {
	printf("mouseScrollCB %.1lf %.1lf\n", x, y);
}
void charFunCB(GLFWwindow* window, unsigned int charInfo) {
	printf("charFunCB %d\n", charInfo);
}
void dropCB(GLFWwindow *window, int num, const char **paths) {
	printf("dropCB %d\n", num);
	for (int i = 0; i < num; i++) {
		printf("%s\n", paths[i]);
	}
}


/*! This routine handles the arrow key operations */
void keyFunCB(GLFWwindow* window, int key, int scancode, int action, int mods) {
	GLdouble x_dbl, y_dbl;
	GLdouble factor;
	
	printf("keyFunCB %d %d %d %d\n", key, scancode, action, mods);	
	
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
		kemoview_zooming((GLdouble) factor);
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
		kemoview_mousedolly(begin, x_dbl, y_dbl);
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
		kemoview_mousepan(begin, x_dbl, y_dbl);
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
		kemoview_startTrackball( begin[0], (-begin[1]));
		kemoview_rollToTrackball( x_dbl, (-y_dbl));
		kemoview_drugging_addToRotationTrackball();
	}
	
	else if (arrow_key_func == SCALE){
		GLdouble current_scale;
        current_scale = kemoview_get_scale_factor();
        
		if (key == GLFW_KEY_DOWN && action == GLFW_PRESS)
		factor = ONE/(ONE + TWO_CENT);
		else if (key == GLFW_KEY_UP && action == GLFW_PRESS){
			factor = ONE + TWO_CENT;
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_scale_factor(current_scale);
 	};
	
	glfwSwapBuffers(window);
	return;
}

GLFWwindow * open_kemoviwer_window(int npixel_x, int npixel_y){
	glfw_window = glfwCreateWindow(npixel_x, npixel_y, "Kemoviewer", NULL, NULL);
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


void glfw_view_modifier_init(GLFWwindow* window){
	/* Set callback for window size changing */
	glfwSetWindowSizeCallback(window, windowSizeCB);
	
	/* set callback for mouse button */
	glfwSetMouseButtonCallback(window, mouseButtonCB);
	/* set callback for cursor position */
	glfwSetCursorPosCallback(window, mousePosCB);
	/* set callback for cursor position */
	glfwSetScrollCallback(window, mouseScrollCB);
	
	/* Set callback for keyboard input */
	glfwSetKeyCallback(window, keyFunCB);
	glfwSetCharCallback(window, charFunCB);
	
	/* ファイルをドラッグ&ドロップした時に呼び出す関数を設定 */
	glfwSetDropCallback(window, dropCB);
	return;
}


void draw_mesh_for_glfw(GLFWwindow* window){
	kemoview_draw_objects_gl3();
	kemoview_update_distance();
	kemoview_modify_view();
	glfwSwapBuffers(window);
	return;
};

void display(GLFWwindow* window){
	kemoview_draw_objects_gl3();
	kemoview_modify_view();
	glfwSwapBuffers(window);
	return;
};


void draw_mesh_keep_menu(){
	kemoview_draw_objects_gl3();
	kemoview_update_distance();
	kemoview_modify_view();
	glfwSwapBuffers(glfw_window);
	return;
};

void write_rotate_views_glut(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg) {
    int i, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;
	
	kemoview_set_animation_rot_axis(i_axis);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_animation_rot_angle(int_degree);
		kemoview_draw_objects_gl3();
		kemoview_rotate();
		glfwSwapBuffers(glfw_window);
		
        kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix);
	};
	draw_mesh_keep_menu();
	return;
}

void write_evolution_views_glut(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt){
	int i;

	glfwSwapBuffers(glfw_window);
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i);
			
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			draw_mesh_keep_menu();
			glfwSwapBuffers(glfw_window);
            
            kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix);
		}
	}
	return;
};


void set_viewtype_mode_glfw(int selected){
	
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
	
	kemoview_set_viewtype(selected);
	return;
}

