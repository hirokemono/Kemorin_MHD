
/* view_modifier_glut.c */


#include "view_modifier_glut.h"

/* initial settings */

static int id_window;
static int id_menu;

static int left_button_func =   ROTATE;
static int middle_button_func = PAN;
static int right_button_func =  ZOOM;
static int arrow_key_func =     PAN;

static int      moving_left, moving_middle, moving_right;
static GLdouble  begin_left[2], begin_middle[2], begin_right[2];

static GLdouble  begin[2];
static GLdouble  gTrackBallRotation[4];

static void mouse(GLint button, GLint state, GLint x, GLint y){
	
	if(button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
		moving_left = 1;
		begin_left[0] = (GLdouble) x;
		begin_left[1] = (GLdouble) y;
	};
	if(button == GLUT_LEFT_BUTTON && state == GLUT_UP) {
		moving_left = 0;
	};
	
	if(button == GLUT_MIDDLE_BUTTON && state == GLUT_DOWN) {
		moving_middle = 1;
		begin_middle[0] = (GLdouble) x;
		begin_middle[1] = (GLdouble) y;
	};
	if(button == GLUT_MIDDLE_BUTTON && state == GLUT_UP) {
		moving_middle = 0;
	};
	
	if(button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
		moving_right = 1;
		begin_right[0] = (GLdouble) x;
		begin_right[1] = (GLdouble) y;
	};
	if(button == GLUT_RIGHT_BUTTON && state == GLUT_UP) {
		moving_right = 0;
	};
	return;
};

static void motion(GLint x, GLint y){
	GLdouble x_dbl, y_dbl;
	
	/*! This gets called when the mouse moves */
	
	GLdouble factor;
	int button_function;
	
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
		y_dbl = (GLdouble) y;
		factor = -0.5*(y_dbl-begin[1]);
		kemoview_zooming((GLdouble) factor, kemo_sgl);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, (GLdouble) x, (GLdouble) y, kemo_sgl);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, (GLdouble) x, (GLdouble) y, kemo_sgl);
	}
	else if (button_function == ROTATE) {
		x_dbl = (GLdouble) x;
		y_dbl = (GLdouble) y;
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]), kemo_sgl);
		kemoview_rollToTrackball( x_dbl, (-y_dbl), kemo_sgl);
		kemoview_drugging_addToRotationTrackball(kemo_sgl);
	}
	else if (button_function == SCALE){
		double current_scale;
        current_scale = kemoview_get_scale_factor();
        
		y_dbl = (GLdouble) y;
		if (y < begin[1]) {
			factor = ONE + TWO_MILI*(begin[1]-y_dbl);
		}
		else if (y > begin[1]) {
			factor = ONE/(ONE  + TWO_MILI*(y_dbl-begin[1]));
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_scale_factor(current_scale);
	};
    /* ! update private variables and redisplay */
	
	if (moving_left) {
		begin_left[0] = (GLdouble) x;
		begin_left[1] = (GLdouble) y;
    }
	else if(moving_middle) {
		begin_middle[0] = (GLdouble) x;
		begin_middle[1] = (GLdouble) y;
	}
	else if(moving_right) {
		begin_right[0] = (GLdouble) x;
		begin_right[1] = (GLdouble) y;
	};
	
	if (moving_left == 1 || moving_middle == 1 || moving_right == 1) {
		glutPostRedisplay();
	};
	
	
	return;
}

static void arrows_c(int key, int x, int y){
	GLdouble x_dbl, y_dbl;
	
	/*! This routine handles the arrow key operations */
	
	GLdouble factor;
	
	if (arrow_key_func == ZOOM){
		if (key == GLUT_KEY_DOWN){
			factor = ONE;
		}
		else if (key == GLUT_KEY_UP){
			factor = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_zooming((GLdouble) factor, kemo_sgl);
	}
	
	else if (arrow_key_func == WALKTO){
		begin[0] = ZERO;
		begin[1] = ZERO;
		if (key == GLUT_KEY_DOWN){
			x_dbl = ONE;
		}
		else if (key == GLUT_KEY_UP){
			y_dbl = -ONE;
		}
		else {
			factor = ZERO;
		};
		kemoview_mousedolly(begin, x_dbl, y_dbl, kemo_sgl);
	}
	
	else if (arrow_key_func == PAN){
		begin[0] = ZERO;
		begin[1] = ZERO;
		if (key == GLUT_KEY_LEFT){
			x_dbl = -ONE;
			y_dbl = ZERO;
		}
		else if (key == GLUT_KEY_RIGHT){
			x_dbl = ONE;
			y_dbl = ZERO;
		}
		else if (key == GLUT_KEY_DOWN){
			x_dbl = ZERO;
			y_dbl = ONE;
		}
		else if (key == GLUT_KEY_UP){
			x_dbl = ZERO;
			y_dbl = -ONE;
		};
		kemoview_mousepan(begin, x_dbl, y_dbl, kemo_sgl);
	}
	
	else if (arrow_key_func == ROTATE){
		if (key == GLUT_KEY_LEFT){
			x_dbl = begin[0] - TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLUT_KEY_RIGHT){
			x_dbl = begin[0] + TEN;
			y_dbl = begin[1] + ZERO;
		}
		else if (key == GLUT_KEY_DOWN){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] + TEN;
		}
		else if (key == GLUT_KEY_UP){
			x_dbl = begin[0] + ZERO;
			y_dbl = begin[1] - TEN;
		};
		kemoview_startTrackball( begin[0], (-begin[1]), kemo_sgl);
		kemoview_rollToTrackball( x_dbl, (-y_dbl), kemo_sgl);
		kemoview_drugging_addToRotationTrackball(kemo_sgl);
	}
	
	else if (arrow_key_func == SCALE){
		double current_scale;
        current_scale = kemoview_get_scale_factor();
        
		if (key == GLUT_KEY_DOWN)
		factor = ONE/(ONE + TWO_CENT);
		else if (key == GLUT_KEY_UP){
			factor = ONE + TWO_CENT;
		}
		else {
			factor = ONE;
		};
		current_scale = current_scale * factor;
		kemoview_set_scale_factor(current_scale);
 	};
	
	glutPostRedisplay();
	return;
}

static void menu_handler(GLint value){
	glutPostRedisplay();
	return;
};

void set_left_button(GLint value){
	left_button_func = value;
	return;
}

static void set_middle_button(GLint value){
	middle_button_func = value;
	return;
}

static void set_right_button(GLint value){
	right_button_func = value;
	return;
}

static void set_arrow_keys(GLint value){
	arrow_key_func = value;
	return;
}


void set_viewtype_mode_glut(int selected){
	
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


static void glut_motion_setting_menu_list(){
	
	glutAddMenuEntry("rotate" , ROTATE);
	glutAddMenuEntry("zoom",    ZOOM);
	glutAddMenuEntry("pan",     PAN);
	glutAddMenuEntry("scale",   SCALE);
	return;
}

void view_modifier_init(){
	
	glutMouseFunc(mouse);
	glutMotionFunc(motion);
	glutSpecialFunc(arrows_c);
	return;
}



int  menu_init(){
	int menuid;
	
	GLint button_left, button_middle, arrow_keys;
	
	button_left = glutCreateMenu(set_left_button);
	glut_motion_setting_menu_list();
	
	button_middle = glutCreateMenu(set_middle_button);
	glut_motion_setting_menu_list();
	
	arrow_keys = glutCreateMenu(set_arrow_keys);
	glut_motion_setting_menu_list();
	
	menuid = glutCreateMenu(menu_handler);
	/*
     glutAddSubMenu("LEFT mouse button",button_left);
     glutAddSubMenu("middle mouse button",button_middle);
     glutAddSubMenu("arrow keys",arrow_keys);
     */
	
	return menuid;
}

void display_menu(){
	kemoview_draw_glut_menubottun();
	glutSwapBuffers();
	
	return;
};

void display(){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    glDrawBuffer(GL_BACK);
    kemoview_modify_view();
	glutSwapBuffers();
	
	return;
};

void modifywindow(int width, int height){
    kemoview_update_projection_by_viewer_size(width, height,
                                              width, height, kemo_sgl);
	return;
}

int create_kemoview_window(){
	id_window = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(id_window);
	
	/*! Set the display callback  */
	glutDisplayFunc(display);
	glutReshapeFunc(modifywindow);
	return id_window;
};

int create_kemoview_menu(){
	id_menu = glutCreateSubWindow(id_window,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	glutDisplayFunc(display_menu);
	return id_menu;
};


static int id_window;

void set_main_window_id_glut(int winid){
	id_window = winid;
	return;
}

void draw_mesh_keep_menu(){
	glutSetWindow(id_window);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_mono_viewmatrix(kemo_sgl);
	glutPostRedisplay();
	return;
};

void write_rotate_views_glut(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg) {
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);

    int i, int_degree, ied_deg;
    if(inc_deg <= 0) inc_deg = 1;
    ied_deg = 360/inc_deg;

    glutSetWindow(id_window);
	
	kemoview_set_animation_rot_axis(i_axis);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*inc_deg;
		
		kemoview_set_animation_rot_angle(int_degree);
        kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW, kemo_sgl);
        glDrawBuffer(GL_BACK);
        kemoview_modify_view();
		glutSwapBuffers();
		
        kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
        kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                             npix_x, npix_y, image);
	};
    free(image);

    draw_mesh_keep_menu();
	return;
}

void write_evolution_views_glut(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt){
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);

    int i;

	glutSwapBuffers();
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			kemoview_viewer_evolution(i, kemo_sgl);
			
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			draw_mesh_keep_menu();
			glutSwapBuffers();
            
            kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            kemoview_write_window_to_file_w_step(iflag_img, i, image_prefix,
                                                 npix_x, npix_y, image);
		}
	}
    free(image);
	return;
};
