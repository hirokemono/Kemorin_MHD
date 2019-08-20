
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
		kemoview_zooming((GLdouble) factor);
	}
	
	if (button_function == WALKTO){
		kemoview_mousedolly(begin, (GLdouble) x, (GLdouble) y);
	}
	else if(button_function == PAN){
		kemoview_mousepan(begin, (GLdouble) x, (GLdouble) y);
	}
	else if (button_function == ROTATE) {
		x_dbl = (GLdouble) x;
		y_dbl = (GLdouble) y;
		gTrackBallRotation[0] = ZERO;
		gTrackBallRotation[1] = ZERO;
		gTrackBallRotation[2] = ZERO;
		gTrackBallRotation[3] = ZERO;
		
		kemoview_startTrackball( begin[0], (-begin[1]));
		kemoview_rollToTrackball( x_dbl, (-y_dbl));
		kemoview_drugging_addToRotationTrackball();
	}
	else if (button_function == SCALE){
		GLdouble current_scale;
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
		kemoview_zooming((GLdouble) factor);
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
		kemoview_mousedolly(begin, x_dbl, y_dbl);
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
		kemoview_mousepan(begin, x_dbl, y_dbl);
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
		kemoview_startTrackball( begin[0], (-begin[1]));
		kemoview_rollToTrackball( x_dbl, (-y_dbl));
		kemoview_drugging_addToRotationTrackball();
	}
	
	else if (arrow_key_func == SCALE){
		GLdouble current_scale;
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
	kemoview_modify_view();
	glutSwapBuffers();
	
	return;
};

void modifywindow(int width, int height){
    kemoview_update_projection_by_viewer_size(width, height);
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


