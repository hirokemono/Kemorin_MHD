
/* kemoview_glut_routines.h*/

#include "kemoview_glut_routines.h"

static int id_window;

void set_main_window_id_glut(int winid){
	id_window = winid;
	return;
}

void draw_mesh_keep_menu(){
	glutSetWindow(id_window);
	draw_kemoviewer_c();
	update_kemoviewer_distance();
	modify_view_kemoview();
	glutPostRedisplay();
	return;
};

void write_rotate_views_glut(int iflag_img, char *image_head, int i_axis) {
	int i, int_degree;
	int int_deg = 2;
	int ied_deg = 360/int_deg;
	
	glutSetWindow(id_window);
	
	set_kemoview_animation_rot_axis(i_axis);
	for (i = 0; i< ied_deg; i++) {
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
		int_degree =  i*int_deg;
		
		set_kemoview_animation_rot_angle(int_degree);
		draw_kemoviewer_c();
		rotate_kemoview();
		glutSwapBuffers();
		
        kemoview_write_window_to_file_w_step(iflag_img, i, image_head);
	};
	draw_mesh_keep_menu();
	return;
}

void write_evolution_views_glut(int iflag_img, char *image_head, 
								int ist_udt, int ied_udt, int inc_udt){
	int i;

	glutSwapBuffers();
	for (i=ist_udt; i<(ied_udt+1); i++) {
		if( ((i-ist_udt)%inc_udt) == 0) {
			
			evolution_viewer(i);
			
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
			draw_mesh_keep_menu();
			glutSwapBuffers();
            
            kemoview_write_window_to_file_w_step(iflag_img, i, image_head);
		}
	}
	return;
};

/* ---------  Action for selected menu -----------   */ 

void set_viewtype_mode_glut(int selected, char *viewtype_title){
	
	if(selected == RESET) selected = VIEW_3D;

	if(selected == VIEW_3D){
		sprintf(viewtype_title,"%s", "3D-View");
		set_left_button(ROTATE);
	}
	else if(selected == VIEW_STEREO){
		sprintf(viewtype_title,"%s", "Stereo-View");
		set_left_button(ROTATE);
	}
	else if(selected == VIEW_MAP) {
		sprintf(viewtype_title,"%s", "Map-projection");
		set_left_button(PAN);
	}
	else if(selected == VIEW_XY) {
		sprintf(viewtype_title,"%s",  "XY-View");
		set_left_button(PAN);
	}
	else if(selected == VIEW_XZ) {
		sprintf(viewtype_title,"%s",  "XZ-View");
		set_left_button(PAN);
	}
	else if(selected == VIEW_YZ) {
		sprintf(viewtype_title,"%s",  "YZ-View");
		set_left_button(PAN);
	};
	
	set_viewtype_glut(selected);
	return;
}
