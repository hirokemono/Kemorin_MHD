
/* draw_axis_c.c */

#include "draw_axis_c.h"

static const GLfloat red[4] =   {RED_R, RED_G, RED_B, RED_A};
static const GLfloat blue[4] =  {BLUE_R, BLUE_G, BLUE_B, BLUE_A};
static const GLfloat green[4] = {L_GREEN_R, L_GREEN_G, L_GREEN_B, L_GREEN_A};

static struct view_element arrow_view;

static float set_ratio_4_axislabel(struct view_element *view_s, 
                                   int end_screen[2], int zero_screen[2], GLfloat l_axis){
	GLfloat ratio;
	GLfloat ratio_tmp[2];
	
	if ( end_screen[0] < view_s->nx_window/10 ) { 
		ratio_tmp[0] = (GLfloat) (view_s->nx_window/10 - zero_screen[0])
        / (GLfloat) (end_screen[0] - zero_screen[0]);
	} else if  ( end_screen[0] > view_s->nx_window*9/10 ) {
		ratio_tmp[0] = (GLfloat) (zero_screen[0]-view_s->nx_window*9/10)
        / (GLfloat) (zero_screen[0] - end_screen[0]);
	} else {
		ratio_tmp[0] = ONE;
	};
	
	if ( end_screen[1] < view_s->ny_window/10 ) { 
		ratio_tmp[1] = (GLfloat) (view_s->ny_window/10 - zero_screen[1])
        / (GLfloat) (end_screen[1] - zero_screen[1]);
	} else if  ( end_screen[1] > view_s->ny_window*9/10 ) {
		ratio_tmp[1] = (GLfloat) (zero_screen[1]-view_s->ny_window*9/10)
        / (GLfloat) (zero_screen[1] - end_screen[1]);
	} else {
		ratio_tmp[1] = ONE;
	};
	
	if (ratio_tmp[0] < ratio_tmp[1]) { ratio = ratio_tmp[0]; }
	else { ratio = ratio_tmp[1]; };
	
	ratio = ratio * l_axis;
	return ratio;
}

static void set_vertexs_for_axis(struct view_element *view_s, GLfloat dist,
								 GLfloat x_arrowx[6], GLfloat x_arrowy[6], GLfloat x_arrowz[6], GLfloat w_ratio[3],
								 GLfloat x_charax[12], GLfloat x_charay[18], GLfloat x_charaz[18]){
	GLfloat l_axis[3], axis_org[3];
	GLfloat label_ratio[3], min_l_ratio;
	
	GLfloat x_text_x1, x_text_x2, x_text_z1, x_text_z2;
	GLfloat y_text_y1, y_text_y2, y_text_z1, y_text_z2;
	GLfloat y_text_y3, y_text_z3;
	GLfloat z_text_z1, z_text_z2, z_text_x1, z_text_x2;
	
	GLfloat xx_axis[3];
	GLfloat x_label[2], y_label[2], z_label[2];
	
	int zero_screen[2], end_screen[2];
	
	glGetDoublev(GL_MODELVIEW_MATRIX, arrow_view.mat_object_2_eye);
	glGetDoublev(GL_PROJECTION_MATRIX, arrow_view.mat_eye_2_clip);
	
	set_axis_positions(view_s, (GLfloat) dist, l_axis, axis_org);
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(zero_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, &arrow_view);
	
	xx_axis[0] = axis_org[0] + l_axis[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, &arrow_view);
	label_ratio[0] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[0]);
	x_label[0] = end_screen[0] / label_ratio[0];
	x_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1] + l_axis[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, &arrow_view);
	label_ratio[1] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[1]);
	y_label[0] = end_screen[0] / label_ratio[0];
	y_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2] + l_axis[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_window, view_s->ny_window, &arrow_view);
	label_ratio[2] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[2]);
	z_label[0] = end_screen[0] / label_ratio[0];
	z_label[1] = end_screen[1] / label_ratio[0];
	
	min_l_ratio = label_ratio[0];
	if(label_ratio[1] <= min_l_ratio) min_l_ratio = label_ratio[1];
	if(label_ratio[2] <= min_l_ratio) min_l_ratio = label_ratio[2];
	
	/*
	 printf("x_label %e, %e \n",x_label[0],x_label[1]);
	 printf("y_label %e, %e \n",y_label[0],y_label[1]);
	 printf("z_label %e, %e \n",z_label[0],z_label[1]);
	 */
	
	x_text_x1 = axis_org[0] + label_ratio[0] * ( 1.06 * (1.0 + dist));
	x_text_x2 = axis_org[0] + label_ratio[0];
	x_text_z1 = axis_org[2] - label_ratio[0] * ( 0.05 * (1.0 + dist));
	x_text_z2 = axis_org[2] - label_ratio[0] * ( 0.15 * (1.0 + dist));
	
	y_text_y1 = axis_org[1] + label_ratio[1];
	y_text_y2 = axis_org[1] + label_ratio[1] * ( 1.03 * (1.0 + dist));
	y_text_y3 = axis_org[1] + label_ratio[1] * ( 1.06 * (1.0 + dist));
	y_text_z1 = axis_org[2] - label_ratio[1] * ( 0.05 * (1.0 + dist));
	y_text_z2 = axis_org[2] - label_ratio[1] * ( 0.10 * (1.0 + dist));
	y_text_z3 = axis_org[2] - label_ratio[1] * ( 0.15 * (1.0 + dist));
	
	z_text_z1 = axis_org[2] + label_ratio[2] * ( 1.07 * (1.0 + dist));
	z_text_z2 = axis_org[2] + label_ratio[2];
	z_text_x1 = axis_org[0] - label_ratio[2] * ( 0.07 * (1.0 + dist));
	z_text_x2 = axis_org[0] - label_ratio[2] * ( 0.13 * (1.0 + dist));
	
    /* Generate vertex array*/
	x_arrowx[ 0] = axis_org[0]; x_arrowx[ 3] = axis_org[0] + label_ratio[0];
	x_arrowx[ 1] = axis_org[1]; x_arrowx[ 4] = axis_org[1];
	x_arrowx[ 2] = axis_org[2]; x_arrowx[ 5] = axis_org[2];
	
	x_arrowy[ 0] = axis_org[0]; x_arrowy[ 3] = axis_org[0];
	x_arrowy[ 1] = axis_org[1]; x_arrowy[ 4] = axis_org[1] + label_ratio[1];
	x_arrowy[ 2] = axis_org[2]; x_arrowy[ 5] = axis_org[2];
	
	x_arrowz[ 0] = axis_org[0]; x_arrowz[ 3] = axis_org[0];
	x_arrowz[ 1] = axis_org[1]; x_arrowz[ 4] = axis_org[1];
	x_arrowz[ 2] = axis_org[2]; x_arrowz[ 5] = axis_org[2] + label_ratio[2];
	
	w_ratio[0] = 0.004 * min_l_ratio / label_ratio[0];
	w_ratio[1] = 0.004 * min_l_ratio / label_ratio[1];
	w_ratio[2] = 0.004 * min_l_ratio / label_ratio[2];
	
	
	x_charax[ 0] = x_text_x1; x_charax[ 3] = x_text_x2;
	x_charax[ 6] = x_text_x2; x_charax[ 9] = x_text_x1;
	
	x_charax[ 1] = axis_org[1]; x_charax[ 4] = axis_org[1];
	x_charax[ 7] = axis_org[1]; x_charax[10] = axis_org[1];
	
	x_charax[ 2] = x_text_z1; x_charax[ 5] = x_text_z2;
	x_charax[ 8] = x_text_z1; x_charax[11] = x_text_z2;
	
	x_charay[ 0] = axis_org[0]; x_charay[ 3] = axis_org[0];
	x_charay[ 6] = axis_org[0]; x_charay[ 9] = axis_org[0];
	x_charay[12] = axis_org[0]; x_charay[15] = axis_org[0];
	
	x_charay[ 1] = y_text_y1; x_charay[ 4] = y_text_y2;
	x_charay[ 7] = y_text_y3; x_charay[10] = y_text_y2;
	x_charay[13] = y_text_y2; x_charay[16] = y_text_y2;
	
	x_charay[ 2] = y_text_z1; x_charay[ 5] = y_text_z2;
	x_charay[ 8] = y_text_z1; x_charay[11] = y_text_z2;
	x_charay[14] = y_text_z2; x_charay[17] = y_text_z3;
	
	
	x_charaz[ 0] = z_text_x1; x_charaz[ 3] = z_text_x2;
	x_charaz[ 6] = z_text_x2; x_charaz[ 9] = z_text_x1;
	x_charaz[12] = z_text_x1; x_charaz[15] = z_text_x2;
	
	x_charaz[ 1] = axis_org[1]; x_charaz[ 4] = axis_org[1];
	x_charaz[ 7] = axis_org[1]; x_charaz[10] = axis_org[1];
	x_charaz[13] = axis_org[1]; x_charaz[16] = axis_org[1];
	
	x_charaz[ 2] = z_text_z1; x_charaz[ 5] = z_text_z1;
	x_charaz[ 8] = z_text_z1; x_charaz[11] = z_text_z2;
	x_charaz[14] = z_text_z2; x_charaz[17] = z_text_z2;
	
	/*
	 printf("l_axis %e %e %e \n",l_axis[0],l_axis[1],l_axis[2]);
	 printf("label_ratio %e %e %e \n",label_ratio[0],label_ratio[1],label_ratio[2]);
	 printf("x_text_x1 %e %e %e %e \n",x_text_x1,x_text_x2,x_text_z1,x_text_z2);
	 printf("y_text_y1 %e %e %e %e %e %e \n",y_text_y1,y_text_y2,y_text_y3,
	 y_text_z1,y_text_z2,y_text_z3);
	 printf("z_text_z1 %e %e %e %e \n",z_text_z1,z_text_z2,z_text_x1,z_text_x2);
	 */
	/*  Draw */
	return;
}

static void draw_axis_gl(GLfloat x_arrowx[6], GLfloat x_arrowy[6], GLfloat x_arrowz[6], GLfloat w_ratio[3],
                         GLfloat x_charax[12], GLfloat x_charay[18], GLfloat x_charaz[18]){
    float xyz_buf[22][3];
	float rgba_buf[22][4];
    int icou, k, nd;
    
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, xyz_buf);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, rgba_buf);
    
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glLineWidth(4.0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_CULL_FACE);
    glShadeModel(GL_SMOOTH);
    
    icou = 0;
    /*draw x axis */
    for (k=0; k<2; k++) {
        for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_arrowx[3*k+nd];
        for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = red[nd];
        icou = icou + 1;
    }
    /*draw y axis */
    for (k=0; k<2; k++) {
        for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_arrowy[3*k+nd];
        for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = green[nd];
        icou = icou + 1;
    }
    /*draw z axis */
    for (k=0; k<2; k++) {
        for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_arrowz[3*k+nd];
        for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = blue[nd];
        icou = icou + 1;
    }
    
    
	/*draw 'X" */
	for (k=0; k<4; k++) {
		for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_charax[3*k+nd];
		for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = red[nd];
        icou = icou + 1;
	}
	/*draw 'Y" */
	for (k=0; k<6; k++) {
		for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_charay[3*k+nd];
		for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = blue[nd];
        icou = icou + 1;
	}
	/*draw 'Z' */
	for (k=0; k<6; k++) {
		for(nd=0;nd<3;nd++) xyz_buf[icou][nd] =  x_charaz[3*k+nd];
		for(nd=0;nd<4;nd++) rgba_buf[icou][nd] = green[nd];
        icou = icou + 1;
	}
	glDrawArrays(GL_LINES, IZERO, (ITWO*11));
    
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
    glLineWidth(1.0);
    
	return;
}


void draw_axis(struct view_element *view_s, GLfloat dist){	
	GLfloat x_arrowx[6], x_arrowy[6], x_arrowz[6];
	GLfloat w_ratio[3];
	GLfloat x_charax[12], x_charay[18], x_charaz[18];
	
	set_vertexs_for_axis(view_s, dist, x_arrowx, x_arrowy, x_arrowz, 
						 w_ratio, x_charax, x_charay, x_charaz);
	
	draw_axis_gl(x_arrowx, x_arrowy, x_arrowz, w_ratio, x_charax, x_charay, x_charaz);
	return;
}
