
/* set_axis_to_buf.c */

#include "set_axis_to_buf.h"

static const float red[4] =   {RED_R, RED_G, RED_B, RED_A};
static const float blue[4] =  {BLUE_R, BLUE_G, BLUE_B, BLUE_A};
static const float green[4] = {L_GREEN_R, L_GREEN_G, L_GREEN_B, L_GREEN_A};

double radius_tmp;
double label_ratio_tmp[3];

static float set_ratio_4_axislabel(struct view_element *view_s, 
                                   int end_screen[2], int zero_screen[2], float l_axis){
    float ratio;
    float ratio_tmp[2];
	
	if ( end_screen[0] < view_s->nx_frame/10 ) { 
		ratio_tmp[0] = (float) (view_s->nx_frame/10 - zero_screen[0])
        / (float) (end_screen[0] - zero_screen[0]);
	} else if  ( end_screen[0] > view_s->nx_frame*9/10 ) {
		ratio_tmp[0] = (float) (zero_screen[0]-view_s->nx_frame*9/10)
        / (float) (zero_screen[0] - end_screen[0]);
	} else {
		ratio_tmp[0] = ONE;
	};
	
	if ( end_screen[1] < view_s->ny_frame/10 ) { 
		ratio_tmp[1] = (float) (view_s->ny_frame/10 - zero_screen[1])
        / (float) (end_screen[1] - zero_screen[1]);
	} else if  ( end_screen[1] > view_s->ny_frame*9/10 ) {
		ratio_tmp[1] = (float) (zero_screen[1]-view_s->ny_frame*9/10)
        / (float) (zero_screen[1] - end_screen[1]);
	} else {
		ratio_tmp[1] = ONE;
	};
	
	if (ratio_tmp[0] < ratio_tmp[1]) { ratio = ratio_tmp[0]; }
	else { ratio = ratio_tmp[1]; };
	
	ratio = ratio * l_axis;
	return ratio;
}

static double set_minimum_length_view(struct view_element *view_s, double dist_mesh,
                                      double axis_org[3], double label_ratio[3]){
    double min_l_ratio;
    
    double xx_axis[3];
    double x_label[2], y_label[2], z_label[2];
    double l_axis[3];
	
	int zero_screen[2], end_screen[2];
	
	set_axis_positions(view_s, dist_mesh, l_axis, axis_org);
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(zero_screen, xx_axis, 
                              view_s->nx_frame, view_s->ny_frame, view_s);
	
	xx_axis[0] = axis_org[0] + l_axis[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_frame, view_s->ny_frame, view_s);
	label_ratio[0] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[0]);
	x_label[0] = end_screen[0] / label_ratio[0];
	x_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1] + l_axis[1];
	xx_axis[2] = axis_org[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_frame, view_s->ny_frame, view_s);
	label_ratio[1] = set_ratio_4_axislabel(view_s, end_screen, zero_screen, l_axis[1]);
	y_label[0] = end_screen[0] / label_ratio[0];
	y_label[1] = end_screen[1] / label_ratio[0];
	
	xx_axis[0] = axis_org[0];
	xx_axis[1] = axis_org[1];
	xx_axis[2] = axis_org[2] + l_axis[2];
	set_3d_position_to_window(end_screen, xx_axis, 
                              view_s->nx_frame, view_s->ny_frame, view_s);
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
    return min_l_ratio;
}

static void scale_axis_positions(double dist_mesh,	double axis_org[3],
                                 double fscale,  double label_ratio[3], double min_l_ratio,
                                double x_arrowx[8],  double x_arrowy[8], 
                                double x_arrowz[8],  double w_ratio[3],
                                double x_charax[16], double x_charay[24],
                                double x_charaz[24]){
	double x_text_x1, x_text_x2, x_text_z1, x_text_z2;
	double y_text_y1, y_text_y2, y_text_z1, y_text_z2;
	double y_text_y3, y_text_z3;
	double z_text_z1, z_text_z2, z_text_x1, z_text_x2;
	
    /* Generate vertex array*/
    x_arrowx[ 0] = axis_org[0];
    x_arrowx[ 1] = axis_org[1];
    x_arrowx[ 2] = axis_org[2];
    x_arrowx[ 3] = 1.0;
    
    x_arrowx[ 4] = axis_org[0] + label_ratio[0];
    x_arrowx[ 5] = axis_org[1];
    x_arrowx[ 6] = axis_org[2];
    x_arrowx[ 7] = 1.0;
    
    
    x_arrowy[ 0] = axis_org[0];
    x_arrowy[ 1] = axis_org[1];
    x_arrowy[ 2] = axis_org[2];
    x_arrowy[ 3] = 1.0;
    
    x_arrowy[ 4] = axis_org[0];
    x_arrowy[ 5] = axis_org[1] + label_ratio[1];
    x_arrowy[ 6] = axis_org[2];
    x_arrowy[ 7] = 1.0;
	
    x_arrowz[ 0] = axis_org[0];
    x_arrowz[ 1] = axis_org[1];
    x_arrowz[ 2] = axis_org[2];
    x_arrowz[ 3] = 1.0;
    
    x_arrowz[ 4] = axis_org[0];
    x_arrowz[ 5] = axis_org[1];
    x_arrowz[ 6] = axis_org[2] + label_ratio[2];
    x_arrowz[ 7] = 1.0;
	
	w_ratio[0] = 0.004 * min_l_ratio / label_ratio[0];
	w_ratio[1] = 0.004 * min_l_ratio / label_ratio[1];
	w_ratio[2] = 0.004 * min_l_ratio / label_ratio[2];
	
	
    
    
	x_text_x1 = x_arrowx[4] + fscale * min_l_ratio * (0.06 + 1.06*dist_mesh);
	x_text_x2 = x_arrowx[4];
	x_text_z1 = x_arrowy[2] - fscale * min_l_ratio * ( 0.05 * (1.0 + dist_mesh));
	x_text_z2 = x_arrowy[2] - fscale * min_l_ratio * ( 0.15 * (1.0 + dist_mesh));
	
	y_text_y1 = x_arrowy[5];
	y_text_y2 = x_arrowy[5] + fscale * min_l_ratio * (0.03 + 1.03 *dist_mesh);
	y_text_y3 = x_arrowy[5] + fscale * min_l_ratio * (0.06 + 1.06 *dist_mesh);
	y_text_z1 = x_arrowy[2] - fscale * min_l_ratio * ( 0.05 * (1.0 + dist_mesh));
	y_text_z2 = x_arrowy[2] - fscale * min_l_ratio * ( 0.10 * (1.0 + dist_mesh));
	y_text_z3 = x_arrowy[2] - fscale * min_l_ratio * ( 0.15 * (1.0 + dist_mesh));
	
	z_text_z1 = x_arrowz[6] + fscale * min_l_ratio * (0.07 + 1.07 * dist_mesh);
	z_text_z2 = x_arrowz[6];
	z_text_x1 = axis_org[0] - fscale * min_l_ratio * ( 0.07 * (1.0 + dist_mesh));
	z_text_x2 = axis_org[0] - fscale * min_l_ratio * ( 0.13 * (1.0 + dist_mesh));
	
    
    x_charax[ 0] = x_text_x1;
    x_charax[ 1] = x_arrowx[ 1];
    x_charax[ 2] = x_text_z1;
    x_charax[ 3] = 1.0;
    
    x_charax[ 4] = x_text_x2;
    x_charax[ 5] = x_arrowx[ 1];
    x_charax[ 6] = x_text_z2;
    x_charax[ 7] = 1.0;
    
    x_charax[ 8] = x_text_x2;
    x_charax[ 9] = x_arrowx[ 1];
    x_charax[10] = x_text_z1;
    x_charax[11] = 1.0;
    
    x_charax[12] = x_text_x1;
    x_charax[13] = x_arrowx[ 1];
    x_charax[14] = x_text_z2;
    x_charax[15] = 1.0;
    
    
    x_charay[ 0] = x_arrowy[ 0];
    x_charay[ 1] = y_text_y1;
    x_charay[ 2] = y_text_z1;
    x_charay[ 3] = 1.0;
    
    x_charay[ 4] = x_arrowy[ 0];
    x_charay[ 5] = y_text_y2;
    x_charay[ 6] = y_text_z2;
    x_charay[ 7] = 1.0;
    
    x_charay[ 8] = x_arrowy[ 0];
    x_charay[ 9] = y_text_y3;
    x_charay[10] = y_text_z1;
    x_charay[11] = 1.0;
    
    x_charay[12] = x_arrowy[ 0];
    x_charay[13] = y_text_y2;
    x_charay[14] = y_text_z2;
    x_charay[15] = 1.0;
    
    x_charay[16] = x_arrowy[ 0];
    x_charay[17] = y_text_y2;
    x_charay[18] = y_text_z2;
    x_charay[19] = 1.0;
    
    x_charay[20] = x_arrowy[ 0];
    x_charay[21] = y_text_y2;
    x_charay[22] = y_text_z3;
    x_charay[23] = 1.0;
	
	
    x_charaz[ 0] = z_text_x1;
    x_charaz[ 1] = x_arrowz[ 1];
    x_charaz[ 2] = z_text_z1;
    x_charaz[ 3] = 1.0;
    
    x_charaz[ 4] = z_text_x2;
    x_charaz[ 5] = x_arrowz[ 1];
    x_charaz[ 6] = z_text_z1;
    x_charaz[ 7] = 1.0;
    
    x_charaz[ 8] = z_text_x2;
    x_charaz[ 9] = x_arrowz[ 1];
    x_charaz[10] = z_text_z1;
    x_charaz[11] = 1.0;
    
    x_charaz[12] = z_text_x1;
    x_charaz[13] = x_arrowz[ 1];
    x_charaz[14] = z_text_z2;
    x_charaz[15] = 1.0;
    
    x_charaz[16] = z_text_x1;
    x_charaz[17] = x_arrowz[ 1];
    x_charaz[18] = z_text_z2;
    x_charaz[19] = 1.0;
    
    x_charaz[20] = z_text_x2;
    x_charaz[21] = x_arrowz[ 1];
    x_charaz[22] = z_text_z2;
    x_charaz[23] = 1.0;
/*
	 printf("label_ratio %e %e %e \n",label_ratio[0],label_ratio[1],label_ratio[2]);
	 printf("x_text_x1 %e %e %e %e \n",x_text_x1,x_text_x2,x_text_z1,x_text_z2);
	 printf("y_text_y1 %e %e %e %e %e %e \n",y_text_y1,y_text_y2,y_text_y3,
	 y_text_z1,y_text_z2,y_text_z3);
	 printf("z_text_z1 %e %e %e %e \n",z_text_z1,z_text_z2,z_text_x1,z_text_x2);
*/
	 return;
}

static long set_axis_rod_to_buf(long ist_tube, int ncorner, float radius,
                                double x_arrowx[8], double x_arrowy[8], double x_arrowz[8],
                                double x_charax[16], double x_charay[24], double x_charaz[24],
                                struct gl_strided_buffer *strided_buf){
	double dir_line[8];
	double color_2p[8];
	int k, nd;
    /*draw x axis */
    dir_line[3] =  1.0;
    dir_line[7] =  1.0;
    
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_arrowx[4+nd] - x_arrowx[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
	}
	/*
	printf("x_arrowx1 %f %f %f \n", x_arrowx[0], x_arrowx[1], x_arrowx[2]);
	printf("x_arrowx2 %f %f %f \n", x_arrowx[4], x_arrowx[5], x_arrowx[6]);
	printf("dir_line1 %f %f %f \n", dir_line[0], dir_line[1], dir_line[2]);
	*/
    long icou_tube = ist_tube;
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, x_arrowx, 
                                         dir_line, color_2p, strided_buf);
	
    /*draw y axis */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_arrowy[4+nd] - x_arrowy[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, x_arrowy, 
                                         dir_line, color_2p, strided_buf);
	
    /*draw z axis */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_arrowz[4+nd] - x_arrowz[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, x_arrowz,
                                         dir_line, color_2p, strided_buf);
	
	
	/*draw 'X' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charax[4+nd] - x_charax[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charax[0],
                                         dir_line, color_2p, strided_buf);
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charax[12+nd] - x_charax[8+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  red[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charax[8],
                                         dir_line, color_2p, strided_buf);
	
	/*draw 'Y' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charay[4+nd] - x_charay[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charay[0],
                                         dir_line, color_2p, strided_buf);
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charay[12+nd] - x_charay[8+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charay[8],
                                         dir_line, color_2p, strided_buf);
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charay[20+nd] - x_charay[16+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  green[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charay[16],
                                         dir_line, color_2p, strided_buf);
	
	/*draw 'Z' */
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charaz[ 4+nd] - x_charaz[nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charaz[0],
                                         dir_line, color_2p, strided_buf);
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charaz[12+nd] - x_charaz[8+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charaz[8],
                                         dir_line, color_2p, strided_buf);
	for (k=0; k<2; k++) {
		for(nd=0;nd<3;nd++){dir_line[4*k+nd] =  x_charaz[20+nd] - x_charaz[16+nd];};
		for(nd=0;nd<4;nd++){color_2p[4*k+nd] =  blue[nd];};
    }
	icou_tube = set_tube_strided_buffer(icou_tube, ncorner, radius, &x_charaz[16],
                                         dir_line, color_2p, strided_buf);
    
	return icou_tube;
}

static double rescale_radius_for_axis(struct view_element *view_s, double min_l_ratio){
	return (min_l_ratio * (1.0 + (float)view_s->iflag_retina) / (float) view_s->ny_frame);
}

long count_axis_to_buf(int ncorner){
	long npatch_wall = 0;
    /*draw x axis */
	npatch_wall = npatch_wall + 2*ncorner;
    /*draw y axis */
	npatch_wall = npatch_wall + 2*ncorner;
    /*draw z axis */
	npatch_wall = npatch_wall + 2*ncorner;
	
	
	/*draw 'X' */
	npatch_wall = npatch_wall + 4*ncorner;
	/*draw 'Y' */
	npatch_wall = npatch_wall + 6*ncorner;
	/*draw 'Z' */
	npatch_wall = npatch_wall + 6*ncorner;
    
	return npatch_wall;
}

double set_tube_radius_by_axis(struct view_element *view_s){
    double axis_org[3], label_ratio[3];
    double min_l_ratio = set_minimum_length_view(view_s, ZERO, axis_org, label_ratio);
    double radius = rescale_radius_for_axis(view_s, min_l_ratio);
	return radius;
};



void set_flex_axis_to_buf(struct view_element *view_s,
                          int iflag_draw_axis, double dist_mesh, double radius_ref,
                          struct gl_strided_buffer *strided_buf){
	double x_arrowx[8], x_arrowy[8], x_arrowz[8];
	double w_ratio[3];
    double x_charax[16], x_charay[24], x_charaz[24];
    double axis_org[3], label_ratio[3];
    double min_l_ratio;
    double radius;
	long icou_patch = 0;
    
    int i;
    
    if(iflag_draw_axis > 0){
        long n_vertex = ITHREE * count_axis_to_buf(view_s->ncorner_tube);
        set_buffer_address_4_patch(n_vertex, strided_buf);
        resize_strided_buffer(strided_buf);
        
        min_l_ratio = set_minimum_length_view(view_s, dist_mesh, axis_org, label_ratio);
        scale_axis_positions(dist_mesh, axis_org, 1.0, label_ratio, min_l_ratio,
                             x_arrowx, x_arrowy, x_arrowz, w_ratio,
                             x_charax, x_charay, x_charaz);
        radius = rescale_radius_for_axis(view_s, min_l_ratio);
        
        icou_patch = set_axis_rod_to_buf(0, view_s->ncorner_tube, (radius*radius_ref),
                                         x_arrowx, x_arrowy, x_arrowz, 
                                         x_charax, x_charay, x_charaz,
                                         strided_buf);
    }else{
        strided_buf->num_nod_buf = 0;
    };
	return;
};

void set_lower_flex_axis_to_buf(struct view_element *view_s,
                                int iflag_draw_axis, double dist_mesh, double radius_ref,
                                struct gl_strided_buffer *strided_buf){
	double x_arrowx[8], x_arrowy[8], x_arrowz[8];
	double w_ratio[3];
    double x_charax[16], x_charay[24], x_charaz[24];
    double axis_org[3], label_ratio[3];
    double min_l_ratio;
    double radius;
	long icou_patch = 0;
    
    int i;
    
    if(iflag_draw_axis > 0){
        long n_vertex = ITHREE * count_axis_to_buf(view_s->ncorner_tube);
        set_buffer_address_4_patch(n_vertex, strided_buf);
        resize_strided_buffer(strided_buf);
        
        
        min_l_ratio = set_minimum_length_view(view_s, dist_mesh, axis_org, label_ratio);
        int nd;
        scale_axis_positions(dist_mesh, axis_org, 4.0, label_ratio, min_l_ratio,
                             x_arrowx, x_arrowy, x_arrowz, w_ratio,
                             x_charax, x_charay, x_charaz);
        radius = rescale_radius_for_axis(view_s, min_l_ratio);
        
        for(i=0;i<2;i++){
            for(nd=0;nd<3;nd++){x_arrowx[4*i+nd] = 0.12 * x_arrowx[4*i+nd];}
            for(nd=0;nd<3;nd++){x_arrowy[4*i+nd] = 0.12 * x_arrowy[4*i+nd];}
            for(nd=0;nd<3;nd++){x_arrowz[4*i+nd] = 0.12 * x_arrowz[4*i+nd];}
        }
        for(i=0;i<4;i++){
            for(nd=0;nd<3;nd++){x_charax[4*i+nd] = 0.12 * x_charax[4*i+nd];}
        }
        for(i=0;i<6;i++){
            for(nd=0;nd<3;nd++){x_charay[4*i+nd] = 0.12 * x_charay[4*i+nd];}
            for(nd=0;nd<3;nd++){x_charaz[4*i+nd] = 0.12 * x_charaz[4*i+nd];}
        }
        

        double shift_on_screen[4] = {-0.3*radius*view_s->nx_frame,
                                     -0.3*radius*view_s->ny_frame,
                                     0.0, 1.0};
        
        double screeen_arrowx[8], screeen_arrowy[8], screeen_arrowz[8];
        double screeen_charax[16], screeen_charay[24], screeen_charaz[24];
        
        transform_frame_xyzw(2, x_arrowx, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowx);
        transform_frame_xyzw(2, x_arrowy, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowy);
        transform_frame_xyzw(2, x_arrowz, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowz);
        transform_frame_xyzw(4, x_charax, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charax);
        transform_frame_xyzw(6, x_charay, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charay);
        transform_frame_xyzw(6, x_charaz, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charaz);
        
        for(i=0;i<2;i++){
            for(nd=0;nd<3;nd++){screeen_arrowx[4*i+nd] = screeen_arrowx[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_arrowy[4*i+nd] = screeen_arrowy[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_arrowz[4*i+nd] = screeen_arrowz[4*i+nd] + shift_on_screen[nd];}
        }
        for(i=0;i<4;i++){
            for(nd=0;nd<3;nd++){screeen_charax[4*i+nd] = screeen_charax[4*i+nd] + shift_on_screen[nd];}
        }
        for(i=0;i<6;i++){
            for(nd=0;nd<3;nd++){screeen_charay[4*i+nd] = screeen_charay[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_charaz[4*i+nd] = screeen_charaz[4*i+nd] + shift_on_screen[nd];}
        }
        
        double a_inv[4][4];
        cal_inverse_44_matrix_c(&view_s->mat_object_2_eye[0], a_inv);
        transform_frame_xyzw(2, screeen_arrowx, a_inv, x_arrowx);
        transform_frame_xyzw(2, screeen_arrowy, a_inv, x_arrowy);
        transform_frame_xyzw(2, screeen_arrowz, a_inv, x_arrowz);
        transform_frame_xyzw(4, screeen_charax, a_inv, x_charax);
        transform_frame_xyzw(6, screeen_charay, a_inv, x_charay);
        transform_frame_xyzw(6, screeen_charaz, a_inv, x_charaz);
        icou_patch = set_axis_rod_to_buf(icou_patch, view_s->ncorner_tube, 0.7*(radius*radius_ref),
                                         x_arrowx, x_arrowy, x_arrowz,
                                         x_charax, x_charay, x_charaz,
                                         strided_buf);
    }else{
        strided_buf->num_nod_buf = 0;
    };
    radius_tmp = radius;
    label_ratio_tmp[0] = label_ratio[0];
    label_ratio_tmp[1] = label_ratio[1];
    label_ratio_tmp[2] = label_ratio[2];
	return;
};


void set_lower_fixed_axis_to_buf(struct view_element *view_s,
                                 int iflag_draw_axis, double dist_mesh, double radius_ref,
                                 struct gl_strided_buffer *strided_buf){
	double x_arrowx[8], x_arrowy[8], x_arrowz[8];
	double w_ratio[3];
    double x_charax[16], x_charay[24], x_charaz[24];
    double axis_org[3], label_ratio[3];
    double min_l_ratio;
    double radius;
	long icou_patch = 0;
    
    int i, nd;
    
    if(iflag_draw_axis > 0){
        long n_vertex = ITHREE * count_axis_to_buf(view_s->ncorner_tube);
        set_buffer_address_4_patch(n_vertex, strided_buf);
        resize_strided_buffer(strided_buf);
        
        
        min_l_ratio = set_minimum_length_view(view_s, dist_mesh, axis_org, label_ratio);
        scale_axis_positions(dist_mesh, axis_org, 4.0, label_ratio, min_l_ratio,
                             x_arrowx, x_arrowy, x_arrowz, w_ratio,
                             x_charax, x_charay, x_charaz);
        radius = rescale_radius_for_axis(view_s, min_l_ratio);
        
        label_ratio[0] = label_ratio_tmp[0];
        label_ratio[1] = label_ratio_tmp[1];
        label_ratio[2] = label_ratio_tmp[2];
        radius = radius_tmp;
        

        for(i=0;i<2;i++){
            for(nd=0;nd<3;nd++){x_arrowx[4*i+nd] = 0.12 * x_arrowx[4*i+nd];}
            for(nd=0;nd<3;nd++){x_arrowy[4*i+nd] = 0.12 * x_arrowy[4*i+nd];}
            for(nd=0;nd<3;nd++){x_arrowz[4*i+nd] = 0.12 * x_arrowz[4*i+nd];}
        }
        for(i=0;i<4;i++){
            for(nd=0;nd<3;nd++){x_charax[4*i+nd] = 0.12 * x_charax[4*i+nd];}
        }
        for(i=0;i<6;i++){
            for(nd=0;nd<3;nd++){x_charay[4*i+nd] = 0.12 * x_charay[4*i+nd];}
            for(nd=0;nd<3;nd++){x_charaz[4*i+nd] = 0.12 * x_charaz[4*i+nd];}
        }
        

        double shift_on_screen[4] = {-0.3*radius*view_s->nx_frame,
                                     -0.3*radius*view_s->ny_frame,
                                     0.0, 1.0};
        
        double screeen_arrowx[8], screeen_arrowy[8], screeen_arrowz[8];
        double screeen_charax[16], screeen_charay[24], screeen_charaz[24];
        
        transform_frame_xyzw(2, x_arrowx, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowx);
        transform_frame_xyzw(2, x_arrowy, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowy);
        transform_frame_xyzw(2, x_arrowz, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_arrowz);
        transform_frame_xyzw(4, x_charax, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charax);
        transform_frame_xyzw(6, x_charay, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charay);
        transform_frame_xyzw(6, x_charaz, (double **) &view_s->mat_object_2_eye[0], 
                             screeen_charaz);
        
        for(i=0;i<2;i++){
            for(nd=0;nd<3;nd++){screeen_arrowx[4*i+nd] = screeen_arrowx[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_arrowy[4*i+nd] = screeen_arrowy[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_arrowz[4*i+nd] = screeen_arrowz[4*i+nd] + shift_on_screen[nd];}
        }
        for(i=0;i<4;i++){
            for(nd=0;nd<3;nd++){screeen_charax[4*i+nd] = screeen_charax[4*i+nd] + shift_on_screen[nd];}
        }
        for(i=0;i<6;i++){
            for(nd=0;nd<3;nd++){screeen_charay[4*i+nd] = screeen_charay[4*i+nd] + shift_on_screen[nd];}
            for(nd=0;nd<3;nd++){screeen_charaz[4*i+nd] = screeen_charaz[4*i+nd] + shift_on_screen[nd];}
        }
        
        double a_inv[4][4];
        cal_inverse_44_matrix_c(&view_s->mat_object_2_eye[0], a_inv);
        transform_frame_xyzw(2, screeen_arrowx, a_inv, x_arrowx);
        transform_frame_xyzw(2, screeen_arrowy, a_inv, x_arrowy);
        transform_frame_xyzw(2, screeen_arrowz, a_inv, x_arrowz);
        transform_frame_xyzw(4, screeen_charax, a_inv, x_charax);
        transform_frame_xyzw(6, screeen_charay, a_inv, x_charay);
        transform_frame_xyzw(6, screeen_charaz, a_inv, x_charaz);
        
        icou_patch = set_axis_rod_to_buf(0, view_s->ncorner_tube, 0.7*(radius*radius_ref),
                                         x_arrowx, x_arrowy, x_arrowz,
                                         x_charax, x_charay, x_charaz,
                                         strided_buf);
    }else{
        strided_buf->num_nod_buf = 0;
    };
	return;
};
