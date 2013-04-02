
 /*  modify_object_4_viewer_c.c */

#include "modify_object_4_viewer_c.h"


static double min_point[3];
static double max_point[3];
static double center[3];

GLdouble axis[3];


static void set_center_4_draw_c(struct view_element *view){
	int nd;
	
	view->r_max = 0.0;
	for (nd = 0; nd < 3; nd++) center[nd] = 0.5 * (max_point[nd]+min_point[nd]);
	for (nd = 0; nd < 3; nd++) {
		if(view->r_max < (max_point[nd]-min_point[nd])) view->r_max = 0.5*(max_point[nd]-min_point[nd]);
	}
	
	if(min_point[0] < 0.0 && max_point[0] > 0.0) center[0] = 0.0;
	if(min_point[1] < 0.0 && max_point[1] > 0.0) center[1] = 0.0;
	if(min_point[2] < 0.0 && max_point[2] > 0.0) center[2] = 0.0;
	
	view->r_max = sqrt(3.0*view->r_max*view->r_max);
	view->scale_factor[0] =  1.0 / view->r_max;
	view->scale_factor[1] =  1.0 / view->r_max;
	view->scale_factor[2] =  1.0 / view->r_max;
	view->x_lookat[0] = center[0];
	view->x_lookat[1] = center[1];
	view->x_lookat[2] = center[2];
	
	printf("r_max: %.12e  %.12e \n", view->r_max, view->scale_factor[0]);
	printf("center: %.12e %.12e %.12e \n", center[0],center[1],center[2]);
	return;
};


static void cal_range_4_draw_c(struct viewer_mesh *mesh_s, struct view_element *view){
	int ip, nd;
	
	for (nd = 0; nd < 3; nd++) {
		min_point[nd] = mesh_s->domain_min[0][nd];
		max_point[nd] = mesh_s->domain_max[0][nd];
	};
	for (ip = 1; ip < mesh_s->num_pe_sf; ip++) {
		for (nd = 0; nd < 3; nd++) {
			if ( min_point[nd] > mesh_s->domain_min[ip][nd]) {
				min_point[nd] = mesh_s->domain_min[ip][nd];
				}
			if ( max_point[nd] < mesh_s->domain_max[ip][nd]) {
				max_point[nd] = mesh_s->domain_max[ip][nd];
				}
		}
	}
	
	set_center_4_draw_c(view);
	/*
	printf("min_point: %.12e %.12e %.12e \n", min_point[0],min_point[1],min_point[2]);
	printf("max_point: %.12e %.12e %.12e \n", max_point[0],max_point[1],max_point[2]);
	*/
	return;
}

void cal_range_4_psf_grid_c(struct psf_data *psf_s, struct view_element *view){
	int i, nd;
	
	for (nd = 0; nd < 3; nd++) {
		min_point[nd] = psf_s->xx_viz[0][nd];
		max_point[nd] = psf_s->xx_viz[0][nd];
	}
	for (i = 1; i < psf_s->nnod_viz; i++) {
		for (nd = 0; nd < 3; nd++) {
			if ( min_point[nd] > psf_s->xx_viz[i][nd]) {
				min_point[nd] = psf_s->xx_viz[i][nd];
				}
			if ( max_point[nd] < psf_s->xx_viz[i][nd]) {
				max_point[nd] = psf_s->xx_viz[i][nd];
				}
		}
	}
	
	set_center_4_draw_c(view);
	/*
	printf("min_point: %.12e %.12e %.12e \n", min_point[0],min_point[1],min_point[2]);
	printf("max_point: %.12e %.12e %.12e \n", max_point[0],max_point[1],max_point[2]);
	*/
	return;
}

void cal_range_4_map_grid_c(struct view_element *view){
	
	min_point[0] = -ONE;
	min_point[1] = -HALF;
	min_point[2] =  ZERO;
	max_point[0] =  ONE;
	max_point[1] =  HALF;
	max_point[2] =  ONE;
	
	center[0] = ZERO;
	center[1] = ZERO;
	center[2] = ZERO;
	view->r_max = ONE;
	
	view->scale_factor[0] =  1.0 / view->r_max;
	view->scale_factor[1] =  1.0 / view->r_max;
	view->scale_factor[2] =  1.0 / view->r_max;
	view->x_lookat[0] = center[0];
	view->x_lookat[1] = center[1];
	view->x_lookat[2] = center[2];
	
	printf("r_max: %.12e  %.12e \n", view->r_max, view->scale_factor[0]);
	printf("center: %.12e %.12e %.12e \n", center[0],center[1],center[2]);
	
	printf("min_point: %.12e %.12e %.12e \n", min_point[0],min_point[1],min_point[2]);
	printf("max_point: %.12e %.12e %.12e \n", max_point[0],max_point[1],max_point[2]);
	
	return;
}


static void modify_object_multi_viewer_c(double dist, struct viewer_mesh *mesh_s, 
			struct view_element *view){
	int ip, i, ist, ied;
	
	for (ip = 0; ip < mesh_s->num_pe_sf; ip++) {
		ist = mesh_s->inod_sf_stack[ip];
		ied = mesh_s->inod_sf_stack[ip+1];
		for (i = ist; i < ied; i++) {
			mesh_s->xx_draw[i][0] = ( mesh_s->xx_view[i][0] 
					+ dist * ( mesh_s->domain_center[ip][0] ) );
			mesh_s->xx_draw[i][1] = ( mesh_s->xx_view[i][1]
					+ dist * ( mesh_s->domain_center[ip][1] ) );
			mesh_s->xx_draw[i][2] = ( mesh_s->xx_view[i][2] 
					+ dist * ( mesh_s->domain_center[ip][2] ) );
		}
	}
	
	return;
}

static void modify_object_sngl_viewer_c(struct viewer_mesh *mesh_s){
	int i;
	
	for (i = 0; i < mesh_s->nodpetot_viewer; i++) {
		mesh_s->xx_draw[i][0] = ( mesh_s->xx_view[i][0] );
		mesh_s->xx_draw[i][1] = ( mesh_s->xx_view[i][1] );
		mesh_s->xx_draw[i][2] = ( mesh_s->xx_view[i][2] );
	}
	return;
}

void set_axis_positions(GLfloat dist, GLfloat *axis_delta, GLfloat *axis_org){
	axis_delta[0] = 1.2 * (max_point[0] + dist + 0.3);
	axis_delta[1] = 1.2 * (max_point[1] + dist + 0.3);
	axis_delta[2] = 1.2 * (max_point[2] + dist + 0.3);
	
	axis_org[0] = center[0];
	axis_org[1] = center[1];
	axis_org[2] = center[2];
	return;
};



void modify_object_for_mesh(double dist, struct viewer_mesh *mesh_s,
			struct view_element *view){
	
	cal_range_4_draw_c(mesh_s, view);
	modify_object_multi_viewer_c(dist, mesh_s, view);
	
	return;
};


void set_axis_positions_d(double dist, GLdouble *axis_delta, GLdouble *axis_org){
	axis_delta[0] = 1.2 * (max_point[0] + dist + 0.3);
	axis_delta[1] = 1.2 * (max_point[1] + dist + 0.3);
	axis_delta[2] = 1.2 * (max_point[2] + dist + 0.3);
	
	axis_org[0] = center[0];
	axis_org[1] = center[1];
	axis_org[2] = center[2];
	return;
};

int set_viewtype(struct view_element *view, int selected, int current_view){
	if(current_view == VIEW_MAP
				|| current_view == VIEW_XY
				|| current_view == VIEW_XZ
				|| current_view == VIEW_YZ){
		reset_to_init_angle(view);
	};
	
	if(selected == VIEW_MAP) {
		cal_range_4_map_grid_c(view);
	}
	else if(selected == VIEW_XY) {
		view_for_xy_plane(view);
	}
	else if(selected == VIEW_XZ) {
		view_for_xz_plane(view);
	}
	else if(selected == VIEW_YZ) {
		view_for_yz_plane(view);
	};
	
	return selected;
};
