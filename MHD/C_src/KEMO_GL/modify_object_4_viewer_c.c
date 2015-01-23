
/*  modify_object_4_viewer_c.c */

#include "modify_object_4_viewer_c.h"


static void set_center_4_draw_c(struct view_element *view, 
                                double xx_min[3], double xx_max[3], 
                                double center[3], double rmax){	
	int nd;
	for (nd = 0; nd < 3; nd++) {
		if(xx_max[nd] > view->max_point[nd]) view->max_point[nd] = xx_max[nd];
		if(xx_min[nd] > view->max_point[nd]) view->min_point[nd] = xx_min[nd];
        if(view->min_point[nd] > 0.0 || view->max_point[nd] < 0.0){
            view->x_lookat[nd] = center[nd];
        } else {
            view->x_lookat[nd] = ZERO;
        };
	};
    view->iso_scale =ONE;
    
    if (view->r_max < sqrt(3.0*rmax*rmax)){view->r_max = sqrt(3.0*rmax*rmax);};   
//    view->x_lookfrom[2] = TEN*view->r_max;
//    view->iso_scale =  1.0 / view->r_max;
    return;
};


void cal_range_4_mesh_c(struct viewer_mesh *mesh_s, struct view_element *view){
    int nd;
    
    for (nd = 0; nd < 3; nd++) {
		view->max_point[nd] = -1.0e30;
		view->min_point[nd] =  1.0e30;
	};
    view->r_max = ZERO;
	set_center_4_draw_c(view, mesh_s->xx_mesh_min, mesh_s->xx_mesh_max,
                        mesh_s->mesh_center, mesh_s->rmax_mesh);
	return;
}

void cal_psf_viewer_range(struct psf_data **psf_s, struct kemo_array_control *psf_a,  
                          struct psf_data *fline_s, struct fline_menu_val *fline_m, 
                          struct view_element *view){
    int i, nd;
    
    for (nd = 0; nd < 3; nd++) {
		view->max_point[nd] = -1.0e30;
		view->min_point[nd] =  1.0e30;
	};
    view->r_max = ZERO;
    
    if (fline_m->if_draw_fline > 0) {
        set_center_4_draw_c(view, fline_s->xmin_psf, fline_s->xmax_psf, 
                            fline_s->center_psf, fline_s->rmax_psf);
    };
    for (i=0; i<psf_a->num_loaded; i++) {
        if(psf_a->iflag_loaded[i] >= ZERO){
            set_center_4_draw_c(view, psf_s[i]->xmin_psf, psf_s[i]->xmax_psf, 
                                psf_s[i]->center_psf, psf_s[i]->rmax_psf);
        };
    };
    return;
}

void cal_range_4_map_grid_c(struct view_element *view){
	
	view->max_point[0] =  ONE;
	view->max_point[1] =  HALF;
	view->max_point[2] =  ONE;
	
	view->r_max = ONE;
    view->iso_scale =ONE;
    	
	view->x_lookat[0] = ZERO;
	view->x_lookat[1] = ZERO;
	view->x_lookat[2] = ZERO;

    return;
}


void modify_object_multi_viewer_c(double dist, struct viewer_mesh *mesh_s){
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

void set_axis_positions(struct view_element *view, GLfloat dist,
                        GLfloat *axis_delta, GLfloat *axis_org){
	axis_delta[0] = 1.2 * (view->max_point[0] + dist + 0.3);
	axis_delta[1] = 1.2 * (view->max_point[1] + dist + 0.3);
	axis_delta[2] = 1.2 * (view->max_point[2] + dist + 0.3);
	
	axis_org[0] = view->x_lookat[0];
	axis_org[1] = view->x_lookat[1];
	axis_org[2] = view->x_lookat[2];
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
