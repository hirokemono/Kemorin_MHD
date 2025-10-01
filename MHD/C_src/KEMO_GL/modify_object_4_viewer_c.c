
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
    
    if (view->r_max < sqrt(3.0*rmax*rmax)){view->r_max = sqrt(3.0*rmax*rmax);};   
    view->iso_scale =  1.0 / view->r_max;
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

void cal_psf_viewer_range(struct psf_data **psf_s,   struct kemo_array_control *psf_a,  
                          struct psf_data *fline_d,  struct psf_menu_val *fline_m, 
                          struct psf_data *tracer_d, struct psf_menu_val *tracer_m, 
                          struct view_element *view){
    int i, nd;
    
    for (nd = 0; nd < 3; nd++) {
		view->max_point[nd] = -1.0e30;
		view->min_point[nd] =  1.0e30;
	};
    view->r_max = ZERO;
    
    if (tracer_m->iflag_draw_viz > 0) {
        set_center_4_draw_c(view, tracer_d->xmin_psf, tracer_d->xmax_psf,
                            tracer_d->center_psf, tracer_d->rmax_psf);
    };
    if (fline_m->iflag_draw_viz > 0) {
        set_center_4_draw_c(view, fline_d->xmin_psf, fline_d->xmax_psf,
                            fline_d->center_psf, fline_d->rmax_psf);
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
	
/*    Skip normalize scale factor
    view->r_max = ONE;
/	view->iso_scale =  1.0 / view->r_max;
*/
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
			mesh_s->xyzw_draw[4*i  ] = (mesh_s->xx_view[4*i  ]
                                     + dist * mesh_s->domain_center[4*ip  ]);
			mesh_s->xyzw_draw[4*i+1] = (mesh_s->xx_view[4*i+1]
                                     + dist * mesh_s->domain_center[4*ip+1]);
			mesh_s->xyzw_draw[4*i+2] = (mesh_s->xx_view[4*i+2]
                                     + dist * mesh_s->domain_center[4*ip+2]);
		}
	}
	
	return;
}

void modify_object_sngl_viewer_c(struct viewer_mesh *mesh_s){
	int i;
	
	for (i = 0; i < mesh_s->nnod_viewer; i++) {
		mesh_s->xyzw_draw[4*i  ] = ( mesh_s->xx_view[4*i  ] );
		mesh_s->xyzw_draw[4*i+1] = ( mesh_s->xx_view[4*i+1] );
		mesh_s->xyzw_draw[4*i+2] = ( mesh_s->xx_view[4*i+2] );
	}
	return;
}

void set_axis_positions(struct view_element *view, double dist,
                        double *axis_delta, double *axis_org){
	axis_delta[0] = 1.2 * (view->max_point[0] + dist + 0.3);
	axis_delta[1] = 1.2 * (view->max_point[1] + dist + 0.3);
	axis_delta[2] = 1.2 * (view->max_point[2] + dist + 0.3);
	
	axis_org[0] = view->x_lookat[0];
	axis_org[1] = view->x_lookat[1];
	axis_org[2] = view->x_lookat[2];
	return;
};

double set_tube_radius_by_view(struct view_element *view_s, double radius){
	double size;
	
	size = sqrt(view_s->max_point[0]*view_s->max_point[0] 
				+ view_s->max_point[1]*view_s->max_point[1]
				+ view_s->max_point[2]*view_s->max_point[2]);
	radius = radius * size * (1.0 + (float)view_s->iflag_retina)
			/ (float) view_s->ny_frame;
	
	return radius;
}


void set_viewtype(struct view_element *view, int selected){
	if(view->iflag_view_type == VIEW_MAP
       || view->iflag_view_type == VIEW_XY
       || view->iflag_view_type == VIEW_XZ
       || view->iflag_view_type == VIEW_YZ){
		reset_to_init_angle(view);
	};
	view->iflag_view_type = selected;
	
	if(view->iflag_view_type == VIEW_MAP) {
		cal_range_4_map_grid_c(view);
	}
	else if(view->iflag_view_type == VIEW_XY) {
		view_for_xy_plane(view);
	}
	else if(view->iflag_view_type == VIEW_XZ) {
		view_for_xz_plane(view);
	}
    else if(view->iflag_view_type == VIEW_YZ) {
        view_for_yz_plane(view);
    };
	return;
};
