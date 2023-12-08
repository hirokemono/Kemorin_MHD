
/* set_mesh_patch_2_gl_buf.c */

#include "set_mesh_patch_2_gl_buf.h"

static int add_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode, 
			double **xx_draw, int **ie_sf_viewer, int *node_quad_2_linear_tri, 
			double normal_ele[3], double normal_nod[9], 
			double f_color[4], int inum_tri, struct gl_strided_buffer *strided_buf){
	int inod, k, kr, k1, nd;
	int count;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = node_quad_2_linear_tri[3*ie_local+kr] - 1;
		inod = ie_sf_viewer[abs(iele)-1][k1]-1;
		
        set_node_stride_buffer((ITHREE*inum_tri+k), strided_buf);
		
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = xx_draw[inod][nd];};
		for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = f_color[nd];};
		
		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_nod[3*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_ele[nd];};
		};
		
		if(polygon_mode == REVERSE_POLYGON){
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = -strided_buf->n_draw[nd];};
		};
	};
	count = inum_tri + 1;
	return count;
};


int count_mesh_patch_buf(int *istack_grp, int *ip_domain_far,
			struct viewer_mesh *mesh_s, int *iflag_domain){
	int i, ip, icou, ist, ied;
	
	int num_patch = 0;
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				num_patch = num_patch + mesh_s->nsurf_each_tri;
			};
		};
	};
	
	return num_patch;
}

int add_mesh_patch_to_buf(int ist_tri, int shading_mode, int polygon_mode, int surface_color,
			int color_mode, int color_loop, double opacity, float single_color[4], 
			int num_grp, int *istack_grp, int *item_grp, 
			double **normal_ele, double **normal_nod, int *isort_grp, 
			int *ip_domain_far, int igrp, struct viewer_mesh *mesh_s, int *iflag_domain, 
			struct gl_strided_buffer *mesh_buf){
	int i, ip, icou, inum, ist, ied, j, jnum;
	double f_color[4];
	
	int inum_tri = ist_tri;
	
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			set_patch_color_mode_c(surface_color, color_mode, color_loop,
                                   ip, mesh_s->num_pe_sf,
                                   igrp, num_grp, opacity,
                                   single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				inum = isort_grp[icou];
				
				for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
					jnum = j + inum * mesh_s->nsurf_each_tri;
					/*
					printf("%d, %f %f %f \n", jnum, normal_ele[jnum][0],
								normal_ele[jnum][1], normal_ele[jnum][2]);
					 */
					inum_tri = add_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
								mesh_s->xx_draw, mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri, 
								normal_ele[jnum], normal_nod[jnum], 
								f_color, inum_tri, mesh_buf);
				};
			};
		};
	};
	
	return inum_tri;
}

int count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	int num_patch = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		num_patch = num_patch
				+ count_mesh_patch_buf(mesh_s->isurf_stack_domain_sf, mesh_s->ip_domain_far, 
					mesh_s, mesh_m->draw_domains_solid);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_solid[i] != 0 && mesh_m->ele_grp_opacity >= 1.0){
			num_patch = num_patch
					+ count_mesh_patch_buf(&mesh_s->ele_stack_sf[ip_st], mesh_s->ip_domain_far, 
						mesh_s, mesh_m->always_draw_domains);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_solid[i] != 0 && mesh_m->surf_grp_opacity >= 1.0){
			num_patch = num_patch
					+ count_mesh_patch_buf(&mesh_s->surf_stack_sf[ip_st], mesh_s->ip_domain_far, 
						mesh_s, mesh_m->always_draw_domains);
		};
	};
	return num_patch;
}


int count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	int num_patch = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		num_patch = num_patch
				+ count_mesh_patch_buf(mesh_s->isurf_stack_domain_sf, mesh_s->ip_domain_far, 
					mesh_s, mesh_m->draw_domains_solid);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			if( mesh_m->draw_elegrp_solid[i] != 0){
				num_patch = num_patch
						+ count_mesh_patch_buf(&mesh_s->ele_stack_sf[ip_st], mesh_s->ip_domain_far, 
							mesh_s, mesh_m->always_draw_domains);
			};
		};
	}
	
	/* ! draw surface group */
	if(mesh_m->surf_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			if( mesh_m->draw_surfgrp_solid[i] != 0){
				num_patch = num_patch
						+ count_mesh_patch_buf(&mesh_s->surf_stack_sf[ip_st], mesh_s->ip_domain_far, 
							mesh_s, mesh_m->always_draw_domains);
			};
		};
	};
	return num_patch;
}

int set_solid_mesh_patches_to_buf(int shading_mode, struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	int ist_tri = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_solid[i] != 0 && mesh_m->ele_grp_opacity >= 1.0){
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
					mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
					mesh_m->ele_surface_color_code, 
					mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
					mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
					mesh_s->iele_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_solid[i] != 0 && mesh_m->surf_grp_opacity >= 1.0){
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
		};
	};
	return ist_tri;
}


int set_transparent_mesh_patches_to_buf(int shading_mode, struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	int ist_tri = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
				mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
				mesh_m->num_of_color_loop, mesh_m->domain_opacity,
				mesh_m->domain_surface_color_code, 
				mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf, 
				mesh_s->isurf_domain_sf, mesh_s->normal_domain, mesh_s->norm_nod_domain,
				mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_buf);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			
			if( mesh_m->draw_elegrp_solid[i] != 0){
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
					mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
					mesh_m->ele_surface_color_code, 
					mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
					mesh_s->ele_item_sf, mesh_s->normal_ele_grp, mesh_s->norm_nod_ele_grp,
					mesh_s->iele_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
			};
		};
	}
	
	/* ! draw surface group */
	if(mesh_m->surf_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
		
			if( mesh_m->draw_surfgrp_solid[i] != 0){
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode, 
					mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
					mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
					mesh_m->surf_surface_color_code,
					mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
					mesh_s->surf_item_sf, mesh_s->normal_surf_grp, mesh_s->norm_nod_surf_grp, 
					mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
					i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
			};
		};
	};
	return ist_tri;
}
