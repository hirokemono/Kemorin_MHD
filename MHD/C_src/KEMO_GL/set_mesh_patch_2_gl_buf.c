
/* set_mesh_patch_2_gl_buf.c */

#include "set_mesh_patch_2_gl_buf.h"

static long add_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode, 
                                    int nnod_4_sf, double *xyzw_draw, int *ie_sf_viewer,
                                    int *node_quad_2_linear_tri, double normal_ele[4], double normal_nod[12],
                                    double f_color[4], const long inum_tri, struct gl_strided_buffer *strided_buf){
	int inod, inum, k1, nd;
    long k, kr;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = node_quad_2_linear_tri[3*ie_local+kr];
        inum = k1-1 + nnod_4_sf * (abs(iele)-1);
		inod = ie_sf_viewer[inum]-1;
		
        set_node_stride_buffer((ITHREE*inum_tri+k), strided_buf);
		
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = xyzw_draw[4*inod+nd];};
		for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = f_color[nd];};
		
		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_nod[4*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_ele[nd];};
		};
		
		if(polygon_mode == REVERSE_POLYGON){
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = -strided_buf->n_draw[nd];};
		};
	};
	return (inum_tri + 1);
};


static long count_mesh_patch_buf(int *istack_grp, long *ip_domain_far,
                                 struct viewer_mesh *mesh_s, int *iflag_domain){
	int i, ip, icou, ist, ied;
	
	long num_patch = 0;
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = (int) ip_domain_far[i] - 1;
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

static long add_mesh_patch_to_buf(const long ist_tri, int shading_mode, int polygon_mode, 
                                  int surface_color, int color_mode, int color_loop,
                                  double opacity, float single_color[4], double *f_color,
                                  int num_grp, int *istack_grp, int *item_grp, 
                                  double *normal_ele, double *normal_nod,
                                  long *isort_grp, long *ip_domain_far, int igrp,
                                  struct viewer_mesh *mesh_s, int *iflag_domain,
                                  struct gl_strided_buffer *mesh_buf){
	int i, ip, icou, ist, ied, j;
    long inum, jnum;
	
	long inum_tri = ist_tri;
	
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = (int) ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				inum = isort_grp[icou];
				
				for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
					jnum = j + inum * mesh_s->nsurf_each_tri;
					/*
					printf("%d, %f %f %f \n", jnum, normal_ele[4*jnum+0],
                            normal_ele[4*jnum+1], normal_ele[4*jnum+2]);
					 */
					inum_tri = add_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
                                                       mesh_s->nnod_4_surf, mesh_s->xyzw_draw,
                                                       mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri,
                                                       &normal_ele[4*jnum], &normal_nod[12*jnum],
                                                       &f_color[4*ip], inum_tri, mesh_buf);
				};
			};
		};
	};
	
	return inum_tri;
}

long count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	long num_patch = 0;
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


long count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	long num_patch = 0;
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

long set_solid_mesh_patches_to_buf(int shading_mode, 
                                   struct viewer_mesh *mesh_s, 
                                   struct mesh_menu_val *mesh_m, 
                                   struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
    long ist_norm;
	long ist_tri = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
        ist_norm = mesh_s->ist_domain_patch * mesh_s->nsurf_each_tri;
		ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                        mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
                                        mesh_m->num_of_color_loop, mesh_m->domain_opacity,
                                        mesh_m->domain_surface_color_code, &mesh_s->mesh_color[4*mesh_s->ist_domain_grp],
                                        mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf,
                                        mesh_s->isurf_domain_sf,
                                        &mesh_s->normal_mesh_patch[4*ist_norm],
                                        &mesh_s->normal_nod_mesh_patch[12*ist_norm],
                                        mesh_s->iele_domain_far, mesh_s->ip_domain_far,
                                        IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_buf);
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_elegrp_solid[i] != 0 && mesh_m->ele_grp_opacity >= 1.0){
            ist_norm = mesh_s->ist_ele_grp_patch * mesh_s->nsurf_each_tri;
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                            mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
                                            mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
                                            mesh_m->ele_surface_color_code, &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_ele_grp)],
                                            mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
                                            mesh_s->ele_item_sf,
                                            &mesh_s->normal_mesh_patch[4*ist_norm],
                                            &mesh_s->normal_nod_mesh_patch[12*ist_norm],
                                            mesh_s->iele_grp_far, mesh_s->ip_domain_far,
                                            i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_solid[i] != 0 && mesh_m->surf_grp_opacity >= 1.0){
            ist_norm = mesh_s->ist_sf_grp_patch * mesh_s->nsurf_each_tri;
			ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                            mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
                                            mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
                                            mesh_m->surf_surface_color_code, &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_surf_grp)],
                                            mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
                                            mesh_s->surf_item_sf,
                                            &mesh_s->normal_mesh_patch[4*ist_norm],
                                            &mesh_s->normal_nod_mesh_patch[12*ist_norm],
                                            mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
                                            i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
		};
	};
	return ist_tri;
}


long set_transparent_mesh_patches_to_buf(int shading_mode,
                                         struct viewer_mesh *mesh_s, 
                                         struct mesh_menu_val *mesh_m,
                                         struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
    long ist_norm;
	long ist_tri = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
        ist_norm = mesh_s->ist_domain_patch * mesh_s->nsurf_each_tri;
		ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                        mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
                                        mesh_m->num_of_color_loop, mesh_m->domain_opacity,
                                        mesh_m->domain_surface_color_code, &mesh_s->mesh_color[4*mesh_s->ist_domain_grp],
                                        mesh_s->num_pe_sf, mesh_s->isurf_stack_domain_sf,
                                        mesh_s->isurf_domain_sf,
                                        &mesh_s->normal_mesh_patch[4*ist_norm],
                                        &mesh_s->normal_nod_mesh_patch[12*ist_norm],
                                        mesh_s->iele_domain_far, mesh_s->ip_domain_far,
				IZERO, mesh_s, mesh_m->draw_domains_solid, mesh_buf);
	};
	
	/* ! draw element group */
	
	if(mesh_m->ele_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
			ip_st = i * mesh_s->num_pe_sf;
			
			if( mesh_m->draw_elegrp_solid[i] != 0){
                ist_norm = mesh_s->ist_ele_grp_patch * mesh_s->nsurf_each_tri;
                ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                            mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
                                            mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
                                            mesh_m->ele_surface_color_code, &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_ele_grp)],
                                            mesh_s->ngrp_ele_sf, &mesh_s->ele_stack_sf[ip_st],
                                            mesh_s->ele_item_sf,
                                                &mesh_s->normal_mesh_patch[4*ist_norm],
                                                &mesh_s->normal_nod_mesh_patch[12*ist_norm],
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
                ist_norm = mesh_s->ist_sf_grp_patch * mesh_s->nsurf_each_tri;
                ist_tri = add_mesh_patch_to_buf(ist_tri, shading_mode, mesh_m->polygon_mode,
                                            mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
                                            mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
                                            mesh_m->surf_surface_color_code, &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_surf_grp)],
                                            mesh_s->ngrp_surf_sf, &mesh_s->surf_stack_sf[ip_st],
                                            mesh_s->surf_item_sf,
                                                &mesh_s->normal_mesh_patch[4*ist_norm],
                                                &mesh_s->normal_nod_mesh_patch[12*ist_norm],
                                            mesh_s->isurf_grp_far, mesh_s->ip_domain_far,
                                            i, mesh_s, mesh_m->always_draw_domains, mesh_buf);
			};
		};
	};
	return ist_tri;
}


void set_mesh_group_patch_colors(int surface_color, int color_mode, int color_loop,
                                 double opacity, float single_color[4], int num_grp, int igrp, 
                                 int num_pe_sf, int *iflag_domain, double *f_color){
	for(int ip = 0; ip < num_pe_sf; ip++){
        if(iflag_domain[ip] == 0){
            f_color[4*ip+3] = 0.0;
        }else{
			set_patch_color_mode_c(surface_color, color_mode, color_loop,
                                   ip, num_pe_sf, igrp, num_grp, opacity,
                                   single_color, &f_color[4*ip]);
		};
	};
	return;
}

void set_mesh_patch_colors(struct mesh_menu_val *mesh_m, struct viewer_mesh *mesh_s){
    int igrp, ip_st;
    set_mesh_group_patch_colors(mesh_m->domain_surface_color, mesh_m->mesh_color_mode,
                                mesh_m->num_of_color_loop, mesh_m->domain_opacity,
                                mesh_m->domain_surface_color_code, mesh_s->num_pe_sf, IZERO,
                                mesh_s->num_pe_sf, mesh_m->draw_domains_solid,
                                &mesh_s->mesh_color[4*mesh_s->ist_domain_grp]);
    
    /* ! draw element group */
    for (igrp = 0; igrp < mesh_s->ngrp_ele_sf; igrp++){
        ip_st = igrp * mesh_s->num_pe_sf;
        set_mesh_group_patch_colors(mesh_m->ele_surface_color, mesh_m->mesh_color_mode,
                                    mesh_m->num_of_color_loop, mesh_m->ele_grp_opacity,
                                    mesh_m->ele_surface_color_code, mesh_s->ngrp_ele_sf, igrp,
                                    mesh_s->num_pe_sf, mesh_m->always_draw_domains,
                                    &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_ele_grp)]);
    };
    
    /* ! draw surface group */
    for (igrp = 0; igrp < mesh_s->ngrp_surf_sf; igrp++){
        ip_st = igrp * mesh_s->num_pe_sf;
        set_mesh_group_patch_colors(mesh_m->surf_surface_color, mesh_m->mesh_color_mode,
                                    mesh_m->num_of_color_loop, mesh_m->surf_grp_opacity,
                                    mesh_m->surf_surface_color_code, mesh_s->ngrp_surf_sf, igrp,
                                    mesh_s->num_pe_sf, mesh_m->always_draw_domains,
                                    &mesh_s->mesh_color[4*(ip_st+mesh_s->ist_surf_grp)]);
    };
    return;
}

