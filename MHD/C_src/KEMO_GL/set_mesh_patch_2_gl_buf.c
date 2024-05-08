
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


static long count_mesh_patch_buf(int *istack_grp, struct viewer_mesh *mesh_s, 
                                 int *iflag_domain){
	int ip, icou, ist, ied;
	long num_patch = 0;
	for(ip=0; ip<mesh_s->num_pe_sf; ip++){
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


static long set_merged_mesh_patch_list(const long ist_tri, int *istack_grp, long ist_group_patch,
                                       int num_pe_sf, int nsurf_each_tri, int *iflag_domain,
                                       long *iele_solid_patch){
	int ip, ist, ied, j;
    long inum;
	
	long inum_tri = ist_tri;
	
	for(ip = 0; ip < num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(inum = ist; inum < ied; inum++){
				for (j = 0; j < nsurf_each_tri; j++) {
                    iele_solid_patch[inum_tri] = j + (inum + ist_group_patch)*nsurf_each_tri;
                    inum_tri = inum_tri + 1;
				};
			};
		};
	};
	
	return inum_tri;
}

void set_trans_mesh_patch_for_sort(struct viewer_mesh *mesh_s,
                                   const long *iele_trans_patch, const double *z_ele_view,
                                   float *z_trans_patch, long *index_trans_patch){
	int j, icolor;
    long inum, jtem, item, icou;
    
    for (inum =0;inum<mesh_s->ntot_trans_patch; inum++) {
        icou = mesh_s->iele_trans_patch[inum] / mesh_s->nsurf_each_tri;
        j =    mesh_s->iele_trans_patch[inum] % mesh_s->nsurf_each_tri;
        
        item =  mesh_s->item_mesh_patch[icou];
        jtem = j + (labs(item)-1) * mesh_s->nsurf_each_tri;
        z_trans_patch[inum] =     (float) z_ele_view[jtem];
        index_trans_patch[inum] = iele_trans_patch[inum];
    };
    return;
}


void add_solid_mesh_patch_to_buf(int shading_mode, int polygon_mode, 
                                 struct viewer_mesh *mesh_s,
                                 struct gl_strided_buffer *mesh_buf){
	int j, icolor;
    long inum, icou, jnum, item;
	
	long inum_tri = 0;
	
    for (inum =0;inum<mesh_s->ntot_solid_patch; inum++) {
        icou = mesh_s->iele_solid_patch[inum] / mesh_s->nsurf_each_tri;
        j =    mesh_s->iele_solid_patch[inum] % mesh_s->nsurf_each_tri;
        
        item =  mesh_s->item_mesh_patch[icou];
        jnum = j + (icou) * mesh_s->nsurf_each_tri;
        icolor = mesh_s->igroup_mesh_patch[jnum];
        inum_tri = add_each_mesh_tri_patch(j, (int) item, shading_mode, polygon_mode,
                                           mesh_s->nnod_4_surf, mesh_s->xyzw_draw,
                                           mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri,
                                           &mesh_s->normal_mesh_patch[4*jnum],
                                           &mesh_s->normal_nod_mesh_patch[12*jnum],
                                           &mesh_s->mesh_color[4*icolor], inum_tri, mesh_buf);
    };
	return;
}


void add_trans_mesh_patch_to_buf(int shading_mode, int polygon_mode,
                                 struct viewer_mesh *mesh_s,
                                 struct gl_strided_buffer *mesh_buf){
	int j, icolor;
    long inum, jnum, item, icou;
	
	long inum_tri = 0;
	
    for (inum =0;inum<mesh_s->ntot_trans_patch; inum++) {
        icou = mesh_s->iele_trans_patch[inum] / mesh_s->nsurf_each_tri;
        j =    mesh_s->iele_trans_patch[inum] % mesh_s->nsurf_each_tri;
        
        item =  mesh_s->item_mesh_patch[icou];
        jnum = j + (icou) * mesh_s->nsurf_each_tri;
        icolor = mesh_s->igroup_mesh_patch[jnum];
//        printf(" %d ",icolor);
        /*
        printf("color %d %f %f %f %f \n",icolor,
                            mesh_s->mesh_color[4*icolor+0],
                            mesh_s->mesh_color[4*icolor+1],
                            mesh_s->mesh_color[4*icolor+2],
                            mesh_s->mesh_color[4*icolor+3]);
         */
        inum_tri = add_each_mesh_tri_patch(j, (int) item, shading_mode, polygon_mode,
                                           mesh_s->nnod_4_surf, mesh_s->xyzw_draw,
                                           mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri,
                                           &mesh_s->normal_mesh_patch[4*jnum],
                                           &mesh_s->normal_nod_mesh_patch[12*jnum],
                                           &mesh_s->mesh_color[4*icolor], inum_tri, mesh_buf);
    };
	return;
}


long count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	long num_patch = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		num_patch = num_patch + count_mesh_patch_buf(mesh_s->isurf_stack_domain_sf,
                                                     mesh_s, mesh_m->draw_domains_solid);
	};
	
	/* ! draw element group */
    if(mesh_m->ele_grp_opacity >= 1.0){
        for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
            if(mesh_m->draw_elegrp_solid[i]){
                ip_st = i * mesh_s->num_pe_sf;
                num_patch = num_patch + count_mesh_patch_buf(&mesh_s->ele_stack_sf[ip_st],
                                                         mesh_s, mesh_m->always_draw_domains);
            };
		};
	};
	
	/* ! draw surface group */
    if(mesh_m->surf_grp_opacity >= 1.0){
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		if( mesh_m->draw_surfgrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
                num_patch = num_patch + count_mesh_patch_buf(&mesh_s->surf_stack_sf[ip_st],
                                                             mesh_s, mesh_m->always_draw_domains);
            };
		};
	};
	return num_patch;
}


long count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m){
	int i, ip_st;
	
	long num_patch = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		num_patch = num_patch + count_mesh_patch_buf(mesh_s->isurf_stack_domain_sf, 
                                                     mesh_s, mesh_m->draw_domains_solid);
	};
	
	/* ! draw element group */
	
    if(mesh_m->ele_grp_opacity < 1.0){
        for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
            if(mesh_m->draw_elegrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
				num_patch = num_patch + count_mesh_patch_buf(&mesh_s->ele_stack_sf[ip_st],
                                                             mesh_s, mesh_m->always_draw_domains);
			};
		};
	}
	
	/* ! draw surface group */
	if(mesh_m->surf_grp_opacity < 1.0){
		for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
			if(mesh_m->draw_surfgrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
				num_patch = num_patch + count_mesh_patch_buf(&mesh_s->surf_stack_sf[ip_st],
                                                             mesh_s, mesh_m->always_draw_domains);
			};
		};
	};
	return num_patch;
}

long set_solid_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                   struct viewer_mesh *mesh_s,
                                   long *iele_solid_patch){
	int i, ip_st;
	long ist_solid = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity >= 1.0){
		ist_solid = set_merged_mesh_patch_list(ist_solid, mesh_s->isurf_stack_domain_sf,
                                               mesh_s->ist_domain_patch, mesh_s->num_pe_sf,
                                               mesh_s->nsurf_each_tri, mesh_m->draw_domains_solid,
                                               iele_solid_patch);
	};
	
	/* ! draw element group */
    if(mesh_m->ele_grp_opacity >= 1.0){
        for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
            if(mesh_m->draw_elegrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
                ist_solid = set_merged_mesh_patch_list(ist_solid, &mesh_s->ele_stack_sf[ip_st],
                                                       mesh_s->ist_ele_grp_patch, mesh_s->num_pe_sf,
                                                       mesh_s->nsurf_each_tri, mesh_m->always_draw_domains,
                                                       iele_solid_patch);
            };
		};
	};
	
	/* ! draw surface group */
    if(mesh_m->surf_grp_opacity >= 1.0){
        for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
            if( mesh_m->draw_surfgrp_solid[i]){
                ip_st = i * mesh_s->num_pe_sf;
                ist_solid = set_merged_mesh_patch_list(ist_solid, &mesh_s->surf_stack_sf[ip_st],
                                                       mesh_s->ist_sf_grp_patch, mesh_s->num_pe_sf,
                                                       mesh_s->nsurf_each_tri, mesh_m->always_draw_domains,
                                                       iele_solid_patch);
            };
		};
	};
    return ist_solid;
}


long set_transparent_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                         struct viewer_mesh *mesh_s, 
                                         long *iele_trans_patch){
	int i, ip_st;
	long ist_trans = 0;
	if(mesh_m->draw_surface_solid != 0 && mesh_m->domain_opacity < 1.0){
		ist_trans = set_merged_mesh_patch_list(ist_trans, mesh_s->isurf_stack_domain_sf,
                                               mesh_s->ist_domain_patch, mesh_s->num_pe_sf,
                                               mesh_s->nsurf_each_tri, mesh_m->draw_domains_solid,
                                               iele_trans_patch);
	};
	
	/* ! draw element group */
    if(mesh_m->ele_grp_opacity < 1.0){
        for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
            if( mesh_m->draw_elegrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
                ist_trans = set_merged_mesh_patch_list(ist_trans, &mesh_s->ele_stack_sf[ip_st],
                                                       mesh_s->ist_ele_grp_patch, mesh_s->num_pe_sf,
                                                       mesh_s->nsurf_each_tri, mesh_m->always_draw_domains,
                                                       iele_trans_patch);
            };
		};
	};
	
	/* ! draw surface group */
    if(mesh_m->surf_grp_opacity < 1.0){
        for(i = 0; i < mesh_s->ngrp_surf_sf; i++){
            if( mesh_m->draw_surfgrp_solid[i] != 0){
                ip_st = i * mesh_s->num_pe_sf;
                ist_trans = set_merged_mesh_patch_list(ist_trans, &mesh_s->surf_stack_sf[ip_st],
                                                       mesh_s->ist_sf_grp_patch, mesh_s->num_pe_sf,
                                                       mesh_s->nsurf_each_tri, mesh_m->always_draw_domains,
                                                       iele_trans_patch);
            };
        };
	};
    return ist_trans;
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
/*
    for (igrp = 0; igrp < mesh_s->ngrp_surf_sf; igrp++){
        printf("S color %d %f %f %f %f \n",igrp+mesh_s->ist_surf_grp,
               mesh_s->mesh_color[4*mesh_s->ist_surf_grp+4*igrp+0],
               mesh_s->mesh_color[4*mesh_s->ist_surf_grp+4*igrp+1],
               mesh_s->mesh_color[4*mesh_s->ist_surf_grp+4*igrp+2],
               mesh_s->mesh_color[4*mesh_s->ist_surf_grp+4*igrp+3]);
    }
    printf("D color %d %f %f %f %f \n",0+mesh_s->ist_domain_grp,
           mesh_s->mesh_color[4*mesh_s->ist_domain_grp+0],
           mesh_s->mesh_color[4*mesh_s->ist_domain_grp+1],
           mesh_s->mesh_color[4*mesh_s->ist_domain_grp+2],
           mesh_s->mesh_color[4*mesh_s->ist_domain_grp+3]);
*/
    return;
}

