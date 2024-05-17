
/* set_mesh_node_2_gl_buf.c */

#include "set_mesh_node_2_gl_buf.h"

static long set_mesh_node_ico_to_buf(int nthreads, long *istack_patch_pe,
                                     int num_grp, int igrp,
                                     int *istack_grp, int *item_grp,
                                     struct viewer_mesh *mesh_s, double node_diam,
                                     int node_color, int color_mode,
                                     int color_loop, float single_color[4],
                                     int *iflag_domain,
                                     struct gl_strided_buffer *mesh_buf){
	double f_color[4];
    int ip;
    long inum_tri;

	set_node_color_mode_c(node_color, color_mode, color_loop,
                          igrp, num_grp, single_color);
	
	inum_tri = istack_patch_pe[0];
    for(ip = 0; ip<mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_patch_color_mode_c(node_color, color_mode, color_loop,
                                   ip, mesh_s->num_pe_sf,
                                   igrp, num_grp, ONE,
                                   single_color, f_color);
	
            inum_tri = sel_each_grp_nod_ico_to_buf_pthread(istack_patch_pe[ip], nthreads,
                                                           istack_grp[ip], istack_grp[ip+1],
                                                           item_grp, mesh_s, node_diam, f_color,
                                                           mesh_buf);
		}
	};
	
	return inum_tri;
}


long count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                            long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                            long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch){
	int i, ip_st;
    long num_patch = 0;
    num_patch = add_mesh_node_ico_to_buf(num_patch, mesh_s->nod_stack_domain_sf,
                                                            mesh_s->num_pe_sf,
                                                            mesh_m->draw_domains_nod,
                                                            &istack_node_domain_patch[0]);
	
	/* ! draw node group */
	
    istack_node_nod_grp_patch[0] = num_patch;
	for(i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if(mesh_m->draw_nodgrp_nod[i]){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_nod_grp_patch[ip_st]);
		};
	};

	/* ! draw element group */
    istack_node_ele_grp_patch[0] = num_patch;
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->ele_nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_ele_grp_patch[ip_st]);
		};
	};

	/* ! draw surface group */
    istack_node_surf_grp_patch[0] = num_patch;
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		
		if( mesh_m->draw_surfgrp_nod[i] ){
			num_patch = add_mesh_node_ico_to_buf(num_patch, &mesh_s->surf_nod_stack_sf[ip_st],
                                                 mesh_s->num_pe_sf,
                                                 mesh_m->always_draw_domains,
                                                 &istack_node_surf_grp_patch[ip_st]);
		};
	};
	return num_patch;
}


long set_mesh_node_to_buf(const int nthreads,
                          long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                          long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch,
                          double node_diam, struct viewer_mesh *mesh_s,
                          struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	long num_tri = 0;
	
    num_tri = set_mesh_node_ico_to_buf(nthreads, istack_node_domain_patch,
                                       mesh_s->num_pe_sf, IZERO,
                                       mesh_s->nod_stack_domain_sf,
                                       mesh_s->nod_item_domain_sf, mesh_s,
                                       node_diam, mesh_m->domain_node_color,
                                       mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                       mesh_m->domain_node_color_code, mesh_m->draw_domains_nod,
                                       mesh_buf);
	
	/* ! draw node group */
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_nodgrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(nthreads, &istack_node_nod_grp_patch[ip_st],
                                               mesh_s->ngrp_nod_sf, i, &mesh_s->nod_stack_sf[ip_st],
                                               mesh_s->nod_item_sf, mesh_s,
                                               node_diam, mesh_m->node_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->node_node_color_code, mesh_m->always_draw_domains,
                                               mesh_buf);
	
		};
	};
	
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_elegrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(nthreads, &istack_node_ele_grp_patch[ip_st],
                                               mesh_s->ngrp_ele_sf, i,
                                               &mesh_s->ele_nod_stack_sf[ip_st],
                                               mesh_s->ele_nod_item_sf, mesh_s,
                                               node_diam, mesh_m->ele_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->ele_node_color_code,
                                               mesh_m->always_draw_domains,
                                               mesh_buf);
	
		};
	};
	
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		ip_st = i * mesh_s->num_pe_sf;
		if( mesh_m->draw_surfgrp_nod[i] ){
            num_tri = set_mesh_node_ico_to_buf(nthreads, &istack_node_surf_grp_patch[ip_st],
                                               mesh_s->ngrp_surf_sf, i,
                                               &mesh_s->surf_nod_stack_sf[ip_st],
                                               mesh_s->surf_nod_item_sf, mesh_s,
                                               node_diam, mesh_m->surf_node_color,
                                               mesh_m->mesh_color_mode, mesh_m->num_of_color_loop,
                                               mesh_m->surf_node_color_code,
                                               mesh_m->always_draw_domains,
                                               mesh_buf);
	
		};
	};
	return num_tri;
}
